# Header ----
#
# Script name: 00_01_cfb_functions.R
#
# Purpose of script: Shared functions for the college football (cfbfastR) project.
#   Sourced by the 02_xx analysis scripts after 00_00_dirs_libs_funs_vals.R.
#     1. Data access with a per-week cache (only new weeks hit the API)
#     2. Red zone table building (play-by-play definition of a red zone trip)
#     3. Models (expected points, opponent-adjusted ridge ratings, shrinkage)
#     4. Plot helpers (theme, logos, watermark, quadrant scatter, saving)
#
# Author: Japbir Gill, MBA MSEd
#
# Date Created: 2026-10-11
#
# Copyright (c) Japbir Gill, 2026
# Email: Japbir.Gill@gmail.com
#
# Notes ----
# - Expects these objects from 00_00_dirs_libs_funs_vals.R:
#     DIR_OUTPUTS, watermark_path
#   (find_quadrant_whitespace() is now defined here; remove the old copy from 00_00)
#   and optionally DIR_DATA (the cache goes there if it exists).
# - Cache layout: <CFB_CACHE_DIR>/<kind>/<season>/<kind>_<season>_<type>_wkNN.rds
#   Delete a file (or use overwrite_weeks) to re-pull that week.
#

library(tidyverse)
library(cfbfastR)
library(glmnet)
library(ggpath)
library(splines)

# Cache Location ----
CFB_CACHE_DIR <- if (exists("DIR_DATA")) file.path(DIR_DATA, "cfbd_cache") else file.path(DIR_OUTPUTS, "cfbd_cache")
dir.create(CFB_CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

# 1. Data Access ----

## Column checks ----
check_cols <- function(df, cols, df_name) {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop(df_name, " is missing columns: ", paste(missing, collapse = ", "),
         "\nInspect names(", df_name, ") and update the column references.", call. = FALSE)
  }
  invisible(TRUE)
}

## One week of one CFBD endpoint, cached to disk ----
# fetch: a function with no arguments that returns the data for that week.
# Empty results are not saved, so the week is tried again next run.
cfbd_cached_week <- function(kind, season, wk, fetch, season_type = "regular", overwrite = FALSE) {
  dir  <- file.path(CFB_CACHE_DIR, kind, season)
  path <- file.path(dir, sprintf("%s_%d_%s_wk%02d.rds", kind, season, season_type, wk))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  
  if (file.exists(path) && !overwrite) return(readRDS(path))
  
  message(sprintf("Querying API: %s, %d %s week %d", kind, season, season_type, wk))
  out <- tryCatch(fetch(), error = function(e) {
    warning(sprintf("%s week %d failed: %s", kind, wk, conditionMessage(e)), call. = FALSE)
    NULL
  })
  if (is.null(out) || nrow(out) == 0) {
    message(sprintf("  %s week %d: no rows returned, nothing saved", kind, wk))
    return(NULL)
  }
  
  out <- as_tibble(out)   # drop cfbfastR's print class so weeks bind cleanly
  saveRDS(out, path)
  out
}

## Weeks whose games are all finished ----
# The calendar is cached once per season (one API call the first time).
completed_weeks <- function(season, season_type = "regular", buffer_hours = 24) {
  dir  <- file.path(CFB_CACHE_DIR, "calendar")
  path <- file.path(dir, sprintf("calendar_%d.rds", season))
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  
  if (file.exists(path)) {
    cal <- readRDS(path)
  } else {
    cal <- as_tibble(cfbd_calendar(season))
    saveRDS(cal, path)
  }
  check_cols(cal, c("week", "season_type", "last_game_start"), "calendar")
  
  last_end <- if (inherits(cal$last_game_start, "POSIXct")) {
    cal$last_game_start
  } else {
    lubridate::ymd_hms(cal$last_game_start, quiet = TRUE)
  }
  
  cal |>
    mutate(last_end = last_end) |>
    filter(season_type == !!season_type, last_end < Sys.time() - buffer_hours * 3600) |>
    pull(week) |>
    sort()
}

## Drives, play-by-play, and game info for a season ----
# weeks = NULL uses every completed week. overwrite_weeks re-pulls specific
# weeks (e.g., after CFBD corrections).
load_cfb_season <- function(season, weeks = NULL, season_type = "regular", overwrite_weeks = integer(0)) {
  if (is.null(weeks)) weeks <- completed_weeks(season, season_type)
  message(sprintf("Loading %d %s season, weeks %s", season, season_type, paste(range(weeks), collapse = "-")))
  
  fetch_all <- function(kind, fetch_fun) {
    map(weeks, \(wk) cfbd_cached_week(kind, season, wk, \() fetch_fun(wk), season_type,
                                      overwrite = wk %in% overwrite_weeks)) |>
      bind_rows()
  }
  
  list(
    drives = fetch_all("drives_fbs", \(wk) cfbd_drives(season, season_type = season_type, week = wk, division = "fbs")),
    pbp    = fetch_all("pbp_epa",    \(wk) cfbd_pbp_data(season, season_type = season_type, week = wk, epa_wpa = TRUE)),
    games  = fetch_all("game_info",  \(wk) cfbd_game_info(season, week = wk, season_type = season_type))
  )
}

## Team info (logos, conferences), refreshed monthly ----
get_team_info <- function(max_age_days = 30) {
  path <- file.path(CFB_CACHE_DIR, "team_info.rds")
  fresh <- file.exists(path) && difftime(Sys.time(), file.mtime(path), units = "days") < max_age_days
  if (fresh) return(readRDS(path))
  out <- as_tibble(cfbd_team_info())
  saveRDS(out, path)
  out
}

# 2. Red Zone Tables ----
# Red zone trip = a drive with at least one scrimmage snap at or inside the
# opponent's rz_line. Long TDs from outside the red zone are NOT trips.
# Drive points = score change on the drive (handles missed PATs and 2-pt tries).
# Excluded: drives ending at end of half/game, 2nd-half garbage time, odd point values.
build_rz_tables <- function(raw, rz_line = 20, garbage_margin = 28) {
  drives_raw <- raw$drives
  if (!"is_home_offense" %in% names(drives_raw)) drives_raw$is_home_offense <- NA
  
  check_cols(drives_raw, c("drive_id", "game_id", "offense", "defense", "drive_result", "start_period",
                           "start_offense_score", "start_defense_score", "end_offense_score",
                           "end_defense_score", "end_yards_to_goal"), "drives")
  check_cols(raw$pbp, c("drive_id", "game_id", "id_play", "pos_team", "def_pos_team", "period", "down",
                        "distance", "yards_to_goal", "yards_gained", "play_type", "rush", "pass",
                        "EPA", "success", "ep_before"), "pbp")
  check_cols(raw$games, c("game_id", "week", "neutral_site", "home_team", "away_team",
                          "home_points", "away_points"), "games")
  
  games <- raw$games |>
    mutate(game_id = as.character(game_id))
  
  ## Drives ----
  drives <- drives_raw |>
    mutate(drive_id = as.character(drive_id), game_id = as.character(game_id)) |>
    select(-any_of(c("week", "neutral_site"))) |>
    left_join(games |> select(game_id, week, neutral_site), by = "game_id") |>
    mutate(
      drive_points      = end_offense_score - start_offense_score,
      def_return_points = end_defense_score - start_defense_score,
      score_diff_start  = start_offense_score - start_defense_score,
      # home indicator for the ridge models: +1 home, -1 away, 0 neutral
      home = case_when(
        neutral_site %in% TRUE    ~ 0,
        is_home_offense %in% TRUE ~ 1,
        TRUE                      ~ -1
      ),
      # outcome flags from points, which avoids relying on drive_result labels
      flag_td    = as.integer(drive_points >= 6),
      flag_fg    = as.integer(drive_points == 3),
      flag_score = as.integer(drive_points >= 3),
      # exclusions
      flag_end_period = str_detect(drive_result, regex("END OF", ignore_case = TRUE)) %in% TRUE,
      flag_garbage    = (start_period >= 3 & abs(score_diff_start) > garbage_margin) %in% TRUE,
      flag_bad_score  = !(drive_points %in% c(0, 3, 6, 7, 8)),
      analysis_drive  = !flag_end_period & !flag_garbage & !flag_bad_score,
      rz_outcome = case_when(
        flag_td == 1 ~ "Touchdown",
        flag_fg == 1 ~ "Field Goal",
        str_detect(drive_result, regex("MISSED FG|BLOCKED FG|FG MISSED", ignore_case = TRUE)) ~ "Missed/Blocked FG",
        str_detect(drive_result, regex("INT|FUMBLE", ignore_case = TRUE)) ~ "Turnover",
        str_detect(drive_result, regex("DOWNS", ignore_case = TRUE)) ~ "Turnover on Downs",
        TRUE ~ "Other"
      )
    )
  
  message(sprintf("Drives with unusual point values (excluded): %d", sum(drives$flag_bad_score)))
  td_pts <- drives$drive_points[drives$flag_td == 1 & !is.na(drives$drive_points)]
  if (length(td_pts) > 0 && mean(td_pts == 6) > 0.9) {
    warning("Over 90% of TD drives show 6 points: PATs appear to be excluded from drive scores. ",
            "drive_points will understate TD value by ~1; consider using drive_points + flag_td.", call. = FALSE)
  }
  
  ## Play-by-play ----
  non_scrimmage <- regex(
    "kickoff|timeout|coin toss|end (of )?(period|half|game|regulation)|^penalty$|extra point|two point|uncategorized|placeholder",
    ignore_case = TRUE
  )
  
  pbp <- raw$pbp |>
    mutate(
      drive_id     = as.character(drive_id),
      game_id      = as.character(game_id),
      is_scrimmage = !str_detect(coalesce(play_type, "uncategorized"), non_scrimmage),
      is_rush_pass = coalesce(rush == 1 | pass == 1, FALSE)
    ) |>
    left_join(drives |> select(drive_id, analysis_drive), by = "drive_id")
  
  # play order within a game: prefer game_play_number; else id_play as a zero-padded string
  pbp$play_order <- if ("game_play_number" %in% names(pbp)) {
    as.numeric(pbp$game_play_number)
  } else {
    str_pad(format(pbp$id_play, scientific = FALSE, trim = TRUE), 25, pad = "0")
  }
  
  match_rate <- mean(unique(pbp$drive_id) %in% drives$drive_id)
  message(sprintf("PBP drive_id match rate to drives table: %.1f%%", 100 * match_rate))
  if (match_rate < 0.9) {
    warning("Low drive_id match rate: check that drive_id formats agree between ",
            "cfbd_drives() and cfbd_pbp_data().", call. = FALSE)
  }
  
  ## Red zone trips ----
  rz_trips <- pbp |>
    filter(is_scrimmage, yards_to_goal <= rz_line, !is.na(drive_id)) |>
    arrange(game_id, play_order) |>
    summarise(
      rz_entry_ytg  = first(yards_to_goal),
      rz_entry_down = first(down),
      rz_entry_dist = first(distance),
      rz_entry_ep   = first(ep_before),
      rz_snaps      = n(),
      .by = drive_id
    )
  
  prod_rz_drives <- drives |>
    inner_join(rz_trips, by = "drive_id") |>
    filter(analysis_drive) |>
    mutate(
      rz_entry_down = coalesce(pmin(rz_entry_down, 4), 1),
      rz_entry_zone = cut(rz_entry_ytg, c(0, 5, 10, rz_line), labels = c("1-5", "6-10", paste0("11-", rz_line)))
    )
  
  drives_clean <- drives |>
    filter(analysis_drive) |>
    mutate(rz_trip = drive_id %in% rz_trips$drive_id)
  
  list(games = games, drives = drives, drives_clean = drives_clean, pbp = pbp,
       rz_trips = rz_trips, prod_rz_drives = prod_rz_drives)
}

# 3. Models ----

## Expected red zone points by entry spot, and points over expected ----
add_poe <- function(rz_df) {
  xp_model <- lm(drive_points ~ ns(rz_entry_ytg, df = 3) + factor(rz_entry_down), data = rz_df)
  out <- rz_df |>
    mutate(
      xpts        = predict(xp_model, newdata = rz_df),
      poe         = drive_points - xpts,
      # CFBD EP is "next score" EP (it prices in the opponent scoring next), so
      # this alternative is not strictly points-this-drive.
      poe_cfbd_ep = drive_points - rz_entry_ep
    )
  attr(out, "xp_model") <- xp_model
  out
}

## Opponent-adjusted team ratings via ridge regression ----
# Full indicators for every offense AND defense (no dropped reference level)
# plus a home term; ridge handles the collinearity. Returns adj_off / adj_def on
# the outcome scale (points, or probability for binomial) at a neutral site vs.
# an average opponent.
# lambda: leave NULL to choose the penalty by cross-validation. Pass a value
#   (e.g., attr(earlier_fit, "lambda")) to reuse a penalty, so two fits on
#   different weeks are comparable. The chosen penalty is returned as attr "lambda".
fit_team_ridge <- function(df, y_col, family = c("gaussian", "binomial"), lambda = NULL) {
  family <- match.arg(family)
  df <- df |> filter(!is.na(.data[[y_col]]))
  
  teams <- sort(unique(c(df$offense, df$defense)))
  X_off <- model.matrix(~ 0 + factor(df$offense, levels = teams)); colnames(X_off) <- paste0("off::", teams)
  X_def <- model.matrix(~ 0 + factor(df$defense, levels = teams)); colnames(X_def) <- paste0("def::", teams)
  X <- cbind(X_off, X_def, home = df$home)
  
  # For binomial ridge, glmnet's automatic lambda path can stop after a few very
  # large values, which shrinks every team to the league average. An explicit
  # grid avoids that.
  lambda_grid <- if (family == "binomial") exp(seq(log(10), log(1e-5), length.out = 100)) else NULL
  
  if (is.null(lambda)) {
    fit <- cv.glmnet(X, df[[y_col]], family = family, alpha = 0, standardize = FALSE, lambda = lambda_grid)
    lambda_used <- fit$lambda.min
    cf <- coef(fit, s = "lambda.min")
  } else {
    # fit a short path ending at the requested lambda (more stable than a single value)
    fit <- glmnet(X, df[[y_col]], family = family, alpha = 0, standardize = FALSE,
                  lambda = lambda * c(100, 30, 10, 3, 1))
    lambda_used <- lambda
    cf <- coef(fit, s = lambda)
  }
  cf <- tibble(term = rownames(cf), coef = as.numeric(cf))
  
  intercept <- cf$coef[cf$term == "(Intercept)"]
  link_inv  <- if (family == "binomial") plogis else identity
  
  off_tbl <- cf |>
    filter(str_starts(term, "off::")) |>
    transmute(team = str_remove(term, "^off::"), off_coef = coef)
  def_tbl <- cf |>
    filter(str_starts(term, "def::")) |>
    transmute(team = str_remove(term, "^def::"), def_coef = coef)
  
  out <- full_join(off_tbl, def_tbl, by = "team") |>
    mutate(
      adj_off = link_inv(intercept + off_coef),
      adj_def = link_inv(intercept + def_coef)
    )
  attr(out, "lambda") <- lambda_used
  out
}

## Empirical Bayes (beta-binomial) shrinkage of a rate ----
# x successes out of n tries; returns rates pulled toward the league mean,
# more strongly for teams with fewer tries.
eb_shrink <- function(x, n) {
  ok     <- !is.na(x) & !is.na(n) & n > 0
  p      <- x[ok] / n[ok]
  m      <- weighted.mean(p, n[ok])
  v_obs  <- sum(n[ok] * (p - m)^2) / sum(n[ok])
  v_true <- max(v_obs - m * (1 - m) * mean(1 / n[ok]), 1e-6)   # remove binomial noise
  k      <- m * (1 - m) / v_true - 1                             # prior "sample size"
  out    <- rep(NA_real_, length(x))
  out[ok] <- (x[ok] + k * m) / (n[ok] + k)
  out
}

# 4. Plot Helpers ----

team_info <- get_team_info() |> select(school, logo, conference)

## Join logos/conferences; keeps FBS teams only ----
add_team_info <- function(df) {
  df |>
    left_join(team_info, by = join_by(team == school)) |>
    filter(!is.na(conference))
}

## Position at fraction p of a vector's range ----
rng_pos <- function(v, p) {
  r <- range(v, na.rm = TRUE)
  r[1] + p * diff(r)
}

## Watermark layer ----
add_watermark <- function(x, y, width = 0.02) {
  geom_from_path(data = watermark_path, aes(x = !!x, y = !!y, path = image_path),
                 width = width, alpha = 0.8, inherit.aes = FALSE)
}

## House theme ----
theme_rz <- function(base_size = 12, subtitle_size = 16) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title    = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = subtitle_size, face = "italic", hjust = 0.5),
      plot.caption  = element_text(hjust = 0)
    )
}

## Find open space for a label in each quadrant ----
# Replaces the version in 00_00 (this file is sourced after it, so this one wins).
# Same inputs and outputs as before (quadrant, x, y), plus `hits` (points under
# the label) and `fits` (whether the label fit inside its quadrant).
#
# How it avoids overlaps:
#   1. Works in 0-1 "panel" units, so x and y distances are comparable.
#   2. Each label must sit entirely inside its own quadrant, at least
#      `center_buffer` away from the mean lines. Labels in neighboring
#      quadrants therefore can never touch, even when both quadrants are
#      emptiest near the mean.
#   3. Each label is treated as a box (label_w x label_h), not a point. Within
#      its quadrant, it picks the spot covering the fewest logos, then the one
#      farthest from any logo, then the one farthest from the mean lines.
#   4. Anything in `avoid` (e.g., the watermark) counts as an obstacle.
#
# reverse_y = TRUE when the plot uses scale_y_reverse(), so "Top" means the top
# of the picture, not the largest y value.
# label_w / label_h can be one number or a named vector by quadrant, in panel
# units (0.20 = 20% of the panel width/height).
find_quadrant_whitespace <- function(df, x_col, y_col,
                                     x_mean = mean(df[[x_col]], na.rm = TRUE),
                                     y_mean = mean(df[[y_col]], na.rm = TRUE),
                                     reverse_y = FALSE,
                                     label_w = 0.18, label_h = 0.09,
                                     point_r = 0.02, center_buffer = 0.05, edge_buffer = 0.02,
                                     avoid = NULL, avoid_r = 0.05,
                                     expand = 0.05, grid_n = 30) {
  x <- df[[x_col]]; y <- df[[y_col]]
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]; y <- y[keep]
  
  # panel limits, matching ggplot's default 5% expansion
  xr <- range(c(x, x_mean)); yr <- range(c(y, y_mean))
  # guard against an axis with no spread (e.g., ratings fully shrunk to the mean)
  if (diff(xr) == 0) xr <- xr + c(-0.5, 0.5) * max(abs(xr[1]), 1)
  if (diff(yr) == 0) yr <- yr + c(-0.5, 0.5) * max(abs(yr[1]), 1)
  xlo <- xr[1] - expand * diff(xr); xhi <- xr[2] + expand * diff(xr)
  ylo <- yr[1] - expand * diff(yr); yhi <- yr[2] + expand * diff(yr)
  
  to_u   <- function(v) (v - xlo) / (xhi - xlo)
  to_v   <- function(v) { t <- (v - ylo) / (yhi - ylo); if (reverse_y) 1 - t else t }
  from_u <- function(u) xlo + u * (xhi - xlo)
  from_v <- function(v) { t <- if (reverse_y) 1 - v else v; ylo + t * (yhi - ylo) }
  
  pu <- to_u(x); pv <- to_v(y)
  obs_r <- rep(point_r, length(pu))
  if (!is.null(avoid) && nrow(avoid) > 0) {
    pu <- c(pu, to_u(avoid$x)); pv <- c(pv, to_v(avoid$y))
    obs_r <- c(obs_r, rep(avoid_r, nrow(avoid)))
  }
  mu <- to_u(x_mean); mv <- to_v(y_mean)
  
  quads <- tribble(
    ~quadrant,      ~u0,                ~u1,                ~v0,                ~v1,
    "Top-Right",    mu + center_buffer, 1 - edge_buffer,    mv + center_buffer, 1 - edge_buffer,
    "Top-Left",     edge_buffer,        mu - center_buffer, mv + center_buffer, 1 - edge_buffer,
    "Bottom-Right", mu + center_buffer, 1 - edge_buffer,    edge_buffer,        mv - center_buffer,
    "Bottom-Left",  edge_buffer,        mu - center_buffer, edge_buffer,        mv - center_buffer
  )
  
  size_for <- function(val, q) if (!is.null(names(val)) && q %in% names(val)) val[[q]] else val[[1]]
  
  pmap_dfr(quads, function(quadrant, u0, u1, v0, v1) {
    lw <- size_for(label_w, quadrant); lh <- size_for(label_h, quadrant)
    
    # range of label centers that keep the whole box inside the quadrant
    cu_rng <- c(u0 + lw / 2, u1 - lw / 2)
    cv_rng <- c(v0 + lh / 2, v1 - lh / 2)
    fits <- cu_rng[1] <= cu_rng[2] && cv_rng[1] <= cv_rng[2]
    if (cu_rng[1] > cu_rng[2]) cu_rng <- rep(mean(c(u0, u1)), 2)
    if (cv_rng[1] > cv_rng[2]) cv_rng <- rep(mean(c(v0, v1)), 2)
    
    cand <- expand_grid(cu = seq(cu_rng[1], cu_rng[2], length.out = grid_n),
                        cv = seq(cv_rng[1], cv_rng[2], length.out = grid_n)) |>
      distinct()
    
    score <- map2_dfr(cand$cu, cand$cv, function(cu, cv) {
      gap_u <- abs(pu - cu) - lw / 2 - obs_r
      gap_v <- abs(pv - cv) - lh / 2 - obs_r
      tibble(
        hits      = sum(gap_u < 0 & gap_v < 0),
        clearance = if (length(pu)) min(pmax(gap_u, gap_v)) else 1
      )
    })
    
    best <- bind_cols(cand, score) |>
      mutate(
        clearance_capped = pmin(clearance, 0.08),          # beyond this, "open enough"
        outward          = abs(cu - mu) + abs(cv - mv)     # prefer away from the mean lines
      ) |>
      arrange(hits, desc(clearance_capped), desc(outward)) |>
      slice(1)
    
    tibble(quadrant = quadrant, x = from_u(best$cu), y = from_v(best$cv),
           hits = best$hits, fits = fits)
  })
}

## Rough label box size in panel units, from the label text ----
# Tuned for size-3 bold text on an ~11.5 x 7 inch plot; scale with label_size.
estimate_label_box <- function(labels, label_size = 3, char_w = 0.0075, line_h = 0.03, pad = 0.02) {
  lines <- str_split(labels, "\n")
  scale <- label_size / 3
  tibble(
    w = map_dbl(lines, \(l) max(nchar(l))) * char_w * scale + pad,
    h = map_int(lines, length) * line_h * scale + pad
  )
}

## Logo scatter with mean lines and optional quadrant labels ----
# quadrant_labels: tibble(quadrant, label, color), where quadrant matches the
#   names returned by find_quadrant_whitespace() ("Top-Right", "Bottom-Left", ...).
# watermark_pos: c(x, y) as fractions of the visible plot, measured from the
#   left and the bottom, so it lands in the same corner whether or not y is reversed.
plot_logo_quadrant <- function(df, x, y, title, subtitle = NULL, x_lab = x, y_lab = y,
                               caption = "Data: cfbfastR, ggplot2, ggpath",
                               reverse_y = TRUE, pct = FALSE,
                               quadrant_labels = NULL, label_alpha = 1, label_size = 3,
                               logo_width = 0.025, watermark_pos = c(0.97, 0.03)) {
  df <- df |> filter(!is.na(.data[[x]]), !is.na(.data[[y]]))
  x_mean <- mean(df[[x]])
  y_mean <- mean(df[[y]])
  
  lab_fun <- if (pct) scales::percent_format(accuracy = 1) else waiver()
  y_scale <- if (reverse_y) scale_y_reverse(labels = lab_fun) else scale_y_continuous(labels = lab_fun)
  wm_x    <- rng_pos(df[[x]], watermark_pos[1])
  wm_y    <- rng_pos(df[[y]], if (reverse_y) 1 - watermark_pos[2] else watermark_pos[2])
  
  p <- ggplot(df, aes(x = .data[[x]], y = .data[[y]])) +
    geom_vline(xintercept = x_mean, linetype = "dashed", color = "gray60") +
    geom_hline(yintercept = y_mean, linetype = "dashed", color = "gray60") +
    geom_from_path(aes(path = logo), width = logo_width) +
    add_watermark(wm_x, wm_y) +
    scale_x_continuous(labels = lab_fun) +
    y_scale +
    labs(title = title, subtitle = subtitle, x = x_lab, y = y_lab, caption = caption) +
    theme_rz()
  
  if (!is.null(quadrant_labels)) {
    box <- estimate_label_box(quadrant_labels$label, label_size)
    pos <- find_quadrant_whitespace(
      df = df, x_col = x, y_col = y, x_mean = x_mean, y_mean = y_mean,
      reverse_y = reverse_y,
      label_w = set_names(box$w, quadrant_labels$quadrant),
      label_h = set_names(box$h, quadrant_labels$quadrant),
      avoid = tibble(x = wm_x, y = wm_y)          # keep labels off the watermark
    )
    if (any(pos$hits > 0 & pos$quadrant %in% quadrant_labels$quadrant)) {
      message("Quadrant labels overlap logos in: ",
              paste(pos$quadrant[pos$hits > 0], collapse = ", "),
              ". Try shorter labels or a smaller label_size.")
    }
    lab <- inner_join(quadrant_labels, select(pos, quadrant, lx = x, ly = y), by = "quadrant")
    p <- p +
      geom_text(data = lab, aes(x = lx, y = ly, label = label, colour = color),
                inherit.aes = FALSE, fontface = "bold", size = label_size,
                alpha = label_alpha, hjust = 0.5, vjust = 0.5, lineheight = 0.9) +
      scale_colour_identity()
  }
  p
}

## Print and save ----
# prefix defaults to OUT_PREFIX defined in the calling script's Settings.
save_plot <- function(p, name, width = 11.5, height = 7, prefix = OUT_PREFIX) {
  print(p)
  ggsave(plot = p, path = DIR_OUTPUTS, filename = paste0(prefix, name, ".png"),
         width = width, height = height, bg = "white")
}