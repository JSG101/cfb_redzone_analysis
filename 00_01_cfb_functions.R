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
#     DIR_OUTPUTS, watermark_path, find_quadrant_whitespace()
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
CFB_CACHE_DIR <- if (exists("DIR_INPUTS")) file.path(DIR_INPUTS, "cfbd_cache") else file.path(DIR_OUTPUTS, "cfbd_cache")
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
fit_team_ridge <- function(df, y_col, family = c("gaussian", "binomial")) {
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
  
  fit <- cv.glmnet(X, df[[y_col]], family = family, alpha = 0, standardize = FALSE, lambda = lambda_grid)
  cf  <- coef(fit, s = "lambda.min")
  cf  <- tibble(term = rownames(cf), coef = as.numeric(cf))
  
  intercept <- cf$coef[cf$term == "(Intercept)"]
  link_inv  <- if (family == "binomial") plogis else identity
  
  off_tbl <- cf |>
    filter(str_starts(term, "off::")) |>
    transmute(team = str_remove(term, "^off::"), off_coef = coef)
  def_tbl <- cf |>
    filter(str_starts(term, "def::")) |>
    transmute(team = str_remove(term, "^def::"), def_coef = coef)
  
  full_join(off_tbl, def_tbl, by = "team") |>
    mutate(
      adj_off = link_inv(intercept + off_coef),
      adj_def = link_inv(intercept + def_coef)
    )
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
    pos <- find_quadrant_whitespace(df = df, x_col = x, y_col = y, x_mean = x_mean, y_mean = y_mean) |>
      as_tibble() |>
      select(quadrant, lx = x, ly = y)
    lab <- inner_join(quadrant_labels, pos, by = "quadrant")
    p <- p +
      geom_text(data = lab, aes(x = lx, y = ly, label = label, colour = color),
                inherit.aes = FALSE, fontface = "bold", size = label_size,
                alpha = label_alpha, hjust = 0.5, vjust = 0) +
      scale_colour_identity()
  }
  p
}

## Print and save ----
# prefix defaults to OUT_PREFIX defined in the calling script's Settings.
save_plot <- function(p, name, width = 11.5, height = 7, prefix = OUT_PREFIX) {
  print(p)
  ggsave(plot = p, path = DIR_OUTPUTS, filename = paste0(prefix, name, ".png"),
         width = width, height = height)
}