# Header ----
#
# Script name: 02_06_cfb_red_zone_game_review.R
#
# Purpose of script: Post-game red zone review, one page per game, for the most
#   recent completed week. Each page grades both teams' red zone game (offense
#   and defense) on the same six measures as the 02_05 preview:
#     1. Header: final score, team colors, both logos, the trip count
#     2. Report card: game value, opponent-adjusted game value, season rating
#        coming in, and a letter grade vs. every FBS offense/defense that week
#     3. Every trip inside the 20: where each trip started and how it ended
#     4. Stock watch: season-to-date FBS percentile before and after the game
#     5. Film-room notes: best and worst grades, trips stopped at the door, biggest mover
#   Pages are saved one PNG each, plus (optionally) one combined PDF for the week.
#
# Location of script:
#
# Author: Japbir Gill, MBA MSEd
#
# Date Created: 2026-11-04
#
# Copyright (c) Japbir Gill, 2026
# Email: Japbir.Gill@gmail.com
#
# Notes ----
# - Only games with at least one Power 5 team (conferences_p5 in 00_00) and two
#   rated FBS teams are included, matching 02_05.
# - Opponent adjustment of a single game: the game value minus the opponent's
#   season-rating effect (its rating minus the FBS average), using the opponent's
#   ratings from BEFORE this game, so the game does not grade itself. In week 1,
#   with no earlier data, the after-game ratings are used instead. Rates are
#   adjusted on their own scale, which is an approximation near 0% or 100%.
# - Grades rank the adjusted game value among every FBS offense (or defense) that
#   played that week: A = top 20%, B, C, D, F = bottom 20%. INC = no red zone
#   trips (or plays), so there was nothing to grade.
# - Stock watch refits the ratings through the week before and through this
#   week with the same penalties (lambdas), as in 02_03's movers, so the change
#   reflects the new game rather than a different amount of shrinkage.
# - One game is a handful of red zone trips. Grades describe what happened; the
#   season ratings move slowly on purpose.
# - Needs the patchwork package for the page layout (install.packages("patchwork")).
#

rm(list = ls())

# Working Directories, Libraries, Functions, Values ----

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("00_00_dirs_libs_funs_vals.R")
source("00_01_cfb_functions.R")
library(patchwork)

# Settings ----
SEASON         <- 2026
SEASON_TYPE    <- "regular"
WEEKS          <- NULL         # NULL = all completed weeks; or e.g. 1:9
REFRESH_WEEKS  <- integer(0)   # weeks to re-pull from the API (e.g., the review week after corrections)
REVIEW_WEEK    <- NULL         # NULL = the most recent week in the data
RZ_LINE        <- 20
GARBAGE_MARGIN <- 28
DOORSTEP_YTG   <- 5            # notes: trips that got to this yard line or closer without a TD
P5_CONFS       <- conferences_p5   # a game qualifies if either team is in one of these
PAGE_TEAMS     <- NULL         # NULL = every qualifying game; or only games involving e.g. c("Ohio State", "Penn State")
GRADE_CUTS     <- c(A = 80, B = 60, C = 40, D = 20)   # weekly percentile floor for each grade (below D = F)
COMBINED_PDF   <- TRUE         # also write every page into one PDF for the week
PAGE_W         <- 9            # page size in inches
PAGE_H         <- 17           # long page: both teams on one page
SEED           <- 2026
OUT_PREFIX     <- "proj_012_02_06_"

CAP_DATA  <- "Data: cfbfastR, ggplot2, ggpath, patchwork"
CAP_RZ    <- "Red zone trip = drive with at least one snap inside the opponent's 20. End-of-half and garbage-time drives excluded."
CAP_ADJ   <- "Opp-adjusted = game value minus the opponent's season effect coming in (its ridge rating minus the FBS average)."
CAP_GRADE <- "Grade = opp-adjusted game value ranked among every FBS offense or defense this week: A top 20%, then B, C, D, F. INC = no trips to grade."
CAP_RIDGE <- "Season ratings: ridge regression with offense, defense, and home terms, refit before and after the week with the same penalty."

COL_OUTCOME <- c("Touchdown" = "#2e7d32", "Field Goal" = "#fbc02d", "No Points" = "#c62828")
COL_GRADE   <- c(A = "#1b5e20", B = "#66bb6a", C = "#e0e0e0", D = "#ff8a65", F = "#b71c1c", INC = "#9e9e9e")
COL_MOVE    <- c("Up" = "#2e7d32", "Down" = "#c62828", "Flat" = "gray55")
COL_DEFAULT <- "#37474f"       # used when a team has no color in the team info

# Script-Specific Helpers ----

## Letter grade from a weekly percentile ----
letter_grade <- function(p) {
  case_when(
    is.na(p)               ~ "INC",
    p >= GRADE_CUTS[["A"]] ~ "A",
    p >= GRADE_CUTS[["B"]] ~ "B",
    p >= GRADE_CUTS[["C"]] ~ "C",
    p >= GRADE_CUTS[["D"]] ~ "D",
    TRUE                   ~ "F"
  )
}

## Season ratings with FBS percentiles, long by team x metric ----
rate_teams <- function(ratings) {
  ratings |>
    add_team_info() |>
    mutate(off_pct = pctile(adj_off), def_pct = pctile(-adj_def), .by = metric) |>
    select(team, metric, adj_off, adj_def, off_pct, def_pct)
}

# Datasets ----
raw   <- load_cfb_season(SEASON, WEEKS, season_type = SEASON_TYPE, overwrite_weeks = REFRESH_WEEKS)
rz    <- build_rz_tables(raw, rz_line = RZ_LINE, garbage_margin = GARBAGE_MARGIN)
trips <- add_poe(rz$prod_rz_drives)
rz$prod_rz_drives <- trips
plays <- build_rz_plays(rz, rz_line = RZ_LINE)

weeks_played <- sort(unique(trips$week))
if (is.null(REVIEW_WEEK)) REVIEW_WEEK <- max(weeks_played)
if (!REVIEW_WEEK %in% weeks_played) stop("No red zone data for week ", REVIEW_WEEK, ".", call. = FALSE)
has_before <- any(weeks_played < REVIEW_WEEK)
wk_label <- paste0("Week ", REVIEW_WEEK)
message(sprintf("Red zone game reviews for %s (season weeks %s)", wk_label, paste(range(weeks_played), collapse = "-")))

## Team colors (team_info in 00_01 keeps only logo and conference) ----
team_info_full <- get_team_info()
team_colors <- tibble(
  team  = team_info_full$school,
  color = clean_color(if ("color" %in% names(team_info_full)) team_info_full$color else NA_character_, COL_DEFAULT)
)

# Processing ----

## Season ratings before and after the week (same penalties) ----
set.seed(SEED)
ratings_after_raw <- fit_rz_ratings(rz$drives_clean, trips, plays, max_week = REVIEW_WEEK)
ratings_after     <- rate_teams(ratings_after_raw)
ratings_before    <- if (has_before) {
  fit_rz_ratings(rz$drives_clean, trips, plays, max_week = REVIEW_WEEK - 1,
                 lambdas = attr(ratings_after_raw, "lambda")) |>
    rate_teams()
} else {
  message("No weeks before ", wk_label, ": opponent adjustment uses after-game ratings; stock watch is skipped.")
  NULL
}
ratings_ref <- if (has_before) ratings_before else ratings_after   # opponent strength coming in

## Opponent effects: each team's rating minus the FBS average ----
opp_effect <- ratings_ref |>
  mutate(off_effect = adj_off - mean(adj_off), def_effect = adj_def - mean(adj_def), .by = metric) |>
  select(team, metric, off_effect, def_effect)

## The week's sides (one offense in one game), long by metric ----
fbs_teams <- unique(ratings_after$team)
week_sides <- rz_side_metrics(rz$drives_clean, trips, plays) |>
  filter(week == REVIEW_WEEK, offense %in% fbs_teams, defense %in% fbs_teams)

side_long <- week_sides |>
  select(game_id, offense, defense, all_of(RZ_METRICS$metric)) |>
  pivot_longer(all_of(RZ_METRICS$metric), names_to = "metric", values_to = "game_val") |>
  # an offense's game, adjusted for the defense it faced (and vice versa)
  left_join(opp_effect |> select(defense = team, metric, def_effect), by = c("defense", "metric")) |>
  left_join(opp_effect |> select(offense = team, metric, off_effect), by = c("offense", "metric")) |>
  mutate(
    adj_for_off = game_val - def_effect,    # offense's game, opponent defense removed
    adj_for_def = game_val - off_effect,    # defense's game (value allowed), opponent offense removed
    off_week_pct = pctile(adj_for_off),
    def_week_pct = pctile(-adj_for_def),
    .by = metric
  ) |>
  mutate(off_grade = letter_grade(off_week_pct), def_grade = letter_grade(def_week_pct))

## Qualifying games and pages ----
team_card <- ratings_after |>
  distinct(team) |>
  add_team_info() |>
  left_join(team_colors, by = "team") |>
  mutate(color = coalesce(color, COL_DEFAULT))

games <- rz$games |>
  filter(week == REVIEW_WEEK) |>
  left_join(team_card |> select(home_team = team, home_conf = conference), by = "home_team") |>
  left_join(team_card |> select(away_team = team, away_conf = conference), by = "away_team") |>
  mutate(
    has_p5 = home_conf %in% P5_CONFS | away_conf %in% P5_CONFS,
    rated  = !is.na(home_conf) & !is.na(away_conf)
  )

message(sprintf("%s: %d games, %d with a P5 team, %d of those with two rated FBS teams",
                wk_label, nrow(games), sum(games$has_p5), sum(games$has_p5 & games$rated)))
games <- games |> filter(has_p5, rated, game_id %in% week_sides$game_id)
if (nrow(games) == 0) stop("No qualifying games for ", wk_label, ".", call. = FALSE)

pages <- games |>
  filter(is.null(PAGE_TEAMS) | home_team %in% PAGE_TEAMS | away_team %in% PAGE_TEAMS) |>
  arrange(across(any_of("start_date")), game_id)

message(sprintf("Building %d game review pages", nrow(pages)))

## Trip log for the week (analysis red zone trips) ----
closest <- rz$pbp |>
  filter(is_scrimmage, !is.na(drive_id)) |>
  summarise(min_ytg = min(yards_to_goal, na.rm = TRUE), .by = drive_id)

trip_log <- trips |>
  filter(week == REVIEW_WEEK) |>
  left_join(closest, by = "drive_id") |>
  mutate(
    outcome = case_when(flag_td == 1 ~ "Touchdown", flag_fg == 1 ~ "Field Goal", TRUE ~ "No Points"),
    seq     = if ("drive_number" %in% names(trips)) drive_number else rank(drive_id)
  ) |>
  select(game_id, offense, defense, start_period, seq, rz_entry_ytg, min_ytg, drive_points, outcome, rz_outcome, xpts)

# Page Builder ----

## Report card rows for one team (its offense and its defense in this game) ----
card_rows <- function(gid, tm, op) {
  before_t <- (if (has_before) ratings_before else ratings_after) |> filter(team == tm)
  bind_rows(
    side_long |>
      filter(game_id == gid, offense == tm) |>
      left_join(before_t |> select(metric, norm = adj_off), by = "metric") |>
      transmute(team = tm, opp = op, side = "Offense", metric, game_val, adj_val = adj_for_off, norm,
                better = adj_val - norm, grade = off_grade),
    side_long |>
      filter(game_id == gid, defense == tm) |>
      left_join(before_t |> select(metric, norm = adj_def), by = "metric") |>
      transmute(team = tm, opp = op, side = "Defense", metric, game_val, adj_val = adj_for_def, norm,
                better = norm - adj_val, grade = def_grade)
  )
}

## Stock watch rows for one team ----
stock_rows <- function(tm) {
  inner_join(
    ratings_before |> filter(team == tm) |> select(metric, off_b = off_pct, def_b = def_pct),
    ratings_after  |> filter(team == tm) |> select(metric, off_a = off_pct, def_a = def_pct),
    by = "metric"
  ) |>
    pivot_longer(-metric, names_to = c("side", ".value"), names_pattern = "(off|def)_(.)") |>
    mutate(team = tm, side = if_else(side == "off", "Offense", "Defense"))
}

## One page per game: away team first, home team second ----
build_review_page <- function(gid, away, home, neutral, pts_away, pts_home) {
  card_a <- team_card |> filter(team == away)
  card_h <- team_card |> filter(team == home)
  col_a  <- card_a$color
  col_h  <- card_h$color
  if (col_a == col_h) col_h <- COL_DEFAULT
  teams  <- c(away, home)
  unit_levels <- paste(rep(teams, each = 2), c("Offense", "Defense"))

  side_of <- function(tm) week_sides |> filter(game_id == gid, offense == tm)
  side_a <- side_of(away)
  side_h <- side_of(home)

  ## Header ----
  trip_txt <- function(tm, s) sprintf("%s %d trip%s (%d TD, %d FG, %d empty)", tm, s$rz_trips,
                                      if_else(s$rz_trips == 1, "", "s"), s$tds, s$fgs, s$empty)
  header <- page_header(
    col_a, col_h, card_a$logo, card_h$logo,
    line1 = sprintf("%s %s, %s %s", str_to_upper(away), pts_away, str_to_upper(home), pts_home),
    line2 = paste(wk_label, SEASON, "\u00b7 Red Zone Game Review", if (neutral %in% TRUE) "\u00b7 Neutral Site" else ""),
    line3 = paste("Inside the 20:", trip_txt(away, side_a), "\u00b7", trip_txt(home, side_h))
  )

  ## Report card ----
  card <- bind_rows(card_rows(gid, away, home), card_rows(gid, home, away)) |>
    left_join(RZ_METRICS |> select(metric, short, unit_type = unit), by = "metric") |>
    mutate(
      unit  = factor(paste(team, side), levels = unit_levels),
      short = factor(short, levels = rev(RZ_METRICS$short)),
      vs_norm = case_when(is.na(better) ~ "-",
                          unit_type == "pct" ~ sprintf("%+.0f pts", 100 * better),
                          TRUE ~ sprintf("%+.2f", better)),
      tone = case_when(is.na(better) ~ "none", better > 0 ~ "up", better < 0 ~ "down", TRUE ~ "none")
    )
  card_cells <- card |>
    select(unit, short, unit_type, game_val, adj_val, norm, vs_norm, tone) |>
    mutate(across(c(game_val, adj_val, norm), \(v) fmt_metric(v, unit_type))) |>
    pivot_longer(c(game_val, adj_val, norm, vs_norm), names_to = "col", values_to = "txt") |>
    mutate(col = factor(col, levels = c("game_val", "adj_val", "norm", "vs_norm"),
                        labels = c("Game", "Opp-Adj.", if (has_before) "Coming In" else "Season", "vs. Norm")),
           fill = if_else(col == "vs. Norm", tone, "none"))

  p_card <- ggplot(card_cells, aes(x = col, y = short)) +
    geom_tile(aes(fill = fill), color = "white", linewidth = 1.2, width = 0.96, height = 0.9) +
    geom_text(aes(label = txt), size = 3, color = "gray10") +
    geom_tile(data = card, aes(x = 5, y = short, fill = grade), inherit.aes = FALSE,
              color = "white", linewidth = 1.2, width = 0.8, height = 0.9) +
    geom_text(data = card, aes(x = 5, y = short, label = grade,
                               color = if_else(grade %in% c("A", "F"), "white", "black")),
              inherit.aes = FALSE, fontface = "bold", size = 4.6) +
    facet_wrap(~ unit, ncol = 2, drop = FALSE) +
    scale_fill_manual(values = c(none = "gray96", up = "#c8e6c9", down = "#ffcdd2", COL_GRADE), guide = "none") +
    scale_color_identity() +
    scale_x_discrete(position = "top", limits = c(levels(card_cells$col), "Grade"), expand = expansion(add = 0.5)) +
    labs(
      title = "Report Card",
      subtitle = "This game, the same game adjusted for the opponent, the season rating coming in, and a grade vs. this week's FBS units",
      x = NULL, y = NULL
    ) +
    theme_rz(base_size = 10, subtitle_size = 9) +
    theme(plot.title = element_text(size = 13), panel.grid = element_blank(),
          strip.text = element_text(face = "bold", size = 11), axis.text.y = element_text(face = "bold", size = 9),
          axis.text.x.top = element_text(face = "bold", size = 8.5), panel.spacing.y = unit(1, "lines"))

  ## Every trip inside the 20 ----
  tl <- trip_log |>
    filter(game_id == gid) |>
    mutate(side = factor(paste(offense, "ball"), levels = paste(teams, "ball"))) |>
    arrange(seq) |>
    mutate(trip_no = row_number(), .by = side)
  n_max <- max(4, count(tl, side)$n)

  p_trips <- ggplot(tl, aes(x = trip_no, y = rz_entry_ytg)) +
    geom_hline(yintercept = DOORSTEP_YTG, linetype = "dotted", color = "gray50") +
    # bar = how far the trip got (entry to its closest snap)
    geom_linerange(aes(ymin = pmin(min_ytg, rz_entry_ytg), ymax = rz_entry_ytg, color = outcome), linewidth = 2.2, alpha = 0.35) +
    geom_point(aes(fill = outcome), shape = 21, size = 7, color = "white", stroke = 0.8) +
    geom_text(aes(label = drive_points), color = "white", fontface = "bold", size = 3) +
    geom_text(aes(label = paste0("Q", pmin(start_period, 5) |> as.character() |> str_replace("5", "OT"))),
              nudge_y = -2.6, size = 2.4, color = "gray35") +
    facet_wrap(~ side, nrow = 1, drop = FALSE) +
    scale_fill_manual(values = COL_OUTCOME, drop = FALSE) +
    scale_colour_manual(values = COL_OUTCOME, guide = "none") +
    scale_x_continuous(limits = c(0.4, n_max + 0.6), breaks = 1:n_max) +
    scale_y_reverse(limits = c(21, -2), breaks = c(20, 10, DOORSTEP_YTG, 1)) +
    labs(title = "Every Trip Inside the 20",
         subtitle = "Dot = where the trip first reached the 20 (number = points); bar = how close it got",
         x = "Red Zone Trip", y = "Entry: Yards to Goal", fill = NULL) +
    theme_rz(base_size = 10, subtitle_size = 9) +
    theme(plot.title = element_text(size = 13), legend.position = "top",
          panel.grid.major.x = element_blank(), strip.text = element_text(face = "bold", size = 11))
  if (nrow(tl) == 0) {
    p_trips <- p_trips + annotate("text", x = 2.5, y = 10, label = "No red zone trips either way", size = 4)
  }

  ## Stock watch ----
  if (has_before) {
    stock <- bind_rows(stock_rows(away), stock_rows(home)) |>
      left_join(RZ_METRICS |> select(metric, short), by = "metric") |>
      mutate(
        unit  = factor(paste(team, side), levels = unit_levels),
        short = factor(short, levels = rev(RZ_METRICS$short)),
        move  = case_when(a - b >= 1 ~ "Up", a - b <= -1 ~ "Down", TRUE ~ "Flat")
      )
    p_stock <- ggplot(stock, aes(y = short)) +
      geom_vline(xintercept = 50, linetype = "dashed", color = "gray70") +
      geom_point(aes(x = b), color = "gray60", size = 2.2) +
      geom_segment(aes(x = b, xend = a, yend = short, color = move), linewidth = 1,
                   arrow = arrow(length = unit(0.09, "inches"), type = "closed")) +
      geom_text(aes(x = a, label = sprintf("%+d", round(a - b)), color = move,
                    hjust = if_else(a >= b, -0.35, 1.35)), size = 2.7, fontface = "bold") +
      facet_wrap(~ unit, ncol = 2) +
      scale_color_manual(values = COL_MOVE, guide = "none") +
      scale_x_continuous(limits = c(-8, 108), breaks = c(0, 25, 50, 75, 100)) +
      labs(title = "Stock Watch",
           subtitle = paste0("Season FBS percentile before (gray) and after ", wk_label),
           x = "FBS Percentile (100 = best / stingiest)", y = NULL) +
      theme_rz(base_size = 10, subtitle_size = 9) +
      theme(plot.title = element_text(size = 13), panel.grid.major.y = element_blank(),
            strip.text = element_text(face = "bold", size = 11), axis.text.y = element_text(size = 8.5))
  } else {
    stock <- NULL
    p_stock <- ggplot() +
      annotate("text", x = 0, y = 0, label = "First week of the season: no before-and-after ratings yet", size = 4) +
      labs(title = "Stock Watch") +
      theme_void() + theme(plot.title = element_text(size = 13, face = "bold"))
  }

  ## Film-room notes ----
  grade_rank <- function(g) match(g, c("F", "D", "C", "B", "A"))
  cash_line <- function(tm, s) {
    if (s$rz_trips == 0) return(sprintf("%s never reached the red zone, which is its own kind of report card.", tm))
    sprintf("%s turned %d of %d red zone trip%s into touchdowns: %s points per trip, %s vs. expected per trip given where the trips started.",
            tm, s$tds, s$rz_trips, if_else(s$rz_trips == 1, "", "s"),
            fmt_metric(s$points, "num"), fmt_metric(s$poe, "signed"))
  }
  grade_line <- function(tm) {
    g <- card |> filter(team == tm, grade != "INC")
    if (nrow(g) == 0) return(NULL)
    best  <- g |> slice_max(grade_rank(grade) * 1000 + better, n = 1, with_ties = FALSE)
    worst <- g |> slice_min(grade_rank(grade) * 1000 + better, n = 1, with_ties = FALSE)
    sprintf("%s report card: best was %s %s (%s, %s vs. norm); needs work: %s %s (%s, %s).",
            tm, str_to_lower(best$side), best$short, best$grade, best$vs_norm,
            str_to_lower(worst$side), worst$short, worst$grade, worst$vs_norm)
  }
  stopped <- function(tm) nrow(tl |> filter(offense == tm, min_ytg <= DOORSTEP_YTG, outcome != "Touchdown"))
  n_stop <- set_names(map_int(teams, stopped), teams)
  door_line <- if (sum(n_stop) == 0) {
    sprintf("No trips stalled inside the %d for either team.", DOORSTEP_YTG)
  } else {
    sprintf("Knocking at the door (reached the %d, no TD): %s %d time%s, %s %d time%s.", DOORSTEP_YTG,
            away, n_stop[[away]], if_else(n_stop[[away]] == 1, "", "s"),
            home, n_stop[[home]], if_else(n_stop[[home]] == 1, "", "s"))
  }
  stock_line <- if (!is.null(stock)) {
    mv <- stock |> slice_max(abs(a - b), n = 1, with_ties = FALSE)
    if (abs(mv$a - mv$b) < 1) "Stock watch: the season ratings barely budged, which is what one game usually does."
    else sprintf("Biggest stock move: %s %s %s, %s to %s percentile.", mv$team, str_to_lower(mv$side), mv$short,
                 ordinal(mv$b), ordinal(mv$a))
  }

  notes <- c(
    cash_line(away, side_a), cash_line(home, side_h),
    grade_line(away), grade_line(home),
    door_line, stock_line,
    "One game is only a few trips, so a grade describes this game; it is not a new verdict on either team."
  )
  p_notes <- notes_panel(notes, "Film-Room Notes")

  ## Assemble ----
  header / p_card / p_trips / p_stock / p_notes +
    plot_layout(heights = c(1.15, 4.6, 2.6, if (has_before) 3.6 else 0.6, 2.1)) +
    plot_annotation(
      caption = paste(CAP_DATA, CAP_RZ, CAP_ADJ, CAP_GRADE, CAP_RIDGE,
                      "vs. Norm: + = better than the season rating coming in (for a defense, allowed less). Rates in percentage points.",
                      sep = "\n"),
      theme = theme(plot.caption = element_text(hjust = 0, size = 7.5, color = "gray30"))
    )
}

# Build and Save Pages ----
page_files <- character(0)
page_list  <- vector("list", nrow(pages))
for (i in seq_len(nrow(pages))) {
  pg <- pages[i, ]
  p  <- build_review_page(pg$game_id, pg$away_team, pg$home_team, pg$neutral_site, pg$away_points, pg$home_points)
  page_list[[i]] <- p
  name <- paste0("week", REVIEW_WEEK, "_review_", slug(pg$away_team), "_at_", slug(pg$home_team))
  save_plot(p, name, width = PAGE_W, height = PAGE_H)
  page_files <- c(page_files, paste0(OUT_PREFIX, name, ".png"))
}

if (COMBINED_PDF && length(page_list) > 0) {
  pdf_path <- file.path(DIR_OUTPUTS, paste0(OUT_PREFIX, "week", REVIEW_WEEK, "_reviews.pdf"))
  grDevices::cairo_pdf(pdf_path, width = PAGE_W, height = PAGE_H, onefile = TRUE)
  walk(page_list, print)
  dev.off()
  message("Combined PDF: ", pdf_path)
}

write_csv(side_long, file.path(DIR_OUTPUTS, paste0(OUT_PREFIX, "week", REVIEW_WEEK, "_grades.csv")))

# Save Results ----
saveRDS(
  list(
    season         = SEASON,
    review_week    = REVIEW_WEEK,
    weeks_played   = weeks_played,
    thresholds     = list(grade_cuts = GRADE_CUTS, doorstep_ytg = DOORSTEP_YTG),
    p5_confs       = P5_CONFS,
    metrics        = RZ_METRICS,
    ratings_before = ratings_before,
    ratings_after  = ratings_after,
    side_long      = side_long,
    week_sides     = week_sides,
    games          = games,
    pages          = pages |> mutate(file = page_files),
    lambdas        = attr(ratings_after_raw, "lambda")
  ),
  file.path(DIR_OUTPUTS, paste0(OUT_PREFIX, "results.rds"))
)
