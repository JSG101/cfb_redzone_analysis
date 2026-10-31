# Header ----
#
# Script name: 02_01_cfb_red_zone_success.R
#
# Purpose of script: Answer the questions: how often do teams get to (and allow) the red zone,
#   and for teams that make the red zone, how successful are they at scoring?
#
# Location of script:
#
# Author: Japbir Gill, MBA MSEd
#
# Date Created: 2026-09-17
# Date Revised: 2026-10-11 (refactored to use 00_01_cfb_functions.R)
#               2026-10-31 (added Plot 0: red zone trip tendencies and red zone vs. non-red zone scoring)
#
# Copyright (c) Japbir Gill, 2026
# Email: Japbir.Gill@gmail.com
#
# Notes ----
# https://cloud.r-project.org/web/packages/cfbfastR/refman/cfbfastR.html#cfbfastR-package
#
# - Red zone trip = a drive with at least one snap at or inside the opponent's 20
#   (play-by-play definition). Long TDs from outside the 20 no longer count, and
#   drives that reached the 20 and then backed out now do.
# - Drive points are the actual score change on the drive (not a flat 7/3).
# - End-of-half/game drives and 2nd-half garbage time are excluded.
# - Adjusted values come from ridge regression with full offense/defense
#   indicators and a home-field term: neutral site vs. an average opponent.
# - Data loads from the weekly cache; only weeks not yet saved hit the API.
# - Plot 0 uses every analysis drive (rz$drives_clean), not just red zone trips.
#   Trip rates are per drive rather than per game, so tempo doesn't inflate them.
#   A non-red zone drive is any analysis drive with no snap inside the 20, so
#   long TDs from outside the 20 count there.
#

rm(list = ls())

# Working Directories, Libraries, Functions, Values ----

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("00_00_dirs_libs_funs_vals.R")
source("00_01_cfb_functions.R")

# Settings ----
SEASON         <- 2026
WEEKS          <- NULL          # NULL = all completed weeks; or e.g. 1:7
REFRESH_WEEKS  <- integer(0)    # weeks to re-pull from the API, e.g. 7
RZ_LINE        <- 20
GARBAGE_MARGIN <- 28            # 2nd-half drives starting with |score diff| > this are excluded
OUT_PREFIX     <- "proj_012_02_01_"

# Caption text reused across plots
CAP_DATA  <- "Data: cfbfastR, ggplot2, ggpath"
CAP_RZ    <- "Red zone trip = drive with at least one snap inside the opponent's 20. End-of-half and garbage-time drives excluded."
CAP_RIDGE <- "Ridge regression (L2) on 2026 red zone trips with offense, defense, and home-field terms; neutral site vs. an average opponent."
CAP_NONRZ <- "Non-red zone drive = drive with no snap inside the opponent's 20 (includes long TDs from outside the 20)."

# Plot 0 quadrant labels: offense = points per drive (x), y = share of those points from red zone trips
LABELS_SOURCE_OFF <- tribble(
  ~quadrant,      ~label,                         ~color,
  "Top-Right",    "Methodical\nMarchers",         "#1b5e20",
  "Bottom-Right", "Home Run\nHitters",            "#0d47a1",
  "Top-Left",     "Inch by Inch,\nRarely a Mile", "#e65100",
  "Bottom-Left",  "Hail Mary\nor Bust",           "#4a148c"
)

# Plot 0 quadrant labels: defense = points allowed per drive (x, lower is better)
LABELS_SOURCE_DEF <- tribble(
  ~quadrant,      ~label,                            ~color,
  "Top-Left",     "Make 'Em\nEarn It",               "#1b5e20",
  "Bottom-Left",  "Stingy, but the\nOdd Deep Shot",  "#0d47a1",
  "Top-Right",    "Death by a\nThousand Cuts",       "#e65100",
  "Bottom-Right", "Burned\nDeep",                    "#b71c1c"
)

# Quadrant labels (offense on x, defense inverted on y, so Top-Right = good on both)
LABELS_TAILGATE <- tribble(
  ~quadrant,      ~label,                            ~color,
  "Top-Right",    "Stress-Free\nTailgating",         "#1b5e20",
  "Top-Left",     "Throwing Pillows\nat the TV",     "#e65100",
  "Bottom-Right", "Cardiac\nArrest\nFootball",       "#b71c1c",
  "Bottom-Left",  "Changing to the\nNature Channel", "#4a148c"
)

LABELS_ELITE <- tribble(
  ~quadrant,     ~label,                           ~color,
  "Top-Right",   "Elite Teams",                    "forestgreen",
  "Bottom-Left", "Struggling\nRed Zone Teams",     "firebrick"
)

# Script-Specific Helpers ----

## Red zone summary for one side of the ball ----
summarise_rz_side <- function(df, side) {
  df |>
    rename(team = all_of(side)) |>
    summarise(
      trips      = n(),
      points     = sum(drive_points),
      avg_points = mean(drive_points),
      scores     = sum(flag_score),
      tds        = sum(flag_td),
      .by = team
    ) |>
    mutate(
      pct_score = scores / trips,
      pct_td    = tds / trips
    )
}

## All-drive summary for one side of the ball (Plot 0) ----
# Every analysis drive, split into red zone trips and non-red zone drives.
summarise_drive_side <- function(df, side) {
  df |>
    rename(team = all_of(side)) |>
    summarise(
      drives     = n(),
      rz_trips   = sum(rz_trip),
      pts_total  = sum(drive_points),
      pts_rz     = sum(drive_points[rz_trip]),
      pts_non_rz = sum(drive_points[!rz_trip]),
      .by = team
    ) |>
    mutate(
      trip_rate       = rz_trips / drives,
      ppd             = pts_total / drives,
      ppd_rz          = if_else(rz_trips > 0, pts_rz / rz_trips, NA_real_),
      ppd_non_rz      = if_else(drives > rz_trips, pts_non_rz / (drives - rz_trips), NA_real_),
      share_pts_rz    = if_else(pts_total > 0, pts_rz / pts_total, NA_real_)
    )
}

## Mean-centered net bar chart ----
plot_net_bar <- function(df, col, y_lab, subtitle, caption) {
  ggplot(df, aes(x = reorder(team, .data[[col]]), y = .data[[col]], fill = .data[[col]])) +
    geom_col(show.legend = FALSE, alpha = 0.65) +
    geom_from_path(aes(path = logo, vjust = ifelse(.data[[col]] >= 0, -0.2, 1.2)), height = 0.025) +
    add_watermark(nrow(df) - 3, min(df[[col]]) * 0.98) +
    labs(title = "Mean-Centered Average Net Red Zone Points, by Team",
         subtitle = subtitle, y = y_lab, caption = caption) +
    theme_rz() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    scale_fill_viridis_c(option = "turbo")
}

# Datasets ----
raw <- load_cfb_season(SEASON, WEEKS, overwrite_weeks = REFRESH_WEEKS)
rz  <- build_rz_tables(raw, rz_line = RZ_LINE, garbage_margin = GARBAGE_MARGIN)
prod_game_drives <- rz$prod_rz_drives

# Processing ----

## Opponent-adjusted ratings ----
ratings_points <- fit_team_ridge(prod_game_drives, "drive_points", family = "gaussian") |>
  select(team, adj_off_points = adj_off, adj_def_points = adj_def)

ratings_tds <- fit_team_ridge(prod_game_drives, "flag_td", family = "binomial") |>
  select(team, adj_off_td_pct = adj_off, adj_def_td_pct = adj_def)

## Games played (for per-game rates) ----
games_played <- rz$drives |>
  summarise(games = n_distinct(game_id), .by = offense) |>
  rename(team = offense)

## Team-level table: one row per FBS team ----
team_rz <- full_join(
  summarise_rz_side(prod_game_drives, "offense") |> rename_with(\(x) paste0(x, "_off"), -team),
  summarise_rz_side(prod_game_drives, "defense") |> rename_with(\(x) paste0(x, "_def"), -team),
  by = "team"
) |>
  left_join(games_played, by = "team") |>
  left_join(ratings_points, by = "team") |>
  left_join(ratings_tds, by = "team") |>
  add_team_info() |>
  filter(!is.na(trips_off), !is.na(trips_def)) |>
  mutate(
    # volume
    trips_per_game_off = trips_off / games,
    # TD rate shrunk toward the FBS average (small-sample correction)
    pct_td_off_shrunk  = eb_shrink(tds_off, trips_off),
    # net points, raw and adjusted, centered on the FBS average
    net_team_points                 = avg_points_off - avg_points_def,
    net_team_points_centralized     = net_team_points - mean(net_team_points),
    net_team_points_adj             = adj_off_points - adj_def_points,
    net_team_points_adj_centralized = net_team_points_adj - mean(net_team_points_adj),
    # TD ratio; undefined when a defense has allowed 0 red zone TDs
    ratio_td_drives = if_else(tds_def > 0, tds_off / tds_def, NA_real_)
  )

## Red zone trip tendencies, all drives (Plot 0) ----
drives_all <- rz$drives_clean

# schedule-adjusted chance a drive reaches the red zone
ratings_access <- fit_team_ridge(drives_all |> mutate(rz_trip = as.integer(rz_trip)),
                                 "rz_trip", family = "binomial") |>
  select(team, adj_off_trip_rate = adj_off, adj_def_trip_rate = adj_def)

team_trip <- full_join(
  summarise_drive_side(drives_all, "offense") |> rename_with(\(x) paste0(x, "_off"), -team),
  summarise_drive_side(drives_all, "defense") |> rename_with(\(x) paste0(x, "_def"), -team),
  by = "team"
) |>
  left_join(games_played, by = "team") |>
  left_join(ratings_access, by = "team") |>
  add_team_info() |>
  filter(!is.na(drives_off), !is.na(drives_def)) |>
  mutate(
    trips_per_game_off = rz_trips_off / games,
    trips_per_game_def = rz_trips_def / games
  )

# league by week: how often drives get there, and what each kind of drive is worth
league_trip_week <- drives_all |>
  summarise(
    drives       = n(),
    rz_trips     = sum(rz_trip),
    trip_rate    = mean(rz_trip),
    ppd_rz       = mean(drive_points[rz_trip]),
    sd_rz        = sd(drive_points[rz_trip]),
    ppd_non_rz   = mean(drive_points[!rz_trip]),
    sd_non_rz    = sd(drive_points[!rz_trip]),
    share_pts_rz = sum(drive_points[rz_trip]) / sum(drive_points),
    .by = week
  ) |>
  arrange(week)

## Summary tables ----
print(league_trip_week, n = 20)
print(team_trip |> arrange(desc(trip_rate_off)) |>
        select(team, drives_off, rz_trips_off, trip_rate_off, adj_off_trip_rate, ppd_off, share_pts_rz_off), n = 25)
print(team_trip |> arrange(trip_rate_def) |>
        select(team, drives_def, rz_trips_def, trip_rate_def, adj_def_trip_rate, ppd_def, share_pts_rz_def), n = 25)
print(team_rz |> arrange(desc(avg_points_off)) |>
        select(team, trips_off, points_off, avg_points_off, adj_off_points), n = 50)
print(team_rz |> arrange(avg_points_def) |>
        select(team, trips_def, points_def, avg_points_def, adj_def_points), n = 50)

# Plot 0: Getting to the Red Zone ----

## Offense vs defense red zone trip rate ----
p <- plot_logo_quadrant(
  team_trip, "trip_rate_off", "trip_rate_def",
  title    = "Who Gets to the Red Zone, and Who Lets Opponents In?",
  subtitle = "Share of Drives Reaching the Red Zone on Offense vs. Allowed on Defense, FBS 2026 Season To Date",
  x_lab    = "Offensive Drives Reaching the Red Zone",
  y_lab    = "Opponent Drives Reaching the Red Zone (Inverted)",
  caption  = paste(CAP_DATA, CAP_RZ, "Rates are per drive, so fast- and slow-paced teams compare evenly.", sep = "\n"),
  pct = TRUE, quadrant_labels = LABELS_TAILGATE
)
save_plot(p, "scatter_offense_defense_trip_rate")

## Offense vs defense red zone trip rate, adjusted ----
p <- plot_logo_quadrant(
  team_trip, "adj_off_trip_rate", "adj_def_trip_rate",
  title    = "Who Gets to the Red Zone, and Who Lets Opponents In?",
  subtitle = "Share of Drives Reaching the Red Zone, Adjusted for Schedule Strength",
  x_lab    = "Adjusted Offensive Drives Reaching the Red Zone",
  y_lab    = "Adjusted Opponent Drives Reaching the Red Zone (Inverted)",
  caption  = str_wrap(
    paste(CAP_DATA, CAP_RZ,
          "Ridge regression (L2) on all 2026 drives with offense, defense, and home-field terms; values are the probability a drive reaches the red zone at a neutral site vs. an average opponent."),
    130),
  pct = TRUE, quadrant_labels = LABELS_TAILGATE
)
save_plot(p, "scatter_offense_defense_trip_rate_adj")

## League trend: how often drives get there, and how much scoring runs through it ----
league_trip_long <- bind_rows(
  league_trip_week |>
    transmute(week, series = "Drives Reaching the Red Zone", value = trip_rate,
              se = sqrt(trip_rate * (1 - trip_rate) / drives)),
  league_trip_week |>
    transmute(week, series = "Points Scored on Red Zone Trips", value = share_pts_rz, se = NA_real_)
)

p <- ggplot(league_trip_long, aes(x = week, y = value, color = series, fill = series)) +
  geom_ribbon(data = \(d) filter(d, !is.na(se)),
              aes(ymin = value - 1.96 * se, ymax = value + 1.96 * se), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  add_watermark(rng_pos(league_trip_long$week, 0.98), rng_pos(league_trip_long$value, 0.5)) +
  scale_x_continuous(breaks = league_trip_week$week) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("Drives Reaching the Red Zone" = "#336699",
                                "Points Scored on Red Zone Trips" = "#2e7d32"),
                     aesthetics = c("color", "fill")) +
  labs(
    title = "Is Everyone Still Knocking on the Door?",
    subtitle = "FBS share of drives reaching the red zone, and share of all points scored on those trips, by week",
    x = "Week", y = "Share", color = NULL, fill = NULL,
    caption = paste(CAP_DATA, CAP_RZ,
                    "Shaded band = normal week-to-week noise in the trip rate.", sep = "\n")
  ) +
  theme_rz(subtitle_size = 13) +
  theme(legend.position = "top")
save_plot(p, "line_league_trip_rate_by_week")

## League trend: points per drive, red zone trips vs non-red zone drives ----
league_ppd_long <- bind_rows(
  league_trip_week |>
    transmute(week, series = "Red Zone Trips", value = ppd_rz, se = sd_rz / sqrt(rz_trips)),
  league_trip_week |>
    transmute(week, series = "Non-Red Zone Drives", value = ppd_non_rz, se = sd_non_rz / sqrt(drives - rz_trips))
) |>
  mutate(series = factor(series, levels = c("Red Zone Trips", "Non-Red Zone Drives")))

p <- ggplot(league_ppd_long, aes(x = week, y = value, color = series, fill = series)) +
  geom_ribbon(aes(ymin = value - 1.96 * se, ymax = value + 1.96 * se), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  add_watermark(rng_pos(league_ppd_long$week, 0.98), rng_pos(league_ppd_long$value, 0.5)) +
  scale_x_continuous(breaks = league_trip_week$week) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values = c("Red Zone Trips" = "#2e7d32", "Non-Red Zone Drives" = "#c62828"),
                     aesthetics = c("color", "fill")) +
  labs(
    title = "Location, Location, Location",
    subtitle = "FBS points per drive on red zone trips vs. drives that never reach the 20, by week",
    x = "Week", y = "Points per Drive", color = NULL, fill = NULL,
    caption = paste(CAP_DATA, CAP_RZ, CAP_NONRZ,
                    "Shaded band = normal week-to-week noise.", sep = "\n")
  ) +
  theme_rz() +
  theme(legend.position = "top")
save_plot(p, "line_league_points_per_drive_rz_vs_non_rz")

## Where each offense's points come from ----
p <- plot_logo_quadrant(
  team_trip, "ppd_off", "share_pts_rz_off",
  title    = "Marching Down the Field or Hitting Home Runs?",
  subtitle = "Offensive Points per Drive vs. Share of Those Points Scored on Red Zone Trips",
  x_lab    = "Offensive Points per Drive",
  y_lab    = "Share of Points Scored on Red Zone Trips",
  caption  = paste(CAP_DATA, CAP_RZ, CAP_NONRZ, sep = "\n"),
  reverse_y = FALSE, quadrant_labels = LABELS_SOURCE_OFF, label_alpha = 0.7
) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
save_plot(p, "scatter_offense_points_source")

## Where each defense's points allowed come from ----
p <- plot_logo_quadrant(
  team_trip, "ppd_def", "share_pts_rz_def",
  title    = "Bend or Break?",
  subtitle = "Points Allowed per Drive vs. Share of Those Points Allowed on Red Zone Trips",
  x_lab    = "Points Allowed per Drive (Lower Is Better)",
  y_lab    = "Share of Points Allowed on Red Zone Trips",
  caption  = paste(CAP_DATA, CAP_RZ, CAP_NONRZ, sep = "\n"),
  reverse_y = FALSE, quadrant_labels = LABELS_SOURCE_DEF, label_alpha = 0.7
) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
save_plot(p, "scatter_defense_points_source")

# Plot 1: Average Points Scored ----

## Scatterplot of average points scored and allowed ----
p <- plot_logo_quadrant(
  team_rz, "avg_points_off", "avg_points_def",
  title    = "Average Red Zone Points Scored and Allowed by Team",
  subtitle = "For Teams in FBS, 2026 Season To Date",
  x_lab    = "Average Red Zone Points Scored on Offense",
  y_lab    = "Average Red Zone Points Allowed on Defense (Inverted)",
  caption  = paste(CAP_DATA, CAP_RZ, sep = "\n"),
  quadrant_labels = LABELS_TAILGATE
)
save_plot(p, "scatter_offense_defense_sores")

## Scatterplot of adjusted average points scored and allowed ----
p <- plot_logo_quadrant(
  team_rz, "adj_off_points", "adj_def_points",
  title    = "Average Red Zone Points Scored and Allowed by Team",
  subtitle = "For Teams in FBS, 2026 Season To Date, Adjusted for Schedule Strength",
  x_lab    = "Adjusted Average Red Zone Points Scored on Offense",
  y_lab    = "Adjusted Average Red Zone Points Allowed on Defense (Inverted)",
  caption  = paste(CAP_DATA, CAP_RZ,
                   paste(CAP_RIDGE, "Values are expected points per red zone trip."), sep = "\n"),
  quadrant_labels = LABELS_TAILGATE
)
save_plot(p, "scatter_offense_defense_sores_adj")

## Mean-centered net team points ----
p <- plot_net_bar(
  team_rz, "net_team_points_centralized",
  y_lab    = "Mean-Centered Average Net Team Points",
  subtitle = "For Teams in FBS, 2026 Season To Date",
  caption  = paste(CAP_DATA,
                   "Net red zone points (offensive points per trip minus defensive points allowed per trip) minus the FBS average.",
                   CAP_RZ, sep = "\n")
)
save_plot(p, "column_net_team_points_meancentered", width = 20, height = 6)

## Adjusted mean-centered net team points ----
p <- plot_net_bar(
  team_rz, "net_team_points_adj_centralized",
  y_lab    = "Adjusted Mean-Centered Average Net Team Points",
  subtitle = "For Teams in FBS, 2026 Season To Date, Adjusted for Schedule Strength",
  caption  = paste(CAP_DATA,
                   "Adjusted net red zone points (offense minus defense) minus the FBS average.",
                   CAP_RIDGE, sep = "\n")
)
save_plot(p, "column_net_team_points_meancentered_adj", width = 20, height = 6)

# Plot 2: Percent TDs in Red Zone ----

## Offense vs defense TD rate ----
p <- plot_logo_quadrant(
  team_rz, "pct_td_off", "pct_td_def",
  title    = "FBS Red Zone Efficiency Matrix",
  subtitle = "Offensive Red Zone Touchdown % Scored vs. Defensive Red Zone Touchdown % Allowed",
  x_lab    = "Offensive Red Zone Touchdown Rate Scored",
  y_lab    = "Defensive Red Zone Touchdown Rate Allowed (Inverted)",
  caption  = paste(CAP_DATA, CAP_RZ, sep = "\n"),
  pct = TRUE, quadrant_labels = LABELS_ELITE, label_alpha = 0.6
)
save_plot(p, "scatter_offense_defense_td_pct")

## Offense vs defense TD rate, adjusted ----
p <- plot_logo_quadrant(
  team_rz, "adj_off_td_pct", "adj_def_td_pct",
  title    = "FBS Red Zone Efficiency Matrix",
  subtitle = str_wrap("Offensive RZ Touchdown % Scored vs. Defensive RZ Touchdown % Allowed, Adjusted for Schedule Strength", 80),
  x_lab    = "Adjusted Offensive RZ Touchdown Rate Scored",
  y_lab    = "Adjusted Defensive RZ Touchdown Rate Allowed (Inverted)",
  caption  = str_wrap(
    paste(CAP_DATA, CAP_RZ,
          paste(CAP_RIDGE, "Values are the probability a red zone trip ends in a touchdown."), sep = "\n"),
    130),
  pct = TRUE, quadrant_labels = LABELS_ELITE, label_alpha = 0.6
)
save_plot(p, "scatter_offense_defense_td_pct_adj")

## Settling for 3 vs going for 7 (top 40 by scoring rate) ----
plot_dumbbell <- team_rz |>
  slice_max(pct_score_off, n = 40, with_ties = FALSE) |>
  mutate(team = reorder(team, pct_score_off))

p <- ggplot(plot_dumbbell) +
  geom_segment(aes(x = pct_td_off, xend = pct_score_off, y = team, yend = team),
               color = "gray70", linewidth = 1.2) +
  geom_point(aes(x = pct_score_off, y = team), color = "#336699", size = 4) +
  geom_from_path(aes(x = pct_td_off, y = team, path = logo), width = 0.025) +
  add_watermark(max(plot_dumbbell$pct_score_off) * 0.98, 3) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Top 40 Red Zone Finishers vs. Field Goal Settlers",
    subtitle = "Logo shows Red Zone Touchdown % | Blue dot shows Total Red Zone Scoring % (TD + FG)",
    x = "Conversion Rate", y = NULL,
    caption = paste(CAP_DATA, CAP_RZ, sep = "\n")
  ) +
  theme_rz() +
  theme(axis.text.y = element_blank())
save_plot(p, "dumbell_red_zone_finishing", width = 13, height = 15)

## Red zone volume vs efficiency ----
p <- ggplot(team_rz, aes(x = trips_per_game_off, y = pct_td_off)) +
  geom_smooth(method = "lm", se = FALSE, color = "gray80", linetype = "dotted") +
  geom_from_path(aes(path = logo), width = 0.025) +
  add_watermark(rng_pos(team_rz$trips_per_game_off, 0.97), rng_pos(team_rz$pct_td_off, 0.03)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Red Zone Opportunity vs. Finishing Ability",
    subtitle = "Offensive Red Zone Trips per Game vs. Touchdown Conversion Rate",
    x = "Red Zone Trips per Game", y = "Red Zone Touchdown Rate",
    caption = paste(CAP_DATA, CAP_RZ, sep = "\n")
  ) +
  theme_rz()
save_plot(p, "scatter_volume_efficiency", width = 13, height = 8)

## Drive outcome breakdown (top 40 by scoring rate) ----
plot_outcomes <- team_rz |>
  slice_max(pct_score_off, n = 40, with_ties = FALSE) |>
  mutate(
    team         = fct_reorder(team, pct_score_off),
    pct_fg_off   = pct_score_off - pct_td_off,
    pct_no_score = 1 - pct_score_off
  ) |>
  select(team, pct_td_off, pct_fg_off, pct_no_score) |>
  pivot_longer(-team, names_to = "outcome", values_to = "pct") |>
  mutate(outcome = factor(outcome,
                          levels = c("pct_no_score", "pct_fg_off", "pct_td_off"),
                          labels = c("No Points", "Field Goal", "Touchdown")))

p <- ggplot(plot_outcomes, aes(x = pct, y = team, fill = outcome)) +
  geom_col(position = "fill", width = 0.7) +
  add_watermark(0.95, 2) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Touchdown" = "#2e7d32", "Field Goal" = "#fbc02d", "No Points" = "#c62828")) +
  labs(
    title = str_wrap("Red Zone Drive Outcome Distribution For The Top 40 Scoring Teams", 35),
    x = "Proportion of Red Zone Drives", y = NULL, fill = NULL,
    caption = str_wrap(paste(CAP_DATA, CAP_RZ, sep = "\n"), 80)
  ) +
  theme_rz() +
  theme(legend.position = "top")
save_plot(p, "columns_outcome_distribution", width = 7, height = 16)

## Net red zone TD ratio ----
plot_ratio <- team_rz |>
  filter(is.finite(ratio_td_drives)) |>
  mutate(team = fct_reorder(team, ratio_td_drives))

p <- ggplot(plot_ratio, aes(x = ratio_td_drives, y = team)) +
  geom_segment(aes(x = 1, xend = ratio_td_drives, yend = team), color = "gray50", linewidth = 1) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  geom_from_path(aes(path = logo), width = 0.015) +
  add_watermark(max(plot_ratio$ratio_td_drives) * 0.90, 8) +
  scale_x_continuous(breaks = seq(0, 15, 1)) +
  labs(
    title = "FBS Net Red Zone Touchdown Ratios",
    subtitle = "Ratio of Offensive Red Zone TDs Scored to Defensive Red Zone TDs Allowed\n(Baseline = 1.0)",
    x = "Offensive Red Zone TDs / Defensive Red Zone TDs Allowed", y = NULL,
    caption = paste(CAP_DATA, CAP_RZ,
                    "Teams that have allowed zero red zone TDs are excluded (ratio undefined).", sep = "\n")
  ) +
  theme_rz() +
  theme(axis.text.y = element_text(size = 7.5))
save_plot(p, "lollipop_redzone_ratio", width = 14, height = 20)

# Save Results for the Report ----
# Read by 03_01_cfb_red_zone_report.Rmd so the manuscript's numbers match these outputs.
saveRDS(
  list(
    season         = SEASON,
    team_rz          = team_rz,
    team_trip        = team_trip,
    league_trip_week = league_trip_week,
    ratings_points   = ratings_points,
    ratings_tds      = ratings_tds,
    ratings_access   = ratings_access,
    lambda_points    = attr(ratings_points, "lambda"),
    lambda_tds       = attr(ratings_tds, "lambda"),
    lambda_access    = attr(ratings_access, "lambda")
  ),
  file.path(DIR_OUTPUTS, paste0(OUT_PREFIX, "results.rds"))
)
