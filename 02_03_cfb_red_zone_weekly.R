# Header ----
#
# Script name: 02_03_cfb_red_zone_weekly.R
#
# Purpose of script: Weekly red zone review. Two questions:
#   A. What happened in the most recent week, and did it change what we think
#      of each team?
#        1. Week leaders and laggards: red zone points over expected (POE) by team-game
#        2. Movers: change in opponent-adjusted red zone rating, before vs. after the week
#        3. Goal-line stands: trips that reached the 5 or closer and came away without a TD
#   B. How is red zone performance trending over the season?
#        4. League trend by week: TD, FG, and empty-trip rates
#        5. Running tab: cumulative POE by team across the season
#        6. Recent form: last few weeks vs. earlier in the season
#
# Location of script:
#
# Author: Japbir Gill, MBA MSEd
#
# Date Created: 2026-10-17
#
# Copyright (c) Japbir Gill, 2026
# Email: Japbir.Gill@gmail.com
#
# Notes ----
# - One week is a small sample (most teams have 2-6 red zone trips), so the
#   week's raw numbers are mostly noise. The movers chart is the steadier read:
#   it shows how much one more week shifts a season-long, schedule-adjusted rating.
# - POE = actual points minus expected points given where (and on what down) the
#   trip entered the red zone. League average POE is 0 by construction.
# - Uses the shared functions in 00_01_cfb_functions.R and the weekly cache.
#

rm(list = ls())

# Working Directories, Libraries, Functions, Values ----

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("00_00_dirs_libs_funs_vals.R")
source("00_01_cfb_functions.R")

# Settings ----
SEASON           <- 2026
WEEKS            <- NULL         # NULL = all completed weeks; or e.g. 1:8
REFRESH_WEEKS    <- integer(0)   # weeks to re-pull from the API (e.g., the latest week after corrections)
LATEST_WEEK      <- NULL         # NULL = most recent week in the data
RZ_LINE          <- 20
GARBAGE_MARGIN   <- 28
RECENT_N         <- 3            # recent-form window, in weeks
SHRINK_K         <- 10           # recent form: pseudo-trips at league-average POE (0) added to each window
MIN_RECENT_TRIPS <- 4            # recent form: minimum red zone trips in the recent window
TOP_N            <- 10           # bars per side in the week leaders chart
FOCUS_TEAMS      <- NULL         # running tab: teams to highlight, e.g. c("Georgia", "Alabama")
FOCUS_CONFERENCE <- NULL         # running tab: or highlight a conference, e.g. "SEC"
SEED             <- 2026
OUT_PREFIX       <- "proj_012_02_03_"

CAP_DATA <- "Data: cfbfastR, ggplot2, ggpath"
CAP_RZ   <- "Red zone trip = drive with at least one snap inside the opponent's 20. End-of-half and garbage-time drives excluded."
CAP_POE  <- "Points over expected = actual points minus expected points given the trip's entry yard line and down (league average = 0)."

# Datasets ----
raw <- load_cfb_season(SEASON, WEEKS, overwrite_weeks = REFRESH_WEEKS)
rz  <- build_rz_tables(raw, rz_line = RZ_LINE, garbage_margin = GARBAGE_MARGIN)
rz$prod_rz_drives <- add_poe(rz$prod_rz_drives)
trips <- rz$prod_rz_drives

weeks_played <- sort(unique(trips$week))
if (is.null(LATEST_WEEK)) LATEST_WEEK <- max(weeks_played)
if (length(weeks_played) < 2 * RECENT_N) {
  warning("Only ", length(weeks_played), " weeks of data; the recent-form comparison will be thin.", call. = FALSE)
}
message(sprintf("Weekly review for week %d (season weeks %s)", LATEST_WEEK, paste(range(weeks_played), collapse = "-")))

wk_label <- paste0("Week ", LATEST_WEEK)

# Processing ----

## Team-game red zone results (includes games with zero red zone trips) ----
team_games <- rz$drives_clean |>
  distinct(game_id, week, offense, defense)

team_game_rz <- team_games |>
  left_join(
    trips |>
      summarise(trips = n(), tds = sum(flag_td), fgs = sum(flag_fg), points = sum(drive_points),
                xpts = sum(xpts), poe = sum(poe), .by = c(game_id, offense)),
    by = c("game_id", "offense")
  ) |>
  mutate(across(c(trips, tds, fgs, points, xpts, poe), \(v) coalesce(v, 0)))

week_games <- team_game_rz |>
  filter(week == LATEST_WEEK)

## Week headline numbers ----
headline <- trips |>
  mutate(period = if_else(week == LATEST_WEEK, wk_label, "Earlier weeks")) |>
  summarise(trips = n(), td_rate = mean(flag_td), score_rate = mean(flag_score),
            pts_per_trip = mean(drive_points), .by = period)
print(headline)

write_csv(week_games |> arrange(desc(poe)),
          file.path(DIR_OUTPUTS, paste0(OUT_PREFIX, "week", LATEST_WEEK, "_team_games.csv")))

# A1. Week Leaders and Laggards ----
week_lb <- week_games |>
  rename(team = offense) |>
  add_team_info() |>
  filter(trips > 0)

week_lb_plot <- bind_rows(
  week_lb |> slice_max(poe, n = TOP_N, with_ties = FALSE),
  week_lb |> slice_min(poe, n = TOP_N, with_ties = FALSE)
) |>
  distinct(game_id, team, .keep_all = TRUE) |>
  mutate(
    bar_label = sprintf("%s vs %s  (%d TD / %d trips)", team, defense, tds, trips),
    bar_label = fct_reorder(bar_label, poe),
    side      = if_else(poe >= 0, "Over", "Under")
  )

p <- ggplot(week_lb_plot, aes(x = poe, y = bar_label, fill = side)) +
  geom_col(width = 0.7, alpha = 0.75, show.legend = FALSE) +
  geom_vline(xintercept = 0) +
  geom_from_path(aes(path = logo, hjust = if_else(poe >= 0, -0.3, 1.3)), width = 0.025) +
  add_watermark(min(week_lb_plot$poe) * 0.9, 2) +
  scale_fill_manual(values = c("Over" = "#2e7d32", "Under" = "#c62828")) +
  scale_x_continuous(expand = expansion(mult = 0.15)) +
  labs(
    title = paste0("Finishers and Field-Goal Fans: ", wk_label),
    subtitle = paste0("Red zone points over expected by team, top and bottom ", TOP_N, " games"),
    x = "Red Zone Points Over Expected (Game Total)", y = NULL,
    caption = paste(CAP_DATA, CAP_POE, CAP_RZ, sep = "\n")
  ) +
  theme_rz() +
  theme(axis.text.y = element_text(size = 9))
save_plot(p, paste0("bar_week", LATEST_WEEK, "_poe_leaders"), width = 12, height = 9)

# A2. Movers: How the Week Changed Adjusted Ratings ----
# Same penalty (lambda) for both fits, so the change reflects the new games,
# not a different amount of shrinkage.
set.seed(SEED)
fit_now  <- fit_team_ridge(trips, "poe", family = "gaussian")
fit_prev <- fit_team_ridge(filter(trips, week < LATEST_WEEK), "poe", family = "gaussian",
                           lambda = attr(fit_now, "lambda"))

teams_this_week <- unique(c(week_games$offense, week_games$defense))

movers <- fit_now |>
  select(team, off_now = adj_off, def_now = adj_def) |>
  inner_join(fit_prev |> select(team, off_prev = adj_off, def_prev = adj_def), by = "team") |>
  mutate(
    # net = offense POE created minus defense POE allowed (higher is better)
    net_now   = off_now - def_now,
    net_prev  = off_prev - def_prev,
    delta     = net_now - net_prev,
    played    = team %in% teams_this_week
  ) |>
  add_team_info()

if (sd(movers$net_now, na.rm = TRUE) < 0.01) {
  warning("Adjusted ratings are nearly identical across teams (the model found little signal), ",
          "so the movers chart will look flat. More weeks of data should help.", call. = FALSE)
}
print(movers |> arrange(desc(delta)) |> select(team, net_prev, net_now, delta, played), n = 15)

movers_plot <- bind_rows(
  movers |> slice_max(delta, n = 8, with_ties = FALSE),
  movers |> slice_min(delta, n = 8, with_ties = FALSE)
) |>
  distinct(team, .keep_all = TRUE) |>
  mutate(team = fct_reorder(team, delta), direction = if_else(delta >= 0, "Up", "Down"))

p <- ggplot(movers_plot, aes(y = team)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_segment(aes(x = net_prev, xend = net_now, yend = team, color = direction),
               linewidth = 1.1, arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
               show.legend = FALSE) +
  geom_point(aes(x = net_prev), color = "gray55", size = 2.5) +
  geom_from_path(aes(x = net_now, path = logo), width = 0.025) +
  add_watermark(max(c(movers_plot$net_now, movers_plot$net_prev)), 2) +
  scale_color_manual(values = c("Up" = "#2e7d32", "Down" = "#c62828")) +
  labs(
    title = "Stock Up, Stock Down",
    subtitle = paste0("Change in schedule-adjusted net red zone POE per trip after ", wk_label,
                      " (gray dot = before, logo = after)"),
    x = "Adjusted Net Red Zone POE per Trip (Offense minus Defense)", y = NULL,
    caption = paste(CAP_DATA,
                    "Ridge regression on all red zone trips, refit with and without the latest week using the same penalty.",
                    CAP_POE, sep = "\n")
  ) +
  theme_rz()
save_plot(p, paste0("arrow_week", LATEST_WEEK, "_movers"), width = 12, height = 9)

# A3. Goal-Line Stands ----
deepest_point <- rz$pbp |>
  filter(is_scrimmage) |>
  summarise(min_ytg = min(yards_to_goal, na.rm = TRUE), .by = drive_id)

goal_line_stands <- trips |>
  filter(week == LATEST_WEEK) |>
  left_join(deepest_point, by = "drive_id") |>
  filter(min_ytg <= 5, flag_td == 0) |>
  select(offense, defense, min_ytg, rz_outcome, drive_points, rz_entry_ytg) |>
  arrange(min_ytg)

message(sprintf("%s: %d trips reached the 5 or closer without a touchdown", wk_label, nrow(goal_line_stands)))
print(goal_line_stands, n = 30)
print(count(goal_line_stands, defense, name = "stands", sort = TRUE))
write_csv(goal_line_stands,
          file.path(DIR_OUTPUTS, paste0(OUT_PREFIX, "week", LATEST_WEEK, "_goal_line_stands.csv")))

# B4. League Trend by Week ----
league_week <- trips |>
  summarise(
    trips      = n(),
    Touchdown  = mean(flag_td),
    `Field Goal` = mean(flag_fg),
    `No Points`  = mean(1 - flag_score),
    .by = week
  ) |>
  pivot_longer(c(Touchdown, `Field Goal`, `No Points`), names_to = "outcome", values_to = "rate") |>
  mutate(
    se      = sqrt(rate * (1 - rate) / trips),     # binomial noise band
    outcome = factor(outcome, levels = c("Touchdown", "Field Goal", "No Points"))
  )

p <- ggplot(league_week, aes(x = week, y = rate, color = outcome, fill = outcome)) +
  annotate("rect", xmin = LATEST_WEEK - 0.4, xmax = LATEST_WEEK + 0.4, ymin = -Inf, ymax = Inf,
           fill = "gray90", alpha = 0.6) +
  geom_ribbon(aes(ymin = rate - 1.96 * se, ymax = rate + 1.96 * se), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = weeks_played) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("Touchdown" = "#2e7d32", "Field Goal" = "#fbc02d", "No Points" = "#c62828"),
                     aesthetics = c("color", "fill")) +
  labs(
    title = "Is the Red Zone Getting Tougher?",
    subtitle = "FBS red zone trip outcomes by week (shaded band = normal week-to-week noise)",
    x = "Week", y = "Share of Red Zone Trips", color = NULL, fill = NULL,
    caption = paste(CAP_DATA, CAP_RZ, sep = "\n")
  ) +
  theme_rz() +
  theme(legend.position = "top")
save_plot(p, "line_league_outcomes_by_week")

# B5. Running Tab: Cumulative POE by Team ----
cum_poe <- trips |>
  summarise(poe = sum(poe), trips = n(), .by = c(offense, week)) |>
  rename(team = offense) |>
  semi_join(team_info, by = join_by(team == school)) |>
  complete(team, week = weeks_played, fill = list(poe = 0, trips = 0)) |>   # bye weeks add 0
  arrange(team, week) |>
  mutate(cum_poe = cumsum(poe), cum_trips = cumsum(trips), .by = team) |>
  left_join(team_info, by = join_by(team == school))

focus <- if (!is.null(FOCUS_TEAMS)) {
  FOCUS_TEAMS
} else if (!is.null(FOCUS_CONFERENCE)) {
  team_info$school[team_info$conference %in% FOCUS_CONFERENCE]
} else {
  latest <- cum_poe |> filter(week == max(weeks_played))
  c(slice_max(latest, cum_poe, n = 3, with_ties = FALSE)$team,
    slice_min(latest, cum_poe, n = 3, with_ties = FALSE)$team)
}
focus_subtitle <- if (!is.null(FOCUS_TEAMS)) {
  "Selected teams highlighted"
} else if (!is.null(FOCUS_CONFERENCE)) {
  paste(paste(FOCUS_CONFERENCE, collapse = ", "), "teams highlighted")
} else {
  "Top 3 and bottom 3 offenses highlighted"
}

cum_focus <- cum_poe |> filter(team %in% focus)
cum_ends  <- cum_focus |> filter(week == max(weeks_played))

p <- ggplot(cum_poe, aes(x = week, y = cum_poe, group = team)) +
  geom_hline(yintercept = 0, color = "gray40") +
  geom_line(color = "gray85", linewidth = 0.4) +
  geom_line(data = cum_focus, aes(color = team), linewidth = 1.1, show.legend = FALSE) +
  geom_from_path(data = cum_ends, aes(path = logo), width = 0.03, hjust = -0.3) +
  add_watermark(min(weeks_played) + 0.3, max(cum_poe$cum_poe) * 0.95) +
  scale_x_continuous(breaks = weeks_played, expand = expansion(mult = c(0.02, 0.08))) +
  scale_color_viridis_d(option = "D", end = 0.85) +
  labs(
    title = "Running Tab: Red Zone Points Over Expected",
    subtitle = paste0("Cumulative offensive POE through ", wk_label, ". ", focus_subtitle),
    x = "Week", y = "Cumulative Points Over Expected",
    caption = paste(CAP_DATA, CAP_POE, "Above 0 = scoring more than the red zone chances would predict.", sep = "\n")
  ) +
  theme_rz()
save_plot(p, "line_cumulative_poe")

# B6. Recent Form: Last Few Weeks vs. Earlier ----
# Each window's POE per trip is shrunk toward 0 (league average) by adding
# SHRINK_K pseudo-trips, so a team going 3-for-3 doesn't top the chart.
recent_weeks <- tail(weeks_played, RECENT_N)

form <- trips |>
  mutate(window = if_else(week %in% recent_weeks, "recent", "earlier")) |>
  summarise(poe_sum = sum(poe), trips = n(), .by = c(offense, window)) |>
  pivot_wider(names_from = window, values_from = c(poe_sum, trips), values_fill = 0) |>
  rename(team = offense) |>
  mutate(
    poe_recent  = poe_sum_recent / (trips_recent + SHRINK_K),
    poe_earlier = poe_sum_earlier / (trips_earlier + SHRINK_K),
    change      = poe_recent - poe_earlier
  ) |>
  filter(trips_recent >= MIN_RECENT_TRIPS) |>
  add_team_info()

form_plot <- bind_rows(
  form |> slice_max(change, n = 8, with_ties = FALSE),
  form |> slice_min(change, n = 8, with_ties = FALSE)
) |>
  distinct(team, .keep_all = TRUE) |>
  mutate(team = fct_reorder(team, change), direction = if_else(change >= 0, "Heating Up", "Cooling Off"))

p <- ggplot(form_plot, aes(y = team)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_segment(aes(x = poe_earlier, xend = poe_recent, yend = team, color = direction),
               linewidth = 1.1, arrow = arrow(length = unit(0.18, "cm"), type = "closed")) +
  geom_point(aes(x = poe_earlier), color = "gray55", size = 2.5) +
  geom_from_path(aes(x = poe_recent, path = logo), width = 0.025) +
  add_watermark(max(c(form_plot$poe_recent, form_plot$poe_earlier)), 2) +
  scale_color_manual(values = c("Heating Up" = "#e65100", "Cooling Off" = "#1565c0")) +
  labs(
    title = "Heating Up, Cooling Off",
    subtitle = paste0("Offensive red zone POE per trip: weeks ", min(recent_weeks), "-", max(recent_weeks),
                      " vs. earlier (gray dot = earlier, logo = recent)"),
    x = "Red Zone Points Over Expected per Trip (shrunk toward average)", y = NULL, color = NULL,
    caption = paste(CAP_DATA,
                    paste0("Each window adds ", SHRINK_K, " league-average trips to damp small samples; teams need ",
                           MIN_RECENT_TRIPS, "+ recent trips."),
                    CAP_POE, sep = "\n")
  ) +
  theme_rz() +
  theme(legend.position = "top")
save_plot(p, "arrow_recent_form", width = 12, height = 9)

# Save Results for the Report ----
# Read by 03_01_cfb_red_zone_report.Rmd so the manuscript's numbers match these outputs.
saveRDS(
  list(
    season           = SEASON,
    latest_week      = LATEST_WEEK,
    weeks_played     = weeks_played,
    recent_weeks     = recent_weeks,
    recent_n         = RECENT_N,
    shrink_k         = SHRINK_K,
    min_recent_trips = MIN_RECENT_TRIPS,
    headline         = headline,
    week_games       = week_games,
    movers           = movers,
    lambda_movers    = attr(fit_now, "lambda"),
    goal_line_stands = goal_line_stands,
    league_week      = league_week,
    cum_poe_latest   = cum_poe |> filter(week == max(weeks_played)),
    form             = form
  ),
  file.path(DIR_OUTPUTS, paste0(OUT_PREFIX, "results.rds"))
)