# Header ----
#
# Script name: 02_02_cfb_red_zone_extended.R
#
# Purpose of script: Extend the red zone analysis in 02_01 with a play-by-play
#   definition of a red zone trip and seven follow-on analyses:
#     0. Red zone trip definition (PBP) + comparison with the end-of-drive definition
#     1. Red zone points over expected (POE), raw and opponent-adjusted
#     2. Reliability of red zone metrics (split-half) + empirical Bayes TD rates
#     3. Points-per-drive decomposition: reaching vs. finishing vs. non-RZ scoring
#     4. Play-level red zone efficiency (EPA, success rate, run/pass, zone)
#     5. Fourth-down decision quality inside the 20
#     6. Turnovers and negative plays on red zone trips
#     7. (Optional) Prior-season check: do early-season red zone metrics
#        predict later-season red zone performance and scoring margin?
#
# Location of script:
#
# Author: Japbir Gill, MBA MSEd
#
# Date Created: 2026-10-10
# Date Revised: 2026-10-11 (shared functions moved to 00_01_cfb_functions.R)
#
# Copyright (c) Japbir Gill, 2026
# Email: Japbir.Gill@gmail.com
#
# Notes ----
# https://cloud.r-project.org/web/packages/cfbfastR/refman/cfbfastR.html#cfbfastR-package
#
# - Data loading, red zone definition, ridge ratings, shrinkage, and plot helpers
#   live in 00_01_cfb_functions.R (shared with 02_01).
# - Data loads from the weekly cache; only weeks not yet saved hit the API.
#

rm(list = ls())

# Working Directories, Libraries, Functions, Values ----

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("00_00_dirs_libs_funs_vals.R")
source("00_01_cfb_functions.R")

# Settings ----
SEASON           <- 2026
PRIOR_SEASON     <- 2025
WEEKS            <- NULL         # NULL = all completed weeks; or e.g. 1:7
REFRESH_WEEKS    <- integer(0)   # weeks to re-pull from the API, e.g. 7
RZ_LINE          <- 20
GARBAGE_MARGIN   <- 28           # 2nd-half drives starting with |score diff| > this are excluded
EARLY_WEEK_MAX   <- 6            # Section 7: weeks 1..6 = "early", later weeks = "late"
RUN_PRIOR_SEASON <- FALSE        # Section 7 pulls a full extra season of PBP (slow the first time)
OUT_PREFIX       <- "proj_012_02_02_"

# Script-Specific Helpers ----

## Split-half reliability with Spearman-Brown correction ----
# Randomly splits each team's observations in half, correlates the half means
# across teams, and repeats. reliability_full ~ share of the between-team spread
# that is signal rather than noise (0.5 means observed gaps should shrink ~halfway).
split_half_reliability <- function(df, team_col, value_col, side_label,
                                   n_iter = 200, min_per_half = 3, seed = 2026) {
  set.seed(seed)
  base <- df |>
    rename(team = all_of(team_col)) |>
    filter(!is.na(.data[[value_col]])) |>
    select(team, value = all_of(value_col))
  
  r <- map_dbl(seq_len(n_iter), \(i) {
    halves <- base |>
      mutate(half = sample(rep(c("h1", "h2"), length.out = n())), .by = team) |>
      summarise(v = mean(value), n = n(), .by = c(team, half)) |>
      pivot_wider(names_from = half, values_from = c(v, n)) |>
      filter(n_h1 >= min_per_half, n_h2 >= min_per_half)
    cor(halves$v_h1, halves$v_h2)
  })
  
  r_half <- mean(r, na.rm = TRUE)
  tibble(
    side = side_label, metric = value_col, teams = n_distinct(base$team),
    split_half_r = r_half, reliability_full = 2 * r_half / (1 + r_half)
  )
}

## Points-per-drive decomposition vs. league average ----
# ppd = r * f + (1 - r) * n, where r = RZ trip rate, f = points per RZ trip,
# n = points per non-RZ drive. The three effects sum exactly to ppd - league ppd.
decompose_ppd <- function(drives_clean, side = c("offense", "defense")) {
  side <- match.arg(side)
  lg <- drives_clean |>
    summarise(
      lg_rate   = mean(rz_trip),
      lg_finish = mean(drive_points[rz_trip]),
      lg_nonrz  = mean(drive_points[!rz_trip]),
      lg_ppd    = mean(drive_points)
    )
  
  drives_clean |>
    rename(team = all_of(side)) |>
    summarise(
      n_drives = n(),
      r        = mean(rz_trip),
      f        = if (any(rz_trip)) mean(drive_points[rz_trip]) else NA_real_,
      n_pts    = if (any(!rz_trip)) mean(drive_points[!rz_trip]) else NA_real_,
      ppd      = mean(drive_points),
      .by = team
    ) |>
    mutate(
      access_effect    = (r - lg$lg_rate) * (lg$lg_finish - lg$lg_nonrz),
      finishing_effect = if_else(is.na(f), 0, r * (f - lg$lg_finish)),
      non_rz_effect    = if_else(is.na(n_pts), 0, (1 - r) * (n_pts - lg$lg_nonrz)),
      ppd_vs_avg       = ppd - lg$lg_ppd
    )
}

# Datasets ----
raw_current <- load_cfb_season(SEASON, WEEKS, overwrite_weeks = REFRESH_WEEKS)
rz <- build_rz_tables(raw_current, rz_line = RZ_LINE, garbage_margin = GARBAGE_MARGIN)
rz$prod_rz_drives <- add_poe(rz$prod_rz_drives)
xp_model <- attr(rz$prod_rz_drives, "xp_model")

# Section 0: Red Zone Definition Check ----

## Old (end-of-drive) vs new (PBP snap) definition ----
definition_check <- rz$drives |>
  filter(analysis_drive) |>
  mutate(
    old_definition = end_yards_to_goal <= RZ_LINE,
    new_definition = drive_id %in% rz$rz_trips$drive_id
  ) |>
  count(old_definition, new_definition, flag_td)
print(definition_check)
# old TRUE / new FALSE / flag_td 1 -> long TDs previously counted as red zone conversions
# old FALSE / new TRUE             -> real red zone trips previously dropped (backed out of the 20)

## Share of each offense's TDs scored from outside the red zone ----
td_source <- rz$drives_clean |>
  filter(flag_td == 1) |>
  summarise(total_tds = n(), tds_outside_rz = sum(!rz_trip), .by = offense) |>
  mutate(pct_tds_outside_rz = tds_outside_rz / total_tds) |>
  arrange(desc(pct_tds_outside_rz))
print(td_source, n = 25)

# Section 1: Red Zone Points Over Expected ----

## League expected points by entry spot ----
xp_curve <- expand_grid(rz_entry_ytg = 1:20, rz_entry_down = 1:3)
xp_curve$xpts <- predict(xp_model, newdata = xp_curve)

xp_observed <- rz$prod_rz_drives |>
  summarise(avg_points = mean(drive_points), trips = n(), .by = rz_entry_ytg)

p <- ggplot(xp_curve, aes(x = rz_entry_ytg, y = xpts, color = factor(rz_entry_down))) +
  geom_point(data = xp_observed, aes(x = rz_entry_ytg, y = avg_points, size = trips),
             inherit.aes = FALSE, alpha = 0.35, color = "gray40") +
  geom_line(linewidth = 1.2) +
  scale_x_reverse(breaks = seq(0, 20, 2)) +
  scale_color_manual(values = c("1" = "#2e7d32", "2" = "#336699", "3" = "#c62828")) +
  labs(
    title = "Expected Red Zone Points by Entry Point",
    subtitle = "Where (and on what down) a trip enters the red zone sets its baseline",
    x = "Yards to Goal at First Red Zone Snap", y = "Expected Points on the Drive",
    color = "Down at Entry", size = "Trips (all downs)",
    caption = "Data: cfbfastR, ggplot2. Curve: linear model with natural spline on entry yard line + entry down."
  ) +
  theme_rz()
save_plot(p, "line_expected_points_by_entry")

## Team POE: raw and opponent-adjusted ----
poe_off <- rz$prod_rz_drives |>
  summarise(trips_off = n(), poe_off_per_trip = mean(poe), poe_off_total = sum(poe), .by = offense) |>
  rename(team = offense)

poe_def <- rz$prod_rz_drives |>
  summarise(trips_def = n(), poe_def_per_trip = mean(poe), poe_def_total = sum(poe), .by = defense) |>
  rename(team = defense)

poe_adj <- fit_team_ridge(rz$prod_rz_drives, "poe", family = "gaussian")

plot_poe <- full_join(poe_off, poe_def, by = "team") |>
  left_join(poe_adj, by = "team") |>
  add_team_info()

print(plot_poe |> arrange(desc(poe_off_total)) |>
        select(team, trips_off, poe_off_per_trip, poe_off_total, adj_off), n = 25)

p <- plot_logo_quadrant(
  plot_poe, "poe_off_per_trip", "poe_def_per_trip",
  title = "Red Zone Points Over Expected",
  subtitle = "Actual minus expected points per trip, given where the trip entered the red zone",
  x_lab = "Offense: Points Over Expected per Trip",
  y_lab = "Defense: Points Over Expected Allowed per Trip (Inverted)"
)
save_plot(p, "scatter_poe")

p <- plot_logo_quadrant(
  plot_poe, "adj_off", "adj_def",
  title = "Red Zone Points Over Expected, Adjusted for Schedule",
  subtitle = "Ridge regression on red zone trips; neutral site vs. an average opponent",
  x_lab = "Offense: Adjusted POE per Trip",
  y_lab = "Defense: Adjusted POE Allowed per Trip (Inverted)",
  caption = "Data: cfbfastR, ggplot2, ggpath. Ridge regression (L2) with full offense/defense indicators and a home-field term."
)
save_plot(p, "scatter_poe_adj")

# Section 2: Reliability of Red Zone Metrics ----

ref_plays <- rz$pbp |>
  filter(is_scrimmage, analysis_drive %in% TRUE, is_rush_pass, !is.na(EPA))

rz_metrics <- c("flag_td", "flag_score", "drive_points", "poe")

reliability <- bind_rows(
  map(rz_metrics, \(m) split_half_reliability(rz$prod_rz_drives, "offense", m, "Offense")),
  map(rz_metrics, \(m) split_half_reliability(rz$prod_rz_drives, "defense", m, "Defense")),
  split_half_reliability(ref_plays, "pos_team", "EPA", "Offense"),
  split_half_reliability(ref_plays, "def_pos_team", "EPA", "Defense")
) |>
  mutate(metric_label = recode(
    metric,
    flag_td      = "RZ TD Rate",
    flag_score   = "RZ Scoring Rate",
    drive_points = "RZ Points per Trip",
    poe          = "RZ Points Over Expected",
    EPA          = "EPA/Play, All Plays (reference)"
  ))
print(reliability)

p <- ggplot(reliability, aes(x = reliability_full, y = fct_reorder(metric_label, reliability_full), fill = side)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray50") +
  scale_x_continuous(limits = c(min(0, min(reliability$reliability_full)), 1)) +
  scale_fill_manual(values = c("Offense" = "#336699", "Defense" = "#c62828")) +
  labs(
    title = "How Much of Red Zone Performance Is Signal?",
    subtitle = "Split-half reliability (Spearman-Brown corrected), 2026 season to date",
    x = "Reliability (0 = pure noise, 1 = pure signal)", y = NULL, fill = NULL,
    caption = "Data: cfbfastR. 200 random splits of each team's trips/plays. Dashed line: half of the observed spread is signal."
  ) +
  theme_rz() +
  theme(legend.position = "top")
save_plot(p, "bar_reliability")

## Empirical Bayes TD rates (how much raw rates should regress) ----
td_shrunk <- rz$prod_rz_drives |>
  summarise(trips = n(), tds = sum(flag_td), .by = offense) |>
  mutate(
    td_rate_raw    = tds / trips,
    td_rate_shrunk = eb_shrink(tds, trips)
  ) |>
  rename(team = offense) |>
  add_team_info() |>
  arrange(desc(td_rate_shrunk))
print(td_shrunk |> select(team, trips, tds, td_rate_raw, td_rate_shrunk), n = 25)

p <- ggplot(td_shrunk, aes(x = td_rate_raw, y = td_rate_shrunk)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
  geom_from_path(aes(path = logo), width = 0.025) +
  add_watermark(rng_pos(td_shrunk$td_rate_raw, 0.98), rng_pos(td_shrunk$td_rate_shrunk, 0.02)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Raw vs. Shrunk Red Zone TD Rates",
    subtitle = "Distance from the dashed line = how much each team's rate should regress",
    x = "Raw Red Zone TD Rate", y = "Empirical Bayes TD Rate",
    caption = "Data: cfbfastR, ggplot2, ggpath. Beta-binomial shrinkage toward the FBS average."
  ) +
  theme_rz()
save_plot(p, "scatter_td_rate_shrinkage")

# Section 3: Points-per-Drive Decomposition ----

decomp_off <- decompose_ppd(rz$drives_clean, "offense")
decomp_def <- decompose_ppd(rz$drives_clean, "defense")

# the three effects should sum to ppd_vs_avg (up to teams with no RZ trips)
message(sprintf("Max decomposition error (offense): %.6f",
                max(abs(decomp_off$access_effect + decomp_off$finishing_effect +
                          decomp_off$non_rz_effect - decomp_off$ppd_vs_avg), na.rm = TRUE)))

decomp_off_plot <- decomp_off |> add_team_info()

## Reaching vs. finishing ----
p <- ggplot(decomp_off_plot, aes(x = access_effect, y = finishing_effect)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_from_path(aes(path = logo), width = 0.025) +
  add_watermark(rng_pos(decomp_off_plot$access_effect, 0.98), rng_pos(decomp_off_plot$finishing_effect, 0.02)) +
  labs(
    title = "Getting There vs. Finishing",
    subtitle = "Points per drive above FBS average from reaching the red zone (x) and from scoring once there (y)",
    x = "Points/Drive from Red Zone Trip Rate", y = "Points/Drive from Red Zone Finishing",
    caption = "Data: cfbfastR, ggplot2, ggpath. Shift-share decomposition of points per drive vs. the FBS average."
  ) +
  theme_rz()
save_plot(p, "scatter_access_vs_finishing")

## Full decomposition, all teams ----
decomp_off_long <- decomp_off_plot |>
  select(team, ppd_vs_avg, access_effect, finishing_effect, non_rz_effect) |>
  pivot_longer(c(access_effect, finishing_effect, non_rz_effect), names_to = "component", values_to = "value") |>
  mutate(
    component = factor(component,
                       levels = c("access_effect", "finishing_effect", "non_rz_effect"),
                       labels = c("Reaching the Red Zone", "Finishing in the Red Zone", "Scoring Outside the Red Zone")),
    team = fct_reorder(team, ppd_vs_avg)
  )

p <- ggplot(decomp_off_long, aes(x = value, y = team, fill = component)) +
  geom_col(width = 0.75) +
  geom_point(data = distinct(decomp_off_long, team, ppd_vs_avg),
             aes(x = ppd_vs_avg, y = team), inherit.aes = FALSE, size = 1.6) +
  geom_vline(xintercept = 0) +
  add_watermark(rng_pos(decomp_off_long$value, 0.95), 3) +
  scale_fill_manual(values = c("#336699", "#2e7d32", "#fbc02d")) +
  labs(
    title = "Where Each Offense's Points Come From",
    subtitle = "Points per drive above/below FBS average, split into three sources (dot = total)",
    x = "Points per Drive vs. FBS Average", y = NULL, fill = NULL,
    caption = "Data: cfbfastR, ggplot2, ggpath"
  ) +
  theme_rz() +
  theme(legend.position = "top", axis.text.y = element_text(size = 7))
save_plot(p, "bar_ppd_decomposition_offense", width = 14, height = 22)

print(decomp_def |> add_team_info() |> arrange(ppd_vs_avg) |>
        select(team, ppd_vs_avg, access_effect, finishing_effect, non_rz_effect), n = 25)

# Section 4: Play-Level Red Zone Efficiency ----

rz_plays <- rz$pbp |>
  filter(is_scrimmage, yards_to_goal <= RZ_LINE, is_rush_pass, !is.na(EPA)) |>
  semi_join(rz$prod_rz_drives, by = "drive_id") |>
  mutate(
    zone       = cut(yards_to_goal, c(0, 5, 10, 20),
                     labels = c("Goal Line (1-5)", "Tight (6-10)", "Fringe (11-20)")),
    goal_to_go = distance >= yards_to_goal,
    play_call  = if_else(coalesce(pass == 1, FALSE), "Pass", "Rush")
  )

## League context: EPA by zone and play type ----
rz_league <- rz_plays |>
  summarise(plays = n(), epa = mean(EPA), success_rate = mean(success, na.rm = TRUE),
            .by = c(zone, play_call)) |> 
  mutate(zone = fct_na_value_to_level(zone, level = "Outside RZ (>20)"))
print(rz_league)

p <- ggplot(rz_league, aes(x = zone, y = epa, fill = play_call)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(scales::percent(success_rate, accuracy = 1), " SR\nn=", plays)),
            position = position_dodge(width = 0.8), vjust = ifelse(rz_league$epa >= 0, -0.3, 1.2), size = 3) +
  geom_hline(yintercept = 0) +
  scale_fill_manual(values = c("Pass" = "#336699", "Rush" = "#2e7d32")) +
  labs(
    title = "Red Zone EPA per Play by Zone and Play Type",
    subtitle = "FBS, 2026 season to date (labels: success rate and play count)",
    x = NULL, y = "EPA per Play", fill = NULL,
    caption = "Data: cfbfastR, ggplot2"
  ) +
  theme_rz() +
  theme(legend.position = "top") +
  scale_y_continuous(
    expand = expansion(mult = c(0.15, 0.15)) 
  )
save_plot(p, "bar_league_epa_by_zone")

## Team play-level summary ----
summarise_rz_plays <- function(df, team_col) {
  df |>
    rename(team = all_of(team_col)) |>
    summarise(
      plays        = n(),
      epa_per_play = mean(EPA),
      success_rate = mean(success, na.rm = TRUE),
      pass_rate    = mean(play_call == "Pass"),
      epa_pass     = mean(EPA[play_call == "Pass"]),
      epa_rush     = mean(EPA[play_call == "Rush"]),
      gtg_success  = mean(success[goal_to_go], na.rm = TRUE),
      .by = team
    )
}

rz_play_team <- full_join(
  summarise_rz_plays(rz_plays, "pos_team") |> rename_with(\(x) paste0(x, "_off"), -team),
  summarise_rz_plays(rz_plays, "def_pos_team") |> rename_with(\(x) paste0(x, "_def"), -team),
  by = "team"
) |>
  add_team_info()

p <- plot_logo_quadrant(
  rz_play_team, "epa_per_play_off", "epa_per_play_def",
  title = "Red Zone EPA per Play",
  subtitle = "Offensive EPA/play generated vs. defensive EPA/play allowed inside the 20",
  x_lab = "Offense: Red Zone EPA per Play",
  y_lab = "Defense: Red Zone EPA per Play Allowed (Inverted)"
)
save_plot(p, "scatter_rz_epa_per_play")

p <- plot_logo_quadrant(
  rz_play_team, "epa_rush_off", "epa_pass_off",
  title = "How Offenses Win Inside the 20",
  subtitle = "Red zone rushing EPA/play vs. passing EPA/play",
  x_lab = "Red Zone Rush EPA per Play", y_lab = "Red Zone Pass EPA per Play",
  reverse_y = FALSE
)
save_plot(p, "scatter_rz_rush_vs_pass_epa")

# Section 5: Fourth-Down Decisions Inside the 20 ----
# Simplified expected-value model. Compares EV of kicking vs. going for it using:
#   - FG make probability by kick distance (league logistic model)
#   - conversion probability by distance (league 3rd/4th-down logistic model)
#   - expected points for a 1st down at a given yard line (from CFBD ep_before)
# Ignores score, time, kicker quality, and weather, so 4th-quarter plays are excluded.
# Treat team-level results as directional, not definitive.

## Expected points for a 1st down by yard line ----
ep_by_ytg <- rz$pbp |>
  filter(is_scrimmage, down == 1, !is.na(ep_before), between(yards_to_goal, 1, 99)) |>
  summarise(ep = mean(ep_before), n = n(), .by = yards_to_goal)
ep_lm  <- lm(ep ~ ns(yards_to_goal, df = 6), data = ep_by_ytg, weights = n)
ep_fun <- function(ytg) {
  as.numeric(predict(ep_lm, newdata = tibble(yards_to_goal = pmin(pmax(ytg, 1), 99))))
}

## Field goal make probability ----
fg_data <- rz$pbp |>
  filter(str_detect(play_type, regex("field goal", ignore_case = TRUE))) |>
  mutate(
    kick_dist = yards_to_goal + 17,
    made      = as.integer(str_detect(play_type, regex("field goal good", ignore_case = TRUE)))
  ) |>
  filter(between(kick_dist, 17, 70))
fg_model <- glm(made ~ kick_dist, family = binomial, data = fg_data)

## Conversion probability ----
conv_data <- rz$pbp |>
  filter(is_scrimmage, down %in% c(3, 4), is_rush_pass, between(distance, 1, 15), yards_to_goal <= 40) |>
  mutate(
    goal_to_go = distance >= yards_to_goal,
    converted  = as.integer(
      coalesce(yards_gained >= distance, FALSE) |
        (str_detect(play_type, regex("touchdown", ignore_case = TRUE)) &
           !str_detect(play_type, regex("return", ignore_case = TRUE)))
    )
  )
conv_model <- glm(converted ~ log(distance) + goal_to_go + yards_to_goal, family = binomial, data = conv_data)

## EV of each choice ----
calc_fourth_ev <- function(df) {
  df <- df |> mutate(goal_to_go = distance >= yards_to_goal, kick_dist = yards_to_goal + 17)
  df$p_fg   <- predict(fg_model, newdata = df, type = "response")
  df$p_conv <- predict(conv_model, newdata = df, type = "response")
  ep_after_kickoff <- ep_fun(75)   # opponent's EP after a kickoff touchback at their 25
  
  df |>
    mutate(
      # made FG: +3, then opponent receives; missed FG: opponent ball at spot of kick or their 20
      ev_fg = p_fg * (3 - ep_after_kickoff) +
        (1 - p_fg) * -ep_fun(100 - pmax(yards_to_goal + 7, 20)),
      # conversion: TD if goal-to-go, else a 1st down `distance` yards closer
      ev_go_success = if_else(goal_to_go, 7 - ep_after_kickoff, ep_fun(yards_to_goal - distance)),
      # failure: opponent ball at the spot
      ev_go_fail    = -ep_fun(100 - yards_to_goal),
      ev_go         = p_conv * ev_go_success + (1 - p_conv) * ev_go_fail,
      go_advantage  = ev_go - ev_fg,
      model_call    = if_else(go_advantage > 0, "Go", "Kick")
    )
}

## League decision chart ----
decision_grid <- expand_grid(yards_to_goal = 1:20, distance = 1:10) |>
  filter(distance <= yards_to_goal) |>
  calc_fourth_ev()

p <- ggplot(decision_grid, aes(x = yards_to_goal, y = distance, fill = go_advantage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%+.1f", go_advantage)), size = 2.6) +
  scale_x_reverse(breaks = 1:20) +
  scale_y_continuous(breaks = 1:10) +
  scale_fill_gradient2(low = "#c62828", mid = "white", high = "#2e7d32", midpoint = 0) +
  labs(
    title = "Red Zone Fourth-Down Decision Chart",
    subtitle = "Expected points gained by going for it instead of kicking (green = go, red = kick)",
    x = "Yards to Goal", y = "Yards to Go", fill = "EV(Go) - EV(Kick)",
    caption = "Data: cfbfastR, ggplot2. Simplified EV model from 2026 FBS data; ignores score, time, kicker, and weather."
  ) +
  theme_rz()
save_plot(p, "tile_fourth_down_chart", width = 13, height = 7)

## Actual 4th-down decisions on red zone trips (quarters 1-3) ----
fourth_downs <- rz$pbp |>
  filter(is_scrimmage, down == 4, yards_to_goal <= RZ_LINE, period <= 3, distance >= 1) |>
  semi_join(rz$prod_rz_drives, by = "drive_id") |>
  mutate(decision = case_when(
    str_detect(play_type, regex("field goal", ignore_case = TRUE)) ~ "Kick",
    is_rush_pass ~ "Go",
    TRUE ~ NA_character_
  )) |>
  filter(!is.na(decision)) |>
  calc_fourth_ev() |>
  mutate(
    agrees  = decision == model_call,
    ev_lost = if_else(agrees, 0, abs(go_advantage))
  )

message(sprintf("League: %d red zone 4th downs; actual go rate %.0f%%; model go rate %.0f%%; agreement %.0f%%",
                nrow(fourth_downs), 100 * mean(fourth_downs$decision == "Go"),
                100 * mean(fourth_downs$model_call == "Go"), 100 * mean(fourth_downs$agrees)))

fourth_team <- fourth_downs |>
  summarise(
    fourth_downs     = n(),
    go_rate          = mean(decision == "Go"),
    model_go_rate    = mean(model_call == "Go"),
    kicked_should_go = sum(decision == "Kick" & model_call == "Go"),
    went_should_kick = sum(decision == "Go" & model_call == "Kick"),
    ev_lost_total    = sum(ev_lost),
    .by = pos_team
  ) |>
  rename(team = pos_team) |>
  add_team_info() |>
  arrange(desc(ev_lost_total))
print(fourth_team, n = 25)

fourth_plot <- fourth_team |>
  slice_max(ev_lost_total, n = 30, with_ties = FALSE) |>
  mutate(team = fct_reorder(team, ev_lost_total))

p <- ggplot(fourth_plot, aes(x = ev_lost_total, y = team)) +
  geom_col(fill = "#c62828", alpha = 0.65, width = 0.7) +
  geom_from_path(aes(path = logo), width = 0.025, hjust = -0.2) +
  geom_text(aes(label = paste0(kicked_should_go, " kicked / ", went_should_kick, " went")),
            x = 0, hjust = -0.05, size = 2.8, color = "white") +
  add_watermark(max(fourth_plot$ev_lost_total) * 0.95, 3) +
  labs(
    title = "Expected Points Left on the Table at 4th Down Inside the 20",
    subtitle = "Top 30 teams by EV lost vs. the model's recommendation (quarters 1-3)",
    x = "Total Expected Points Lost", y = NULL,
    caption = "Data: cfbfastR, ggplot2, ggpath. Bar labels: decisions where the team kicked but model said go / went but model said kick."
  ) +
  theme_rz() +
  theme(axis.text.y = element_blank())
save_plot(p, "bar_fourth_down_ev_lost", width = 12, height = 12)

# Section 6: Turnovers and Negative Plays ----

## How red zone trips end, by offense ----
outcome_levels <- c("Touchdown", "Field Goal", "Missed/Blocked FG", "Turnover on Downs", "Turnover", "Other")

rz_outcome_team <- rz$prod_rz_drives |>
  count(team = offense, rz_outcome) |>
  mutate(pct = n / sum(n), .by = team) |>
  semi_join(team_info, by = join_by(team == school))

team_order <- rz_outcome_team |>
  summarise(td = sum(pct[rz_outcome == "Touchdown"]), .by = team) |>
  arrange(td) |>
  pull(team)

p <- rz_outcome_team |>
  mutate(
    rz_outcome = factor(rz_outcome, levels = rev(outcome_levels)),
    team = factor(team, levels = team_order)
  ) |>
  ggplot(aes(x = pct, y = team, fill = rz_outcome)) +
  geom_col(width = 0.75) +
  add_watermark(0.95, 3) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c(
    "Touchdown" = "#2e7d32", "Field Goal" = "#fbc02d", "Missed/Blocked FG" = "#ef6c00",
    "Turnover on Downs" = "#6a1b9a", "Turnover" = "#c62828", "Other" = "gray70"
  ), breaks = outcome_levels) +
  labs(
    title = "How Every Red Zone Trip Ends",
    subtitle = "Share of red zone trips by result, sorted by touchdown rate",
    x = "Share of Red Zone Trips", y = NULL, fill = NULL,
    caption = "Data: cfbfastR, ggplot2, ggpath"
  ) +
  theme_rz() +
  theme(legend.position = "top", axis.text.y = element_text(size = 7))
save_plot(p, "bar_rz_outcomes_all_teams", width = 12, height = 22)

## Negative plays, sacks, and turnovers vs. TD rate ----
rz_negative <- rz_plays |>
  mutate(
    is_sack     = str_detect(play_type, regex("sack", ignore_case = TRUE)),
    is_turnover = str_detect(play_type, regex("interception|fumble recovery \\(opponent\\)|fumble return",
                                              ignore_case = TRUE)),
    is_negative = coalesce(yards_gained < 0, FALSE)
  ) |>
  summarise(
    plays              = n(),
    dropbacks          = sum(play_call == "Pass"),
    sack_rate          = sum(is_sack) / pmax(dropbacks, 1),
    negative_play_rate = mean(is_negative),
    turnover_plays     = sum(is_turnover),
    .by = pos_team
  ) |>
  rename(team = pos_team) |>
  left_join(
    rz$prod_rz_drives |>
      summarise(trips = n(), td_rate = mean(flag_td), .by = offense) |>
      rename(team = offense),
    by = "team"
  ) |>
  mutate(turnovers_per_trip = turnover_plays / trips) |>
  add_team_info()

print(cor.test(rz_negative$negative_play_rate, rz_negative$td_rate))

p <- ggplot(rz_negative, aes(x = negative_play_rate, y = td_rate)) +
  geom_smooth(method = "lm", se = TRUE, color = "gray60", fill = "gray90", linetype = "dotted") +
  geom_from_path(aes(path = logo), width = 0.025) +
  add_watermark(rng_pos(rz_negative$negative_play_rate, 0.98), rng_pos(rz_negative$td_rate, 0.02)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Negative Plays and Red Zone Finishing",
    subtitle = "Share of red zone snaps losing yardage vs. red zone touchdown rate",
    x = "Red Zone Negative Play Rate", y = "Red Zone TD Rate",
    caption = "Data: cfbfastR, ggplot2, ggpath. Association, not causation."
  ) +
  theme_rz()
save_plot(p, "scatter_negative_plays_vs_td_rate")

# Section 7 (Optional): Prior-Season Predictiveness ----
# Uses the prior season to ask: do weeks 1..EARLY_WEEK_MAX red zone metrics
# predict the rest of the season, and do they add anything beyond overall
# EPA/play when predicting later scoring margin?

if (RUN_PRIOR_SEASON) {
  
  raw_prior <- load_cfb_season(PRIOR_SEASON)
  rzp <- build_rz_tables(raw_prior, rz_line = RZ_LINE, garbage_margin = GARBAGE_MARGIN)
  rzp$prod_rz_drives <- add_poe(rzp$prod_rz_drives)
  
  period_label <- function(week) if_else(week <= EARLY_WEEK_MAX, "early", "late")
  
  ## Team metrics by period ----
  rz_off_period <- rzp$prod_rz_drives |>
    mutate(period = period_label(week)) |>
    summarise(rz_td_rate = mean(flag_td), rz_pts_per_trip = mean(drive_points), rz_poe = mean(poe),
              .by = c(offense, period)) |>
    rename(team = offense)
  
  rz_def_period <- rzp$prod_rz_drives |>
    mutate(period = period_label(week)) |>
    summarise(def_rz_td_rate = mean(flag_td), def_rz_poe = mean(poe), .by = c(defense, period)) |>
    rename(team = defense)
  
  plays_period <- rzp$pbp |>
    filter(is_scrimmage, analysis_drive %in% TRUE, is_rush_pass, !is.na(EPA)) |>
    select(-any_of("week")) |>
    left_join(rzp$games |> select(game_id, week), by = "game_id") |>
    mutate(period = period_label(week))
  
  epa_off_period <- plays_period |>
    summarise(off_epa = mean(EPA), .by = c(pos_team, period)) |>
    rename(team = pos_team)
  epa_def_period <- plays_period |>
    summarise(def_epa = mean(EPA), .by = c(def_pos_team, period)) |>
    rename(team = def_pos_team)
  
  margin_period <- rzp$games |>
    filter(!is.na(home_points), !is.na(away_points)) |>
    transmute(week, home_team, away_team, margin = home_points - away_points) |>
    pivot_longer(c(home_team, away_team), names_to = "side", values_to = "team") |>
    mutate(
      margin = if_else(side == "home_team", margin, -margin),
      period = period_label(week)
    ) |>
    summarise(avg_margin = mean(margin), .by = c(team, period))
  
  team_period <- list(rz_off_period, rz_def_period, epa_off_period, epa_def_period, margin_period) |>
    reduce(full_join, by = c("team", "period")) |>
    semi_join(team_info, by = join_by(team == school)) |>
    pivot_wider(names_from = period, values_from = -c(team, period), names_glue = "{.value}_{period}")
  
  ## Early-to-late stability ----
  stability <- tibble(metric = c("rz_td_rate", "rz_pts_per_trip", "rz_poe", "def_rz_td_rate",
                                 "def_rz_poe", "off_epa", "def_epa")) |>
    mutate(early_late_r = map_dbl(metric, \(m) cor(team_period[[paste0(m, "_early")]],
                                                   team_period[[paste0(m, "_late")]],
                                                   use = "complete.obs")))
  print(stability)
  
  p <- ggplot(stability, aes(x = early_late_r, y = fct_reorder(metric, early_late_r))) +
    geom_col(fill = "#336699", alpha = 0.75, width = 0.7) +
    geom_text(aes(label = sprintf("%.2f", early_late_r)), hjust = -0.15, size = 3.5) +
    scale_x_continuous(limits = c(min(0, min(stability$early_late_r)), 1)) +
    labs(
      title = paste0("Does Early-Season Performance Persist? (", PRIOR_SEASON, ")"),
      subtitle = paste0("Correlation between weeks 1-", EARLY_WEEK_MAX, " and the rest of the season, FBS teams"),
      x = "Early-to-Late Correlation", y = NULL,
      caption = "Data: cfbfastR, ggplot2"
    ) +
    theme_rz()
  save_plot(p, "bar_prior_season_stability")
  
  ## Does early RZ performance predict later margin beyond EPA? ----
  pred_df <- team_period |>
    filter(!is.na(avg_margin_late), !is.na(off_epa_early), !is.na(def_epa_early),
           !is.na(rz_poe_early), !is.na(def_rz_poe_early))
  
  m_base <- lm(avg_margin_late ~ off_epa_early + def_epa_early, data = pred_df)
  m_rz   <- update(m_base, . ~ . + rz_poe_early + def_rz_poe_early)
  
  message(sprintf("Adj. R-squared, EPA only: %.3f | EPA + red zone POE: %.3f",
                  summary(m_base)$adj.r.squared, summary(m_rz)$adj.r.squared))
  print(summary(m_rz))
  print(anova(m_base, m_rz))
}

# Save Results for the Report ----
# Read by 03_01_cfb_red_zone_report.Rmd so the manuscript's numbers match these outputs.
fbs_teams <- team_info$school
saveRDS(
  list(
    season = SEASON,
    weeks  = sort(unique(rz$drives$week)),
    rz_line = RZ_LINE,
    garbage_margin = GARBAGE_MARGIN,
    sample = tibble(
      drives_total     = nrow(rz$drives),
      drives_analysis  = sum(rz$drives$analysis_drive),
      excl_end_period  = sum(rz$drives$flag_end_period),
      excl_garbage     = sum(rz$drives$flag_garbage & !rz$drives$flag_end_period),
      excl_bad_score   = sum(rz$drives$flag_bad_score & !rz$drives$flag_end_period & !rz$drives$flag_garbage),
      rz_trips         = nrow(rz$prod_rz_drives),
      rz_plays         = nrow(rz_plays),
      games            = n_distinct(rz$drives$game_id),
      fbs_teams        = n_distinct(intersect(c(rz$drives$offense, rz$drives$defense), fbs_teams))
    ),
    definition_check = definition_check,
    td_source        = td_source,
    xp_model_r2      = summary(xp_model)$r.squared,
    xp_curve         = xp_curve,
    plot_poe         = plot_poe,
    lambda_poe       = attr(poe_adj, "lambda"),
    reliability      = reliability,
    td_shrunk        = td_shrunk,
    decomp_off       = decomp_off,
    decomp_def       = decomp_def,
    rz_league        = rz_league,
    rz_play_team     = rz_play_team,
    fg_model_coef    = coef(fg_model),
    conv_model_coef  = coef(conv_model),
    decision_grid    = decision_grid,
    fourth_summary   = tibble(
      n            = nrow(fourth_downs),
      go_rate      = mean(fourth_downs$decision == "Go"),
      model_go     = mean(fourth_downs$model_call == "Go"),
      agreement    = mean(fourth_downs$agrees),
      ev_lost      = sum(fourth_downs$ev_lost)
    ),
    fourth_team      = fourth_team,
    rz_outcomes      = count(rz$prod_rz_drives, rz_outcome) |> mutate(pct = n / sum(n)),
    rz_negative      = rz_negative,
    neg_td_cor       = cor.test(rz_negative$negative_play_rate, rz_negative$td_rate),
    prior_season     = if (RUN_PRIOR_SEASON) list(stability = stability, adj_r2_base = summary(m_base)$adj.r.squared,
                                                  adj_r2_rz = summary(m_rz)$adj.r.squared,
                                                  anova_p = anova(m_base, m_rz)$`Pr(>F)`[2]) else NULL
  ),
  file.path(DIR_OUTPUTS, paste0(OUT_PREFIX, "results.rds"))
)