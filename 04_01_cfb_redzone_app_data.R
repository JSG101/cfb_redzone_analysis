# Header ----
#
# Script name: 04_01_cfb_redzone_app_data.R
#
# Purpose of script: Build the tidy tables behind the red zone web app and load
#   them to BigQuery (one dataset, one table per grain):
#     1. trips        one row per red zone trip, with every cross section the app filters on
#     2. plays        one row per red zone rush/pass snap on those trips
#     3. team_games   one row per team x game x side (includes games with no trips)
#     4. team_season  one row per FBS team x side: raw, shrunk, and adjusted ratings
#     5. team_ratings_weekly  adjusted POE ratings refit through each week (trend + movers)
#     6. teams        FBS team reference (conference, logo, colors)
#     7. glossary     plain-English definitions, read by the app and the chat agent
#     8. meta         settings and counts for this build
#
# Location of script:
#
# Author: Japbir Gill, MBA MSEd
#
# Date Created: 2026-10-25
#
# Copyright (c) Japbir Gill, 2026
# Email: Japbir.Gill@gmail.com
#
# Notes ----
# - Run after 02_01 to 02_03 in the weekly workflow. Uses the same cache, so no
#   new API calls beyond any missing weeks.
# - BigQuery: set CFB_BQ_PROJECT (and optionally CFB_BQ_KEY, a path to a
#   service-account JSON) in .Renviron. Neither is ever committed. Without a
#   key, bigrquery opens a browser sign-in the first time.
# - Seasonal tables are replaced one season at a time (DELETE that season, then
#   append), so earlier seasons loaded later can sit in the same tables.
# - A local copy of every table is always saved to DIR_OUTPUTS as an .rds, so
#   the app can run offline during development.
# - Opponent tiers use full-season EPA/play on all scrimmage rush/pass plays,
#   not red zone results, so a trip's own outcome barely moves its opponent's
#   tier and the tiers rest on a far larger sample.
#

rm(list = ls())

# Working Directories, Libraries, Functions, Values ----

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("00_00_dirs_libs_funs_vals.R")
source("00_01_cfb_functions.R")
library(bigrquery)

# Settings ----
SEASON           <- 2026
WEEKS            <- NULL          # NULL = all completed weeks; or e.g. 1:8
REFRESH_WEEKS    <- integer(0)    # weeks to re-pull from the API, e.g. 8
RZ_LINE          <- 20
GARBAGE_MARGIN   <- 28
SHRINK_K         <- 10            # POE per trip: pseudo-trips at league average (0), as in 02_03
MIN_RATING_WEEK  <- 3             # first week for the weekly rating refits
POWER_CONFS      <- c("ACC", "Big 12", "Big Ten", "SEC")
SEED             <- 2026
OUT_PREFIX       <- "proj_012_04_01_"

WRITE_BIGQUERY   <- TRUE
BQ_PROJECT       <- Sys.getenv("CFB_BQ_PROJECT")
BQ_DATASET       <- "cfb_redzone"
BQ_LOCATION      <- "US"
BQ_KEY_PATH      <- Sys.getenv("CFB_BQ_KEY")   # optional service-account JSON

# Script-Specific Helpers ----

## Tier teams into thirds by a rating (tier 1 = strongest) ----
tier_thirds <- function(x, higher_is_better) {
  r <- if (higher_is_better) desc(x) else x
  c("Top third", "Middle third", "Bottom third")[ntile(r, 3)]
}

## Write one table to BigQuery, replacing only this season's rows ----
bq_write <- function(df, name) {
  tbl <- bq_table(BQ_PROJECT, BQ_DATASET, name)
  df  <- df |> mutate(across(where(is.factor), as.character))
  if ("season" %in% names(df) && bq_table_exists(tbl)) {
    bq_project_query(BQ_PROJECT, sprintf("DELETE FROM `%s.%s.%s` WHERE season = %d",
                                         BQ_PROJECT, BQ_DATASET, name, SEASON))
    bq_table_upload(tbl, df, write_disposition = "WRITE_APPEND")
  } else {
    bq_table_upload(tbl, df, create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
  }
  message(sprintf("BigQuery: %s.%s (%d rows)", BQ_DATASET, name, nrow(df)))
}

# Datasets ----
raw <- load_cfb_season(SEASON, WEEKS, overwrite_weeks = REFRESH_WEEKS)
rz  <- build_rz_tables(raw, rz_line = RZ_LINE, garbage_margin = GARBAGE_MARGIN)
rz$prod_rz_drives <- add_poe(rz$prod_rz_drives)

teams_ref <- get_team_info() |>
  select(any_of(c("team_id", "school", "mascot", "abbreviation", "conference",
                  "color", "alt_color", "logo"))) |>
  rename(team = school) |>
  mutate(power_conf = conference %in% POWER_CONFS)

conf_lookup <- teams_ref |> select(team, conference)
fbs_teams   <- teams_ref$team

# Processing ----

## Opponent strength tiers (full-season EPA/play, FBS teams) ----
all_plays <- rz$pbp |>
  filter(is_scrimmage, is_rush_pass, analysis_drive %in% TRUE, !is.na(EPA))

opp_strength <- full_join(
  all_plays |> summarise(off_epa_play = mean(EPA), .by = pos_team) |> rename(team = pos_team),
  all_plays |> summarise(def_epa_play = mean(EPA), .by = def_pos_team) |> rename(team = def_pos_team),
  by = "team"
) |>
  filter(team %in% fbs_teams) |>
  mutate(
    off_tier = tier_thirds(off_epa_play, higher_is_better = TRUE),
    def_tier = tier_thirds(def_epa_play, higher_is_better = FALSE)   # fewer EPA allowed = stronger
  )

## Season ratings (opponent-adjusted ridge) ----
set.seed(SEED)
ratings_points <- fit_team_ridge(rz$prod_rz_drives, "drive_points", family = "gaussian")
ratings_td     <- fit_team_ridge(rz$prod_rz_drives, "flag_td", family = "binomial")
ratings_poe    <- fit_team_ridge(rz$prod_rz_drives, "poe", family = "gaussian")
lambda_poe     <- attr(ratings_poe, "lambda")

## 1. Trips ----
trips <- rz$prod_rz_drives |>
  left_join(conf_lookup |> rename(offense = team, offense_conference = conference), by = "offense") |>
  left_join(conf_lookup |> rename(defense = team, defense_conference = conference), by = "defense") |>
  left_join(opp_strength |> select(defense = team, opp_def_tier = def_tier), by = "defense") |>
  left_join(opp_strength |> select(offense = team, opp_off_tier = off_tier), by = "offense") |>
  mutate(
    season      = SEASON,
    offense_fbs = offense %in% fbs_teams,
    defense_fbs = defense %in% fbs_teams,
    conf_game   = coalesce(offense_conference == defense_conference &
                             offense_conference != "FBS Independents", FALSE),
    venue       = case_when(home == 1 ~ "Home", home == -1 ~ "Away", TRUE ~ "Neutral"),   # offense's view
    game_state  = case_when(
      score_diff_start >= 9  ~ "Up 9+",
      score_diff_start >= 1  ~ "Up 1-8",
      score_diff_start == 0  ~ "Tied",
      score_diff_start >= -8 ~ "Down 1-8",
      TRUE                   ~ "Down 9+"
    ),
    half         = if_else(start_period <= 2, "First half", "Second half or OT"),
    opp_def_tier = coalesce(opp_def_tier, "Non-FBS"),
    opp_off_tier = coalesce(opp_off_tier, "Non-FBS")
  ) |>
  select(season, week, game_id, drive_id, offense, defense, offense_conference, defense_conference,
         offense_fbs, defense_fbs, conf_game, venue, start_period, half,
         score_margin_drive_start = score_diff_start, game_state,
         rz_entry_ytg, rz_entry_down, rz_entry_dist, rz_entry_zone, rz_snaps,
         rz_outcome, drive_points, flag_td, flag_fg, flag_score, xpts, poe,
         opp_def_tier, opp_off_tier)

## 2. Plays ----
plays <- rz$pbp |>
  filter(is_scrimmage, yards_to_goal <= RZ_LINE, is_rush_pass, !is.na(EPA)) |>
  select(-any_of("week")) |>
  inner_join(trips |> select(drive_id, week), by = "drive_id") |>
  transmute(
    season = SEASON, week, game_id, drive_id, play_id = as.character(id_play),
    offense = pos_team, defense = def_pos_team, period, down, distance, yards_to_goal,
    # include.lowest keeps snaps recorded at 0 yards to goal in the goal-line zone
    zone        = as.character(cut(yards_to_goal, c(0, 5, 10, RZ_LINE), include.lowest = TRUE,
                                   labels = c("Goal Line (1-5)", "Tight (6-10)", paste0("Fringe (11-", RZ_LINE, ")")))),
    goal_to_go  = distance >= yards_to_goal,
    play_call   = if_else(coalesce(pass == 1, FALSE), "Pass", "Rush"),
    play_type, yards_gained, epa = EPA, success,
    is_sack     = str_detect(play_type, regex("sack", ignore_case = TRUE)),
    is_turnover = str_detect(play_type, regex("interception|fumble recovery \\(opponent\\)|fumble return",
                                              ignore_case = TRUE)),
    is_negative = coalesce(yards_gained < 0, FALSE)
  )

## 3. Team games (both sides, zero-trip games included) ----
trip_game_sums <- function(side) {
  trips |>
    rename(team = all_of(side)) |>
    summarise(trips = n(), tds = sum(flag_td), fgs = sum(flag_fg), points = sum(drive_points),
              xpts = sum(xpts), poe = sum(poe), .by = c(game_id, team))
}

game_pairs <- rz$drives_clean |>
  distinct(game_id, week, offense, defense)

team_games <- bind_rows(
  game_pairs |> transmute(game_id, week, team = offense, opponent = defense, side = "Offense"),
  game_pairs |> transmute(game_id, week, team = defense, opponent = offense, side = "Defense")
) |>
  distinct() |>
  left_join(
    bind_rows(trip_game_sums("offense") |> mutate(side = "Offense"),
              trip_game_sums("defense") |> mutate(side = "Defense")),
    by = c("game_id", "team", "side")
  ) |>
  mutate(across(c(trips, tds, fgs, points, xpts, poe), \(v) coalesce(v, 0)),
         season = SEASON) |>
  relocate(season)

## 4. Team season (long by side, FBS teams) ----
side_summary <- function(side) {
  trips |>
    rename(team = all_of(side)) |>
    filter(team %in% fbs_teams) |>
    summarise(trips = n(), tds = sum(flag_td), fgs = sum(flag_fg), scores = sum(flag_score),
              points = sum(drive_points), poe_total = sum(poe), .by = team)
}

games_played <- team_games |>
  filter(side == "Offense") |>
  summarise(games = n_distinct(game_id), .by = team)

ratings_long <- function(adj_col) {
  list(ratings_points, ratings_td, ratings_poe) |>
    map2(c("adj_points_per_trip", "adj_td_rate", "adj_poe_per_trip"),
         \(r, nm) r |> select(team, value = all_of(adj_col)) |> rename(!!nm := value)) |>
    reduce(left_join, by = "team")
}

team_season <- bind_rows(
  side_summary("offense") |> mutate(side = "Offense") |> left_join(ratings_long("adj_off"), by = "team"),
  side_summary("defense") |> mutate(side = "Defense") |> left_join(ratings_long("adj_def"), by = "team")
) |>
  left_join(games_played, by = "team") |>
  mutate(
    season               = SEASON,
    trips_per_game       = trips / games,
    points_per_trip      = points / trips,
    td_rate              = tds / trips,
    score_rate           = scores / trips,
    td_rate_shrunk       = eb_shrink(tds, trips),
    poe_per_trip         = poe_total / trips,
    poe_per_trip_shrunk  = poe_total / (trips + SHRINK_K),
    .by = side
  ) |>
  relocate(season, team, side, games)

## 5. Weekly adjusted POE ratings (same penalty every week, as in 02_03) ----
weeks_played <- sort(unique(trips$week))

team_ratings_weekly <- weeks_played[weeks_played >= MIN_RATING_WEEK] |>
  map(\(w) {
    fit_team_ridge(filter(rz$prod_rz_drives, week <= w), "poe", family = "gaussian", lambda = lambda_poe) |>
      transmute(team, week = w, adj_poe_off = adj_off, adj_poe_def = adj_def)
  }) |>
  bind_rows() |>
  filter(team %in% fbs_teams) |>
  mutate(net = adj_poe_off - adj_poe_def) |>
  arrange(team, week) |>
  mutate(net_change = net - lag(net), .by = team) |>
  mutate(season = SEASON) |>
  relocate(season)

## 6. Teams ----
teams <- teams_ref |>
  left_join(opp_strength, by = "team")

## 7. Glossary ----
glossary <- tribble(
  ~term, ~definition,
  "Red zone trip", paste0("A drive with at least one scrimmage snap at or inside the opponent's ", RZ_LINE,
                          ". Long touchdowns launched from outside the ", RZ_LINE, " are not trips; drives that reached it and were pushed back are."),
  "Excluded drives", paste0("Drives ending at the end of a half or game, second-half drives starting with a margin above ",
                            GARBAGE_MARGIN, " points (garbage time), and drives with unusual point values."),
  "Drive points", "The actual score change on the drive, so missed extra points and two-point tries count correctly.",
  "Expected points (xpts)", "League-average points for a trip that enters the red zone at the same yard line and down.",
  "Points over expected (POE)", "Drive points minus expected points. League average is 0; positive is better for an offense, negative is better for a defense.",
  "Adjusted rating", "Ridge regression on every trip with offense, defense, and home-field terms: the value against an average opponent at a neutral site.",
  "Shrunk TD rate", "Touchdown rate pulled toward the league average, more strongly for teams with fewer trips (empirical Bayes).",
  "Shrunk POE per trip", paste0("POE total divided by trips plus ", SHRINK_K, " league-average trips, so tiny samples do not top the list."),
  "Opponent tier", "Opponents split into thirds by full-season EPA per play on all scrimmage plays (defense: allowed; offense: generated). Non-FBS opponents form their own tier.",
  "Venue", "Home, away, or neutral from the offense's point of view.",
  "Game state", "Score margin for the offense when the drive started.",
  "Zone", "Where a red zone snap took place: Goal Line (1-5 yards to go), Tight (6-10), Fringe (11-20).",
  "EPA", "Expected points added on a play (CFBD model).",
  "Success", "A play that gains enough of the needed yardage to stay on schedule (CFBD definition).",
  "Weekly rating", "Adjusted POE rating refit on all trips through that week, with the same penalty every week so changes reflect new games."
)

## 8. Meta ----
meta <- tibble(
  season          = SEASON,
  weeks           = paste(range(weeks_played), collapse = "-"),
  latest_week     = max(weeks_played),
  built_at        = Sys.time(),
  rz_line         = RZ_LINE,
  garbage_margin  = GARBAGE_MARGIN,
  shrink_k        = SHRINK_K,
  lambda_points   = attr(ratings_points, "lambda"),
  lambda_td       = attr(ratings_td, "lambda"),
  lambda_poe      = lambda_poe,
  n_games         = n_distinct(trips$game_id),
  n_trips         = nrow(trips),
  n_plays         = nrow(plays)
)

app_tables <- list(
  trips = trips, plays = plays, team_games = team_games, team_season = team_season,
  team_ratings_weekly = team_ratings_weekly, teams = teams, glossary = glossary, meta = meta
)

## Checks ----
stopifnot(
  !anyDuplicated(trips$drive_id),
  !anyDuplicated(team_season[c("team", "side")]),
  all(trips$venue %in% c("Home", "Away", "Neutral")),
  !anyNA(plays$zone)
)
message(sprintf("Built %d trips, %d plays, %d team-games, %d team-season rows (weeks %s)",
                nrow(trips), nrow(plays), nrow(team_games), nrow(team_season), meta$weeks))

# Save ----

## Local copy (offline development) ----
saveRDS(app_tables, file.path(DIR_OUTPUTS, paste0(OUT_PREFIX, "app_tables.rds")))

## BigQuery ----
if (WRITE_BIGQUERY && !nzchar(BQ_PROJECT)) {
  warning("CFB_BQ_PROJECT is not set, so nothing was loaded to BigQuery. ",
          "Add it to .Renviron and restart R.", call. = FALSE)
} else if (WRITE_BIGQUERY) {
  if (nzchar(BQ_KEY_PATH)) bq_auth(path = BQ_KEY_PATH) else bq_auth()
  ds <- bq_dataset(BQ_PROJECT, BQ_DATASET)
  if (!bq_dataset_exists(ds)) bq_dataset_create(ds, location = BQ_LOCATION)
  iwalk(app_tables, bq_write)
}
