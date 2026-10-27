# Header ----
#
# Script name: 02_04_cfb_red_zone_preview.R
#
# Purpose of script: Pre-game red zone preview for the upcoming week. Looks at
#   who is playing and lines up each offense against the defense it will face,
#   using season-to-date, schedule-adjusted red zone ratings:
#     1. Matchup map: every offense vs. the defense across the field
#     2. Must-watch games: an intrigue index built from unit quality and
#        strength-vs-strength clashes
#     3. Tale of the tape: getting there vs. cashing in, for the featured games
#     4. Biggest mismatches: the widest offense-vs-defense gaps on the slate
#     5. Short written insights for posts
#
# Location of script:
#
# Author: Japbir Gill, MBA MSEd
#
# Date Created: 2026-10-27
#
# Copyright (c) Japbir Gill, 2026
# Email: Japbir.Gill@gmail.com
#
# Notes ----
# - This is a look at the data before kickoff, not a forecast. It produces no win
#   probabilities, spreads, or projected scores. "Edge" and "mismatch" describe
#   how two units compare on season-to-date ratings, nothing more.
# - Ratings are refit here with the same ridge models as 02_01 (points per trip,
#   TD rate) plus a red zone trip rate per drive ("getting there"). They are
#   neutral-site values vs. an average opponent; no home-field term is applied.
# - Percentiles rank each unit among FBS teams (100 = best; for defenses, best =
#   stingiest). Ridge ratings are already shrunk, but percentiles can still
#   stretch small differences, so the CSV keeps the points-per-trip values too.
# - The upcoming week's schedule is cached as kind "schedule" (one API call).
#   Set REFRESH_SCHEDULE <- TRUE to re-pull it (e.g., after a game is moved).
# - Run midweek, after 02_01 to 02_03 have picked up the latest completed week.
#

rm(list = ls())

# Working Directories, Libraries, Functions, Values ----

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("00_00_dirs_libs_funs_vals.R")
source("00_01_cfb_functions.R")

# Settings ----
SEASON           <- 2026
SEASON_TYPE      <- "regular"
WEEKS            <- NULL         # ratings data: NULL = all completed weeks; or e.g. 1:8
REFRESH_WEEKS    <- integer(0)   # weeks to re-pull from the API
PREVIEW_WEEK     <- NULL         # NULL = the week after the latest completed week
REFRESH_SCHEDULE <- FALSE        # TRUE = re-pull the preview week's schedule
RZ_LINE          <- 20
GARBAGE_MARGIN   <- 28
MIN_TRIPS        <- 8            # units with fewer red zone trips (offense or defense) are marked "thin sample"
STRONG_PCT       <- 75           # strength vs. strength: both units at or above this FBS percentile
WEAK_PCT         <- 25           # weakness vs. weakness: both units at or below this percentile
MISMATCH_GAP     <- 50           # mismatch: offense and defense percentiles at least this far apart
PROFILE_HI       <- 60           # game profiles: "good" side of the field
PROFILE_LO       <- 40           # game profiles: "poor" side of the field
N_FEATURED       <- 8            # games in the must-watch and tale-of-the-tape charts
N_MISMATCH       <- 16           # matchups in the mismatch chart
SEED             <- 2026
OUT_PREFIX       <- "proj_012_02_04_"

CAP_DATA  <- "Data: cfbfastR, ggplot2, ggpath"
CAP_RZ    <- "Red zone trip = drive with at least one snap inside the opponent's 20. End-of-half and garbage-time drives excluded."
CAP_RIDGE <- "Ratings: ridge regression on season-to-date drives with offense, defense, and home terms; neutral site vs. an average opponent."
CAP_PCT   <- "Percentile among FBS teams: 100 = best (for defenses, the stingiest)."
CAP_NOTE  <- "A pre-game look at season-to-date red zone ratings, not a prediction of the result."

# Quadrant labels (offense percentile on x, opposing defense percentile on y; Top-Right = strength vs. strength)
LABELS_PREVIEW <- tribble(
  ~quadrant,      ~label,                                 ~color,
  "Top-Right",    "Unstoppable Force,\nImmovable Object", "#1b5e20",
  "Top-Left",     "Defense Holds\nthe Edge",              "#1565c0",
  "Bottom-Right", "Offense Holds\nthe Edge",              "#e65100",
  "Bottom-Left",  "Leaky Roof,\nRusty Bucket",            "#4a148c"
)

COL_EDGE <- c("Offense edge" = "#e65100", "Defense edge" = "#1565c0", "Even" = "gray55")

# Script-Specific Helpers ----

## FBS percentile, 0-100 (midpoint ranks, so nobody is the "0th" or "100th") ----
pctile <- function(x) 100 * (rank(x, na.last = "keep") - 0.5) / sum(!is.na(x))

## 1st, 2nd, 3rd, ... for percentiles in the written insights ----
ordinal <- function(x) {
  x <- round(x)
  suffix <- if_else(x %% 100 %in% 11:13, "th",
                    c("th", "st", "nd", "rd", rep("th", 6))[x %% 10 + 1])
  paste0(x, suffix)
}

# Datasets ----
raw <- load_cfb_season(SEASON, WEEKS, season_type = SEASON_TYPE, overwrite_weeks = REFRESH_WEEKS)
rz  <- build_rz_tables(raw, rz_line = RZ_LINE, garbage_margin = GARBAGE_MARGIN)
trips <- rz$prod_rz_drives

weeks_played <- sort(unique(trips$week))
if (is.null(PREVIEW_WEEK)) PREVIEW_WEEK <- max(weeks_played) + 1
wk_label <- paste0("Week ", PREVIEW_WEEK)
message(sprintf("Red zone preview for %s, using ratings through week %d", wk_label, max(weeks_played)))

## Upcoming schedule ----
schedule <- cfbd_cached_week(
  "schedule", SEASON, PREVIEW_WEEK,
  \() cfbd_game_info(SEASON, week = PREVIEW_WEEK, season_type = SEASON_TYPE),
  season_type = SEASON_TYPE, overwrite = REFRESH_SCHEDULE
)
if (is.null(schedule)) stop("No games found for ", SEASON, " ", wk_label, ". Check PREVIEW_WEEK.", call. = FALSE)
check_cols(schedule, c("game_id", "week", "neutral_site", "home_team", "away_team"), "schedule")

schedule <- schedule |>
  mutate(game_id = as.character(game_id)) |>
  select(game_id, week, any_of("start_date"), neutral_site, home_team, away_team)

# Processing ----

## Schedule-adjusted ratings ----
set.seed(SEED)
ratings_points <- fit_team_ridge(trips, "drive_points", family = "gaussian")
ratings_tds    <- fit_team_ridge(trips, "flag_td", family = "binomial")
# getting there: share of (non-garbage, non-end-of-half) drives that reach the red zone
ratings_access <- fit_team_ridge(rz$drives_clean |> mutate(rz_trip = as.integer(rz_trip)),
                                 "rz_trip", family = "binomial")

if (sd(ratings_points$adj_off, na.rm = TRUE) < 0.01) {
  warning("Adjusted points-per-trip ratings are nearly identical across teams (little signal yet), ",
          "so percentiles will exaggerate tiny differences.", call. = FALSE)
}

league_ppt <- mean(trips$drive_points)

## Team table: one row per FBS team ----
team_rz <- ratings_points |>
  select(team, off_pts = adj_off, def_pts = adj_def) |>
  left_join(ratings_tds |> select(team, off_td = adj_off, def_td = adj_def), by = "team") |>
  left_join(ratings_access |> select(team, off_access = adj_off, def_access = adj_def), by = "team") |>
  left_join(count(trips, team = offense, name = "trips_off"), by = "team") |>
  left_join(count(trips, team = defense, name = "trips_def"), by = "team") |>
  mutate(across(c(trips_off, trips_def), \(v) coalesce(v, 0L))) |>
  add_team_info() |>
  mutate(
    # FBS percentiles, 100 = best. Defenses rank by fewest allowed.
    off_pct        = pctile(off_pts),
    def_pct        = pctile(-def_pts),
    off_td_pct     = pctile(off_td),
    def_td_pct     = pctile(-def_td),
    off_access_pct = pctile(off_access),
    def_access_pct = pctile(-def_access)
  )

## Matchup sides: each offense vs. the defense it faces ----
off_cols <- team_rz |>
  select(offense = team, off_logo = logo, off_conf = conference, off_pts, off_td, off_access,
         trips_off, off_pct, off_td_pct, off_access_pct)
def_cols <- team_rz |>
  select(defense = team, def_logo = logo, def_conf = conference, def_pts, def_td, def_access,
         trips_def, def_pct, def_td_pct, def_access_pct)

sides <- bind_rows(
  schedule |> mutate(offense = home_team, defense = away_team, venue = if_else(neutral_site %in% TRUE, "neutral", "home")),
  schedule |> mutate(offense = away_team, defense = home_team, venue = if_else(neutral_site %in% TRUE, "neutral", "away"))
) |>
  inner_join(off_cols, by = "offense") |>
  inner_join(def_cols, by = "defense") |>
  mutate(
    game_label = if_else(neutral_site %in% TRUE,
                         paste(away_team, "vs.", home_team),
                         paste(away_team, "at", home_team)),
    side_label = paste0(offense, " O vs. ", defense, " D"),
    # points per trip relative to the FBS average (positive = better for that unit)
    off_vs_avg = off_pts - league_ppt,
    def_vs_avg = league_ppt - def_pts,
    gap        = off_pct - def_pct,
    edge       = case_when(gap >=  MISMATCH_GAP / 2 ~ "Offense edge",
                           gap <= -MISMATCH_GAP / 2 ~ "Defense edge",
                           TRUE ~ "Even"),
    strength_clash = off_pct >= STRONG_PCT & def_pct >= STRONG_PCT,
    weakness_clash = off_pct <= WEAK_PCT & def_pct <= WEAK_PCT,
    mismatch       = abs(gap) >= MISMATCH_GAP,
    thin_sample    = trips_off < MIN_TRIPS | trips_def < MIN_TRIPS
  )

dropped <- schedule |> filter(!game_id %in% sides$game_id)
message(sprintf("%s: %d games on the schedule, %d with two rated FBS teams (%d skipped, e.g., FCS opponents)",
                wk_label, nrow(schedule), n_distinct(sides$game_id), nrow(dropped)))

## Game level: quality, clash, and profile tags ----
# quality   = average percentile of the four red zone units on the field
# clash     = the stronger of the two strength-vs-strength pairings (the lower of
#             that offense's and defense's percentiles)
# intrigue  = average of quality and clash, 0-100. It ranks how much red zone
#             talent meets head-on; it says nothing about who wins.
games <- sides |>
  summarise(
    sides        = n(),
    quality      = mean(c(off_pct, def_pct)),
    clash        = max(pmin(off_pct, def_pct)),
    max_gap      = max(abs(gap)),
    any_strength = any(strength_clash),
    any_weakness = any(weakness_clash),
    any_mismatch = any(mismatch),
    track_meet   = all(off_pct >= PROFILE_HI) & all(def_pct <= PROFILE_LO),
    grind        = all(def_pct >= PROFILE_HI) & all(off_pct <= PROFILE_LO),
    thin_sample  = any(thin_sample),
    .by = c(game_id, game_label, home_team, away_team, neutral_site)
  ) |>
  filter(sides == 2) |>
  mutate(
    intrigue = (quality + clash) / 2,
    tags = pmap_chr(
      list(any_strength, any_mismatch, track_meet, grind, any_weakness),
      \(s, m, t, g, w) paste(c("Strength vs. Strength", "Mismatch", "Track Meet Profile",
                               "Goal-Line Grind", "Weakness vs. Weakness")[c(s, m, t, g, w)],
                             collapse = " | ")
    )
  ) |>
  arrange(desc(intrigue))

print(games |> select(game_label, intrigue, quality, clash, max_gap, tags), n = 25)

featured <- games |> slice_head(n = N_FEATURED)

write_csv(sides |> select(-off_logo, -def_logo) |> arrange(desc(abs(gap))),
          file.path(DIR_OUTPUTS, paste0(OUT_PREFIX, "week", PREVIEW_WEEK, "_matchups.csv")))
write_csv(games,
          file.path(DIR_OUTPUTS, paste0(OUT_PREFIX, "week", PREVIEW_WEEK, "_games.csv")))

# Plot 1: Matchup Map ----
map_df <- sides |> mutate(logo = off_logo)
map_flagged <- map_df |> filter(strength_clash | mismatch | weakness_clash)

p <- plot_logo_quadrant(
  map_df, "off_pct", "def_pct",
  title    = paste0("Red Zone Matchup Map: ", wk_label),
  subtitle = "Each logo is an offense, placed against the defense it faces this week",
  x_lab    = "Offense: Adjusted Red Zone Points per Trip (FBS Percentile)",
  y_lab    = "Opposing Defense: Adjusted Points Allowed per Trip\n(FBS Percentile, Higher = Stingier)",
  caption  = paste(CAP_DATA, CAP_RZ, CAP_RIDGE, CAP_PCT,
                   "Labeled: strength vs. strength, weakness vs. weakness, and the widest mismatches.",
                   CAP_NOTE, sep = "\n"),
  reverse_y = FALSE, quadrant_labels = LABELS_PREVIEW, label_alpha = 0.6
) +
  geom_text(data = map_flagged, aes(label = paste("vs.", defense)),
            size = 2.4, vjust = 2.6, color = "gray30")
save_plot(p, paste0("scatter_week", PREVIEW_WEEK, "_matchup_map"), width = 12, height = 8.5)

# Plot 2: Must-Watch Red Zone Games ----
featured_plot <- featured |>
  left_join(team_rz |> select(away_team = team, away_logo = logo), by = "away_team") |>
  left_join(team_rz |> select(home_team = team, home_logo = logo), by = "home_team") |>
  mutate(game_label = fct_reorder(game_label, intrigue),
         tags = if_else(tags == "", "-", tags))

p <- ggplot(featured_plot, aes(x = intrigue, y = game_label)) +
  geom_col(fill = "#1b5e20", alpha = 0.65, width = 0.7) +
  geom_text(aes(label = round(intrigue)), hjust = 1.3, color = "white", fontface = "bold") +
  geom_from_path(aes(x = 104, path = away_logo), width = 0.035) +
  geom_from_path(aes(x = 113, path = home_logo), width = 0.035) +
  geom_text(aes(x = 120, label = tags), hjust = 0, size = 3.4, color = "gray25") +
  add_watermark(150, 1) +
  scale_x_continuous(limits = c(0, 160), breaks = seq(0, 100, 25), expand = expansion(mult = 0.01)) +
  labs(
    title = paste0("Must-Watch Red Zone Games: ", wk_label),
    subtitle = "Where the most red zone talent meets head-on (away team logo first)",
    x = "Red Zone Intrigue Index (0-100)", y = NULL,
    caption = paste(CAP_DATA,
                    "Intrigue index = average of (1) the mean FBS percentile of all four red zone units and (2) the best strength-vs-strength pairing.",
                    CAP_RIDGE, CAP_NOTE, sep = "\n")
  ) +
  theme_rz() +
  theme(panel.grid.major.y = element_blank())
save_plot(p, paste0("bar_week", PREVIEW_WEEK, "_must_watch"), width = 13, height = 7)

# Plot 3: Tale of the Tape for the Featured Games ----
# For each side: the offense's percentile vs. the opposing defense's, on getting
# there (trip rate per drive) and cashing in (points per trip).
tape <- sides |>
  filter(game_id %in% featured$game_id) |>
  select(game_id, game_label, side_label, off_logo, def_logo,
         off_pct, def_pct, off_access_pct, def_access_pct) |>
  pivot_longer(c(off_pct, def_pct, off_access_pct, def_access_pct),
               names_to = c("unit", "metric"), names_pattern = "(off|def)_(.*)", values_to = "pct") |>
  pivot_wider(names_from = unit, values_from = pct) |>
  mutate(
    metric     = factor(if_else(metric == "pct", "Cashing In: Points per Trip", "Getting There: Trip Rate per Drive"),
                        levels = c("Getting There: Trip Rate per Drive", "Cashing In: Points per Trip")),
    gap        = off - def,
    edge       = case_when(gap >=  MISMATCH_GAP / 2 ~ "Offense edge",
                           gap <= -MISMATCH_GAP / 2 ~ "Defense edge",
                           TRUE ~ "Even"),
    game_label = factor(game_label, levels = featured$game_label),
    game_strip = str_wrap(as.character(game_label), 20)
  ) |>
  mutate(game_strip = fct_reorder(game_strip, as.integer(game_label)))

tape_wm <- watermark_path |>
  mutate(game_strip = factor(last(levels(tape$game_strip)), levels = levels(tape$game_strip)),
         metric = factor(last(levels(tape$metric)), levels = levels(tape$metric)),
         side_label = tape$side_label[tape$game_strip == game_strip][1])

p <- ggplot(tape, aes(y = side_label)) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "gray60") +
  geom_segment(aes(x = def, xend = off, yend = side_label, color = edge), linewidth = 1.6, alpha = 0.7) +
  geom_from_path(aes(x = def, path = def_logo), width = 0.04, alpha = 0.55) +
  geom_from_path(aes(x = off, path = off_logo), width = 0.04) +
  geom_from_path(data = tape_wm, aes(x = -2, y = side_label, path = image_path),
                 width = 0.03, alpha = 0.8, inherit.aes = FALSE) +
  facet_grid(game_strip ~ metric, scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(limits = c(-5, 105), breaks = seq(0, 100, 25)) +
  scale_color_manual(values = COL_EDGE) +
  labs(
    title = paste0("Tale of the Tape: ", wk_label),
    subtitle = "Each row is one offense vs. the defense across the field (solid logo = offense, faded = defense)",
    x = "FBS Percentile (100 = best offense / stingiest defense)", y = NULL, color = NULL,
    caption = paste(CAP_DATA, CAP_RZ, CAP_RIDGE,
                    "Getting there = share of drives reaching the red zone. Cashing in = points per red zone trip.",
                    CAP_NOTE, sep = "\n")
  ) +
  theme_rz() +
  theme(
    legend.position = "top",
    strip.placement = "outside",
    strip.text.y.left = element_text(angle = 0, face = "bold", hjust = 1),
    strip.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 8),
    panel.spacing.y = unit(0.6, "lines")
  )
save_plot(p, paste0("dumbbell_week", PREVIEW_WEEK, "_tale_of_the_tape"), width = 14, height = 2 + 1.3 * N_FEATURED)

# Plot 4: Biggest Mismatches ----
mismatch_plot <- sides |>
  slice_max(abs(gap), n = N_MISMATCH, with_ties = FALSE) |>
  mutate(
    side_label = fct_reorder(side_label, gap),
    edge = if_else(gap >= 0, "Offense edge", "Defense edge")
  )

p <- ggplot(mismatch_plot, aes(x = gap, y = side_label, fill = edge)) +
  geom_col(width = 0.7, alpha = 0.75) +
  geom_vline(xintercept = 0) +
  geom_from_path(aes(path = off_logo, hjust = if_else(gap >= 0, -0.3, 1.3)), width = 0.03) +
  add_watermark(90, 1) +
  scale_fill_manual(values = COL_EDGE) +
  scale_x_continuous(limits = c(-110, 110), breaks = seq(-100, 100, 25)) +
  labs(
    title = paste0("Mind the Gap: ", wk_label, "'s Biggest Red Zone Mismatches"),
    subtitle = "Offense percentile minus opposing defense percentile, adjusted points per trip (logo = offense)",
    x = "Percentile Gap (positive = offense rates higher, negative = defense rates higher)", y = NULL, fill = NULL,
    caption = paste(CAP_DATA, CAP_RIDGE, CAP_PCT, CAP_NOTE, sep = "\n")
  ) +
  theme_rz() +
  theme(legend.position = "top")
save_plot(p, paste0("bar_week", PREVIEW_WEEK, "_mismatches"), width = 12, height = 9)

# Written Insights ----
# Short, post-ready lines. They describe the matchup on paper and stop there.
line_strength <- sides |>
  filter(strength_clash) |>
  arrange(desc(pmin(off_pct, def_pct))) |>
  slice_head(n = 3) |>
  mutate(text = sprintf(
    "Strength vs. strength: %s's red zone offense (%s percentile, %+.1f pts/trip vs. avg) runs into %s's defense (%s percentile, %+.1f). Somebody's streak has to bend.",
    offense, ordinal(off_pct), off_vs_avg, defense, ordinal(def_pct), def_vs_avg)) |>
  pull(text)

line_mismatch <- sides |>
  filter(mismatch) |>
  slice_max(abs(gap), n = 3, with_ties = FALSE) |>
  mutate(text = if_else(
    gap > 0,
    sprintf("%s: %s's offense (%s percentile) vs. %s's defense (%s). On paper, the offense has been the better unit by %d percentile points.",
            if_else(row_number() == 1, "Biggest gap on the board", "Mind the gap"),
            offense, ordinal(off_pct), defense, ordinal(def_pct), round(abs(gap))),
    sprintf("Brick wall alert: %s's defense (%s percentile) vs. %s's offense (%s). On paper, the defense has been the better unit by %d percentile points.",
            defense, ordinal(def_pct), offense, ordinal(off_pct), round(abs(gap)))
  )) |>
  pull(text)

line_games <- c(
  if (nrow(featured) > 0) sprintf(
    "Top of the must-watch list: %s (intrigue index %d). %s",
    featured$game_label[1], round(featured$intrigue[1]),
    if (featured$tags[1] == "") "" else paste0("Tags: ", featured$tags[1], ".")),
  games |> filter(track_meet) |> slice_head(n = 2) |>
    mutate(text = sprintf("Track meet profile: %s. Both offenses finish drives and neither defense has slammed the door.", game_label)) |>
    pull(text),
  games |> filter(grind) |> slice_head(n = 2) |>
    mutate(text = sprintf("Goal-line grind profile: %s. Two stingy red zone defenses and two offenses that have had to fight for every yard inside the 20.", game_label)) |>
    pull(text),
  games |> filter(any_weakness) |> slice_head(n = 1) |>
    mutate(text = sprintf("Leaky roof, rusty bucket: %s has a bottom-quartile offense facing a bottom-quartile defense. Something has to give, just not necessarily in a good way.", game_label)) |>
    pull(text)
)

thin_note <- if (any(sides$thin_sample)) {
  sprintf("Sample-size note: %d of %d matchups involve a unit with fewer than %d red zone trips, so treat those ratings as rough.",
          sum(sides$thin_sample), nrow(sides), MIN_TRIPS)
}

insights <- c(line_games, line_strength, line_mismatch, thin_note)
cat(paste0("- ", insights), sep = "\n")

writeLines(
  c(paste0("# Red Zone Preview: ", SEASON, " ", wk_label), "",
    paste0("Ratings through week ", max(weeks_played), ". ", CAP_NOTE), "",
    paste0("- ", insights)),
  file.path(DIR_OUTPUTS, paste0(OUT_PREFIX, "week", PREVIEW_WEEK, "_insights.md"))
)

# Save Results ----
saveRDS(
  list(
    season         = SEASON,
    preview_week   = PREVIEW_WEEK,
    weeks_played   = weeks_played,
    thresholds     = list(min_trips = MIN_TRIPS, strong_pct = STRONG_PCT, weak_pct = WEAK_PCT,
                          mismatch_gap = MISMATCH_GAP, profile_hi = PROFILE_HI, profile_lo = PROFILE_LO),
    league_ppt     = league_ppt,
    team_rz        = team_rz,
    sides          = sides,
    games          = games,
    insights       = insights,
    lambda_points  = attr(ratings_points, "lambda"),
    lambda_tds     = attr(ratings_tds, "lambda"),
    lambda_access  = attr(ratings_access, "lambda")
  ),
  file.path(DIR_OUTPUTS, paste0(OUT_PREFIX, "results.rds"))
)
