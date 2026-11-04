# Header ----
#
# Script name: 02_05_cfb_red_zone_game_preview.R
#
# Purpose of script: Pre-game red zone scouting report, one page per team per
#   game, for the upcoming week. Each page lines a team up against this week's
#   opponent on six opponent-adjusted red zone measures:
#     1. Header: team colors, both logos, venue, sample sizes
#     2. "When we have the ball": our offense vs. their defense (FBS percentiles)
#     3. "When they have the ball": our defense vs. their offense
#     4. Season ladder: every game so far, trips by outcome for and against
#     5. Scouting notes: offensive and defensive identity, biggest edges, sample size
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
# - A look at the data before kickoff, not a forecast (same rule as 02_04): no
#   win probabilities, spreads, or projected scores. "Edge" means one unit has
#   rated higher than the other on season-to-date numbers, nothing more.
# - Only games with at least one Power 5 team (conferences_p5 in 00_00) and two
#   rated FBS teams are included. FCS opponents have no ratings, so those games
#   are skipped (the console says how many).
# - All six measures are opponent-adjusted ridge ratings (fit_rz_ratings() in
#   00_01), neutral site vs. an average opponent. The season ladder is the one
#   raw panel: it shows what actually happened in each game.
# - Percentiles rank each unit among FBS teams (100 = best; for defenses, best =
#   stingiest). Units with fewer than MIN_TRIPS red zone trips are flagged on
#   the page, because their ratings lean heavily on the league average.
# - The upcoming week's schedule is read from the same "schedule" cache as 02_04.
# - Needs the patchwork package for the page layout (install.packages("patchwork")).
#

rm(list = ls())

# Working Directories, Libraries, Functions, Values ----

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("00_00_dirs_libs_funs_vals.R")
source("00_01_cfb_functions.R")
library(patchwork)

# Settings ----
SEASON           <- 2026
SEASON_TYPE      <- "regular"
WEEKS            <- NULL         # ratings data: NULL = all completed weeks; or e.g. 1:9
REFRESH_WEEKS    <- integer(0)   # weeks to re-pull from the API
PREVIEW_WEEK     <- NULL         # NULL = the week after the latest completed week
REFRESH_SCHEDULE <- FALSE        # TRUE = re-pull the preview week's schedule
RZ_LINE          <- 20
GARBAGE_MARGIN   <- 28
P5_CONFS         <- conferences_p5   # a game qualifies if either team is in one of these
PAGES_FOR        <- "both"       # "both" = a page for each team in a qualifying game; "p5" = P5 teams only
PAGE_TEAMS       <- NULL         # NULL = every qualifying team; or e.g. c("Ohio State", "Penn State")
MIN_TRIPS        <- 8            # units with fewer red zone trips are flagged "thin sample"
EDGE_GAP         <- 25           # percentile gap between two units to call it an edge in the notes
IDENTITY_SPLIT   <- 50           # percentile that splits "good" and "poor" for the identity tags
COMBINED_PDF     <- TRUE         # also write every page into one PDF for the week
PAGE_W           <- 9            # page size in inches
PAGE_H           <- 12
SEED             <- 2026
OUT_PREFIX       <- "proj_012_02_05_"

CAP_DATA  <- "Data: cfbfastR, ggplot2, ggpath, patchwork"
CAP_RZ    <- "Red zone trip = drive with at least one snap inside the opponent's 20. End-of-half and garbage-time drives excluded."
CAP_RIDGE <- "Ratings: ridge regression on season-to-date drives, trips, or plays with offense, defense, and home terms; neutral site vs. an average opponent."
CAP_PCT   <- "Percentile among FBS teams: 100 = best offense / stingiest defense; 50 = FBS median."
CAP_NOTE  <- "A pre-game look at season-to-date red zone ratings, not a prediction of the result."

COL_OUTCOME <- c("Touchdown" = "#2e7d32", "Field Goal" = "#fbc02d", "No Points" = "#c62828")
COL_DEFAULT <- "#37474f"         # used when a team has no color in the team info

# Identity tags: offense (getting there x cashing in) and defense (trips allowed x points allowed)
IDENTITY_OFF <- c(hh = "Six-Point Factory", hl = "All Driveway, No Garage",
                  lh = "Cashing Every Check", ll = "Stuck in the Parking Lot")
IDENTITY_DEF <- c(hh = "Brick Wall D", hl = "Bend, Don't Break",
                  lh = "Gatekeepers With a Glass Jaw", ll = "Welcome Mat")

# Script-Specific Helpers ----

## Identity tag from two percentiles ----
identity_tag <- function(p_get, p_cash, tags) {
  key <- paste0(if_else(p_get >= IDENTITY_SPLIT, "h", "l"), if_else(p_cash >= IDENTITY_SPLIT, "h", "l"))
  unname(tags[key])
}

## Butterfly: one unit vs. the unit across the field, by metric ----
# df: metric rows with left_pct / right_pct and their value labels.
butterfly <- function(df, title, subtitle, col_left, col_right, name_left, name_right) {
  df <- df |>
    mutate(
      short = factor(short, levels = rev(RZ_METRICS$short)),
      lead  = case_when(left_pct - right_pct >=  EDGE_GAP ~ "left",
                        right_pct - left_pct >= EDGE_GAP ~ "right",
                        TRUE ~ "even")
    )
  ggplot(df, aes(y = short)) +
    geom_vline(xintercept = c(-50, 50), linetype = "dashed", color = "gray70") +
    geom_col(aes(x = -left_pct, alpha = lead == "left"), fill = col_left, width = 0.68) +
    geom_col(aes(x = right_pct, alpha = lead == "right"), fill = col_right, width = 0.68) +
    geom_vline(xintercept = 0, color = "gray20") +
    geom_text(aes(x = -left_pct - 3, label = left_lab), hjust = 1, size = 2.7, color = "gray15") +
    geom_text(aes(x = right_pct + 3, label = right_lab), hjust = 0, size = 2.7, color = "gray15") +
    annotate("text", x = c(-100, 100), y = nrow(df) + 0.75, label = c(name_left, name_right),
             hjust = c(0, 1), fontface = "bold", size = 3.2, color = c(col_left, col_right)) +
    scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.45), guide = "none") +
    scale_x_continuous(limits = c(-165, 165), breaks = c(-100, -50, 0, 50, 100), labels = abs) +
    scale_y_discrete(expand = expansion(add = c(0.6, 1.1))) +
    labs(title = title, subtitle = subtitle, x = "FBS Percentile", y = NULL) +
    theme_rz(base_size = 10, subtitle_size = 9) +
    theme(plot.title = element_text(size = 13), panel.grid.major.y = element_blank(),
          axis.text.y = element_text(face = "bold", size = 9))
}

# Datasets ----
raw   <- load_cfb_season(SEASON, WEEKS, season_type = SEASON_TYPE, overwrite_weeks = REFRESH_WEEKS)
rz    <- build_rz_tables(raw, rz_line = RZ_LINE, garbage_margin = GARBAGE_MARGIN)
trips <- add_poe(rz$prod_rz_drives)
rz$prod_rz_drives <- trips
plays <- build_rz_plays(rz, rz_line = RZ_LINE)

weeks_played <- sort(unique(trips$week))
if (is.null(PREVIEW_WEEK)) PREVIEW_WEEK <- max(weeks_played) + 1
wk_label <- paste0("Week ", PREVIEW_WEEK)
message(sprintf("Red zone game previews for %s, using ratings through week %d", wk_label, max(weeks_played)))

## Team colors (team_info in 00_01 keeps only logo and conference) ----
team_info_full <- get_team_info()
team_colors <- tibble(
  team  = team_info_full$school,
  color = clean_color(if ("color" %in% names(team_info_full)) team_info_full$color else NA_character_, COL_DEFAULT)
)

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

## Opponent-adjusted ratings, all six measures ----
set.seed(SEED)
ratings <- fit_rz_ratings(rz$drives_clean, trips, plays)

## Sample sizes ----
samples <- full_join(
  count(trips, team = offense, name = "trips_off"),
  count(trips, team = defense, name = "trips_def"),
  by = "team"
) |>
  mutate(across(c(trips_off, trips_def), \(v) coalesce(v, 0L)))

## Team x metric table with FBS percentiles ----
team_metric <- ratings |>
  add_team_info() |>
  mutate(off_pct = pctile(adj_off), def_pct = pctile(-adj_def), .by = metric) |>
  left_join(RZ_METRICS |> select(metric, short, unit), by = "metric")

thin_metrics <- team_metric |>
  summarise(spread = sd(adj_off), .by = metric) |>
  filter(spread < 1e-3)
if (nrow(thin_metrics) > 0) {
  warning("Ratings are nearly flat (little signal yet) for: ", paste(thin_metrics$metric, collapse = ", "),
          ". Percentiles will exaggerate tiny differences.", call. = FALSE)
}

team_card <- team_metric |>
  distinct(team, logo, conference) |>
  left_join(samples, by = "team") |>
  left_join(team_colors, by = "team") |>
  mutate(color = coalesce(color, COL_DEFAULT))

## Qualifying games and pages ----
games <- schedule |>
  left_join(team_card |> select(home_team = team, home_conf = conference), by = "home_team") |>
  left_join(team_card |> select(away_team = team, away_conf = conference), by = "away_team") |>
  mutate(
    has_p5 = home_conf %in% P5_CONFS | away_conf %in% P5_CONFS,
    rated  = !is.na(home_conf) & !is.na(away_conf),
    game_label = if_else(neutral_site %in% TRUE,
                         paste(away_team, "vs.", home_team),
                         paste(away_team, "at", home_team))
  )

message(sprintf("%s: %d games on the schedule, %d with a P5 team, %d of those with two rated FBS teams",
                wk_label, nrow(games), sum(games$has_p5), sum(games$has_p5 & games$rated)))
games <- games |> filter(has_p5, rated)
if (nrow(games) == 0) stop("No qualifying games for ", wk_label, ".", call. = FALSE)

pages <- bind_rows(
  games |> mutate(team = home_team, opp = away_team, team_conf = home_conf,
                  venue = if_else(neutral_site %in% TRUE, "vs.", "hosts")),
  games |> mutate(team = away_team, opp = home_team, team_conf = away_conf,
                  venue = if_else(neutral_site %in% TRUE, "vs.", "at"))
) |>
  filter(PAGES_FOR == "both" | team_conf %in% P5_CONFS) |>
  filter(is.null(PAGE_TEAMS) | team %in% PAGE_TEAMS) |>
  arrange(across(any_of("start_date")), game_label, team)

message(sprintf("Building %d preview pages", nrow(pages)))

## Season results by game, raw (for the ladder) ----
side_games <- rz_side_metrics(rz$drives_clean, trips, plays)

# Page Builder ----
build_preview_page <- function(tm, op, venue, game_label) {
  card_t <- team_card |> filter(team == tm)
  card_o <- team_card |> filter(team == op)
  col_t  <- card_t$color
  col_o  <- card_o$color
  if (col_t == col_o) col_o <- COL_DEFAULT   # keep the two sides apart when colors match

  m_t <- team_metric |> filter(team == tm)
  m_o <- team_metric |> filter(team == op)

  ## Header ----
  thin_t <- card_t$trips_off < MIN_TRIPS | card_t$trips_def < MIN_TRIPS
  thin_o <- card_o$trips_off < MIN_TRIPS | card_o$trips_def < MIN_TRIPS
  header <- page_header(
    col_t, card_t$logo, card_o$logo,
    line1 = str_to_upper(tm),
    line2 = paste(venue, op, "\u00b7", wk_label, SEASON, "\u00b7 Red Zone Scouting Report"),
    line3 = sprintf("Season to date through week %d \u00b7 %s: %d trips for, %d against \u00b7 %s: %d for, %d against",
                    max(weeks_played), tm, card_t$trips_off, card_t$trips_def,
                    op, card_o$trips_off, card_o$trips_def)
  )

  ## Butterflies ----
  side_o <- m_t |>
    select(metric, short, unit, left_pct = off_pct, left_val = adj_off) |>
    inner_join(m_o |> select(metric, right_pct = def_pct, right_val = adj_def), by = "metric") |>
    mutate(left_lab  = paste0(fmt_metric(left_val, unit), " \u00b7 ", ordinal(left_pct)),
           right_lab = paste0(ordinal(right_pct), " \u00b7 ", fmt_metric(right_val, unit)))
  side_d <- m_t |>
    select(metric, short, unit, left_pct = def_pct, left_val = adj_def) |>
    inner_join(m_o |> select(metric, right_pct = off_pct, right_val = adj_off), by = "metric") |>
    mutate(left_lab  = paste0(fmt_metric(left_val, unit), " \u00b7 ", ordinal(left_pct)),
           right_lab = paste0(ordinal(right_pct), " \u00b7 ", fmt_metric(right_val, unit)))

  bf_off <- butterfly(side_o, paste("When", tm, "Has the Ball"),
                      paste0(tm, " offense vs. ", op, " defense (value \u00b7 FBS percentile)"),
                      col_t, col_o, paste(tm, "O"), paste(op, "D"))
  bf_def <- butterfly(side_d, paste("When", op, "Has the Ball"),
                      paste0(tm, " defense (allowed) vs. ", op, " offense"),
                      col_t, col_o, paste(tm, "D"), paste(op, "O"))

  ## Season ladder ----
  ladder <- bind_rows(
    side_games |> filter(offense == tm) |> mutate(opp = defense, side = "For"),
    side_games |> filter(defense == tm) |> mutate(opp = offense, side = "Against")
  ) |>
    select(game_id, week, opp, side, Touchdown = tds, `Field Goal` = fgs, `No Points` = empty) |>
    pivot_longer(c(Touchdown, `Field Goal`, `No Points`), names_to = "outcome", values_to = "n") |>
    mutate(
      outcome = factor(outcome, levels = names(COL_OUTCOME)),
      n_signed = if_else(side == "For", n, -n),
      # same stacking order above and below the line (no points nearest it)
      outcome_stack = factor(outcome, levels = rev(names(COL_OUTCOME)))
    )

  ladder_games <- ladder |>
    distinct(week, opp) |>
    left_join(team_card |> select(opp = team, opp_logo = logo), by = "opp")
  y_max <- max(5, ladder |> filter(side == "For") |> summarise(t = sum(n), .by = week) |> pull(t))
  y_min <- min(-5, -(ladder |> filter(side == "Against") |> summarise(t = sum(n), .by = week) |> pull(t)))

  p_ladder <- ggplot(ladder, aes(x = week, y = n_signed, fill = outcome)) +
    geom_col(aes(group = outcome_stack), width = 0.7, position = position_stack(reverse = TRUE),
             data = \(d) filter(d, side == "For")) +
    geom_col(aes(group = outcome_stack), width = 0.7, alpha = 0.55, position = position_stack(reverse = TRUE),
             data = \(d) filter(d, side == "Against")) +
    geom_hline(yintercept = 0, color = "gray20") +
    annotate("text", x = min(weeks_played) - 0.75, y = c(y_max, y_min),
             label = c(paste(tm, "trips"), "Trips allowed"), hjust = 0, vjust = c(1, 0),
             size = 3, fontface = "bold", color = "gray30") +
    scale_fill_manual(values = COL_OUTCOME) +
    scale_x_continuous(breaks = weeks_played, limits = c(min(weeks_played) - 0.8, max(weeks_played) + 0.6)) +
    scale_y_continuous(limits = c(y_min - 1.8, y_max + 0.5), labels = abs,
                       breaks = scales::breaks_pretty(6)) +
    labs(
      title = "The Season So Far Inside the 20",
      subtitle = paste0("Red zone trips by outcome each game (raw counts): above the line = ", tm,
                        " offense, below = trips allowed. Logos = opponent."),
      x = "Week", y = "Red Zone Trips", fill = NULL
    ) +
    theme_rz(base_size = 10, subtitle_size = 9) +
    theme(plot.title = element_text(size = 13), legend.position = "top", panel.grid.major.x = element_blank())
  if (nrow(ladder_games) > 0) {
    p_ladder <- p_ladder +
      geom_from_path(data = ladder_games, aes(x = week, y = y_min - 1.1, path = opp_logo),
                     width = 0.045, inherit.aes = FALSE)
  }

  ## Scouting notes ----
  pct_of <- function(m, tm_metric, col) tm_metric[[col]][tm_metric$metric == m]
  id_off_t <- identity_tag(pct_of("access", m_t, "off_pct"), pct_of("points", m_t, "off_pct"), IDENTITY_OFF)
  id_def_t <- identity_tag(pct_of("access", m_t, "def_pct"), pct_of("points", m_t, "def_pct"), IDENTITY_DEF)
  id_off_o <- identity_tag(pct_of("access", m_o, "off_pct"), pct_of("points", m_o, "off_pct"), IDENTITY_OFF)
  id_def_o <- identity_tag(pct_of("access", m_o, "def_pct"), pct_of("points", m_o, "def_pct"), IDENTITY_DEF)

  gaps <- bind_rows(
    side_o |> mutate(side = "o"),
    side_d |> mutate(side = "d")
  ) |>
    mutate(gap = left_pct - right_pct)
  best  <- gaps |> slice_max(gap, n = 1, with_ties = FALSE)
  worst <- gaps |> slice_min(gap, n = 1, with_ties = FALSE)

  edge_line <- function(row, favorable) {
    unit_t <- if (row$side == "o") paste0(tm, "'s offense") else paste0(tm, "'s defense")
    unit_o <- if (row$side == "o") paste0(op, "'s defense") else paste0(op, "'s offense")
    if (abs(row$gap) < EDGE_GAP) {
      return(sprintf("%s: no gap of %d+ percentile points anywhere on the page. On paper, this one is even inside the 20.",
                     if (favorable) "Biggest edge" else "Biggest worry", EDGE_GAP))
    }
    if (favorable) {
      sprintf("Biggest edge: %s (%s percentile) vs. %s (%s) in %s. On paper, %s has been the better unit by %d points.",
              unit_t, ordinal(row$left_pct), unit_o, ordinal(row$right_pct), row$short,
              unit_t, round(row$gap))
    } else {
      sprintf("Biggest worry: %s (%s percentile) vs. %s (%s) in %s. On paper, %s has been the better unit by %d points.",
              unit_t, ordinal(row$left_pct), unit_o, ordinal(row$right_pct), row$short,
              unit_o, round(-row$gap))
    }
  }

  notes <- c(
    sprintf("%s offense: \"%s\" (getting there %s pct., cashing in %s). %s defense: \"%s\" (trips allowed %s, points allowed %s).",
            tm, id_off_t, ordinal(pct_of("access", m_t, "off_pct")), ordinal(pct_of("points", m_t, "off_pct")),
            tm, id_def_t, ordinal(pct_of("access", m_t, "def_pct")), ordinal(pct_of("points", m_t, "def_pct"))),
    sprintf("Across the field, %s brings \"%s\" on offense and \"%s\" on defense.", op, id_off_o, id_def_o),
    edge_line(best, TRUE),
    edge_line(worst, FALSE),
    if (thin_t || thin_o) {
      sprintf("Small-sample alert: %s has fewer than %d red zone trips on one side of the ball, so its ratings lean on the league average.",
              paste(c(tm, op)[c(thin_t, thin_o)], collapse = " and "), MIN_TRIPS)
    } else {
      "Red zone samples are small all season (a few trips a game), so read a 10-point percentile gap as a nudge, not a verdict."
    }
  )
  p_notes <- notes_panel(notes, "Scouting Notes")

  ## Assemble ----
  header / (bf_off | bf_def) / p_ladder / p_notes +
    plot_layout(heights = c(1.15, 3.1, 2.9, 1.5)) +
    plot_annotation(
      caption = paste(CAP_DATA, CAP_RZ, CAP_RIDGE, CAP_PCT,
                      "Bars: solid = the unit that rates higher by at least 25 percentile points; POE = points over expected given where the trip started.",
                      CAP_NOTE, sep = "\n"),
      theme = theme(plot.caption = element_text(hjust = 0, size = 7.5, color = "gray30"))
    )
}

# Build and Save Pages ----
page_files <- character(0)
page_list  <- vector("list", nrow(pages))
for (i in seq_len(nrow(pages))) {
  pg <- pages[i, ]
  p  <- build_preview_page(pg$team, pg$opp, pg$venue, pg$game_label)
  page_list[[i]] <- p
  name <- paste0("week", PREVIEW_WEEK, "_preview_", slug(pg$team), "_vs_", slug(pg$opp))
  save_plot(p, name, width = PAGE_W, height = PAGE_H)
  page_files <- c(page_files, paste0(OUT_PREFIX, name, ".png"))
}

if (COMBINED_PDF && length(page_list) > 0) {
  pdf_path <- file.path(DIR_OUTPUTS, paste0(OUT_PREFIX, "week", PREVIEW_WEEK, "_previews.pdf"))
  grDevices::cairo_pdf(pdf_path, width = PAGE_W, height = PAGE_H, onefile = TRUE)
  walk(page_list, print)
  dev.off()
  message("Combined PDF: ", pdf_path)
}

# Save Results ----
saveRDS(
  list(
    season        = SEASON,
    preview_week  = PREVIEW_WEEK,
    weeks_played  = weeks_played,
    thresholds    = list(min_trips = MIN_TRIPS, edge_gap = EDGE_GAP, identity_split = IDENTITY_SPLIT),
    p5_confs      = P5_CONFS,
    metrics       = RZ_METRICS,
    team_metric   = team_metric,
    team_card     = team_card,
    games         = games,
    pages         = pages |> mutate(file = page_files),
    lambdas       = attr(ratings, "lambda")
  ),
  file.path(DIR_OUTPUTS, paste0(OUT_PREFIX, "results.rds"))
)
