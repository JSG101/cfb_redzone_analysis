# Header ----
#
# Script name: 02_01_cfb_red_zone_success.R
#
# Purpose of script: Answer the question: for teams that make the redzone, how successful are they at scoring?
#
# Location of script:
#
# Author: Japbir Gill, MBA MSEd
#
# Date Created: 2026-09-17
#
# Copyright (c) Japbir Gill, 2026
# Email: Japbir.Gill@gmail.com
#
# Notes ----
# https://cloud.r-project.org/web/packages/cfbfastR/refman/cfbfastR.html#cfbfastR-package
#   
#

rm(list=ls())

# Working Directories, Libraries, Functions, Values ----

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("00_00_dirs_libs_funs_vals.R")

# Formatting and Global Options ----

# Raw Datasets ----

## Game Drives ----
raw_game_drives <- 
  read.csv(
    file.path(DIR_INPUTS, "proj_012_01_01_cfb_redzone_raw_game_drives.csv"),
    header = TRUE
    )

## Play by Play ----
raw_play_by_play <-  
  read.csv(
    file.path(DIR_INPUTS, "proj_012_01_01_cfb_redzone_raw_play_by_play.csv"),
    header = TRUE
  )
  
## Team Information ----
raw_team_info <- 
  read.csv(
    file.path(DIR_INPUTS, "proj_012_01_01_cfb_redzone_raw_team_info.csv"),
    header = TRUE
  )

# Prepared Datasets ----

## Play by Play ----
prod_play_by_play <- 
  raw_play_by_play |> 
  filter(
    yards_to_goal <= 20,
    !play_type %in% c("Kickoff", "Kickoff Return (Offense)", "Timeout", "Punt Return", 
                      "End Period", "End of Half", "End of Game")
  ) |>
  group_by(drive_id) |>
  summarise(
    first_rz_ytg = max(yards_to_goal),     # entry point into the red zone
    rz_ep_entry  = first(ep_before),       # EP at first RZ snap (for points-over-expected)
    # .by = drive_id,
    .groups = "drop"
  ) 

## Game Drives ----
prod_game_drives <- 
  # start with game drives
  raw_game_drives |> 
  # filter for those that made the red zone
  # filter(end_yards_to_goal <= 20) |> 
  inner_join(prod_play_by_play, by = join_by(drive_id)) |> 
  # create fields
  mutate(
    # is a red zone drive 
    # flag_red_zone_drive = end_yards_to_goal <= 20,
    # conference or non-conference game
    game_type = ifelse(offense_conference == defense_conference, "In-Conference", "Out of Conference"),
    # points scored on drive
    drive_points = case_when(
      drive_result %in% c("TD") ~ 6,
      drive_result %in% c("FG") ~ 3,
      # drive_result %in% c("FUMBLE RETURN TD", "FUMBLE TD", "INT TD", "MISSED FG TD") ~ -7,
      TRUE ~ 0
    ),
    # scoring flags
    flag_score = ifelse(drive_result %in% c("TD", "FG") , 1, 0),
    flag_td = ifelse(drive_result %in% c("TD"), 1, 0),
    flag_fg = ifelse(drive_result %in% c("FG"), 1, 0)
  ) 

# Opponent adjusted red zone efficiency - Points
coef_rating_points <- 
  coef(
    cv.glmnet(
      model.matrix(~ offense + defense, data = prod_game_drives,
                   contrasts.arg = list(offense = contrasts(factor(prod_game_drives$offense), contrasts = FALSE),
                                        defense = contrasts(factor(prod_game_drives$defense), contrasts = FALSE)))[, -1],
      prod_game_drives$drive_points, 
      family = "gaussian", alpha = 0
    ), 
    s = "lambda.min")

team_ratings_points_df <- 
  as.data.frame(as.matrix(coef_rating_points)) |> 
  rownames_to_column(var = "term") |>
  rename(coef = 2) |>
  filter(term != "(Intercept)")

off_ratings_points <- team_ratings_points_df |>
  filter(str_detect(term, "^offense")) |>
  mutate(
    team = str_remove(term, "^offense"),
    adj_off_rz_coef = coef
  ) |>
  select(team, adj_off_rz_coef)

def_ratings_points <- team_ratings_points_df |>
  filter(str_detect(term, "^defense")) |>
  mutate(
    team = str_remove(term, "^defense"),
    adj_def_rz_coef = coef
  ) |>
  select(team, adj_def_rz_coef)

# Opponent adjusted red zone efficiency - TDs
coef_ratings_tds <- 
  coef(
    cv.glmnet(
      model.matrix(~ offense + defense, data = prod_game_drives,
                   contrasts.arg = list(offense = contrasts(factor(prod_game_drives$offense), contrasts = FALSE),
                                        defense = contrasts(factor(prod_game_drives$defense), contrasts = FALSE)))[, -1],
      prod_game_drives$flag_td,
      family= "binomial", alpha = 0, standardize = FALSE
      ),
    s = "lambda.min"
    )

team_ratings_tds_df <- as.data.frame(as.matrix(coef_ratings_tds)) |> 
  rownames_to_column(var = "term") |>
  rename(coef = 2) |>
  filter(term != "(Intercept)")

off_ratings_tds <- team_ratings_tds_df |> 
  filter(str_detect(term, "^offense")) |>
  mutate(
    team = str_remove(term, "^offense"),
    adj_off_rz_coef = coef
  ) |>
  select(team, adj_off_rz_coef)

def_ratings_tds <- team_ratings_tds_df |> 
  filter(str_detect(term, "^defense")) |>
  mutate(
    team = str_remove(term, "^defense"),
    adj_def_rz_coef = coef
  ) |>
  select(team, adj_def_rz_coef)

# offsense mean and total scored by team
print(
  prod_game_drives |> 
    summarize(
      total_drives = n(),
      total_points = sum(drive_points),
      avg_points = mean(drive_points, na.rm = TRUE),
      .by = offense) |> 
    arrange(desc(avg_points)) |> 
    as.tibble(),
  n = 50
)

# defenses average scored by team 
print(
  prod_game_drives |> 
    summarize(
      total_drives = n(),
      total_points = sum(drive_points),
      avg_points = mean(drive_points, na.rm = TRUE),
      .by = defense) |> 
    arrange(avg_points) |> 
    as.tibble(),
  n = 50
)

# Red Zone Plots ----
# Plot 1: Average Points Scored ----
plot_01_data <-
  full_join(
    prod_game_drives |> 
      # filter(offense_conference %in% conferences_p5) |> 
      summarise(average_points_offense = mean(drive_points), .by = offense) |> 
      rename(team = offense),
    prod_game_drives |> 
      # filter(defense_conference %in% conferences_p5) |> 
      summarise(average_points_defense = mean(drive_points), .by = defense) |> 
      rename(team = defense),
    by = join_by(team)
  ) |> 
  # add offense and defense ratings
  left_join(off_ratings_points, by = join_by(team)) |> 
  left_join(def_ratings_points, by = join_by(team)) |> 
  mutate(
    adj_off_rz_coef = replace_na(adj_off_rz_coef, 0),
    adj_def_rz_coef = replace_na(adj_def_rz_coef, 0),
    intercept = as.numeric(coef_rating_points["(Intercept)", 1]),
    adj_off_points = intercept + adj_off_rz_coef,
    adj_def_points = intercept + adj_def_rz_coef
  ) |> 
  # add team information 
  left_join(
    raw_team_info |> 
      select(school, logo, conference) |> 
      mutate(p5_conference = conference %in% conferences_p5),
    by = join_by(team == school)
  ) |> 
  filter(!is.na(conference)) |> 
  # add fields
  mutate(
    net_team_points = average_points_offense - average_points_defense,
    net_team_points_centralized = net_team_points - mean(net_team_points), 
    net_team_points_adj = adj_off_points - adj_def_points,
    net_team_points_adj_centralized = net_team_points_adj - mean(net_team_points_adj)
  )

plot_01_mean_offense <- mean(plot_01_data$average_points_offense)
plot_01_mean_defense <- mean(plot_01_data$average_points_defense)
# placement_factors <- c(0.6, 1.40)

plot_01_mean_offense_adj = mean(plot_01_data$adj_off_points)
plot_01_mean_defense_adj = mean(plot_01_data$adj_def_points)

## Scatterplot of average points scored and allowed by team ----
label_pos <- find_quadrant_whitespace(
  df = plot_01_data, x_col = "average_points_offense", y_col = "average_points_defense",
  x_mean = plot_01_mean_offense, y_mean = plot_01_mean_defense
)
ggplot(plot_01_data, aes(x = average_points_offense, y = average_points_defense)) +
  # add reference lines
  geom_vline(aes(xintercept = plot_01_mean_offense)) +
  geom_hline(aes(yintercept = plot_01_mean_defense)) +
  # add scatterplot from data
  geom_from_path(aes(path = logo), width = 0.025) +
  # add watermark
  geom_from_path(data = watermark_path, 
                         aes(x = max(plot_01_data$average_points_offense)*0.90, 
                             y = max(plot_01_data$average_points_defense)*0.99, 
                             path = image_path), width = 0.02, alpha = 0.80, inherit.aes = FALSE) +
  # quadrant labels 
  # Top-Right: Good Offense + Good Defense
  annotate("text", 
           x = label_pos$x[label_pos$quadrant == "Top-Right"], 
           y = label_pos$y[label_pos$quadrant == "Top-Right"], 
           label = "Stress-Free\nTailgating", hjust = 0.5, vjust = 0, 
           fontface = "bold", color = "#1b5e20", size = 3) +
  
  # Top-Left: Bad Offense + Good Defense
  annotate("text", 
           x = label_pos$x[label_pos$quadrant == "Top-Left"], 
           y = label_pos$y[label_pos$quadrant == "Top-Left"], 
           label = "Throwing Pillows\nat the TV", hjust = 0.5, vjust = 0, 
           fontface = "bold", color = "#e65100", size = 3) +
  
  # Bottom-Right: Good Offense + Bad Defense
  annotate("text", 
           x = label_pos$x[label_pos$quadrant == "Bottom-Right"], 
           y = label_pos$y[label_pos$quadrant == "Bottom-Right"], 
           label = "Cardiac\nArrest\nFootball", hjust = 0.5, vjust = 0, 
           fontface = "bold", color = "#b71c1c", size = 3) +
  
  # Bottom-Left: Bad Offense + Bad Defense
  annotate("text", 
           x = label_pos$x[label_pos$quadrant == "Bottom-Left"], 
           y = label_pos$y[label_pos$quadrant == "Bottom-Left"], 
           label = "Changing to the\nNature Channel", hjust = 0.5, vjust = 0, 
           fontface = "bold", color = "#4a148c", size = 3) +
  # labels
  labs(title = "Average Red Zone Points Scored and Allowed by Team", 
       subtitle = "For Teams in FBS, 2026 Season To Date", 
       caption = "cfbfastR, ggplot2, ggpath") +
  ylab("Average Red Zone Points Allowed on Defense") + xlab("Average Red Zone Points Scored on Offense") +
  # formatting and other
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
    plot.caption = element_text(hjust = 0)
  ) +
  scale_y_reverse() 
ggsave(plot = get_last_plot(), path = DIR_OUTPUTS, filename = "proj_012_02_01_scatter_offense_defense_sores.png", 
       width = 11.5, height = 7)

## Scatterplot of ADJ average points scored and allowed by team ----
label_pos <- find_quadrant_whitespace(
  df = plot_01_data, x_col = "adj_off_points", y_col = "adj_def_points",
  x_mean = plot_01_mean_offense_adj, y_mean = plot_01_mean_defense_adj
)
ggplot(plot_01_data, aes(x = adj_off_points, y = adj_def_points)) +
  # add reference lines
  geom_vline(aes(xintercept = plot_01_mean_offense_adj)) +
  geom_hline(aes(yintercept = plot_01_mean_defense_adj)) +
  # add scatterplot from data
  geom_from_path(aes(path = logo), width = 0.025) +
  # add watermark
  geom_from_path(data = watermark_path, 
                 aes(x = max(plot_01_data$adj_off_points)*0.99, 
                     y = max(plot_01_data$adj_def_points)*0.99, 
                     path = image_path), width = 0.02, alpha = 0.80, inherit.aes = FALSE) +
  # quadrant labels 
  # Top-Right: Good Offense + Good Defense
  annotate("text", 
           x = label_pos$x[label_pos$quadrant == "Top-Right"], 
           y = label_pos$y[label_pos$quadrant == "Top-Right"], 
           label = "Stress-Free\nTailgating", hjust = 0.5, vjust = 0, 
           fontface = "bold", color = "#1b5e20", size = 3) +
  
  # Top-Left: Bad Offense + Good Defense
  annotate("text", 
           x = label_pos$x[label_pos$quadrant == "Top-Left"], 
           y = label_pos$y[label_pos$quadrant == "Top-Left"], 
           label = "Throwing Pillows\nat the TV", hjust = 0.5, vjust = 0, 
           fontface = "bold", color = "#e65100", size = 3) +
  
  # Bottom-Right: Good Offense + Bad Defense
  annotate("text", 
           x = label_pos$x[label_pos$quadrant == "Bottom-Right"], 
           y = label_pos$y[label_pos$quadrant == "Bottom-Right"], 
           label = "Cardiac\nArrest\nFootball", hjust = 0.5, vjust = 0, 
           fontface = "bold", color = "#b71c1c", size = 3) +
  
  # Bottom-Left: Bad Offense + Bad Defense
  annotate("text", 
           x = label_pos$x[label_pos$quadrant == "Bottom-Left"], 
           y = label_pos$y[label_pos$quadrant == "Bottom-Left"], 
           label = "Changing to the\nNature Channel", hjust = 0.5, vjust = 0, 
           fontface = "bold", color = "#4a148c", size = 3) +
  # labels
  labs(title = "Average Red Zone Points Scored and Allowed by Team", 
       subtitle = "For Teams in FBS, 2026 Season To Date, Adjusted for Schedule Strength", 
       caption = "cfbfastR, ggplot2, ggpath. Ridge regression (L2 regularization) fit on 2026 red zone drives. Values represent expected points per drive against an average opponent.") +
  ylab("Adjusted Average Red Zone Points Allowed on Defense") + xlab("Adjusted Average Red Zone Points Scored on Offense") +
  # formatting and other
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
    plot.caption = element_text(hjust = 0)
  ) +
  scale_y_reverse() 
ggsave(plot = get_last_plot(), path = DIR_OUTPUTS, filename = "proj_012_02_01_scatter_offense_defense_sores_adj.png", 
       width = 11.5, height = 7)

## Bar Chart of mean-centered net team score ----
ggplot(plot_01_data, aes(x = reorder(team, net_team_points_centralized), y = net_team_points_centralized, 
                         fill = net_team_points_centralized)) +
  # Column Chart
  geom_col(show.legend = FALSE, alpha = 0.65) +
  # logo labels
  geom_from_path(
    aes(path = logo,
        vjust = ifelse(net_team_points_centralized >= 0, -0.2, 1.2)), 
    height = 0.025
  ) +
  # add watermark
  geom_from_path(
    data = watermark_path,
    aes(x = max(n_distinct(plot_01_data$team) - 3), 
        y = min(plot_01_data$net_team_points_centralized)*0.98, 
        path = image_path), width = 0.02, alpha = 0.80, inherit.aes = FALSE) +
  # labels
  labs(title = "Mean-Centered Average Net Red Zone Points, by Team", 
       subtitle = "For Teams in FBS, 2026 Season To Date",
       caption = "cfbfastR, ggplot2, ggpath\nCalculated as a team's net red zone points (Offensive Points Scored − Defensive Points Allowed) minus the FBS average") +
  ylab("Mean-Centered Average Net Team Points") +
  # formatting and other
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
    plot.caption = element_text(hjust = 0)
  ) +
  scale_fill_viridis_c(option = "turbo")
ggsave(plot = get_last_plot(), path = DIR_OUTPUTS, filename = "proj_012_02_01_column_net_team_points_meancentered.png", 
       width = 20, height = 6)

## Bar Chart of Adjusted mean-centered net team score ----
ggplot(plot_01_data, aes(x = reorder(team, net_team_points_adj_centralized), y = net_team_points_adj_centralized, 
                         fill = net_team_points_adj_centralized)) +
  # column graph
  geom_col(show.legend = FALSE, alpha = 0.65) +
  # logo labels
  geom_from_path(
    aes(path = logo,
        vjust = ifelse(net_team_points_adj_centralized >= 0, -0.2, 1.2)), 
    height = 0.025
  ) +
  # add watermark
  geom_from_path(
    data = watermark_path,
    aes(x = max(n_distinct(plot_01_data$team) - 3), 
        y = min(plot_01_data$net_team_points_adj_centralized)*0.98, 
        path = image_path), width = 0.02, alpha = 0.80, inherit.aes = FALSE) +
  # labels
  labs(title = "Mean-Centered Average Net Red Zone Points, by Team", 
       subtitle = "For Teams in FBS, 2026 Season To Date, Adjusted for Schedule Strength",
       caption = "cfbfastR, ggplot2, ggpath\nCalculated as a team's net red zone points (Offensive Points Scored − Defensive Points Allowed) minus the FBS average\nRidge regression (L2 regularization) fit on 2026 red zone drives. Values represent expected points per drive against an average opponent.") +
  ylab("Adjusted Mean-Centered Average Net Team Points") +
  # formatting and other
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
    plot.caption = element_text(hjust = 0)
  ) +
  scale_fill_viridis_c(option = "turbo")
ggsave(plot = get_last_plot(), path = DIR_OUTPUTS, filename = "proj_012_02_01_column_net_team_points_meancentered_adj.png", 
       width = 20, height = 6)

# Plot 2: Percent TDs in Red Zone ----
plot_02_data <- 
  full_join(
    # offense
    prod_game_drives |> 
      # filter(offense_conference %in% conferences_p5) |> 
      summarise(
        # number of drives
        count_drive_offense = n(),
        # number of scoring drives
        scoring_drive_offense = sum(drive_result %in% c("TD", "FG")),
        # number of TD scoring drives
        td_drive_offense = sum(drive_result %in% c("TD")),
        .by = offense
      ) |> 
      rename(team = offense),
    # defense
    prod_game_drives |> 
      # filter(defense_conference %in% conferences_p5) |> 
      summarise(
        # number of drives
        count_drive_defense = n(),
        # number of scoring drives
        scoring_drive_defense = sum(drive_result %in% c("TD", "FG")),
        # number of TD scoring drives
        td_drive_defense = sum(drive_result %in% c("TD")),
        .by = defense
      ) |> 
      rename(team = defense),
    by = join_by(team)
  ) |> 
  # add team adjusted ratings
  left_join(off_ratings_tds, by = join_by(team)) |> 
  left_join(def_ratings_tds, by = join_by(team)) |> 
  mutate(
    adj_off_rz_coef = replace_na(adj_off_rz_coef, 0),
    adj_def_rz_coef = replace_na(adj_def_rz_coef, 0)
  ) |> 
  # additional fields
  mutate(
    # ratio of offense:defense drives
    ratio_drives = round(count_drive_offense/count_drive_defense, 5),
    # ratio of offense:defense scoring drives
    ratio_scoring_drives = round(scoring_drive_offense/scoring_drive_defense, 5),
    # ratio of offense: defense TD drives
    ratio_td_drives = round(td_drive_offense/td_drive_defense, 5),
    # proportion offense scoring drive
    pct_scoring_drives_offense = round(scoring_drive_offense/count_drive_offense, 5),
    # proportion offense TD drives
    pct_td_drives_offense = round(td_drive_offense/count_drive_offense, 5),
    # proportion defense scoring drives
    pct_scoring_drives_defense = round(scoring_drive_defense/count_drive_defense, 5),
    # proportion defense TD drives 
    pct_td_drives_defense = round(td_drive_defense/count_drive_defense, 5),
    # logistic function
    intercept = as.numeric(coef_ratings_tds["(Intercept)", 1]),
    adj_off_td_pct = plogis(intercept + adj_off_rz_coef),
    adj_def_td_pct = plogis(intercept + adj_def_rz_coef)
  )|> 
  # add team information 
  left_join(
    raw_team_info |> 
      select(school, logo, conference) |> 
      mutate(p5_conference = conference %in% conferences_p5),
    by = join_by(team == school)
  ) |> 
  filter(!is.na(conference)) 

## Four Quadrant Offense vs Defense Plot ----

# Calculate league averages for quadrant dividers
avg_off_td <- mean(plot_02_data$pct_td_drives_offense, na.rm = TRUE)
avg_def_td <- mean(plot_02_data$pct_td_drives_defense, na.rm = TRUE)

label_pos <- find_quadrant_whitespace(
  df = plot_02_data, x_col = "pct_td_drives_offense", y_col = "pct_td_drives_defense",
  x_mean = avg_off_td, y_mean = avg_def_td
)

ggplot(plot_02_data, aes(x = pct_td_drives_offense, y = pct_td_drives_defense)) +
  # Quadrant reference lines
  geom_vline(xintercept = avg_off_td, linetype = "dashed", color = "gray60") +
  geom_hline(yintercept = avg_def_td, linetype = "dashed", color = "gray60") +
  # Plot team logos
  geom_from_path(aes(path = logo), width = 0.025) +
  # add watermark
  geom_from_path(data = watermark_path, 
                 aes(x = max(plot_02_data$pct_td_drives_defense)*0.99, 
                     y = max(plot_02_data$pct_td_drives_defense)*0.99, 
                     path = image_path), width = 0.04, alpha = 0.8, inherit.aes = FALSE) +
  # Invert Y-axis so lower defensive TD % allowed is at the top
  scale_y_reverse(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  # Quadrant Annotations
  annotate("text", 
           x = label_pos$x[label_pos$quadrant == "Top-Right"], 
           y = label_pos$y[label_pos$quadrant == 'Top-Right'], 
           label = "Elite Teams", fontface = "bold", color = "forestgreen", alpha = 0.6, size = 3) +
  annotate("text", 
           x = label_pos$x[label_pos$quadrant == "Bottom-Left"], 
           y = label_pos$y[label_pos$quadrant == "Bottom-Left"], 
           label = "Struggling\nRed Zone Teams", fontface = "bold", color = "firebrick", alpha = 0.6, size = 3) +
  labs(
    title = "FBS Red Zone Efficiency Matrix",
    subtitle = "Offensive Red Zone Touchdown % Scored vs. Defensive Red Zone Touchdown % Allowed",
    x = "Offensive Red Zone Touchdown Rate Scored",
    y = "Defensive Red Zone Touchdown Rate Allowed (Inverted)",
    caption = "Data: cfbfastR, ggplot2, ggpath"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
    plot.caption = element_text(hjust = 0)
    )
ggsave(plot = get_last_plot(), path = DIR_OUTPUTS, filename = "proj_012_02_01_scatter_offense_defense_td_pct.png", 
       width = 11.5, height = 7)

## Four Quadrant Offense vs Defense Plot Adjusted ----

# Calculate league averages for quadrant dividers
avg_off_td_adj <- mean(plot_02_data$adj_off_td_pct, na.rm = TRUE)
avg_def_td_adj <- mean(plot_02_data$adj_def_td_pct, na.rm = TRUE)

label_pos <- find_quadrant_whitespace(
  df = plot_02_data, x_col = "adj_off_td_pct", y_col = "adj_def_td_pct",
  x_mean = avg_off_td_adj, y_mean = avg_def_td_adj
)

ggplot(plot_02_data, aes(x = adj_off_td_pct, y = adj_def_td_pct)) +
  # Quadrant reference lines
  geom_vline(xintercept = avg_off_td_adj, linetype = "dashed", color = "gray60") +
  geom_hline(yintercept = avg_def_td_adj, linetype = "dashed", color = "gray60") +
  # Plot team logos
  geom_from_path(aes(path = logo), width = 0.025) +
  # add watermark
  geom_from_path(data = watermark_path, 
                 aes(x = max(plot_02_data$adj_off_td_pct)*0.99, 
                     y = max(plot_02_data$adj_def_td_pct)*0.99, 
                     path = image_path), width = 0.04, alpha = 0.8, inherit.aes = FALSE) +
  # Invert Y-axis so lower defensive TD % allowed is at the top
  scale_y_reverse(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  # Quadrant Annotations
  annotate("text", 
           x = label_pos$x[label_pos$quadrant == "Top-Right"], 
           y = label_pos$y[label_pos$quadrant == 'Top-Right'], 
           label = "Elite Teams", fontface = "bold", color = "forestgreen", alpha = 0.6, size = 3) +
  annotate("text", 
           x = label_pos$x[label_pos$quadrant == "Bottom-Left"], 
           y = label_pos$y[label_pos$quadrant == "Bottom-Left"], 
           label = "Struggling\nRed Zone Teams", fontface = "bold", color = "firebrick", alpha = 0.6, size = 3) +
  labs(
    title = "FBS Red Zone Efficiency Matrix",
    subtitle = "Offensive Red Zone Touchdown % Scored vs. Defensive Red Zone Touchdown % Allowed,\nAdjusted For Schedule Strength",
    x = "Offensive Red Zone Touchdown Rate Scored",
    y = "Defensive Red Zone Touchdown Rate Allowed (Inverted)",
    caption = "Data: cfbfastR, ggplot2, ggpath\nRidge regression (L2 regularization) fit on 2026 red zone drives. Values represent touchdown probability per drive against an average opponent."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
    plot.caption = element_text(hjust = 0)
  )
ggsave(plot = get_last_plot(), path = DIR_OUTPUTS, filename = "proj_012_02_01_scatter_offense_defense_td_pct_adj.png", 
       width = 11.5, height = 7)

## Settling for 3 vs Going For 7 ----
plot_02_data |>
  # Top N teams by RZ scoring rate
  slice_max(pct_scoring_drives_offense, n = 40) |>
  mutate(team = reorder(team, pct_scoring_drives_offense)) |>
  ggplot() +
  # Segment representing FG gap
  geom_segment(
    aes(x = pct_td_drives_offense, xend = pct_scoring_drives_offense, y = team, yend = team),
    color = "gray70", linewidth = 1.2
  ) +
  # Point for Total Scoring %
  geom_point(aes(x = pct_scoring_drives_offense, y = team), color = "#336699", size = 4) +
  # Logo placed on Touchdown %
  geom_from_path(aes(x = pct_td_drives_offense, y = team, path = logo), width = 0.025) +
  # add watermark
  geom_from_path(data = watermark_path, 
                 aes(x = max(plot_02_data$pct_scoring_drives_offense)*0.98, 
                     y = 3, 
                     path = image_path), width = 0.02, alpha = 0.8, inherit.aes = FALSE) +
  # labels
  labs(
    title = "Top 40 Red Zone Finishers vs. Field Goal Settlers",
    subtitle = "Logo shows Red Zone Touchdown % | Blue dot shows Total Red Zone Scoring % (TD + FG)",
    x = "Conversion Rate",
    y = NULL,
    caption = "Data: cfbfastR, ggplot2, ggpath"
  ) +
  # format and others
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
    plot.caption = element_text(hjust = 0),
    axis.text.y = element_blank()
  )
ggsave(plot = get_last_plot(), path = DIR_OUTPUTS, filename = "proj_012_02_01_dumbell_red_zone_finishing.png", 
       width = 13, height = 15)

## Red Zone Volume vs Efficiency ----
ggplot(plot_02_data, aes(x = count_drive_offense, y = pct_td_drives_offense)) +
  # add the causal line
  geom_smooth(method = "lm", se = FALSE, color = "gray80", linetype = "dotted") +
  # add the logos
  geom_from_path(aes(path = logo), width = 0.025, position = position_jitter(height = 0.015, width = 0.015)) +
  # add watermark
  geom_from_path(data = watermark_path, 
                 aes(x = max(plot_02_data$count_drive_offense)*0.99, 
                     y = min(plot_02_data$pct_td_drives_offense)*1.02, 
                     path = image_path), width = 0.02, alpha = 0.8, inherit.aes = FALSE) +
  # labels
  labs(
    title = "Red Zone Opportunity vs. Finishing Ability",
    subtitle = "Total Offensive Red Zone Trips vs. Touchdown Conversion Rate",
    x = "Total Red Zone Drives",
    y = "Red Zone Touchdown Rate",
    caption = "Data: cfbfastR, ggplot2, ggpath"
  ) +
  # formatting and others
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
    plot.caption = element_text(hjust = 0)
  )
ggsave(plot = get_last_plot(), path = DIR_OUTPUTS, filename = "proj_012_02_01_scatter_volume_efficiency.png", 
       width = 13, height = 8)

## Drive Outcome Breakdown ----
plot_02_data |>
  # creating fields
  mutate(
    # percent of drives that result in a field goal
    pct_fg_drives_offense = pct_scoring_drives_offense - pct_td_drives_offense,
    # percent of drives that result in no score
    pct_no_score = 1 - pct_scoring_drives_offense
  ) |>
  # retain the top teams by scoring drives
  slice_max(pct_scoring_drives_offense, n = 40) |>
  # sort data and retain fields
  arrange(desc(pct_scoring_drives_offense)) |>
  select(team, pct_td_drives_offense, pct_fg_drives_offense, pct_no_score) |> 
  # pivot the data longer
  pivot_longer(
    cols = c(pct_td_drives_offense, pct_fg_drives_offense, pct_no_score),
    names_to = "outcome",
    values_to = "pct"
  ) |>
  # create fields
  mutate(
    # factorize the outcome field
    outcome = factor(outcome, levels = c("pct_no_score", "pct_fg_drives_offense", "pct_td_drives_offense"),
                     labels = c("No Points", "Field Goal", "Touchdown"))
  ) |>
  ggplot(aes(x = pct, y = reorder(team, pct), fill = outcome)) +
  # column chart
  geom_col(position = "fill", width = 0.7) +
  # add watermark
  geom_from_path(data = watermark_path, 
                 aes(x = 0.95, y = 0.76, path = image_path), width = 0.02, alpha = 0.80, inherit.aes = FALSE) +
  # labels
  labs(
    title = "Red Zone Drive Outcome Distribution\nFor The Top 40 Scoring Teams",
    x = "Proportion of Red Zone Drives",
    y = NULL,
    fill = "Drive Result",
    caption = "Data: cfbfastR, ggplot2, ggpath"
  ) +
  # formats and other
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Touchdown" = "#2e7d32", "Field Goal" = "#fbc02d", "No Points" = "#c62828")) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5)
    )
ggsave(plot = get_last_plot(), path = DIR_OUTPUTS, filename = "proj_012_02_01_columns_outcome_distribution.png", 
       width = 7, height = 16)

## Net Red Zone Ratio Lollipop Chart ----
plot_02_data |>
  # filter for teams that have a ratio (excluding missing and infinite values)
  filter(is.finite(ratio_td_drives)) |>
  # slice_max(ratio_td_drives, n = 15) |>
  ggplot(aes(x = ratio_td_drives, y = reorder(team, ratio_td_drives))) +
  # lollipops
  geom_segment(aes(x = 1, xend = ratio_td_drives, y = team, yend = team), color = "gray50", linewidth = 1) +
  # baseline 
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  # logo labels
  geom_from_path(aes(path = logo), width = 0.015) +
  # watermark
  geom_from_path(data = watermark_path, 
                 aes(x = max(plot_02_data$ratio_td_drives)*0.90, 
                     y = 8, 
                     path = image_path), width = 0.02, alpha = 0.80, inherit.aes = FALSE) +
  # labels
  labs(
    title = "Top FBS Net Red Zone Touchdown Ratios",
    subtitle = "Ratio of Offensive Red Zone TDs Scored\nto Defensive Red Zone TDs Allowed\n(Baseline = 1.0)",
    x = "Offensive Red Zone TDs Scored / Defensive Red Zone TDs Allowed",
    y = NULL,
    caption = "Data: cfbfastR, ggplot2, ggpath"
  ) +
  # themes and others
  scale_x_continuous(breaks = seq(0, 15, 1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
    axis.text.y = element_text(size = 7.5)
  )
ggsave(plot = get_last_plot(), path = DIR_OUTPUTS, filename = "proj_012_02_01_lollipop_redzone_ratio.png", 
       width = 14, height = 20)

