# Header ----
#
# Script name: 00_00_dirs_libs_funs_vals.R
#
# Purpose of script: Determine the Directories, Libraries, Functions, and Values for XXX Project
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
#
#   
#

# Working Directories ----
DIR_OUTPUTS <- file.path("C:/Users/japbi/Dropbox/Data Science/00_outputs")
DIR_INPUTS <- file.path("C:/Users/japbi/Dropbox/Data Science/00_inputs")

# Formatting and Global Options ----
conferences_p5 <- c("ACC", "Big 12", "Big Ten", "FBS Independents", "Pac-12", "SEC")
watermark_path <- data.frame(
  x = 4, y = 4, 
  image_path = "C:/Users/japbi/Dropbox/Data Science/3_proj/proj_000_practice/3_output/proj000_01_logos/logo_segments/proj000_10_background_segments_079.png"
)

# Packages ----

library(tidyverse)
library(janitor)
library(cfbfastR)
library(ggpath)
library(glmnet)

# Functions ----

find_quadrant_whitespace <- function(df, x_col, y_col, x_mean, y_mean, grid_n = 20) {
  # Create grid across axis ranges
  x_range <- range(df[[x_col]], na.rm = TRUE)
  y_range <- range(df[[y_col]], na.rm = TRUE)
  
  x_seq <- seq(x_range[1], x_range[2], length.out = grid_n)
  y_seq <- seq(y_range[1], y_range[2], length.out = grid_n)
  grid <- expand.grid(x = x_seq, y = y_seq)
  
  # Calculate minimum distance from each grid point to nearest data point
  grid$min_dist <- apply(grid, 1, function(pt) {
    min(sqrt((df[[x_col]] - pt[1])^2 + (df[[y_col]] - pt[2])^2))
  })
  
  # Assign quadrant flags
  grid$quadrant <- case_when(
    grid$x >= x_mean & grid$y <= y_mean ~ "Top-Right",
    grid$x <  x_mean & grid$y <= y_mean ~ "Top-Left",
    grid$x >= x_mean & grid$y >  y_mean ~ "Bottom-Right",
    grid$x <  x_mean & grid$y >  y_mean ~ "Bottom-Left"
  )
  
  # Pick point with max distance from data points in each quadrant
  grid %>%
    group_by(quadrant) %>%
    slice_max(min_dist, n = 1) %>%
    ungroup()
}


# Sources ----