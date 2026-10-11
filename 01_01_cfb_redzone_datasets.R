# Header ----
#
# Script name: 01_01_cfb_redzone_datasets.R 
#
# Purpose of script: Minimize API use by once-downloading datasets
#
# Location of script:
#
# Author: Japbir Gill, MBA MSEd
#
# Date Created: 2026-10-10
#
# Copyright (c) Japbir Gill, 2026
# Email: Japbir.Gill@gmail.com
#
# Notes ----
#
#   
#

rm(list=ls())

# Working Directories, Libraries, Functions, Values ----

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("00_00_dirs_libs_funs_vals.R")

# Formatting and Global Options ----


# Raw Datasets ----

## Game Drives ----
write.csv(
  cfbd_drives(2026, season_type = "regular", division = "fbs"),
  file.path(DIR_INPUTS, "proj_012_01_01_cfb_redzone_raw_game_drives.csv"),
  row.names = FALSE
)

## Play by Play ----
write.csv(
  map_dfr(1:7, \(wk) cfbd_pbp_data(2026, season_type = "regular", week = wk, epa_wpa = TRUE)),
  file.path(DIR_INPUTS, "proj_012_01_01_cfb_redzone_raw_play_by_play.csv"),
  row.names = FALSE
)

## Team Information ----
write.csv(
  cfbd_team_info(),
  file.path(DIR_INPUTS, "proj_012_01_01_cfb_redzone_raw_team_info.csv"),
  row.names = FALSE
)


