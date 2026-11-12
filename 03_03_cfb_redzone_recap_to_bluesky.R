# Header ----
#
# Script name: 03_03_cfb_redzone_recap_to_bluesky.R
#
# Purpose of script:
#
# Location of script:
#
# Author: Japbir Gill, MBA MSEd
#
# Date Created: 2026-11-05
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
library(atrrr)
# auth(
#   user = Sys.getenv("BSKY_HANDLE"),
#   password = Sys.getenv("BSKY_APP_PSSWRD")
#   )

format_team <- function(x) {
  cleaned <- gsub("_", " ", x)
  title_cased <- toupper(cleaned)
  
  # Optional: Fix short acronyms that should be fully capitalized
  title_cased <- gsub("Smu", "SMU", title_cased)
  
  return(title_cased)
}

# Formatting and Global Options ----

# Datasets ----

files_list <- list.files(path = DIR_OUTPUTS, full.names = TRUE)
files_filtered <- files_list[grepl("week10_review.*\\.png$", files_list)]

# Processing ----

for (i in 1:length(files_filtered)) { #length(files_filtered)
  
  # message(file_iter)
  
  file_iter <- files_filtered[i] 
  
  pattern <- ".*_review_(.*?)_at_(.*?)\\.png$"
  visiting_raw <- format_team(sub(pattern, "\\1", file_iter))
  home_raw <- format_team(sub(pattern, "\\2", file_iter))
  header <- (paste0(visiting_raw, " at ", home_raw))
  message(paste0("Posting ",header))
  
  body <- paste0(
    "Red zone report cards are in for ",
    visiting_raw,
    " at ",
    home_raw,".
    Grades are curved against everyone who played this week, and the opponent is factored in. Dots show every trip inside the 20 (the number of points scored), and arrows show whose stock went up or down.
    #CollegeFootball #CFB"
  )
  
  alt_text <- paste0(
    "Red Zone analysis for the college footbal game of ",
    visiting_raw, 
    " at ",
    home_raw
  )
  
  post_skeet(
    text = body,
    image = file_iter,
    image_alt = alt_text
  )

  message("")
  
  Sys.sleep(5)
  
}
