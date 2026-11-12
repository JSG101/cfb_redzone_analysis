# Header ----
#
# Script name: 02_00_full_run.R 
#
# Purpose of script: Single space to run all processing codes
#
# Location of script:
#
# Author: Japbir Gill, MBA MSEd
#
# Date Created: 2026-11-08
#
# Copyright (c) Japbir Gill, 2026
# Email: Japbir.Gill@gmail.com
#
# Notes ----
#
#   
#

rm(list=ls())

# 1. Identify all scripts in the working directory starting with "02_0"
script_files <- setdiff(
  list.files(path = ".", pattern = "^02_0.*\\.R$", full.names = TRUE),
  c("./02_00_full_run.R")
)

# 2. Execute each script in succession
for (script in script_files) {
  message("Executing: ", script)
  source(script)
}
