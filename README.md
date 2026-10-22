# Finishing Drives: FBS Red Zone Analytics

How well do FBS college football teams score once they reach the red zone, and how well do they keep opponents out of the end zone? This project answers that with play-by-play data from [cfbfastR](https://cfbfastR.sportsdataverse.org/), using methods that account for field position, schedule strength and small samples.

It produces weekly team figures and CSVs for the current season (2026), an academic-style R Markdown manuscript that explains the methods and every figure, and charts written for posting on social media.

## What it measures

- **Red zone trip:** a drive with at least one scrimmage snap at or inside the opponent's 20, taken from play-by-play data. Long touchdowns launched from outside the 20 are not trips; drives that reached the 20 and were pushed back are.
- **Drive points:** the actual score change on the drive (not a flat 7 or 3), so missed PATs and two-point tries count correctly.
- **Exclusions:** end-of-half and end-of-game drives, second-half drives that start with a margin above 28 points (garbage time), and unusual point values.
- **Outcomes:** touchdown, field goal, missed or blocked field goal, turnover, turnover on downs, other.

## Methods

- **Points over expected (POE):** each trip's points minus an expected value based on the yards to goal and down at entry (`drive_points ~ ns(rz_entry_ytg, df = 3) + factor(rz_entry_down)`).
- **Schedule adjustment:** ridge regression (`glmnet`) with an indicator for every offense and every defense plus a home-field term, giving offense and defense ratings against an average opponent at a neutral site.
- **Shrinkage:** empirical Bayes (beta-binomial) touchdown rates pulled toward the league rate, so a 4-for-4 team isn't ranked first.
- **Reliability:** split-half correlations (Spearman-Brown corrected) showing how much of each metric is signal versus noise.
- **Points-per-drive decomposition:** splits scoring into red zone access, red zone finishing and non-red-zone scoring.
- **Fourth downs inside the 20:** expected-value comparison of going for it versus kicking, and the EV each team gave up.
- **Weekly movers and recent form:** change in schedule-adjusted rating week over week, and shrunk POE over the last three weeks versus earlier weeks.

Results are associations, not causal claims about play-calling. Red zone samples are small, and the reliability section says how much to trust each number.

## Repository layout

| File | Role |
|---|---|
| `00_00_dirs_libs_funs_vals.R` | Base setup: output and input directories, libraries, watermark image path. Sourced first by every script. |
| `00_01_cfb_functions.R` | Shared functions: cached data access, red zone table building, models, plot theme and helpers. Sourced second. |
| `01_01_cfb_redzone_datasets.R` | Legacy one-off download of drives, play-by-play and team info to CSV. No longer used; the per-week cache in `00_01` replaced it. |
| `02_01_cfb_red_zone_success.R` | Core team view: red zone points and TD rates, raw and schedule-adjusted, net rankings and outcome charts. |
| `02_02_cfb_red_zone_extended.R` | Extended analyses: definition check, POE, reliability, shrinkage, scoring decomposition, play-level EPA, fourth downs, negative plays, optional prior-season validation. |
| `02_03_cfb_red_zone_weekly.R` | Weekly review: latest-week POE leaders, rating movers, goal-line stands, season trends, recent form. |
| `03_01_cfb_red_zone_report.Rmd` | The manuscript (bookdown). Reads each script's `results.rds` and saved figures. |

## Data

All data comes from the [CollegeFootballData.com](https://collegefootballdata.com/) API through `cfbfastR` (drives, play-by-play with EPA, game info, calendar, team info).

The first run downloads every completed week; later runs read the local cache and query only weeks not yet saved. The cache lives in `DIR_DATA/cfbd_cache` if `DIR_DATA` is defined, otherwise `DIR_OUTPUTS/cfbd_cache`, with one `.rds` file per data type, season and week. Empty API responses aren't cached, so they are retried next time. To re-pull a week after CFBD corrects its data, set `REFRESH_WEEKS` in a script's Settings block (for example `REFRESH_WEEKS <- 8`).

## Getting started

### Requirements

- R (4.1 or later, for the native pipe and `\(x)` lambdas) and RStudio. The scripts set the working directory with `rstudioapi`, so run them from RStudio.
- Packages: `tidyverse`, `janitor`, `cfbfastR`, `ggpath`, `glmnet`, `scales`, `rstudioapi`, plus `knitr`, `rmarkdown` and `bookdown` for the report. PDF output also needs `tinytex`.

```r
install.packages(c("tidyverse", "janitor", "cfbfastR", "ggpath", "glmnet", "scales",
                   "rstudioapi", "knitr", "rmarkdown", "bookdown"))
```

- A free CFBD API key from [collegefootballdata.com/key](https://collegefootballdata.com/key). cfbfastR reads it from the `CFBD_API_KEY` environment variable; add `CFBD_API_KEY=your-key` to your `.Renviron` (`usethis::edit_r_environ()`) and restart R.

### Configure paths

`00_00_dirs_libs_funs_vals.R` points to the author's local folders. Before running, edit it to set:

- `DIR_OUTPUTS`: where figures, results files and (by default) the data cache are written;
- `DIR_INPUTS`: used only by `01_01`;
- `watermark_path$image_path`: the watermark image added to every chart;
- optionally `DIR_DATA`, to keep the data cache separately.

### Run order

1. Open and run `02_01_cfb_red_zone_success.R`, `02_02_cfb_red_zone_extended.R`, then `02_03_cfb_red_zone_weekly.R`. Each script sources the two `00_` files itself.
2. Knit `03_01_cfb_red_zone_report.Rmd` (HTML by default). It looks for the results files in `DIR_OUTPUTS`; set the `outputs_dir` parameter to point elsewhere.

Each script's Settings block at the top controls the season, weeks (`WEEKS <- NULL` means all completed weeks), red zone line, garbage-time margin and other thresholds. In `02_02`, `RUN_PRIOR_SEASON <- TRUE` adds a prior-season validation (slow the first time, since it downloads a full extra season). In `02_03`, `FOCUS_TEAMS` or `FOCUS_CONFERENCE` sets which teams are highlighted in the trend charts.

## Outputs

Everything is written to `DIR_OUTPUTS`, with file names prefixed by script (`proj_012_02_01_`, `proj_012_02_02_`, `proj_012_02_03_`).

- **Figures (PNG):** logo scatter plots of offense versus defense (raw and schedule-adjusted), net rankings, outcome distributions, expected points by entry point, POE, reliability, shrinkage, access versus finishing, EPA by zone, fourth-down decision charts, weekly leaders, movers, cumulative POE and recent form.
- **CSVs:** `weekN_team_games.csv` and `weekN_goal_line_stands.csv` from the weekly review.
- **`<prefix>results.rds`:** one per analysis script, read by the report.
- **Report:** `03_01_cfb_red_zone_report.html` (or PDF).

## Weekly workflow

1. After the week's games are final, run `02_01`, `02_02` and `02_03`. Only the new week is downloaded.
2. Check the console messages: column checks, the drive and play-by-play match rate, the missing-PAT warning and any label-overlap notes.
3. Review the figures, then knit the report.

## Known limitations

- The expected points model uses only entry yards to goal and down, not distance, score or time.
- Trips within a game are not independent.
- The garbage-time rule is a simple score-margin cutoff.
- The fourth-down model ignores game state and kicker quality.

## Planned work

An interactive web app for exploring red zone performance by team, conference and week, with a chat assistant for asking questions of the data, is in the planning stage. It is not part of this repository yet.

## License

The code in this repository is released under the [MIT License](LICENSE). Data from CollegeFootballData.com remains subject to its own terms, and team logos shown in figures belong to their respective schools.

## Author

Japbir Gill, MBA, MSEd. Copyright (c) Japbir Gill, 2026.

Data courtesy of [CollegeFootballData.com](https://collegefootballdata.com/) via [cfbfastR](https://cfbfastR.sportsdataverse.org/).
