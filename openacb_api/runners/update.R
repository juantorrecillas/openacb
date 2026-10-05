# =============================================================================
# Quick Weekly Update Script for OpenACB
# =============================================================================

# resolve the api directory when sourced or run from the repository root
update_source_files <- vapply(sys.frames(), function(frame) {
  if (!is.null(frame$ofile)) as.character(frame$ofile) else ""
}, character(1))
update_source_file <- tail(update_source_files[nzchar(update_source_files)], 1)
if (length(update_source_file) == 0) {
  update_file_arg <- grep("^--file=", commandArgs(), value = TRUE)
  update_source_file <- if (length(update_file_arg) > 0) {
    sub("^--file=", "", update_file_arg[1])
  } else {
    "runners/update.R"
  }
}
update_api_dir <- dirname(dirname(normalizePath(update_source_file, mustWork = TRUE)))
setwd(update_api_dir)

# Windows R may inherit the unsupported C.UTF-8 locale from the shell.
# Use the machine's UTF-8 locale so accented team names parse consistently.
if (.Platform$OS.type == "windows") {
  suppressWarnings(Sys.setlocale("LC_CTYPE", ".UTF-8"))
}

cat("\n╔══════════════════════════════════════════════════════════════╗\n")
cat("║  OpenACB Weekly Update                                       ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

# Load modules
source("./config/seasons.R")
source("./etl/01_scrape.R")
source("./etl/02_clean.R")
source("./etl/03_variables.R")
source("./etl/04_team_stats.R")
source("./etl/05_shot_charts.R")
source("./etl/06_lineup_analysis.R")
source("./etl/07_player_stats.R")
source("./etl/08_game_flow.R")
source("./etl/09_team_pace.R")
source("./etl/13_clutch_stats.R")
source("./etl/12_player_positions.R")

# Current season to update (the incomplete season in config/seasons.R)
CURRENT_SEASON <- get_current_season()

# Scrape current season
cat("→ Scraping games...\n")
scrape_season(CURRENT_SEASON, only_missing = TRUE)

cat("\n→ Processing data...\n")
clean_pbp(CURRENT_SEASON)
create_pbp_variables(CURRENT_SEASON)
# refresh positions for debuting players before calculating their percentiles
generate_player_bio(seasons = CURRENT_SEASON)
calculate_team_stats(CURRENT_SEASON)
calculate_team_stats(CURRENT_SEASON, competition_stage = "regular")
calculate_team_stats(CURRENT_SEASON, competition_stage = "playoffs")
process_shot_charts(CURRENT_SEASON)
calculate_player_stats(CURRENT_SEASON)
calculate_player_stats(CURRENT_SEASON, competition_stage = "regular")
calculate_player_stats(CURRENT_SEASON, competition_stage = "playoffs")
generate_game_flow(CURRENT_SEASON)
generate_team_pace(CURRENT_SEASON)
generate_clutch_stats(CURRENT_SEASON)
# Export to React
cat("\n→ Exporting to React app...\n")
old_lineup_auto <- getOption("openacb.skip_auto_lineup_export")
options(openacb.skip_auto_lineup_export = TRUE)
source("./runners/export_lineup_data.R")
options(openacb.skip_auto_lineup_export = old_lineup_auto)
export_single_season(CURRENT_SEASON)
# leave historical season files untouched during weekly updates
old_react_options <- options(openacb.react_export_seasons = CURRENT_SEASON)
tryCatch(
  source("./runners/export_to_react.R"),
  finally = options(old_react_options)
)

cat("\n Update completed! \n")
