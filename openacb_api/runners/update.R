# =============================================================================
# Quick Weekly Update Script for OpenACB
# =============================================================================

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

# Current season to update
CURRENT_SEASON <- 2026

# Scrape current season
cat("→ Scraping games...\n")
scrape_season(CURRENT_SEASON)

cat("\n→ Processing data...\n")
clean_pbp(CURRENT_SEASON)
create_pbp_variables(CURRENT_SEASON)
calculate_team_stats(CURRENT_SEASON)
process_shot_charts(CURRENT_SEASON)
calculate_player_stats(CURRENT_SEASON)
generate_game_flow(CURRENT_SEASON)
generate_team_pace(CURRENT_SEASON)
generate_clutch_stats(CURRENT_SEASON)
# Export to React
cat("\n→ Exporting to React app...\n")
source("./runners/export_lineup_data.R")
export_single_season(CURRENT_SEASON)
source("./runners/export_to_react.R")

cat("\n Update completed! \n")