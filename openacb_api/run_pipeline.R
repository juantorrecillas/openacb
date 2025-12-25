# =============================================================================
# ACB Basketball Analytics Pipeline - Master Pipeline Script
# =============================================================================
# Orchestrates the complete ETL pipeline from API scraping to final datasets.
# Usage: source("run_pipeline.R") then call run_full_pipeline(2025)
# =============================================================================

# =============================================================================
# Load All Modules
# =============================================================================

load_pipeline_modules <- function(base_dir = ".") {
  cat("\n╔══════════════════════════════════════════════════════════════╗\n")
  cat("║       OpenACB 2.0                                              ║\n")
  cat("╚══════════════════════════════════════════════════════════════  ╝\n\n")
  
  # Load configuration
  source(file.path(base_dir, "config/seasons.R"))
  
  # Load ETL modules
  source(file.path(base_dir, "etl/01_scrape.R"))
  source(file.path(base_dir, "etl/02_clean.R"))
  source(file.path(base_dir, "etl/03_variables.R"))
  source(file.path(base_dir, "etl/04_team_stats.R"))
  source(file.path(base_dir, "etl/05_shot_charts.R"))
  source(file.path(base_dir, "etl/06_lineup_analysis.R"))
  source(file.path(base_dir, "etl/07_player_stats.R"))
  
  cat("\n✓ All modules loaded successfully!\n")
}

# =============================================================================
# Pipeline Execution Functions
# =============================================================================

#' Run the complete pipeline for a single season
#' 
#' @param season_id Integer year (e.g., 2025 for 2024-2025 season)
#' @param steps Which steps to run (default: all)
#'   - "scrape": Download data from ACB API
#'   - "clean": Clean and standardize PBP data
#'   - "variables": Create player on-court tracking
#'   - "team_stats": Calculate team advanced statistics
#'   - "shot_charts": Process shot location data
#' @param data_dir Base directory for data
#' @param config_path Path to configuration file
#' 
run_season_pipeline <- function(
    season_id,
    steps = c("scrape", "clean", "variables", "team_stats", "shot_charts"),
    data_dir = "./data",
    config_path = "./config/seasons.R"
) {
  
  start_time <- Sys.time()
  
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat(sprintf("║  Processing Season %d                                        ║\n", season_id))
  cat("╚══════════════════════════════════════════════════════════════╝\n")
  
  # Create data directories if needed
  dir.create(file.path(data_dir, "raw"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(data_dir, "processed"), recursive = TRUE, showWarnings = FALSE)
  
  results <- list()
  
  # Step 1: Scrape
  if ("scrape" %in% steps) {
    cat("\n📥 STEP 1/5: Scraping data from ACB API\n")
    results$scrape <- tryCatch({
      scrape_season(season_id, data_dir = file.path(data_dir, "raw"), config_path = config_path)
      "✓ Success"
    }, error = function(e) paste("✗ Error:", e$message))
  }
  
  # Step 2: Clean
  if ("clean" %in% steps) {
    cat("\n🧹 STEP 2/5: Cleaning play-by-play data\n")
    results$clean <- tryCatch({
      clean_pbp(season_id, data_dir = data_dir, config_path = config_path)
      "✓ Success"
    }, error = function(e) paste("✗ Error:", e$message))
  }
  
  # Step 3: Variables
  if ("variables" %in% steps) {
    cat("\n👥 STEP 3/5: Creating player tracking variables\n")
    results$variables <- tryCatch({
      create_pbp_variables(season_id, data_dir = data_dir, config_path = config_path)
      "✓ Success"
    }, error = function(e) paste("✗ Error:", e$message))
  }
  
  # Step 4: Team Stats
  if ("team_stats" %in% steps) {
    cat("\n📊 STEP 4/5: Calculating team statistics\n")
    results$team_stats <- tryCatch({
      calculate_team_stats(season_id, data_dir = data_dir, config_path = config_path)
      "✓ Success"
    }, error = function(e) paste("✗ Error:", e$message))
  }
  
  # Step 5: Shot Charts
  if ("shot_charts" %in% steps) {
    cat("\n🎯 STEP 5/5: Processing shot chart data\n")
    results$shot_charts <- tryCatch({
      process_shot_charts(season_id, data_dir = data_dir, config_path = config_path)
      "✓ Success"
    }, error = function(e) paste("✗ Error:", e$message))
  }

  # Step 6: Lineup Analysis
  if ("lineup_analysis" %in% steps) {
    cat("\n👥 STEP 6/6: Calculating lineup analysis\n")
    results$lineup_analysis <- tryCatch({
      calculate_lineup_analysis(season_id, data_dir = data_dir, config_path = config_path)
      "✓ Success"
    }, error = function(e) paste("✗ Error:", e$message))
  }
  
  # Summary
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "mins")
  
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║  Pipeline Summary                                            ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n")
  cat(sprintf("  Season: %d\n", season_id))
  cat(sprintf("  Duration: %.1f minutes\n", as.numeric(duration)))
  cat("\n  Step Results:\n")
  
  for (step_name in names(results)) {
    cat(sprintf("    %s: %s\n", step_name, results[[step_name]]))
  }
  
  invisible(results)
}

#' Run pipeline for multiple seasons
#' 
#' @param season_ids Vector of season IDs (default: all available)
#' @param ... Additional arguments passed to run_season_pipeline
#' 
run_full_pipeline <- function(season_ids = NULL, ...) {
  source("./config/seasons.R")
  
  if (is.null(season_ids)) {
    season_ids <- get_available_seasons()
  }
  
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║  Running Full Pipeline for Multiple Seasons                  ║\n")
  cat(sprintf("║  Seasons: %s                                    ║\n", 
              paste(season_ids, collapse = ", ")))
  cat("╚══════════════════════════════════════════════════════════════╝\n")
  
  all_results <- list()
  
  for (sid in season_ids) {
    all_results[[as.character(sid)]] <- run_season_pipeline(sid, ...)
  }
  
  # Final summary
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║  All Seasons Complete!                                       ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n")
  
  invisible(all_results)
}

#' Quick update: Run only specific steps for the current season
#' 
#' @param season_id Season to update (default: current year)
#' @param steps Steps to run (default: from clean onwards, assuming data exists)
#' 
quick_update <- function(
    season_id = as.integer(format(Sys.Date(), "%Y")),
    steps = c("clean", "variables", "team_stats", "shot_charts")
) {
  run_season_pipeline(season_id, steps = steps)
}

# =============================================================================
# Auto-load modules when script is sourced
# =============================================================================

if (interactive()) {
  load_pipeline_modules()
  
  cat("\n")
  cat("Available commands:\n")
  cat("  run_season_pipeline(2025)      - Process single season\n")
  cat("  run_full_pipeline()            - Process all seasons\n")
  cat("  quick_update(2025)             - Update without re-scraping\n")
  cat("\n")
  cat("Individual steps:\n")
  cat("  scrape_season(2025)            - Download from ACB API\n")
  cat("  clean_pbp(2025)                - Clean PBP data\n")
  cat("  create_pbp_variables(2025)     - Create lineup tracking\n")
  cat("  calculate_team_stats(2025)     - Team statistics\n")
  cat("  process_shot_charts(2025)      - Shot chart data\n")
  cat("  calculate_lineup_analysis(2025) - Lineup on/off analysis\n")
  cat("  calculate_player_stats(2025) - Player Statistics\n")
  cat("\n")
}

#Quick update
# {
#   clean_all_pbp()
#   calculate_all_team_stats()
#   process_all_shot_charts()
#   calculate_all_lineup_analysis()
#   calculate_all_player_stats()
#   source("export_lineup_data.R")
#   export_lineup_data_to_react()
#   source("./openacb_react/export_to_react.R")
# }

