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

  # Load ETL modules (per-season)
  source(file.path(base_dir, "etl/01_scrape.R"))
  source(file.path(base_dir, "etl/02_clean.R"))
  source(file.path(base_dir, "etl/03_variables.R"))
  source(file.path(base_dir, "etl/04_team_stats.R"))
  source(file.path(base_dir, "etl/05_shot_charts.R"))
  source(file.path(base_dir, "etl/06_lineup_analysis.R"))
  source(file.path(base_dir, "etl/07_player_stats.R"))
  source(file.path(base_dir, "etl/08_game_flow.R"))
  source(file.path(base_dir, "etl/09_team_pace.R"))
  source(file.path(base_dir, "etl/13_clutch_stats.R"))

  # Load ETL modules (cross-season, run once)
  source(file.path(base_dir, "etl/10_team_logos.R"))
  source(file.path(base_dir, "etl/11_player_photos.R"))
  source(file.path(base_dir, "etl/12_player_positions.R"))

  # Load export modules
  source(file.path(base_dir, "runners/export_to_react.R"))
  source(file.path(base_dir, "runners/export_lineup_data.R"))

  cat("\n✓ All modules loaded successfully!\n")
}

# =============================================================================
# Pipeline Execution Functions
# =============================================================================

#' Run the complete pipeline for a single season
#'
#' @param season_id Integer year (e.g., 2025 for 2024-2025 season)
#' @param steps Which steps to run (default: all per-season steps)
#'   - "scrape":          Download data from ACB API
#'   - "clean":           Clean and standardize PBP data
#'   - "variables":       Create player on-court tracking variables
#'   - "team_stats":      Calculate team advanced statistics
#'   - "shot_charts":     Process shot location data
#'   - "lineup_analysis": Calculate lineup on/off statistics
#'   - "player_stats":    Calculate player advanced statistics
#'   - "game_flow":       Generate within-game score evolution data
#'   - "team_pace":       Calculate quarter-by-quarter team pace splits
#' @param data_dir Base directory for data
#' @param config_path Path to configuration file
#'
run_season_pipeline <- function(
    season_id,
    steps = c("scrape", "clean", "variables", "team_stats", "shot_charts",
              "lineup_analysis", "player_stats", "game_flow", "team_pace", "clutch"),
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
  n <- length(steps)
  i <- 0

  step_run <- function(name, label, expr) {
    if (name %in% steps) {
      i <<- i + 1
      cat(sprintf("\n[%d/%d] %s\n", i, n, label))
      results[[name]] <<- tryCatch({
        force(expr)
        "✓ Success"
      }, error = function(e) paste("✗ Error:", e$message))
    }
  }

  step_run("scrape",          "Scraping data from ACB API",
    scrape_season(season_id, data_dir = file.path(data_dir, "raw"), config_path = config_path))

  step_run("clean",           "Cleaning play-by-play data",
    clean_pbp(season_id, data_dir = data_dir, config_path = config_path))

  step_run("variables",       "Creating player tracking variables",
    create_pbp_variables(season_id, data_dir = data_dir, config_path = config_path))

  step_run("team_stats",      "Calculating team statistics",
    {
      calculate_team_stats(season_id, data_dir = data_dir, config_path = config_path)
      calculate_team_stats(season_id, data_dir = data_dir, config_path = config_path, competition_stage = "regular")
      calculate_team_stats(season_id, data_dir = data_dir, config_path = config_path, competition_stage = "playoffs")
    })

  step_run("shot_charts",     "Processing shot chart data",
    process_shot_charts(season_id, data_dir = data_dir, config_path = config_path))

  step_run("lineup_analysis", "Calculating lineup on/off analysis",
    calculate_lineup_analysis(season_id, data_dir = data_dir, config_path = config_path))

  step_run("player_stats",    "Calculating player statistics",
    {
      calculate_player_stats(season_id, data_dir = data_dir, config_path = config_path)
      calculate_player_stats(season_id, data_dir = data_dir, config_path = config_path, competition_stage = "regular")
      calculate_player_stats(season_id, data_dir = data_dir, config_path = config_path, competition_stage = "playoffs")
    })

  step_run("game_flow",       "Generating game flow data",
    generate_game_flow(season_id, data_dir = data_dir, config_path = config_path))

  step_run("team_pace",       "Calculating team pace/quarter splits",
    generate_team_pace(season_id, data_dir = data_dir, config_path = config_path))

  step_run("clutch",          "Calculating clutch statistics",
    generate_clutch_stats(season_id, data_dir = data_dir, config_path = config_path))

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

#' Run pipeline for multiple seasons, then cross-season steps and export
#'
#' @param season_ids Vector of season IDs (default: all available)
#' @param run_cross_season Run steps 10-12 (logos, photos, bio) after all seasons (default: TRUE)
#' @param run_export Run export to React after all seasons (default: TRUE)
#' @param ... Additional arguments passed to run_season_pipeline
#'
run_full_pipeline <- function(season_ids = NULL, run_cross_season = TRUE, run_export = TRUE, ...) {
  source("./config/seasons.R")

  if (is.null(season_ids)) {
    season_ids <- get_available_seasons()
  }

  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║  Running Full Pipeline for Multiple Seasons                  ║\n")
  cat(sprintf("║  Seasons: %-51s║\n", paste(season_ids, collapse = ", ")))
  cat("╚══════════════════════════════════════════════════════════════╝\n")

  all_results <- list()

  # Per-season steps (01-09)
  for (sid in season_ids) {
    all_results[[as.character(sid)]] <- run_season_pipeline(sid, ...)
  }

  # Cross-season steps (10-12): run once across all seasons
  if (run_cross_season) {
    cat("\n╔══════════════════════════════════════════════════════════════╗\n")
    cat("║  Cross-Season Steps                                          ║\n")
    cat("╚══════════════════════════════════════════════════════════════╝\n")

    cat("\n[10] Generating team logos\n")
    tryCatch(
      generate_team_logos(season_ids = season_ids),
      error = function(e) cat("✗ Error:", e$message, "\n")
    )

    cat("\n[11] Generating player photos\n")
    tryCatch(
      generate_player_photos(season_ids = season_ids),
      error = function(e) cat("✗ Error:", e$message, "\n")
    )

    cat("\n[12] Generating player bio data\n")
    tryCatch(
      generate_player_bio(seasons = season_ids),
      error = function(e) cat("✗ Error:", e$message, "\n")
    )
  }

  # Export to React
  if (run_export) {
    cat("\n╔══════════════════════════════════════════════════════════════╗\n")
    cat("║  Exporting to React                                          ║\n")
    cat("╚══════════════════════════════════════════════════════════════╝\n")

    cat("\n[Export] Exporting team identities\n")
    tryCatch(
      export_team_identities(),
      error = function(e) cat("Team identity export error:", e$message, "\n")
    )

    cat("\n[Export] Exporting shot data\n")
    tryCatch(export_shot_data(),        error = function(e) cat("✗ Error:", e$message, "\n"))

    cat("\n[Export] Exporting team data\n")
    tryCatch(export_team_data(),        error = function(e) cat("✗ Error:", e$message, "\n"))

    cat("\n[Export] Exporting player data\n")
    tryCatch(
      export_team_data(c("regular", "playoffs"), "teams-by-stage.json"),
      error = function(e) cat("Stage team export error:", e$message, "\n")
    )
    tryCatch({
      all_players <- load_all_player_data()
      export_player_data(all_players)
      stage_players <- load_all_player_data(c("regular", "playoffs"))
      export_player_data(stage_players, "players-by-stage.json")
      export_similarity_data(all_players)
    }, error = function(e) cat("✗ Error:", e$message, "\n"))

    cat("\n[Export] Exporting clutch data\n")
    tryCatch(
      export_clutch_data(),
      error = function(e) cat("Clutch export error:", e$message, "\n")
    )

    cat("\n[Export] Exporting team pace data\n")
    tryCatch(export_teampace_data(),    error = function(e) cat("✗ Error:", e$message, "\n"))

    cat("\n[Export] Exporting game flow data\n")
    tryCatch(export_gameflow_data(),    error = function(e) cat("✗ Error:", e$message, "\n"))

    cat("\n[Export] Exporting lineup data\n")
    tryCatch(
      export_lineup_data_to_react(seasons = season_ids),
      error = function(e) cat("✗ Error:", e$message, "\n")
    )
  }

  # Final summary
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║  All Seasons Complete!                                       ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n")

  invisible(all_results)
}

#' Quick update: re-process stats for a season without re-scraping
#'
#' @param season_id Season to update (default: current year)
#' @param export Also run export to React afterwards (default: FALSE)
#'
quick_update <- function(
    season_id = as.integer(format(Sys.Date(), "%Y")),
    export = FALSE
) {
  run_season_pipeline(
    season_id,
    steps = c("clean", "variables", "team_stats", "shot_charts",
              "lineup_analysis", "player_stats", "game_flow", "team_pace", "clutch")
  )

  if (export) {
    cat("\n[Export] Running export to React...\n")
    all_players <- load_all_player_data()
    export_team_identities()
    export_shot_data()
    export_team_data()
    export_team_data(c("regular", "playoffs"), "teams-by-stage.json")
    export_player_data(all_players)
    stage_players <- load_all_player_data(c("regular", "playoffs"))
    export_player_data(stage_players, "players-by-stage.json")
    export_similarity_data(all_players)
    export_clutch_data()
    export_teampace_data()
    export_gameflow_data()
    export_lineup_data_to_react()
  }
}

# =============================================================================
# Auto-load modules when script is sourced
# =============================================================================

if (interactive()) {
  load_pipeline_modules()

  cat("\n")
  cat("Available commands:\n")
  cat("  run_season_pipeline(2025)           - Process single season (all steps)\n")
  cat("  run_full_pipeline()                 - Process all seasons + cross-season + export\n")
  cat("  run_full_pipeline(c(2016:2020))     - Process specific seasons\n")
  cat("  quick_update(2026)                  - Re-process stats without re-scraping\n")
  cat("  quick_update(2026, export = TRUE)   - Re-process + export to React\n")
  cat("\n")
  cat("Individual ETL steps:\n")
  cat("  scrape_season(2025)                 - Download from ACB API\n")
  cat("  clean_pbp(2025)                     - Clean PBP data\n")
  cat("  create_pbp_variables(2025)          - Create lineup tracking\n")
  cat("  calculate_team_stats(2025)          - Team statistics\n")
  cat("  process_shot_charts(2025)           - Shot chart data\n")
  cat("  calculate_lineup_analysis(2025)     - Lineup on/off analysis\n")
  cat("  calculate_player_stats(2025)        - Player statistics\n")
  cat("  generate_game_flow(2025)            - Game flow data\n")
  cat("  generate_team_pace(2025)            - Team pace/quarter splits\n")
  cat("  generate_clutch_stats(2025)         - Clutch statistics (last 5 min, ≤5 pts)\n")
  cat("  generate_team_logos()               - Team logos (cross-season)\n")
  cat("  generate_player_photos()            - Player photos (cross-season)\n")
  cat("  generate_player_bio()               - Player bio data (cross-season)\n")
  cat("\n")
  cat("Export:\n")
  cat("  export_team_identities()            - Export stable team identities\n")
  cat("  export_shot_data()                  - Export shot charts to React\n")
  cat("  export_team_data()                  - Export team stats to React\n")
  cat("  export_player_data(load_all_player_data()) - Export player stats to React\n")
  cat("  export_clutch_data()                - Export clutch stats to React\n")
  cat("  export_teampace_data()              - Export team pace to React\n")
  cat("  export_gameflow_data()              - Export game flow to React\n")
  cat("  export_lineup_data_to_react()       - Export lineup data to React\n")
  cat("\n")
}
