#' Export Lineup Analysis Data for OpenACB React App
#'
#' This script calculates real on/off court statistics and exports them to JSON
#' for the React frontend. Uses the optimized lineup analysis module that
#' processes by team-season to avoid unnecessary cross-team computations.
#'
#' Usage:
#'   source("export_lineup_data.R")
#'   export_lineup_data_to_react()ex

# ============================================================================
# Configuration
# ============================================================================

# Where your processed PBP data files are
PBP_DATA_DIR <- "./data/processed"

# Where the React app is located
REACT_APP_DIR <- "../openacb_react"

# Output directory for per-season files
OUTPUT_DIR <- file.path(REACT_APP_DIR, "public/data")

# Which seasons to include
SEASONS <- c(2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026)

# ============================================================================
# Main Export Function
# ============================================================================

export_lineup_data_to_react <- function(seasons = SEASONS) {
  cat("\n========================================\n")
  cat("OpenACB Lineup Analysis Data Export\n")
  cat("========================================\n\n")

  # Load required modules
  source("./config/seasons.R")
  source("./etl/06_lineup_analysis.R")

  # Ensure output directory exists
  dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

  # Calculate lineup analysis for each season
  cat("Calculating lineup analysis data...\n")

  # Ensure seasons is a proper vector
  seasons_vec <- unlist(seasons)
  exported_seasons <- c()

  for (i in seq_along(seasons_vec)) {
    season_id <- seasons_vec[i]
    cat("\n", paste(rep("-", 50), collapse = ""), "\n")
    cat("Processing season:", season_id, "\n")

    # Check if PBP data exists
    pbp_file <- file.path(PBP_DATA_DIR, paste0("PbP_adjustedData", season_id, ".Rds"))

    if (file.exists(pbp_file)) {
      cat("  Found PBP data file\n")

      # Calculate lineup analysis using optimized function
      lineup_data <- tryCatch({
        calculate_lineup_analysis(
          season_id,
          data_dir = "./data",
          config_path = "./config/seasons.R",
          include_lineups = TRUE
        )
      }, error = function(e) {
        cat("  Error calculating lineup analysis:", e$message, "\n")
        NULL
      })

      if (!is.null(lineup_data)) {
        # Export to per-season file: lineups-YYYY.json
        output_file <- file.path(OUTPUT_DIR, paste0("lineups-", season_id, ".json"))
        export_season_lineup_json(lineup_data, season_id, output_file)
        exported_seasons <- c(exported_seasons, season_id)
        cat("  Completed lineup analysis for", season_id, "\n")
      }
    } else {
      cat("  Warning: PBP data file not found:", pbp_file, "\n")
    }
  }

  cat("\n========================================\n")
  cat("Lineup data export complete!\n")
  cat("Exported", length(exported_seasons), "season files to:", OUTPUT_DIR, "\n")
  cat("Seasons:", paste(exported_seasons, collapse = ", "), "\n")
  cat("========================================\n")
}

# ============================================================================
# Export Functions
# ============================================================================

#' Export single season lineup data to per-season JSON file
#' Format: lineups-YYYY.json with structure { season, generatedAt, data: { "TeamName": {...} } }
export_season_lineup_json <- function(lineup_data, season_id, output_file) {
  # Transform to React-friendly format
  teams_data <- list()

  for (team_name in names(lineup_data)) {
    team_info <- lineup_data[[team_name]]

    teams_data[[team_name]] <- list(
      team = team_info$team,
      season = as.integer(season_id),
      players = transform_individual_stats(team_info$individual),
      pairs = transform_pair_stats(team_info$pairs),
      trios = transform_trio_stats(team_info$trios),
      lineups = transform_lineup_stats(team_info$lineups)
    )
  }

  json_output <- list(
    season = as.integer(season_id),
    generatedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    totalTeams = length(teams_data),
    data = teams_data
  )

  write_json(json_output, output_file, pretty = FALSE, auto_unbox = TRUE)

  file_size <- file.info(output_file)$size
  cat("  Exported:", output_file, "(", format(file_size, units = "auto"), ")\n")
}

#' Export combined lineup data for all seasons
export_combined_lineup_data <- function(all_lineup_data, output_file) {
  cat("\nExporting combined lineup data...\n")

  combined_data <- list()

  for (season_id in names(all_lineup_data)) {
    season_data <- all_lineup_data[[season_id]]

    for (team_name in names(season_data)) {
      team_info <- season_data[[team_name]]

      # Create unique key: season_team
      key <- paste(season_id, gsub(" ", "_", team_name), sep = "_")

      combined_data[[key]] <- list(
        team = team_info$team,
        season = as.integer(season_id),
        players = transform_individual_stats(team_info$individual),
        pairs = transform_pair_stats(team_info$pairs),
        trios = transform_trio_stats(team_info$trios),
        lineups = transform_lineup_stats(team_info$lineups)
      )
    }
  }

  json_output <- list(
    generatedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    totalTeamSeasons = length(combined_data),
    data = combined_data
  )

  write_json(json_output, output_file, pretty = TRUE, auto_unbox = TRUE)

  file_size <- file.info(output_file)$size
  cat("  Exported combined file:", output_file, "\n")
  cat("  File size:", format(file_size, units = "auto"), "\n")
  cat("  Team-seasons:", length(combined_data), "\n")
}

#' Create index file with metadata about available lineup data
create_lineup_index <- function(all_lineup_data, output_dir) {
  index_data <- list(
    generatedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    seasons = list()
  )

  for (season_id in names(all_lineup_data)) {
    season_data <- all_lineup_data[[season_id]]

    teams_summary <- lapply(names(season_data), function(team_name) {
      team_info <- season_data[[team_name]]
      list(
        team = team_name,
        playerCount = length(team_info$individual),
        pairCount = length(team_info$pairs),
        trioCount = length(team_info$trios),
        lineupCount = length(team_info$lineups)
      )
    })

    index_data$seasons[[season_id]] <- list(
      seasonId = as.integer(season_id),
      teamCount = length(season_data),
      teams = teams_summary
    )
  }

  index_file <- file.path(output_dir, "index.json")
  write_json(index_data, index_file, pretty = TRUE, auto_unbox = TRUE)
  cat("  Created index file:", index_file, "\n")
}

# ============================================================================
# Transform Functions (Convert R list to React-friendly arrays)
# ============================================================================

#' Transform individual player stats to array format
transform_individual_stats <- function(individual_data) {
  if (is.null(individual_data) || length(individual_data) == 0) {
    return(list())
  }

  lapply(individual_data, function(p) {
    list(
      id = p$playerId,
      name = p$displayName,
      # player contains the unique key (nick_id), nickname contains just the surname
      playerKey = p$player,
      nickname = if (!is.null(p$nickname)) p$nickname else p$player,
      # Minutes
      onMin = p$onMin,
      offMin = p$offMin,
      # Ratings
      onORtg = p$onORtg,
      offORtg = p$offORtg,
      onDRtg = p$onDRtg,
      offDRtg = p$offDRtg,
      onNetRtg = p$onNetRtg,
      offNetRtg = p$offNetRtg,
      netDiff = p$netDiff,
      onPoss = p$onPoss,
      offPoss = p$offPoss,
      # Shooting
      onTS = p$onTS,
      offTS = p$offTS,
      onEFG = p$onEFG,
      offEFG = p$offEFG,
      onOppEFG = p$onOppEFG,
      offOppEFG = p$offOppEFG,
      # Turnovers
      onTOV = p$onTOV,
      offTOV = p$offTOV,
      onOppTOV = p$onOppTOV,
      offOppTOV = p$offOppTOV,
      # Rebounding
      onORB = p$onORB,
      offORB = p$offORB,
      onDRB = p$onDRB,
      offDRB = p$offDRB,
      # Free throws & Assists
      onFTr = p$onFTr,
      offFTr = p$offFTr,
      onAST = p$onAST,
      offAST = p$offAST
    )
  })
}

#' Transform pair stats to array format
transform_pair_stats <- function(pair_data) {
  if (is.null(pair_data) || length(pair_data) == 0) {
    return(list())
  }

  lapply(pair_data, function(p) {
    list(
      players = p$players,
      player1 = p$player1,
      player2 = p$player2,
      player1Id = p$player1Id,
      player2Id = p$player2Id,
      # Minutes
      onMin = p$onMin,
      offMin = p$offMin,
      # Ratings
      onORtg = p$onORtg,
      offORtg = p$offORtg,
      onDRtg = p$onDRtg,
      offDRtg = p$offDRtg,
      onNetRtg = p$onNetRtg,
      offNetRtg = p$offNetRtg,
      netDiff = p$netDiff,
      # Possessions
      onPoss = p$onPoss,
      offPoss = p$offPoss,
      # Four Factors
      onTS = p$onTS,
      onEFG = p$onEFG,
      onOppEFG = p$onOppEFG,
      onTOV = p$onTOV,
      onDRB = p$onDRB,
      onAST = p$onAST,
      without = p$without
    )
  })
}

#' Transform trio stats to array format
transform_trio_stats <- function(trio_data) {
  if (is.null(trio_data) || length(trio_data) == 0) {
    return(list())
  }

  lapply(trio_data, function(t) {
    list(
      players = t$players,
      playerList = t$playerList,
      playerIds = t$playerIds,
      # Minutes
      onMin = t$onMin,
      offMin = t$offMin,
      # Ratings
      onORtg = t$onORtg,
      offORtg = t$offORtg,
      onDRtg = t$onDRtg,
      offDRtg = t$offDRtg,
      onNetRtg = t$onNetRtg,
      offNetRtg = t$offNetRtg,
      netDiff = t$netDiff,
      # Possessions
      onPoss = t$onPoss,
      offPoss = t$offPoss,
      # Four Factors
      onTS = t$onTS,
      onEFG = t$onEFG,
      onOppEFG = t$onOppEFG,
      onTOV = t$onTOV,
      onDRB = t$onDRB,
      onAST = t$onAST
    )
  })
}

#' Transform 5-man lineup stats to array format
transform_lineup_stats <- function(lineup_data) {
  if (is.null(lineup_data) || length(lineup_data) == 0) {
    return(list())
  }

  lapply(lineup_data, function(l) {
    list(
      players = l$players,
      playerList = l$playerList,
      playerIds = l$playerIds,
      onMin = l$onMin,
      onORtg = l$onORtg,
      onDRtg = l$onDRtg,
      onNetRtg = l$onNetRtg,
      onPoss = l$onPoss,
      pts = l$pts,
      ptsAllowed = l$ptsAllowed,
      # Four Factors
      onTS = l$onTS,
      onEFG = l$onEFG,
      onOppEFG = l$onOppEFG,
      onTOV = l$onTOV,
      onOppTOV = l$onOppTOV,
      onORB = l$onORB,
      onDRB = l$onDRB,
      onFTr = l$onFTr,
      onAST = l$onAST
    )
  })
}

# ============================================================================
# Quick Export Function (for single season)
# ============================================================================

#' Export a single season to its own per-season file
export_single_season <- function(season_id) {
  source("./config/seasons.R")
  source("./etl/06_lineup_analysis.R")

  cat("Calculating lineup analysis for season", season_id, "...\n")

  lineup_data <- calculate_lineup_analysis(
    season_id,
    data_dir = "./data",
    config_path = "./config/seasons.R",
    include_lineups = TRUE
  )

  # Export to per-season file
  output_file <- file.path(OUTPUT_DIR, paste0("lineups-", season_id, ".json"))
  dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

  export_season_lineup_json(lineup_data, season_id, output_file)

  cat("\nExported to:", output_file, "\n")
  cat("Teams in file:", length(lineup_data), "\n")

  invisible(lineup_data)
}

# ============================================================================
# Run if executed directly
# ============================================================================

if (!interactive()) {
  season_args <- suppressWarnings(as.integer(commandArgs(trailingOnly = TRUE)))
  season_args <- season_args[!is.na(season_args)]
  export_lineup_data_to_react(if (length(season_args) > 0) season_args else SEASONS)
}
