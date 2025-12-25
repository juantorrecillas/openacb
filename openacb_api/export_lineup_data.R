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

# Output file path (single file for all data)
OUTPUT_FILE <- file.path(REACT_APP_DIR, "public/data/lineups.json")

# Which seasons to include
SEASONS <- c(2021, 2022, 2023, 2024, 2025, 2026)

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
  dir.create(dirname(OUTPUT_FILE), showWarnings = FALSE, recursive = TRUE)

  # Calculate lineup analysis for each season
  cat("Calculating lineup analysis data...\n")

  all_lineup_data <- list()

  # Ensure seasons is a proper vector
  seasons_vec <- unlist(seasons)

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
        all_lineup_data[[as.character(season_id)]] <- lineup_data
        cat("  Completed lineup analysis for", season_id, "\n")
      }
    } else {
      cat("  Warning: PBP data file not found:", pbp_file, "\n")
    }
  }

  # Export to single lineups.json file
  if (length(all_lineup_data) > 0) {
    export_combined_lineup_data(all_lineup_data, OUTPUT_FILE)
  } else {
    cat("\nWarning: No lineup data to export\n")
  }

  cat("\n========================================\n")
  cat("Lineup data export complete!\n")
  cat("Output file:", OUTPUT_FILE, "\n")
  cat("========================================\n")
}

# ============================================================================
# Export Functions
# ============================================================================

#' Export single season lineup data
export_season_lineup_data <- function(lineup_data, season_id, output_file) {
  # Transform to React-friendly format
  teams_data <- list()

  for (team_name in names(lineup_data)) {
    team_info <- lineup_data[[team_name]]

    teams_data[[team_name]] <- list(
      team = team_info$team,
      season = season_id,
      players = transform_individual_stats(team_info$individual),
      pairs = transform_pair_stats(team_info$pairs),
      trios = transform_trio_stats(team_info$trios),
      lineups = transform_lineup_stats(team_info$lineups)
    )
  }

  json_output <- list(
    season = season_id,
    generatedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    teams = teams_data
  )

  write_json(json_output, output_file, pretty = TRUE, auto_unbox = TRUE)
  cat("  Exported:", output_file, "\n")
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
      nickname = p$player,
      onORtg = p$onORtg,
      offORtg = p$offORtg,
      onDRtg = p$onDRtg,
      offDRtg = p$offDRtg,
      onNetRtg = p$onNetRtg,
      offNetRtg = p$offNetRtg,
      netDiff = p$netDiff,
      onPoss = p$onPoss,
      offPoss = p$offPoss,
      onTS = p$onTS,
      onEFG = p$onEFG
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
      onORtg = p$onORtg,
      onDRtg = p$onDRtg,
      onNetRtg = p$onNetRtg,
      onPoss = p$onPoss,
      onTS = p$onTS
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
      onORtg = t$onORtg,
      onDRtg = t$onDRtg,
      onNetRtg = t$onNetRtg,
      onPoss = t$onPoss,
      onTS = t$onTS
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
      onORtg = l$onORtg,
      onDRtg = l$onDRtg,
      onNetRtg = l$onNetRtg,
      onPoss = l$onPoss,
      pts = l$pts,
      ptsAllowed = l$ptsAllowed,
      onTS = l$onTS,
      onEFG = l$onEFG,
      tovRate = l$tovRate
    )
  })
}

# ============================================================================
# Quick Export Function (for single season)
# ============================================================================

#' Export a single season, merging with existing lineups.json
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

  # Load existing data if present
  existing_data <- list()
  if (file.exists(OUTPUT_FILE)) {
    cat("Loading existing lineups.json to merge...\n")
    existing_json <- read_json(OUTPUT_FILE)
    existing_data <- existing_json$data
  }

  # Transform new data
  new_data <- list()
  for (team_name in names(lineup_data)) {
    team_info <- lineup_data[[team_name]]
    key <- paste(season_id, gsub(" ", "_", team_name), sep = "_")

    new_data[[key]] <- list(
      team = team_info$team,
      season = as.integer(season_id),
      players = transform_individual_stats(team_info$individual),
      pairs = transform_pair_stats(team_info$pairs),
      trios = transform_trio_stats(team_info$trios),
      lineups = transform_lineup_stats(team_info$lineups)
    )
  }

  # Merge: new data overwrites existing for same season/team
  merged_data <- existing_data
  for (key in names(new_data)) {
    merged_data[[key]] <- new_data[[key]]
  }

  # Write merged output
  dir.create(dirname(OUTPUT_FILE), showWarnings = FALSE, recursive = TRUE)

  json_output <- list(
    generatedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    totalTeamSeasons = length(merged_data),
    data = merged_data
  )

  write_json(json_output, OUTPUT_FILE, pretty = TRUE, auto_unbox = TRUE)

  cat("\nExported to:", OUTPUT_FILE, "\n")
  cat("Total team-seasons in file:", length(merged_data), "\n")

  invisible(lineup_data)
}

# ============================================================================
# Run if executed directly
# ============================================================================

if (!interactive()) {
  export_lineup_data_to_react()
}
