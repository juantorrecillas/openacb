# =============================================================================
# ACB Basketball Analytics Pipeline - Lineup Analysis Module (Optimized)
# =============================================================================
# Calculates on/off court statistics for individual players, pairs, trios,
# and full 5-man lineups. Optimized to process by team-season to avoid
# computing combinations across players who never played together.
# =============================================================================

library(data.table)
library(jsonlite)

# =============================================================================
# Configuration
# =============================================================================

# Boxscore columns needed for calculations
BOXSCORE_COLUMNS <- c("puntos", "T1A", "T1I", "T2A", "T2I", "T3A", "T3I",
                      "contr2A", "contr3A", "mateA", "mateI",
                      "reb_def", "reb_of", "recuperacion", "FT_trip", "asistencias",
                      "tapon", "tapon_rec", "perdida", "falta", "falta_rec", "tecnica")

# Minimum possessions required for lineup stats (filters noise)
MIN_POSSESSIONS <- 10

# =============================================================================
# Main Function: Calculate Lineup Analysis for a Season
# =============================================================================

#' Calculate on/off court statistics for a season (optimized)
#'
#' @param season_id Integer year (e.g., 2025 for 2024-2025 season)
#' @param data_dir Base directory for data (default: "./data")
#' @param config_path Path to seasons.R config file
#' @param include_lineups Logical, whether to include 5-man lineup analysis
#' @return List with individual, pair, trio, and lineup data by team-season
#'
calculate_lineup_analysis <- function(season_id,
                                      data_dir = "./data",
                                      config_path = "./config/seasons.R",
                                      include_lineups = TRUE) {

  # Load configuration
  source(config_path)
  season <- get_season_config(season_id)

  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("Calculating Lineup Analysis:", season$season_name, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")

  # Define paths
  processed_dir <- file.path(data_dir, "processed")
  input_file <- file.path(processed_dir, paste0("PbP_adjustedData", season_id, ".Rds"))

  if (!file.exists(input_file)) {
    stop("Processed PBP data file not found: ", input_file,
         "\nRun create_pbp_variables(", season_id, ") first.")
  }

  # Load data as data.table for performance
  cat("Loading PBP data from:", input_file, "\n")
  df_pbp <- as.data.table(readRDS(input_file))
  cat("  Rows:", format(nrow(df_pbp), big.mark = ","), "\n")

  # Get player tracking columns
  pista_cols <- grep("_pista$", names(df_pbp), value = TRUE)
  cat("  Total player tracking columns:", length(pista_cols), "\n")

  # Build team-player mapping (only players who actually played for each team)
  team_players <- build_team_player_mapping(df_pbp, pista_cols)
  teams <- names(team_players)
  cat("  Teams found:", length(teams), "\n")

  # Build player info lookup (ID and display name)
  player_info <- build_player_info(df_pbp)
  cat("  Player info entries:", nrow(player_info), "\n")

  # Initialize results list
  results <- list()

  # Process each team
  for (team_name in teams) {
    cat("\nProcessing team:", team_name, "\n")

    # Get players for this team
    team_player_list <- team_players[[team_name]]
    cat("  Players on roster:", length(team_player_list), "\n")

    if (length(team_player_list) < 5) {
      cat("  Skipping - not enough players\n")
      next
    }

    # Pre-filter data for this team (major optimization)
    team_data <- df_pbp[team.team_actual_name == team_name | opponent == team_name]

    # Calculate statistics
    individual_stats <- calculate_individual_stats_optimized(
      team_data, team_name, team_player_list, player_info
    )
    cat("  Individual stats:", length(individual_stats), "players\n")

    pair_stats <- calculate_pair_stats_optimized(
      team_data, team_name, team_player_list, player_info
    )
    cat("  Pair stats:", length(pair_stats), "pairs\n")

    trio_stats <- calculate_trio_stats_optimized(
      team_data, team_name, team_player_list, player_info
    )
    cat("  Trio stats:", length(trio_stats), "trios\n")

    lineup_stats <- NULL
    if (include_lineups) {
      lineup_stats <- calculate_lineup_stats_optimized(
        team_data, team_name, team_player_list, player_info
      )
      cat("  5-man lineups:", length(lineup_stats), "lineups\n")
    }

    # Combine results for this team
    results[[team_name]] <- list(
      team = team_name,
      season = season_id,
      individual = individual_stats,
      pairs = pair_stats,
      trios = trio_stats,
      lineups = lineup_stats
    )

    cat("  Completed:", team_name, "\n")
  }

  cat("\nLineup analysis completed for", length(results), "teams\n")

  invisible(results)
}

# =============================================================================
# Build Team-Player Mapping (Key Optimization)
# =============================================================================

#' Identifies which players actually played for each team
#' Uses license.licenseNick from actual events (not _pista columns which include opponents)
#'
build_team_player_mapping <- function(df_pbp, pista_cols) {
  cat("Building team-player mapping...\n")

  team_players <- list()
  teams <- unique(df_pbp$team.team_actual_name)
  teams <- teams[!is.na(teams)]

  for (team_name in teams) {
    # Get players who actually made actions FOR this team
    team_rows <- df_pbp[team.team_actual_name == team_name]
    roster <- unique(team_rows$license.licenseNick)
    roster <- roster[!is.na(roster) & roster != ""]

    # Only keep players that have _pista columns
    roster <- roster[paste0(roster, "_pista") %in% pista_cols]

    team_players[[team_name]] <- roster
  }

  # Summary
  total_unique <- length(unique(unlist(team_players)))
  cat("  Mapped", total_unique, "unique players across", length(teams), "teams\n")

  return(team_players)
}

#' Build player info lookup (ID and display name)
#' Returns a data.table with licenseNick, license.id, and displayName (N.Surname format)
#'
build_player_info <- function(df_pbp) {
  player_info <- unique(df_pbp[!is.na(license.licenseNick) & license.licenseNick != "",
    .(licenseNick = license.licenseNick,
      playerId = license.id,
      displayName = license.licenseAbbrev)
  ])

  # Keep first occurrence for each licenseNick (in case of duplicates)
  player_info <- player_info[!duplicated(licenseNick)]

  return(player_info)
}

# =============================================================================
# Core Aggregation Function (Reusable)
# =============================================================================

#' Calculate team and opponent stats for filtered data
#'
calculate_stats_for_subset <- function(subset_dt, team_name) {
  if (nrow(subset_dt) == 0) return(NULL)

  # Team statistics (offensive)
  team_stats <- subset_dt[team.team_actual_name == team_name,
    lapply(.SD, sum, na.rm = TRUE),
    .SDcols = BOXSCORE_COLUMNS
  ]

  if (nrow(team_stats) == 0) return(NULL)

  team_stats[, pos := T2I + T3I + 0.44 * T1I - reb_of + perdida]

  # Opponent statistics (defensive)
  opp_stats <- subset_dt[opponent == team_name,
    lapply(.SD, sum, na.rm = TRUE),
    .SDcols = BOXSCORE_COLUMNS
  ]

  if (nrow(opp_stats) == 0) return(NULL)

  opp_stats[, pos := T2I + T3I + 0.44 * T1I - reb_of + perdida]

  # Skip if not enough possessions
  if (team_stats$pos < MIN_POSSESSIONS) return(NULL)

  # Calculate ratings
  oer <- if (team_stats$pos > 0) team_stats$puntos / team_stats$pos else 0
  der <- if (opp_stats$pos > 0) opp_stats$puntos / opp_stats$pos else 0
  ner <- oer - der

  # Additional stats
  ts <- if ((team_stats$T2I + team_stats$T3I + 0.44 * team_stats$T1I) > 0) {
    team_stats$puntos / (2 * (team_stats$T2I + team_stats$T3I + 0.44 * team_stats$T1I))
  } else 0

  efg <- if ((team_stats$T2I + team_stats$T3I) > 0) {
    (team_stats$T2A + 1.5 * team_stats$T3A) / (team_stats$T2I + team_stats$T3I)
  } else 0

  tov_rate <- if (team_stats$pos > 0) team_stats$perdida / team_stats$pos else 0

  list(
    pos = team_stats$pos,
    oer = oer,
    der = der,
    ner = ner,
    pts = team_stats$puntos,
    pts_allowed = opp_stats$puntos,
    ts = ts,
    efg = efg,
    tov_rate = tov_rate
  )
}

# =============================================================================
# Individual Player Statistics (Optimized)
# =============================================================================

calculate_individual_stats_optimized <- function(team_data, team_name, players, player_info) {
  results <- list()

  for (player in players) {
    pista_col <- paste0(player, "_pista")
    if (!pista_col %in% names(team_data)) next

    # On court stats
    on_subset <- team_data[team_data[[pista_col]] == 1]
    on_stats <- calculate_stats_for_subset(on_subset, team_name)

    # Off court stats
    off_subset <- team_data[team_data[[pista_col]] == 0]
    off_stats <- calculate_stats_for_subset(off_subset, team_name)

    if (!is.null(on_stats) && !is.null(off_stats)) {
      # Get player info (ID and display name)
      pinfo <- player_info[licenseNick == player]
      player_id <- if (nrow(pinfo) > 0) pinfo$playerId[1] else NA
      display_name <- if (nrow(pinfo) > 0) pinfo$displayName[1] else player

      results[[player]] <- list(
        player = player,
        playerId = player_id,
        displayName = display_name,
        onORtg = round(on_stats$oer * 100, 1),
        offORtg = round(off_stats$oer * 100, 1),
        onDRtg = round(on_stats$der * 100, 1),
        offDRtg = round(off_stats$der * 100, 1),
        onNetRtg = round(on_stats$ner * 100, 1),
        offNetRtg = round(off_stats$ner * 100, 1),
        netDiff = round((on_stats$ner - off_stats$ner) * 100, 1),
        onPoss = round(on_stats$pos),
        offPoss = round(off_stats$pos),
        onTS = round(on_stats$ts * 100, 1),
        onEFG = round(on_stats$efg * 100, 1)
      )
    }
  }

  return(results)
}

# =============================================================================
# Player Pair Statistics (Optimized)
# =============================================================================

calculate_pair_stats_optimized <- function(team_data, team_name, players, player_info) {
  results <- list()

  # Only generate combinations if we have enough players
  if (length(players) < 2) return(results)

  # Generate pairs (now only for team's actual players!)
  pairs <- combn(players, 2, simplify = FALSE)

  for (pair in pairs) {
    player1 <- pair[1]
    player2 <- pair[2]

    pista_col1 <- paste0(player1, "_pista")
    pista_col2 <- paste0(player2, "_pista")

    if (!pista_col1 %in% names(team_data) || !pista_col2 %in% names(team_data)) next

    # Both players on court
    on_subset <- team_data[team_data[[pista_col1]] == 1 & team_data[[pista_col2]] == 1]
    on_stats <- calculate_stats_for_subset(on_subset, team_name)

    if (!is.null(on_stats)) {
      # Get display names
      p1info <- player_info[licenseNick == player1]
      p2info <- player_info[licenseNick == player2]
      display1 <- if (nrow(p1info) > 0) p1info$displayName[1] else player1
      display2 <- if (nrow(p2info) > 0) p2info$displayName[1] else player2

      pair_key <- paste(sort(c(player1, player2)), collapse = "_")
      results[[pair_key]] <- list(
        players = paste(display1, display2, sep = " & "),
        player1 = player1,
        player2 = player2,
        player1Id = if (nrow(p1info) > 0) p1info$playerId[1] else NA,
        player2Id = if (nrow(p2info) > 0) p2info$playerId[1] else NA,
        onORtg = round(on_stats$oer * 100, 1),
        onDRtg = round(on_stats$der * 100, 1),
        onNetRtg = round(on_stats$ner * 100, 1),
        onPoss = round(on_stats$pos),
        onTS = round(on_stats$ts * 100, 1)
      )
    }
  }

  return(results)
}

# =============================================================================
# Player Trio Statistics (Optimized)
# =============================================================================

calculate_trio_stats_optimized <- function(team_data, team_name, players, player_info) {
  results <- list()

  if (length(players) < 3) return(results)

  # Generate trios (now only for team's actual players!)
  trios <- combn(players, 3, simplify = FALSE)

  for (trio in trios) {
    pista_cols <- paste0(trio, "_pista")

    if (!all(pista_cols %in% names(team_data))) next

    # All three players on court
    on_subset <- team_data[
      team_data[[pista_cols[1]]] == 1 &
      team_data[[pista_cols[2]]] == 1 &
      team_data[[pista_cols[3]]] == 1
    ]
    on_stats <- calculate_stats_for_subset(on_subset, team_name)

    if (!is.null(on_stats)) {
      # Get display names and IDs
      display_names <- sapply(trio, function(p) {
        pinfo <- player_info[licenseNick == p]
        if (nrow(pinfo) > 0) pinfo$displayName[1] else p
      })
      player_ids <- sapply(trio, function(p) {
        pinfo <- player_info[licenseNick == p]
        if (nrow(pinfo) > 0) pinfo$playerId[1] else NA
      })

      trio_key <- paste(sort(trio), collapse = "_")
      results[[trio_key]] <- list(
        players = paste(display_names, collapse = " & "),
        playerList = trio,
        playerIds = as.list(player_ids),
        onORtg = round(on_stats$oer * 100, 1),
        onDRtg = round(on_stats$der * 100, 1),
        onNetRtg = round(on_stats$ner * 100, 1),
        onPoss = round(on_stats$pos),
        onTS = round(on_stats$ts * 100, 1)
      )
    }
  }

  return(results)
}

# =============================================================================
# Full 5-Man Lineup Statistics (New Feature)
# =============================================================================

calculate_lineup_stats_optimized <- function(team_data, team_name, players, player_info) {
  results <- list()

  if (length(players) < 5) return(results)

  # Get all _pista columns for this team's players
  pista_cols <- paste0(players, "_pista")
  pista_cols <- pista_cols[pista_cols %in% names(team_data)]

  if (length(pista_cols) < 5) return(results)

  # Create lineup identifier for each row
  # A lineup is defined by exactly which 5 players are on court
  team_rows <- team_data[team.team_actual_name == team_name]

  # Build lineup string for each row
  lineup_ids <- apply(team_rows[, ..pista_cols], 1, function(row) {
    on_court <- names(row)[row == 1]
    if (length(on_court) == 5) {
      players_on <- gsub("_pista$", "", on_court)
      paste(sort(players_on), collapse = "|")
    } else {
      NA_character_
    }
  })

  # Add lineup ID to data
  team_rows[, lineup_id := lineup_ids]

  # Get unique lineups with enough data
  lineup_counts <- team_rows[!is.na(lineup_id), .N, by = lineup_id]
  valid_lineups <- lineup_counts[N >= MIN_POSSESSIONS * 2]$lineup_id  # ~2 rows per possession

  cat("    Found", length(valid_lineups), "5-man lineups with sufficient data\n")

  # Calculate stats for each lineup
  for (lineup_id in valid_lineups) {
    lineup_players <- strsplit(lineup_id, "\\|")[[1]]
    pista_cols_lineup <- paste0(lineup_players, "_pista")

    # Filter to rows where this exact lineup is on court
    lineup_subset <- team_data[
      team_data[[pista_cols_lineup[1]]] == 1 &
      team_data[[pista_cols_lineup[2]]] == 1 &
      team_data[[pista_cols_lineup[3]]] == 1 &
      team_data[[pista_cols_lineup[4]]] == 1 &
      team_data[[pista_cols_lineup[5]]] == 1
    ]

    stats <- calculate_stats_for_subset(lineup_subset, team_name)

    if (!is.null(stats)) {
      # Get display names and IDs
      display_names <- sapply(lineup_players, function(p) {
        pinfo <- player_info[licenseNick == p]
        if (nrow(pinfo) > 0) pinfo$displayName[1] else p
      })
      player_ids <- sapply(lineup_players, function(p) {
        pinfo <- player_info[licenseNick == p]
        if (nrow(pinfo) > 0) pinfo$playerId[1] else NA
      })

      results[[lineup_id]] <- list(
        players = paste(display_names, collapse = " | "),
        playerList = lineup_players,
        playerIds = as.list(player_ids),
        onORtg = round(stats$oer * 100, 1),
        onDRtg = round(stats$der * 100, 1),
        onNetRtg = round(stats$ner * 100, 1),
        onPoss = round(stats$pos),
        pts = stats$pts,
        ptsAllowed = stats$pts_allowed,
        onTS = round(stats$ts * 100, 1),
        onEFG = round(stats$efg * 100, 1),
        tovRate = round(stats$tov_rate * 100, 1)
      )
    }
  }

  return(results)
}

# =============================================================================
# Export Function for React App
# =============================================================================

#' Export lineup analysis data to JSON for React app
#'
#' @param lineup_data List returned by calculate_lineup_analysis()
#' @param output_file Path to output JSON file
#' @param pretty Logical, whether to format JSON with indentation
#'
export_lineup_json <- function(lineup_data, output_file, pretty = TRUE) {
  cat("\nExporting lineup data to JSON...\n")

  # Convert to JSON-friendly format
  json_data <- lapply(lineup_data, function(team_data) {
    list(
      team = team_data$team,
      season = team_data$season,
      individual = if (length(team_data$individual) > 0) team_data$individual else list(),
      pairs = if (length(team_data$pairs) > 0) team_data$pairs else list(),
      trios = if (length(team_data$trios) > 0) team_data$trios else list(),
      lineups = if (length(team_data$lineups) > 0) team_data$lineups else list()
    )
  })

  # Ensure output directory exists
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)

  # Write to file
  write_json(json_data, output_file, pretty = pretty, auto_unbox = TRUE)

  # Summary
  file_size <- file.info(output_file)$size
  cat("  Exported to:", output_file, "\n")
  cat("  File size:", format(file_size, units = "auto"), "\n")

  invisible(json_data)
}

# =============================================================================
# Calculate All Seasons
# =============================================================================

#' Calculate lineup analysis for all available seasons
#'
#' @param data_dir Base directory for data (default: "./data")
#' @param config_path Path to seasons.R config file
#' @param output_dir Directory for output JSON files
#' @return List with lineup data for all seasons
#'
calculate_all_lineup_analysis <- function(data_dir = "./data",
                                          config_path = "./config/seasons.R",
                                          output_dir = NULL) {
  source(config_path)
  season_ids <- get_available_seasons()

  all_results <- list()

  for (season_id in season_ids) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("Processing Season:", season_id, "\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")

    result <- tryCatch({
      calculate_lineup_analysis(season_id, data_dir, config_path)
    }, error = function(e) {
      cat("Error processing season", season_id, ":", e$message, "\n")
      NULL
    })

    if (!is.null(result)) {
      all_results[[as.character(season_id)]] <- result

      # Export individual season file if output_dir specified
      if (!is.null(output_dir)) {
        output_file <- file.path(output_dir, paste0("lineups_", season_id, ".json"))
        export_lineup_json(result, output_file)
      }
    }
  }

  return(all_results)
}

# =============================================================================
# Auto-load message
# =============================================================================

if (interactive()) {
  cat("\nLineup Analysis Module Loaded (Optimized)\n")
  cat("Key improvements:\n")
  cat("  - Processes by team-season (no cross-team player combinations)\n")
  cat("  - Uses data.table for faster aggregations\n")
  cat("  - Pre-filters data once per team\n")
  cat("  - Includes 5-man lineup analysis\n")
  cat("  - Minimum possession threshold filters noise\n")
  cat("\nAvailable functions:\n")
  cat("  calculate_lineup_analysis(2025)   - Single season\n")
  cat("  calculate_all_lineup_analysis()   - All seasons\n")
  cat("  export_lineup_json(data, file)    - Export to JSON\n")
  cat("\n")
}
