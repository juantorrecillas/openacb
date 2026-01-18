# =============================================================================
# ACB Basketball Analytics Pipeline - Lineup Analysis Module (Optimized)
# =============================================================================
# Calculates on/off court statistics for individual players, pairs, trios,
# and full 5-man lineups. Optimized to process by team-season to avoid
# computing combinations across players who never played together.
# Now includes minutes calculation alongside possessions.
# =============================================================================

library(data.table)
library(jsonlite)
library(dplyr)

# =============================================================================
# Configuration
# =============================================================================

# Boxscore columns needed for calculations
BOXSCORE_COLUMNS <- c("puntos", "T1A", "T1I", "T2A", "T2I", "T3A", "T3I",
                      "contr2A", "contr3A", "mateA", "mateI",
                      "reb_def", "reb_of", "recuperacion", "FT_trip", "asistencias",
                      "tapon", "tapon_rec", "perdida", "falta", "falta_rec", "tecnica", "altercado", "revision", "salto_ganado", "salto_perdido",
                      #Context specific variables
                      "transition_fgm2", "transition_fgm3", 
                      "second_chance_fgm2", "second_chance_fgm3",
                      "assisted_fgm2", "assisted_fgm3")

# Minimum possessions required for lineup stats (filters noise)
MIN_POSSESSIONS <- 10

# Minimum minutes required (alternative filter)
MIN_MINUTES <- 2

# =============================================================================
# Time Difference Calculation (for minutes tracking)
# =============================================================================

#' Prepare time differences for minutes calculation
#'
#' Calculates the time elapsed between consecutive events within each match.
#' This is used to calculate how long each lineup was on court.
#'
#' @param df_pbp Play-by-play data as data.table
#' @return data.table with time_diff column added
#'
prepare_time_diff <- function(df_pbp) {
  cat("→ Calculating time differences for minutes tracking...\n")

  # Ensure time columns are numeric
  df_pbp[, minute := as.numeric(minute)]
  df_pbp[, second := as.numeric(second)]
  df_pbp[, period := as.numeric(period)]

  # Calculate time differences within each match
  # Sort: Period ascending, Time descending (10:00 -> 00:00)
  setorder(df_pbp, id_match, period, -minute, -second)

  df_pbp[, `:=`(
    current_seconds = minute * 60 + second,
    prev_seconds = shift(minute * 60 + second, type = "lag"),
    prev_period = shift(period, type = "lag")
  ), by = id_match]

  # Calculate time difference
  df_pbp[, time_diff := prev_seconds - current_seconds]

  # Cleanup:
  # 1. If period changed, time_diff is 0 (don't calc time across quarters)
  df_pbp[period != prev_period | is.na(prev_period), time_diff := 0]
  # 2. Sanity check: remove negative times or huge gaps (>5 mins = 300 sec)
  df_pbp[time_diff < 0 | time_diff > 300 | is.na(time_diff), time_diff := 0]

  total_minutes <- sum(df_pbp$time_diff, na.rm = TRUE) / 60
  cat(sprintf("  Total game time found: %.1f minutes\n", total_minutes))

  return(df_pbp)
}

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

  # Calculate time differences for minutes tracking
  df_pbp <- prepare_time_diff(df_pbp)

  # Get player tracking columns
  pista_cols <- grep("_pista$", names(df_pbp), value = TRUE)
  cat("  Total player tracking columns:", length(pista_cols), "\n")

  # Build team-player mapping
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
#' Uses license.licenseNick and license.id from actual events
#' 
build_team_player_mapping <- function(df_pbp, pista_cols) {
  cat("Building team-player mapping...\n")

  team_players <- list()
  teams <- unique(df_pbp$team.team_actual_name)
  teams <- teams[!is.na(teams)]

  for (team_name in teams) {
    # Get players who actually made actions FOR this team
    team_rows <- df_pbp[team.team_actual_name == team_name]
    roster_df <- unique(team_rows[, .(license.licenseNick, license.id)])
    roster_df <- roster_df[!is.na(license.licenseNick) & license.licenseNick != "" & !is.na(license.id)]

    # Build column names in the new format: {nick}_{id}_pista
    roster_df[, pista_col := paste0(license.licenseNick, "_", license.id, "_pista")]

    # Only keep players that have _pista columns
    roster_df <- roster_df[pista_col %in% pista_cols]

    # Return the pista column names (without _pista suffix) for consistency
    # These will be used to filter data later
    team_players[[team_name]] <- roster_df$pista_col
  }

  # Summary
  total_unique <- length(unique(unlist(team_players)))
  cat("  Mapped", total_unique, "unique player columns across", length(teams), "teams\n")

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
# Core Aggregation Function
# =============================================================================

#' Calculate team and opponent stats for filtered data
#' Now includes minutes calculation from time_diff column

calculate_stats_for_subset <- function(subset_dt, team_name) {
  if (nrow(subset_dt) == 0) return(NULL)

  # Calculate total minutes from time_diff (in seconds, convert to minutes)
  total_seconds <- sum(subset_dt$time_diff, na.rm = TRUE)
  minutes <- total_seconds / 60

  # Team statistics (offensive)
  team_stats <- subset_dt[team.team_actual_name == team_name,
    lapply(.SD, sum, na.rm = TRUE),
    .SDcols = BOXSCORE_COLUMNS
  ]

  if (nrow(team_stats) == 0) return(NULL)

  team_stats[, pos := T2I + T3I + FT_trip - reb_of + perdida]

  # Opponent statistics (defensive)
  opp_stats <- subset_dt[opponent == team_name,
    lapply(.SD, sum, na.rm = TRUE),
    .SDcols = BOXSCORE_COLUMNS
  ]

  if (nrow(opp_stats) == 0) return(NULL)

  opp_stats[, pos := T2I + T3I + FT_trip - reb_of + perdida]

  # Skip if not enough minutes (use MIN_MINUTES threshold)
  if (minutes < MIN_MINUTES) return(NULL)

  # Calculate ratings
  oer <- if (team_stats$pos > 0) team_stats$puntos / team_stats$pos else 0
  der <- if (opp_stats$pos > 0) opp_stats$puntos / opp_stats$pos else 0
  ner <- oer - der

  # === FOUR FACTORS CALCULATIONS ===

  # True Shooting %
  ts <- if ((team_stats$T2I + team_stats$T3I + team_stats$FT_trip) > 0) {
    team_stats$puntos / (2 * (team_stats$T2I + team_stats$T3I + team_stats$FT_trip))
  } else 0

  # Effective Field Goal % (Offensive)
  efg <- if ((team_stats$T2I + team_stats$T3I) > 0) {
    (team_stats$T2A + 1.5 * team_stats$T3A) / (team_stats$T2I + team_stats$T3I)
  } else 0

  # Opponent Effective Field Goal % (Defensive)
  opp_efg <- if ((opp_stats$T2I + opp_stats$T3I) > 0) {
    (opp_stats$T2A + 1.5 * opp_stats$T3A) / (opp_stats$T2I + opp_stats$T3I)
  } else 0

  # Turnover Rate (Offensive - lower is better)
  tov_rate <- if (team_stats$pos > 0) team_stats$perdida / team_stats$pos else 0

  # Opponent Turnover Rate (Defensive - higher is better = forced turnovers)
  opp_tov_rate <- if (opp_stats$pos > 0) opp_stats$perdida / opp_stats$pos else 0

  # Offensive Rebound % = ORB / (ORB + Opp_DRB)
  orb_pct <- if ((team_stats$reb_of + opp_stats$reb_def) > 0) {
    team_stats$reb_of / (team_stats$reb_of + opp_stats$reb_def)
  } else 0

  # Defensive Rebound % = DRB / (DRB + Opp_ORB)
  drb_pct <- if ((team_stats$reb_def + opp_stats$reb_of) > 0) {
    team_stats$reb_def / (team_stats$reb_def + opp_stats$reb_of)
  } else 0

  # Free Throw Rate (Offensive) = FTA / FGA
  ft_rate <- if ((team_stats$T2I + team_stats$T3I) > 0) {
    team_stats$T1I / (team_stats$T2I + team_stats$T3I)
  } else 0

  # Opponent Free Throw Rate (Defensive)
  opp_ft_rate <- if ((opp_stats$T2I + opp_stats$T3I) > 0) {
    opp_stats$T1I / (opp_stats$T2I + opp_stats$T3I)
  } else 0

  # Assist Rate = Assists / FGM
  ast_rate <- if ((team_stats$T2A + team_stats$T3A) > 0) {
    team_stats$asistencias / (team_stats$T2A + team_stats$T3A)
  } else 0

  list(
    minutes = minutes,
    pos = team_stats$pos,
    oer = oer,
    der = der,
    ner = ner,
    pts = team_stats$puntos,
    pts_allowed = opp_stats$puntos,
    ts = ts,
    efg = efg,
    opp_efg = opp_efg,
    tov_rate = tov_rate,
    opp_tov_rate = opp_tov_rate,
    orb_pct = orb_pct,
    drb_pct = drb_pct,
    ft_rate = ft_rate,
    opp_ft_rate = opp_ft_rate,
    ast_rate = ast_rate
  )
}

# =============================================================================
# Individual Player Statistics (Optimized)
# =============================================================================

calculate_individual_stats_optimized <- function(team_data, team_name, player_cols, player_info) {
  results <- list()

  # player_cols are now full column names like "García_20210685_pista"
  for (pista_col in player_cols) {
    if (!pista_col %in% names(team_data)) next

    # Extract player nick from column name (everything before the last _NUMBER_pista)
    player <- sub("_[0-9]+_pista$", "", pista_col)

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
        # Minutes (primary metric now)
        onMin = round(on_stats$minutes, 1),
        offMin = round(off_stats$minutes, 1),
        # Ratings
        onORtg = round(on_stats$oer * 100, 1),
        offORtg = round(off_stats$oer * 100, 1),
        onDRtg = round(on_stats$der * 100, 1),
        offDRtg = round(off_stats$der * 100, 1),
        onNetRtg = round(on_stats$ner * 100, 1),
        offNetRtg = round(off_stats$ner * 100, 1),
        netDiff = round((on_stats$ner - off_stats$ner) * 100, 1),
        onPoss = round(on_stats$pos),
        offPoss = round(off_stats$pos),
        # Shooting efficiency
        onTS = round(on_stats$ts * 100, 1),
        offTS = round(off_stats$ts * 100, 1),
        onEFG = round(on_stats$efg * 100, 1),
        offEFG = round(off_stats$efg * 100, 1),
        # Defensive shooting (opponent eFG%)
        onOppEFG = round(on_stats$opp_efg * 100, 1),
        offOppEFG = round(off_stats$opp_efg * 100, 1),
        # Turnovers
        onTOV = round(on_stats$tov_rate * 100, 1),
        offTOV = round(off_stats$tov_rate * 100, 1),
        onOppTOV = round(on_stats$opp_tov_rate * 100, 1),
        offOppTOV = round(off_stats$opp_tov_rate * 100, 1),
        # Rebounding
        onORB = round(on_stats$orb_pct * 100, 1),
        offORB = round(off_stats$orb_pct * 100, 1),
        onDRB = round(on_stats$drb_pct * 100, 1),
        offDRB = round(off_stats$drb_pct * 100, 1),
        # Free throws
        onFTr = round(on_stats$ft_rate * 100, 1),
        offFTr = round(off_stats$ft_rate * 100, 1),
        # Assists
        onAST = round(on_stats$ast_rate * 100, 1),
        offAST = round(off_stats$ast_rate * 100, 1)
      )
    }
  }

  return(results)
}

# =============================================================================
# Player Pair Statistics (Optimized)
# =============================================================================

calculate_pair_stats_optimized <- function(team_data, team_name, player_cols, player_info) {
  results <- list()

  # Only generate combinations if we have enough players
  if (length(player_cols) < 2) return(results)

  # Generate pairs (now only for team's actual players!)
  pairs <- combn(player_cols, 2, simplify = FALSE)

  for (pair in pairs) {
    pista_col1 <- pair[1]
    pista_col2 <- pair[2]

    if (!pista_col1 %in% names(team_data) || !pista_col2 %in% names(team_data)) next

    # Extract player nicks from column names
    player1 <- sub("_[0-9]+_pista$", "", pista_col1)
    player2 <- sub("_[0-9]+_pista$", "", pista_col2)

    # Both players on court
    on_subset <- team_data[team_data[[pista_col1]] == 1 & team_data[[pista_col2]] == 1]
    on_stats <- calculate_stats_for_subset(on_subset, team_name)

    # At least one player off court (NOT both together)
    off_subset <- team_data[!(team_data[[pista_col1]] == 1 & team_data[[pista_col2]] == 1)]
    off_stats <- calculate_stats_for_subset(off_subset, team_name)

    if (!is.null(on_stats)) {
      # Get display names
      p1info <- player_info[licenseNick == player1]
      p2info <- player_info[licenseNick == player2]
      display1 <- if (nrow(p1info) > 0) p1info$displayName[1] else player1
      display2 <- if (nrow(p2info) > 0) p2info$displayName[1] else player2

      # Calculate impact (net rating difference)
      net_diff <- if (!is.null(off_stats)) {
        round((on_stats$ner - off_stats$ner) * 100, 1)
      } else {
        NA
      }

      pair_key <- paste(sort(c(player1, player2)), collapse = "_")
      results[[pair_key]] <- list(
        players = paste(display1, display2, sep = " & "),
        player1 = player1,
        player2 = player2,
        player1Id = if (nrow(p1info) > 0) p1info$playerId[1] else NA,
        player2Id = if (nrow(p2info) > 0) p2info$playerId[1] else NA,
        onMin = round(on_stats$minutes, 1),
        offMin = if (!is.null(off_stats)) round(off_stats$minutes, 1) else NA,
        onORtg = round(on_stats$oer * 100, 1),
        offORtg = if (!is.null(off_stats)) round(off_stats$oer * 100, 1) else NA,
        onDRtg = round(on_stats$der * 100, 1),
        offDRtg = if (!is.null(off_stats)) round(off_stats$der * 100, 1) else NA,
        onNetRtg = round(on_stats$ner * 100, 1),
        offNetRtg = if (!is.null(off_stats)) round(off_stats$ner * 100, 1) else NA,
        netDiff = net_diff,
        onPoss = round(on_stats$pos),
        offPoss = if (!is.null(off_stats)) round(off_stats$pos) else NA,
        # Four Factors
        onTS = round(on_stats$ts * 100, 1),
        onEFG = round(on_stats$efg * 100, 1),
        onOppEFG = round(on_stats$opp_efg * 100, 1),
        onTOV = round(on_stats$tov_rate * 100, 1),
        onDRB = round(on_stats$drb_pct * 100, 1),
        onAST = round(on_stats$ast_rate * 100, 1)
      )
    }
  }

  return(results)
}

# =============================================================================
# Player Trio Statistics (Optimized)
# =============================================================================

calculate_trio_stats_optimized <- function(team_data, team_name, player_cols, player_info) {
  results <- list()

  if (length(player_cols) < 3) return(results)

  # Generate trios (now only for team's actual players!)
  trios <- combn(player_cols, 3, simplify = FALSE)

  for (trio_cols in trios) {
    if (!all(trio_cols %in% names(team_data))) next

    # Extract player nicks from column names
    trio <- sapply(trio_cols, function(col) sub("_[0-9]+_pista$", "", col))

    # All three players on court
    on_subset <- team_data[
      team_data[[trio_cols[1]]] == 1 &
      team_data[[trio_cols[2]]] == 1 &
      team_data[[trio_cols[3]]] == 1
    ]
    on_stats <- calculate_stats_for_subset(on_subset, team_name)

    # At least one player off court (NOT all three together)
    off_subset <- team_data[!(
      team_data[[trio_cols[1]]] == 1 &
      team_data[[trio_cols[2]]] == 1 &
      team_data[[trio_cols[3]]] == 1
    )]
    off_stats <- calculate_stats_for_subset(off_subset, team_name)

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

      # Calculate impact (net rating difference)
      net_diff <- if (!is.null(off_stats)) {
        round((on_stats$ner - off_stats$ner) * 100, 1)
      } else {
        NA
      }

      trio_key <- paste(sort(trio), collapse = "_")
      results[[trio_key]] <- list(
        players = paste(display_names, collapse = " & "),
        playerList = trio,
        playerIds = as.list(player_ids),
        onMin = round(on_stats$minutes, 1),
        offMin = if (!is.null(off_stats)) round(off_stats$minutes, 1) else NA,
        onORtg = round(on_stats$oer * 100, 1),
        offORtg = if (!is.null(off_stats)) round(off_stats$oer * 100, 1) else NA,
        onDRtg = round(on_stats$der * 100, 1),
        offDRtg = if (!is.null(off_stats)) round(off_stats$der * 100, 1) else NA,
        onNetRtg = round(on_stats$ner * 100, 1),
        offNetRtg = if (!is.null(off_stats)) round(off_stats$ner * 100, 1) else NA,
        netDiff = net_diff,
        onPoss = round(on_stats$pos),
        offPoss = if (!is.null(off_stats)) round(off_stats$pos) else NA,
        # Four Factors
        onTS = round(on_stats$ts * 100, 1),
        onEFG = round(on_stats$efg * 100, 1),
        onOppEFG = round(on_stats$opp_efg * 100, 1),
        onTOV = round(on_stats$tov_rate * 100, 1),
        onDRB = round(on_stats$drb_pct * 100, 1),
        onAST = round(on_stats$ast_rate * 100, 1)
      )
    }
  }

  return(results)
}

# =============================================================================
# Full 5-Man Lineup Statistics (New Feature)
# =============================================================================

calculate_lineup_stats_optimized <- function(team_data, team_name, player_cols, player_info) {
  results <- list()

  if (length(player_cols) < 5) return(results)

  # player_cols are already full column names like "García_20210685_pista"
  pista_cols <- player_cols[player_cols %in% names(team_data)]

  if (length(pista_cols) < 5) return(results)

  # Create lineup identifier for each row
  # A lineup is defined by exactly which 5 players are on court
  team_rows <- team_data[team.team_actual_name == team_name]

  # Build lineup string for each row
  lineup_ids <- apply(team_rows[, ..pista_cols], 1, function(row) {
    on_court <- names(row)[row == 1]
    if (length(on_court) == 5) {
      # Extract player nicks (remove _id_pista suffix)
      players_on <- sapply(on_court, function(col) sub("_[0-9]+_pista$", "", col))
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

  # Build a mapping from player nick to full column name
  nick_to_col <- setNames(pista_cols, sapply(pista_cols, function(col) sub("_[0-9]+_pista$", "", col)))

  # Calculate stats for each lineup
  for (lineup_id in valid_lineups) {
    lineup_players <- strsplit(lineup_id, "\\|")[[1]]

    # Get the full column names for these players
    pista_cols_lineup <- nick_to_col[lineup_players]

    # Skip if we can't find all columns
    if (any(is.na(pista_cols_lineup)) || !all(pista_cols_lineup %in% names(team_data))) next

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
        onMin = round(stats$minutes, 1),
        onORtg = round(stats$oer * 100, 1),
        onDRtg = round(stats$der * 100, 1),
        onNetRtg = round(stats$ner * 100, 1),
        onPoss = round(stats$pos),
        pts = stats$pts,
        ptsAllowed = stats$pts_allowed,
        # Four Factors
        onTS = round(stats$ts * 100, 1),
        onEFG = round(stats$efg * 100, 1),
        onOppEFG = round(stats$opp_efg * 100, 1),
        onTOV = round(stats$tov_rate * 100, 1),
        onOppTOV = round(stats$opp_tov_rate * 100, 1),
        onORB = round(stats$orb_pct * 100, 1),
        onDRB = round(stats$drb_pct * 100, 1),
        onFTr = round(stats$ft_rate * 100, 1),
        onAST = round(stats$ast_rate * 100, 1)
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

  # Convert to JSON-friendly format expected by React component
  # Format: { data: { "SEASON_TEAMNAME": { team, season, players, pairs, trios, lineups } } }
  json_data_inner <- list()

  for (team_name in names(lineup_data)) {
    team_data <- lineup_data[[team_name]]
    # Create key in format "SEASON_TEAMNAME"
    key <- paste(team_data$season, team_name, sep = "_")

    json_data_inner[[key]] <- list(
      team = team_data$team,
      season = team_data$season,
      # React expects "players" not "individual"
      players = if (length(team_data$individual) > 0) team_data$individual else list(),
      pairs = if (length(team_data$pairs) > 0) team_data$pairs else list(),
      trios = if (length(team_data$trios) > 0) team_data$trios else list(),
      lineups = if (length(team_data$lineups) > 0) team_data$lineups else list()
    )
  }

  # Wrap in "data" object as React expects
  json_data <- list(data = json_data_inner)

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
