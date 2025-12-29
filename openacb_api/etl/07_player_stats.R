# =============================================================================
# ACB Basketball Analytics Pipeline - Player Statistics Module
# =============================================================================
# Calculates player statistics from play-by-play data including:
# - Basic boxscore stats (points, rebounds, assists, etc.)
# - Minutes played (calculated from PBP on-court tracking)
# - Possessions
# - Percentile rankings
# Usage: calculate_player_stats(2025) or calculate_all_player_stats()
# =============================================================================

library(dplyr)
library(tidyr)

#' Calculate minutes played from play-by-play data
#'
#' Uses the crono column and player on-court tracking to calculate
#' how long each player was on the court.
#'
#' @param df Play-by-play data with player tracking columns
#' @param player_nicks Vector of player nicknames
#' @return Data frame with player minutes per game
#'
calculate_minutes <- function(df, player_nicks) {
  cat("→ Calculating minutes played (Vectorized)...\n")
  
  # 1. Prepare Data & Fix Types
  # Ensure time columns are numeric for correct sorting
  df_calc <- df %>%
    mutate(
      minute = as.numeric(minute),
      second = as.numeric(second),
      period = as.numeric(period)
    ) %>%
    # Remove rows without time info
    filter(!is.na(minute), !is.na(second))
  
  # 2. Calculate Time Differences using dplyr (Faster & Safer)
  df_calc <- df_calc %>%
    group_by(id_match) %>%
    # Sort: Period ascending, Time descending (10:00 -> 00:00)
    arrange(period, desc(minute), desc(second), .by_group = TRUE) %>%
    mutate(
      # Calculate seconds from the clock
      current_seconds = minute * 60 + second,
      
      # Get time of the PREVIOUS event (which happened 'earlier' in game time, i.e., higher clock value)
      prev_seconds = lag(current_seconds),
      prev_period = lag(period),
      
      # Calculate duration
      time_diff = prev_seconds - current_seconds
    ) %>%
    ungroup() %>%
    mutate(
      # CLEANUP:
      # 1. If period changed, time_diff is 0 (don't calc time across quarters)
      time_diff = ifelse(period != prev_period, 0, time_diff),
      # 2. If first row of match, time_diff is 0
      time_diff = ifelse(is.na(time_diff), 0, time_diff),
      # 3. Sanity check: remove negative times or huge gaps (>5 mins)
      time_diff = ifelse(time_diff < 0 | time_diff > 300, 0, time_diff)
    )
  
  # Debug Print: Check if we actually found time
  total_time_found <- sum(df_calc$time_diff, na.rm = TRUE)
  cat(sprintf("  DEBUG: Total game time found in dataset: %.1f minutes\n", total_time_found / 60))
  if(total_time_found == 0) warning("  WARNING: No time differences calculated. Check 'minute'/'second' columns.")
  
  # 3. Calculate Player Minutes (Matrix Method)
  # Identify the player tracking columns that actually exist in data
  valid_player_cols <- paste0(player_nicks, "_pista")
  valid_player_cols <- valid_player_cols[valid_player_cols %in% names(df_calc)]
  
  if (length(valid_player_cols) == 0) {
    stop("No player tracking columns (_pista) found in dataframe.")
  }
  
  # Convert tracking columns to a matrix (0/1)
  # We handle NAs by converting them to 0
  player_matrix <- as.matrix(df_calc[, valid_player_cols])
  player_matrix[is.na(player_matrix)] <- 0
  
  # Multiply Time Vector by Player Matrix
  # Result: Sum of seconds for each player
  # t(PlayerMatrix) %*% TimeDiff -> Vector of totals
  player_seconds <- crossprod(player_matrix, df_calc$time_diff)
  
  # 4. Format Results
  player_minutes <- data.frame(
    player_col = rownames(player_seconds),
    minutes = as.numeric(player_seconds) / 60,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      player = gsub("_pista$", "", player_col)
    ) %>%
    filter(minutes > 0) %>%
    select(player, minutes)
  
  # Add id_match breakdown if needed (The original function returned id_match)
  # If you need minutes PER GAME, we must group by id_match first.
  # Re-running the summation grouped by match for compatibility:
  
  results_list <- list()
  
  # Iterate matches for the final result structure
  # (Matrix method per match is still faster than loop per row)
  unique_matches <- unique(df_calc$id_match)
  
  # Simple progress bar
  pb <- txtProgressBar(min = 0, max = length(unique_matches), style = 3)
  
  for(i in seq_along(unique_matches)) {
    m <- unique_matches[i]
    match_data <- df_calc[df_calc$id_match == m, ]
    
    # Get Time vector
    t_vec <- match_data$time_diff
    
    # Get Player matrix for this match
    p_cols <- valid_player_cols # check all potential players
    p_mat <- as.matrix(match_data[, p_cols])
    p_mat[is.na(p_mat)] <- 0
    
    # Calc totals
    totals <- colSums(p_mat * t_vec, na.rm = TRUE)
    
    # Store
    totals <- totals[totals > 0]
    if(length(totals) > 0) {
      results_list[[i]] <- data.frame(
        player = gsub("_pista$", "", names(totals)),
        id_match = m,
        minutes = as.numeric(totals) / 60,
        stringsAsFactors = FALSE
      )
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  final_df <- do.call(rbind, results_list)
  return(final_df)
}

#' Calculate player statistics for a season
#'
#' @param season_id Integer year (e.g., 2025 for 2024-2025 season)
#' @param data_dir Base directory for data (default: "./data")
#' @param config_path Path to seasons.R config file
#' @return Data frame with player statistics
#'
calculate_player_stats <- function(season_id,
                                    data_dir = "./data",
                                    config_path = "./config/seasons.R") {

  # Load configuration
  source(config_path)
  season <- get_season_config(season_id)

  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("Calculating Player Stats:", season$season_name, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")

  # Define paths
  processed_dir <- file.path(data_dir, "processed")

  # Try to load adjusted data (with player tracking) first
  rds_file <- file.path(processed_dir, paste0("PbP_adjustedData", season_id, ".Rds"))
  csv_file <- file.path(processed_dir, paste0("PbP_adjustedData", season_id, ".csv"))

  if (file.exists(rds_file)) {
    cat("→ Loading adjusted PBP data (RDS)...\n")
    df <- readRDS(rds_file)
  } else if (file.exists(csv_file)) {
    cat("→ Loading adjusted PBP data (CSV)...\n")
    df <- read.csv(csv_file, encoding = "UTF-8", stringsAsFactors = FALSE)
  } else {
    stop("Adjusted PBP data not found. Run create_pbp_variables(", season_id, ") first.")
  }

  cat("  Rows:", format(nrow(df), big.mark = ","), "\n")

  # Get unique players
  players <- unique(df$license.licenseNick)
  players <- players[!is.na(players) & players != ""]
  cat("  Players:", length(players), "\n")

  # Get player tracking columns
  pista_cols <- grep("_pista$", names(df), value = TRUE)
  player_nicks <- gsub("_pista$", "", pista_cols)
  cat("  Tracked players:", length(player_nicks), "\n")

  # ==========================================================================
  # CALCULATE MINUTES
  # ==========================================================================

  player_minutes_df <- calculate_minutes(df, player_nicks)

  # Aggregate minutes per player
  minutes_summary <- player_minutes_df %>%
    group_by(player) %>%
    summarise(
      total_minutes = sum(minutes, na.rm = TRUE),
      games_played = n_distinct(id_match),
      .groups = "drop"
    ) %>%
    mutate(mpg = total_minutes / games_played)

  cat("\n  Minutes calculated for", nrow(minutes_summary), "players\n")

  # ==========================================================================
  # AGGREGATE BASIC BOXSCORE STATS
  # ==========================================================================

  cat("→ Aggregating boxscore statistics...\n")

  # Basic stats aggregation by player
  basic_stats <- df %>%
    filter(!is.na(license.licenseNick) & license.licenseNick != "") %>%
    group_by(license.licenseNick, team.team_actual_name) %>%
    summarise(
      # Shooting
      fgm2 = sum(T2A, na.rm = TRUE),
      fga2 = sum(T2I, na.rm = TRUE),
      fgm3 = sum(T3A, na.rm = TRUE),
      fga3 = sum(T3I, na.rm = TRUE),
      ftm = sum(T1A, na.rm = TRUE),
      fta = sum(T1I, na.rm = TRUE),
      points = sum(puntos, na.rm = TRUE),

      # Rebounds
      oreb = sum(reb_of, na.rm = TRUE),
      dreb = sum(reb_def, na.rm = TRUE),

      # Playmaking
      assists = sum(asistencias, na.rm = TRUE),
      turnovers = sum(perdida, na.rm = TRUE),

      # Defense
      steals = sum(recuperacion, na.rm = TRUE),
      blocks = sum(tapon, na.rm = TRUE),
      blocks_received = sum(tapon_rec, na.rm = TRUE),

      # Fouls
      fouls = sum(falta, na.rm = TRUE),
      fouls_received = sum(falta_rec, na.rm = TRUE),

      # Free throw trips (for possession calculation)
      ft_trips = sum(FT_trip, na.rm = TRUE),

      # Count games
      games = n_distinct(id_match),

      .groups = "drop"
    ) %>%
    rename(player = license.licenseNick, team = team.team_actual_name)

  # Calculate derived stats
  basic_stats <- basic_stats %>%
    mutate(
      # Totals
      rebounds = oreb + dreb,
      fgm = fgm2 + fgm3,
      fga = fga2 + fga3,

      # Percentages
      fg_pct = ifelse(fga > 0, fgm / fga * 100, 0),
      fg2_pct = ifelse(fga2 > 0, fgm2 / fga2 * 100, 0),
      fg3_pct = ifelse(fga3 > 0, fgm3 / fga3 * 100, 0),
      ft_pct = ifelse(fta > 0, ftm / fta * 100, 0),

      # Advanced
      efg = ifelse(fga > 0, (fgm2 + 1.5 * fgm3) / fga * 100, 0),
      ts = ifelse(fga + ft_trips > 0, points / (2 * (fga + ft_trips)) * 100, 0),

      # Possessions (individual contribution)
      possessions = fga + ft_trips + turnovers,

      # 3PT attempt rate
      three_rate = ifelse(fga > 0, fga3 / fga * 100, 0)
    )

  # ==========================================================================
  # MERGE MINUTES WITH STATS
  # ==========================================================================

  cat("→ Merging minutes with statistics...\n")

  # Join minutes data
  player_stats <- basic_stats %>%
    left_join(minutes_summary, by = "player") %>%
    mutate(
      # Use games from boxscore if minutes not available
      games = coalesce(games_played, games),
      total_minutes = coalesce(total_minutes, 0),
      mpg = coalesce(mpg, 0)
    ) %>%
    select(-games_played)

  # ==========================================================================
  # CALCULATE PER-GAME STATS
  # ==========================================================================

  cat("→ Calculating per-game statistics...\n")

  player_stats <- player_stats %>%
    mutate(
      ppg = points / games,
      rpg = rebounds / games,
      apg = assists / games,
      spg = steals / games,
      bpg = blocks / games,
      topg = turnovers / games,
      fpg = fouls / games,
      orebpg = oreb / games,
      drebpg = dreb / games,

      # Per-minute stats (per 40 minutes)
      pts_per40 = ifelse(total_minutes > 0, points / total_minutes * 40, 0),
      reb_per40 = ifelse(total_minutes > 0, rebounds / total_minutes * 40, 0),
      ast_per40 = ifelse(total_minutes > 0, assists / total_minutes * 40, 0),

      # Possessions per game
      poss_pg = possessions / games
    )

  # ==========================================================================
  # CALCULATE PERCENTILES
  # ==========================================================================

  cat("→ Calculating percentile rankings...\n")

  # Only calculate percentiles for players with minimum games
  min_games <- 5
  qualified <- player_stats$games >= min_games

  # Define stats to calculate percentiles for
  pct_stats <- c("ppg", "rpg", "orebpg", "drebpg", "apg", "spg", "bpg", "fpg", "mpg",
                 "fg_pct", "fg3_pct", "ft_pct", "efg", "ts",
                 "three_rate", "poss_pg")

  # Calculate percentiles
  for (stat in pct_stats) {
    pct_col <- paste0(stat, "_pct")
    qualified_values <- player_stats[[stat]][qualified]

    player_stats[[pct_col]] <- sapply(player_stats[[stat]], function(x) {
      if (is.na(x)) return(NA)
      ecdf_func <- ecdf(qualified_values)
      round(ecdf_func(x) * 100, 1)
    })
  }

  # Inverse percentile for turnovers (lower is better)
  player_stats$topg_pct <- sapply(player_stats$topg, function(x) {
    if (is.na(x)) return(NA)
    qualified_values <- player_stats$topg[qualified]
    ecdf_func <- ecdf(qualified_values)
    round((1 - ecdf_func(x)) * 100, 1)
  })

  # ==========================================================================
  # FINALIZE AND SAVE
  # ==========================================================================

  # Add season identifier
  player_stats$season <- season_id

  # Select and order columns
  final_stats <- player_stats %>%
    select(
      player, team, season, games, total_minutes, mpg,
      # Basic totals
      points, rebounds, oreb, dreb, assists, steals, blocks, turnovers, fouls,
      # Shooting totals
      fgm, fga, fgm2, fga2, fgm3, fga3, ftm, fta,
      # Per game
      ppg, rpg, orebpg, drebpg, apg, spg, bpg, topg, fpg,
      # Percentages
      fg_pct, fg2_pct, fg3_pct, ft_pct, efg, ts, three_rate,
      # Possessions
      possessions, poss_pg,
      # Percentiles
      ends_with("_pct")
    ) %>%
    arrange(desc(ppg))

  # Save results
  output_file <- file.path(processed_dir, paste0("PlayerStats", season_id, ".csv"))
  cat("→ Saving to:", output_file, "\n")
  write.csv(final_stats, output_file, row.names = FALSE, fileEncoding = "UTF-8")

  # Also save as RDS for efficiency
  rds_output <- file.path(processed_dir, paste0("PlayerStats", season_id, ".Rds"))
  saveRDS(final_stats, rds_output)

  # Report
  cat("\n✓ Player statistics complete!\n")
  cat("  Players:", nrow(final_stats), "\n")
  cat("  Metrics:", ncol(final_stats), "\n")
  cat("  Min games filter:", min_games, "\n")

  # Print top scorers
  cat("\n  Top 5 Scorers:\n")
  top5 <- final_stats %>%
    filter(games >= min_games) %>%
    head(5)

  for (i in seq_len(nrow(top5))) {
    cat(sprintf("    %d. %s (%s): %.1f PPG, %.1f RPG, %.1f APG in %.1f MPG\n",
                i, top5$player[i], top5$team[i],
                top5$ppg[i], top5$rpg[i], top5$apg[i], top5$mpg[i]))
  }

  invisible(final_stats)
}

#' Calculate player statistics for multiple seasons
#'
#' @param season_ids Vector of season IDs (default: all available)
#' @param ... Additional arguments passed to calculate_player_stats
#' @return Combined data frame with all seasons
#'
calculate_all_player_stats <- function(season_ids = NULL, ...) {
  source("./config/seasons.R")

  if (is.null(season_ids)) {
    season_ids <- get_available_seasons()
  }

  all_stats <- lapply(season_ids, function(sid) {
    tryCatch({
      calculate_player_stats(sid, ...)
    }, error = function(e) {
      cat("  Error processing season", sid, ":", e$message, "\n")
      NULL
    })
  })

  # Remove NULL entries
  all_stats <- all_stats[!sapply(all_stats, is.null)]

  if (length(all_stats) == 0) {
    stop("No player stats could be calculated")
  }

  combined <- do.call("rbind", all_stats)

  # Save combined file
  output_file <- file.path("./data/processed", "PlayerStats_AllSeasons.csv")
  write.csv(combined, output_file, row.names = FALSE, fileEncoding = "UTF-8")
  cat("\n✓ Combined stats saved to:", output_file, "\n")

  invisible(combined)
}

cat("✓ Player Statistics module loaded\n")
cat("  Usage: calculate_player_stats(2025) or calculate_all_player_stats()\n")
