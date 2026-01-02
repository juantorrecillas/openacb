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

  # Get player tracking columns (now format: {player_nick}_{license_id}_pista)
  pista_cols <- grep("_pista$", names(df), value = TRUE)

  # Extract player identifiers (includes both nick and license_id, e.g., "Kurucs_20211797")
  player_identifiers <- gsub("_pista$", "", pista_cols)
  cat("  Tracked players:", length(player_identifiers), "\n")

  # ==========================================================================
  # CALCULATE MINUTES
  # ==========================================================================

  # calculate_minutes expects player_nicks that will be used to form {nick}_pista columns
  # Now our columns are {nick}_{id}_pista, so we pass the full identifiers
  player_minutes_df <- calculate_minutes(df, player_identifiers)

  # Extract license_id and player nick from the identifiers
  # Format: {player_nick}_{license_id}
  player_map <- data.frame(
    player_identifier = player_identifiers,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      # Extract license_id (last part after final underscore)
      license_id = as.integer(sub(".*_([0-9]+)$", "\\1", player_identifier)),
      # Extract player nick (everything before the last underscore followed by digits)
      player = sub("_[0-9]+$", "", player_identifier)
    )

  # Join with original data to get team for each license_id AND match
  # This ensures players who change teams mid-season are handled correctly
  player_team_match_map <- df %>%
    filter(!is.na(license.id)) %>%
    select(license.id, team.team_actual_name, id_match) %>%
    distinct() %>%
    rename(license_id = license.id, team = team.team_actual_name)

  # Combine mappings with match info to preserve team changes
  player_full_map <- player_map %>%
    # Don't join by team yet - we'll do that with match info later
    select(player_identifier, license_id, player)

  # Join minutes with player info (player_identifier -> license_id)
  minutes_with_license <- player_minutes_df %>%
    rename(player_identifier = player) %>%
    left_join(player_full_map, by = "player_identifier")

  # Join with team info using both license_id AND id_match
  # This ensures correct team assignment for players who changed teams
  minutes_with_team <- minutes_with_license %>%
    left_join(player_team_match_map, by = c("license_id", "id_match"))

  # Aggregate minutes per player-team combination
  minutes_summary <- minutes_with_team %>%
    filter(!is.na(license_id) & !is.na(team)) %>%
    group_by(license_id, player, team) %>%
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
    group_by(license.id, license.licenseNick, license.licenseStr, team.team_actual_name) %>%
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
    rename(license_id = license.id, player = license.licenseNick, player_full = license.licenseStr, team = team.team_actual_name)

  # Calculate derived stats
  basic_stats <- basic_stats %>%
    mutate(
      # Create unique player ID using license_id + team for true uniqueness
      player_id = paste(license_id, team, sep = "_"),

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

      # Offensive Rating (points per 100 possessions)
      ortg = ifelse(possessions > 0, (points / possessions) * 100, 0),

      # 3PT attempt rate
      three_rate = ifelse(fga > 0, fga3 / fga * 100, 0)
    )

  # ==========================================================================
  # CALCULATE TEAM STATS WHILE PLAYER IS ON COURT
  # ==========================================================================

  cat("→ Calculating team stats while player on court...\n")

  # Get all tracked players with their license IDs and teams
  # Need to get unique player-team combinations from the data
  tracked_players_df <- df %>%
    filter(!is.na(license.id) & !is.na(license.licenseNick)) %>%
    select(license.id, license.licenseNick, team.team_actual_name) %>%
    distinct() %>%
    rename(license_id = license.id, player = license.licenseNick, team = team.team_actual_name)

  # Calculate team and opponent stats while each player is on court
  team_stats_on_court <- lapply(seq_len(nrow(tracked_players_df)), function(idx) {
    player_id <- tracked_players_df$license_id[idx]
    player <- tracked_players_df$player[idx]
    player_team <- tracked_players_df$team[idx]

    # Column name format: {player_nick}_{license_id}_pista
    pista_col <- paste0(player, "_", player_id, "_pista")

    # Skip if player tracking column doesn't exist
    if (!pista_col %in% names(df)) {
      return(NULL)
    }

    if (is.na(player_team)) return(NULL)

    # Filter to events where player is on court
    on_court_df <- df %>% filter(.data[[pista_col]] == 1)

    # Calculate team stats (player's team)
    team_stats <- on_court_df %>%
      filter(team.team_actual_name == player_team) %>%
      summarise(
        team_fga_on = sum(T2I + T3I, na.rm = TRUE),
        team_fgm_on = sum(T2A + T3A, na.rm = TRUE),
        team_fga2_on = sum(T2I, na.rm = TRUE),
        team_fgm2_on = sum(T2A, na.rm = TRUE),
        team_fta_on = sum(T1I, na.rm = TRUE),
        team_ftm_on = sum(T1A, na.rm = TRUE),
        team_ft_trips_on = sum(FT_trip, na.rm = TRUE),
        team_turnovers_on = sum(perdida, na.rm = TRUE),
        team_assists_on = sum(asistencias, na.rm = TRUE),
        team_oreb_on = sum(reb_of, na.rm = TRUE),
        team_dreb_on = sum(reb_def, na.rm = TRUE),
        team_poss_on = sum(T2I + T3I + FT_trip + perdida, na.rm = TRUE),
        .groups = "drop"
      )

    # Calculate opponent stats (opponent's team)
    opp_stats <- on_court_df %>%
      filter(team.team_actual_name != player_team) %>%
      summarise(
        opp_fga_on = sum(T2I + T3I, na.rm = TRUE),
        opp_fgm_on = sum(T2A + T3A, na.rm = TRUE),
        opp_fga2_on = sum(T2I, na.rm = TRUE),
        opp_fta_on = sum(T1I, na.rm = TRUE),
        opp_ft_trips_on = sum(FT_trip, na.rm = TRUE),
        opp_turnovers_on = sum(perdida, na.rm = TRUE),
        opp_oreb_on = sum(reb_of, na.rm = TRUE),
        opp_dreb_on = sum(reb_def, na.rm = TRUE),
        opp_poss_on = sum(T2I + T3I + FT_trip + perdida, na.rm = TRUE),
        .groups = "drop"
      )

    # Combine team and opponent stats
    combined_stats <- bind_cols(team_stats, opp_stats) %>%
      mutate(
        license_id = player_id,
        player = player,
        team = player_team
      )

    return(combined_stats)
  })

  # Combine all team stats
  team_stats_on_court <- bind_rows(team_stats_on_court) %>%
    filter(!is.na(player) & !is.na(license_id))

  # ==========================================================================
  # MERGE MINUTES WITH STATS
  # ==========================================================================

  cat("→ Merging minutes with statistics...\n")

  # Join minutes data (using license_id and team for unique matching)
  player_stats <- basic_stats %>%
    left_join(minutes_summary, by = c("license_id", "player", "team")) %>%
    mutate(
      # Use games from boxscore if minutes not available
      games = coalesce(games_played, games),
      total_minutes = coalesce(total_minutes, 0),
      mpg = coalesce(mpg, 0)
    ) %>%
    select(-games_played)

  # Join team stats (while player on court) - using license_id for proper matching
  player_stats <- player_stats %>%
    left_join(team_stats_on_court, by = c("license_id", "player", "team"))

  # ==========================================================================
  # CALCULATE ADVANCED STATS (USAGE RATE, ETC.)
  # ==========================================================================

  cat("→ Calculating advanced statistics...\n")

  player_stats <- player_stats %>%
    mutate(
      # CTG-style Usage Rate
      # Player contribution: FGA + TOV + FT trips
      # BUT: Assisted FGs count as 0.5, and assists also count as 0.5
      # We need to estimate assisted FGs - using league average ~60% of FGM are assisted
      # For now, we'll use a simplified version and can refine later

      # Simplified player usage numerator
      # FGA + turnovers + FT trips - 0.5 * (estimated assisted FGM) + 0.5 * assists
      # Estimate assisted FGM as: FGM * (team_assists / team_fgm) for approximation
      # For CTG accuracy, we'd need play-by-play assist tracking, but this is close

      assisted_fgm_est = ifelse(team_fgm_on > 0,
                                fgm * (team_assists_on / team_fgm_on),
                                0),

      player_poss_adj = fga + turnovers + ft_trips - 0.5 * assisted_fgm_est + 0.5 * assists,

      # Team possessions (adjusted for assists) while player on court
      team_assisted_fgm = team_fgm_on,  # All team made FGs while on court
      team_poss_adj = ifelse(!is.na(team_poss_on),
                             team_fga_on + team_turnovers_on + team_ft_trips_on -
                               0.5 * team_assisted_fgm + 0.5 * team_assists_on,
                             NA),

      # Usage Rate (as percentage)
      usg = ifelse(!is.na(team_poss_adj) & team_poss_adj > 0,
                   player_poss_adj / team_poss_adj * 100,
                   NA),

      # =======================================================================
      # REBOUND PERCENTAGES
      # =======================================================================

      # ORB% - Offensive Rebound Percentage
      # Player's offensive rebounds / Available offensive rebounds while on court
      # Available ORB = Team ORB + Opponent DRB
      available_oreb = team_oreb_on + opp_dreb_on,
      orb_pct = ifelse(!is.na(available_oreb) & available_oreb > 0,
                       oreb / available_oreb * 100,
                       NA),

      # DRB% - Defensive Rebound Percentage
      # Player's defensive rebounds / Available defensive rebounds while on court
      # Available DRB = Team DRB + Opponent ORB
      available_dreb = team_dreb_on + opp_oreb_on,
      drb_pct = ifelse(!is.na(available_dreb) & available_dreb > 0,
                       dreb / available_dreb * 100,
                       NA),

      # TRB% - Total Rebound Percentage
      # Player's total rebounds / Total available rebounds while on court
      available_treb = available_oreb + available_dreb,
      trb_pct = ifelse(!is.na(available_treb) & available_treb > 0,
                       rebounds / available_treb * 100,
                       NA),

      # =======================================================================
      # PLAYMAKING & DEFENSIVE PERCENTAGES
      # =======================================================================

      # AST% - Assist Percentage
      # Player's assists / Team's made FGs while on court (excluding player's own FGM)
      # Note: Ideally we'd exclude player's own FGM, but that requires tracking
      # For now: assists / (team_fgm - player_fgm)
      team_fgm_teammates = pmax(team_fgm_on - fgm, 0),
      ast_pct = ifelse(!is.na(team_fgm_teammates) & team_fgm_teammates > 0,
                       assists / team_fgm_teammates * 100,
                       NA),

      # STL% - Steal Percentage
      # Player's steals / Opponent possessions while on court
      stl_pct = ifelse(!is.na(opp_poss_on) & opp_poss_on > 0,
                       steals / opp_poss_on * 100,
                       NA),

      # BLK% - Block Percentage
      # Player's blocks / Opponent 2-point FGA while on court
      blk_pct = ifelse(!is.na(opp_fga2_on) & opp_fga2_on > 0,
                       blocks / opp_fga2_on * 100,
                       NA),

      # TOV% - Turnover Percentage
      # Turnovers per 100 plays (plays = FGA + 0.44 * FTA + TOV)
      plays = fga + 0.44 * fta + turnovers,
      tov_pct = ifelse(!is.na(plays) & plays > 0,
                       turnovers / plays * 100,
                       NA)
    )

  # ==========================================================================
  # CALCULATE ZONE SHOOTING STATS
  # ==========================================================================

  cat("→ Calculating zone shooting statistics...\n")

  # Load shot chart data for this season
  shot_file <- file.path(processed_dir, paste0("ShotChartData", season_id, ".csv"))

  if (file.exists(shot_file)) {
    shots <- read.csv(shot_file, encoding = "UTF-8", stringsAsFactors = FALSE)

    # Map R zones to CtG-style zones
    shots <- shots %>%
      mutate(
        ctg_zone = case_when(
          zone == "Zona (Restringida)" ~ "rim",
          zone == "Zona no restringida" ~ "short_mid",
          zone %in% c("Media Distancia Esquina", "Media Distancia Codo", "Media Distancia Centro") ~ "long_mid",
          zone == "Triple Esquina" ~ "corner_three",
          zone %in% c("Triple Codo", "Triple Centro") ~ "nc_three",
          TRUE ~ "other"
        )
      )

    # Aggregate by player (license.id) and team
    zone_stats <- shots %>%
      filter(!is.na(license.id) & !is.na(team.team_actual_name)) %>%
      group_by(license.id, team.team_actual_name, ctg_zone) %>%
      summarise(
        zone_fga = n(),
        zone_fgm = sum(made_numeric, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(license_id = license.id, team = team.team_actual_name)

    # Calculate total FGA per player-team for frequency calculation
    player_totals <- zone_stats %>%
      group_by(license_id, team) %>%
      summarise(total_fga = sum(zone_fga), .groups = "drop")

    # Join and calculate percentages
    zone_stats <- zone_stats %>%
      left_join(player_totals, by = c("license_id", "team")) %>%
      mutate(
        zone_fg_pct = ifelse(zone_fga > 0, zone_fgm / zone_fga * 100, NA),
        zone_freq = ifelse(total_fga > 0, zone_fga / total_fga * 100, NA)
      )

    # Pivot to wide format for each zone
    zone_freq_wide <- zone_stats %>%
      select(license_id, team, ctg_zone, zone_freq) %>%
      pivot_wider(
        names_from = ctg_zone,
        values_from = zone_freq,
        names_prefix = "freq_"
      )

    zone_fgpct_wide <- zone_stats %>%
      select(license_id, team, ctg_zone, zone_fg_pct) %>%
      pivot_wider(
        names_from = ctg_zone,
        values_from = zone_fg_pct,
        names_prefix = "fgpct_"
      )

    zone_fga_wide <- zone_stats %>%
      select(license_id, team, ctg_zone, zone_fga) %>%
      pivot_wider(
        names_from = ctg_zone,
        values_from = zone_fga,
        names_prefix = "fga_"
      )

    # Combine all zone stats
    zone_combined <- zone_freq_wide %>%
      left_join(zone_fgpct_wide, by = c("license_id", "team")) %>%
      left_join(zone_fga_wide, by = c("license_id", "team"))

    # Calculate aggregate zones (all_mid, all_three)
    zone_combined <- zone_combined %>%
      mutate(
        # All mid = short_mid + long_mid
        fga_all_mid = coalesce(fga_short_mid, 0) + coalesce(fga_long_mid, 0),
        freq_all_mid = coalesce(freq_short_mid, 0) + coalesce(freq_long_mid, 0),

        # All three = corner_three + nc_three
        fga_all_three = coalesce(fga_corner_three, 0) + coalesce(fga_nc_three, 0),
        freq_all_three = coalesce(freq_corner_three, 0) + coalesce(freq_nc_three, 0)
      )

    # For aggregate FG%, we need to recalculate from raw makes/attempts
    zone_agg_stats <- shots %>%
      filter(!is.na(license.id) & !is.na(team.team_actual_name)) %>%
      mutate(
        agg_zone = case_when(
          ctg_zone %in% c("short_mid", "long_mid") ~ "all_mid",
          ctg_zone %in% c("corner_three", "nc_three") ~ "all_three",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(agg_zone)) %>%
      group_by(license.id, team.team_actual_name, agg_zone) %>%
      summarise(
        agg_fga = n(),
        agg_fgm = sum(made_numeric, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(agg_fg_pct = ifelse(agg_fga > 0, agg_fgm / agg_fga * 100, NA)) %>%
      rename(license_id = license.id, team = team.team_actual_name) %>%
      select(license_id, team, agg_zone, agg_fg_pct) %>%
      pivot_wider(
        names_from = agg_zone,
        values_from = agg_fg_pct,
        names_prefix = "fgpct_"
      )

    # Join aggregate FG% to zone_combined
    zone_combined <- zone_combined %>%
      left_join(zone_agg_stats, by = c("license_id", "team"))

    # Join zone stats to player_stats
    player_stats <- player_stats %>%
      left_join(zone_combined, by = c("license_id", "team"))

    cat("  Zone stats calculated for", nrow(zone_combined), "player-team combinations\n")

    # =======================================================================
    # CALCULATE OPPONENT ZONE SHOOTING (ON COURT vs TEAM'S OPPONENTS AVERAGE)
    # =======================================================================

    cat("→ Calculating opponent zone shooting vs team's opponents average...\n")

    # Step 1: Calculate each TEAM'S OPPONENTS AVERAGE FG% per zone
    # For each team, find all shots taken BY their opponents across all their matches

    # Get all matches with team information
    matches_teams <- df %>%
      select(id_match, team.team_actual_name) %>%
      distinct() %>%
      rename(team = team.team_actual_name)

    # For each team, get all shots taken by their opponents
    team_opp_shots_list <- lapply(unique(matches_teams$team), function(ref_team) {
      # Get all matches this team played
      team_matches <- matches_teams %>%
        filter(team == ref_team) %>%
        pull(id_match) %>%
        unique()

      # Get shots from those matches that were NOT taken by this team
      opp_shots <- shots %>%
        filter(
          id_match %in% team_matches,
          team.team_actual_name != ref_team
        ) %>%
        mutate(reference_team = ref_team)

      return(opp_shots)
    })

    team_opp_shots <- bind_rows(team_opp_shots_list)

    # Calculate average FG% by zone for each team's opponents
    team_opp_avg_by_zone <- team_opp_shots %>%
      filter(!is.na(ctg_zone) & ctg_zone != "other") %>%
      group_by(reference_team, ctg_zone) %>%
      summarise(
        opp_fga = n(),
        opp_fgm = sum(made_numeric, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(team_opp_fg_pct = ifelse(opp_fga > 0, opp_fgm / opp_fga * 100, 0))

    # Also calculate aggregate zones (all_mid, all_three) for team opponents
    team_opp_avg_agg <- team_opp_shots %>%
      filter(!is.na(ctg_zone) & ctg_zone != "other") %>%
      mutate(
        agg_zone = case_when(
          ctg_zone %in% c("short_mid", "long_mid") ~ "all_mid",
          ctg_zone %in% c("corner_three", "nc_three") ~ "all_three",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(agg_zone)) %>%
      group_by(reference_team, agg_zone) %>%
      summarise(
        opp_fga = n(),
        opp_fgm = sum(made_numeric, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(team_opp_fg_pct = ifelse(opp_fga > 0, opp_fgm / opp_fga * 100, 0)) %>%
      rename(ctg_zone = agg_zone)

    # Combine individual and aggregate team opponents averages
    team_opp_avg_all <- bind_rows(team_opp_avg_by_zone, team_opp_avg_agg)

    cat("  Team opponents averages calculated for", length(unique(team_opp_avg_all$reference_team)), "teams\n")

    # Step 2: Get PBP data with player on-court tracking
    # Join zone data from shot chart to PBP by matching on game/period/minute/second/player
    shots_for_join <- shots %>%
      mutate(
        shot_key = paste(id_match, period, minute, second, id_license, sep = "_")
      ) %>%
      select(shot_key, ctg_zone) %>%
      distinct()  # Remove duplicates

    # Note: T2I/T2A/T3I/T3A are 0/1 binary columns, not NA-based
    pbp_shots <- df %>%
      filter(T2I == 1 | T3I == 1) %>%
      mutate(
        made_numeric = ifelse(T2A == 1 | T3A == 1, 1, 0),
        shot_key = paste(id_match, period, minute, second, license.id, sep = "_")
      ) %>%
      left_join(shots_for_join, by = "shot_key", relationship = "many-to-many") %>%
      mutate(
        # Fallback: if zone not matched from shot chart, use shot type
        ctg_zone = ifelse(is.na(ctg_zone),
                         ifelse(T2I == 1, "long_mid", "nc_three"),
                         ctg_zone)
      )

    cat("  PBP shots with zones: ", nrow(pbp_shots), "\n")

    # Step 3: For each player, calculate opponent FG% by zone when ON court
    opp_zone_list <- lapply(seq_len(nrow(tracked_players_df)), function(idx) {
      player_id <- tracked_players_df$license_id[idx]
      player <- tracked_players_df$player[idx]
      player_team <- tracked_players_df$team[idx]

      # Column name format: {player_nick}_{license_id}_pista
      pista_col <- paste0(player, "_", player_id, "_pista")

      # Skip if player tracking column doesn't exist
      if (!pista_col %in% names(pbp_shots)) {
        return(NULL)
      }

      if (is.na(player_team)) return(NULL)

      # Filter to opponent shots when player is ON court
      opp_on <- pbp_shots %>%
        filter(
          team.team_actual_name != player_team,
          .data[[pista_col]] == 1,
          ctg_zone != "other"
        )

      if (nrow(opp_on) == 0) return(NULL)

      # Calculate opponent FG% by zone when player ON court
      on_stats <- opp_on %>%
        group_by(ctg_zone) %>%
        summarise(
          on_fga = n(),
          on_fgm = sum(made_numeric, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(on_fg_pct = ifelse(on_fga > 0, on_fgm / on_fga * 100, NA))

      # Also calculate aggregate zones
      on_agg <- opp_on %>%
        mutate(
          agg_zone = case_when(
            ctg_zone %in% c("short_mid", "long_mid") ~ "all_mid",
            ctg_zone %in% c("corner_three", "nc_three") ~ "all_three",
            TRUE ~ NA_character_
          )
        ) %>%
        filter(!is.na(agg_zone)) %>%
        group_by(agg_zone) %>%
        summarise(
          on_fga = n(),
          on_fgm = sum(made_numeric, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(on_fg_pct = ifelse(on_fga > 0, on_fgm / on_fga * 100, NA)) %>%
        rename(ctg_zone = agg_zone)

      # Combine individual and aggregate stats
      all_stats <- bind_rows(on_stats, on_agg)

      # Join with TEAM'S opponents averages and calculate difference
      result <- all_stats %>%
        left_join(
          team_opp_avg_all %>%
            filter(reference_team == player_team) %>%
            select(ctg_zone, team_opp_fg_pct),
          by = "ctg_zone"
        ) %>%
        mutate(
          diff_vs_team_opp = on_fg_pct - team_opp_fg_pct,  # Negative = good defense (below team's opp avg)
          license_id = player_id,
          team = player_team
        )

      return(result)
    })

    # Combine all opponent zone stats (filter out NULLs first)
    opp_zone_list <- Filter(Negate(is.null), opp_zone_list)

    if (length(opp_zone_list) > 0) {
      opp_zone_stats <- bind_rows(opp_zone_list)

      cat("  Opponent stats calculated for", length(opp_zone_list), "players\n")

      # Pivot to wide format: on FG%, diff vs league, FGA
      opp_on_wide <- opp_zone_stats %>%
        select(license_id, team, ctg_zone, on_fg_pct) %>%
        pivot_wider(
          names_from = ctg_zone,
          values_from = on_fg_pct,
          names_prefix = "opp_on_fgpct_"
        )

      opp_diff_wide <- opp_zone_stats %>%
        select(license_id, team, ctg_zone, diff_vs_team_opp) %>%
        pivot_wider(
          names_from = ctg_zone,
          values_from = diff_vs_team_opp,
          names_prefix = "opp_diff_"
        )

      opp_fga_wide <- opp_zone_stats %>%
        select(license_id, team, ctg_zone, on_fga) %>%
        pivot_wider(
          names_from = ctg_zone,
          values_from = on_fga,
          names_prefix = "opp_fga_"
        )

      # Combine all opponent zone stats
      opp_combined <- opp_on_wide %>%
        left_join(opp_diff_wide, by = c("license_id", "team")) %>%
        left_join(opp_fga_wide, by = c("license_id", "team"))
    } else {
      opp_combined <- data.frame()
    }

    # Join opponent zone stats to player_stats
    if (nrow(opp_combined) > 0) {
      player_stats <- player_stats %>%
        left_join(opp_combined, by = c("license_id", "team"))
      cat("  Opponent zone stats joined for", nrow(opp_combined), "player-team combinations\n")
    } else {
      cat("  ⚠ No opponent zone stats calculated\n")
    }
  } else {
    cat("  ⚠ Shot chart data not found, skipping zone stats\n")
  }

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

  # Determine if this is the most recent season
  all_seasons <- get_available_seasons()
  most_recent_season <- max(all_seasons)
  is_most_recent <- (season_id == most_recent_season)

  # Apply different thresholds based on season
  # Most recent season: 5+ games AND 50+ minutes
  # Previous seasons: 10+ games AND 150+ minutes
  if (is_most_recent) {
    qualified <- player_stats$games >= 5 & player_stats$total_minutes >= 50
    cat("  Using most recent season threshold: 5+ games AND 50+ minutes\n")
  } else {
    qualified <- player_stats$games >= 10 & player_stats$total_minutes >= 150
    cat("  Using previous season threshold: 10+ games AND 150+ minutes\n")
  }
  cat("  Qualified players for percentile calculation:", sum(qualified), "of", nrow(player_stats), "\n")

  # Store qualified status in dataframe for later use
  player_stats$qualified <- qualified

  # Define stats to calculate percentiles for (higher is better)
  pct_stats <- c("ppg", "rpg", "orebpg", "drebpg", "apg", "spg", "bpg", "fpg", "mpg",
                 "fg_pct", "fg3_pct", "ft_pct", "efg", "ts", "ortg",
                 "three_rate", "poss_pg", "usg",
                 "orb_pct", "drb_pct", "trb_pct", "ast_pct", "stl_pct", "blk_pct")

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

  # Inverse percentiles for stats where lower is better (turnovers)
  inverse_stats <- c("topg", "tov_pct")
  for (stat in inverse_stats) {
    pct_col <- paste0(stat, "_pct")
    if (pct_col == "tov_pct_pct") pct_col <- "tov_pct_pctile"  # Avoid naming conflict

    player_stats[[pct_col]] <- sapply(player_stats[[stat]], function(x) {
      if (is.na(x)) return(NA)
      qualified_values <- player_stats[[stat]][qualified]
      ecdf_func <- ecdf(qualified_values)
      round((1 - ecdf_func(x)) * 100, 1)
    })
  }

  # ==========================================================================
  # FINALIZE AND SAVE
  # ==========================================================================

  # Add season identifier
  player_stats$season <- season_id

  # Define zone stat columns (may not exist if shot data unavailable)
  zone_cols <- c(
    "freq_rim", "freq_short_mid", "freq_long_mid", "freq_all_mid",
    "freq_corner_three", "freq_nc_three", "freq_all_three",
    "fgpct_rim", "fgpct_short_mid", "fgpct_long_mid", "fgpct_all_mid",
    "fgpct_corner_three", "fgpct_nc_three", "fgpct_all_three",
    "fga_rim", "fga_short_mid", "fga_long_mid", "fga_all_mid",
    "fga_corner_three", "fga_nc_three", "fga_all_three",
    # Opponent zone stats (defensive impact)
    "opp_on_fgpct_rim", "opp_on_fgpct_short_mid", "opp_on_fgpct_long_mid", "opp_on_fgpct_all_mid",
    "opp_on_fgpct_corner_three", "opp_on_fgpct_nc_three", "opp_on_fgpct_all_three",
    "opp_diff_rim", "opp_diff_short_mid", "opp_diff_long_mid", "opp_diff_all_mid",
    "opp_diff_corner_three", "opp_diff_nc_three", "opp_diff_all_three",
    "opp_fga_rim", "opp_fga_short_mid", "opp_fga_long_mid", "opp_fga_all_mid",
    "opp_fga_corner_three", "opp_fga_nc_three", "opp_fga_all_three"
  )

  # Select and order columns
  final_stats <- player_stats %>%
    select(
      player_id, license_id, player, player_full, team, season, games, total_minutes, mpg, qualified,
      # Basic totals
      points, rebounds, oreb, dreb, assists, steals, blocks, turnovers, fouls,
      # Shooting totals
      fgm, fga, fgm2, fga2, fgm3, fga3, ftm, fta,
      # Per game
      ppg, rpg, orebpg, drebpg, apg, spg, bpg, topg, fpg,
      # Percentages
      fg_pct, fg2_pct, fg3_pct, ft_pct, efg, ts, three_rate,
      # Possessions & Usage
      possessions, poss_pg, ortg, usg,
      # Advanced Rate Stats
      orb_pct, drb_pct, trb_pct, ast_pct, stl_pct, blk_pct, tov_pct,
      # Zone Shooting Stats
      any_of(zone_cols),
      # Percentiles (explicitly named to avoid conflicts)
      ppg_pct, rpg_pct, orebpg_pct, drebpg_pct, apg_pct, spg_pct, bpg_pct, fpg_pct, mpg_pct,
      fg_pct_pct, fg3_pct_pct, ft_pct_pct, efg_pct, ts_pct, ortg_pct,
      three_rate_pct, poss_pg_pct, usg_pct,
      orb_pct_pct, drb_pct_pct, trb_pct_pct, ast_pct_pct, stl_pct_pct, blk_pct_pct,
      topg_pct, tov_pct_pctile
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
  cat("  Qualified players:", sum(qualified), "\n")

  # Print top scorers (using qualified players)
  cat("\n  Top 5 Scorers:\n")
  top5 <- final_stats %>%
    filter(qualified) %>%
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
