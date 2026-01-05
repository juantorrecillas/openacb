# =============================================================================
# EXPERIMENTAL MINUTES CALCULATION PIPELINE
# =============================================================================
# A robust, event-stream based approach to calculating player minutes.
# Eliminates synchronization issues by processing game events sequentially.
#
# Usage:
# source("openacb_api/etl/experimental_minutes.R")
# df_minutes <- calculate_minutes_robust(2025)
# =============================================================================

library(dplyr)
library(tidyr)

calculate_minutes_robust <- function(season_id, data_dir = "./data") {
  
  cat("\n======================================================\n")
  cat("ROBUST MINUTES CALCULATION (Event Stream Approach)\n")
  cat("======================================================\n")
  
  # 1. Load Data
  processed_dir <- file.path(data_dir, "processed")
  input_file <- file.path(processed_dir, paste0("FinalData_", season_id, ".csv"))
  
  if (!file.exists(input_file)) {
    # Try alternate path if running from root
    input_file <- file.path("openacb_api", "data", "processed", paste0("FinalData_", season_id, ".csv"))
  }
  
  cat("→ Loading data from:", input_file, "\n")
  df <- read.csv(input_file, encoding = "UTF-8", stringsAsFactors = FALSE)
  cat("  Rows:", format(nrow(df), big.mark = ","), "\n")
  
  # 2. Prepare Data
  # We need strictly sorted events
  cat("→ Sorting events chronologically...\n")
  
  df_events <- df %>%
    mutate(
      minute = as.numeric(minute),
      second = as.numeric(second),
      period = as.numeric(period),
      # Create absolute time seconds (descending within period)
      game_seconds_remaining = minute * 60 + second
    ) %>%
    # Filter out bad data
    filter(!is.na(period), !is.na(minute), !is.na(second)) %>%
    group_by(id_match) %>%
    # STRICT SORT: Period ASC > Time DESC (Order removed for safety)
    arrange(period, desc(minute), desc(second), .by_group = TRUE) %>%
    ungroup()
  
  # 3. Calculate Time Deltas
  # Calculate duration from current event to next event
  cat("→ Calculating time intervals...\n")
  
  df_events <- df_events %>%
    group_by(id_match) %>%
    mutate(
      next_period = lead(period),
      next_remaining = lead(game_seconds_remaining),
      
      # Duration: Current Time - Next Time
      # If period changes, duration is 0 (clock resets)
      duration = ifelse(period != next_period, 0, game_seconds_remaining - next_remaining),
      
      # Fix NAs at end of game/period
      duration = ifelse(is.na(duration), 0, duration),
      
      # Sanity check: remove negative time (shouldn't happen with correct sort)
      duration = pmax(0, duration) 
    ) %>%
    ungroup()
  
  # 4. Process State (Iterative)
  # Unfortuntely, vectorization is hard for state-dependent logic (lineups).
  # We will iterate through matches, but optimize by using a simplified event list.
  
  cat("→ Processing player tracking state (this may take a moment)...\n")
  
  # Select only columns needed for tracking
  events_slim <- df_events %>%
    select(id_match, period, type.description, license.id, license.licenseNick, team.team_actual_name, duration) %>%
    mutate(
      # Encode event types
      is_starter = type.description == "Cinco Inicial",
      is_sub_in = type.description == "Entra Pista",
      is_sub_out = type.description == "Sale Pista"
    )
  
  # Storage for results
  # We will accumulate seconds directly into a list/environment
  player_seconds <- new.env(hash = TRUE)
  
  # Helper to add time
  add_time <- function(player_id, seconds) {
    if (is.na(player_id)) return()
    key <- as.character(player_id)
    if (exists(key, envir = player_seconds)) {
      player_seconds[[key]] <- player_seconds[[key]] + seconds
    } else {
      player_seconds[[key]] <- seconds
    }
  }
  
  # Iterate by Match
  match_ids <- unique(events_slim$id_match)
  pb <- txtProgressBar(min = 0, max = length(match_ids), style = 3)
  
  for (i in seq_along(match_ids)) {
    m_id <- match_ids[i]
    m_events <- events_slim[events_slim$id_match == m_id, ]
    
    # State: Current Lineup for tracking players
    # Map: Team Name -> Vector of License IDs
    lineups <- list() 
    
    # Iterate events
    # Note: Using a for-loop in R is slow, but for ~400 events/game * 300 games = 120k iterations
    # it might be acceptable. Let's see. 
    # Optimization: Filter to only lineup-changing events OR events with duration > 0?
    # No, we need to process every duration.
    
    for (j in 1:nrow(m_events)) {
      ev <- m_events[j, ]
      p_id <- ev$license.id
      team <- ev$team.team_actual_name
      dur <- ev$duration
      
      # 1. Update State (Lineups)
      if (ev$is_starter) {
        # Cinco Inicial: Reset/ensure player is in lineup
        if (is.null(lineups[[team]])) lineups[[team]] <- c()
        lineups[[team]] <- unique(c(lineups[[team]], p_id))
        
      } else if (ev$is_sub_in) {
        if (is.null(lineups[[team]])) lineups[[team]] <- c()
        lineups[[team]] <- unique(c(lineups[[team]], p_id))
        
      } else if (ev$is_sub_out) {
        if (!is.null(lineups[[team]])) {
          lineups[[team]] <- lineups[[team]][lineups[[team]] != p_id]
        }
      }
      
      # 2. Attribute Time (Duration to NEXT event)
      # Time is credited to everyone CURRENTLY in the lineup
      # Note: If this event was "Sale Pista", p_id was just removed. Correct.
      # Note: If this event was "Entra Pista", p_id was just added. Correct.
      
      if (dur > 0) {
         # Add for all teams
         for (tm in names(lineups)) {
           active_players <- lineups[[tm]]
           if (length(active_players) > 0) {
             for (ap in active_players) {
               add_time(ap, dur)
             }
           }
         }
      }
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  # 5. Format Results
  cat("\n→ Formatting results...\n")
  
  results_df <- data.frame(
    license_id = names(player_seconds),
    seconds = unlist(as.list(player_seconds)),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      license_id = as.integer(license_id),
      minutes = seconds / 60
    )
  
  # Join with player info (from original dataframe) to get Names
  player_info <- df %>%
    select(license.id, license.licenseNick, team.team_actual_name) %>%
    distinct() %>%
    rename(license_id = license.id, player = license.licenseNick, team = team.team_actual_name) %>%
    filter(!is.na(license_id)) %>%
    # Handle duplicates (take first)
    group_by(license_id) %>%
    slice(1) %>%
    ungroup()
  
  final_stats <- results_df %>%
    left_join(player_info, by = "license_id") %>%
    select(license_id, player, team, minutes) %>%
    arrange(desc(minutes))
    
  # Check Targets
  check_targets(final_stats, df)
  
  return(final_stats)
}

check_targets <- function(stats, raw_df) {
  cat("\n------------------------------------------------------\n")
  cat("TARGET VALIDATION\n")
  cat("------------------------------------------------------\n")
  
  targets <- list(
    list(name = "Ennis", target = 28.4),
    list(name = "Kurucs", target = 25.4)
  )
  
  # Calculate games played for these players to get MPG
  # (Simple count of matches where they appear in roster/events)
  
  for (t in targets) {
    # Find player
    p_data <- stats %>% filter(grepl(t$name, player, ignore.case = TRUE))
    
    if (nrow(p_data) > 0) {
      # Get total minutes
      total_min <- sum(p_data$minutes)
      
      # Get games count from raw data
      p_id <- p_data$license_id[1]
      
      # Count matches where this ID appears in events
      games <- raw_df %>% 
        filter(license.id == p_id) %>%
        pull(id_match) %>%
        n_distinct()
        
      mpg <- total_min / games
      
      cat(sprintf("Player: %s | ID: %d\n", t$name, p_id))
      cat(sprintf("  Total Minutes: %.1f\n", total_min))
      cat(sprintf("  Games Played:  %d\n", games))
      cat(sprintf("  MPG Calculated: %.2f\n", mpg))
      cat(sprintf("  MPG Target:     %.2f\n", t$target))
      cat(sprintf("  Difference:     %.2f\n", mpg - t$target))
      cat("------------------------------------------------------\n")
    } else {
      cat(sprintf("Player %s not found in results.\n", t$name))
    }
  }
}

# Auto-run if sourcing
if(sys.nframe() == 0) {
  # Try to detect season
  season <- 2025
  cat("Running standalone for Season", season, "\n")
  calculate_minutes_robust(season)
}
