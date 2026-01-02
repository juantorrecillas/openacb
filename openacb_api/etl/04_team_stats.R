# =============================================================================
# ACB Basketball Analytics Pipeline - Team Statistics Module
# =============================================================================
# Calculates advanced team statistics from play-by-play data.
# Includes Four Factors, offensive/defensive ratings, and opponent stats.
# Usage: calculate_team_stats(2025) or calculate_all_team_stats()
# =============================================================================

library(dplyr)

# Load the clean module for team name standardization
source("./etl/02_clean.R")

#' Calculate advanced team statistics for a season
#' 
#' @param season_id Integer year (e.g., 2025 for 2024-2025 season)
#' @param data_dir Base directory for data (default: "./data")
#' @param config_path Path to seasons.R config file
#' @return Data frame with team statistics
#' 
calculate_team_stats <- function(season_id,
                                  data_dir = "./data",
                                  config_path = "./config/seasons.R") {
  
  # Load configuration
  source(config_path)
  season <- get_season_config(season_id)
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("Calculating Team Stats:", season$season_name, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")
  
  # Define paths
  processed_dir <- file.path(data_dir, "processed")
  input_file <- file.path(processed_dir, paste0("FinalData_", season_id, ".csv"))
  
  if (!file.exists(input_file)) {
    stop("Processed data file not found: ", input_file,
         "\nRun clean_pbp(", season_id, ") first.")
  }
  
  # Load data
  cat("→ Loading data...\n")
  df <- read.csv(input_file, encoding = "UTF-8", stringsAsFactors = FALSE)
  
  # Apply team name standardization - this is critical for consistent team names
  cat("→ Applying team name standardization...\n")
  df <- standardize_team_names(df, season_id, config_path)
  
  # Verify team names are correct
  unique_teams <- unique(na.omit(df$team.team_actual_name))
  cat("  Unique teams found:", length(unique_teams), "\n")
  for (team in sort(unique_teams)) {
    cat(sprintf("    - %s\n", team))
  }
  
  # Team aggregations
  cat("→ Aggregating team statistics...\n")
  
  temp <- df %>%
    group_by(team.team_actual_name) %>%
    summarise(across(all_of(BOXSCORE_COLUMNS), sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      pos = T2I + T3I + FT_trip - reb_of + perdida,
      ngames = salto_ganado + salto_perdido
    )
  
  # Opponent aggregations
  cat("→ Aggregating opponent statistics...\n")
  
  temp_opp <- df %>%
    group_by(opponent) %>%
    summarise(across(all_of(BOXSCORE_COLUMNS), sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(pos = T2I + T3I + FT_trip - reb_of + perdida)
  
  # Merge team and opponent data
  cat("→ Merging and calculating advanced stats...\n")
  
  merged <- merge(
    temp, temp_opp,
    by.x = "team.team_actual_name",
    by.y = "opponent",
    suffixes = c("", "_opponent")
  )
  
  # Calculate advanced statistics
  stats <- merged %>%
    mutate(
      # Team offensive stats
      threefg = T3A / T3I,
      tci = T2I + T3I,
      threeatt_rate = T3I / tci,
      oer = (puntos / pos) * 100,
      der = (puntos_opponent / pos_opponent) * 100,
      S_DefReb = reb_def / (reb_def + reb_of_opponent),
      S_OffReb = reb_of / (reb_def_opponent + reb_of),
      S_Reb = (reb_def + reb_of) / (reb_def + reb_of + reb_of_opponent + reb_def_opponent),
      S_assist = asistencias / (T2A + T3A),
      ts = puntos / (2 * (T2I + T3I + FT_trip)),
      efg = (T2A + 1.5 * T3A) / (T2I + T3I),
      S_Tov = perdida / pos,
      S_steal = recuperacion / pos_opponent,
      S_blocks = tapon / pos_opponent,
      FT_rate = T1A / (T2I + T3I),

      # Opponent stats
      threefg_opponent = T3A_opponent / T3I_opponent,
      tci_opponent = T2I_opponent + T3I_opponent,
      threeatt_rate_opponent = T3I_opponent / tci_opponent,
      oer_opponent = (puntos_opponent / pos_opponent) * 100,
      der_opponent = (puntos / pos) * 100,
      S_DefReb_opponent = reb_def_opponent / (reb_def_opponent + reb_of),
      S_OffReb_opponent = reb_of_opponent / (reb_def_opponent + reb_of_opponent),
      S_assist_opponent = asistencias_opponent / (T2A_opponent + T3A_opponent),
      ts_opponent = puntos_opponent / (2 * (T2I_opponent + T3I_opponent + FT_trip_opponent)),
      efg_opponent = (T2A_opponent + 1.5 * T3A_opponent) / (T2I_opponent + T3I_opponent),
      S_Tov_opponent = perdida_opponent / pos_opponent,
      S_steal_opponent = recuperacion_opponent / pos,
      S_blocks_opponent = tapon_opponent / pos,
      FT_rate_opponent = T1A_opponent / (T2I_opponent + T3I_opponent),
      tecnica_opponent = tecnica_opponent,

      # Season identifier
      year = season_id
    )
  
  # Calculate per-game boxscore stats
  # Note: PBP data counts events for both teams, so we divide by 2 to get per-team values
  stats <- stats %>%
    mutate(
      # Pace = possessions per game (divide by 2 due to PBP double-counting)
      pace = pos / ngames / 2,

      # Per-game boxscore stats (team) - derived from efficiency and pace
      ppg = oer * pace / 100,  # Points = ORtg * possessions / 100
      rpg = (reb_def + reb_of) / ngames / 2,  # Divide by 2 to fix PBP double-counting
      orebpg = reb_of / ngames / 2,
      drebpg = reb_def / ngames / 2,
      apg = asistencias / ngames / 2,
      spg = recuperacion / ngames / 2,
      bpg = tapon / ngames / 2,
      topg = perdida / ngames / 2,
      fpg = falta / ngames / 2,
      fgm_pg = (T2A + T3A) / ngames / 2,
      fga_pg = (T2I + T3I) / ngames / 2,
      fg3m_pg = T3A / ngames / 2,
      fg3a_pg = T3I / ngames / 2,
      ftm_pg = T1A / ngames / 2,
      fta_pg = T1I / ngames / 2,
      fg_pct = (T2A + T3A) / (T2I + T3I) * 100,  # Percentages don't need adjustment
      ft_pct = T1A / T1I * 100,

      # Per-game boxscore stats (opponent) - derived from efficiency
      opp_ppg = der * pace / 100,  # Opponent points = DRtg * possessions / 100
      opp_rpg = (reb_def_opponent + reb_of_opponent) / ngames / 2,
      opp_orebpg = reb_of_opponent / ngames / 2,
      opp_drebpg = reb_def_opponent / ngames / 2,
      opp_apg = asistencias_opponent / ngames / 2,
      opp_spg = recuperacion_opponent / ngames / 2,
      opp_bpg = tapon_opponent / ngames / 2,
      opp_topg = perdida_opponent / ngames / 2,
      opp_fpg = falta_opponent / ngames / 2,
      opp_fgm_pg = (T2A_opponent + T3A_opponent) / ngames / 2,
      opp_fga_pg = (T2I_opponent + T3I_opponent) / ngames / 2,
      opp_fg3m_pg = T3A_opponent / ngames / 2,
      opp_fg3a_pg = T3I_opponent / ngames / 2,
      opp_ftm_pg = T1A_opponent / ngames / 2,
      opp_fta_pg = T1I_opponent / ngames / 2,
      opp_fg_pct = (T2A_opponent + T3A_opponent) / (T2I_opponent + T3I_opponent) * 100,
      opp_ft_pct = T1A_opponent / T1I_opponent * 100
    )

  # Select final columns
  final_stats <- stats %>%
    select(
      team.team_actual_name, ngames, year,
      # Team boxscore per-game stats
      ppg, rpg, orebpg, drebpg, apg, spg, bpg, topg, fpg,
      fgm_pg, fga_pg, fg3m_pg, fg3a_pg, ftm_pg, fta_pg,
      fg_pct, ft_pct, pace,
      # Team advanced stats
      threefg, threeatt_rate, oer, der,
      S_DefReb, S_OffReb, S_assist, S_blocks, ts, efg,
      S_Tov, S_steal, FT_rate, tecnica, altercado, revision,
      salto_ganado, salto_perdido,
      # Opponent boxscore per-game stats
      opp_ppg, opp_rpg, opp_orebpg, opp_drebpg, opp_apg, opp_spg, opp_bpg, opp_topg, opp_fpg,
      opp_fgm_pg, opp_fga_pg, opp_fg3m_pg, opp_fg3a_pg, opp_ftm_pg, opp_fta_pg,
      opp_fg_pct, opp_ft_pct,
      # Opponent advanced stats
      threefg_opponent, threeatt_rate_opponent,
      oer_opponent, der_opponent, S_DefReb_opponent, S_OffReb_opponent,
      S_assist_opponent, ts_opponent, efg_opponent, S_Tov_opponent,
      S_steal_opponent, S_blocks_opponent, FT_rate_opponent, tecnica_opponent
    ) %>%
    filter(!is.na(team.team_actual_name))
  
  # Add team logo URLs
  cat("→ Adding team logos...\n")
  final_stats$url <- sapply(final_stats$team.team_actual_name, function(team) {
    if (team %in% names(TEAM_LOGOS)) {
      TEAM_LOGOS[[team]]
    } else {
      NA_character_
    }
  })
  
  # Save results
  output_file <- file.path(processed_dir, paste0("TeamAdvancedStats", season_id, ".csv"))
  cat("→ Saving to:", output_file, "\n")
  write.csv(final_stats, output_file, row.names = FALSE, fileEncoding = "UTF-8")
  
  # Report
  cat("\n✓ Team statistics complete!\n")
  cat("  Teams:", nrow(final_stats), "\n")
  cat("  Metrics:", ncol(final_stats), "\n")
  
  # Print summary
  cat("\n  Top 5 by Net Rating (ORtg - DRtg):\n")
  top5 <- final_stats %>%
    mutate(net_rtg = oer - der) %>%
    arrange(desc(net_rtg)) %>%
    head(5) %>%
    select(team.team_actual_name, oer, der, net_rtg)
  
  for (i in seq_len(nrow(top5))) {
    cat(sprintf("    %d. %s: +%.1f (O: %.1f, D: %.1f)\n",
                i, top5$team.team_actual_name[i],
                top5$net_rtg[i], top5$oer[i], top5$der[i]))
  }
  
  invisible(final_stats)
}

#' Calculate team statistics for multiple seasons
#' 
#' @param season_ids Vector of season IDs (default: all available)
#' @param ... Additional arguments passed to calculate_team_stats
#' @return Combined data frame with all seasons
#' 
calculate_all_team_stats <- function(season_ids = NULL, ...) {
  source("./config/seasons.R")
  
  if (is.null(season_ids)) {
    season_ids <- get_available_seasons()
  }
  
  all_stats <- lapply(season_ids, function(sid) {
    calculate_team_stats(sid, ...)
  })
  
  combined <- do.call("rbind", all_stats)
  
  # Save combined file
  output_file <- file.path("./data/processed", "TeamAdvancedStats_AllSeasons.csv")
  write.csv(combined, output_file, row.names = FALSE, fileEncoding = "UTF-8")
  cat("\n✓ Combined stats saved to:", output_file, "\n")
  
  invisible(combined)
}

cat("✓ Team Statistics module loaded\n")
cat("  Usage: calculate_team_stats(2025) or calculate_all_team_stats()\n")
