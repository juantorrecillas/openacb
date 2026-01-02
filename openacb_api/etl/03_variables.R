# =============================================================================
# ACB Basketball Analytics Pipeline - PBP Variables Module
# =============================================================================
# Creates player on-court tracking variables for lineup analysis.
# This is computationally intensive - tracks when each player is on/off court.
# Usage: create_pbp_variables(2025)
# =============================================================================

library(dplyr)

#' Create player on-court tracking variables
#' 
#' For each player, creates a column {player_nick}_pista that is 1 when the
#' player is on court and 0 when off court. This enables lineup analysis.
#' 
#' @param season_id Integer year (e.g., 2025 for 2024-2025 season)
#' @param data_dir Base directory for data (default: "./data")
#' @param config_path Path to seasons.R config file
#' @param parallel Use parallel processing (default: FALSE)
#' @return Invisibly returns the processed data frame
#' 
create_pbp_variables <- function(season_id,
                                  data_dir = "./data",
                                  config_path = "./config/seasons.R",
                                  parallel = FALSE) {
  
  # Load configuration
  source(config_path)
  season <- get_season_config(season_id)
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("Creating PBP Variables:", season$season_name, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")
  
  # Define paths
  processed_dir <- file.path(data_dir, "processed")
  input_file <- file.path(processed_dir, paste0("FinalData_", season_id, ".csv"))
  
  if (!file.exists(input_file)) {
    stop("Processed data file not found: ", input_file,
         "\nRun clean_pbp(", season_id, ") first.")
  }
  
  # Load data
  cat("→ Loading data from:", input_file, "\n")
  df <- read.csv(input_file, encoding = "UTF-8", stringsAsFactors = FALSE)
  cat("  Rows:", format(nrow(df), big.mark = ","), "\n")
  
  # Get starting lineups (Cinco Inicial)
  cat("→ Identifying starting lineups...\n")
  quinteto <- df[df$type.description == "Cinco Inicial", c("license.id", "license.licenseNick", "id_match")]

  # Get unique players using license.id (to handle players with same short name)
  # Create a unique player list with both license.id and short name
  jugadores_df <- df %>%
    filter(!is.na(license.licenseNick) & !is.na(license.id)) %>%
    select(license.id, license.licenseNick) %>%
    distinct()

  partidos <- unique(df$id_match)
  partidos <- partidos[!is.na(partidos)]

  cat("  Players:", nrow(jugadores_df), "\n")
  cat("  Matches:", length(partidos), "\n")

  # Initialize player columns using license.id for uniqueness
  # Column names: {short_name}_{license_id}_pista
  cat("→ Initializing player tracking columns...\n")
  for (i in seq_len(nrow(jugadores_df))) {
    player_id <- jugadores_df$license.id[i]
    player_nick <- jugadores_df$license.licenseNick[i]
    col_name <- paste0(player_nick, "_", player_id, "_pista")
    df[[col_name]] <- 0L
  }
  
  # Process each match
  cat("→ Processing matches (this may take a while)...\n")
  pb <- txtProgressBar(min = 0, max = length(partidos), style = 3)
  
  df_pbp <- NULL
  
  for (m_idx in seq_along(partidos)) {
    m <- partidos[m_idx]
    
    # Get data for this match
    container <- df[df$id_match == m, ]

    # Set starting lineup (using license.id for unique matching)
    starters <- quinteto[quinteto$id_match == m, c("license.id", "license.licenseNick")]
    for (s_idx in seq_len(nrow(starters))) {
      if (nrow(starters) == 0) break
      player_id <- starters$license.id[s_idx]
      player_nick <- starters$license.licenseNick[s_idx]
      if (is.na(player_id) || is.na(player_nick)) next

      col_name <- paste0(player_nick, "_", player_id, "_pista")
      if (col_name %in% names(container)) {
        container[[col_name]] <- 1L
      }
    }

    # Track substitutions for each player (using license.id)
    for (j_idx in seq_len(nrow(jugadores_df))) {
      player_id <- jugadores_df$license.id[j_idx]
      player_nick <- jugadores_df$license.licenseNick[j_idx]
      col_name <- paste0(player_nick, "_", player_id, "_pista")

      if (!col_name %in% names(container)) next

      for (i in seq_len(nrow(container))) {
        if (is.na(container$license.id[i])) next

        # Match by license.id instead of short name
        if (container$license.id[i] == player_id) {
          if (container$type.description[i] == "Sale Pista") {
            # Player leaves court
            container[[col_name]][i:nrow(container)] <- 0L
          } else if (container$type.description[i] == "Entra Pista") {
            # Player enters court
            container[[col_name]][i:nrow(container)] <- 1L
          }
        }
      }
    }
    
    # Append to result
    if (is.null(df_pbp)) {
      df_pbp <- container
    } else {
      df_pbp <- rbind(df_pbp, container)
    }
    
    setTxtProgressBar(pb, m_idx)
  }
  
  close(pb)
  
  # Remove unnecessary columns to reduce file size
  cat("\n→ Removing unnecessary columns...\n")
  cols_to_remove <- intersect(PBP_COLUMNS_TO_DELETE, names(df_pbp))
  if (length(cols_to_remove) > 0) {
    df_pbp <- df_pbp[, !names(df_pbp) %in% cols_to_remove]
    cat("  Removed", length(cols_to_remove), "columns\n")
  }
  
  # Save results
  cat("→ Saving results...\n")
  
  # CSV output (for compatibility)
  csv_output <- file.path(processed_dir, paste0("PbP_adjustedData", season_id, ".csv"))
  write.csv(df_pbp, csv_output, row.names = FALSE, fileEncoding = "UTF-8")
  cat("  CSV saved:", csv_output, "\n")
  
  # RDS output (more efficient for R)
  rds_output <- file.path(processed_dir, paste0("PbP_adjustedData", season_id, ".Rds"))
  saveRDS(df_pbp, rds_output)
  cat("  RDS saved:", rds_output, "\n")
  
  # Report
  cat("\n✓ Variable creation complete!\n")
  cat("  Final rows:", format(nrow(df_pbp), big.mark = ","), "\n")
  cat("  Final columns:", ncol(df_pbp), "\n")
  cat("  File size (RDS):", format(file.size(rds_output) / 1e6, digits = 2), "MB\n")
  
  invisible(df_pbp)
}

#' Create PBP variables for multiple seasons
#' 
#' @param season_ids Vector of season IDs (default: all available)
#' @param ... Additional arguments passed to create_pbp_variables
#' 
create_all_pbp_variables <- function(season_ids = NULL, ...) {
  source("./config/seasons.R")
  
  if (is.null(season_ids)) {
    season_ids <- get_available_seasons()
  }
  
  for (sid in season_ids) {
    create_pbp_variables(sid, ...)
  }
}

cat("✓ PBP Variables module loaded\n")
cat("  Usage: create_pbp_variables(2025) or create_all_pbp_variables()\n")
