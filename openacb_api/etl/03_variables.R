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
  quinteto <- df[df$type.description == "Cinco Inicial", c("license.licenseNick", "id_match")]
  
  # Get unique players and matches
  jugadores <- unique(df$license.licenseNick)
  jugadores <- jugadores[!is.na(jugadores)]
  
  partidos <- unique(df$id_match)
  partidos <- partidos[!is.na(partidos)]
  
  cat("  Players:", length(jugadores), "\n")
  cat("  Matches:", length(partidos), "\n")
  
  # Initialize player columns
  cat("→ Initializing player tracking columns...\n")
  for (p in jugadores) {
    df[[paste0(p, "_pista")]] <- 0L
  }
  
  # Process each match
  cat("→ Processing matches (this may take a while)...\n")
  pb <- txtProgressBar(min = 0, max = length(partidos), style = 3)
  
  df_pbp <- NULL
  
  for (m_idx in seq_along(partidos)) {
    m <- partidos[m_idx]
    
    # Get data for this match
    container <- df[df$id_match == m, ]
    
    # Set starting lineup
    starters <- quinteto[quinteto$id_match == m, "license.licenseNick"]
    for (p in starters) {
      if (!is.na(p) && paste0(p, "_pista") %in% names(container)) {
        container[[paste0(p, "_pista")]] <- 1L
      }
    }
    
    # Track substitutions for each player
    for (p in jugadores) {
      col_name <- paste0(p, "_pista")
      if (!col_name %in% names(container)) next
      
      for (i in seq_len(nrow(container))) {
        if (is.na(container$license.licenseNick[i])) next
        
        if (container$license.licenseNick[i] == p) {
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
