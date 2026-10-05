# =============================================================================
# ACB Basketball Analytics Pipeline - Shot Chart Module
# =============================================================================
# Processes shot data: rescales coordinates, calculates distances/angles,
# and assigns shots to zones for visualization.
# Usage: process_shot_charts(2025) or process_all_shot_charts()
# =============================================================================

# Coordinate Transformation Functions
# =============================================================================
library(dplyr)

# Load the clean module for team name standardization
source("./etl/02_clean.R")

# =============================================================================
# Coordinate Transformation Functions
# ==========================================================================================================================================================
# Coordinate Transformation Functions
# =============================================================================

#' Rescale X coordinate from ACB API to court coordinates
#' @param x Original X coordinate (from API)
#' @param y Original Y coordinate (from API)
#' @return Rescaled X coordinate
rescale_x <- function(x, y) {
  # Court bounds (FIBA court, half-court)
  x1 <- -7.3; x2 <- 7.3
  y1 <- -12.45; y2 <- -0.5
  
  # API coordinate bounds
  x1p <- -7297; x2p <- 7297
  y1p <- 0; y2p <- 12000
  
  razon_x <- (x2 - x1) / (x2p - x1p)
  b_x <- x1 - x1p * razon_x
  
  razon_x * x + b_x
}

#' Rescale Y coordinate from ACB API to court coordinates
#' @param x Original X coordinate (from API)
#' @param y Original Y coordinate (from API)
#' @return Rescaled Y coordinate
rescale_y <- function(x, y) {
  x1 <- -7.3; x2 <- 7.3
  y1 <- -12.45; y2 <- -0.5
  
  x1p <- -7297; x2p <- 7297
  y1p <- 0; y2p <- 12000
  
  razon_y <- (y2 - y1) / (y2p - y1p)
  b_y <- y1 - y1p * razon_y
  
  razon_y * y + b_y
}

#' Calculate distance from basket
#' @param x2 Shot X coordinate (rescaled)
#' @param y2 Shot Y coordinate (rescaled)
#' @param x1 Basket X coordinate (default: 0)
#' @param y1 Basket Y coordinate (default: -12.45)
#' @return Distance in meters
calc_distance <- function(x2, y2, x1 = 0, y1 = -12.45) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

#' Calculate angle from basket (for zone classification)
#' @param x2 Shot X coordinate (rescaled)
#' @param y2 Shot Y coordinate (rescaled)
#' @param x1 Basket X coordinate (default: 0)
#' @param y1 Basket Y coordinate (default: -12.45)
#' @return Angle in degrees
calc_angle <- function(x2, y2, x1 = 0, y1 = -12.45) {
  phi <- ifelse(x2 >= x1,
                atan((y2 - y1) / (x2 - x1)),
                atan((y2 - y1) / (x1 - x2)))
  theta <- -(phi - (pi / 2))
  theta * (180 / pi)
}

# =============================================================================
# Zone Classification
# =============================================================================

#' Classify shot into broad zone categories
#' @param posX_res Rescaled X coordinate
#' @param posY_res Rescaled Y coordinate
#' @param distance Distance from basket
#' @param angle Angle from basket
#' @return Zone name (character)
classify_zone <- function(posX_res, posY_res, distance, angle) {
  # Restricted area (< 1.25m from basket)
  if (distance <= 1.25) return("Zona (Restringida)")
  
  # Paint (non-restricted)
  if (posX_res > -2.4 && posX_res < 2.4 && posY_res < -8.2) {
    return("Zona no restringida")
  }
  
  # Corner areas
  if (posY_res <= -11) {
    if (posX_res >= 6.6 || posX_res <= -6.6) {
      return("Triple Esquina")
    } else if (posX_res >= 2.4 || posX_res <= -2.4) {
      return("Media Distancia Esquina")
    }
  }
  
  # Elbow/wing areas
  if (angle > 29.7 && posY_res > -11) {
    if (distance >= 6.75) {
      return("Triple Codo")
    } else {
      return("Media Distancia Codo")
    }
  }
  
  # Center areas
  if (angle <= 29.7 && angle >= 0 && posY_res > -8.2) {
    if (distance >= 6.75) {
      return("Triple Centro")
    } else {
      return("Media Distancia Centro")
    }
  }
  
  return("")
}

#' Classify shot into detailed zone categories (left/right split)
#' @param posX_res Rescaled X coordinate
#' @param posY_res Rescaled Y coordinate
#' @param distance Distance from basket
#' @param angle Angle from basket
#' @param zone Broad zone classification
#' @return Detailed zone name (character)
classify_zone_detailed <- function(posX_res, posY_res, distance, angle, zone) {
  # Keep restricted and non-restricted zones as-is
  if (zone == "Zona (Restringida)" || zone == "Zona no restringida") {
    return(zone)
  }
  
  # Corner areas with left/right
  if (posY_res <= -11) {
    if (posX_res <= -6.6) return("Triple Esquina Derecha")
    if (posX_res >= 6.6) return("Triple Esquina Izquierda")
    if (posX_res <= -2.4) return("Media Distancia Esquina Derecha")
    if (posX_res >= 2.4) return("Media Distancia Esquina Izquierda")
  }
  
  # Elbow/wing areas with left/right
  if (angle > 29.7 && posY_res > -11) {
    if (distance >= 6.75) {
      return(ifelse(posX_res < 0, "Triple Codo Derecha", "Triple Codo Izquierda"))
    } else if (zone != "Zona (Restringida)" && zone != "Zona no restringida") {
      return(ifelse(posX_res < 0, "Media Distancia Codo Derecha", "Media Distancia Codo Izquierda"))
    }
  }
  
  # Center areas
  if (angle <= 29.7 && angle >= 0 && posY_res > -8.2) {
    if (distance >= 6.75) return("Triple Centro")
    return("Media Distancia Centro")
  }
  
  return(zone)
}

# =============================================================================
# Main Processing Functions
# =============================================================================

#' Process shot chart data for a season
#' 
#' @param season_id Integer year (e.g., 2025 for 2024-2025 season)
#' @param data_dir Base directory for data (default: "./data")
#' @param config_path Path to seasons.R config file
#' @return Data frame with processed shot data
#' 
process_shot_charts <- function(season_id,
                                 data_dir = "./data",
                                 config_path = "./config/seasons.R") {
  
  # Load configuration
  source(config_path)
  season <- get_season_config(season_id)
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("Processing Shot Charts:", season$season_name, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")
  
  # Define paths
  raw_dir <- file.path(data_dir, "raw", season$folder_name)
  processed_dir <- file.path(data_dir, "processed")
  
  if (!dir.exists(raw_dir)) {
    stop("Raw data directory not found: ", raw_dir)
  }
  
  # Load all game files
  cat("→ Loading game files...\n")
  filenames <- list.files(raw_dir, pattern = "*.csv", full.names = TRUE)
  
  df_list <- lapply(filenames, function(f) {
    tryCatch({
      read.csv(f, encoding = "UTF-8", stringsAsFactors = FALSE)
    }, error = function(e) {
      read.csv(f, encoding = "latin1", stringsAsFactors = FALSE)
    })
  })
  
  data <- do.call("rbind", df_list)
  cat("  Loaded", nrow(data), "play-by-play events\n")
  
  # Standardize team names
  cat("→ Standardizing team names...\n")
  data <- standardize_team_names(data, season_id, config_path)
  
  # Filter to shots only
  cat("→ Filtering to shots...\n")
  shot_types <- c(
    "Canasta de 3", "Intento fallado de 3",
    "Canasta de 2", "Intento fallado de 2",
    "Mate", "Mate fuera"
  )
  
  shots <- data %>%
    filter(type.description %in% shot_types)
  
  cat("  Found", nrow(shots), "shots\n")
  
  # Rescale coordinates
  cat("→ Rescaling coordinates...\n")
  shots$posX_res <- rescale_x(shots$posY, shots$posX)
  shots$posY_res <- rescale_y(shots$posY, shots$posX)
  
  # Calculate distance and angle
  cat("→ Calculating distance and angle...\n")
  shots$distance <- calc_distance(shots$posX_res, shots$posY_res)
  shots$angle <- calc_angle(shots$posX_res, shots$posY_res)
  
  # Add made/missed indicators
  cat("→ Adding shot result columns...\n")
  shots$made <- ifelse(
    shots$type.normalized_description %in% c("2-Point Shot Made", "3-Point Shot Made") |
      shots$type.description == "Mate",
    "Made shot", "Missed shot"
  )
  
  shots$made_numeric <- as.integer(shots$made == "Made shot")
  
  # Add points
  shots$points <- case_when(
    shots$type.normalized_description == "2-Point Shot Made" ~ 2L,
    shots$type.normalized_description == "3-Point Shot Made" ~ 3L,
    shots$type.normalized_description == "Dunk" ~ 2L,
    TRUE ~ 0L
  )
  
  # Classify zones
  cat("→ Classifying shot zones...\n")
  shots$zone <- mapply(classify_zone,
                       shots$posX_res, shots$posY_res,
                       shots$distance, shots$angle)
  
  shots$zoned <- mapply(classify_zone_detailed,
                        shots$posX_res, shots$posY_res,
                        shots$distance, shots$angle, shots$zone)
  
  # Add season identifier
  shots$season_id <- season_id
  
  # Save results
  output_file <- file.path(processed_dir, paste0("ShotChartData", season_id, ".csv"))
  cat("→ Saving to:", output_file, "\n")
  write.csv(shots, output_file, row.names = FALSE, fileEncoding = "UTF-8")
  
  # Report
  cat("\n✓ Shot chart processing complete!\n")
  cat("  Total shots:", nrow(shots), "\n")
  cat("  Made:", sum(shots$made_numeric), sprintf("(%.1f%%)", mean(shots$made_numeric) * 100), "\n")
  
  # Zone summary
  cat("\n  Shots by zone:\n")
  zone_summary <- shots %>%
    group_by(zone) %>%
    summarise(
      n = n(),
      fg_pct = mean(made_numeric) * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(n))
  
  for (i in seq_len(min(6, nrow(zone_summary)))) {
    cat(sprintf("    %s: %d shots (%.1f%% FG)\n",
                zone_summary$zone[i], zone_summary$n[i], zone_summary$fg_pct[i]))
  }
  
  invisible(shots)
}

#' Process shot charts for multiple seasons
#' 
#' @param season_ids Vector of season IDs (default: all available)
#' @param ... Additional arguments passed to process_shot_charts
#' @return Combined data frame with all seasons
#' 
process_all_shot_charts <- function(season_ids = NULL, ...) {
  source("./config/seasons.R")
  
  if (is.null(season_ids)) {
    season_ids <- get_available_seasons()
  }
  
  all_shots <- lapply(season_ids, function(sid) {
    process_shot_charts(sid, ...)
  })
  
  combined <- do.call("rbind", all_shots)
  
  # Save combined file
  output_file <- file.path("./data/processed", "ShotChartData_AllSeasons.csv")
  write.csv(combined, output_file, row.names = FALSE, fileEncoding = "UTF-8")
  cat("\n✓ Combined shot data saved to:", output_file, "\n")
  
  invisible(combined)
}

cat("✓ Shot Chart module loaded\n")
cat("  Usage: process_shot_charts(2025) or process_all_shot_charts()\n")
