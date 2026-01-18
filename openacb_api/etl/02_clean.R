# =============================================================================
# ACB Basketball Analytics Pipeline - PBP Cleaner Module
# =============================================================================
# Cleans and standardizes play-by-play data, creating boxscore event columns.
# Usage: clean_pbp(2025) or clean_all_pbp()
# =============================================================================

library(dplyr)

#' Create boxscore event columns from play-by-play descriptions
#' 
#' @param df Data frame with type.description column
#' @return Data frame with added event columns
#' 
add_boxscore_columns <- function(df) {

  df <- df %>%
    mutate(
      # Rebounds
      reb_def = as.integer(type.description == "Rebote Defensivo"),
      reb_of = as.integer(type.description == "Rebote Ofensivo"),

      # Assists
      asis_t2 = as.integer(type.description == "Asistencia tiro de 2"),
      asis_t3 = as.integer(type.description == "Asistencia tiro de 3"),
      asis_falta = as.integer(type.description == "Asistencia Falta Recibida"),

      # Other events
      recuperacion = as.integer(type.description == "Recuperación"),
      tapon = as.integer(type.description == "Tapón"),
      tapon_rec = as.integer(type.description == "Tapón Recibido"),
      perdida = as.integer(type.description == "Pérdida"),
      falta_rec = as.integer(type.description == "Falta recibida"),

      # Fouls (multiple types)
      falta = as.integer(type.description %in% c(
        "Personal 2TL", "Personal 1TL", "Falta en Ataque",
        "Personal no TL", "Personal TL Comp", "Personal 3TL",
        "Antidep 1TL", "Antidep 2TL", "Técnica 1TL", "Técnica 2TL"
      )),

      # Shots
      T1A = as.integer(type.description == "Canasta de 1"),
      T1F = as.integer(type.description == "Intento fallado de 1"),
      T2A = as.integer(type.description == "Canasta de 2"),
      T2F = as.integer(type.description == "Intento fallado de 2"),
      T3A = as.integer(type.description == "Canasta de 3"),
      T3F = as.integer(type.description == "Intento fallado de 3"),

      # Fast breaks and dunks
      contr2A = as.integer(type.description == "Contraataque 2pt"),
      contr3A = as.integer(type.description == "Contraataque 3pt"),
      mateA = as.integer(type.description == "Mate"),
      mateF = as.integer(type.description == "Mate fuera"),

      # Technical fouls
      tecnica = as.integer(type.description %in% c(
        "Técnica 1TL", "Técnica 2TL", "Tec. Banq 2TL",
        "Téc. Comp", "Tec. Coach 1TL", "Tec. Banq 1TL"
      )),

      # Miscellaneous
      altercado = as.integer(type.description == "Altercado no TL"),

      # Instant replay reviews
      revision = as.integer(type.description %in% c(
        "IR - Revisión por enfrentamiento",
        "IR - Revisión de una violación",
        "IR - Revisión reloj de posesión",
        "IR - Comprobación del tipo de tiro convertido",
        "IR - Revisión de la validez de una canasta",
        "IR - Revisión acción jugador",
        "IR - Challenge entrenador visitante",
        "IR - Revisión último jugador en tocar balón",
        "IR - Revisión del reloj de partido",
        "IR - Revisión del tipo de falta",
        "IR - Challenge entrenador local"
      )),

      # Jump balls
      salto_ganado = as.integer(type.description == "Salto ganado"),
      salto_perdido = as.integer(type.description == "Salto perdido")
    )

  # Aggregate shot columns (dunks count as 2-point attempts)
  df <- df %>%
    mutate(
      T2A = T2A + mateA,
      T2F = T2F + mateF,
      mateI = mateA + mateF,
      T1I = T1A + T1F,
      T2I = T2A + T2F,
      T3I = T3A + T3F,
      puntos = T1A + 2 * T2A + 3 * T3A,
      asistencias = asis_t2 + asis_t3
    )
  
  # Calculate FT_trip based on opponent fouls that makes free throws without
  #a previous made shot and end the possession
  df <- df %>%
    group_by(id_match) %>%
    arrange(order, .by_group = TRUE) %>%
    mutate(
      prev_is_qualifying_foul = lag(type.description %in% c("Personal 2TL", "Personal 3TL"), n = 1, default = FALSE),
      FT_trip = as.integer(
        type.description == "Falta recibida" &
          prev_is_qualifying_foul
      )
    ) %>%
    ungroup() %>%
    select(-prev_is_qualifying_foul)

  # MISSED FG FOLLOWED BY OFFENSIVE REBOUND
  df <- df %>%
    group_by(id_match) %>%
    arrange(order, .by_group = TRUE) %>%
    mutate(
      next_is_oreb = lead(reb_of, n = 1, default = 0) == 1,
      missed_fg_off_reb = as.integer(
        (T2F == 1 | T3F == 1) & next_is_oreb
      )
    ) %>%
    ungroup() %>%
    select(-next_is_oreb)  
  
  # =========================================================================
  # CALCULATE ASSISTED FIELD GOALS 
  #==========================================================================
  
  df <- df %>%
    group_by(id_match) %>%
    arrange(order, .by_group = TRUE) %>%
    mutate(
      # Check if next event was an assist
      prev_was_assist = lead(asistencias, default = 0) > 0,
      # Mark assisted made FGs (2pt or 3pt makes that follow an assist)
      assisted_fgm2 = as.integer(T2A > 0 & prev_was_assist),
      assisted_fgm3 = as.integer(T3A > 0 & prev_was_assist),
      assisted_fgm = assisted_fgm2 + assisted_fgm3,
      # Unassisted makes
      unassisted_fgm = (T2A + T3A) - assisted_fgm
    ) %>%
    ungroup()
  
  #=======================================================================
  # CALCULATE POINTS OFF TURNOVER
  # ======================================================================
  df <- df %>%
    group_by(id_match) %>%
    arrange(order, .by_group = TRUE) %>%
    mutate(
      # Get previous event's team
      prev_team = lag(team.team_actual_name),
      
      # our steal
      prev_was_steal = lag(recuperacion, default = 0) > 0,
      
      # Our offensive rebound (previous event was our own OREB)
      prev_was_oreb = lag(reb_of, default = 0) > 0 & prev_team == team.team_actual_name,
      
      # Transition FGM (after steal)
      transition_fgm2 = as.integer(T2A > 0 & prev_was_steal),
      transition_fgm3 = as.integer(T3A > 0 & prev_was_steal),
      transition_fgm = transition_fgm2 + transition_fgm3,
      
      # 2nd chance FGM (after offensive rebound)
      second_chance_fgm2 = as.integer(T2A > 0 & prev_was_oreb),
      second_chance_fgm3 = as.integer(T3A > 0 & prev_was_oreb),
      second_chance_fgm = second_chance_fgm2 + second_chance_fgm3
    ) %>%
    ungroup()
  
  df <- df %>% arrange(order, id_match)
  return(df)
}

#' Standardize team names within a season
#' 
#' @param df Data frame with team and opponent columns
#' @param season_id Season ID for season-specific mappings
#' @param config_path Path to seasons.R config file
#' @return Data frame with standardized team names
#' 
standardize_team_names <- function(df, season_id = NULL, config_path = "./config/seasons.R") {
  source(config_path)
  
  # Get the appropriate mappings for this season
  if (!is.null(season_id)) {
    mappings <- get_team_name_mappings(season_id)
  } else {
    # If no season_id provided, use global mappings only (for backward compatibility)
    mappings <- TEAM_NAME_MAPPINGS_GLOBAL
    cat("Warning: No season_id provided, using global team name mappings only\n")
  }
  
  # Function to standardize a single team name
  standardize_single_name <- function(name) {
    if (is.na(name) || name == "") {
      return(name)
    }
    
    # Check for exact matches
    if (name %in% names(mappings)) {
      return(mappings[[name]])
    }
    
    # Default: return original name if no match found
    # This preserves season-specific names that shouldn't be changed
    return(name)
  }
  
  # Apply standardization to all relevant columns
  if ("team" %in% names(df)) {
    df$team <- sapply(df$team, standardize_single_name)
  }
  
  if ("opponent" %in% names(df)) {
    df$opponent <- sapply(df$opponent, standardize_single_name)
  }
  
  if ("team.team_actual_name" %in% names(df)) {
    df$team.team_actual_name <- sapply(df$team.team_actual_name, standardize_single_name)
  }
  
  # Also standardize any other team-related columns that might exist
  team_cols <- grep("team\\.|opponent", names(df), value = TRUE)
  for (col in team_cols) {
    if (col %in% c("team", "opponent", "team.team_actual_name")) next
    if (is.character(df[[col]])) {
      df[[col]] <- sapply(df[[col]], standardize_single_name)
    }
  }
  
  return(df)
}

#' Clean play-by-play data for a given season
#' 
#' @param season_id Integer year (e.g., 2025 for 2024-2025 season)
#' @param data_dir Base directory for data (default: "./data")
#' @param config_path Path to seasons.R config file
#' @return Invisibly returns the cleaned data frame
#' 
clean_pbp <- function(season_id,
                      data_dir = "./data",
                      config_path = "./config/seasons.R") {
  
  # Load configuration
  source(config_path)
  season <- get_season_config(season_id)
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("Cleaning PBP Data:", season$season_name, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")
  
  # Define paths
  raw_dir <- file.path(data_dir, "raw", season$folder_name)
  processed_dir <- file.path(data_dir, "processed")
  
  if (!dir.exists(raw_dir)) {
    stop("Raw data directory not found: ", raw_dir, 
         "\nRun scrape_season(", season_id, ") first.")
  }
  
  if (!dir.exists(processed_dir)) {
    dir.create(processed_dir, recursive = TRUE)
  }
  
  # Read all CSV files
  cat("→ Reading CSV files from:", raw_dir, "\n")
  filenames <- list.files(raw_dir, pattern = "*.csv", full.names = TRUE)
  
  if (length(filenames) == 0) {
    stop("No CSV files found in: ", raw_dir)
  }
  
  cat("  Found", length(filenames), "files\n")
  
  # Load and combine all files
  cat("→ Loading and combining data...\n")
  
  df_list <- lapply(filenames, function(f) {
    tryCatch({
      read.csv(f, encoding = "UTF-8", stringsAsFactors = FALSE)
    }, error = function(e) {
      # Try latin1 encoding as fallback
      read.csv(f, encoding = "latin1", stringsAsFactors = FALSE)
    })
  })
  
  df <- do.call("rbind", df_list)
  cat("  Total rows:", format(nrow(df), big.mark = ","), "\n")
  
  # Add boxscore columns
  cat("→ Creating boxscore event columns...\n")
  df <- add_boxscore_columns(df)
  
  # Standardize team names
  cat("→ Standardizing team names...\n")
  df <- standardize_team_names(df, season_id, config_path)
  
  # Add season identifier
  df$season_id <- season_id
  
  # Save processed data
  output_file <- file.path(processed_dir, paste0("FinalData_", season_id, ".csv"))
  cat("→ Saving to:", output_file, "\n")
  write.csv(df, output_file, row.names = FALSE, fileEncoding = "UTF-8")
  
  # Report
  cat("\n✓ Cleaning complete!\n")
  cat("  Rows:", format(nrow(df), big.mark = ","), "\n")
  cat("  Columns:", ncol(df), "\n")
  cat("  Teams:", length(unique(df$team)), "\n")
  cat("  Matches:", length(unique(df$id_match)), "\n")
  
  invisible(df)
}

#' Clean play-by-play data for multiple seasons
#' 
#' @param season_ids Vector of season IDs (default: all available)
#' @param ... Additional arguments passed to clean_pbp
#' 
clean_all_pbp <- function(season_ids = NULL, ...) {
  source("./config/seasons.R")
  
  if (is.null(season_ids)) {
    season_ids <- get_available_seasons()
  }
  
  for (sid in season_ids) {
    clean_pbp(sid, ...)
  }
}

cat("✓ PBP Cleaner module loaded\n")
cat("  Usage: clean_pbp(2025) or clean_all_pbp()\n")
