# =============================================================================
# ACB Basketball Analytics Pipeline - API Scraper Module
# =============================================================================
# Scrapes play-by-play data from the ACB API for a given season.
# Usage: scrape_season(2025) or scrape_season(2025, data_dir = "path/to/data")
# =============================================================================

library(jsonlite)
library(httr)
library(dplyr)

#' Scrape all play-by-play data for a given season
#' 
#' @param season_id Integer year (e.g., 2025 for 2024-2025 season)
#' @param data_dir Base directory for storing data (default: "./data/raw")
#' @param config_path Path to seasons.R config file
#' @return Invisibly returns the match data frame
#' 
scrape_season <- function(season_id, 
                          data_dir = "./data/raw",
                          config_path = "./config/seasons.R") {
  
  # Load configuration
  source(config_path)
  season <- get_season_config(season_id)
  
  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("Scraping ACB Season:", season$season_name, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")
  
  # Create output directory
  output_dir <- file.path(data_dir, season$folder_name)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("✓ Created directory:", output_dir, "\n")
  } else {
    cat("ℹ Directory exists:", output_dir, "\n")
  }
  
  # Build API URLs
  weeks_url <- gsub("\\{edition_id\\}", season$edition_id, API_CONFIG$weeks_url_template)
  matches_base_url <- gsub("\\{edition_id\\}", season$edition_id, API_CONFIG$matches_url_template)
  
  # Fetch matchweeks
  cat("\n→ Fetching matchweeks...\n")
  week_response <- GET(weeks_url, add_headers(Authorization = API_CONFIG$bearer_token))
  
  if (status_code(week_response) != 200) {
    stop("Failed to fetch matchweeks. Status: ", status_code(week_response))
  }
  
  weeks_json <- fromJSON(content(week_response, type = "text", encoding = "UTF-8"), flatten = TRUE)
  weeks <- data.frame(id = weeks_json$id, jornada = weeks_json$descriptor)
  cat("  Found", nrow(weeks), "matchweeks\n")
  
  # Fetch all matches
  cat("→ Fetching match list...\n")
  match_response <- GET(matches_base_url, add_headers(Authorization = API_CONFIG$bearer_token))
  
  if (status_code(match_response) != 200) {
    stop("Failed to fetch matches. Status: ", status_code(match_response))
  }
  
  match_json <- fromJSON(content(match_response, type = "text", encoding = "UTF-8"), flatten = TRUE)
  
  all_matches <- data.frame(
    match_id = match_json$id,
    team_local = match_json$local_team.team_actual_name,
    team_visitor = match_json$visitor_team.team_actual_name,
    score_local = match_json$score_local,
    score_visitor = match_json$score_visitor,
    finalized = match_json$finalized,
    weekid = match_json$id_matchweek
  ) %>%
    merge(weeks, by.x = "weekid", by.y = "id") %>%
    filter(finalized == TRUE) %>%
    mutate(jornada = as.numeric(gsub("Jornada ", "", jornada)))
  
  cat("  Found", nrow(all_matches), "completed matches\n")
  
  # Get unique match IDs
 match_ids <- unique(all_matches$match_id)
  
  # Scrape each match
  cat("\n→ Scraping play-by-play data...\n")
  pb <- txtProgressBar(min = 0, max = length(match_ids), style = 3)
  
  errors <- c()
  
  for (i in seq_along(match_ids)) {
    match_id <- match_ids[i]
    
    tryCatch({
      # Build PBP URL
      pbp_url <- paste0(API_CONFIG$pbp_url, "?idMatch=", match_id, "&jvFilter=true")
      
      # Fetch PBP data
      pbp_response <- GET(pbp_url, add_headers(Authorization = API_CONFIG$bearer_token))
      
      if (status_code(pbp_response) != 200) {
        errors <- c(errors, paste("Match", match_id, "- Status:", status_code(pbp_response)))
        next
      }
      
      pbp_data <- fromJSON(content(pbp_response, type = "text", encoding = "UTF-8"), flatten = TRUE)
      
      # Remove media columns if they exist
      cols_to_remove <- intersect(c("license.media", "team.media"), names(pbp_data))
      if (length(cols_to_remove) > 0) {
        pbp_data <- pbp_data[, !names(pbp_data) %in% cols_to_remove]
      }
      
      # Get match info
      match_info <- all_matches[all_matches$match_id == match_id, ]
      jornada <- match_info$jornada[1]
      
      # Determine local/visitor teams
      team_summary <- pbp_data %>%
        group_by(team.team_actual_name, local) %>%
        summarise(n = n(), .groups = "drop") %>%
        filter(!is.na(team.team_actual_name))
      
      team_local <- as.character(team_summary$team.team_actual_name[team_summary$local == TRUE][1])
      team_visitor <- as.character(team_summary$team.team_actual_name[team_summary$local == FALSE][1])
      
      team_abbrev <- pbp_data %>%
        group_by(team.team_abbrev_name, local) %>%
        summarise(n = n(), .groups = "drop") %>%
        filter(!is.na(team.team_abbrev_name), n > 1)
      
      team_local_abb <- as.character(team_abbrev$team.team_abbrev_name[team_abbrev$local == TRUE][1])
      team_visitor_abb <- as.character(team_abbrev$team.team_abbrev_name[team_abbrev$local == FALSE][1])
      
      # Add metadata columns
      pbp_data$jornada <- jornada
      pbp_data$team <- ifelse(pbp_data$local == TRUE, team_local, team_visitor)
      pbp_data$opponent <- ifelse(pbp_data$local == TRUE, team_visitor, team_local)
      
      # Merge final scores
      pbp_data <- pbp_data %>%
        merge(
          all_matches %>% 
            select(match_id, score_local, score_visitor) %>%
            rename(score_local_final = score_local, score_visitor_final = score_visitor),
          by.x = "id_match", 
          by.y = "match_id"
        )
      
      # Save to CSV
      filename <- paste0("J", jornada, "_", team_local_abb, "_", team_visitor_abb, "_PBP.csv")
      filepath <- file.path(output_dir, filename)
      write.csv(pbp_data, filepath, row.names = FALSE, fileEncoding = "UTF-8")
      
    }, error = function(e) {
      errors <- c(errors, paste("Match", match_id, "-", e$message))
    })
    
    setTxtProgressBar(pb, i)
    Sys.sleep(0.1)  # Rate limiting
  }
  
  close(pb)
  
  # Report results
  cat("\n\n✓ Scraping complete!\n")
  cat("  Matches processed:", length(match_ids), "\n")
  cat("  Output directory:", output_dir, "\n")
  
  if (length(errors) > 0) {
    cat("\n⚠ Errors encountered:\n")
    for (err in errors) {
      cat("  -", err, "\n")
    }
  }
  
  invisible(all_matches)
}

#' Scrape multiple seasons
#' 
#' @param season_ids Vector of season IDs to scrape
#' @param ... Additional arguments passed to scrape_season
#' 
scrape_seasons <- function(season_ids, ...) {
  for (sid in season_ids) {
    scrape_season(sid, ...)
  }
}

cat("✓ Scraper module loaded\n")
cat("  Usage: scrape_season(2025) or scrape_seasons(c(2021, 2022, 2023, 2024, 2025))\n")
