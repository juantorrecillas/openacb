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
  saved  <- 0L

  for (i in seq_along(match_ids)) {
    match_id <- match_ids[i]

    tryCatch({
      # Build PBP URL
      pbp_url <- paste0(API_CONFIG$pbp_url, "?idMatch=", match_id, "&jvFilter=true")

      # Fetch PBP data
      pbp_response <- GET(pbp_url, add_headers(Authorization = API_CONFIG$bearer_token))

      if (status_code(pbp_response) != 200) {
        errors <<- c(errors, paste("Match", match_id, "- Status:", status_code(pbp_response)))
        next
      }

      pbp_raw <- fromJSON(content(pbp_response, type = "text", encoding = "UTF-8"), flatten = TRUE)

      # Older API seasons return a named list wrapping the events; newer seasons return a data.frame directly
      if (is.data.frame(pbp_raw)) {
        pbp_data <- pbp_raw
      } else if (is.list(pbp_raw)) {
        df_elements <- Filter(is.data.frame, pbp_raw)
        if (length(df_elements) == 0) stop("API response is a list but contains no data.frame")
        pbp_data <- df_elements[[1]]
      } else {
        stop("Unexpected API response type: ", class(pbp_raw))
      }

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
        filter(!is.na(team.team_actual_name), !is.na(local)) %>%
        group_by(team.team_actual_name, local) %>%
        summarise(n = n(), .groups = "drop") %>%
        slice_max(n, n = 1, by = local, with_ties = FALSE)

      team_local <- as.character(team_summary$team.team_actual_name[team_summary$local == TRUE][1])
      team_visitor <- as.character(team_summary$team.team_actual_name[team_summary$local == FALSE][1])

      # Use most-frequent abbreviation per side; drop the n > 1 guard that caused NA filenames
      team_abbrev <- pbp_data %>%
        filter(!is.na(team.team_abbrev_name), !is.na(local)) %>%
        count(team.team_abbrev_name, local) %>%
        slice_max(n, n = 1, by = local, with_ties = FALSE)

      team_local_abb <- as.character(team_abbrev$team.team_abbrev_name[team_abbrev$local == TRUE][1])
      team_visitor_abb <- as.character(team_abbrev$team.team_abbrev_name[team_abbrev$local == FALSE][1])

      # Add metadata columns
      pbp_data$jornada <- jornada
      pbp_data$team <- ifelse(pbp_data$local == TRUE, team_local, team_visitor)
      pbp_data$opponent <- ifelse(pbp_data$local == TRUE, team_visitor, team_local)

      # Merge final scores — defensive: older API responses may lack 'id_match'
      if ("id_match" %in% names(pbp_data)) {
        score_lut <- all_matches %>%
          select(match_id, score_local, score_visitor) %>%
          rename(score_local_final = score_local, score_visitor_final = score_visitor)
        pbp_data <- left_join(pbp_data, score_lut, by = c("id_match" = "match_id"))
      } else {
        pbp_data$score_local_final   <- NA_real_
        pbp_data$score_visitor_final <- NA_real_
      }

      # Save to CSV
      filename <- paste0("J", jornada, "_", team_local_abb, "_", team_visitor_abb, "_PBP.csv")
      filepath <- file.path(output_dir, filename)
      write.csv(pbp_data, filepath, row.names = FALSE, fileEncoding = "UTF-8")
      saved <<- saved + 1L

    }, error = function(e) {
      errors <<- c(errors, paste("Match", match_id, "-", e$message))
    })

    setTxtProgressBar(pb, i)
    Sys.sleep(0.1)  # Rate limiting
  }

  close(pb)

  # Report results
  cat("\n\n✓ Scraping complete!\n")
  cat("  Matches fetched:", length(match_ids), "\n")
  cat("  Files saved:    ", saved, "\n")
  cat("  Output directory:", output_dir, "\n")

  if (length(errors) > 0) {
    cat("\n⚠ Errors encountered (", length(errors), "):\n", sep = "")
    for (err in head(errors, 20)) {
      cat("  -", err, "\n")
    }
    if (length(errors) > 20) cat("  ... and", length(errors) - 20, "more\n")
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
