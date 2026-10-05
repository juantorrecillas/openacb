# =============================================================================
# 10 - Team Logo Extractor
# =============================================================================
# Scrapes team logo URLs from the ACB API.
# Produces a single team-logos.json: { "Team Name": "https://..." }
# covering all team name variants across all seasons.
#
# Matches are processed in jornada order; a match is skipped only when both
# teams already have logos. This handles mid-season rebrands (e.g. Baskonia)
# where the new logo only appears in the API after the name change.
#
# Usage: generate_team_logos()
# =============================================================================

library(jsonlite)
library(httr)
library(dplyr)

generate_team_logos <- function(
    season_ids   = NULL,
    data_dir     = "./data/raw",
    config_path  = "./config/seasons.R",
    output_dir   = "../openacb_react/public/data"
) {
  if (!nzchar(trimws(Sys.getenv("ACB_BEARER_TOKEN")))) {
    environ_path <- file.path(dirname(config_path), "..", ".Renviron")
    if (file.exists(environ_path)) readRenviron(environ_path)
  }
  source(config_path, local = TRUE)
  if (!nzchar(trimws(API_CONFIG$bearer_token))) stop("Missing ACB_BEARER_TOKEN")
  if (is.null(season_ids)) season_ids <- get_available_seasons()

  cat("\n--- Extracting team logos ---\n")

  output_file <- file.path(output_dir, "team-logos.json")
  logo_map <- if (file.exists(output_file)) {
    fromJSON(output_file, simplifyVector = FALSE)
  } else {
    list()
  }

  for (season_id in season_ids) {
    season <- get_season_config(season_id)
    cat(sprintf("  Season %d (%s)...\n", season_id, season$season_name))

    # ── Fetch matchweeks ──────────────────────────────────────────────────────
    weeks_url <- gsub("\\{edition_id\\}", season$edition_id, API_CONFIG$weeks_url_template)
    week_resp <- GET(weeks_url, add_headers(Authorization = API_CONFIG$bearer_token))
    if (status_code(week_resp) != 200) {
      cat(sprintf("    WARNING: Could not fetch weeks for season %d\n", season_id))
      next
    }
    weeks_json <- fromJSON(content(week_resp, type = "text", encoding = "UTF-8"), flatten = TRUE)
    weeks_df   <- data.frame(id = weeks_json$id, jornada = weeks_json$descriptor,
                              stringsAsFactors = FALSE)

    # ── Fetch match list ──────────────────────────────────────────────────────
    matches_url <- gsub("\\{edition_id\\}", season$edition_id, API_CONFIG$matches_url_template)
    match_resp  <- GET(matches_url, add_headers(Authorization = API_CONFIG$bearer_token))
    if (status_code(match_resp) != 200) {
      cat(sprintf("    WARNING: Could not fetch matches for season %d\n", season_id))
      next
    }
    match_json <- fromJSON(content(match_resp, type = "text", encoding = "UTF-8"), flatten = TRUE)

    all_matches <- data.frame(
      match_id     = match_json$id,
      weekid       = match_json$id_matchweek,
      finalized    = match_json$finalized,
      team_local   = match_json$local_team.team_actual_name,
      team_visitor = match_json$visitor_team.team_actual_name,
      stringsAsFactors = FALSE
    ) %>%
      merge(weeks_df, by.x = "weekid", by.y = "id") %>%
      filter(finalized == TRUE) %>%
      mutate(jornada_num = as.numeric(gsub("Jornada ", "", jornada))) %>%
      arrange(jornada_num)

    if (nrow(all_matches) == 0) {
      cat(sprintf("    WARNING: No finalized matches for season %d\n", season_id))
      next
    }

    cat(sprintf("    %d finalized matches across %d jornadas\n",
                nrow(all_matches), max(all_matches$jornada_num)))

    # Name mappings for this season
    season_mappings <- get_team_name_mappings(season_id)
    apply_map <- function(name) {
      if (!is.na(name) && name %in% names(season_mappings)) season_mappings[[name]] else name
    }

    # ── Fetch PbP for each match in jornada order, extract team.media ─────────
    for (j in rev(seq_len(nrow(all_matches)))) {
      row <- all_matches[j, ]

      canon_local   <- apply_map(row$team_local)
      canon_visitor <- apply_map(row$team_visitor)

      # Both teams already covered — no need to fetch this match
      if (!is.null(logo_map[[canon_local]]) && !is.null(logo_map[[canon_visitor]])) next

      pbp_url  <- paste0(API_CONFIG$pbp_url, "?idMatch=", row$match_id, "&jvFilter=true")
      pbp_resp <- GET(pbp_url, add_headers(Authorization = API_CONFIG$bearer_token))
      Sys.sleep(0.15)
      if (status_code(pbp_resp) != 200) next

      pbp_raw <- fromJSON(content(pbp_resp, type = "text", encoding = "UTF-8"), flatten = TRUE)
      if (!"team.media" %in% names(pbp_raw)) next

      unique_rows <- pbp_raw %>%
        select(team.team_actual_name, team.media) %>%
        filter(!is.na(team.team_actual_name) & team.team_actual_name != "") %>%
        distinct(team.team_actual_name, .keep_all = TRUE)

      for (i in seq_len(nrow(unique_rows))) {
        raw_name  <- unique_rows$team.team_actual_name[i]
        team_name <- apply_map(raw_name)

        if (!is.null(logo_map[[team_name]])) next

        media <- unique_rows$team.media[[i]]
        if (is.null(media) || nrow(media) == 0) next

        logo_rows <- media[media$id_type == 6, ]
        if (nrow(logo_rows) == 0) {
          logo_rows <- media[grepl("\\.png|\\.jpg|\\.jpeg", media$url, ignore.case = TRUE), ]
        }
        if (nrow(logo_rows) == 0) next

        logo_map[[team_name]] <- logo_rows$url[1]
        cat(sprintf("      J%d: %s -> %s\n", row$jornada_num, team_name, logo_rows$url[1]))
      }
    }
  }
  # ── Export ──────────────────────────────────────────────────────────────────
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  write_json(logo_map, output_file, auto_unbox = TRUE)

  cat(sprintf("\n  Saved %d team logos to %s\n", length(logo_map), output_file))
  invisible(logo_map)
}
