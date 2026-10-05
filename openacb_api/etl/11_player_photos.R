# Player headshots from the ACB play-by-play API.
# Output: player-photos.json, keyed by license ID and season.

library(jsonlite)
library(httr)

generate_player_photos <- function(
    season_ids = NULL,
    config_path = "./config/seasons.R",
    output_dir = "../openacb_react/public/data"
) {
  if (!nzchar(trimws(Sys.getenv("ACB_BEARER_TOKEN")))) {
    environ_path <- file.path(dirname(config_path), "..", ".Renviron")
    if (file.exists(environ_path)) readRenviron(environ_path)
  }
  source(config_path, local = TRUE)
  if (!nzchar(trimws(API_CONFIG$bearer_token))) stop("Missing ACB_BEARER_TOKEN")
  if (is.null(season_ids)) season_ids <- get_available_seasons()

  output_file <- file.path(output_dir, "player-photos.json")
  photo_map <- if (file.exists(output_file)) {
    fromJSON(output_file, simplifyVector = FALSE)
  } else {
    list()
  }

  for (season_id in season_ids) {
    season <- get_season_config(season_id)
    matches_url <- gsub("\\{edition_id\\}", season$edition_id, API_CONFIG$matches_url_template)
    matches_response <- GET(
      matches_url,
      add_headers(Authorization = API_CONFIG$bearer_token),
      timeout(20)
    )
    if (status_code(matches_response) != 200) {
      stop("Could not fetch matches for season ", season_id)
    }

    matches <- fromJSON(content(matches_response, type = "text", encoding = "UTF-8"), flatten = TRUE)
    match_ids <- unique(matches$id[!is.na(matches$finalized) & matches$finalized])
    added <- 0L

    for (match_id in match_ids) {
      pbp_url <- paste0(API_CONFIG$pbp_url, "?idMatch=", match_id, "&jvFilter=true")
      pbp_response <- GET(
        pbp_url,
        add_headers(Authorization = API_CONFIG$bearer_token),
        timeout(20)
      )
      Sys.sleep(0.1)
      if (status_code(pbp_response) != 200) next

      raw <- fromJSON(content(pbp_response, type = "text", encoding = "UTF-8"), flatten = TRUE)
      pbp <- if (is.data.frame(raw)) raw else {
        tables <- Filter(is.data.frame, raw)
        if (length(tables) == 0) NULL else tables[[1]]
      }
      if (is.null(pbp) || !all(c("license.id", "license.media") %in% names(pbp))) next

      player_ids <- unique(as.character(pbp$license.id[!is.na(pbp$license.id)]))
      for (player_id in player_ids) {
        year <- as.character(season_id)
        if (!is.null(photo_map[[player_id]][[year]])) next

        rows <- which(as.character(pbp$license.id) == player_id)
        for (row in rows) {
          media <- pbp$license.media[[row]]
          if (!is.data.frame(media) || !all(c("id_type", "url") %in% names(media))) next
          headshots <- which(media$id_type == 5 & !is.na(media$url) & nzchar(media$url))
          if (length(headshots) == 0) next

          photo_map[[player_id]][[year]] <- media$url[headshots[1]]
          added <- added + 1L
          break
        }
      }
    }

    cat(sprintf("Season %d: %d new player photos from %d completed matches\n",
                season_id, added, length(match_ids)))
  }

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  write_json(photo_map, output_file, auto_unbox = TRUE, null = "null")
  invisible(photo_map)
}
