# incremental scraping regressions with local fixtures and mocked API responses
# usage from the repository root: Rscript openacb_api/tests/test_incremental_scraper.R

if (.Platform$OS.type == "windows") {
  suppressWarnings(Sys.setlocale("LC_CTYPE", ".UTF-8"))
}
api_dir <- normalizePath(if (dir.exists("openacb_api")) "openacb_api" else ".")
config_path <- file.path(api_dir, "config", "seasons.R")
scraper <- new.env(parent = globalenv())
base::source(file.path(api_dir, "etl", "01_scrape.R"), local = scraper)
Sys.setenv(ACB_BEARER_TOKEN = "mock-token")

fixture_dir <- tempfile("openacb-scraper-test-")
fixture_season_dir <- file.path(fixture_dir, "2026-2027")
dir.create(fixture_season_dir, recursive = TRUE)
write.csv(data.frame(
  id_match = 100,
  competition_stage = "regular",
  competition_round = NA_character_
), file.path(fixture_season_dir, "J1_HOME_AWAY_PBP.csv"), row.names = FALSE)

mock_weeks <- data.frame(id = c(1, 2), descriptor = c("Jornada 1", "Jornada 2"))
mock_matches <- data.frame(
  id = c(100, 101),
  local_team.team_actual_name = c("Home", "Home"),
  visitor_team.team_actual_name = c("Away", "Away"),
  score_local = c(80, 90),
  score_visitor = c(70, 80),
  finalized = TRUE,
  id_matchweek = c(1, 2)
)
mock_pbp_json <- "[]"
scraper$requests <- character()
scraper$GET <- function(url, ...) {
  scraper$requests <- c(scraper$requests, url)
  list(url = url)
}
scraper$status_code <- function(response) 200L
scraper$content <- function(response, ...) {
  if (grepl("Matchweeks/lite", response$url, fixed = TRUE)) {
    jsonlite::toJSON(mock_weeks, dataframe = "rows", auto_unbox = TRUE)
  } else if (grepl("Matches/matchesbymatchweeklite", response$url, fixed = TRUE)) {
    jsonlite::toJSON(mock_matches, dataframe = "rows", auto_unbox = TRUE)
  } else {
    mock_pbp_json
  }
}

# an update with no new games must skip all pbp requests and preserve raw files
mock_matches_all <- mock_matches
mock_matches <- mock_matches_all[1, ]
raw_before <- tools::md5sum(list.files(fixture_season_dir, full.names = TRUE))
unchanged_matches <- scraper$scrape_season(
  2027, data_dir = fixture_dir, config_path = config_path, only_missing = TRUE
)
stopifnot(nrow(unchanged_matches) == 1)
stopifnot(length(scraper$requests) == 2)
stopifnot(identical(raw_before, tools::md5sum(names(raw_before))))

# a finalized current-season game without pbp must block processing and export
mock_matches <- mock_matches_all
unavailable_error <- tryCatch({
  scraper$scrape_season(2027, data_dir = fixture_dir,
                         config_path = config_path, only_missing = TRUE)
  NULL
}, error = identity)
stopifnot(inherits(unavailable_error, "error"))
stopifnot(grepl("without play-by-play", conditionMessage(unavailable_error)))
stopifnot(identical(raw_before, tools::md5sum(names(raw_before))))

# failed downloads must surface the match and http status
scraper$status_code <- function(response) {
  if (grepl("PlayByPlay", response$url, fixed = TRUE)) 503L else 200L
}
download_error <- tryCatch({
  scraper$scrape_season(2027, data_dir = fixture_dir,
                         config_path = config_path, only_missing = TRUE)
  NULL
}, error = identity)
stopifnot(inherits(download_error, "error"))
stopifnot(grepl("101.*503", conditionMessage(download_error)))
stopifnot(identical(raw_before, tools::md5sum(names(raw_before))))

# a newly finalized game is fetched once with its scores and stage metadata
scraper$status_code <- function(response) 200L
scraper$requests <- character()
mock_pbp <- data.frame(
  id_match = c(101, 101),
  team.team_actual_name = c("Home", "Away"),
  team.team_abbrev_name = c("HOME", "AWAY"),
  local = c(TRUE, FALSE),
  license.media = c("unused-player-media", "unused-player-media"),
  team.media = c("unused-team-media", "unused-team-media")
)
mock_pbp_json <- jsonlite::toJSON(mock_pbp, dataframe = "rows", auto_unbox = TRUE)
updated_matches <- scraper$scrape_season(
  2027, data_dir = fixture_dir, config_path = config_path, only_missing = TRUE
)
pbp_requests <- scraper$requests[grepl("PlayByPlay", scraper$requests, fixed = TRUE)]
stopifnot(nrow(updated_matches) == 2)
stopifnot(length(pbp_requests) == 1)
stopifnot(grepl("idMatch=101&", pbp_requests, fixed = TRUE))
stopifnot(length(list.files(fixture_season_dir, pattern = "_PBP[.]csv$")) == 2)
stopifnot(identical(raw_before, tools::md5sum(names(raw_before))))
new_raw <- read.csv(file.path(fixture_season_dir, "J2_HOME_AWAY_PBP.csv"))
stopifnot(all(new_raw$id_match == 101))
stopifnot(all(new_raw$jornada == 2))
stopifnot(all(new_raw$competition_stage == "regular"))
stopifnot(all(is.na(new_raw$competition_round)))
stopifnot(identical(new_raw$team, c("Home", "Away")))
stopifnot(identical(new_raw$opponent, c("Away", "Home")))
stopifnot(all(new_raw$score_local_final == 90))
stopifnot(all(new_raw$score_visitor_final == 80))
stopifnot(!any(c("license.media", "team.media") %in% names(new_raw)))

# rerunning the successful update does not fetch or overwrite either game
raw_updated <- tools::md5sum(list.files(fixture_season_dir, full.names = TRUE))
scraper$requests <- character()
scraper$scrape_season(2027, data_dir = fixture_dir,
                      config_path = config_path, only_missing = TRUE)
stopifnot(length(scraper$requests) == 2)
stopifnot(identical(raw_updated, tools::md5sum(names(raw_updated))))

cat("incremental scraper regressions passed\n")
