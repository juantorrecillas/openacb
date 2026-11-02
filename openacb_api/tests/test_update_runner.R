# runner regressions with mocked scraping, processing, and exports
# usage from the repository root: Rscript openacb_api/tests/test_update_runner.R

if (.Platform$OS.type == "windows") {
  suppressWarnings(Sys.setlocale("LC_CTYPE", ".UTF-8"))
}
api_dir <- normalizePath(if (dir.exists("openacb_api")) "openacb_api" else ".")
update_path <- file.path(api_dir, "runners", "update.R")
pipeline_path <- file.path(api_dir, "runners", "run_pipeline.R")
repo_dir <- dirname(api_dir)

record_step <- function(name, state) {
  force(name)
  force(state)
  function(...) {
    state$calls <- c(state$calls, name)
    state$args[[name]] <- list(...)
    invisible(NULL)
  }
}

weekly <- new.env(parent = globalenv())
weekly$calls <- character()
weekly$args <- list()
weekly$loaded_files <- character()
weekly_steps <- c(
  "scrape_season", "clean_pbp", "create_pbp_variables", "generate_player_bio",
  "calculate_team_stats", "process_shot_charts", "calculate_player_stats",
  "generate_game_flow", "generate_team_pace", "generate_clutch_stats",
  "export_single_season"
)
for (step in weekly_steps) weekly[[step]] <- record_step(step, weekly)
weekly$scrape_season <- function(...) {
  record_step("scrape_season", weekly)(...)
  structure(data.frame(), new_matches = 1L)
}
weekly$source <- function(file, ...) {
  weekly$loaded_files <- c(weekly$loaded_files, file)
  if (grepl("config/seasons[.]R$", file)) {
    base::source(file, local = weekly)
  } else if (grepl("export_lineup_data[.]R$", file)) {
    stopifnot(isTRUE(getOption("openacb.skip_auto_lineup_export")))
  } else if (grepl("export_to_react[.]R$", file)) {
    stopifnot(getOption("openacb.react_export_seasons") == weekly$CURRENT_SEASON)
    weekly$calls <- c(weekly$calls, "react_export")
    if (isTRUE(weekly$fail_export)) stop("mock React export failure")
  }
}

# nested source from the repository root must find the api and current season
old_react_options <- options(openacb.react_export_seasons = 2025)
setwd(repo_dir)
base::source(update_path, local = weekly)
stopifnot(identical(normalizePath(getwd()), api_dir))
stopifnot(weekly$CURRENT_SEASON == 2027)
stopifnot(weekly$args$scrape_season[[1]] == 2027)
stopifnot(isTRUE(weekly$args$scrape_season$only_missing))
stopifnot(weekly$args$generate_player_bio$seasons == 2027)
stopifnot("./etl/12_player_positions.R" %in% weekly$loaded_files)
stopifnot(match("create_pbp_variables", weekly$calls) < match("generate_player_bio", weekly$calls))
stopifnot(match("generate_player_bio", weekly$calls) < match("calculate_player_stats", weekly$calls))
stopifnot(match("generate_clutch_stats", weekly$calls) < match("react_export", weekly$calls))
stopifnot(weekly$args$export_single_season[[1]] == 2027)
stopifnot(getOption("openacb.react_export_seasons") == 2025)

# failed exports must also restore the standalone export scope
weekly$fail_export <- TRUE
setwd(repo_dir)
react_error <- tryCatch(base::source(update_path, local = weekly), error = identity)
stopifnot(inherits(react_error, "error"))
stopifnot(grepl("mock React export failure", conditionMessage(react_error)))
stopifnot(getOption("openacb.react_export_seasons") == 2025)
weekly$fail_export <- FALSE
options(old_react_options)

# a weekly processing failure must stop before every export
weekly$calls <- character()
weekly$clean_pbp <- function(...) stop("mock cleaning failure")
setwd(repo_dir)
weekly_error <- tryCatch({
  base::source(update_path, local = weekly)
  NULL
}, error = identity)
stopifnot(inherits(weekly_error, "error"))
stopifnot(!any(c("export_single_season", "react_export") %in% weekly$calls))

master <- new.env(parent = globalenv())
master$calls <- character()
master$args <- list()
setwd(repo_dir)
base::source(pipeline_path, local = master)
stopifnot(identical(normalizePath(master$PIPELINE_BASE_DIR), api_dir))
base::source(file.path(api_dir, "config", "seasons.R"), local = master)

# module loading must never execute an export and must restore both options
old_options <- options(openacb.skip_auto_export = FALSE, openacb.skip_auto_lineup_export = FALSE)
master$loaded_exports <- character()
master$source <- function(file, ...) {
  if (grepl("runners/export_", file)) {
    stopifnot(isTRUE(getOption("openacb.skip_auto_export")))
    stopifnot(isTRUE(getOption("openacb.skip_auto_lineup_export")))
    master$loaded_exports <- c(master$loaded_exports, basename(file))
  }
}
master$load_pipeline_modules()
stopifnot(length(master$loaded_exports) == 2)
stopifnot(identical(getOption("openacb.skip_auto_export"), FALSE))
stopifnot(identical(getOption("openacb.skip_auto_lineup_export"), FALSE))
options(old_options)

master$source <- function(file, ...) {
  if (grepl("config/seasons[.]R$", file)) base::source(file, local = master)
}
master_steps <- c(weekly_steps, "calculate_lineup_analysis", "load_all_player_data",
                  "export_team_identities", "export_shot_data", "export_player_names",
                  "export_team_data", "export_player_data", "export_similarity_data",
                  "export_clutch_data", "export_teampace_data", "export_gameflow_data",
                  "export_lineup_data_to_react")
for (step in master_steps) master[[step]] <- record_step(step, master)
mock_data_dir <- tempfile("openacb-runner-test-")

# failed scraping cannot run downstream processing or publish stale files
master$scrape_season <- function(...) {
  master$calls <- c(master$calls, "scrape_season")
  stop("mock API failure")
}
master_error <- tryCatch({
  master$run_full_pipeline(2027, run_cross_season = FALSE,
                           steps = c("scrape", "clean"), data_dir = mock_data_dir)
  NULL
}, error = identity)
stopifnot(inherits(master_error, "error"))
stopifnot(grepl("Pipeline stopped at scrape", conditionMessage(master_error)))
stopifnot(identical(master$calls, "scrape_season"))

# the successful pipeline exports only after processing and includes player names
master$calls <- character()
master$scrape_season <- record_step("scrape_season", master)
master$run_full_pipeline(2027, run_cross_season = FALSE,
                        steps = c("scrape", "clean"), data_dir = mock_data_dir)
stopifnot(match("clean_pbp", master$calls) < match("export_team_data", master$calls))
stopifnot("export_player_names" %in% master$calls)
stopifnot(master$args$export_lineup_data_to_react$seasons == 2027)

# export failures must propagate to the caller
master$export_team_data <- function(...) stop("mock export failure")
export_error <- tryCatch({
  master$run_full_pipeline(2027, run_cross_season = FALSE,
                           steps = character(), data_dir = mock_data_dir)
  NULL
}, error = identity)
stopifnot(inherits(export_error, "error"))
stopifnot(grepl("mock export failure", conditionMessage(export_error)))

cat("weekly and master runner regressions passed\n")
