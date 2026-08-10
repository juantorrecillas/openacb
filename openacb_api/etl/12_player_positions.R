# =============================================================================
# 12 - Player Bio Scraper
# =============================================================================
# Scrapes player bio data (position, height, birth date) from acb.com using
# rvest + CSS selectors. Outputs:
#   - player-bio.json  (for React, keyed by licenseId)
#   - player_bio.csv   (for R pipeline, used by 07_player_stats.R)
#
# CSS selectors (from ACB player page):
#   Position:   .posicion .roboto_condensed_bold
#   Height:     .altura .roboto_condensed_bold
#   Birth date: .fecha_nacimiento .roboto_condensed_bold
#
# Usage (from openacb_api/ directory):
#   source("etl/12_player_positions.R")
#   generate_player_bio()                         # all seasons, incremental
#   generate_player_bio(incremental = FALSE)      # force re-scrape all
# =============================================================================

library(rvest)
library(jsonlite)

generate_player_bio <- function(
    data_dir    = "./data/processed",
    output_dir  = "../openacb_react/public/data",
    seasons     = c(2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026),
    incremental = TRUE,
    sleep_sec   = 1
) {
  cat("\n--- Extracting player bio data from acb.com ---\n")

  # collect ids from both existing stats and the current adjusted pbp
  all_ids <- character(0)
  for (yr in seasons) {
    stats_paths <- c(
      file.path(data_dir, paste0("PlayerStats", yr, ".Rds")),
      file.path(data_dir, paste0("PlayerStats", yr, ".csv"))
    )
    stats_paths <- stats_paths[file.exists(stats_paths)]

    if (length(stats_paths) > 0) {
      stats_path <- stats_paths[1]
      stats <- if (grepl("\\.Rds$", stats_path)) {
        readRDS(stats_path)
      } else {
        read.csv(stats_path, encoding = "UTF-8", stringsAsFactors = FALSE)
      }

      if ("license_id" %in% names(stats)) {
        all_ids <- c(all_ids, as.character(stats$license_id[!is.na(stats$license_id)]))
      }
    }

    pbp_paths <- c(
      file.path(data_dir, paste0("PbP_adjustedData", yr, ".Rds")),
      file.path(data_dir, paste0("PbP_adjustedData", yr, ".csv"))
    )
    pbp_paths <- pbp_paths[file.exists(pbp_paths)]

    if (length(pbp_paths) > 0) {
      pbp_path <- pbp_paths[1]
      pbp <- if (grepl("\\.Rds$", pbp_path)) {
        readRDS(pbp_path)
      } else {
        read.csv(pbp_path, encoding = "UTF-8", stringsAsFactors = FALSE)
      }

      if ("license.id" %in% names(pbp)) {
        all_ids <- c(all_ids, as.character(pbp$license.id[!is.na(pbp$license.id)]))
      }

      pista_cols <- grep("_[0-9]+_pista$", names(pbp), value = TRUE)
      pista_ids <- sub("^.*_([0-9]+)_pista$", "\\1", pista_cols)
      all_ids <- c(all_ids, pista_ids)
    }
  }
  all_ids <- unique(all_ids[nzchar(all_ids)])
  cat(sprintf("  Found %d unique players across all seasons.\n", length(all_ids)))

  if (length(all_ids) == 0) {
    cat("  No licenseIds found. Run the player stats pipeline first.\n")
    return(invisible(list()))
  }

  # -- 2. Load existing output (incremental mode) -----------------------------
  output_file <- file.path(output_dir, "player-bio.json")
  bio_map <- list()
  if (incremental && file.exists(output_file)) {
    bio_map <- fromJSON(output_file, simplifyVector = FALSE)
    cat(sprintf("  Loaded %d existing entries (incremental mode).\n", length(bio_map)))
  }

  has_position <- function(entry) {
    if (is.null(entry) || is.null(entry$position)) return(FALSE)
    value <- gsub(intToUtf8(160), " ", as.character(entry$position), fixed = TRUE)
    nzchar(trimws(value))
  }

  pending <- all_ids[!vapply(all_ids, function(lid) {
    has_position(bio_map[[lid]])
  }, logical(1))]
  cat(sprintf("  %d players to scrape.\n", length(pending)))

  if (length(pending) == 0) {
    cat("  All players already have bio data. Nothing to do.\n")
    return(invisible(bio_map))
  }

  # -- 3. Scrape each player page with rvest -----------------------------------
  base_url <- "https://www.acb.com/jugador/temporada-a-temporada/id/"
  css_position <- ".posicion .roboto_condensed_bold"
  css_altura   <- ".altura .roboto_condensed_bold"
  css_fecha    <- ".fecha_nacimiento .roboto_condensed_bold"

  ok_count  <- 0
  err_count <- 0

  for (i in seq_along(pending)) {
    lid <- pending[i]
    url <- paste0(base_url, lid)

    if (i > 1) Sys.sleep(sleep_sec)

    html <- tryCatch(read_html(url), error = function(e) NULL)

    if (is.null(html)) {
      cat(sprintf("    [%d/%d] %s — connection error, skipping.\n", i, length(pending), lid))
      err_count <- err_count + 1
      next
    }

    # Extract fields via CSS selectors
    pos_text    <- html %>% html_nodes(css_position) %>% html_text() %>% trimws()
    height_text <- html %>% html_nodes(css_altura)   %>% html_text() %>% trimws()
    fecha_text  <- html %>% html_nodes(css_fecha)    %>% html_text() %>% trimws()

    # Position: keep as-is (e.g. "Escolta", "Base", "Ala-Pívot")
    position <- NA_character_
    if (length(pos_text) > 0) {
      position_value <- gsub(intToUtf8(160), " ", pos_text[1], fixed = TRUE)
      position_value <- trimws(position_value)
      if (nzchar(position_value)) position <- position_value
    }

    # Height: "1,88" → 1.88 (numeric)
    height_m <- NA_real_
    if (length(height_text) > 0 && nzchar(height_text[1])) {
      h <- gsub(",", ".", height_text[1])
      h <- sub("\\s*m.*$", "", h)
      height_m <- suppressWarnings(as.numeric(trimws(h)))
    }

    # Birth date: "26/12/1991" → "1991-12-26"
    birth_date <- NA_character_
    if (length(fecha_text) > 0 && nzchar(fecha_text[1])) {
      dmy <- regmatches(fecha_text[1], regexpr("\\d{2}/\\d{2}/\\d{4}", fecha_text[1]))
      if (length(dmy) == 1) {
        parts <- strsplit(dmy, "/")[[1]]
        birth_date <- paste(parts[3], parts[2], parts[1], sep = "-")
      }
    }

    if (is.na(position) && is.na(height_m) && is.na(birth_date)) {
      cat(sprintf("    [%d/%d] %s — no bio data found.\n", i, length(pending), lid))
      err_count <- err_count + 1
    } else {
      bio_map[[lid]] <- list(
        position  = if (!is.na(position))   position   else NULL,
        heightM   = if (!is.na(height_m))   height_m   else NULL,
        birthDate = if (!is.na(birth_date)) birth_date else NULL
      )
      ok_count <- ok_count + 1
    }

    if (i %% 25 == 0 || i == length(pending)) {
      cat(sprintf("    [%d/%d] done — %d found, %d errors so far.\n",
                  i, length(pending), ok_count, err_count))
    }
  }

  # -- 4. Save outputs --------------------------------------------------------

  # JSON for React
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  write_json(bio_map, output_file, auto_unbox = TRUE, null = "null")

  # CSV for R pipeline (07_player_stats.R joins position from here)
  csv_file <- file.path(data_dir, "player_bio.csv")
  clean_position <- function(entry) {
    if (!has_position(entry)) return(NA_character_)
    value <- gsub(intToUtf8(160), " ", as.character(entry$position), fixed = TRUE)
    trimws(value)
  }

  bio_df <- data.frame(
    license_id = as.integer(names(bio_map)),
    position   = sapply(bio_map, clean_position),
    height_m   = sapply(bio_map, function(x) if (!is.null(x$heightM))   x$heightM   else NA_real_),
    birth_date = sapply(bio_map, function(x) if (!is.null(x$birthDate)) x$birthDate else NA_character_),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  write.csv(bio_df, csv_file, row.names = FALSE, fileEncoding = "UTF-8")

  cat(sprintf("\n  Saved %d player bio records.\n", length(bio_map)))
  cat(sprintf("    New: %d  |  Errors: %d\n", ok_count, err_count))

  invisible(bio_map)
}
