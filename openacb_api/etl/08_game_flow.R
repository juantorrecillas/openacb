# =============================================================================
# 08 - Game Flow Data
# =============================================================================
# Extracts slim per-game event data from PbP for the Game Flow visualization.
# Produces one JSON file per season: gameflow-{season_id}.json
#
# For each match, keeps only score-changing events, timeouts, and period markers.
# Each event: { t (seconds elapsed), sl (score local), sv (score visitor),
#               type, player, team, local }
# =============================================================================

library(dplyr)
library(jsonlite)

# ─── Main function ───────────────────────────────────────────────────────────

generate_game_flow <- function(
    season_id,
    data_dir = "./data",
    config_path = "./config/seasons.R",
    output_dir = "../openacb_react/public/data"
) {
  source(config_path, local = TRUE)

  cat(sprintf("\n--- Game Flow: season %d ---\n", season_id))

  # Load PbP data (prefer .Rds for speed)
  rds_path <- file.path(data_dir, "processed", paste0("PbP_adjustedData", season_id, ".Rds"))
  csv_path <- file.path(data_dir, "processed", paste0("PbP_adjustedData", season_id, ".csv"))

  if (file.exists(rds_path)) {
    cat("  Loading .Rds file...\n")
    pbp <- readRDS(rds_path)
  } else if (file.exists(csv_path)) {
    cat("  Loading .csv file...\n")
    pbp <- read.csv(csv_path, encoding = "UTF-8", stringsAsFactors = FALSE)
  } else {
    stop(sprintf("No PbP data found for season %d", season_id))
  }

  cat(sprintf("  Loaded %d rows\n", nrow(pbp)))

  # ─── Event types to keep ─────────────────────────────────────────────────

  scoring_types <- c(
    "Canasta de 2", "Canasta de 3", "Canasta de 1",
    "Mate", "Contraataque 2pt", "Contraataque 3pt"
  )

  key_event_types <- c(
    scoring_types,
    "Tiempo Muerto", "Tiempo de Televisi\u00f3n",
    "Inicio Periodo", "Final de Periodo",
    "Inicio de partido", "Final Partido"
  )

  # ─── Process each match ──────────────────────────────────────────────────

  match_ids <- unique(pbp$id_match)
  cat(sprintf("  Processing %d matches...\n", length(match_ids)))

  games_list <- list()

  for (mid in match_ids) {
    match_pbp <- pbp %>% filter(id_match == mid)

    # Get match metadata from first row
    first_row <- match_pbp[1, ]
    jornada <- first_row$jornada

    # Identify local and visitor teams
    local_rows <- match_pbp %>% filter(local == TRUE)
    visitor_rows <- match_pbp %>% filter(local == FALSE)
    local_team <- if (nrow(local_rows) > 0) local_rows$team[1] else NA
    visitor_team <- if (nrow(visitor_rows) > 0) visitor_rows$team[1] else NA

    # Final score from the last row with score data
    score_rows <- match_pbp %>% filter(!is.na(score_local) & !is.na(score_visitor))
    if (nrow(score_rows) == 0) next
    last_score <- tail(score_rows, 1)
    final_local <- last_score$score_local
    final_visitor <- last_score$score_visitor

    # Filter to key events only
    events_df <- match_pbp %>%
      filter(type.description %in% key_event_types) %>%
      arrange(period, desc(minute), desc(second))

    if (nrow(events_df) == 0) next

    # Max period (to detect overtime)
    max_period <- max(match_pbp$period, na.rm = TRUE)

    # Convert clock to seconds elapsed from game start
    # Each quarter: 10 minutes = 600 seconds
    # Overtime: 5 minutes = 300 seconds
    calc_elapsed <- function(period, minute, second) {
      if (period <= 4) {
        quarter_start <- (period - 1) * 600
        elapsed_in_quarter <- (10 - minute) * 60 - second
      } else {
        # Overtime periods
        quarter_start <- 4 * 600 + (period - 5) * 300
        elapsed_in_quarter <- (5 - minute) * 60 - second
      }
      return(quarter_start + elapsed_in_quarter)
    }

    events_out <- lapply(seq_len(nrow(events_df)), function(i) {
      row <- events_df[i, ]

      t_elapsed <- calc_elapsed(row$period, row$minute, row$second)

      # Determine short type code
      type_desc <- row$type.description
      type_code <- case_when(
        type_desc == "Canasta de 3" ~ "3p",
        type_desc == "Contraataque 3pt" ~ "3p",
        type_desc == "Canasta de 2" ~ "2p",
        type_desc == "Mate" ~ "2p",
        type_desc == "Contraataque 2pt" ~ "2p",
        type_desc == "Canasta de 1" ~ "ft",
        type_desc == "Tiempo Muerto" ~ "to",
        type_desc == "Tiempo de Televisi\u00f3n" ~ "tvto",
        type_desc == "Inicio Periodo" ~ "per_start",
        type_desc == "Final de Periodo" ~ "per_end",
        type_desc == "Inicio de partido" ~ "game_start",
        type_desc == "Final Partido" ~ "game_end",
        TRUE ~ "other"
      )

      is_scoring <- type_desc %in% scoring_types

      evt <- list(
        t = t_elapsed,
        sl = if (!is.na(row$score_local)) row$score_local else NULL,
        sv = if (!is.na(row$score_visitor)) row$score_visitor else NULL,
        type = type_code
      )

      # Only add player/team info for scoring events and timeouts
      if (is_scoring) {
        evt$player <- if (!is.na(row$license.licenseNick)) row$license.licenseNick else NULL
        evt$team <- if (isTRUE(row$local)) "L" else "V"
      } else if (type_code == "to") {
        evt$team <- if (isTRUE(row$local)) "L" else "V"
      }

      # Add period info for period markers
      if (type_code %in% c("per_start", "per_end")) {
        evt$period <- row$period
      }

      evt
    })

    # ─── Detect scoring runs via sliding-window margin swing ─────────────────
    # A "parcial" = a short stretch where the margin swings >= 10 pts dominated
    # by one team. Constraints: max 5 min, dominant team outscores >= 2:1.
    # Uses sliding-window min/max trackers so stale anchors don't block detection.

    scoring_events <- events_df %>%
      filter(type.description %in% scoring_types,
             !is.na(score_local), !is.na(score_visitor)) %>%
      mutate(
        t_elapsed = mapply(calc_elapsed, period, minute, second),
        margin    = score_local - score_visitor
      )

    runs <- list()
    if (nrow(scoring_events) >= 4) {
      sl_c  <- c(0L, scoring_events$score_local)
      sv_c  <- c(0L, scoring_events$score_visitor)
      mg_c  <- c(0L, scoring_events$margin)
      t_c   <- c(0,  scoring_events$t_elapsed)
      n_c   <- length(mg_c)

      threshold  <- 10L
      max_dur    <- 300   # 5 minutes max

      min_i <- 1L
      max_i <- 1L

      for (k in 2L:n_c) {
        # Expire stale trackers: rescan for best anchor within time window
        if (t_c[k] - t_c[min_i] > max_dur) {
          min_i <- k
          for (w in (k - 1L):1L) {
            if (t_c[k] - t_c[w] > max_dur) break
            if (mg_c[w] <= mg_c[min_i]) min_i <- w
          }
        }
        if (t_c[k] - t_c[max_i] > max_dur) {
          max_i <- k
          for (w in (k - 1L):1L) {
            if (t_c[k] - t_c[w] > max_dur) break
            if (mg_c[w] >= mg_c[max_i]) max_i <- w
          }
        }

        if (mg_c[k] <= mg_c[min_i]) min_i <- k
        if (mg_c[k] >= mg_c[max_i]) max_i <- k

        up_swing   <- mg_c[k] - mg_c[min_i]
        down_swing <- mg_c[max_i] - mg_c[k]

        if (up_swing >= threshold || down_swing >= threshold) {
          if (up_swing >= down_swing) {
            i <- min_i; j <- k
            run_team <- "L"
          } else {
            i <- max_i; j <- k
            run_team <- "V"
          }

          local_pts   <- sl_c[j] - sl_c[i]
          visitor_pts <- sv_c[j] - sv_c[i]
          won_pts  <- if (run_team == "L") local_pts else visitor_pts
          lost_pts <- if (run_team == "L") visitor_pts else local_pts

          # Keep only dominant runs (opponent scores at most half)
          if (lost_pts * 2 <= won_pts) {
            runs <- append(runs, list(list(
              team    = run_team,
              tStart  = t_c[i],
              tEnd    = t_c[j],
              slStart = sl_c[i], svStart = sv_c[i],
              slEnd   = sl_c[j], svEnd   = sv_c[j]
            )))
            # Only reset trackers when a run is accepted
            min_i <- k
            max_i <- k
          }
        }
      }
    }

    # Build game object
    game <- list(
      id = mid,
      jornada = jornada,
      local = local_team,
      visitor = visitor_team,
      scoreL = final_local,
      scoreV = final_visitor,
      maxPeriod = max_period,
      events = events_out,
      runs = runs
    )

    games_list <- append(games_list, list(game))
  }

  # Sort by jornada
  games_list <- games_list[order(sapply(games_list, function(g) g$jornada))]

  # ─── Save intermediate RDS ───────────────────────────────────────────
  rds_output <- file.path(data_dir, "processed", paste0("GameFlow_", season_id, ".Rds"))
  saveRDS(games_list, rds_output)
  cat(sprintf("  Saved intermediate: %s\n", basename(rds_output)))

  # ─── Export ────────────────────────────────────────────────────────────

  output_file <- file.path(output_dir, paste0("gameflow-", season_id, ".json"))
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  write_json(games_list, output_file, auto_unbox = TRUE, null = "null")

  file_size_mb <- round(file.size(output_file) / 1024 / 1024, 1)
  cat(sprintf("  Exported %d games to %s (%.1f MB)\n", length(games_list), basename(output_file), file_size_mb))

  invisible(games_list)
}
