# =============================================================================
# 09 - Team Pace & Quarter Analysis
# =============================================================================
# Aggregates per-quarter and per-segment scoring for each team in a season.
# Produces one JSON file per season: teampace-{season_id}.json
#
# Includes:
#   - Per-quarter scoring (scored/allowed/diff, Q1-Q4, no overtime)
#   - Per 2-minute segment breakdown (5 segments per quarter, 20 total)
#   - After-timeout efficiency (first scoring event after each timeout)
# =============================================================================

library(dplyr)
library(tidyr)
library(jsonlite)

# ─── Main function ───────────────────────────────────────────────────────────

generate_team_pace <- function(
    season_id,
    data_dir = "./data",
    config_path = "./config/seasons.R",
    output_dir = "../openacb_react/public/data"
) {
  source(config_path, local = TRUE)
  source(
    file.path(dirname(config_path), "team_identities.R"),
    local = TRUE,
    encoding = "UTF-8"
  )

  cat(sprintf("\n--- Team Pace: season %d ---\n", season_id))

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

  # ─── Filter: only regulation quarters, scoring events ────────────────────

  scoring_types <- c(
    "Canasta de 2", "Canasta de 3", "Canasta de 1",
    "Mate", "Contraataque 2pt", "Contraataque 3pt"
  )

  scoring <- pbp %>%
    filter(period <= 4, type.description %in% scoring_types) %>%
    mutate(
      # Derive per-play points from event type (statistics.points is cumulative)
      pts = case_when(
        type.description %in% c("Canasta de 3", "Contraataque 3pt") ~ 3L,
        type.description %in% c("Canasta de 2", "Mate", "Contraataque 2pt") ~ 2L,
        type.description == "Canasta de 1" ~ 1L,
        TRUE ~ 0L
      ),
      # Segment: which 2-min block within the quarter
      # minute counts down 10 -> 0, so:
      #   min 10-8 = segment 1 (first 2 min)
      #   min  8-6 = segment 2
      #   min  6-4 = segment 3
      #   min  4-2 = segment 4
      #   min  2-0 = segment 5
      segment = case_when(
        minute >= 8               ~ 1L,
        minute >= 6 & minute < 8  ~ 2L,
        minute >= 4 & minute < 6  ~ 3L,
        minute >= 2 & minute < 4  ~ 4L,
        TRUE                      ~ 5L
      )
    )

  # ─── All unique teams and matches ────────────────────────────────────────

  all_teams <- unique(pbp$team[!is.na(pbp$team) & pbp$team != ""])
  all_team_ids <- validate_unique_team_seasons(
    all_teams,
    rep(season_id, length(all_teams)),
    context = sprintf("team pace for season %d", season_id)
  )
  names(all_team_ids) <- all_teams
  match_ids <- unique(pbp$id_match)

  cat(sprintf("  Found %d teams, %d matches\n", length(all_teams), length(match_ids)))

  # ─── Per-match, per-team, per-quarter scoring ────────────────────────────
  # For each match and team, we compute points scored per quarter.
  # Points allowed = points scored by the opponent in that match-quarter.

  # Build opponent mapping: for each match, pair each team with its opponent
  match_teams <- pbp %>%
    filter(!is.na(team) & team != "") %>%
    distinct(id_match, team) %>%
    group_by(id_match) %>%
    mutate(role = row_number()) %>%
    ungroup() %>%
    filter(role <= 2)

  team1 <- match_teams %>% filter(role == 1) %>% select(id_match, team)
  team2 <- match_teams %>% filter(role == 2) %>% select(id_match, team)

  match_opponents <- bind_rows(
    inner_join(team1, team2, by = "id_match") %>% rename(team = team.x, opponent = team.y),
    inner_join(team2, team1, by = "id_match") %>% rename(team = team.x, opponent = team.y)
  )

  match_teams <- match_teams %>% select(id_match, team)

  # Scored per match-team-period (only from scoring events)
  scored_per_quarter_raw <- scoring %>%
    group_by(id_match, team, period) %>%
    summarise(scored = sum(pts, na.rm = TRUE), .groups = "drop")

  # Complete grid: every match × team × period 1-4, filling 0 for scoreless quarters.
  # Without this, games where a team scored 0 in a quarter are missing, which means
  # the opponent's points in those quarters are never counted as "allowed", inflating avg_diff.
  match_team_quarter <- match_opponents %>%
    distinct(id_match, team) %>%
    crossing(period = 1L:4L) %>%
    left_join(scored_per_quarter_raw, by = c("id_match", "team", "period")) %>%
    mutate(scored = ifelse(is.na(scored), 0L, scored))

  # Join to get allowed (= opponent scored)
  quarter_stats <- match_team_quarter %>%
    left_join(match_opponents, by = c("id_match", "team")) %>%
    left_join(
      match_team_quarter %>% rename(allowed = scored, opponent = team),
      by = c("id_match", "opponent", "period")
    ) %>%
    mutate(allowed = ifelse(is.na(allowed), 0, allowed))

  # ─── Aggregate per team per quarter (season averages) ────────────────────

  team_games <- match_opponents %>%
    distinct(id_match, team) %>%
    group_by(team) %>%
    summarise(games = n(), .groups = "drop")

  team_quarter_avg <- quarter_stats %>%
    group_by(team, period) %>%
    summarise(
      total_scored = sum(scored, na.rm = TRUE),
      total_allowed = sum(allowed, na.rm = TRUE),
      n_games = n(),
      .groups = "drop"
    ) %>%
    mutate(
      avg_scored = round(total_scored / n_games, 1),
      avg_allowed = round(total_allowed / n_games, 1),
      avg_diff = round((total_scored - total_allowed) / n_games, 1)
    )

  # ─── Per-segment breakdown (2-min segments) ─────────────────────────────

  # Scored per match-team-period-segment (only from scoring events)
  scored_per_segment_raw <- scoring %>%
    group_by(id_match, team, period, segment) %>%
    summarise(scored = sum(pts, na.rm = TRUE), .groups = "drop")

  # Complete grid: every match × team × period × segment, filling 0 for scoreless segments.
  # Teams frequently score 0 in a 2-min segment. Without the complete grid those games
  # are excluded, meaning the opponent's points in those segments are never counted as
  # "allowed", inflating avg_diff and making segment sums inconsistent with quarter totals.
  segment_stats <- match_opponents %>%
    distinct(id_match, team) %>%
    crossing(period = 1L:4L, segment = 1L:5L) %>%
    left_join(scored_per_segment_raw, by = c("id_match", "team", "period", "segment")) %>%
    mutate(scored = ifelse(is.na(scored), 0L, scored))

  # Add opponent allowed per segment
  segment_stats <- segment_stats %>%
    left_join(match_opponents, by = c("id_match", "team")) %>%
    left_join(
      segment_stats %>% rename(allowed = scored, opponent = team),
      by = c("id_match", "opponent", "period", "segment")
    ) %>%
    mutate(allowed = ifelse(is.na(allowed), 0, allowed))

  team_segment_avg <- segment_stats %>%
    group_by(team, period, segment) %>%
    summarise(
      total_scored = sum(scored, na.rm = TRUE),
      total_allowed = sum(allowed, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    ) %>%
    left_join(team_games, by = "team") %>%
    mutate(
      avg_scored = round(total_scored / games, 1),
      avg_allowed = round(total_allowed / games, 1),
      avg_diff = round((total_scored - total_allowed) / games, 1)
    )

  # ─── After-timeout efficiency ────────────────────────────────────────────
  # For each timeout, look at the next scoring event by the same team
  # within a short window (next ~30 seconds)

  timeout_events <- pbp %>%
    filter(period <= 4, type.description == "Tiempo Muerto") %>%
    select(id_match, team, period, minute, second) %>%
    mutate(
      to_time = (4 - period) * 600 + minute * 60 + second  # time remaining
    )

  scoring_for_to <- scoring %>%
    select(id_match, team, period, minute, second, pts) %>%
    mutate(
      sc_time = (4 - period) * 600 + minute * 60 + second
    )

  # For each timeout, find next scoring event by same team within 30s
  after_to_results <- list()
  for (i in seq_len(nrow(timeout_events))) {
    to <- timeout_events[i, ]
    candidates <- scoring_for_to %>%
      filter(
        id_match == to$id_match,
        team == to$team,
        sc_time < to$to_time,           # happened after the timeout (clock decreasing)
        sc_time >= to$to_time - 30      # within 30 seconds
      ) %>%
      arrange(desc(sc_time)) %>%        # earliest after timeout
      slice(1)

    if (nrow(candidates) == 1) {
      after_to_results <- append(after_to_results, list(data.frame(
        team = to$team,
        pts = candidates$pts
      )))
    } else {
      # No scoring event within window - count as 0
      after_to_results <- append(after_to_results, list(data.frame(
        team = to$team,
        pts = 0
      )))
    }
  }

  if (length(after_to_results) > 0) {
    after_to_df <- bind_rows(after_to_results)
    after_to_summary <- after_to_df %>%
      group_by(team) %>%
      summarise(
        timeouts = n(),
        total_pts = sum(pts),
        ppp = round(total_pts / n(), 2),
        scoring_pct = round(sum(pts > 0) / n() * 100, 1),
        .groups = "drop"
      )
    league_ppp_after_to <- round(sum(after_to_df$pts) / nrow(after_to_df), 2)
  } else {
    after_to_summary <- data.frame(team = character(), timeouts = integer(),
                                    total_pts = integer(), ppp = numeric(),
                                    scoring_pct = numeric())
    league_ppp_after_to <- 0
  }

  # ─── Build output JSON ──────────────────────────────────────────────────

  segment_labels <- c("0-2", "2-4", "4-6", "6-8", "8-10")

  teams_output <- lapply(all_teams, function(tm) {
    tg <- team_games %>% filter(team == tm)
    if (nrow(tg) == 0) return(NULL)
    n_games <- tg$games[1]

    # Quarter data
    q_data <- team_quarter_avg %>% filter(team == tm) %>% arrange(period)
    scored_q <- rep(0, 4)
    allowed_q <- rep(0, 4)
    diff_q <- rep(0, 4)
    for (j in seq_len(nrow(q_data))) {
      p <- q_data$period[j]
      if (p >= 1 && p <= 4) {
        scored_q[p] <- q_data$avg_scored[j]
        allowed_q[p] <- q_data$avg_allowed[j]
        diff_q[p] <- q_data$avg_diff[j]
      }
    }

    # Best and worst quarter
    best_q <- which.max(diff_q)
    worst_q <- which.min(diff_q)

    # Segment data
    seg_data <- team_segment_avg %>% filter(team == tm) %>% arrange(period, segment)
    segments <- list()
    for (q in 1:4) {
      for (s in 1:5) {
        row <- seg_data %>% filter(period == q, segment == s)
        segments <- append(segments, list(list(
          q = q,
          seg = s,
          label = segment_labels[s],
          scored = if (nrow(row) > 0) row$avg_scored[1] else 0,
          allowed = if (nrow(row) > 0) row$avg_allowed[1] else 0,
          diff = if (nrow(row) > 0) row$avg_diff[1] else 0
        )))
      }
    }

    # After timeout
    to_row <- after_to_summary %>% filter(team == tm)
    after_to <- if (nrow(to_row) > 0) {
      list(
        timeouts = to_row$timeouts[1],
        ppp = to_row$ppp[1],
        scoringPct = to_row$scoring_pct[1],
        leaguePpp = league_ppp_after_to
      )
    } else {
      list(timeouts = 0, ppp = 0, scoringPct = 0, leaguePpp = league_ppp_after_to)
    }

    list(
      team = tm,
      teamId = unname(all_team_ids[[tm]]),
      games = n_games,
      quarters = list(
        scored = scored_q,
        allowed = allowed_q,
        diff = diff_q
      ),
      bestQ = best_q,
      worstQ = worst_q,
      segments = segments,
      afterTimeout = after_to
    )
  })

  # Remove NULLs
  teams_output <- Filter(Negate(is.null), teams_output)

  # Sort by team name
  teams_output <- teams_output[order(sapply(teams_output, function(x) x$team))]

  # ─── Save intermediate RDS ───────────────────────────────────────────
  rds_output <- file.path(data_dir, "processed", paste0("TeamPace_", season_id, ".Rds"))
  saveRDS(teams_output, rds_output)
  cat(sprintf("  Saved intermediate: %s\n", basename(rds_output)))

  # ─── Export ────────────────────────────────────────────────────────────

  output_file <- file.path(output_dir, paste0("teampace-", season_id, ".json"))
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  write_json(teams_output, output_file, auto_unbox = TRUE, null = "null")

  file_size_kb <- round(file.size(output_file) / 1024, 1)
  cat(sprintf("  Exported %d teams to %s (%.1f KB)\n", length(teams_output), basename(output_file), file_size_kb))

  invisible(teams_output)
}
