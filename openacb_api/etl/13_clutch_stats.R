# =============================================================================
# Clutch Statistics
# =============================================================================
# Computes clutch stats for teams and players.
# Definition: last 5 minutes of Q4 (or any OT period) with score diff ≤ 5.
# Output: clutch-{season_id}.json
# =============================================================================

library(dplyr)
library(tidyr)
library(jsonlite)

generate_clutch_stats <- function(
    season_id,
    data_dir = "./data",
    config_path = "./config/seasons.R",
    output_dir = "../openacb_react/public/data"
) {
  source(config_path, local = TRUE)
  cat(sprintf("\n--- Clutch Stats: season %d ---\n", season_id))

  # ─── Load PBP data ────────────────────────────────────────────────────────
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

  # ─── Forward-fill scores within each game ────────────────────────────────
  # score_local / score_visitor are only set on scoring events in the raw API
  # data. We carry them forward so every row has the current score, allowing
  # us to correctly identify clutch moments for non-scoring events too.
  #
  # IMPORTANT: scoring events store the POST-basket score on their own row.
  # A basket that pushes the margin from 4 → 6 would read |6| > 5 and be
  # wrongly excluded if we used score_local/score_visitor directly.  Instead
  # we compute pre_score_* via lag() (= the score BEFORE this event) so that
  # any event is judged by the margin that existed when it actually occurred.
  pbp_scored <- pbp %>%
    group_by(id_match) %>%
    arrange(period, desc(minute), desc(second), .by_group = TRUE) %>%
    fill(score_local, score_visitor, .direction = "down") %>%
    mutate(
      score_local   = if_else(is.na(score_local),   0L, as.integer(score_local)),
      score_visitor = if_else(is.na(score_visitor), 0L, as.integer(score_visitor)),
      pre_score_local   = lag(score_local,   default = 0L),
      pre_score_visitor = lag(score_visitor, default = 0L)
    ) %>%
    ungroup()

  # ─── Clutch filter ────────────────────────────────────────────────────────
  # Last 5 minutes of Q4 or any overtime period, score difference ≤ 5 points.
  # Use pre-event scores so that a basket taken when the margin was 4 is
  # correctly classified as clutch even if it pushes the margin to 6.
  clutch <- pbp_scored %>%
    filter(
      (period == 4 & minute <= 5) | period > 4,
      abs(pre_score_local - pre_score_visitor) <= 5
    )

  cat(sprintf("  Clutch events: %d\n", nrow(clutch)))

  if (nrow(clutch) == 0) {
    cat("  No clutch events found, skipping\n")
    return(invisible(NULL))
  }

  clutch_match_ids <- unique(clutch$id_match)
  cat(sprintf("  Clutch games: %d\n", length(clutch_match_ids)))

  # ─── Match metadata ───────────────────────────────────────────────────────
  # Identify local/visitor teams and final scores for each match

  local_map <- pbp %>%
    filter(!is.na(team), team != "", as.logical(local) == TRUE) %>%
    group_by(id_match) %>%
    slice(1) %>%
    ungroup() %>%
    select(id_match, local_team = team)

  visitor_map <- pbp %>%
    filter(!is.na(team), team != "", as.logical(local) == FALSE) %>%
    group_by(id_match) %>%
    slice(1) %>%
    ungroup() %>%
    select(id_match, visitor_team = team)

  # Final score: last row with score data per match (sorted chronologically)
  final_scores <- pbp %>%
    filter(!is.na(score_local), !is.na(score_visitor)) %>%
    group_by(id_match) %>%
    arrange(period, desc(minute), desc(second), .by_group = TRUE) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(id_match, final_local = score_local, final_visitor = score_visitor)

  match_info <- local_map %>%
    inner_join(visitor_map,  by = "id_match") %>%
    inner_join(final_scores, by = "id_match") %>%
    filter(!is.na(local_team), !is.na(visitor_team)) %>%
    filter(id_match %in% clutch_match_ids)

  # ─── Team W/L in clutch games ─────────────────────────────────────────────
  # A team "wins" a clutch game = they won the overall game
  team_wl <- bind_rows(
    match_info %>%
      select(id_match, team = local_team, final_local, final_visitor) %>%
      mutate(won = final_local > final_visitor),
    match_info %>%
      select(id_match, team = visitor_team, final_local, final_visitor) %>%
      mutate(won = final_visitor > final_local)
  ) %>%
    group_by(team) %>%
    summarise(
      clutchGames = n(),
      wins        = sum(won, na.rm = TRUE),
      losses      = sum(!won, na.rm = TRUE),
      .groups = "drop"
    )

  # ─── Team clutch scoring (pts scored/allowed during clutch minutes) ────────
  match_opp <- bind_rows(
    match_info %>% select(id_match, team = local_team,   opponent = visitor_team),
    match_info %>% select(id_match, team = visitor_team, opponent = local_team)
  )

  scoring_types <- c(
    "Canasta de 2", "Canasta de 3", "Canasta de 1",
    "Mate", "Contraataque 2pt", "Contraataque 3pt"
  )

  clutch_pts_raw <- clutch %>%
    filter(type.description %in% scoring_types, !is.na(team), team != "") %>%
    mutate(
      pts = case_when(
        type.description %in% c("Canasta de 3", "Contraataque 3pt") ~ 3L,
        type.description %in% c("Canasta de 2", "Mate", "Contraataque 2pt") ~ 2L,
        type.description == "Canasta de 1" ~ 1L,
        TRUE ~ 0L
      )
    ) %>%
    group_by(id_match, team) %>%
    summarise(scored = sum(pts), .groups = "drop")

  # Complete grid: ensure zero-scoring clutch games are counted
  scoring_grid <- match_opp %>%
    distinct(id_match, team) %>%
    left_join(clutch_pts_raw, by = c("id_match", "team")) %>%
    mutate(scored = ifelse(is.na(scored), 0L, scored))

  team_clutch_pts <- scoring_grid %>%
    left_join(match_opp, by = c("id_match", "team")) %>%
    left_join(
      scoring_grid %>% rename(allowed = scored, opponent = team),
      by = c("id_match", "opponent")
    ) %>%
    mutate(allowed = ifelse(is.na(allowed), 0L, allowed)) %>%
    group_by(team) %>%
    summarise(
      ptsScoredAvg  = round(sum(scored)  / n(), 1),
      ptsAllowedAvg = round(sum(allowed) / n(), 1),
      plusMinus     = round((sum(scored) - sum(allowed)) / n(), 1),
      .groups = "drop"
    )

  # ─── Team shooting in clutch ─────────────────────────────────────────────
  team_shooting <- clutch %>%
    filter(!is.na(team), team != "") %>%
    group_by(team) %>%
    summarise(
      fg2M = sum(T2A, na.rm = TRUE),
      fg2A = sum(T2I, na.rm = TRUE),
      fg3M = sum(T3A, na.rm = TRUE),
      fg3A = sum(T3I, na.rm = TRUE),
      ftM  = sum(T1A, na.rm = TRUE),
      ftA  = sum(T1I, na.rm = TRUE),
      .groups = "drop"
    )

  # Combine
  team_stats <- team_wl %>%
    left_join(team_clutch_pts, by = "team") %>%
    left_join(team_shooting,   by = "team") %>%
    mutate(
      fg2Pct = if_else(fg2A > 0, round(fg2M / fg2A * 100, 1), NA_real_),
      fg3Pct = if_else(fg3A > 0, round(fg3M / fg3A * 100, 1), NA_real_),
      ftPct  = if_else(ftA  > 0, round(ftM  / ftA  * 100, 1), NA_real_),
      efgPct = if_else((fg2A + fg3A) > 0,
                       round((fg2M + 1.5 * fg3M) / (fg2A + fg3A) * 100, 1), NA_real_),
      fg2Apg = round(fg2A / clutchGames, 1),
      fg3Apg = round(fg3A / clutchGames, 1),
      ftApg  = round(ftA  / clutchGames, 1)
    ) %>%
    arrange(team)

  # ─── Player clutch stats ─────────────────────────────────────────────────
  player_clutch <- clutch %>%
    filter(!is.na(license.id), !is.na(license.licenseNick), license.licenseNick != "") %>%
    group_by(licenseId = as.character(license.id), nick = license.licenseNick, team) %>%
    summarise(
      games = n_distinct(id_match),
      pts   = sum(puntos,       na.rm = TRUE),
      dreb  = sum(reb_def,      na.rm = TRUE),
      oreb  = sum(reb_of,       na.rm = TRUE),
      ast   = sum(asistencias,  na.rm = TRUE),
      stl   = sum(recuperacion, na.rm = TRUE),
      blk   = sum(tapon,        na.rm = TRUE),
      tov   = sum(perdida,      na.rm = TRUE),
      fg2M  = sum(T2A,          na.rm = TRUE),
      fg2A  = sum(T2I,          na.rm = TRUE),
      fg3M  = sum(T3A,          na.rm = TRUE),
      fg3A  = sum(T3I,          na.rm = TRUE),
      ftM   = sum(T1A,          na.rm = TRUE),
      ftA   = sum(T1I,          na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      reb    = dreb + oreb,
      ptsG   = round(pts / games, 1),
      rebG   = round(reb / games, 1),
      astG   = round(ast / games, 1),
      stlG   = round(stl / games, 1),
      blkG   = round(blk / games, 1),
      tovG   = round(tov / games, 1),
      fg2Pct = if_else(fg2A > 0, round(fg2M / fg2A * 100, 1), NA_real_),
      fg3Pct = if_else(fg3A > 0, round(fg3M / fg3A * 100, 1), NA_real_),
      ftPct  = if_else(ftA  > 0, round(ftM  / ftA  * 100, 1), NA_real_),
      efgPct = if_else((fg2A + fg3A) > 0,
                       round((fg2M + 1.5 * fg3M) / (fg2A + fg3A) * 100, 1), NA_real_)
    ) %>%
    arrange(desc(ptsG))

  cat(sprintf("  Teams: %d  Players: %d\n", nrow(team_stats), nrow(player_clutch)))

  # ─── Build output ─────────────────────────────────────────────────────────
  teams_out <- lapply(seq_len(nrow(team_stats)), function(i) {
    r <- team_stats[i, ]
    list(
      team          = r$team,
      games         = r$clutchGames,
      wins          = r$wins,
      losses        = r$losses,
      ptsScoredAvg  = r$ptsScoredAvg,
      ptsAllowedAvg = r$ptsAllowedAvg,
      plusMinus     = r$plusMinus,
      fg2Pct        = r$fg2Pct,
      fg3Pct        = r$fg3Pct,
      ftPct         = r$ftPct,
      efgPct        = r$efgPct,
      fg2Apg        = r$fg2Apg,
      fg3Apg        = r$fg3Apg,
      ftApg         = r$ftApg
    )
  })

  players_out <- lapply(seq_len(nrow(player_clutch)), function(i) {
    r <- player_clutch[i, ]
    list(
      nick      = r$nick,
      licenseId = r$licenseId,
      team      = r$team,
      games     = r$games,
      pts       = r$ptsG,
      reb       = r$rebG,
      ast       = r$astG,
      stl       = r$stlG,
      blk       = r$blkG,
      tov       = r$tovG,
      fg2Pct    = r$fg2Pct,
      fg3Pct    = r$fg3Pct,
      ftPct     = r$ftPct,
      efgPct    = r$efgPct,
      fg2A      = r$fg2A,
      fg3A      = r$fg3A,
      ftA       = r$ftA
    )
  })

  output <- list(teams = teams_out, players = players_out)

  # ─── Save ─────────────────────────────────────────────────────────────────
  rds_out <- file.path(data_dir, "processed", paste0("Clutch_", season_id, ".Rds"))
  saveRDS(output, rds_out)
  cat(sprintf("  Saved intermediate: %s\n", basename(rds_out)))

  json_out <- file.path(output_dir, paste0("clutch-", season_id, ".json"))
  dir.create(dirname(json_out), showWarnings = FALSE, recursive = TRUE)
  write_json(output, json_out, auto_unbox = TRUE, null = "null", na = "null")

  file_size_kb <- round(file.size(json_out) / 1024, 1)
  cat(sprintf("  Exported to %s (%.1f KB)\n", basename(json_out), file_size_kb))

  invisible(output)
}
