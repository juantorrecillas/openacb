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
  source(
    file.path(dirname(config_path), "team_identities.R"),
    local = TRUE,
    encoding = "UTF-8"
  )
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

  # ─── Team shooting + rebounds + misc in clutch ───────────────────────────
  shooting_per_game <- clutch %>%
    filter(!is.na(team), team != "") %>%
    group_by(id_match, team) %>%
    summarise(
      fg2M = sum(T2A,          na.rm = TRUE),
      fg2A = sum(T2I,          na.rm = TRUE),
      fg3M = sum(T3A,          na.rm = TRUE),
      fg3A = sum(T3I,          na.rm = TRUE),
      ftM  = sum(T1A,          na.rm = TRUE),
      ftA  = sum(T1I,          na.rm = TRUE),
      oreb = sum(reb_of,       na.rm = TRUE),
      dreb = sum(reb_def,      na.rm = TRUE),
      ast  = sum(asistencias,  na.rm = TRUE),
      stl  = sum(recuperacion, na.rm = TRUE),
      blk  = sum(tapon,        na.rm = TRUE),
      tov  = sum(perdida,      na.rm = TRUE),
      .groups = "drop"
    )

  team_shooting <- shooting_per_game %>%
    group_by(team) %>%
    summarise(
      fg2M = sum(fg2M), fg2A = sum(fg2A),
      fg3M = sum(fg3M), fg3A = sum(fg3A),
      ftM  = sum(ftM),  ftA  = sum(ftA),
      oreb = sum(oreb), dreb = sum(dreb),
      ast  = sum(ast),  stl  = sum(stl),
      blk  = sum(blk),  tov  = sum(tov),
      .groups = "drop"
    )

  opp_shooting <- match_opp %>%
    left_join(
      shooting_per_game %>% rename(opponent = team),
      by = c("id_match", "opponent")
    ) %>%
    group_by(team) %>%
    summarise(
      opp_fg2M = sum(fg2M, na.rm = TRUE),
      opp_fg2A = sum(fg2A, na.rm = TRUE),
      opp_fg3M = sum(fg3M, na.rm = TRUE),
      opp_fg3A = sum(fg3A, na.rm = TRUE),
      opp_ftM  = sum(ftM,  na.rm = TRUE),
      opp_ftA  = sum(ftA,  na.rm = TRUE),
      opp_oreb = sum(oreb, na.rm = TRUE),
      opp_dreb = sum(dreb, na.rm = TRUE),
      opp_ast  = sum(ast,  na.rm = TRUE),
      opp_stl  = sum(stl,  na.rm = TRUE),
      opp_blk  = sum(blk,  na.rm = TRUE),
      opp_tov  = sum(tov,  na.rm = TRUE),
      .groups = "drop"
    )

  # Combine
  team_stats <- team_wl %>%
    left_join(team_clutch_pts, by = "team") %>%
    left_join(team_shooting,   by = "team") %>%
    left_join(opp_shooting,    by = "team") %>%
    mutate(
      # shooting percentages
      fgPct       = if_else((fg2A + fg3A) > 0,
                            round((fg2M + fg3M) / (fg2A + fg3A) * 100, 1), NA_real_),
      fg2Pct      = if_else(fg2A > 0, round(fg2M / fg2A * 100, 1), NA_real_),
      fg3Pct      = if_else(fg3A > 0, round(fg3M / fg3A * 100, 1), NA_real_),
      ftPct       = if_else(ftA  > 0, round(ftM  / ftA  * 100, 1), NA_real_),
      efgPct      = if_else((fg2A + fg3A) > 0,
                            round((fg2M + 1.5 * fg3M) / (fg2A + fg3A) * 100, 1), NA_real_),
      # per-game counting
      fg2Apg      = round(fg2A / clutchGames, 1),
      fg3Apg      = round(fg3A / clutchGames, 1),
      ftApg       = round(ftA  / clutchGames, 1),
      orebAvg     = round(oreb / clutchGames, 1),
      drebAvg     = round(dreb / clutchGames, 1),
      rebAvg      = round((oreb + dreb) / clutchGames, 1),
      apg         = round(ast / clutchGames, 1),
      spg         = round(stl / clutchGames, 1),
      bpg         = round(blk / clutchGames, 1),
      topg        = round(tov / clutchGames, 1),
      # possessions estimate (Hollinger): FGA + 0.44*FTA + TOV - OREB
      poss_tot    = (fg2A + fg3A) + 0.44 * ftA + tov - oreb,
      opp_poss_tot = (opp_fg2A + opp_fg3A) + 0.44 * opp_ftA + opp_tov - opp_oreb,
      # ratings per 100 possessions
      ortg        = if_else(poss_tot > 0,
                            round(ptsScoredAvg  * clutchGames / poss_tot * 100, 1), NA_real_),
      drtg        = if_else(opp_poss_tot > 0,
                            round(ptsAllowedAvg * clutchGames / opp_poss_tot * 100, 1), NA_real_),
      # opponent shooting
      opp_fgPct   = if_else((opp_fg2A + opp_fg3A) > 0,
                            round((opp_fg2M + opp_fg3M) / (opp_fg2A + opp_fg3A) * 100, 1), NA_real_),
      opp_fg2Pct  = if_else(opp_fg2A > 0, round(opp_fg2M / opp_fg2A * 100, 1), NA_real_),
      opp_fg3Pct  = if_else(opp_fg3A > 0, round(opp_fg3M / opp_fg3A * 100, 1), NA_real_),
      opp_ftPct   = if_else(opp_ftA  > 0, round(opp_ftM  / opp_ftA  * 100, 1), NA_real_),
      opp_efgPct  = if_else((opp_fg2A + opp_fg3A) > 0,
                            round((opp_fg2M + 1.5 * opp_fg3M) / (opp_fg2A + opp_fg3A) * 100, 1), NA_real_),
      opp_fg2Apg  = round(opp_fg2A / clutchGames, 1),
      opp_fg3Apg  = round(opp_fg3A / clutchGames, 1),
      opp_ftApg   = round(opp_ftA  / clutchGames, 1),
      opp_orebAvg = round(opp_oreb / clutchGames, 1),
      opp_drebAvg = round(opp_dreb / clutchGames, 1),
      opp_apg     = round(opp_ast / clutchGames, 1),
      opp_spg     = round(opp_stl / clutchGames, 1),
      opp_bpg     = round(opp_blk / clutchGames, 1),
      opp_topg    = round(opp_tov / clutchGames, 1),
      # rebound percentages
      orbPct      = if_else((oreb + opp_dreb) > 0,
                            round(oreb / (oreb + opp_dreb) * 100, 1), NA_real_),
      drbPct      = if_else((dreb + opp_oreb) > 0,
                            round(dreb / (dreb + opp_oreb) * 100, 1), NA_real_),
      # rate stats (stored as 0-100 to match display in clutch table)
      astRate     = if_else((fg2M + fg3M) > 0,
                            round(ast / (fg2M + fg3M) * 100, 1), NA_real_),
      blkRate     = if_else(opp_fg2A > 0,
                            round(blk / opp_fg2A * 100, 1), NA_real_),
      stlRate     = if_else(opp_poss_tot > 0,
                            round(stl / opp_poss_tot * 100, 1), NA_real_),
      tovRate     = if_else(poss_tot > 0,
                            round(tov / poss_tot * 100, 1), NA_real_),
      astToRatio  = if_else(tov > 0, round(ast / tov, 2), NA_real_),
      opp_astRate = if_else((opp_fg2M + opp_fg3M) > 0,
                            round(opp_ast / (opp_fg2M + opp_fg3M) * 100, 1), NA_real_),
      opp_blkRate = if_else(fg2A > 0,
                            round(opp_blk / fg2A * 100, 1), NA_real_),
      opp_stlRate = if_else(poss_tot > 0,
                            round(opp_stl / poss_tot * 100, 1), NA_real_),
      opp_tovRate = if_else(opp_poss_tot > 0,
                            round(opp_tov / opp_poss_tot * 100, 1), NA_real_),
      opp_astToRatio = if_else(opp_tov > 0, round(opp_ast / opp_tov, 2), NA_real_)
    ) %>%
    mutate(netRtg = if_else(!is.na(ortg) & !is.na(drtg), round(ortg - drtg, 1), NA_real_)) %>%
    arrange(team)

  # ─── Player clutch stats ─────────────────────────────────────────────────
  player_clutch <- clutch %>%
    filter(
      !is.na(license.id),
      !is.na(license.licenseNick),
      license.licenseNick != "",
      !is.na(team),
      team != ""
    ) %>%
    group_by(licenseId = as.character(license.id), nick = license.licenseNick, team) %>%
    summarise(
      games  = n_distinct(id_match),
      pts    = sum(puntos,       na.rm = TRUE),
      dreb   = sum(reb_def,      na.rm = TRUE),
      oreb   = sum(reb_of,       na.rm = TRUE),
      ast    = sum(asistencias,  na.rm = TRUE),
      stl    = sum(recuperacion, na.rm = TRUE),
      blk    = sum(tapon,        na.rm = TRUE),
      tov    = sum(perdida,      na.rm = TRUE),
      fouls  = sum(falta,        na.rm = TRUE),
      fg2M   = sum(T2A,          na.rm = TRUE),
      fg2A   = sum(T2I,          na.rm = TRUE),
      fg3M   = sum(T3A,          na.rm = TRUE),
      fg3A   = sum(T3I,          na.rm = TRUE),
      ftM    = sum(T1A,          na.rm = TRUE),
      ftA    = sum(T1I,          na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      reb    = dreb + oreb,
      ptsG   = round(pts   / games, 1),
      rebG   = round(reb   / games, 1),
      orebG  = round(oreb  / games, 1),
      drebG  = round(dreb  / games, 1),
      astG   = round(ast   / games, 1),
      stlG   = round(stl   / games, 1),
      blkG   = round(blk   / games, 1),
      tovG   = round(tov   / games, 1),
      foulsG = round(fouls / games, 1),
      fg2Pct = if_else(fg2A > 0, round(fg2M / fg2A * 100, 1), NA_real_),
      fg3Pct = if_else(fg3A > 0, round(fg3M / fg3A * 100, 1), NA_real_),
      ftPct  = if_else(ftA  > 0, round(ftM  / ftA  * 100, 1), NA_real_),
      fgPct  = if_else((fg2A + fg3A) > 0,
                       round((fg2M + fg3M) / (fg2A + fg3A) * 100, 1), NA_real_),
      efgPct = if_else((fg2A + fg3A) > 0,
                       round((fg2M + 1.5 * fg3M) / (fg2A + fg3A) * 100, 1), NA_real_)
    ) %>%
    arrange(desc(ptsG))

  # ─── Per-player W/L in clutch games ──────────────────────────────────────
  player_game_result <- clutch %>%
    filter(!is.na(license.id), license.licenseNick != "") %>%
    distinct(licenseId = as.character(license.id), nick = license.licenseNick, team, id_match) %>%
    left_join(
      bind_rows(
        match_info %>% transmute(id_match, team = local_team,   won = final_local > final_visitor),
        match_info %>% transmute(id_match, team = visitor_team, won = final_visitor > final_local)
      ),
      by = c("id_match", "team")
    ) %>%
    group_by(licenseId, nick, team) %>%
    summarise(
      wins   = sum(won == TRUE,  na.rm = TRUE),
      losses = sum(won == FALSE, na.rm = TRUE),
      .groups = "drop"
    )

  player_clutch <- player_clutch %>%
    left_join(player_game_result, by = c("licenseId", "nick", "team"))

  # ─── Clutch minutes ───────────────────────────────────────────────────────
  # Same approach as calculate_minutes() in 07_player_stats.R but scoped to
  # clutch-filtered rows. _pista columns (1 = on court) survive the filter.
  pista_cols <- grep("_pista$", names(clutch), value = TRUE)

  if (length(pista_cols) > 0) {
    clutch_timed <- clutch %>%
      group_by(id_match) %>%
      arrange(period, desc(minute), desc(second), .by_group = TRUE) %>%
      mutate(
        cur_sec   = as.numeric(minute) * 60 + as.numeric(second),
        nxt_sec   = lead(cur_sec),
        nxt_per   = lead(period),
        time_diff = cur_sec - nxt_sec
      ) %>%
      mutate(
        time_diff = ifelse(period != nxt_per | is.na(nxt_per), 0, time_diff),
        time_diff = ifelse(is.na(time_diff) | time_diff < 0 | time_diff > 300, 0, time_diff)
      ) %>%
      ungroup()

    valid_pista <- pista_cols[pista_cols %in% names(clutch_timed)]

    if (length(valid_pista) > 0) {
      p_mat <- as.matrix(clutch_timed[, valid_pista])
      p_mat[is.na(p_mat)] <- 0
      clutch_sec <- colSums(p_mat * clutch_timed$time_diff, na.rm = TRUE)

      clutch_min_df <- data.frame(
        col     = names(clutch_sec),
        total_s = as.numeric(clutch_sec),
        stringsAsFactors = FALSE
      ) %>%
        filter(total_s > 0) %>%
        mutate(
          ident     = gsub("_pista$", "", col),
          licenseId = as.character(sub(".*_([0-9]+)$", "\\1", ident))
        ) %>%
        group_by(licenseId) %>%
        summarise(clutchMinutes = round(sum(total_s) / 60, 1), .groups = "drop")

      cat(sprintf("  Clutch minutes for %d players\n", nrow(clutch_min_df)))
      player_clutch <- player_clutch %>%
        left_join(clutch_min_df, by = "licenseId") %>%
        mutate(
          clutchMpg = if_else(!is.na(clutchMinutes) & games > 0,
                              round(clutchMinutes / games, 1), NA_real_)
        )
    }
  } else {
    cat("  No _pista columns found, clutch minutes skipped\n")
  }

  cat(sprintf("  Teams: %d  Players: %d\n", nrow(team_stats), nrow(player_clutch)))

  # attach stable club identities to compact output records
  team_stats$teamId <- validate_unique_team_seasons(
    team_stats$team,
    rep(season_id, nrow(team_stats)),
    context = sprintf("clutch teams for season %d", season_id)
  )
  player_clutch$teamId <- resolve_team_ids(
    player_clutch$team,
    rep(season_id, nrow(player_clutch)),
    context = sprintf("clutch players for season %d", season_id)
  )

  # ─── Build output ─────────────────────────────────────────────────────────
  teams_out <- lapply(seq_len(nrow(team_stats)), function(i) {
    r <- team_stats[i, ]
    list(
      team          = r$team,
      teamId        = r$teamId,
      games         = r$clutchGames,
      wins          = r$wins,
      losses        = r$losses,
      ptsScoredAvg  = r$ptsScoredAvg,
      ptsAllowedAvg = r$ptsAllowedAvg,
      plusMinus     = r$plusMinus,
      # shooting
      fgPct         = r$fgPct,
      fg2Pct        = r$fg2Pct,
      fg3Pct        = r$fg3Pct,
      ftPct         = r$ftPct,
      efgPct        = r$efgPct,
      fg2Apg        = r$fg2Apg,
      fg3Apg        = r$fg3Apg,
      ftApg         = r$ftApg,
      # rebounds
      orebAvg       = r$orebAvg,
      drebAvg       = r$drebAvg,
      rebAvg        = r$rebAvg,
      orbPct        = r$orbPct,
      drbPct        = r$drbPct,
      # misc counting
      apg           = r$apg,
      spg           = r$spg,
      bpg           = r$bpg,
      topg          = r$topg,
      # ratings
      ortg          = r$ortg,
      drtg          = r$drtg,
      netRtg        = r$netRtg,
      # rate stats
      astRate       = r$astRate,
      blkRate       = r$blkRate,
      stlRate       = r$stlRate,
      tovRate       = r$tovRate,
      astToRatio    = r$astToRatio,
      # opponent shooting
      opp_fgPct     = r$opp_fgPct,
      opp_fg2Pct    = r$opp_fg2Pct,
      opp_fg3Pct    = r$opp_fg3Pct,
      opp_ftPct     = r$opp_ftPct,
      opp_efgPct    = r$opp_efgPct,
      opp_fg2Apg    = r$opp_fg2Apg,
      opp_fg3Apg    = r$opp_fg3Apg,
      opp_ftApg     = r$opp_ftApg,
      opp_orebAvg   = r$opp_orebAvg,
      opp_drebAvg   = r$opp_drebAvg,
      opp_apg       = r$opp_apg,
      opp_spg       = r$opp_spg,
      opp_bpg       = r$opp_bpg,
      opp_topg      = r$opp_topg,
      opp_astRate   = r$opp_astRate,
      opp_blkRate   = r$opp_blkRate,
      opp_stlRate   = r$opp_stlRate,
      opp_tovRate   = r$opp_tovRate,
      opp_astToRatio = r$opp_astToRatio
    )
  })

  players_out <- lapply(seq_len(nrow(player_clutch)), function(i) {
    r <- player_clutch[i, ]
    list(
      nick      = r$nick,
      licenseId = r$licenseId,
      team      = r$team,
      teamId    = r$teamId,
      games     = r$games,
      wins      = r$wins,
      losses    = r$losses,
      clutchMin = if (!is.null(r$clutchMinutes) && !is.na(r$clutchMinutes)) r$clutchMinutes else NULL,
      clutchMpg = if (!is.null(r$clutchMpg)     && !is.na(r$clutchMpg))     r$clutchMpg     else NULL,
      # per-game counting stats
      pts       = r$ptsG,
      reb       = r$rebG,
      oreb      = r$orebG,
      dreb      = r$drebG,
      ast       = r$astG,
      stl       = r$stlG,
      blk       = r$blkG,
      tov       = r$tovG,
      fouls     = r$foulsG,
      # shooting totals (used to derive per-game and percentages in frontend)
      fg2M      = r$fg2M,
      fg2A      = r$fg2A,
      fg3M      = r$fg3M,
      fg3A      = r$fg3A,
      ftM       = r$ftM,
      ftA       = r$ftA,
      # shooting percentages
      fgPct     = r$fgPct,
      fg2Pct    = r$fg2Pct,
      fg3Pct    = r$fg3Pct,
      ftPct     = r$ftPct,
      efgPct    = r$efgPct,
      # raw counting totals for Absolutos tab
      ptsT   = r$pts,
      rebT   = r$reb,
      orebT  = r$oreb,
      drebT  = r$dreb,
      astT   = r$ast,
      stlT   = r$stl,
      blkT   = r$blk,
      tovT   = r$tov,
      foulsT = r$fouls
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
