# =============================================================================
# ACB Basketball Analytics Pipeline - Player Impact Module (RAPM + Box Prior)
# =============================================================================
# LEBRON/EPM-style impact metric estimated from play-by-play stints:
# - stints: stretches with the same 10 players on court
# - luck adjustment: 3pt and ft points use the shooter's regressed percentage
# - box prior: on-court box-score profile, with weights estimated in the model
# - player effects are ridge-penalized toward that prior
# All seasons are pooled in one model (player-season units), so this is a
# cross-season step. Needs PbP_adjustedData{year}.Rds and PlayerStats{year}.Rds.
# Usage: calculate_player_impact() or calculate_player_impact(2022:2027)
# =============================================================================

library(dplyr)
library(Matrix)

# luck adjustment: pseudo-attempts that pull each shooter toward the league rate
LUCK_FG3_PRIOR_ATTEMPTS <- 250
LUCK_FT_PRIOR_ATTEMPTS  <- 50

# box prior: pseudo-minutes that pull low-minute profiles toward the league mean
BOX_PRIOR_MINUTES <- 250

# box-score profile behind the prior (built from PlayerStats{year} columns)
BOX_OFF_VARS <- c("usg", "ts_x_usg", "ast_pct", "tov_pct", "orb_pct", "three_rate")
BOX_DEF_VARS <- c("drb_pct", "stl_pct", "blk_pct", "fouls_per40")

# ridge penalty grid (in possessions) and game-level cross-validation
IMPACT_LAMBDA_GRID <- c(250, 500, 1000, 2000, 4000, 8000, 16000)
IMPACT_CV_FOLDS    <- 5
IMPACT_SEED        <- 42

# a penalty this large sets every player effect to zero (season + home baseline)
IMPACT_BASELINE_LAMBDA <- 1e9

# small ridge on unpenalized columns, for numerical stability only
IMPACT_FIXED_RIDGE <- 1e-6

# minimum possessions for the console leaderboard
IMPACT_REPORT_MIN_POSS <- 1500

# events that do not fix the lineup of a free throw (subs often happen before fts)
FT_LINEUP_SKIP_TYPES <- c("Entra Pista", "Sale Pista", "Tiempo Muerto")

# shrink a rate toward a prior with pseudo-observations
regressed_rate <- function(made, attempts, prior_rate, prior_attempts) {
  (made + prior_rate * prior_attempts) / (attempts + prior_attempts)
}

# split "id-id-id-id-id" lineup keys into player-season unit keys
lineup_unit_keys <- function(lineup_keys, seasons) {
  paste(rep(seasons, each = 5), unlist(strsplit(lineup_keys, "-", fixed = TRUE)))
}

#' build offense-vs-defense stints for one season
#'
#' one row per game, offensive five, defensive five and home flag, with
#' actual points, luck-adjusted points and estimated possessions.
#'
#' @param season_id integer year (e.g., 2026 for 2025-2026 season)
#' @param data_dir base directory for data (default: "./data")
#' @return data frame of stints
#'
build_impact_stints <- function(season_id, data_dir = "./data") {

  rds_file <- file.path(data_dir, "processed", paste0("PbP_adjustedData", season_id, ".Rds"))
  if (!file.exists(rds_file)) {
    stop("Adjusted PBP data not found: ", rds_file,
         "\nRun create_pbp_variables(", season_id, ") first.")
  }

  cat("→ Building stints for", season_id, "...\n")
  df <- readRDS(rds_file)

  # shooter 3pt% and ft% regressed to the league rate (luck adjustment)
  lg_fg3_pct <- sum(df$T3A, na.rm = TRUE) / sum(df$T3I, na.rm = TRUE)
  lg_ft_pct <- sum(df$T1A, na.rm = TRUE) / sum(df$T1I, na.rm = TRUE)

  shooters <- df %>%
    filter(!is.na(license.id)) %>%
    group_by(license.id) %>%
    summarise(
      fg3m = sum(T3A, na.rm = TRUE),
      fg3a = sum(T3I, na.rm = TRUE),
      ftm = sum(T1A, na.rm = TRUE),
      fta = sum(T1I, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      exp_fg3_pct = regressed_rate(fg3m, fg3a, lg_fg3_pct, LUCK_FG3_PRIOR_ATTEMPTS),
      exp_ft_pct = regressed_rate(ftm, fta, lg_ft_pct, LUCK_FT_PRIOR_ATTEMPTS)
    ) %>%
    select(license.id, exp_fg3_pct, exp_ft_pct)

  # home team of each game
  home_teams <- df %>%
    filter(local %in% TRUE, !is.na(team.team_actual_name)) %>%
    distinct(id_match, home_team = team.team_actual_name) %>%
    group_by(id_match) %>%
    slice(1) %>%
    ungroup()

  # team of each player in each game (players can change teams mid-season)
  player_teams <- df %>%
    filter(!is.na(license.id), !is.na(team.team_actual_name)) %>%
    distinct(id_match, license_id = license.id, player_team = team.team_actual_name) %>%
    group_by(id_match, license_id) %>%
    slice(1) %>%
    ungroup()

  # free throws take the lineup of the last event before them that is not a
  # sub, timeout or another free throw (the foul), like the ft trip itself
  row_idx <- seq_len(nrow(df))
  fixes_lineup <- !(df$T1I %in% 1) & !(df$type.description %in% FT_LINEUP_SKIP_TYPES)
  df$lineup_row <- cummax(ifelse(fixes_lineup, row_idx, 0L))
  df$lineup_row <- ifelse(df$lineup_row > 0 & df$id_match[pmax(df$lineup_row, 1L)] == df$id_match,
                          df$lineup_row, row_idx)

  # keep only events that move points or possessions
  events <- df %>%
    filter(!is.na(team.team_actual_name),
           T2I + T3I + T1I + FT_trip + perdida + reb_of > 0) %>%
    mutate(event = row_number())

  # on-court players at each event, labelled offense or defense
  pista_cols <- grep("_pista$", names(df), value = TRUE)
  pista_ids <- as.integer(sub(".*_([0-9]+)_pista$", "\\1", pista_cols))
  pista_matrix <- as.matrix(df[events$lineup_row, pista_cols])
  rm(df)

  on_court_idx <- which(pista_matrix == 1, arr.ind = TRUE)
  on_court <- data.frame(
    event = on_court_idx[, "row"],
    license_id = pista_ids[on_court_idx[, "col"]]
  ) %>%
    mutate(
      id_match = events$id_match[event],
      off_team = events$team.team_actual_name[event]
    ) %>%
    left_join(player_teams, by = c("id_match", "license_id")) %>%
    filter(!is.na(player_team)) %>%
    mutate(side = ifelse(player_team == off_team, "off", "def"))

  lineups <- on_court %>%
    arrange(event, side, license_id) %>%
    group_by(event, side) %>%
    summarise(key = paste(license_id, collapse = "-"), n_players = n(), .groups = "drop")

  off_keys <- lineups %>% filter(side == "off") %>% select(event, off_key = key, n_off = n_players)
  def_keys <- lineups %>% filter(side == "def") %>% select(event, def_key = key, n_def = n_players)

  # points (actual and luck-adjusted) and possessions per event
  events <- events %>%
    select(event, id_match, team.team_actual_name, license.id,
           puntos, T2A, T3I, T1I, T2I, FT_trip, perdida, reb_of) %>%
    left_join(off_keys, by = "event") %>%
    left_join(def_keys, by = "event") %>%
    left_join(home_teams, by = "id_match") %>%
    left_join(shooters, by = "license.id") %>%
    mutate(
      home = as.integer(team.team_actual_name == home_team),
      points_adj = 2 * T2A +
        3 * T3I * coalesce(exp_fg3_pct, lg_fg3_pct) +
        T1I * coalesce(exp_ft_pct, lg_ft_pct),
      poss = T2I + T3I + FT_trip + perdida - reb_of
    )

  # only events with exactly five players per side
  valid <- events$n_off %in% 5 & events$n_def %in% 5 & !is.na(events$home)
  cat(sprintf("  Events: %s | with 5v5 lineups: %.2f%%\n",
              format(nrow(events), big.mark = ","), mean(valid) * 100))

  stints <- events[valid, ] %>%
    group_by(id_match, off_key, def_key, home) %>%
    summarise(
      points = sum(puntos, na.rm = TRUE),
      points_adj = sum(points_adj, na.rm = TRUE),
      poss = sum(poss, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(season = season_id)

  cat(sprintf("  Stints: %s | points: %s | possessions: %s\n",
              format(nrow(stints), big.mark = ","),
              format(sum(stints$points), big.mark = ","),
              format(sum(stints$poss), big.mark = ",")))

  stints
}

#' load the centred box-score profile of every player-season
#'
#' rates come from PlayerStats{year}; players with several teams are
#' minutes-weighted, profiles are centred within position, and low-minute
#' players are pulled toward their position mean.
#'
#' @param season_id integer year
#' @param data_dir base directory for data (default: "./data")
#' @return data frame with one row per license_id and season
#'
load_box_profiles <- function(season_id, data_dir = "./data") {

  stats_file <- file.path(data_dir, "processed", paste0("PlayerStats", season_id, ".Rds"))
  if (!file.exists(stats_file)) {
    stop("Player stats not found: ", stats_file,
         "\nRun calculate_player_stats(", season_id, ") first.")
  }
  stats <- readRDS(stats_file)

  # scoring efficiency weighted by volume, relative to the league
  lg_ts <- weighted.mean(stats$ts, stats$fga + 0.44 * stats$fta, na.rm = TRUE)

  profiles <- stats %>%
    mutate(
      ts_x_usg = (ts - lg_ts) * usg / 100,
      fouls_per40 = ifelse(total_minutes > 0, fouls / total_minutes * 40, NA_real_)
    ) %>%
    group_by(license_id) %>%
    summarise(
      player = first(player),
      player_full = first(player_full),
      position = first(position),
      team = team[which.max(total_minutes)],
      across(all_of(c(BOX_OFF_VARS, BOX_DEF_VARS)),
             ~ if (any(is.finite(.x) & total_minutes > 0)) {
               weighted.mean(.x[is.finite(.x)], total_minutes[is.finite(.x)])
             } else NA_real_),
      minutes = sum(total_minutes),
      .groups = "drop"
    )

  # every lineup mixes positions, so stints cannot tell a level shift by position
  # (e.g. rebounds of centres) from the season mean: centre within position,
  # on the minutes-weighted mean, and shrink by minutes played
  profiles %>%
    mutate(position_group = coalesce(position, "unknown")) %>%
    group_by(position_group) %>%
    mutate(across(all_of(c(BOX_OFF_VARS, BOX_DEF_VARS)), ~ {
      known <- is.finite(.x) & minutes > 0
      centred <- (.x - weighted.mean(.x[known], minutes[known])) * minutes / (minutes + BOX_PRIOR_MINUTES)
      ifelse(is.finite(centred), centred, 0)
    })) %>%
    ungroup() %>%
    select(-position_group) %>%
    mutate(season = season_id)
}

# sparse design: season effects, home, box sums, offensive (+1) and defensive (-1) units
build_impact_design <- function(stints, unit_keys, z_off, z_def, use_box = TRUE) {
  n <- nrow(stints)
  n_units <- length(unit_keys)
  rows <- rep(seq_len(n), each = 5)

  off_units <- match(lineup_unit_keys(stints$off_key, stints$season), unit_keys)
  def_units <- match(lineup_unit_keys(stints$def_key, stints$season), unit_keys)
  x_off <- sparseMatrix(i = rows, j = off_units, x = 1, dims = c(n, n_units))
  x_def <- sparseMatrix(i = rows, j = def_units, x = -1, dims = c(n, n_units))

  seasons <- sort(unique(stints$season))
  x_fixed <- cbind(
    sparseMatrix(i = seq_len(n), j = match(stints$season, seasons), x = 1,
                 dims = c(n, length(seasons))),
    Matrix(stints$home, ncol = 1, sparse = TRUE)
  )
  fixed_names <- c(paste0("season_", seasons), "home")

  # box prior enters as the sum of the five players' profiles (defense with minus sign)
  if (use_box) {
    x_fixed <- cbind(x_fixed,
                     Matrix(as.matrix(x_off %*% z_off), sparse = TRUE),
                     Matrix(as.matrix(x_def %*% z_def), sparse = TRUE))
    fixed_names <- c(fixed_names, paste0("boxoff_", BOX_OFF_VARS), paste0("boxdef_", BOX_DEF_VARS))
  }

  x <- cbind(x_fixed, x_off, x_def)
  colnames(x) <- c(fixed_names, paste0("off_", unit_keys), paste0("def_", unit_keys))

  list(
    x = x,
    x_off = x_off,
    x_def = x_def,
    penalty = c(rep(0, ncol(x_fixed)), rep(1, 2 * n_units)),
    n_units = n_units
  )
}

# weighted normal equations x'wx and x'wy (reused across the lambda grid)
ridge_normal_equations <- function(x, y, w) {
  xs <- Diagonal(x = sqrt(w)) %*% x
  list(xtx = crossprod(xs), xty = crossprod(xs, sqrt(w) * y))
}

# closed-form ridge: (x'wx + lambda * penalty) b = x'wy
solve_ridge <- function(normal_eq, penalty, lambda) {
  ridge <- Diagonal(x = ifelse(penalty > 0, lambda * penalty, IMPACT_FIXED_RIDGE))
  beta <- as.vector(solve(normal_eq$xtx + ridge, normal_eq$xty))
  names(beta) <- colnames(normal_eq$xtx)
  beta
}

# game-level k-fold cross-validation: weighted out-of-sample mse per lambda
cross_validate_lambda <- function(design, y, w, game_ids, lambda_grid) {
  set.seed(IMPACT_SEED)
  games <- unique(game_ids)
  game_fold <- sample(rep(seq_len(IMPACT_CV_FOLDS), length.out = length(games)))
  fold <- game_fold[match(game_ids, games)]

  sse <- rep(0, length(lambda_grid))
  for (k in seq_len(IMPACT_CV_FOLDS)) {
    train <- fold != k
    normal_eq <- ridge_normal_equations(design$x[train, ], y[train], w[train])
    x_test <- design$x[!train, ]
    for (l in seq_along(lambda_grid)) {
      beta <- solve_ridge(normal_eq, design$penalty, lambda_grid[l])
      y_hat <- as.vector(x_test %*% beta)
      sse[l] <- sse[l] + sum(w[!train] * (y[!train] - y_hat)^2)
    }
    cat(sprintf("    fold %d/%d done\n", k, IMPACT_CV_FOLDS))
  }

  data.frame(lambda = lambda_grid, cv_mse = sse / sum(w))
}

#' estimate player impact (rapm and rapm + box prior) for several seasons
#'
#' @param season_ids vector of season ids pooled in the model (default: all available)
#' @param data_dir base directory for data (default: "./data")
#' @param config_path path to seasons.R config file
#' @return invisibly, data frame with one row per player-season
#'
calculate_player_impact <- function(season_ids = NULL,
                                    data_dir = "./data",
                                    config_path = "./config/seasons.R") {

  source(config_path)
  if (is.null(season_ids)) season_ids <- get_available_seasons()
  processed_dir <- file.path(data_dir, "processed")

  cat("\n", paste(rep("=", 60), collapse = ""), "\n")
  cat("Calculating Player Impact (RAPM + box prior):", paste(range(season_ids), collapse = "-"), "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")

  # ==========================================================================
  # BUILD STINTS AND BOX PROFILES
  # ==========================================================================

  stints <- bind_rows(lapply(season_ids, build_impact_stints, data_dir = data_dir))
  profiles <- bind_rows(lapply(season_ids, load_box_profiles, data_dir = data_dir))

  # weighted least squares needs positive possessions
  dropped_points <- sum(stints$points[stints$poss <= 0])
  stints <- stints %>% filter(poss > 0)
  cat(sprintf("\n  Model rows: %s stints (%.2f%% of points dropped with poss <= 0)\n",
              format(nrow(stints), big.mark = ","),
              100 * dropped_points / (dropped_points + sum(stints$points))))

  # response: luck-adjusted points per 100 possessions, weighted by possessions
  y <- 100 * stints$points_adj / stints$poss
  w <- stints$poss
  game_ids <- paste(stints$season, stints$id_match)

  # units: player-seasons on court in at least one stint
  units <- data.frame(
    unit_key = unique(c(lineup_unit_keys(stints$off_key, stints$season),
                        lineup_unit_keys(stints$def_key, stints$season))),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      season = as.integer(sub(" .*$", "", unit_key)),
      license_id = as.integer(sub("^.* ", "", unit_key))
    ) %>%
    left_join(profiles, by = c("season", "license_id"))

  z_off <- as.matrix(units[, BOX_OFF_VARS])
  z_def <- as.matrix(units[, BOX_DEF_VARS])
  z_off[!is.finite(z_off)] <- 0
  z_def[!is.finite(z_def)] <- 0
  cat("  Player-seasons:", nrow(units), "\n")

  # ==========================================================================
  # CROSS-VALIDATE PENALTIES
  # ==========================================================================

  design_rapm <- build_impact_design(stints, units$unit_key, z_off, z_def, use_box = FALSE)
  design_box <- build_impact_design(stints, units$unit_key, z_off, z_def, use_box = TRUE)

  cat("\n→ Cross-validating RAPM penalty...\n")
  cv_rapm <- cross_validate_lambda(design_rapm, y, w, game_ids,
                                   c(IMPACT_LAMBDA_GRID, IMPACT_BASELINE_LAMBDA))
  baseline_mse <- cv_rapm$cv_mse[cv_rapm$lambda == IMPACT_BASELINE_LAMBDA]
  cv_rapm <- cv_rapm %>% filter(lambda != IMPACT_BASELINE_LAMBDA)

  cat("→ Cross-validating RAPM + box prior penalty...\n")
  cv_box <- cross_validate_lambda(design_box, y, w, game_ids, IMPACT_LAMBDA_GRID)

  lambda_rapm <- cv_rapm$lambda[which.min(cv_rapm$cv_mse)]
  lambda_box <- cv_box$lambda[which.min(cv_box$cv_mse)]

  cat(sprintf("\n  Out-of-sample MSE (per 100 poss)^2: baseline %.1f | RAPM %.1f (lambda %s) | RAPM + box %.1f (lambda %s)\n",
              baseline_mse, min(cv_rapm$cv_mse), lambda_rapm, min(cv_box$cv_mse), lambda_box))

  # ==========================================================================
  # FIT FINAL MODELS
  # ==========================================================================

  cat("→ Fitting final models on all stints...\n")
  beta_rapm <- solve_ridge(ridge_normal_equations(design_rapm$x, y, w), design_rapm$penalty, lambda_rapm)
  beta_box <- solve_ridge(ridge_normal_equations(design_box$x, y, w), design_box$penalty, lambda_box)

  box_weights <- data.frame(
    variable = c(BOX_OFF_VARS, BOX_DEF_VARS),
    side = c(rep("offense", length(BOX_OFF_VARS)), rep("defense", length(BOX_DEF_VARS))),
    weight = unname(beta_box[c(paste0("boxoff_", BOX_OFF_VARS), paste0("boxdef_", BOX_DEF_VARS))])
  )
  gamma_off <- box_weights$weight[box_weights$side == "offense"]
  gamma_def <- box_weights$weight[box_weights$side == "defense"]

  # unit-level effects (points per 100 possessions)
  results <- units %>%
    mutate(
      off_poss = as.vector(crossprod(design_box$x_off, w)),
      def_poss = -as.vector(crossprod(design_box$x_def, w)),
      o_box = as.vector(z_off %*% gamma_off),
      d_box = as.vector(z_def %*% gamma_def),
      o_impact = o_box + unname(beta_box[paste0("off_", unit_key)]),
      d_impact = d_box + unname(beta_box[paste0("def_", unit_key)]),
      o_rapm = unname(beta_rapm[paste0("off_", unit_key)]),
      d_rapm = unname(beta_rapm[paste0("def_", unit_key)])
    ) %>%
    # league-average player = 0 each season (the shift is absorbed by season effects)
    group_by(season) %>%
    mutate(
      across(c(o_box, o_impact, o_rapm), ~ .x - weighted.mean(.x, off_poss)),
      across(c(d_box, d_impact, d_rapm), ~ .x - weighted.mean(.x, def_poss))
    ) %>%
    ungroup() %>%
    mutate(
      box = o_box + d_box,
      impact = o_impact + d_impact,
      rapm = o_rapm + d_rapm
    ) %>%
    select(license_id, player, player_full, position, team, season, minutes, off_poss, def_poss,
           o_impact, d_impact, impact, o_box, d_box, box, o_rapm, d_rapm, rapm) %>%
    arrange(season, desc(impact))

  # ==========================================================================
  # SAVE
  # ==========================================================================

  for (sid in season_ids) {
    season_results <- results %>% filter(season == sid)
    output_file <- file.path(processed_dir, paste0("PlayerImpact", sid, ".csv"))
    write.csv(season_results, output_file, row.names = FALSE, fileEncoding = "UTF-8")
    saveRDS(season_results, file.path(processed_dir, paste0("PlayerImpact", sid, ".Rds")))
  }
  cat("→ Saved PlayerImpact{year}.csv/.Rds for", length(season_ids), "seasons\n")

  model_summary <- list(
    seasons = season_ids,
    n_stints = nrow(stints),
    n_units = nrow(units),
    lambda_rapm = lambda_rapm,
    lambda_box = lambda_box,
    baseline_mse = baseline_mse,
    cv_rapm = cv_rapm,
    cv_box = cv_box,
    box_weights = box_weights,
    home_rapm = unname(beta_rapm["home"]),
    home_box = unname(beta_box["home"])
  )
  saveRDS(model_summary, file.path(processed_dir, "PlayerImpact_model.Rds"))
  write.csv(box_weights, file.path(processed_dir, "PlayerImpact_box_weights.csv"), row.names = FALSE)

  # report
  cat("\n✓ Player impact complete!\n")
  cat(sprintf("  Home advantage: %.2f points per 100 possessions\n", model_summary$home_box))
  cat("  Box prior weights:\n")
  for (i in seq_len(nrow(box_weights))) {
    cat(sprintf("    %-8s %-12s %7.3f\n", box_weights$side[i], box_weights$variable[i], box_weights$weight[i]))
  }

  # leaderboard for the latest completed season (in-progress seasons are too short)
  complete_seasons <- intersect(season_ids, SEASONS$season_id[SEASONS$is_complete %in% TRUE])
  report_season <- if (length(complete_seasons) > 0) max(complete_seasons) else max(season_ids)
  top10 <- results %>%
    filter(season == report_season, off_poss + def_poss >= IMPACT_REPORT_MIN_POSS) %>%
    head(10)
  cat(sprintf("\n  Top 10 impact, %d (%d+ possessions):\n", report_season, IMPACT_REPORT_MIN_POSS))
  for (i in seq_len(nrow(top10))) {
    cat(sprintf("    %2d. %-22s %-24s %+5.1f (O %+5.1f, D %+5.1f)\n",
                i, top10$player[i], top10$team[i], top10$impact[i],
                top10$o_impact[i], top10$d_impact[i]))
  }

  invisible(results)
}

cat("✓ Player Impact module loaded\n")
cat("  Usage: calculate_player_impact() or calculate_player_impact(2022:2027)\n")
