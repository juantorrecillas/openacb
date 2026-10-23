# focused regressions for player-stat percentile and stint helpers

source(file.path("etl", "07_player_stats.R"))

percentiles <- midrank_percentile(
  c(0, 1, 2, NA_real_),
  c(0, 0, 1, 2)
)
inverse_percentiles <- midrank_percentile(
  c(0, 1, 2),
  c(0, 0, 1, 2),
  lower_is_better = TRUE
)

stopifnot(identical(percentiles, c(25, 62.5, 87.5, NA_real_)))
stopifnot(identical(inverse_percentiles, c(75, 37.5, 12.5)))

toy_events <- data.frame(
  id_match = c(1, 1, 2, 2),
  team = c("old_team", "opponent", "new_team", "opponent"),
  player_1_pista = c(1, 1, 1, 1)
)

old_stint <- filter_player_stint_events(
  toy_events,
  "player_1_pista",
  match_ids = 1
)

stopifnot(nrow(old_stint) == 2)
stopifnot(identical(unique(old_stint$id_match), 1))
stopifnot(!"new_team" %in% old_stint$team)

# game score: hollinger's weights on a toy season line
toy_line <- data.frame(points = 20, fgm = 8, fga = 15, ftm = 4, fta = 5, oreb = 2, dreb = 6,
                       steals = 1, assists = 4, blocks = 1, fouls = 3, turnovers = 2, games = 1)
stopifnot(isTRUE(all.equal(add_game_score(toy_line)$gmsc, 16.8)))

# win shares: replacement levels keep a team of average players at half its games
stopifnot(isTRUE(all.equal((1 - WS_OFF_REPLACEMENT) + (WS_DEF_REPLACEMENT - 1), WS_MARGINAL_COEF / 2)))

# per: rescaled so the minutes-weighted mean is the league average
toy_league <- data.frame(ast_factor = 0.6, vop = 1.0, drb_share = 0.7,
                         ftm = 100, fta = 140, pf = 120, pace = 72)
toy_players <- data.frame(
  fgm3 = c(10, 2), assists = c(30, 5), fgm = c(60, 40), ftm = c(20, 30), fta = c(25, 40),
  fga = c(130, 70), turnovers = c(20, 10), dreb = c(40, 80), oreb = c(10, 40),
  steals = c(10, 5), blocks = c(2, 20), fouls = c(30, 50), total_minutes = c(600, 500),
  tm_ast = 500, tm_fgm = 900, tm_pace = c(70, 74)
)
toy_per <- add_per(toy_players, toy_league)$per
stopifnot(isTRUE(all.equal(
  sum(toy_per * toy_players$total_minutes) / sum(toy_players$total_minutes),
  PER_LEAGUE_AVERAGE
)))

cat("player stats helper regressions passed\n")
