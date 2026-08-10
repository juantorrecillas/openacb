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

cat("player stats helper regressions passed\n")
