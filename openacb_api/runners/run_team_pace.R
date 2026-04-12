setwd("C:/Juan/Personal/ACB/OpenACB2.0/openacb_api")

source("config/seasons.R")
source("etl/09_team_pace.R")

season_ids <- get_available_seasons()
cat(sprintf("\nRegenerating team pace for seasons: %s\n", paste(season_ids, collapse = ", ")))

for (sid in season_ids) {
  tryCatch(
    generate_team_pace(sid),
    error = function(e) cat(sprintf("  ERROR season %d: %s\n", sid, e$message))
  )
}

cat("\nAll done.\n")
