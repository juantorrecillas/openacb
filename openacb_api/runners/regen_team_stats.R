# Regenerate team stats for all seasons (adds wins/losses)
setwd("C:/Juan/Personal/ACB/OpenACB2.0/openacb_api")

source("./config/seasons.R")
source("./etl/02_clean.R")
source("./etl/04_team_stats.R")

seasons <- c(2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026)

for (s in seasons) {
  cat("\n============================\n")
  cat("Processing season:", s, "\n")
  tryCatch({
    calculate_team_stats(s)
    cat("✓ Done:", s, "\n")
  }, error = function(e) {
    cat("✗ Error for season", s, ":", e$message, "\n")
  })
}

cat("\n============================\n")
cat("Exporting to React...\n")
source("./export_to_react.R")
cat("✓ Export complete!\n")
