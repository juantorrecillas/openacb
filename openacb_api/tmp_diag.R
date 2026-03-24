ps <- read.csv("C:/Juan/Personal/ACB/OpenACB2.0/openacb_api/data/processed/PlayerStats2017.csv",
               stringsAsFactors = FALSE)

# Find players with partial name match
targets <- c("Bargnani", "Antetokounmpo", "Antetokounmp")
for (t in targets) {
  rows <- ps[grepl(t, ps$player, ignore.case = TRUE), ]
  if (nrow(rows) > 0) {
    cat("\n---", t, "---\n")
    print(rows[, c("player", "license_id", "games", "total_minutes", "qualified", "position",
                   "ppg_pct", "ppg_pos_pct")])
  }
}

# Also summarise: how many per position, how many qualified per position
cat("\n--- Qualified players per position ---\n")
print(table(ps$position[ps$qualified == TRUE], useNA = "ifany"))
cat("\n--- All players per position ---\n")
print(table(ps$position, useNA = "ifany"))
