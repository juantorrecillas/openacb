# Script to analyze possession patterns around different foul types
library(dplyr)

# Read one game file to analyze
df <- read.csv("data/raw/2025-2026/J1_CAZ_BKN_PBP.csv", stringsAsFactors = FALSE)

# Function to show context around specific foul types
show_foul_context <- function(df, foul_type, context_rows = 5) {
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("Foul Type:", foul_type, "\n")
  cat(paste(rep("=", 80), collapse = ""), "\n\n")

  # Find rows with this foul type
  foul_indices <- which(df$type.description == foul_type)

  if (length(foul_indices) == 0) {
    cat("No instances found\n")
    return(NULL)
  }

  cat("Found", length(foul_indices), "instances\n\n")

  # Show first 3 examples with context
  for (i in head(foul_indices, 3)) {
    cat("Example", which(foul_indices == i), "- Order:", df$order[i], "\n")
    cat(paste(rep("-", 80), collapse = ""), "\n")

    # Get context rows
    start_idx <- max(1, i - context_rows)
    end_idx <- min(nrow(df), i + context_rows)

    context_df <- df[start_idx:end_idx, c("order", "team.team_actual_name", "type.description", "period", "minute", "second")]

    # Mark the foul row
    context_df$marker <- ifelse(seq(start_idx, end_idx) == i, " >>> ", "     ")

    print(context_df[, c("marker", "order", "team.team_actual_name", "type.description")], row.names = FALSE)
    cat("\n")
  }
}

# Analyze different foul types
foul_types <- c(
  "Personal 2TL",
  "Personal 3TL",
  "Personal 1TL",
  "Técnica 1TL",
  "Tec. Coach 1TL",
  "Tec. Banq 1TL",
  "Antidep 2TL",
  "Personal TL Comp",
  "Antidep TL Comp",
  "Tec. Banq Comp"
)

for (ft in foul_types) {
  show_foul_context(df, ft, context_rows = 8)
}

# Also check for TL Comp variations
cat("\n\nSearching for all 'Comp' foul types:\n")
comp_fouls <- unique(df$type.description[grepl("Comp", df$type.description, ignore.case = TRUE)])
print(comp_fouls)
