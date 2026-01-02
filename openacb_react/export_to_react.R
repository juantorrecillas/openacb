#' Export Data for OpenACB React App
#' 
#' This script reads your processed R data files and converts them to JSON
#' for the React frontend. Run this after updating your data with the main pipeline.
#'
#' Usage: 
#'   1. Open this file in RStudio
#'   2. Update the paths below if needed
#'   3. Click "Source" or press Ctrl+Shift+S
#'
#' Or from console: source("export_to_react.R")

# Install jsonlite if needed
if (!require(jsonlite)) {
  install.packages("jsonlite")
  library(jsonlite)
}

# ============================================================================
# CONFIGURATION - Update these paths to match YOUR setup!
# ============================================================================

# Where your Shiny app data files are (ShotChartData, TeamAdvancedStats, etc.)
# Updated to use the actual project structure
SHINY_DATA_DIR <- "../openacb_api/data/processed"

# Where the React app is located
REACT_APP_DIR <- "../openacb_react/"

# Which seasons to include (add/remove as needed)
SEASONS <- c(2021, 2022, 2023, 2024, 2025, 2026)

# ============================================================================
# Don't edit below this line unless you know what you're doing :)
# ============================================================================

# ============================================================================
# EXPORT FUNCTIONS
# ============================================================================

export_shot_data <- function() {
  cat("Exporting shot chart data (per-season files for lazy loading)...\n")

  total_shots <- 0

  for (year in SEASONS) {
    # Try different naming patterns
    patterns <- c(
      paste0("ShotChartData", substr(year, 3, 4), ".csv"),
      paste0("ShotChartData", year, ".csv")
    )

    for (pattern in patterns) {
      file_path <- file.path(SHINY_DATA_DIR, pattern)
      if (file.exists(file_path)) {
        shots <- read.csv(file_path, encoding = "UTF-8", stringsAsFactors = FALSE)

        # Select and rename columns for the React app
        shots_export <- data.frame(
          id = seq_len(nrow(shots)),
          matchId = shots$id_match,
          player = shots$license.licenseNick,
          playerFull = shots$license.licenseStr15,
          team = shots$team.team_actual_name,
          opponent = shots$opponent,
          period = shots$period,
          minute = shots$minute,
          x = shots$posX_res,
          y = shots$posY_res,
          distance = round(shots$distance, 2),
          made = shots$made_numeric == 1,
          points = shots$points,
          zone = shots$zoned,
          shotType = shots$type.description,
          season = year
        )

        # Export to separate file per season
        output_file <- file.path(REACT_APP_DIR, "public/data", paste0("shots-", year, ".json"))
        dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
        write_json(shots_export, output_file, pretty = TRUE, auto_unbox = TRUE)

        total_shots <- total_shots + nrow(shots_export)
        cat(sprintf("  - Exported %d shots for season %d to shots-%d.json\n", nrow(shots_export), year, year))
        break
      }
    }
  }

  if (total_shots > 0) {
    cat(sprintf("  ✓ Exported %d total shots across %d season files\n\n", total_shots, length(SEASONS)))
  } else {
    cat("  ⚠ No shot data found. Check your SHINY_DATA_DIR path.\n\n")
  }
}

export_team_data <- function() {
  cat("Exporting team statistics...\n")
  
  all_teams <- data.frame()
  
  for (year in SEASONS) {
    patterns <- c(
      paste0("TeamAdvancedStats", year, "Clean.csv"),
      paste0("TeamAdvancedStats", year, ".csv")
    )
    
    for (pattern in patterns) {
      file_path <- file.path(SHINY_DATA_DIR, pattern)
      if (file.exists(file_path)) {
        teams <- read.csv(file_path, encoding = "UTF-8", stringsAsFactors = FALSE)
        teams$season <- year
        all_teams <- rbind(all_teams, teams)
        cat(sprintf("  - Loaded %d teams from %s\n", nrow(teams), pattern))
        break
      }
    }
  }
  
  if (nrow(all_teams) > 0) {
    # Helper function for safe value extraction
    safe_val <- function(x, digits = 1, default = 0) {
      if (is.null(x) || is.na(x)) return(default)
      round(as.numeric(x), digits)
    }

    teams_export <- lapply(seq_len(nrow(all_teams)), function(i) {
      t <- all_teams[i, ]
      list(
        team = t$team.team_actual_name,
        season = t$season,
        games = t$ngames,

        # Team boxscore per-game stats
        ppg = safe_val(t$ppg, 1),
        rpg = safe_val(t$rpg, 1),
        orebpg = safe_val(t$orebpg, 1),
        drebpg = safe_val(t$drebpg, 1),
        apg = safe_val(t$apg, 1),
        spg = safe_val(t$spg, 1),
        bpg = safe_val(t$bpg, 1),
        topg = safe_val(t$topg, 1),
        fpg = safe_val(t$fpg, 1),
        fgmPg = safe_val(t$fgm_pg, 1),
        fgaPg = safe_val(t$fga_pg, 1),
        fg3mPg = safe_val(t$fg3m_pg, 1),
        fg3aPg = safe_val(t$fg3a_pg, 1),
        ftmPg = safe_val(t$ftm_pg, 1),
        ftaPg = safe_val(t$fta_pg, 1),
        fgPct = safe_val(t$fg_pct, 1),
        ftPct = safe_val(t$ft_pct, 1),
        pace = safe_val(t$pace, 1),

        # Team advanced stats
        ortg = safe_val(t$oer, 1),
        drtg = safe_val(t$der, 1),
        netRtg = safe_val(t$oer - t$der, 1),
        efg = t$efg,
        ts = t$ts,
        threePct = t$threefg,
        threeRate = t$threeatt_rate,
        astRate = t$S_assist,
        tovRate = t$S_Tov,
        orbPct = t$S_OffReb,
        drbPct = t$S_DefReb,
        ftRate = t$FT_rate,
        stlRate = t$S_steal,
        blkRate = t$S_blocks,

        # Opponent boxscore per-game stats
        opp_ppg = safe_val(t$opp_ppg, 1),
        opp_rpg = safe_val(t$opp_rpg, 1),
        opp_orebpg = safe_val(t$opp_orebpg, 1),
        opp_drebpg = safe_val(t$opp_drebpg, 1),
        opp_apg = safe_val(t$opp_apg, 1),
        opp_spg = safe_val(t$opp_spg, 1),
        opp_bpg = safe_val(t$opp_bpg, 1),
        opp_topg = safe_val(t$opp_topg, 1),
        opp_fpg = safe_val(t$opp_fpg, 1),
        opp_fgmPg = safe_val(t$opp_fgm_pg, 1),
        opp_fgaPg = safe_val(t$opp_fga_pg, 1),
        opp_fg3mPg = safe_val(t$opp_fg3m_pg, 1),
        opp_fg3aPg = safe_val(t$opp_fg3a_pg, 1),
        opp_ftmPg = safe_val(t$opp_ftm_pg, 1),
        opp_ftaPg = safe_val(t$opp_fta_pg, 1),
        opp_fgPct = safe_val(t$opp_fg_pct, 1),
        opp_ftPct = safe_val(t$opp_ft_pct, 1),

        # Opponent advanced stats
        opp_ortg = safe_val(t$oer_opponent, 1),
        opp_drtg = safe_val(t$der_opponent, 1),
        opp_efg = t$efg_opponent,
        opp_ts = t$ts_opponent,
        opp_threePct = t$threefg_opponent,
        opp_threeRate = t$threeatt_rate_opponent,
        opp_astRate = t$S_assist_opponent,
        opp_tovRate = t$S_Tov_opponent,
        opp_orbPct = t$S_OffReb_opponent,
        opp_drbPct = t$S_DefReb_opponent,
        opp_ftRate = t$FT_rate_opponent,
        opp_stlRate = t$S_steal_opponent,
        opp_blkRate = t$S_blocks_opponent
      )
    })
    
    # Remove any with NA team names
    teams_export <- Filter(function(x) !is.na(x$team) && x$team != "NA", teams_export)
    
    output_file <- file.path(REACT_APP_DIR, "public/data/teams.json")
    write_json(teams_export, output_file, pretty = TRUE, auto_unbox = TRUE)
    cat(sprintf("  ✓ Exported %d team records\n\n", length(teams_export)))
  } else {
    cat("  ⚠ No team data found. Check your SHINY_DATA_DIR path.\n\n")
  }
}

export_player_data <- function() {
  cat("Exporting player statistics...\n")

  all_players <- data.frame()

  for (year in SEASONS) {
    # Try different naming patterns
    patterns <- c(
      paste0("PlayerStats", year, ".csv"),
      paste0("PlayerStats", year, ".Rds")
    )

    for (pattern in patterns) {
      file_path <- file.path(SHINY_DATA_DIR, pattern)
      if (file.exists(file_path)) {
        if (grepl("\\.Rds$", pattern)) {
          players <- readRDS(file_path)
        } else {
          players <- read.csv(file_path, encoding = "UTF-8", stringsAsFactors = FALSE)
        }
        all_players <- rbind(all_players, players)
        cat(sprintf("  - Loaded %d players from %s\n", nrow(players), pattern))
        break
      }
    }
  }

  if (nrow(all_players) > 0) {
    players_export <- lapply(seq_len(nrow(all_players)), function(i) {
      p <- all_players[i, ]

      # Helper function for safe value extraction
      safe_val <- function(x, digits = 1, default = 0) {
        if (is.null(x) || is.na(x)) return(default)
        round(as.numeric(x), digits)
      }

      list(
        playerId = p$player_id,
        licenseId = p$license_id,
        player = p$player,
        playerFull = p$player_full,
        season = p$season,
        team = p$team,
        games = safe_val(p$games, 0),

        # Minutes
        totalMinutes = safe_val(p$total_minutes, 1),
        mpg = safe_val(p$mpg, 1),

        # Qualified flag (meets games/minutes threshold for percentile calculation)
        qualified = ifelse(is.null(p$qualified) || is.na(p$qualified), TRUE, as.logical(p$qualified)),

        # Basic totals
        points = safe_val(p$points, 0),
        rebounds = safe_val(p$rebounds, 0),
        oreb = safe_val(p$oreb, 0),
        dreb = safe_val(p$dreb, 0),
        assists = safe_val(p$assists, 0),
        steals = safe_val(p$steals, 0),
        blocks = safe_val(p$blocks, 0),
        turnovers = safe_val(p$turnovers, 0),
        fouls = safe_val(p$fouls, 0),

        # Shooting totals
        fgm = safe_val(p$fgm, 0),
        fga = safe_val(p$fga, 0),
        fgm2 = safe_val(p$fgm2, 0),
        fga2 = safe_val(p$fga2, 0),
        fgm3 = safe_val(p$fgm3, 0),
        fga3 = safe_val(p$fga3, 0),
        ftm = safe_val(p$ftm, 0),
        fta = safe_val(p$fta, 0),

        # Per game stats
        ppg = safe_val(p$ppg, 1),
        rpg = safe_val(p$rpg, 1),
        orebpg = safe_val(p$orebpg, 1),
        drebpg = safe_val(p$drebpg, 1),
        apg = safe_val(p$apg, 1),
        spg = safe_val(p$spg, 1),
        bpg = safe_val(p$bpg, 1),
        topg = safe_val(p$topg, 1),
        fpg = safe_val(p$fpg, 1),

        # Shooting percentages
        fgPct = safe_val(p$fg_pct, 1),
        fg2Pct = safe_val(p$fg2_pct, 1),
        fg3Pct = safe_val(p$fg3_pct, 1),
        ftPct = safe_val(p$ft_pct, 1),

        # Advanced stats
        efg = safe_val(p$efg, 1),
        ts = safe_val(p$ts, 1),
        threeRate = safe_val(p$three_rate, 1),

        # Possessions
        possessions = safe_val(p$possessions, 0),
        possPg = safe_val(p$poss_pg, 1),

        # Offensive Rating
        ortg = safe_val(p$ortg, 1),

        # Usage
        usg = safe_val(p$usg, 1),

        # Advanced Rate Stats
        orbPct = safe_val(p$orb_pct, 1),
        drbPct = safe_val(p$drb_pct, 1),
        trbPct = safe_val(p$trb_pct, 1),
        astPct = safe_val(p$ast_pct, 1),
        stlPct = safe_val(p$stl_pct, 1),
        blkPct = safe_val(p$blk_pct, 1),
        tovPct = safe_val(p$tov_pct, 1),

        # Percentiles (all stats ending with _pct that are percentiles)
        ppgPct = safe_val(p$ppg_pct, 1),
        rpgPct = safe_val(p$rpg_pct, 1),
        orebpgPct = safe_val(p$orebpg_pct, 1),
        drebpgPct = safe_val(p$drebpg_pct, 1),
        apgPct = safe_val(p$apg_pct, 1),
        spgPct = safe_val(p$spg_pct, 1),
        bpgPct = safe_val(p$bpg_pct, 1),
        fpgPct = safe_val(p$fpg_pct, 1),
        topgPct = safe_val(p$topg_pct, 1),
        mpgPct = safe_val(p$mpg_pct, 1),
        fgPctPct = safe_val(p$fg_pct_pct, 1),
        fg3PctPct = safe_val(p$fg3_pct_pct, 1),
        ftPctPct = safe_val(p$ft_pct_pct, 1),
        efgPct = safe_val(p$efg_pct, 1),
        tsPct = safe_val(p$ts_pct, 1),
        ortgPct = safe_val(p$ortg_pct, 1),
        threeRatePct = safe_val(p$three_rate_pct, 1),
        possPgPct = safe_val(p$poss_pg_pct, 1),
        usgPct = safe_val(p$usg_pct, 1),

        # Advanced Rate Percentiles
        orbPctPct = safe_val(p$orb_pct_pct, 1),
        drbPctPct = safe_val(p$drb_pct_pct, 1),
        trbPctPct = safe_val(p$trb_pct_pct, 1),
        astPctPct = safe_val(p$ast_pct_pct, 1),
        stlPctPct = safe_val(p$stl_pct_pct, 1),
        blkPctPct = safe_val(p$blk_pct_pct, 1),
        tovPctPct = safe_val(p$tov_pct_pctile, 1),  # Note: uses pctile suffix for inverse

        # Zone Shooting Frequency (% of shots from each zone)
        freqRim = safe_val(p$freq_rim, 1),
        freqShortMid = safe_val(p$freq_short_mid, 1),
        freqLongMid = safe_val(p$freq_long_mid, 1),
        freqAllMid = safe_val(p$freq_all_mid, 1),
        freqCornerThree = safe_val(p$freq_corner_three, 1),
        freqNcThree = safe_val(p$freq_nc_three, 1),
        freqAllThree = safe_val(p$freq_all_three, 1),

        # Zone Shooting Accuracy (FG% per zone)
        fgpctRim = safe_val(p$fgpct_rim, 1),
        fgpctShortMid = safe_val(p$fgpct_short_mid, 1),
        fgpctLongMid = safe_val(p$fgpct_long_mid, 1),
        fgpctAllMid = safe_val(p$fgpct_all_mid, 1),
        fgpctCornerThree = safe_val(p$fgpct_corner_three, 1),
        fgpctNcThree = safe_val(p$fgpct_nc_three, 1),
        fgpctAllThree = safe_val(p$fgpct_all_three, 1),

        # Zone Attempts (for display as # in table)
        fgaRim = safe_val(p$fga_rim, 0),
        fgaShortMid = safe_val(p$fga_short_mid, 0),
        fgaLongMid = safe_val(p$fga_long_mid, 0),
        fgaAllMid = safe_val(p$fga_all_mid, 0),
        fgaCornerThree = safe_val(p$fga_corner_three, 0),
        fgaNcThree = safe_val(p$fga_nc_three, 0),
        fgaAllThree = safe_val(p$fga_all_three, 0),

        # Opponent Zone Shooting (Defensive Impact)
        # FG% allowed when player is ON court
        oppOnFgpctRim = safe_val(p$opp_on_fgpct_rim, 1),
        oppOnFgpctShortMid = safe_val(p$opp_on_fgpct_short_mid, 1),
        oppOnFgpctLongMid = safe_val(p$opp_on_fgpct_long_mid, 1),
        oppOnFgpctAllMid = safe_val(p$opp_on_fgpct_all_mid, 1),
        oppOnFgpctCornerThree = safe_val(p$opp_on_fgpct_corner_three, 1),
        oppOnFgpctNcThree = safe_val(p$opp_on_fgpct_nc_three, 1),
        oppOnFgpctAllThree = safe_val(p$opp_on_fgpct_all_three, 1),

        # Differential (ON - OFF, negative is good defense)
        oppDiffRim = safe_val(p$opp_diff_rim, 1),
        oppDiffShortMid = safe_val(p$opp_diff_short_mid, 1),
        oppDiffLongMid = safe_val(p$opp_diff_long_mid, 1),
        oppDiffAllMid = safe_val(p$opp_diff_all_mid, 1),
        oppDiffCornerThree = safe_val(p$opp_diff_corner_three, 1),
        oppDiffNcThree = safe_val(p$opp_diff_nc_three, 1),
        oppDiffAllThree = safe_val(p$opp_diff_all_three, 1),

        # Opponent attempts when player ON court
        oppFgaRim = safe_val(p$opp_fga_rim, 0),
        oppFgaShortMid = safe_val(p$opp_fga_short_mid, 0),
        oppFgaLongMid = safe_val(p$opp_fga_long_mid, 0),
        oppFgaAllMid = safe_val(p$opp_fga_all_mid, 0),
        oppFgaCornerThree = safe_val(p$opp_fga_corner_three, 0),
        oppFgaNcThree = safe_val(p$opp_fga_nc_three, 0),
        oppFgaAllThree = safe_val(p$opp_fga_all_three, 0)
      )
    })

    # Remove any with NA player names
    players_export <- Filter(function(x) !is.na(x$player) && x$player != "", players_export)

    output_file <- file.path(REACT_APP_DIR, "public/data/players.json")
    write_json(players_export, output_file, pretty = TRUE, auto_unbox = TRUE)
    cat(sprintf("  ✓ Exported %d player records\n\n", length(players_export)))
  } else {
    cat("  ⚠ No player data found. Run calculate_player_stats() first.\n\n")
  }
}

# ============================================================================
# MAIN
# ============================================================================

cat("\n========================================\n")
cat("OpenACB Data Export\n")
cat("========================================\n\n")

export_shot_data()
export_team_data()
export_player_data()

cat("\n========================================\n")
cat("✓ Export complete!\n")
cat("Open the React app to see your data.\n")
cat("========================================\n")
