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

# load the stable team identity registry
team_identity_config <- "./config/team_identities.R"
if (!file.exists(team_identity_config)) {
  team_identity_config <- "../openacb_api/config/team_identities.R"
}
source(team_identity_config, encoding = "UTF-8")

# ============================================================================
# CONFIGURATION - Update these paths to match YOUR setup!
# ============================================================================

# Where your Shiny app data files are (ShotChartData, TeamAdvancedStats, etc.)
# Updated to use the actual project structure
SHINY_DATA_DIR <- "../openacb_api/data/processed"

# Where the React app is located
REACT_APP_DIR <- "../openacb_react/"

# Which seasons to include (add/remove as needed)
SEASONS <- c(2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026)

# ============================================================================
# Don't edit below this line unless you know what you're doing :)
# ============================================================================

# ============================================================================
# EXPORT FUNCTIONS
# ============================================================================

export_team_identities <- function() {
  cat("Exporting stable team identities...\n")

  output_file <- file.path(REACT_APP_DIR, "public/data", "team-identities.json")
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  write_json(
    team_identities_for_json(),
    output_file,
    pretty = TRUE,
    auto_unbox = TRUE
  )

  cat(sprintf("  - Exported %d stable club identities\n\n", length(TEAM_IDENTITIES)))
}

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

        # validate aliases without duplicating ids into every large shot row
        resolve_team_ids(
          shots$team.team_actual_name,
          rep(year, nrow(shots)),
          context = sprintf("shot teams for season %d", year)
        )
        resolve_team_ids(
          shots$opponent,
          rep(year, nrow(shots)),
          context = sprintf("shot opponents for season %d", year)
        )

        # Select and rename columns for the React app
        shots_export <- data.frame(
          id = seq_len(nrow(shots)),
          matchId = shots$id_match,
          playerId = shots$license.id,
          player = shots$license.licenseStr15,
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
          competitionStage = shots$competition_stage,
          competitionRound = shots$competition_round,
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

export_player_names <- function() {
  cat("Exporting player display names...\n")

  player_names <- list()

  for (year in SEASONS) {
    paths <- c(
      file.path(SHINY_DATA_DIR, paste0("PbP_adjustedData", year, ".Rds")),
      file.path(SHINY_DATA_DIR, paste0("PbP_adjustedData", year, ".csv"))
    )
    source_path <- paths[file.exists(paths)][1]
    if (is.na(source_path)) next

    pbp <- if (grepl("\\.Rds$", source_path)) {
      readRDS(source_path)
    } else {
      read.csv(source_path, encoding = "UTF-8", stringsAsFactors = FALSE)
    }

    if (!all(c("license.id", "license.licenseStr15") %in% names(pbp))) next

    names_for_season <- unique(pbp[c("license.id", "license.licenseStr15")])
    for (i in seq_len(nrow(names_for_season))) {
      license_id <- names_for_season$license.id[i]
      display_name <- trimws(names_for_season$license.licenseStr15[i])
      if (!is.na(license_id) && !is.na(display_name) && nzchar(display_name)) {
        player_names[[as.character(license_id)]] <- display_name
      }
    }
  }

  output_file <- file.path(REACT_APP_DIR, "public/data", "player-names.json")
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  write_json(player_names, output_file, pretty = TRUE, auto_unbox = TRUE)
  cat(sprintf("  - Exported %d player display names\n\n", length(player_names)))
}

export_team_data <- function(competition_stages = NULL, output_name = "teams.json") {
  cat("Exporting team statistics...\n")
  
  all_teams <- data.frame()
  
  for (year in SEASONS) {
    stages <- if (is.null(competition_stages)) NA_character_ else competition_stages

    for (stage in stages) {
      patterns <- if (is.na(stage)) {
        c(
          paste0("TeamAdvancedStats", year, "Clean.csv"),
          paste0("TeamAdvancedStats", year, ".csv")
        )
      } else {
        paste0("TeamAdvancedStats", year, "_", stage, ".csv")
      }

      for (pattern in patterns) {
        file_path <- file.path(SHINY_DATA_DIR, pattern)
        if (file.exists(file_path)) {
          teams <- read.csv(file_path, encoding = "UTF-8", stringsAsFactors = FALSE)
          teams$season <- year
          if (!"competition_stage" %in% names(teams)) {
            teams$competition_stage <- if (is.na(stage)) "all" else stage
          }
          all_teams <- dplyr::bind_rows(all_teams, teams)
          cat(sprintf("  - Loaded %d teams from %s\n", nrow(teams), pattern))
          break
        }
      }
    }
  }
  
  if (nrow(all_teams) > 0) {
    all_teams$team_id <- validate_unique_team_seasons(
      all_teams$team.team_actual_name,
      all_teams$season,
      partitions = all_teams$competition_stage,
      context = sprintf("%s export", output_name)
    )

    # Helper function for safe value extraction
    safe_val <- function(x, digits = 1, default = 0) {
      if (is.null(x) || is.na(x)) return(default)
      round(as.numeric(x), digits)
    }

    teams_export <- lapply(seq_len(nrow(all_teams)), function(i) {
      t <- all_teams[i, ]
      list(
        team = t$team.team_actual_name,
        teamId = t$team_id,
        season = t$season,
        competitionStage = t$competition_stage,
        games = t$ngames,
        wins = safe_val(t$wins, 0),
        losses = safe_val(t$losses, 0),

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
        astToRatio = safe_val(t$ast_to_ratio, 2),
        # Context stats
        offTo = safe_val(t$off_to, 3),
        secondChance = safe_val(t$second_chance, 3),
        assistedFgm = safe_val(t$S_assisted_fgm, 3),
        assistedFgm2 = safe_val(t$S_assisted_fgm2, 3),
        assistedFgm3 = safe_val(t$S_assisted_fgm3, 3),

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
        opp_blkRate = t$S_blocks_opponent,
        opp_astToRatio = safe_val(t$ast_to_ratio_opponent, 2),
        # Opponent context stats
        opp_offTo = safe_val(t$off_to_opponent, 3),
        opp_secondChance = safe_val(t$second_chance_opponent, 3),
        opp_assistedFgm = safe_val(t$S_assisted_fgm_opponent, 3),
        opp_assistedFgm2 = safe_val(t$S_assisted_fgm2_opponent, 3),
        opp_assistedFgm3 = safe_val(t$S_assisted_fgm3_opponent, 3)
      )
    })
    
    # Remove any with NA team names
    teams_export <- Filter(function(x) !is.na(x$team) && x$team != "NA", teams_export)
    
    output_file <- file.path(REACT_APP_DIR, "public/data", output_name)
    write_json(teams_export, output_file, pretty = TRUE, auto_unbox = TRUE)
    cat(sprintf("  ✓ Exported %d team records\n\n", length(teams_export)))
  } else {
    cat("  ⚠ No team data found. Check your SHINY_DATA_DIR path.\n\n")
  }
}

load_all_player_data <- function(competition_stages = NULL) {
  cat("Loading player statistics...\n")

  all_players <- data.frame()

  for (year in SEASONS) {
    stages <- if (is.null(competition_stages)) NA_character_ else competition_stages

    for (stage in stages) {
      patterns <- if (is.na(stage)) {
        c(
          paste0("PlayerStats", year, ".csv"),
          paste0("PlayerStats", year, ".Rds")
        )
      } else {
        c(
          paste0("PlayerStats", year, "_", stage, ".csv"),
          paste0("PlayerStats", year, "_", stage, ".Rds")
        )
      }

      for (pattern in patterns) {
        file_path <- file.path(SHINY_DATA_DIR, pattern)
        if (file.exists(file_path)) {
          if (grepl("\\.Rds$", pattern)) {
            players <- readRDS(file_path)
          } else {
            players <- read.csv(file_path, encoding = "UTF-8", stringsAsFactors = FALSE)
          }
          if (!"competition_stage" %in% names(players)) {
            players$competition_stage <- if (is.na(stage)) "all" else stage
          }
          all_players <- dplyr::bind_rows(all_players, players)
          cat(sprintf("  - Loaded %d players from %s\n", nrow(players), pattern))
          break
        }
      }
    }
  }

  all_players
}

export_player_data <- function(all_players, output_name = "players.json") {
  cat("Exporting player statistics...\n")

  if (nrow(all_players) > 0) {
    all_players$team_id <- resolve_team_ids(
      all_players$team,
      all_players$season,
      context = sprintf("%s export", output_name)
    )

    players_export <- lapply(seq_len(nrow(all_players)), function(i) {
      p <- all_players[i, ]

      # Helper function for safe value extraction
      safe_val <- function(x, digits = 1, default = 0) {
        if (is.null(x) || is.na(x)) return(default)
        round(as.numeric(x), digits)
      }
      # Helper for percentile fields: returns NULL (not 0) when NA
      pct_val <- function(x, digits = 1) {
        if (is.null(x) || is.na(x)) return(NULL)
        round(as.numeric(x), digits)
      }
      text_val <- function(x) {
        if (is.null(x) || is.na(x)) return(NULL)
        value <- gsub(intToUtf8(160), " ", as.character(x), fixed = TRUE)
        value <- trimws(value)
        if (!nzchar(value)) return(NULL)
        value
      }

      list(
        playerId = p$player_id,
        licenseId = p$license_id,
        player = p$player,
        playerAbbrev = p$player_abbrev,
        playerFull = p$player_full,
        position = text_val(p$position),
        heightM = if (!is.null(p$height_m) && !is.na(p$height_m)) round(as.numeric(p$height_m), 2) else NULL,
        birthDate = text_val(p$birth_date),
        season = p$season,
        competitionStage = p$competition_stage,
        team = p$team,
        teamId = p$team_id,
        games = safe_val(p$games, 0),

        # Minutes
        totalMinutes = safe_val(p$total_minutes, 1),
        mpg = safe_val(p$mpg, 1),

        # Qualified flag (meets games/minutes threshold for percentile calculation)
        qualified = ifelse(is.null(p$qualified) || is.na(p$qualified), FALSE, as.logical(p$qualified)),

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
        fgPct = pct_val(p$fg_pct, 1),
        fg2Pct = pct_val(p$fg2_pct, 1),
        fg3Pct = pct_val(p$fg3_pct, 1),
        ftPct = pct_val(p$ft_pct, 1),

        # Advanced stats
        efg = pct_val(p$efg, 1),
        ts = pct_val(p$ts, 1),
        threeRate = pct_val(p$three_rate, 1),

        # Possessions
        possessions = safe_val(p$possessions, 0),
        possPg = safe_val(p$poss_pg, 1),

        # Offensive Rating
        ortg = pct_val(p$ortg, 1),

        # Usage
        usg = pct_val(p$usg, 1),

        # Advanced Rate Stats
        orbPct = pct_val(p$orb_pct, 1),
        drbPct = pct_val(p$drb_pct, 1),
        trbPct = pct_val(p$trb_pct, 1),
        astPct = pct_val(p$ast_pct, 1),
        stlPct = pct_val(p$stl_pct, 1),
        blkPct = pct_val(p$blk_pct, 1),
        tovPct = pct_val(p$tov_pct, 1),
        astToRatio = pct_val(p$ast_to_ratio, 2),
        # Context stats
        offTo = pct_val(p$off_to, 3),
        secondChance = pct_val(p$second_chance, 3),
        assistedFgm = pct_val(p$S_assisted_fgm, 3),
        assistedFgm2 = pct_val(p$S_assisted_fgm2, 3),
        assistedFgm3 = pct_val(p$S_assisted_fgm3, 3),

        # Percentiles — league-wide (NULL for unqualified players)
        ppgPct = pct_val(p$ppg_pct, 1),
        rpgPct = pct_val(p$rpg_pct, 1),
        orebpgPct = pct_val(p$orebpg_pct, 1),
        drebpgPct = pct_val(p$drebpg_pct, 1),
        apgPct = pct_val(p$apg_pct, 1),
        spgPct = pct_val(p$spg_pct, 1),
        bpgPct = pct_val(p$bpg_pct, 1),
        fpgPct = pct_val(p$fpg_pct, 1),
        topgPct = pct_val(p$topg_pct, 1),
        mpgPct = pct_val(p$mpg_pct, 1),
        fgPctPct = pct_val(p$fg_pct_pct, 1),
        fg3PctPct = pct_val(p$fg3_pct_pct, 1),
        ftPctPct = pct_val(p$ft_pct_pct, 1),
        efgPct = pct_val(p$efg_pct, 1),
        tsPct = pct_val(p$ts_pct, 1),
        ortgPct = pct_val(p$ortg_pct, 1),
        threeRatePct = pct_val(p$three_rate_pct, 1),
        possPgPct = pct_val(p$poss_pg_pct, 1),
        usgPct = pct_val(p$usg_pct, 1),

        # Advanced Rate Percentiles — league-wide
        orbPctPct = pct_val(p$orb_pct_pct, 1),
        drbPctPct = pct_val(p$drb_pct_pct, 1),
        trbPctPct = pct_val(p$trb_pct_pct, 1),
        astPctPct = pct_val(p$ast_pct_pct, 1),
        astToRatioPct = pct_val(p$ast_to_ratio_pct, 1),
        stlPctPct = pct_val(p$stl_pct_pct, 1),
        blkPctPct = pct_val(p$blk_pct_pct, 1),
        tovPctPct = pct_val(p$tov_pct_pctile, 1),

        # Percentiles — by position (NULL when not available)
        ppgPosPct = pct_val(p$ppg_pos_pct, 1),
        rpgPosPct = pct_val(p$rpg_pos_pct, 1),
        orebpgPosPct = pct_val(p$orebpg_pos_pct, 1),
        drebpgPosPct = pct_val(p$drebpg_pos_pct, 1),
        apgPosPct = pct_val(p$apg_pos_pct, 1),
        spgPosPct = pct_val(p$spg_pos_pct, 1),
        bpgPosPct = pct_val(p$bpg_pos_pct, 1),
        fpgPosPct = pct_val(p$fpg_pos_pct, 1),
        topgPosPct = pct_val(p$topg_pos_pct, 1),
        mpgPosPct = pct_val(p$mpg_pos_pct, 1),
        fgPctPosPct = pct_val(p$fg_pct_pos_pct, 1),
        fg3PctPosPct = pct_val(p$fg3_pct_pos_pct, 1),
        ftPctPosPct = pct_val(p$ft_pct_pos_pct, 1),
        efgPosPct = pct_val(p$efg_pos_pct, 1),
        tsPosPct = pct_val(p$ts_pos_pct, 1),
        ortgPosPct = pct_val(p$ortg_pos_pct, 1),
        threeRatePosPct = pct_val(p$three_rate_pos_pct, 1),
        possPgPosPct = pct_val(p$poss_pg_pos_pct, 1),
        usgPosPct = pct_val(p$usg_pos_pct, 1),

        # Advanced Rate Percentiles — by position
        orbPctPosPct = pct_val(p$orb_pct_pos_pct, 1),
        drbPctPosPct = pct_val(p$drb_pct_pos_pct, 1),
        trbPctPosPct = pct_val(p$trb_pct_pos_pct, 1),
        astPctPosPct = pct_val(p$ast_pct_pos_pct, 1),
        astToRatioPosPct = pct_val(p$ast_to_ratio_pos_pct, 1),
        stlPctPosPct = pct_val(p$stl_pct_pos_pct, 1),
        blkPctPosPct = pct_val(p$blk_pct_pos_pct, 1),
        tovPctPosPct = pct_val(p$tov_pct_pos_pctile, 1),

        # Zone Shooting Frequency (% of shots from each zone)
        # pct_val: returns NULL for NA so UI can show "N/D" for pre-2021 seasons
        freqRim = pct_val(p$freq_rim, 1),
        freqShortMid = pct_val(p$freq_short_mid, 1),
        freqLongMid = pct_val(p$freq_long_mid, 1),
        freqAllMid = pct_val(p$freq_all_mid, 1),
        freqCornerThree = pct_val(p$freq_corner_three, 1),
        freqNcThree = pct_val(p$freq_nc_three, 1),
        freqAllThree = pct_val(p$freq_all_three, 1),

        # Zone Shooting Accuracy (FG% per zone)
        fgpctRim = pct_val(p$fgpct_rim, 1),
        fgpctShortMid = pct_val(p$fgpct_short_mid, 1),
        fgpctLongMid = pct_val(p$fgpct_long_mid, 1),
        fgpctAllMid = pct_val(p$fgpct_all_mid, 1),
        fgpctCornerThree = pct_val(p$fgpct_corner_three, 1),
        fgpctNcThree = pct_val(p$fgpct_nc_three, 1),
        fgpctAllThree = pct_val(p$fgpct_all_three, 1),

        # Zone Attempts (for display as # in table)
        fgaRim = pct_val(p$fga_rim, 0),
        fgaShortMid = pct_val(p$fga_short_mid, 0),
        fgaLongMid = pct_val(p$fga_long_mid, 0),
        fgaAllMid = pct_val(p$fga_all_mid, 0),
        fgaCornerThree = pct_val(p$fga_corner_three, 0),
        fgaNcThree = pct_val(p$fga_nc_three, 0),
        fgaAllThree = pct_val(p$fga_all_three, 0),

        # Opponent Zone Shooting (Defensive Impact)
        # FG% allowed when player is ON court
        oppOnFgpctRim = pct_val(p$opp_on_fgpct_rim, 1),
        oppOnFgpctShortMid = pct_val(p$opp_on_fgpct_short_mid, 1),
        oppOnFgpctLongMid = pct_val(p$opp_on_fgpct_long_mid, 1),
        oppOnFgpctAllMid = pct_val(p$opp_on_fgpct_all_mid, 1),
        oppOnFgpctCornerThree = pct_val(p$opp_on_fgpct_corner_three, 1),
        oppOnFgpctNcThree = pct_val(p$opp_on_fgpct_nc_three, 1),
        oppOnFgpctAllThree = pct_val(p$opp_on_fgpct_all_three, 1),

        # Differential (ON - OFF, negative is good defense)
        oppDiffRim = pct_val(p$opp_diff_rim, 1),
        oppDiffShortMid = pct_val(p$opp_diff_short_mid, 1),
        oppDiffLongMid = pct_val(p$opp_diff_long_mid, 1),
        oppDiffAllMid = pct_val(p$opp_diff_all_mid, 1),
        oppDiffCornerThree = pct_val(p$opp_diff_corner_three, 1),
        oppDiffNcThree = pct_val(p$opp_diff_nc_three, 1),
        oppDiffAllThree = pct_val(p$opp_diff_all_three, 1),

        # Opponent attempts when player ON court
        oppFgaRim = pct_val(p$opp_fga_rim, 0),
        oppFgaShortMid = pct_val(p$opp_fga_short_mid, 0),
        oppFgaLongMid = pct_val(p$opp_fga_long_mid, 0),
        oppFgaAllMid = pct_val(p$opp_fga_all_mid, 0),
        oppFgaCornerThree = pct_val(p$opp_fga_corner_three, 0),
        oppFgaNcThree = pct_val(p$opp_fga_nc_three, 0),
        oppFgaAllThree = pct_val(p$opp_fga_all_three, 0)
      )
    })

    # Remove any with NA player names
    players_export <- Filter(function(x) !is.na(x$player) && x$player != "", players_export)

    output_file <- file.path(REACT_APP_DIR, "public/data", output_name)
    write_json(players_export, output_file, pretty = TRUE, auto_unbox = TRUE, null = "null")
    cat(sprintf("  ✓ Exported %d player records\n\n", length(players_export)))
  } else {
    cat("  ⚠ No player data found. Run calculate_player_stats() first.\n\n")
  }
}

export_similarity_data <- function(all_players) {
  cat("Exporting player similarity data...\n")

  if (nrow(all_players) == 0) {
    cat("  ⚠ No player data found. Skipping similarity export.\n\n")
    return()
  }

  # Filter: minimum 10 games AND 10+ mpg
  qualified <- all_players[!is.na(all_players$games) & all_players$games >= 10 &
                           !is.na(all_players$mpg) & all_players$mpg >= 10, ]
  qualified$team_id <- resolve_team_ids(
    qualified$team,
    qualified$season,
    context = "player similarity export"
  )
  cat(sprintf("  - %d player-seasons after filtering (games>=10, mpg>=10)\n", nrow(qualified)))

  if (nrow(qualified) < 2) {
    cat("  ⚠ Not enough qualified players for similarity. Skipping.\n\n")
    return()
  }

  # 13-feature stat vector
  stat_cols <- c("mpg", "ts", "usg", "ast_pct", "tov_pct", "orb_pct", "drb_pct",
                 "stl_pct", "blk_pct", "three_rate", "freq_rim", "freq_all_mid",
                 "S_assisted_fgm")

  mat <- as.matrix(qualified[, stat_cols])

  # Replace NAs with column means
  for (j in seq_len(ncol(mat))) {
    col_mean <- mean(mat[, j], na.rm = TRUE)
    if (is.na(col_mean)) col_mean <- 0
    mat[is.na(mat[, j]), j] <- col_mean
  }

  # Z-score normalize each column
  for (j in seq_len(ncol(mat))) {
    col_sd <- sd(mat[, j])
    col_mean <- mean(mat[, j])
    if (is.na(col_sd) || col_sd == 0) {
      mat[, j] <- 0
    } else {
      mat[, j] <- (mat[, j] - col_mean) / col_sd
    }
  }

  # Cosine similarity: normalize rows to unit vectors, then dot product
  row_norms <- sqrt(rowSums(mat^2))
  row_norms[row_norms == 0] <- 1  # avoid division by zero
  mat_norm <- mat / row_norms

  # Compute full similarity matrix
  sim_matrix <- tcrossprod(mat_norm)

  n <- nrow(qualified)
  similarity_ids <- paste(
    qualified$license_id,
    qualified$season,
    qualified$team_id,
    sep = "_"
  )
  if (anyDuplicated(similarity_ids)) {
    duplicate_ids <- unique(similarity_ids[
      duplicated(similarity_ids) | duplicated(similarity_ids, fromLast = TRUE)
    ])
    stop(
      sprintf(
        "Player similarity identities are not unique: %s",
        paste(duplicate_ids, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  cat(sprintf("  - Computing similarity for %d player-seasons...\n", n))

  # Build output: top 20 similar for each player-season
  similarity_export <- vector("list", n)

  for (i in seq_len(n)) {
    scores <- sim_matrix[i, ]
    scores[i] <- -Inf  # exclude self

    # Get top 20 indices
    top_idx <- order(scores, decreasing = TRUE)[1:min(20, n - 1)]

    similar <- lapply(top_idx, function(j) {
      # Rescale cosine similarity [-1, 1] to [0, 100]
      raw_score <- sim_matrix[i, j]
      scaled <- round((raw_score + 1) / 2 * 100, 1)
      list(
        licenseId = qualified$license_id[j],
        season = qualified$season[j],
        teamId = qualified$team_id[j],
        score = scaled
      )
    })

    similarity_export[[i]] <- list(
      id = similarity_ids[i],
      licenseId = qualified$license_id[i],
      season = qualified$season[i],
      teamId = qualified$team_id[i],
      similar = similar
    )
  }

  output_file <- file.path(REACT_APP_DIR, "public/data/similarity.json")
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  write_json(similarity_export, output_file, pretty = TRUE, auto_unbox = TRUE)
  cat(sprintf("  ✓ Exported similarity data for %d player-seasons\n\n", n))
}

export_clutch_data <- function() {
  cat("Exporting clutch data with stable team identities...\n")

  for (year in SEASONS) {
    rds_path <- file.path(SHINY_DATA_DIR, paste0("Clutch_", year, ".Rds"))
    if (!file.exists(rds_path)) {
      cat(sprintf("  ! No pre-computed clutch data for season %d\n", year))
      next
    }

    clutch_output <- readRDS(rds_path)
    has_player_team <- vapply(
      clutch_output$players,
      function(record) {
        !is.null(record$team) && !is.na(record$team) && nzchar(record$team)
      },
      logical(1)
    )
    if (any(!has_player_team)) {
      cat(sprintf(
        "  - Removed %d player rows without a team in season %d\n",
        sum(!has_player_team),
        year
      ))
      clutch_output$players <- clutch_output$players[has_player_team]
    }

    team_names <- vapply(
      clutch_output$teams,
      function(record) record$team,
      character(1)
    )
    team_ids <- validate_unique_team_seasons(
      team_names,
      rep(year, length(team_names)),
      context = sprintf("clutch teams for season %d", year)
    )
    clutch_output$teams <- Map(function(record, team_id) {
      record$teamId <- team_id
      record
    }, clutch_output$teams, team_ids)

    player_team_names <- vapply(
      clutch_output$players,
      function(record) record$team,
      character(1)
    )
    player_team_ids <- resolve_team_ids(
      player_team_names,
      rep(year, length(player_team_names)),
      context = sprintf("clutch players for season %d", year)
    )
    clutch_output$players <- Map(function(record, team_id) {
      record$teamId <- team_id
      record
    }, clutch_output$players, player_team_ids)

    output_file <- file.path(
      REACT_APP_DIR,
      "public/data",
      paste0("clutch-", year, ".json")
    )
    dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
    write_json(
      clutch_output,
      output_file,
      auto_unbox = TRUE,
      null = "null",
      na = "null"
    )
    cat(sprintf("  - Exported clutch identities for season %d\n", year))
  }

  cat("  Done.\n\n")
}

export_teampace_data <- function() {
  cat("Exporting team pace data (per-season files for lazy loading)...\n")

  for (year in SEASONS) {
    rds_path <- file.path(SHINY_DATA_DIR, paste0("TeamPace_", year, ".Rds"))
    if (file.exists(rds_path)) {
      teams_output <- readRDS(rds_path)
      team_names <- vapply(teams_output, function(record) record$team, character(1))
      team_ids <- validate_unique_team_seasons(
        team_names,
        rep(year, length(team_names)),
        context = sprintf("team pace for season %d", year)
      )
      teams_output <- Map(function(record, team_id) {
        record$teamId <- team_id
        record
      }, teams_output, team_ids)
      output_file <- file.path(REACT_APP_DIR, "public/data", paste0("teampace-", year, ".json"))
      dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
      write_json(teams_output, output_file, auto_unbox = TRUE, null = "null")
      cat(sprintf("  - Exported %d teams for season %d\n", length(teams_output), year))
    } else {
      cat(sprintf("  ! No pre-computed data for season %d (run pipeline first)\n", year))
    }
  }

  cat("  Done.\n\n")
}

export_gameflow_data <- function() {
  cat("Exporting game flow data (per-season files for lazy loading)...\n")

  for (year in SEASONS) {
    rds_path <- file.path(SHINY_DATA_DIR, paste0("GameFlow_", year, ".Rds"))
    if (file.exists(rds_path)) {
      games_list <- readRDS(rds_path)
      local_names <- vapply(games_list, function(game) game$local, character(1))
      visitor_names <- vapply(games_list, function(game) game$visitor, character(1))
      local_ids <- resolve_team_ids(
        local_names,
        rep(year, length(local_names)),
        context = sprintf("game flow local teams for season %d", year)
      )
      visitor_ids <- resolve_team_ids(
        visitor_names,
        rep(year, length(visitor_names)),
        context = sprintf("game flow visitor teams for season %d", year)
      )
      games_list <- Map(function(game, local_id, visitor_id) {
        game$localTeamId <- local_id
        game$visitorTeamId <- visitor_id
        game
      }, games_list, local_ids, visitor_ids)
      output_file <- file.path(REACT_APP_DIR, "public/data", paste0("gameflow-", year, ".json"))
      dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
      write_json(games_list, output_file, auto_unbox = TRUE, null = "null")
      cat(sprintf("  - Exported %d games for season %d\n", length(games_list), year))
    } else {
      cat(sprintf("  ! No pre-computed data for season %d (run pipeline first)\n", year))
    }
  }

  cat("  Done.\n\n")
}

# ============================================================================
# MAIN
# ============================================================================

if (!isTRUE(getOption("openacb.skip_auto_export"))) {
  cat("\n========================================\n")
  cat("OpenACB Data Export\n")
  cat("========================================\n\n")

  export_team_identities()
  export_shot_data()
  export_player_names()
  export_team_data()
  export_team_data(c("regular", "playoffs"), "teams-by-stage.json")
  all_players <- load_all_player_data()
  export_player_data(all_players)
  stage_players <- load_all_player_data(c("regular", "playoffs"))
  export_player_data(stage_players, "players-by-stage.json")
  export_similarity_data(all_players)
  export_clutch_data()
  export_gameflow_data()
  export_teampace_data()

  # player bio: position, height, birth date
  source("etl/12_player_positions.R")
  generate_player_bio(
    data_dir   = SHINY_DATA_DIR,
    output_dir = file.path(REACT_APP_DIR, "public/data")
  )

  cat("\n========================================\n")
  cat("Export complete!\n")
  cat("Open the React app to see your data.\n")
  cat("========================================\n")
}
