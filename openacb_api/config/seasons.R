# =============================================================================
# ACB Basketball Analytics Pipeline - Season Configuration
# =============================================================================
# This file contains all season-specific parameters in one place.
# To add a new season, simply add a new row to the SEASONS tibble.
# =============================================================================

library(tibble)

# -----------------------------------------------------------------------------
# Season Configuration Table
# -----------------------------------------------------------------------------
# Each row represents one ACB season with all its parameters

SEASONS <- tibble::tribble(
  ~season_id, ~season_name,   ~edition_id, ~folder_name,   ~year_label,
  2021,       "2020-2021",    85,          "2020-2021",    "21",
  2022,       "2021-2022",    86,          "2021-2022",    "22",
  2023,       "2022-2023",    87,          "2022-2023",    "23",
  2024,       "2023-2024",    88,          "2023-2024",    "24",
  2025,       "2024-2025",    89,          "2024-2025",    "25",
  2026,       "2025-2026",    90,          "2025-2026",    "26"
)

# -----------------------------------------------------------------------------
# API Configuration
# -----------------------------------------------------------------------------
API_CONFIG <- list(
  base_url = "https://api2.acb.com/api/v1/openapilive",
  pbp_url = "https://api2.acb.com/api/v1/openapilive/PlayByPlay/matchevents",
  weeks_url_template = "https://api2.acb.com/api/v1/openapilive/Matchweeks/lite?idCompetition=1&idEdition={edition_id}",
  matches_url_template = "https://api2.acb.com/api/v1/openapilive/Matches/matchesbymatchweeklite?idCompetition=1&idEdition={edition_id}&idMatchweek=",
  
  # Bearer token
  bearer_token = "Bearer H4sIAAAAAAAAA32Ry3aqMBSG36iLi7qOw4oFk9PEQyohZEYilkCirIMF5OkbOmiVQUdZ+/bv798pbrAUkVR7BUEyAhcr0IIzWcoArEDdMBrA9VNxg5BPiQqp1wDGnEGTp/009MIZD4sd0dIsSxFMw7ATUaL2ulWxN7jZGV2poTegehuvrzKiTp7iRniL7zxhoZuzTZl5uhPKTTkD055Q+qTkkR6Pka4y9qNDTDjyu/hg6zxdVnkajq+UazmZ0Bstz7gT5j5HGhmtx4MPa9s/34dyxvUj41LL22OdMOxkqdXxSZN5d9r10BwNDS2v5Vh/zPg8ntD6W9OhwzHVvfDnPU033ZFHL495uinlmTTcsv/q5Yshmc+2GdNB8fXWs32wK3a4Ej7UMx8JT13LBy3LvUdsNXDDjf2P2X3YW6+EgQZUlwEf6gXeIgdvk4W9my52z2pfAReNcY8q1KPte4+CXtn/cub9p/jJef7j/Xvf/j150Sag6C1ctS41qVe3p6Fml5U4gf/xdecli8sn/TA0Eb8CAAA="
)

# -----------------------------------------------------------------------------
# Team Name Standardization (Season-Specific)
# -----------------------------------------------------------------------------
# Maps team name variations to canonical names WITHIN EACH SEASON
# This handles sponsor changes and abbreviation inconsistencies WITHIN a season
# but preserves different names ACROSS seasons (e.g., "Kosner Baskonia" in 2026 vs "Bitci Baskonia" in 2025)

# Global mappings that apply to all seasons (for consistency)
TEAM_NAME_MAPPINGS_GLOBAL <- list(
  # Format: "variant" = "canonical"
  # These are variations that should always be standardized regardless of season
  "Casademont Zgz" = "Casademont Zaragoza",
  "Joventut" = "Joventut Badalona",
  "Club Joventut Badalona" = "Joventut Badalona",
  "Monbus Obra" = "Monbus Obradoiro",
  "Carplus Fuenla" = "Carplus Fuenlabrada",
  "UCAM Murcia CB" = "UCAM Murcia",
  "Valencia Basket Club" = "Valencia Basket"
  )

# Season-specific mappings - these only apply within their respective seasons
TEAM_NAME_MAPPINGS_BY_SEASON <- list(
  # Format: season_id = list("variant" = "canonical")
  `2021` = list(
  ),
  `2022` = list(
  ),
  `2023` = list(
    "Dreamland Gran Canaria" = "Gran Canaria"
  ),
  `2024` = list(
  ),
  `2025` = list(
  ),
  `2026` = list(
    "Bitci Baskonia" = "Kosner Baskonia",  # In 2026, the sponsor is Kosner
    "Surne Bilbao" = "Surne Bilbao Basket",
    "Baskonia" = "Kosner Baskonia"
  )
)

# Get team name mappings for a specific season
get_team_name_mappings <- function(season_id) {
  # Start with global mappings
  mappings <- TEAM_NAME_MAPPINGS_GLOBAL
  
  # Add season-specific mappings if they exist
  if (as.character(season_id) %in% names(TEAM_NAME_MAPPINGS_BY_SEASON)) {
    season_mappings <- TEAM_NAME_MAPPINGS_BY_SEASON[[as.character(season_id)]]
    # Season-specific mappings override global ones
    mappings <- c(mappings, season_mappings)
  }
  
  return(mappings)
}

# -----------------------------------------------------------------------------
# Team Logo URLs
# -----------------------------------------------------------------------------
TEAM_LOGOS <- list(
  "Barça" = "https://static.acb.com/img/www/clubes2023/barca_logopos.png",
  "BAXI Manresa" = "https://static.acb.com/img/31/9/2f/75632.png",
  "Bàsquet Girona" = "https://static.acb.com/img/www/clubes2023/BASQUET-GIRONA-Logonegativo2223.png",
  "Carplus Fuenlabrada" = "https://static.acb.com/img/www/clubes2023/CarplusFuenlabrada-Logo-56.png",
  "Casademont Zaragoza" = "https://static.acb.com/img/3a/16/1c/logo_zza_2020.png",
  "Bitci Baskonia" = "https://static.acb.com/img/www/clubes2022/CazooBaskonia-Logo-56.png",
  "Coviran Granada" = "https://static.acb.com/img/www/clubes2023/coviran-granada_logopositivo.png",
  "Dreamland Gran Canaria" = "https://static.acb.com/img/2/58/5b/75611.png",
  "Joventut Badalona" = "https://static.acb.com/img/e/53/16/1453195579.png",
  "Lenovo Tenerife" = "https://static.acb.com/img/www/clubes2021/LenovoTenerife-pos-vert.png",
  "Monbus Obradoiro" = "https://static.acb.com/img/www/clubes2023/MonbusObradoiro_Positivo.png",
  "Real Betis Baloncesto" = "https://static.acb.com/img/www/clubes2023/RealBetisBaloncestoLogoPos.png",
  "Real Madrid" = "https://static.acb.com/img/20/22/3d/75629.png",
  "Río Breogán" = "https://static.acb.com/logos/1718/breogan_00.png",
  "Surne Bilbao Basket" = "https://static.acb.com/img/www/clubes2022/logo_surnebb_pos.png",
  "UCAM Murcia" = "https://static.acb.com/img/14/31/34/75635.png",
  "Unicaja" = "https://static.acb.com/img/www/clubes2022/Logoescudo_unicaja2.png",
  "Valencia Basket" = "https://static.acb.com/img/23/2/25/1654748673.png",
  "Hiopos Lleida" = "",
  "Leyma Coruña" = "",
  "MoraBanc Andorra" = "",
  "Recoletas Salud San Pablo Burgos" = "",
  "Zunder Palencia" = ""
)

# -----------------------------------------------------------------------------
# Boxscore Column Definitions
# -----------------------------------------------------------------------------
BOXSCORE_COLUMNS <- c(
  "puntos", "T1A", "T1I", "T2A", "T2I", "T3A", "T3I", 
  "contr2A", "contr3A", "mateA", "mateI",
  "reb_def", "reb_of", "recuperacion", "FT_trip", "asistencias",
  "tapon", "tapon_rec", "perdida", "falta", "falta_rec", 
  "tecnica", "altercado", "revision", "salto_ganado", "salto_perdido"
)

# Columns to remove from PBP data (reduces file size significantly)
PBP_COLUMNS_TO_DELETE <- c(
  "X", "id_competition", "id_edition", "id_phase", "id_subphase", 
  "id_round", "id_license", "id_license_type", "id_license_subtype", 
  "order", "id_playbyplaytype", "competition.name", "competition.official_name", 
  "competition.initial_date", "competition.final_date",
  "competition.url_image", "competition.url_image_negative",
  "statistics.3pt_success", "statistics.3pt_tried",
  "statistics.2pt_success", "statistics.2pt_tried", 
  "statistics.1pt_success", "statistics.1pt_tried", 
  "statistics.total_rebound", "statistics.asis",
  "statistics.steals", "statistics.turnovers", "statistics.blocks",
  "statistics.personal_fouls", "statistics.received_fouls"
)

# -----------------------------------------------------------------------------
# Helper Functions
# -----------------------------------------------------------------------------

#' Get season configuration by season_id (e.g., 2025)
#' @param season_id Integer year (e.g., 2025 for 2024-2025 season)
#' @return Named list with season parameters
get_season_config <- function(season_id) {
  row <- SEASONS[SEASONS$season_id == season_id, ]
  if (nrow(row) == 0) {
    stop(paste("Unknown season_id:", season_id, "- Add it to SEASONS in config/seasons.R"))
  }
  as.list(row)
}

#' Standardize team name using the mapping table
#' @param name Team name (possibly non-standard)
#' @return Canonical team name
standardize_team_name <- function(name) {
  if (name %in% names(TEAM_NAME_MAPPINGS)) {
    return(TEAM_NAME_MAPPINGS[[name]])
  }
  return(name)
}

#' Get all available season IDs
#' @return Vector of season IDs
get_available_seasons <- function() {
  SEASONS$season_id
}

cat("✓ Season configuration loaded\n")
cat("  Available seasons:", paste(SEASONS$season_id, collapse = ", "), "\n")
