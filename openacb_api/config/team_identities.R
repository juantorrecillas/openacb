# stable club identities used by exports and public urls

TEAM_IDENTITIES <- list(
  list(
    team_id = "barcelona",
    aliases = list(
      list(name = "Barcelona Lassa", seasons = 2017:2018),
      list(name = "Barça Lassa", seasons = 2019L),
      list(name = "Barça", seasons = 2020:2026)
    )
  ),
  list(
    team_id = "baskonia",
    aliases = list(
      list(name = "Baskonia", seasons = c(2017L, 2024L, 2025L)),
      list(name = "Kirolbet Baskonia", seasons = 2018:2020),
      list(name = "TD Systems Baskonia", seasons = 2021L),
      list(name = "Bitci Baskonia", seasons = 2022L),
      list(name = "Cazoo Baskonia", seasons = 2023L),
      list(name = "Kosner Baskonia", seasons = 2026L)
    )
  ),
  list(
    team_id = "joventut",
    aliases = list(
      list(name = "Divina Seguros Joventut", seasons = 2017:2019),
      list(name = "Joventut Badalona", seasons = 2020:2026)
    )
  ),
  list(
    team_id = "gran-canaria",
    aliases = list(
      list(name = "Herbalife Gran Canaria", seasons = 2017:2021),
      list(name = "Gran Canaria", seasons = 2022:2023),
      list(name = "Dreamland Gran Canaria", seasons = 2024:2026)
    )
  ),
  list(
    team_id = "tenerife",
    aliases = list(
      list(name = "Iberostar Tenerife", seasons = 2017:2020),
      list(name = "Lenovo Tenerife", seasons = 2021:2024),
      list(name = "La Laguna Tenerife", seasons = 2025:2026)
    )
  ),
  list(
    team_id = "manresa",
    aliases = list(
      list(name = "ICL Manresa", seasons = 2017L),
      list(name = "BAXI Manresa", seasons = 2019:2026)
    )
  ),
  list(
    team_id = "fuenlabrada",
    aliases = list(
      list(name = "Montakit Fuenlabrada", seasons = 2017:2020),
      list(name = "Urbas Fuenlabrada", seasons = 2021:2022),
      list(name = "Carplus Fuenlabrada", seasons = 2023L)
    )
  ),
  list(
    team_id = "andorra",
    aliases = list(
      list(
        name = "MoraBanc Andorra",
        seasons = c(2017:2019, 2021:2022, 2024:2026)
      ),
      list(name = "Morabanc Andorra", seasons = 2020L)
    )
  ),
  list(
    team_id = "estudiantes",
    aliases = list(
      list(name = "Movistar Estudiantes", seasons = 2017:2021)
    )
  ),
  list(
    team_id = "real-betis",
    aliases = list(
      list(name = "Real Betis Energía Plus", seasons = 2017L),
      list(name = "Real Betis Energia Plus", seasons = 2018L),
      list(name = "Coosur Real Betis", seasons = 2020:2022),
      list(name = "Real Betis Baloncesto", seasons = 2023L)
    )
  ),
  list(
    team_id = "real-madrid",
    aliases = list(
      list(name = "Real Madrid", seasons = 2017:2026)
    )
  ),
  list(
    team_id = "bilbao-basket",
    aliases = list(
      list(
        name = "RETAbet Bilbao Basket",
        seasons = c(2017:2018, 2020:2021)
      ),
      list(name = "Surne Bilbao Basket", seasons = 2022:2026)
    )
  ),
  list(
    team_id = "obradoiro",
    aliases = list(
      list(name = "Rio Natura Monbus Obradoiro", seasons = 2017L),
      list(name = "Monbus Obradoiro", seasons = 2018:2024)
    )
  ),
  list(
    team_id = "zaragoza",
    aliases = list(
      list(name = "Tecnyconta Zaragoza", seasons = 2017:2019),
      list(name = "Casademont Zaragoza", seasons = 2020:2026)
    )
  ),
  list(
    team_id = "murcia",
    aliases = list(
      list(name = "UCAM Murcia", seasons = 2017:2026)
    )
  ),
  list(
    team_id = "unicaja",
    aliases = list(
      list(name = "Unicaja", seasons = 2017:2026)
    )
  ),
  list(
    team_id = "valencia-basket",
    aliases = list(
      list(name = "Valencia Basket", seasons = 2017:2026)
    )
  ),
  list(
    team_id = "gipuzkoa",
    aliases = list(
      list(name = "Delteco GBC", seasons = 2018:2019),
      list(name = "Acunsa GBC", seasons = 2021L)
    )
  ),
  list(
    team_id = "burgos",
    aliases = list(
      list(name = "San Pablo Burgos", seasons = 2018:2020),
      list(name = "Hereda San Pablo Burgos", seasons = 2021:2022),
      list(name = "Recoletas Salud San Pablo Burgos", seasons = 2026L)
    )
  ),
  list(
    team_id = "breogan",
    aliases = list(
      list(name = "Cafés Candelas Breogán", seasons = 2019L),
      list(name = "Río Breogán", seasons = 2022:2026)
    )
  ),
  list(
    team_id = "girona",
    aliases = list(
      list(name = "Bàsquet Girona", seasons = 2023:2026)
    )
  ),
  list(
    team_id = "granada",
    aliases = list(
      list(name = "Coviran Granada", seasons = 2023:2026)
    )
  ),
  list(
    team_id = "palencia",
    aliases = list(
      list(name = "Zunder Palencia", seasons = 2024L)
    )
  ),
  list(
    team_id = "lleida",
    aliases = list(
      list(name = "Hiopos Lleida", seasons = 2025:2026)
    )
  ),
  list(
    team_id = "coruna",
    aliases = list(
      list(name = "Leyma Coruña", seasons = 2025L)
    )
  )
)

# expand the nested registry to one row per alias and season
team_identity_alias_table <- function(registry = TEAM_IDENTITIES) {
  rows <- lapply(registry, function(team) {
    do.call(rbind, lapply(team$aliases, function(alias) {
      data.frame(
        team_id = team$team_id,
        name = alias$name,
        season = as.integer(alias$seasons),
        stringsAsFactors = FALSE
      )
    }))
  })

  alias_table <- do.call(rbind, rows)
  row.names(alias_table) <- NULL
  alias_table
}

# fail early when ids or aliases are malformed or ambiguous
validate_team_identity_registry <- function(
    registry = TEAM_IDENTITIES,
    expected_teams = 25L
) {
  if (!is.list(registry) || length(registry) != expected_teams) {
    stop(
      sprintf(
        "Team identity registry must contain exactly %d clubs; found %d",
        expected_teams,
        length(registry)
      ),
      call. = FALSE
    )
  }

  team_ids <- vapply(registry, function(team) team$team_id, character(1))
  if (anyDuplicated(team_ids)) {
    stop("Team identity registry contains duplicate team ids", call. = FALSE)
  }
  if (any(!grepl("^[a-z0-9]+(?:-[a-z0-9]+)*$", team_ids))) {
    stop("Team ids must be lowercase, accent-free URL slugs", call. = FALSE)
  }

  valid_alias <- vapply(registry, function(team) {
    is.list(team$aliases) && length(team$aliases) > 0L &&
      all(vapply(team$aliases, function(alias) {
        is.character(alias$name) && length(alias$name) == 1L &&
          nzchar(alias$name) && length(alias$seasons) > 0L &&
          !anyNA(alias$seasons) && !anyDuplicated(alias$seasons)
      }, logical(1)))
  }, logical(1))
  if (any(!valid_alias)) {
    stop("Each team alias needs one name and unique seasons", call. = FALSE)
  }

  alias_table <- team_identity_alias_table(registry)
  alias_season_key <- paste(alias_table$name, alias_table$season, sep = "\r")
  if (anyDuplicated(alias_season_key)) {
    stop("Team identity registry contains an ambiguous alias-season", call. = FALSE)
  }

  alias_teams <- split(alias_table$team_id, alias_table$name)
  ambiguous_aliases <- names(alias_teams)[vapply(
    alias_teams,
    function(ids) length(unique(ids)) > 1L,
    logical(1)
  )]
  if (length(ambiguous_aliases) > 0L) {
    stop(
      paste(
        "Team aliases map to multiple clubs:",
        paste(ambiguous_aliases, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# resolve display names using season-aware exact matching
resolve_team_ids <- function(
    team_names,
    season_ids,
    context = "team data",
    registry = TEAM_IDENTITIES
) {
  team_names <- as.character(team_names)
  season_ids <- as.integer(season_ids)

  if (length(team_names) == 1L && length(season_ids) > 1L) {
    team_names <- rep(team_names, length(season_ids))
  }
  if (length(season_ids) == 1L && length(team_names) > 1L) {
    season_ids <- rep(season_ids, length(team_names))
  }
  if (length(team_names) != length(season_ids)) {
    stop("Team names and seasons must have matching lengths", call. = FALSE)
  }

  validate_team_identity_registry(registry)
  alias_table <- team_identity_alias_table(registry)
  alias_keys <- paste(alias_table$name, alias_table$season, sep = "\r")
  record_keys <- paste(team_names, season_ids, sep = "\r")
  matches <- match(record_keys, alias_keys)

  missing <- is.na(team_names) | !nzchar(team_names) |
    is.na(season_ids) | is.na(matches)
  if (any(missing)) {
    missing_labels <- unique(sprintf(
      "%s (%s)",
      ifelse(is.na(team_names[missing]), "<missing>", team_names[missing]),
      ifelse(is.na(season_ids[missing]), "unknown season", season_ids[missing])
    ))
    stop(
      sprintf(
        "%s contains unmapped team identities: %s",
        context,
        paste(missing_labels, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  alias_table$team_id[matches]
}

# validate one row per club within each season and optional partition
validate_unique_team_seasons <- function(
    team_names,
    season_ids,
    partitions = NULL,
    context = "team data",
    registry = TEAM_IDENTITIES
) {
  team_ids <- resolve_team_ids(
    team_names,
    season_ids,
    context = context,
    registry = registry
  )
  if (is.null(partitions)) {
    partitions <- rep("all", length(team_ids))
  }

  keys <- paste(as.integer(season_ids), partitions, team_ids, sep = "\r")
  duplicate_rows <- duplicated(keys) | duplicated(keys, fromLast = TRUE)
  if (any(duplicate_rows)) {
    duplicate_labels <- unique(sprintf(
      "%s/%s/%s",
      as.integer(season_ids)[duplicate_rows],
      partitions[duplicate_rows],
      team_ids[duplicate_rows]
    ))
    stop(
      sprintf(
        "%s contains duplicate club-season rows: %s",
        context,
        paste(duplicate_labels, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  team_ids
}

# preserve seasons as json arrays even when an alias appears once
team_identities_for_json <- function(registry = TEAM_IDENTITIES) {
  validate_team_identity_registry(registry)

  list(
    teams = lapply(registry, function(team) {
      list(
        teamId = team$team_id,
        aliases = lapply(team$aliases, function(alias) {
          list(
            name = alias$name,
            seasons = as.list(as.integer(alias$seasons))
          )
        })
      )
    })
  )
}

validate_team_identity_registry()
