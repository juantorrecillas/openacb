import {
  buildGameSegment,
  buildPlayerSegment,
  getPlayerProfileSlug,
  isTeamId,
  parseEntitySegment,
  safeDecodePathSegment,
  slugify,
} from './identifiers.js'

function optionalTeamId(teamId) {
  if (teamId == null || teamId === '') return null
  const candidate = safeDecodePathSegment(teamId)
  if (!isTeamId(candidate)) throw new TypeError(`teamId no válido: ${teamId}`)
  return candidate
}

function playerSegment(player, name) {
  if (typeof player === 'string' && player.includes('--')) {
    const segment = safeDecodePathSegment(player)
    if (!parseEntitySegment(segment)) throw new TypeError(`Segmento de jugador no válido: ${player}`)
    return segment
  }
  return buildPlayerSegment(player, name)
}

function gameSegment(game, name) {
  if (typeof game === 'string' && game.includes('--')) {
    const segment = safeDecodePathSegment(game)
    if (!parseEntitySegment(segment)) throw new TypeError(`Segmento de partido no válido: ${game}`)
    return segment
  }
  return buildGameSegment(game, name)
}

export const buildHomePath = () => '/'
export const buildAboutPath = () => '/info'

export const buildTeamStatsPath = () => '/equipos/estadisticas'
export const buildFourFactorsPath = () => '/equipos/cuatro-factores'
export const buildTeamQuartersPath = () => '/equipos/cuartos'
export const buildTeamClutchPath = () => '/equipos/clutch'

export function buildTeamProfilePath(teamId) {
  const id = optionalTeamId(teamId)
  return id ? `/equipos/perfil/${id}` : '/equipos/perfil'
}

export function buildTeamComparisonPath(teamAId, teamBId) {
  const teamA = optionalTeamId(teamAId)
  const teamB = optionalTeamId(teamBId)
  if (!teamA && !teamB) return '/equipos/comparar'
  if (!teamA || !teamB) throw new TypeError('La comparación requiere dos teamId')
  return `/equipos/comparar/${teamA}/${teamB}`
}

export function buildGamesPath(game, label) {
  return game == null || game === '' ? '/partidos' : `/partidos/${gameSegment(game, label)}`
}

export const buildPlayerStatsPath = () => '/jugadores/estadisticas'
export const buildPlayerClutchPath = () => '/jugadores/clutch'

export function buildPlayerProfilePath(player, name) {
  if (player == null || player === '') return '/jugadores/perfil'
  if (typeof player === 'object' && player.profileSlug) {
    const storedSlug = safeDecodePathSegment(player.profileSlug)
    if (!parseEntitySegment(storedSlug) && !isTeamId(storedSlug)) {
      throw new TypeError(`Slug de perfil no válido: ${player.profileSlug}`)
    }
    return `/jugadores/perfil/${storedSlug}`
  }
  if (typeof player === 'string') {
    const candidate = safeDecodePathSegment(player)
    if (candidate.includes('--')) {
      const parsed = parseEntitySegment(candidate)
      if (!parsed) throw new TypeError(`Segmento de jugador no válido: ${player}`)
      return `/jugadores/perfil/${parsed.slug}`
    }
    if (name) return `/jugadores/perfil/${getPlayerProfileSlug(null, name)}`
    if (!isTeamId(candidate) || /^\d+$/.test(candidate)) {
      throw new TypeError(`Slug de jugador no válido: ${player}`)
    }
    return `/jugadores/perfil/${candidate}`
  }
  return `/jugadores/perfil/${getPlayerProfileSlug(player, name)}`
}

export function buildPlayerSimilarityPath(player, name) {
  return player == null || player === ''
    ? '/jugadores/similitud'
    : `/jugadores/similitud/${playerSegment(player, name)}`
}

export function buildPlayerComparisonPath(playerA, playerB, names = {}) {
  if ((playerA == null || playerA === '') && (playerB == null || playerB === '')) {
    return '/jugadores/comparar'
  }
  if (playerA == null || playerB == null || playerA === '' || playerB === '') {
    throw new TypeError('La comparación requiere dos jugadores')
  }
  return `/jugadores/comparar/${playerSegment(playerA, names.playerA)}/${playerSegment(playerB, names.playerB)}`
}

export function buildLineupAnalysisPath(teamId) {
  const id = optionalTeamId(teamId)
  return id ? `/alineaciones/analisis/${id}` : '/alineaciones/analisis'
}

export const buildLineupRankingsPath = () => '/alineaciones/rankings'
export const buildShotChartsPath = () => '/tiro/cartas'

export function buildTeamShotChartPath(teamId) {
  const id = optionalTeamId(teamId)
  if (!id) throw new TypeError('La carta de tiro de equipo requiere un teamId')
  return `/tiro/cartas/equipo/${id}`
}

export function buildPlayerShotChartPath(player, name) {
  if (player == null || player === '') throw new TypeError('La carta de tiro de jugador requiere un jugador')
  return `/tiro/cartas/jugador/${playerSegment(player, name)}`
}

export const buildZoneLeadersPath = () => '/tiro/lideres'

export const SECTION_DEFAULT_PATHS = Object.freeze({
  equipos: buildTeamStatsPath(),
  jugadores: buildPlayerStatsPath(),
  alineaciones: buildLineupAnalysisPath(),
  tiro: buildShotChartsPath(),
})

export const PATH_BUILDERS = Object.freeze({
  home: buildHomePath,
  teamStats: buildTeamStatsPath,
  teamProfile: buildTeamProfilePath,
  teamComparison: buildTeamComparisonPath,
  fourFactors: buildFourFactorsPath,
  teamQuarters: buildTeamQuartersPath,
  teamClutch: buildTeamClutchPath,
  games: buildGamesPath,
  playerStats: buildPlayerStatsPath,
  playerProfile: buildPlayerProfilePath,
  playerSimilarity: buildPlayerSimilarityPath,
  playerComparison: buildPlayerComparisonPath,
  playerClutch: buildPlayerClutchPath,
  lineupAnalysis: buildLineupAnalysisPath,
  lineupRankings: buildLineupRankingsPath,
  shotCharts: buildShotChartsPath,
  teamShotChart: buildTeamShotChartPath,
  playerShotChart: buildPlayerShotChartPath,
  zoneLeaders: buildZoneLeadersPath,
  about: buildAboutPath,
})

export function legacyTeamNameSlug(name) {
  return slugify(name, 'equipo')
}
