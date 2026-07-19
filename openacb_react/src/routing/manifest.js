import {
  buildAboutPath,
  buildFourFactorsPath,
  buildGamesPath,
  buildHomePath,
  buildLineupAnalysisPath,
  buildLineupRankingsPath,
  buildPlayerClutchPath,
  buildPlayerComparisonPath,
  buildPlayerProfilePath,
  buildPlayerShotChartPath,
  buildPlayerSimilarityPath,
  buildPlayerStatsPath,
  buildShotChartsPath,
  buildTeamClutchPath,
  buildTeamComparisonPath,
  buildTeamProfilePath,
  buildTeamQuartersPath,
  buildTeamShotChartPath,
  buildTeamStatsPath,
  buildZoneLeadersPath,
} from './paths.js'
import { ROUTE_QUERY_SCHEMAS, serializeRouteQuery, withQuery } from './query.js'
import { safeDecodePathSegment } from './identifiers.js'

function route(definition) {
  return Object.freeze({
    navigation: true,
    ...definition,
    patterns: Object.freeze(definition.patterns),
    legacyPatterns: Object.freeze(definition.legacyPatterns || []),
    querySchema: ROUTE_QUERY_SCHEMAS[definition.id],
  })
}

export const ROUTE_MANIFEST = Object.freeze([
  route({
    id: 'home', sectionId: 'home', navGroupId: 'home', tabId: 'home',
    label: 'Inicio', patterns: ['/'], legacyPatterns: [], build: () => buildHomePath(),
  }),
  route({
    id: 'teamStats', sectionId: 'equipos', navGroupId: 'equipos', tabId: 'teams',
    label: 'Estadísticas de Equipo', patterns: ['/equipos/estadisticas'], legacyPatterns: ['/equipos'],
    build: () => buildTeamStatsPath(),
  }),
  route({
    id: 'teamProfile', sectionId: 'equipos', navGroupId: 'equipos', tabId: 'fingerprint',
    label: 'Perfil de Equipo', patterns: ['/equipos/perfil', '/equipos/perfil/:teamId'],
    legacyPatterns: ['/perfil-equipo', '/perfil-equipo/:season/:team'],
    build: params => buildTeamProfilePath(params.teamId),
  }),
  route({
    id: 'teamComparison', sectionId: 'equipos', navGroupId: 'herramientas', tabId: 'matchup',
    label: 'Cara a Cara', patterns: ['/equipos/comparar', '/equipos/comparar/:teamAId/:teamBId'],
    legacyPatterns: ['/matchup-equipos', '/matchup-equipos/:season/:teamA/:teamB'],
    build: params => buildTeamComparisonPath(params.teamAId, params.teamBId),
  }),
  route({
    id: 'fourFactors', sectionId: 'equipos', navGroupId: 'equipos', tabId: 'factors',
    label: 'Four Factors', patterns: ['/equipos/cuatro-factores'], legacyPatterns: ['/cuatro-factores'],
    build: () => buildFourFactorsPath(),
  }),
  route({
    id: 'teamQuarters', sectionId: 'equipos', navGroupId: 'equipos', tabId: 'quarters',
    label: 'Rendimiento por Cuarto', patterns: ['/equipos/cuartos'], navigation: false,
    build: () => buildTeamQuartersPath(),
  }),
  route({
    id: 'teamClutch', sectionId: 'equipos', navGroupId: 'equipos', tabId: 'teamClutch',
    label: 'Clutch por Equipo', patterns: ['/equipos/clutch'], navigation: false,
    build: () => buildTeamClutchPath(),
  }),
  route({
    id: 'games', sectionId: 'partidos', navGroupId: 'equipos', tabId: 'gameflow',
    label: 'Análisis de Partido', patterns: ['/partidos', '/partidos/:game'], legacyPatterns: ['/flujo-partido'],
    build: params => buildGamesPath(params.game ?? params.gameId, params.name),
  }),
  route({
    id: 'playerStats', sectionId: 'jugadores', navGroupId: 'jugadores', tabId: 'players',
    label: 'Estadísticas de Jugador', patterns: ['/jugadores/estadisticas'], legacyPatterns: ['/jugadores'],
    build: () => buildPlayerStatsPath(),
  }),
  route({
    id: 'playerProfile', sectionId: 'jugadores', navGroupId: 'jugadores', tabId: 'profile',
    label: 'Perfil de Jugador', patterns: ['/jugadores/perfil', '/jugadores/perfil/:player'],
    legacyPatterns: ['/jugador', '/jugador/:licenseId'],
    build: params => buildPlayerProfilePath(params.player ?? params.licenseId, params.name),
  }),
  route({
    id: 'playerSimilarity', sectionId: 'jugadores', navGroupId: 'herramientas', tabId: 'similarity',
    label: 'Similitud', patterns: ['/jugadores/similitud', '/jugadores/similitud/:player'],
    legacyPatterns: ['/similitud', '/similitud/:licenseId/:season'],
    build: params => buildPlayerSimilarityPath(params.player ?? params.licenseId, params.name),
  }),
  route({
    id: 'playerComparison', sectionId: 'jugadores', navGroupId: 'herramientas', tabId: 'comparison',
    label: 'Comparar Jugadores', patterns: ['/jugadores/comparar', '/jugadores/comparar/:playerA/:playerB'],
    legacyPatterns: ['/comparar', '/comparar/:aId/:aSeason/:bId/:bSeason'],
    build: params => buildPlayerComparisonPath(params.playerA, params.playerB, params.names),
  }),
  route({
    id: 'playerClutch', sectionId: 'jugadores', navGroupId: 'jugadores', tabId: 'clutch',
    label: 'Estadísticas Clutch', patterns: ['/jugadores/clutch'], legacyPatterns: ['/estadisticas-clutch'],
    build: () => buildPlayerClutchPath(),
  }),
  route({
    id: 'lineupAnalysis', sectionId: 'alineaciones', navGroupId: 'alineaciones', tabId: 'lineups',
    label: 'Análisis On/Off', patterns: ['/alineaciones/analisis', '/alineaciones/analisis/:teamId'],
    legacyPatterns: ['/alineaciones', '/alineaciones/:season/:team'],
    build: params => buildLineupAnalysisPath(params.teamId),
  }),
  route({
    id: 'lineupRankings', sectionId: 'alineaciones', navGroupId: 'alineaciones', tabId: 'rankings',
    label: 'Mejores Alineaciones', patterns: ['/alineaciones/rankings'], legacyPatterns: ['/mejores-alineaciones'],
    build: () => buildLineupRankingsPath(),
  }),
  route({
    id: 'shotCharts', sectionId: 'tiro', navGroupId: 'tiro', tabId: 'shots',
    label: 'Cartas de Tiro',
    patterns: ['/tiro/cartas', '/tiro/cartas/equipo/:teamId', '/tiro/cartas/jugador/:player'],
    legacyPatterns: ['/cartas-tiro'],
    build: params => {
      if (params.teamId) return buildTeamShotChartPath(params.teamId)
      if (params.player) return buildPlayerShotChartPath(params.player, params.name)
      return buildShotChartsPath()
    },
  }),
  route({
    id: 'zoneLeaders', sectionId: 'tiro', navGroupId: 'tiro', tabId: 'zoneleaders',
    label: 'Líderes por Zona', patterns: ['/tiro/lideres'],
    legacyPatterns: ['/lideres-zona', '/lideres-zona/:season/:metric'],
    build: () => buildZoneLeadersPath(),
  }),
  route({
    id: 'about', sectionId: 'info', navGroupId: 'about', tabId: 'about',
    label: 'Info', patterns: ['/info'], legacyPatterns: [], build: () => buildAboutPath(),
  }),
])

export const ROUTES_BY_ID = Object.freeze(Object.fromEntries(ROUTE_MANIFEST.map(item => [item.id, item])))

export const NAVIGATION_GROUPS = Object.freeze([
  Object.freeze({ id: 'equipos', label: 'Equipos', routeIds: Object.freeze(['teamStats', 'teamProfile', 'games', 'fourFactors']) }),
  Object.freeze({ id: 'jugadores', label: 'Jugadores', routeIds: Object.freeze(['playerStats', 'playerProfile', 'playerClutch']) }),
  Object.freeze({ id: 'herramientas', label: 'Herramientas', routeIds: Object.freeze(['playerSimilarity', 'playerComparison', 'teamComparison']) }),
  Object.freeze({ id: 'alineaciones', label: 'Alineaciones', routeIds: Object.freeze(['lineupAnalysis', 'lineupRankings']) }),
  Object.freeze({ id: 'tiro', label: 'Tiro', routeIds: Object.freeze(['shotCharts', 'zoneLeaders']) }),
  Object.freeze({ id: 'about', label: 'Info', routeIds: Object.freeze(['about']), single: true }),
])

export const SECTION_REDIRECTS = Object.freeze({
  '/equipos': '/equipos/estadisticas',
  '/jugadores': '/jugadores/estadisticas',
  '/alineaciones': '/alineaciones/analisis',
  '/tiro': '/tiro/cartas',
})

export function canonicalizePathname(pathname) {
  const value = String(pathname || '/').split(/[?#]/, 1)[0]
  if (value === '/') return '/'
  return `/${value.split('/').filter(Boolean).join('/')}`
}

function matchPattern(pathname, pattern) {
  const pathSegments = canonicalizePathname(pathname).split('/').filter(Boolean)
  const patternSegments = canonicalizePathname(pattern).split('/').filter(Boolean)
  if (pathSegments.length !== patternSegments.length) return null

  const params = {}
  for (let index = 0; index < patternSegments.length; index += 1) {
    const expected = patternSegments[index]
    const actual = pathSegments[index]
    if (expected.startsWith(':')) params[expected.slice(1)] = safeDecodePathSegment(actual)
    else if (expected.toLocaleLowerCase('es') !== actual.toLocaleLowerCase('es')) return null
  }
  return params
}

export function matchCanonicalRoute(pathname) {
  for (const item of ROUTE_MANIFEST) {
    for (const pattern of item.patterns) {
      const params = matchPattern(pathname, pattern)
      if (params) return { route: item, routeId: item.id, pattern, params }
    }
  }
  return null
}

export function matchLegacyRoute(pathname) {
  for (const item of ROUTE_MANIFEST) {
    for (const pattern of item.legacyPatterns) {
      const params = matchPattern(pathname, pattern)
      if (params) return { route: item, routeId: item.id, pattern, params }
    }
  }
  return null
}

export function getRouteById(routeId) {
  return ROUTES_BY_ID[routeId] || null
}

export function getRouteForPath(pathname) {
  return matchCanonicalRoute(pathname)?.route || null
}

export function getActiveTabId(pathname) {
  return matchCanonicalRoute(pathname)?.route.tabId || null
}

export function getActiveNavigationGroup(pathname) {
  return matchCanonicalRoute(pathname)?.route.navGroupId || null
}

export function buildRoutePath(routeId, params = {}) {
  const item = getRouteById(routeId)
  if (!item) throw new TypeError(`Ruta desconocida: ${routeId}`)
  return item.build(params)
}

export function buildRouteUrl(routeId, params = {}, query = {}, options = {}) {
  const pathname = buildRoutePath(routeId, params)
  return withQuery(pathname, serializeRouteQuery(routeId, query, options))
}
