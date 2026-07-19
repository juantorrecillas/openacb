import {
  buildFourFactorsPath,
  buildGamesPath,
  buildLineupAnalysisPath,
  buildLineupRankingsPath,
  buildPlayerClutchPath,
  buildPlayerComparisonPath,
  buildPlayerProfilePath,
  buildPlayerShotChartPath,
  buildPlayerSimilarityPath,
  buildPlayerStatsPath,
  buildShotChartsPath,
  buildTeamComparisonPath,
  buildTeamProfilePath,
  buildTeamStatsPath,
  buildZoneLeadersPath,
  SECTION_DEFAULT_PATHS,
} from './paths.js'
import {
  buildPlayerSegment,
  getPlayerProfileSlug,
  isTeamId,
  parseEntitySegment,
  safeDecodePathSegment,
} from './identifiers.js'
import { matchCanonicalRoute, matchLegacyRoute } from './manifest.js'
import { parseRouteQuery, serializeRouteQuery, withQuery } from './query.js'
import { resolveTeamIdentity } from './teamIdentities.js'

function locationParts(location) {
  if (typeof location === 'string') {
    const url = new URL(location, 'https://openacb.local')
    return { pathname: url.pathname, search: url.search }
  }
  return {
    pathname: location?.pathname || '/',
    search: location?.search || '',
  }
}

function identitySource(context) {
  return context.teamIdentityIndex
    || context.teamIdentities
    || context.teamRows
    || context.teams
    || null
}

function playerRows(context) {
  return context.players || context.playerRows || []
}

function redirect(to, routeId, reason = 'legacy-route') {
  return { status: 'redirect', to, routeId, reason, replace: true }
}

function needsData(routeId, resource) {
  return { status: 'needs-data', routeId, resource }
}

function resolveLegacyTeam(context, nameOrId, season, routeId) {
  const source = identitySource(context)
  if (!source) return needsData(routeId, 'team-identities')
  const resolution = resolveTeamIdentity(source, safeDecodePathSegment(nameOrId), season)
  if (resolution.status !== 'resolved') {
    return {
      status: resolution.status === 'ambiguous' ? 'ambiguous' : 'not-found',
      routeId,
      entity: 'team',
      value: nameOrId,
      season: Number(season) || null,
      candidates: resolution.candidates,
    }
  }
  return resolution.teamId
}

function distinctPlayerRecords(rows) {
  const records = new Map()
  for (const row of rows) {
    const key = `${row.licenseId}|${row.season}|${row.teamId || row.team || ''}`
    if (!records.has(key)) records.set(key, row)
  }
  return [...records.values()]
}

function matchingPlayers(context, licenseId, season, teamHint) {
  const id = String(licenseId)
  const targetSeason = season == null ? null : Number(season)
  let matches = distinctPlayerRecords(playerRows(context)).filter(row => (
    String(row.licenseId) === id
    && (targetSeason == null || Number(row.season) === targetSeason)
  ))

  if (targetSeason == null && matches.length > 0) {
    const latestSeason = Math.max(...matches.map(row => Number(row.season)).filter(Number.isFinite))
    matches = matches.filter(row => Number(row.season) === latestSeason)
  }

  if (teamHint) {
    const decodedHint = safeDecodePathSegment(teamHint)
    matches = matches.filter(row => row.teamId === decodedHint || row.team === decodedHint)
  }

  return matches.sort((a, b) => String(a.team || '').localeCompare(String(b.team || ''), 'es'))
}

function resolveLegacyPlayer(context, licenseId, season, teamHint, routeId, slot) {
  const rows = playerRows(context)
  if (rows.length === 0) {
    return {
      status: 'resolved',
      record: { licenseId, season: Number(season) || null, playerDisplay: 'Jugador' },
      teamId: null,
    }
  }

  const matches = matchingPlayers(context, licenseId, season, teamHint)
  if (matches.length === 0) {
    return { status: 'not-found', routeId, entity: 'player', slot, value: licenseId, season: Number(season) || null }
  }
  if (matches.length > 1 && !teamHint) {
    return { status: 'ambiguous', routeId, entity: 'player', slot, value: licenseId, season: Number(season) || null, candidates: matches }
  }

  const record = matches[0]
  let teamId = record.teamId || null
  if (!teamId && record.team && identitySource(context)) {
    const teamResolution = resolveTeamIdentity(identitySource(context), record.team, record.season)
    if (teamResolution.status === 'resolved') teamId = teamResolution.teamId
  }
  return { status: 'resolved', record, teamId }
}

function canonicalLegacyQuery(routeId, search, additions = {}) {
  const values = {
    ...parseRouteQuery(routeId, search).values,
    ...additions,
  }
  return serializeRouteQuery(routeId, values, { strict: false })
}

function legacySeason(value) {
  const raw = String(value ?? '')
  if (!/^\d{4}$/.test(raw)) return null
  const season = Number(raw)
  return season >= 2000 && season <= 2100 ? season : null
}

function invalidLegacySeason(routeId, value, slot = null) {
  return { status: 'not-found', routeId, entity: 'season', slot, value }
}

function playerName(record) {
  return record.playerDisplay || record.playerFull || record.fullName || record.name || record.player || 'Jugador'
}

function resolveSimpleLegacy(match, search) {
  const builders = {
    teamStats: buildTeamStatsPath,
    fourFactors: buildFourFactorsPath,
    games: buildGamesPath,
    playerStats: buildPlayerStatsPath,
    playerClutch: buildPlayerClutchPath,
    lineupRankings: buildLineupRankingsPath,
    shotCharts: buildShotChartsPath,
    zoneLeaders: buildZoneLeadersPath,
  }
  const build = builders[match.routeId]
  if (!build) return null
  return redirect(withQuery(build(), canonicalLegacyQuery(match.routeId, search)), match.routeId)
}

export function resolveSectionLocation(location) {
  const { pathname, search } = locationParts(location)
  const target = SECTION_DEFAULT_PATHS[pathname.replace(/^\//, '')]
  return target ? redirect(withQuery(target, search), null, 'section-default') : null
}

export function resolveLegacyLocation(location, context = {}) {
  const { pathname, search } = locationParts(location)
  const match = matchLegacyRoute(pathname)
  if (!match) return resolveSectionLocation({ pathname, search })
  const params = match.params
  const queryParams = new URLSearchParams(search)

  if (match.routeId === 'teamProfile') {
    if (!params.team) return redirect(withQuery(
      buildTeamProfilePath(),
      canonicalLegacyQuery(match.routeId, search),
    ), match.routeId)
    const season = legacySeason(params.season)
    if (season == null) return invalidLegacySeason(match.routeId, params.season)
    const teamId = resolveLegacyTeam(context, params.team, season, match.routeId)
    if (typeof teamId !== 'string') return teamId
    const query = canonicalLegacyQuery(match.routeId, search, { temporada: season })
    return redirect(withQuery(buildTeamProfilePath(teamId), query), match.routeId)
  }

  if (match.routeId === 'teamComparison') {
    if (!params.teamA || !params.teamB) return redirect(withQuery(
      buildTeamComparisonPath(),
      canonicalLegacyQuery(match.routeId, search),
    ), match.routeId)
    const season = legacySeason(params.season)
    if (season == null) return invalidLegacySeason(match.routeId, params.season)
    const teamAId = resolveLegacyTeam(context, params.teamA, season, match.routeId)
    if (typeof teamAId !== 'string') return teamAId
    const teamBId = resolveLegacyTeam(context, params.teamB, season, match.routeId)
    if (typeof teamBId !== 'string') return teamBId
    const query = canonicalLegacyQuery(match.routeId, search, { temporada: season })
    return redirect(withQuery(buildTeamComparisonPath(teamAId, teamBId), query), match.routeId)
  }

  if (match.routeId === 'playerProfile') {
    if (!params.licenseId) return redirect(withQuery(
      buildPlayerProfilePath(),
      canonicalLegacyQuery(match.routeId, search),
    ), match.routeId)
    const resolved = resolveLegacyPlayer(context, params.licenseId, null, queryParams.get('team'), match.routeId)
    if (resolved.status !== 'resolved') return resolved
    const query = canonicalLegacyQuery(match.routeId, search, {
      temporada: resolved.record.season,
      equipo: resolved.teamId,
    })
    return redirect(withQuery(buildPlayerProfilePath(resolved.record, playerName(resolved.record)), query), match.routeId)
  }

  if (match.routeId === 'playerSimilarity') {
    if (!params.licenseId) return redirect(withQuery(
      buildPlayerSimilarityPath(),
      canonicalLegacyQuery(match.routeId, search),
    ), match.routeId)
    const season = legacySeason(params.season)
    if (season == null) return invalidLegacySeason(match.routeId, params.season)
    const resolved = resolveLegacyPlayer(context, params.licenseId, season, queryParams.get('team'), match.routeId)
    if (resolved.status !== 'resolved') return resolved
    const query = canonicalLegacyQuery(match.routeId, search, {
      temporada: season,
      equipo: resolved.teamId,
    })
    return redirect(withQuery(buildPlayerSimilarityPath(resolved.record, playerName(resolved.record)), query), match.routeId)
  }

  if (match.routeId === 'playerComparison') {
    if (!params.aId || !params.bId) return redirect(withQuery(
      buildPlayerComparisonPath(),
      canonicalLegacyQuery(match.routeId, search),
    ), match.routeId)
    const seasonA = legacySeason(params.aSeason)
    const seasonB = legacySeason(params.bSeason)
    if (seasonA == null) return invalidLegacySeason(match.routeId, params.aSeason, 'a')
    if (seasonB == null) return invalidLegacySeason(match.routeId, params.bSeason, 'b')
    const playerA = resolveLegacyPlayer(context, params.aId, seasonA, queryParams.get('teamA'), match.routeId, 'a')
    if (playerA.status !== 'resolved') return playerA
    const playerB = resolveLegacyPlayer(context, params.bId, seasonB, queryParams.get('teamB'), match.routeId, 'b')
    if (playerB.status !== 'resolved') return playerB
    const query = canonicalLegacyQuery(match.routeId, search, {
      'temporada-a': seasonA,
      'equipo-a': playerA.teamId,
      'temporada-b': seasonB,
      'equipo-b': playerB.teamId,
    })
    return redirect(withQuery(buildPlayerComparisonPath(playerA.record, playerB.record), query), match.routeId)
  }

  if (match.routeId === 'lineupAnalysis') {
    if (!params.team) return redirect(withQuery(
      buildLineupAnalysisPath(),
      canonicalLegacyQuery(match.routeId, search),
    ), match.routeId)
    const season = legacySeason(params.season)
    if (season == null) return invalidLegacySeason(match.routeId, params.season)
    const teamId = resolveLegacyTeam(context, params.team, season, match.routeId)
    if (typeof teamId !== 'string') return teamId
    const query = canonicalLegacyQuery(match.routeId, search, { temporada: season })
    return redirect(withQuery(buildLineupAnalysisPath(teamId), query), match.routeId)
  }

  if (match.routeId === 'zoneLeaders' && params.metric) {
    const season = legacySeason(params.season)
    if (season == null) return invalidLegacySeason(match.routeId, params.season)
    const metrics = { 'maximo-anotador': 'points', 'mejor-eficiencia': 'fgPct' }
    if (!Object.hasOwn(metrics, params.metric)) {
      return { status: 'not-found', routeId: match.routeId, entity: 'metric', value: params.metric }
    }
    const query = canonicalLegacyQuery(match.routeId, search, {
      temporada: season,
      metrica: metrics[params.metric],
    })
    return redirect(withQuery(buildZoneLeadersPath(), query), match.routeId)
  }

  return resolveSimpleLegacy(match, search)
}

function findCanonicalPlayer(context, segment, season, teamId) {
  const parsed = parseEntitySegment(segment)
  if (!parsed) return null
  const matches = matchingPlayers(context, parsed.id, season, teamId)
  return matches[0] || null
}

export function resolveCanonicalEntityLocation(location, context = {}) {
  const { pathname, search } = locationParts(location)
  const match = matchCanonicalRoute(pathname)
  if (!match) return null
  const query = parseRouteQuery(match.routeId, search).values

  if (match.routeId === 'games' && match.params.game) {
    const parsed = parseEntitySegment(match.params.game)
    if (!parsed) return { status: 'not-found', routeId: match.routeId, entity: 'game' }
    const game = (context.games || []).find(item => String(item.matchId || item.id) === parsed.id)
    if (!game) return context.games ? { status: 'not-found', routeId: match.routeId, entity: 'game', value: parsed.id } : null
    const canonicalPath = buildGamesPath(game)
    return canonicalPath === pathname ? null : redirect(withQuery(canonicalPath, search), match.routeId, 'decorative-slug')
  }

  if (match.routeId === 'playerProfile' && match.params.player) {
    const rows = distinctPlayerRecords(playerRows(context))
    if (rows.length === 0) return null
    const segment = safeDecodePathSegment(match.params.player)
    const parsed = parseEntitySegment(segment)
    let licenseId = parsed?.id || null

    if (!licenseId) {
      if (!isTeamId(segment)) return { status: 'not-found', routeId: match.routeId, entity: 'player' }
      const matchingIds = [...new Set(rows
        .filter(row => getPlayerProfileSlug(row, playerName(row)) === segment)
        .map(row => String(row.licenseId)))]
      if (matchingIds.length === 0) return { status: 'not-found', routeId: match.routeId, entity: 'player', value: segment }
      if (matchingIds.length > 1) {
        return { status: 'ambiguous', routeId: match.routeId, entity: 'player', value: segment }
      }
      licenseId = matchingIds[0]
    }

    const player = matchingPlayers(context, licenseId, query.temporada, query.equipo)[0]
      || matchingPlayers(context, licenseId, null, null)[0]
    if (!player) return { status: 'not-found', routeId: match.routeId, entity: 'player', value: licenseId }

    const profileSlug = getPlayerProfileSlug(player, playerName(player))
    const slugIds = new Set(rows
      .filter(row => getPlayerProfileSlug(row, playerName(row)) === profileSlug)
      .map(row => String(row.licenseId)))
    const canonicalPath = slugIds.size > 1
      ? `/jugadores/perfil/${buildPlayerSegment(player, playerName(player))}`
      : buildPlayerProfilePath(player, playerName(player))
    return canonicalPath === pathname
      ? null
      : redirect(withQuery(canonicalPath, search), match.routeId, 'profile-slug')
  }

  const playerSegments = match.routeId === 'playerComparison'
    ? [['playerA', 'a'], ['playerB', 'b']]
    : match.params.player
      ? [['player', null]]
      : []
  if (playerSegments.length === 0) return null

  const canonicalSegments = {}
  for (const [key, slot] of playerSegments) {
    const season = slot ? query[`temporada-${slot}`] : query.temporada
    const teamId = slot ? query[`equipo-${slot}`] : query.equipo
    const parsed = parseEntitySegment(match.params[key])
    if (!parsed) return { status: 'not-found', routeId: match.routeId, entity: 'player' }
    const player = findCanonicalPlayer(context, match.params[key], season, teamId)
    if (!player) {
      const knownPlayer = distinctPlayerRecords(playerRows(context)).find(row => String(row.licenseId) === parsed.id)
      if (!knownPlayer && playerRows(context).length > 0) {
        return { status: 'not-found', routeId: match.routeId, entity: 'player', value: parsed.id }
      }
      if (!knownPlayer) return null
      canonicalSegments[key] = buildPlayerSegment(knownPlayer, playerName(knownPlayer))
      continue
    }
    canonicalSegments[key] = buildPlayerSegment(player, playerName(player))
  }

  let canonicalPath
  if (match.routeId === 'playerProfile') canonicalPath = buildPlayerProfilePath(canonicalSegments.player)
  else if (match.routeId === 'playerSimilarity') canonicalPath = buildPlayerSimilarityPath(canonicalSegments.player)
  else if (match.routeId === 'playerComparison') canonicalPath = buildPlayerComparisonPath(canonicalSegments.playerA, canonicalSegments.playerB)
  else if (match.routeId === 'shotCharts') canonicalPath = buildPlayerShotChartPath(canonicalSegments.player)
  if (!canonicalPath || canonicalPath === pathname) return null
  return redirect(withQuery(canonicalPath, search), match.routeId, 'decorative-slug')
}
