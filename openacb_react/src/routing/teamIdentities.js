import { isTeamId, slugify } from './identifiers.js'

export const TEAM_IDENTITIES_URL = '/data/team-identities.json'

function seasonNumber(value) {
  const season = Number(value)
  return Number.isInteger(season) ? season : null
}

function normalizeSeasons(value) {
  if (value == null) return []
  const values = Array.isArray(value) ? value : [value]
  return [...new Set(values.map(seasonNumber).filter(season => season != null))].sort((a, b) => a - b)
}

export function normalizeTeamIdentityKey(value) {
  return slugify(value)
}

function normalizeAlias(alias) {
  if (typeof alias === 'string') return { name: alias, seasons: [] }
  if (!alias || typeof alias !== 'object' || !alias.name) return null
  return {
    name: String(alias.name),
    seasons: normalizeSeasons(alias.seasons),
  }
}

function addAlias(aliasMap, team, alias) {
  const key = normalizeTeamIdentityKey(alias.name)
  if (!key) return
  const candidates = aliasMap.get(key) || []
  candidates.push({
    teamId: team.teamId,
    team,
    name: alias.name,
    seasons: alias.seasons,
  })
  aliasMap.set(key, candidates)
}

function makeIndex(teams) {
  const byId = new Map()
  const byAlias = new Map()

  for (const team of teams) {
    if (!isTeamId(team.teamId)) throw new TypeError(`teamId no válido: ${team.teamId}`)
    if (byId.has(team.teamId)) throw new TypeError(`teamId duplicado: ${team.teamId}`)
    byId.set(team.teamId, team)
    for (const alias of team.aliases) addAlias(byAlias, team, alias)
  }

  return Object.freeze({
    kind: 'team-identity-index',
    teams: Object.freeze(teams),
    byId,
    byAlias,
  })
}

export function createTeamIdentityIndex(registry) {
  const sourceTeams = Array.isArray(registry) ? registry : registry?.teams
  if (!Array.isArray(sourceTeams)) throw new TypeError('El registro debe contener un array teams')

  const teams = sourceTeams.map(source => {
    const aliases = (source.aliases || []).map(normalizeAlias).filter(Boolean)
    const fallbackName = source.name || source.canonicalName || source.displayName
    if (aliases.length === 0 && fallbackName) aliases.push({ name: String(fallbackName), seasons: [] })
    return Object.freeze({
      ...source,
      teamId: String(source.teamId || ''),
      aliases: Object.freeze(aliases),
    })
  })

  return makeIndex(teams)
}

export function createTeamIdentityIndexFromRows(rows) {
  if (!Array.isArray(rows)) throw new TypeError('Las filas de equipos deben ser un array')
  const grouped = new Map()

  for (const row of rows) {
    if (!row?.teamId || !row?.team) continue
    const teamId = String(row.teamId)
    const season = seasonNumber(row.season)
    let team = grouped.get(teamId)
    if (!team) {
      team = { teamId, aliases: new Map() }
      grouped.set(teamId, team)
    }

    const name = String(row.team)
    const key = normalizeTeamIdentityKey(name)
    let alias = team.aliases.get(key)
    if (!alias) {
      alias = { name, seasons: new Set() }
      team.aliases.set(key, alias)
    }
    if (season != null) alias.seasons.add(season)
  }

  const teams = [...grouped.values()].map(team => Object.freeze({
    teamId: team.teamId,
    aliases: Object.freeze([...team.aliases.values()].map(alias => Object.freeze({
      name: alias.name,
      seasons: Object.freeze([...alias.seasons].sort((a, b) => a - b)),
    }))),
  }))

  return makeIndex(teams)
}

export function isTeamIdentityIndex(value) {
  return value?.kind === 'team-identity-index' && value.byId instanceof Map && value.byAlias instanceof Map
}

export function ensureTeamIdentityIndex(source) {
  if (isTeamIdentityIndex(source)) return source
  if (Array.isArray(source) && source.some(row => row?.team && !row?.aliases)) {
    return createTeamIdentityIndexFromRows(source)
  }
  return createTeamIdentityIndex(source)
}

function supportsSeason(alias, season) {
  const target = seasonNumber(season)
  return target == null || alias.seasons.length === 0 || alias.seasons.includes(target)
}

export function resolveTeamIdentity(source, nameOrId, season) {
  const index = ensureTeamIdentityIndex(source)
  const value = String(nameOrId ?? '').trim()
  if (!value) return { status: 'not-found', value, candidates: [] }

  if (index.byId.has(value)) {
    const team = index.byId.get(value)
    return { status: 'resolved', value, teamId: value, team, candidates: [team] }
  }

  const key = normalizeTeamIdentityKey(value)
  const allCandidates = index.byAlias.get(key) || []
  const matching = allCandidates.filter(candidate => supportsSeason(candidate, season))
  const uniqueIds = [...new Set(matching.map(candidate => candidate.teamId))]

  if (uniqueIds.length === 1) {
    const teamId = uniqueIds[0]
    return {
      status: 'resolved',
      value,
      teamId,
      team: index.byId.get(teamId),
      alias: matching.find(candidate => candidate.teamId === teamId),
      candidates: matching,
    }
  }

  if (uniqueIds.length > 1) {
    return { status: 'ambiguous', value, candidates: matching }
  }

  return {
    status: allCandidates.length > 0 ? 'out-of-season' : 'not-found',
    value,
    candidates: allCandidates,
  }
}

export function resolveTeamId(source, nameOrId, season) {
  const result = resolveTeamIdentity(source, nameOrId, season)
  return result.status === 'resolved' ? result.teamId : null
}

export function getTeamIdentity(source, teamId) {
  return ensureTeamIdentityIndex(source).byId.get(String(teamId)) || null
}

export function getTeamName(source, teamId, season) {
  const team = getTeamIdentity(source, teamId)
  if (!team) return null
  const matchingAlias = team.aliases.find(alias => supportsSeason(alias, season))
  return matchingAlias?.name || team.name || team.canonicalName || team.displayName || team.aliases[0]?.name || null
}

export function createTeamIdentityResolver(source) {
  const index = ensureTeamIdentityIndex(source)
  return Object.freeze({
    index,
    resolve: (nameOrId, season) => resolveTeamIdentity(index, nameOrId, season),
    resolveId: (nameOrId, season) => resolveTeamId(index, nameOrId, season),
    get: teamId => getTeamIdentity(index, teamId),
    getName: (teamId, season) => getTeamName(index, teamId, season),
  })
}

export async function fetchTeamIdentities(options = {}) {
  const fetcher = options.fetcher || globalThis.fetch
  const url = options.url || TEAM_IDENTITIES_URL
  if (typeof fetcher !== 'function') throw new TypeError('No hay una implementación de fetch disponible')
  const response = await fetcher(url)
  if (!response.ok) throw new Error(`No se pudo cargar ${url}`)
  return response.json()
}

export async function loadTeamIdentityIndex(options = {}) {
  if (options.rows) return createTeamIdentityIndexFromRows(options.rows)
  const registry = options.registry || await fetchTeamIdentities(options)
  return createTeamIdentityIndex(registry)
}
