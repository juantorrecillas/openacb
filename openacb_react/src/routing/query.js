import { isTeamId, normalizeNumericId } from './identifiers.js'

const TOKEN_PATTERN = /^[a-zA-Z0-9._~-]+$/
const SEASON_MIN = 2000
const SEASON_MAX = 2100

export function queryField(key, type = 'token', options = {}) {
  return Object.freeze({ key, type, ...options })
}

export function enumQueryField(key, values, options = {}) {
  const publicValues = options.publicValues || {}
  const parseValues = Object.fromEntries(values.map(value => [String(value), value]))
  for (const [internal, publicValue] of Object.entries(publicValues)) parseValues[String(publicValue)] = internal
  Object.assign(parseValues, options.aliases || {})
  return queryField(key, 'enum', {
    ...options,
    values: Object.freeze([...values]),
    parseValues: Object.freeze(parseValues),
    publicValues: Object.freeze({ ...publicValues }),
  })
}

export const seasonQueryField = (key = 'temporada', options = {}) => queryField(key, 'season', options)
export const integerQueryField = (key, options = {}) => queryField(key, 'integer', options)
export const tokenQueryField = (key, options = {}) => queryField(key, 'token', options)
export const textQueryField = (key, options = {}) => queryField(key, 'text', options)
export const teamIdQueryField = (key = 'equipo', options = {}) => queryField(key, 'teamId', options)
export const booleanQueryField = (key, options = {}) => queryField(key, 'boolean', options)
export const idListQueryField = (key, options = {}) => queryField(key, 'idList', options)

function enumField(key, values, publicValues, defaultValue) {
  return enumQueryField(key, values, { publicValues, defaultValue })
}

const season = (key = 'temporada', options = {}) => seasonQueryField(key, {
  required: true,
  allowAll: false,
  ...options,
})
const metric = (key = 'metrica', defaultValue) => tokenQueryField(key, { defaultValue })
const team = (key = 'equipo') => teamIdQueryField(key)

const stage = enumField('fase', ['regular', 'playoffs'], {}, 'regular')
const side = enumField('lado', ['attack', 'defense'], { attack: 'ataque', defense: 'defensa' }, 'attack')
const shotMetric = enumField('metrica', ['efficiency', 'frequency'], {
  efficiency: 'eficiencia',
  frequency: 'frecuencia',
}, 'efficiency')

const ROUTE_QUERY_SCHEMA_DEFINITIONS = {
  home: Object.freeze([]),
  teamStats: Object.freeze([
    season('temporada', { allowAll: true }),
    stage,
    enumField('vista', ['basic', 'advanced', 'oppBasic', 'oppAdvanced'], {
      basic: 'basico',
      advanced: 'avanzado',
      oppBasic: 'rival-basico',
      oppAdvanced: 'rival-avanzado',
    }, 'basic'),
    metric('x', 'ortg'),
    metric('y', 'drtg'),
  ]),
  teamProfile: Object.freeze([
    season(),
    metric('tendencia', 'ortg'),
    side,
    shotMetric,
  ]),
  teamComparison: Object.freeze([season(), side, shotMetric]),
  fourFactors: Object.freeze([season('temporada', { allowAll: true })]),
  teamQuarters: Object.freeze([
    season(),
    team(),
    enumField('vista', ['diff', 'scored', 'allowed'], {
      diff: 'diferencial',
      scored: 'anotado',
      allowed: 'recibido',
    }, 'diff'),
  ]),
  teamClutch: Object.freeze([
    season(),
    enumField('vista', ['basic', 'advanced', 'opponent', 'opponentAdvanced'], {
      basic: 'basico',
      advanced: 'avanzado',
      opponent: 'rival',
      opponentAdvanced: 'rival-avanzado',
    }, 'basic'),
  ]),
  games: Object.freeze([season(), textQueryField('ronda', { maxLength: 100 })]),
  playerStats: Object.freeze([
    season('temporada', { allowAll: true }),
    stage,
    enumField('vista', ['basic', 'advanced', 'absolutes', 'misc', 'frequency', 'accuracy', 'defense'], {
      basic: 'basico',
      advanced: 'avanzado',
      absolutes: 'absolutos',
      misc: 'otros',
      frequency: 'frecuencia',
      accuracy: 'precision',
      defense: 'defensa',
    }, 'basic'),
    team(),
    enumQueryField('posicion', ['Base', 'Escolta', 'Alero', 'Ala-pívot', 'Pívot'], {
      publicValues: {
        Base: 'base',
        Escolta: 'escolta',
        Alero: 'alero',
        'Ala-pívot': 'ala-pivot',
        'Pívot': 'pivot',
      },
    }),
    enumField('referencia', ['league', 'position'], { league: 'liga', position: 'posicion' }, 'league'),
    booleanQueryField('minimos', { defaultValue: true }),
  ]),
  playerProfile: Object.freeze([
    season(),
    team(),
    stage,
    tokenQueryField('tabla', { defaultValue: 'basico' }),
    enumField('radar', ['league', 'position'], { league: 'liga', position: 'posicion' }, 'league'),
    enumField('percentiles', ['league', 'position'], { league: 'liga', position: 'posicion' }, 'league'),
    enumField('tiro', ['own', 'rival'], { own: 'propio', rival: 'rival' }, 'own'),
  ]),
  playerSimilarity: Object.freeze([season(), team()]),
  playerComparison: Object.freeze([
    season('temporada-a'),
    team('equipo-a'),
    season('temporada-b'),
    team('equipo-b'),
    enumField('referencia', ['league', 'position'], { league: 'liga', position: 'posicion' }, 'league'),
  ]),
  playerClutch: Object.freeze([
    season(),
    integerQueryField('min-partidos', { min: 1, max: 100, defaultValue: 3 }),
    team(),
    enumQueryField('posicion', ['Base', 'Escolta', 'Alero', 'Ala-pívot', 'Pívot'], {
      publicValues: {
        Base: 'base',
        Escolta: 'escolta',
        Alero: 'alero',
        'Ala-pívot': 'ala-pivot',
        'Pívot': 'pivot',
      },
    }),
    enumField('vista', ['basic', 'advanced', 'absolutes'], {
      basic: 'basico',
      advanced: 'avanzado',
      absolutes: 'absolutos',
    }, 'basic'),
  ]),
  lineupAnalysis: Object.freeze([
    season(),
    idListQueryField('con'),
    idListQueryField('sin', { maxItems: 1 }),
  ]),
  lineupRankings: Object.freeze([
    season(),
    team(),
    enumField('categoria', ['players', 'pairs', 'trios', 'lineups'], {
      players: 'jugadores',
      pairs: 'duos',
      trios: 'trios',
      lineups: 'quintetos',
    }, 'players'),
    enumField('metrica', ['impact', 'offense'], { impact: 'impacto', offense: 'eficiencia' }, 'impact'),
    enumField('extremo', ['top', 'bottom'], { top: 'mejores', bottom: 'peores' }, 'top'),
  ]),
  shotCharts: Object.freeze([
    season(),
    enumField('tipo', ['team', 'player'], { team: 'equipo', player: 'jugador' }, 'team'),
    enumField('resultado', ['all', 'made', 'missed'], {
      all: 'todos',
      made: 'anotados',
      missed: 'fallados',
    }, 'all'),
    enumField('vista', ['shots', 'heatmap', 'zones'], {
      shots: 'tiros',
      heatmap: 'mapa-calor',
      zones: 'zonas',
    }, 'shots'),
    enumField('mapa', ['frequency', 'density'], { frequency: 'frecuencia', density: 'densidad' }, 'frequency'),
    enumField('zonas', ['efficiency', 'frequency'], { efficiency: 'eficiencia', frequency: 'frecuencia' }, 'efficiency'),
    team(),
  ]),
  zoneLeaders: Object.freeze([
    season(),
    enumField('metrica', ['points', 'fgPct'], { points: 'puntos', fgPct: 'porcentaje' }, 'points'),
    team(),
    integerQueryField('min-intentos', { min: 3, max: 50, defaultValue: 15 }),
  ]),
  about: Object.freeze([]),
}

export const ROUTE_QUERY_SCHEMAS = Object.freeze({
  ...ROUTE_QUERY_SCHEMA_DEFINITIONS,
  teamShotChart: ROUTE_QUERY_SCHEMA_DEFINITIONS.shotCharts,
  playerShotChart: ROUTE_QUERY_SCHEMA_DEFINITIONS.shotCharts,
})

function asSearchParams(input) {
  if (input instanceof URLSearchParams) return new URLSearchParams(input)
  if (typeof input === 'string') return new URLSearchParams(input.startsWith('?') ? input.slice(1) : input)
  if (input?.searchParams instanceof URLSearchParams) return new URLSearchParams(input.searchParams)
  if (input?.search != null) return asSearchParams(input.search)
  const params = new URLSearchParams()
  for (const [key, value] of Object.entries(input || {})) {
    if (value == null) continue
    if (Array.isArray(value)) value.forEach(item => params.append(key, String(item)))
    else params.set(key, String(value))
  }
  return params
}

function sortNumericIds(values) {
  return [...new Set(values)].sort((a, b) => a.length - b.length || a.localeCompare(b))
}

function parsed(value, valid) {
  return { value, valid }
}

function containsControlCharacter(value) {
  return [...value].some(character => {
    const code = character.charCodeAt(0)
    return code <= 31 || code === 127
  })
}

function parseValue(raw, field) {
  if (field.type === 'season') {
    if (raw === 'todas' || raw === 'all') return parsed('all', field.allowAll !== false)
    if (!/^\d{4}$/.test(raw)) return parsed(undefined, false)
    const value = Number(raw)
    return parsed(value, value >= (field.min || SEASON_MIN) && value <= (field.max || SEASON_MAX))
  }

  if (field.type === 'integer') {
    if (!/^-?\d+$/.test(raw)) return parsed(undefined, false)
    const value = Number(raw)
    const valid = Number.isSafeInteger(value)
      && (field.min == null || value >= field.min)
      && (field.max == null || value <= field.max)
    return parsed(value, valid)
  }

  if (field.type === 'enum') {
    const value = field.parseValues[raw]
    return parsed(value, value !== undefined)
  }

  if (field.type === 'boolean') {
    if (['si', 'true', '1'].includes(raw)) return parsed(true, true)
    if (['no', 'false', '0'].includes(raw)) return parsed(false, true)
    return parsed(undefined, false)
  }

  if (field.type === 'idList') {
    const ids = raw.split(',').filter(Boolean).map(normalizeNumericId)
    const valid = ids.length > 0 && ids.every(Boolean) && (field.maxItems == null || ids.length <= field.maxItems)
    return parsed(valid ? sortNumericIds(ids) : undefined, valid)
  }

  if (field.type === 'teamId') return parsed(raw, isTeamId(raw))
  if (field.type === 'token') return parsed(raw, TOKEN_PATTERN.test(raw))
  if (field.type === 'text') {
    const value = raw.trim()
    const valid = value.length > 0
      && value.length <= (field.maxLength || 200)
      && !containsControlCharacter(value)
    return parsed(value, valid)
  }

  return parsed(undefined, false)
}

function fieldDefault(field, defaults) {
  return Object.hasOwn(defaults, field.key) ? defaults[field.key] : field.defaultValue
}

function sameValue(left, right) {
  if (Array.isArray(left) || Array.isArray(right)) {
    return Array.isArray(left) && Array.isArray(right)
      && left.length === right.length
      && left.every((value, index) => value === right[index])
  }
  return left === right
}

function publicValue(value, field) {
  if (field.type === 'season') return value === 'all' ? 'todas' : String(value)
  if (field.type === 'enum') return String(field.publicValues[value] ?? value)
  if (field.type === 'boolean') return value ? 'si' : 'no'
  if (field.type === 'idList') return sortNumericIds(value.map(String)).join(',')
  return String(value)
}

function encoded(value) {
  return encodeURIComponent(value).replace(/%2C/gi, ',')
}

export function serializeQuery(values, schema, options = {}) {
  const defaults = options.defaults || {}
  const pairs = []

  for (const field of schema) {
    const value = values?.[field.key]
    if (value == null || value === '') continue
    const defaultValue = fieldDefault(field, defaults)
    if (options.includeDefaults !== true && defaultValue !== undefined && sameValue(value, defaultValue)) continue

    const raw = publicValue(value, field)
    const validation = parseValue(raw, field)
    const internallyValid = field.type === 'enum'
      ? field.values.includes(value)
      : field.type === 'boolean'
        ? typeof value === 'boolean'
        : validation.valid
    if (!internallyValid) {
      if (options.strict !== false) throw new TypeError(`Valor no válido para ${field.key}: ${value}`)
      continue
    }
    pairs.push(`${encoded(field.key)}=${encoded(raw)}`)
  }

  return pairs.join('&')
}

export function parseQuery(input, schema, options = {}) {
  const params = asSearchParams(input)
  const defaults = options.defaults || {}
  const schemaKeys = new Set(schema.map(field => field.key))
  const values = {}
  const invalidKeys = []
  const duplicateKeys = []

  for (const field of schema) {
    const rawValues = params.getAll(field.key)
    if (rawValues.length > 1) duplicateKeys.push(field.key)
    const raw = rawValues[0]
    if (raw == null || raw === '') {
      const defaultValue = fieldDefault(field, defaults)
      if (defaultValue !== undefined) values[field.key] = defaultValue
      continue
    }

    const result = parseValue(raw, field)
    if (result.valid) values[field.key] = result.value
    else {
      invalidKeys.push(field.key)
      const defaultValue = fieldDefault(field, defaults)
      if (defaultValue !== undefined) values[field.key] = defaultValue
    }
  }

  const unknownKeys = [...new Set([...params.keys()].filter(key => !schemaKeys.has(key)))]
  const canonicalSearch = serializeQuery(values, schema, { defaults, strict: false })
  const sourceSearch = typeof input === 'string'
    ? input.replace(/^\?/, '')
    : params.toString()

  return {
    values,
    invalidKeys,
    duplicateKeys,
    unknownKeys,
    canonicalSearch,
    needsCanonicalization: sourceSearch !== canonicalSearch,
  }
}

export function getRouteQuerySchema(routeId) {
  const schema = ROUTE_QUERY_SCHEMAS[routeId]
  if (!schema) throw new TypeError(`Ruta desconocida: ${routeId}`)
  return schema
}

export function parseRouteQuery(routeId, input, options) {
  return parseQuery(input, getRouteQuerySchema(routeId), options)
}

export function readRouteQuery(routeId, input, options) {
  return parseRouteQuery(routeId, input, options).values
}

export function serializeRouteQuery(routeId, values, options) {
  return serializeQuery(values, getRouteQuerySchema(routeId), options)
}

export function withQuery(pathname, search) {
  const query = typeof search === 'string' ? search.replace(/^\?/, '') : new URLSearchParams(search || {}).toString()
  return query ? `${pathname}?${query}` : pathname
}
