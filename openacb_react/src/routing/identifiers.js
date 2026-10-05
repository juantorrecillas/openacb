const NUMERIC_ID_PATTERN = /^[1-9]\d*$/
const SLUG_PATTERN = /^[a-z0-9]+(?:-[a-z0-9]+)*$/

export function safeDecodePathSegment(value) {
  try {
    return decodeURIComponent(String(value ?? ''))
  } catch {
    return String(value ?? '')
  }
}

export function slugify(value, fallback = '') {
  const slug = safeDecodePathSegment(value)
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .toLocaleLowerCase('es')
    .replace(/&/g, ' y ')
    .replace(/[^a-z0-9]+/g, '-')
    .replace(/^-+|-+$/g, '')
    .replace(/-{2,}/g, '-')

  return slug || fallback
}

export function normalizeNumericId(value) {
  const candidate = String(value ?? '').trim()
  return NUMERIC_ID_PATTERN.test(candidate) ? candidate : null
}

export function isTeamId(value) {
  return SLUG_PATTERN.test(String(value ?? ''))
}

export function buildEntitySegment(label, id, fallback = 'entidad') {
  const numericId = normalizeNumericId(id)
  if (!numericId) throw new TypeError(`Identificador numérico no válido: ${id}`)
  return `${slugify(label, fallback)}--${numericId}`
}

export function parseEntitySegment(segment) {
  const decoded = safeDecodePathSegment(segment)
  const match = decoded.match(/^([a-z0-9]+(?:-[a-z0-9]+)*)--([1-9]\d*)$/)
  if (!match) return null

  return {
    segment: decoded,
    slug: match[1],
    id: match[2],
  }
}

export function canonicalizeEntitySegment(segment, label, fallback = 'entidad') {
  const parsed = parseEntitySegment(segment)
  return parsed ? buildEntitySegment(label, parsed.id, fallback) : null
}

export function isCanonicalEntitySegment(segment, label, fallback = 'entidad') {
  const canonical = canonicalizeEntitySegment(segment, label, fallback)
  return canonical != null && canonical === safeDecodePathSegment(segment)
}

function entityId(value, fields) {
  if (value == null || typeof value !== 'object') return normalizeNumericId(value)
  for (const field of fields) {
    const id = normalizeNumericId(value[field])
    if (id) return id
  }
  return null
}

export function getPlayerUrlName(player, fallback = 'jugador') {
  if (!player || typeof player !== 'object') return fallback
  return player.playerDisplay
    || player.playerFull
    || player.fullName
    || player.name
    || player.nickname
    || player.player
    || fallback
}

export function getPlayerProfileSlug(player, name) {
  return slugify(name || getPlayerUrlName(player), 'jugador')
}

export function buildPlayerSegment(playerOrId, name) {
  const id = entityId(playerOrId, ['licenseId', 'id'])
  const label = name || getPlayerUrlName(playerOrId)
  return buildEntitySegment(label, id, 'jugador')
}

export function getGameUrlName(game, fallback = 'partido') {
  if (!game || typeof game !== 'object') return fallback
  if (game.local && game.visitor) return `${game.local} ${game.visitor}`
  return game.label || game.name || fallback
}

export function buildGameSegment(gameOrId, label) {
  const id = entityId(gameOrId, ['matchId', 'id', 'gameId'])
  return buildEntitySegment(label || getGameUrlName(gameOrId), id, 'partido')
}

export function parsePlayerSegment(segment) {
  return parseEntitySegment(segment)
}

export function parseGameSegment(segment) {
  return parseEntitySegment(segment)
}
