const clean = value => typeof value === 'string' && value.trim() ? value.trim() : null

export function getPlayerDisplayName(player, fallback = '-') {
  if (!player) return fallback

  return clean(player.playerDisplay)
    || clean(player.playerName)
    || clean(player.displayName)
    || clean(player.name)
    || clean(player.playerFull)
    || clean(player.playerAbbrev)
    || clean(player.player)
    || clean(player.nickname)
    || clean(player.nick)
    || fallback
}

export function getPlayerCompactName(player, fallback = '-') {
  const fullName = getPlayerDisplayName(player, fallback)
  const abbreviation = clean(player?.playerAbbrev)

  if (abbreviation) {
    const alreadyHasInitial = /^\p{L}\.(?:\s*\p{L}\.)*\s+/u.test(abbreviation)
    if (alreadyHasInitial) return abbreviation

    const initial = Array.from(fullName)[0]
    return initial ? `${initial}. ${abbreviation}` : abbreviation
  }

  const parts = fullName.split(/\s+/).filter(Boolean)
  if (parts.length < 2) return fullName
  return `${Array.from(parts[0])[0]}. ${parts.slice(1).join(' ')}`
}

export function getPlayerSearchText(player) {
  if (!player) return ''

  const searchText = [
    player.playerDisplay,
    player.playerName,
    player.displayName,
    player.name,
    player.playerFull,
    player.playerAbbrev,
    player.player,
    player.nickname,
    player.nick,
  ]
    .map(clean)
    .filter(Boolean)
    .join(' ')
    .toLocaleLowerCase('es')

  const withoutAccents = searchText.normalize('NFD').replace(/[\u0300-\u036f]/g, '')
  return withoutAccents === searchText ? searchText : `${searchText} ${withoutAccents}`
}
