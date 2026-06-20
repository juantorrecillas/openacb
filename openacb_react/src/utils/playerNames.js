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

export function getPlayerSearchText(player) {
  if (!player) return ''

  return [
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
}
