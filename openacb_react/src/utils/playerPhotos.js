/**
 * Get the best available photo URL for a player.
 *
 * player-photos.json is keyed { licenseId: { season: url } }.
 * If an exact season match exists, return it.
 * Otherwise return the photo from the most recent available season.
 */
export function getPlayerPhoto(playerPhotos, licenseId, season) {
  const byYear = playerPhotos[String(licenseId)]
  if (!byYear || typeof byYear !== 'object') return null
  if (season != null && byYear[String(season)]) return byYear[String(season)]
  // Fall back to most recent
  const years = Object.keys(byYear).map(Number).sort((a, b) => b - a)
  return years.length > 0 ? byYear[String(years[0])] : null
}
