import { buildPlayerSegment, getPlayerProfileSlug } from '../routing/identifiers.js'

// Profile URL slug per licenseId. A slug shared by several players gets the id appended.
// The build script uses the same function, so prerendered pages get the app's URLs.
export function buildProfileSlugs(rows, names) {
  const idsBySlug = new Map()
  const slugById = new Map()
  const playerById = new Map()

  rows.forEach(player => {
    const id = String(player.licenseId)
    const slug = getPlayerProfileSlug(player, names[id])
    if (!idsBySlug.has(slug)) idsBySlug.set(slug, new Set())
    idsBySlug.get(slug).add(id)
    slugById.set(id, slug)
    if (!playerById.has(id)) playerById.set(id, player)
  })

  return Object.fromEntries([...slugById].map(([id, slug]) => [
    id,
    idsBySlug.get(slug).size > 1
      ? buildPlayerSegment(playerById.get(id), names[id])
      : slug,
  ]))
}

// Adds playerDisplay and profileSlug to each row in place. Copying the rows
// instead ({ ...player }) took ~0.5 s on desktop for the 179-column player files.
// The returned array is new so components that memoize on it recompute.
export function addPlayerNames(rows, names, slugs) {
  if (!rows) return []
  rows.forEach(player => {
    const id = String(player.licenseId)
    player.playerDisplay = names[id] || undefined
    player.profileSlug = slugs[id]
  })
  return rows.slice()
}
