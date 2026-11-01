import { readdirSync, readFileSync, writeFileSync } from 'node:fs'

// Write small files derived from the full exports, so tools that only need a
// slice of them don't download and parse tens of MB:
// - player-index.json: ids, names, team and position from players.json
//   (lineups, games, shots and clutch only need these to name players)
// - onoff-YYYY.json: the per-player on/off ratings from lineups-YYYY.json
//   (the On/Off cards in the player profile and comparison)
const dataDir = new URL('../public/data/', import.meta.url)
const readJson = file => JSON.parse(readFileSync(new URL(file, dataDir), 'utf8'))
const writeJson = (file, value) => writeFileSync(new URL(file, dataDir), JSON.stringify(value))

const players = readJson('players.json')
const playerIndex = players.map(row => ({
  licenseId: row.licenseId,
  season: row.season,
  team: row.team,
  teamId: row.teamId,
  player: row.player,
  playerAbbrev: row.playerAbbrev,
  playerFull: row.playerFull,
  position: row.position,
}))
writeJson('player-index.json', playerIndex)

const lineupFiles = readdirSync(dataDir).filter(file => /^lineups-\d{4}\.json$/.test(file))
for (const file of lineupFiles) {
  const lineups = readJson(file)
  const data = Object.fromEntries(Object.entries(lineups.data).map(([team, teamData]) => [
    team,
    { team: teamData.team, teamId: teamData.teamId, players: teamData.players },
  ]))
  writeJson(file.replace('lineups-', 'onoff-'), { season: lineups.season, data })
}

console.log(`Light data: player-index.json (${playerIndex.length} rows), ${lineupFiles.length} onoff files`)
