import { readFileSync, writeFileSync } from 'node:fs'
import { getZonePolygons } from '../src/utils/shotZones.js'

// Refresh only the shot preview; keep the other landing examples unchanged.
const snapshotPath = new URL('../src/data/home-tool-examples.json', import.meta.url)
const snapshot = JSON.parse(readFileSync(snapshotPath, 'utf8'))
const season = new URL(snapshot.team.path, 'https://openacb.local').searchParams.get('temporada')
const shots = JSON.parse(readFileSync(new URL(`../public/data/shots-${season}.json`, import.meta.url), 'utf8'))
  .filter(shot => shot.competitionStage === 'regular')
const zones = Object.keys(getZonePolygons())
const aggregate = rows => {
  const totals = Object.fromEntries(zones.map(zone => [zone, { att: 0, made: 0, points: 0 }]))
  for (const shot of rows) {
    const total = totals[shot.zoned || shot.zone]
    if (!total) continue
    total.att += 1
    const made = [true, 1, 'true', '1'].includes(shot.made)
    if (made) {
      total.made += 1
      total.points += Number(shot.points) || 0
    }
  }
  return totals
}
const team = aggregate(shots.filter(shot => shot.team === snapshot.team.name))
const league = aggregate(shots)
for (const zone of zones) {
  if (!team[zone].att || !league[zone].att) throw new Error(`Missing source shots for ${zone}`)
}
const efg = total => Number((50 * total.points / total.att).toFixed(1))
snapshot.team.compactZones = {
  stats: Object.fromEntries(zones.map(zone => [zone, { att: team[zone].att, made: team[zone].made, efg: efg(team[zone]) }])),
  leagueEfg: Object.fromEntries(zones.map(zone => [zone, efg(league[zone])])),
}
writeFileSync(snapshotPath, `${JSON.stringify(snapshot, null, 2)}\n`)
console.log(`${snapshot.team.name}, regular season ${season}: ${zones.length} zones, ${Object.values(team).reduce((sum, zone) => sum + zone.att, 0)} attempts.`)
