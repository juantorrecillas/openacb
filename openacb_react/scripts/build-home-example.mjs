import { readFileSync, writeFileSync } from 'node:fs'
import { getTeamRadarAxes } from '../src/utils/teamRadar.js'
import { readRouteQuery } from '../src/routing/query.js'

// Refresh the team radar from the exact source and reference used by TeamFingerprint.
// Preserve the separate player, lineup and shot examples.
const snapshotPath = new URL('../src/data/home-tool-examples.json', import.meta.url)
const snapshot = JSON.parse(readFileSync(snapshotPath, 'utf8'))
const teams = JSON.parse(readFileSync(new URL('../public/data/teams.json', import.meta.url), 'utf8'))
const url = new URL(snapshot.team.path, 'http://openacb.local')
const season = Number(url.searchParams.get('temporada'))
const teamId = url.pathname.split('/').at(-1)
const team = teams.find(row => row.season === season && row.teamId === teamId)
if (!team) throw new Error(`Missing team-season: ${teamId}, ${season}`)
const referenceType = readRouteQuery('teamProfile', url.searchParams).referencia
const reference = referenceType === 'season' ? teams.filter(row => row.season === season) : teams
const axes = getTeamRadarAxes(team, reference)
if (axes.some(axis => !Number.isFinite(axis.value))) throw new Error('Missing team radar metrics')
snapshot.team.name = team.team
snapshot.team.seasonLabel = `${season - 1}–${String(season).slice(-2)}`
snapshot.team.axes = axes
snapshot.team.reference = {
  type: reference === teams ? 'historical' : 'season',
  source: 'teams.json',
  observations: reference.length,
  seasons: [...new Set(reference.map(row => row.season))].sort((a, b) => a - b),
}
writeFileSync(snapshotPath, `${JSON.stringify(snapshot, null, 2)}\n`)
console.log(JSON.stringify({ team: team.team, season, reference: snapshot.team.reference, axes }, null, 2))
