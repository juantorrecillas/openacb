import { readFileSync } from 'node:fs'
import { describe, expect, test } from 'vitest'
import { resolveCanonicalEntityLocation } from '../routing/legacy.js'
import { addPlayerNames, buildProfileSlugs } from '../utils/profileSlugs.js'
import { buildLineupPage, buildPlayerPage, buildTeamPage, buildTeamShotPage, seasonLabel } from './entities.js'

const row = (overrides) => ({
  licenseId: 20211331, season: 2027, team: 'Real Madrid', teamId: 'real-madrid', games: 6, mpg: 20.4,
  ppg: 8.8, rpg: 2.5, apg: 7.8, ts: 51.5, usg: 28.6, per: 16.1, position: 'Base', ...overrides,
})

describe('player pages', () => {
  const rows = [
    row(),
    row({ season: 2026, games: 28, ppg: 9.5, per: 14.8 }),
    row({ season: 2021, games: 30, ppg: 12.1, per: 22.5 }),
    row({ season: 2017, team: 'UCAM Murcia', teamId: 'murcia', games: 30, per: 15 }),
  ]
  const page = buildPlayerPage({
    licenseId: '20211331', name: 'Facu Campazzo', fullName: 'Facundo Campazzo Avedano', profileSlug: 'facu-campazzo',
    rows, bio: { position: 'Base', heightM: 1.81 }, currentSeason: 2027,
    teamPathById: { 'real-madrid': '/equipos/perfil/real-madrid' }, hasSimilarity: true,
  })

  test('title, description and path name the player', () => {
    expect(page.path).toBe('/jugadores/perfil/facu-campazzo')
    expect(page.title).toBe('Facu Campazzo: estadísticas avanzadas en la ACB | openACB')
    expect(page.description).toContain('8.8 puntos')
    expect(page.description.length).toBeLessThanOrEqual(160)
    expect(page.fullName).toBe('Facundo Campazzo Avedano')
  })

  test('summary uses the latest season, the career span and the best PER', () => {
    expect(page.summary[0]).toContain('temporada regular 2026-27')
    expect(page.summary[0]).toContain('promedia')
    expect(page.summary.join(' ')).toContain('4 temporadas suyas en la Liga Endesa, de 2016-17 a 2026-27, con UCAM Murcia y Real Madrid')
    expect(page.summary.join(' ')).toContain('Su mejor PER es 22.5, en 2020-21')
    expect(page.table.rows.map(cells => cells[0])).toEqual(['2026-27', '2025-26', '2020-21', '2016-17'])
  })

  test('links use the app paths, with the id in tool segments', () => {
    expect(page.links.map(link => link.href)).toEqual([
      '/tiro/cartas/jugador/facu-campazzo--20211331?temporada=2027',
      '/jugadores/similitud/facu-campazzo--20211331?temporada=2027',
      '/equipos/perfil/real-madrid',
    ])
  })

  test('a retired player is described in the past and without shot links', () => {
    const old = buildPlayerPage({
      licenseId: '1', name: 'Edwin Jackson', profileSlug: 'edwin-jackson', currentSeason: 2027,
      rows: [row({ licenseId: 1, season: 2018, team: 'Movistar Estudiantes', teamId: 'estudiantes' })],
    })
    expect(old.summary[0]).toContain('promedió')
    expect(old.links).toEqual([])
    expect(old.card.meta).toBe('Movistar Estudiantes · Base · 2017-18 · 6 partidos')
  })
})

describe('team pages', () => {
  const team = overrides => ({ team: 'Real Madrid', teamId: 'real-madrid', season: 2027, wins: 6, losses: 0, ortg: 130.5, drtg: 105.6, netRtg: 24.9, pace: 83.5, efg: 0.5966, threeRate: 0.4943, ...overrides })
  const seasonRows = [team(), team({ teamId: 'barcelona', team: 'Barça', ortg: 120, drtg: 100, pace: 85 }), team({ teamId: 'unicaja', team: 'Unicaja', ortg: 110, drtg: 112, pace: 70 })]
  const page = buildTeamPage({ teamId: 'real-madrid', rows: [team(), team({ season: 2026, wins: 30, losses: 4 })], seasonRows, currentSeason: 2027 })

  test('ranks ratings within the season and converts fractions to percentages', () => {
    expect(page.summary[1]).toContain('130.5 puntos por 100 posesiones (1.º de 3)')
    expect(page.summary[1]).toContain('recibe 105.6 (2.º)')
    expect(page.summary[2]).toContain('59.7% de tiro efectivo')
    expect(page.card.stats.find(stat => stat.label === 'eFG%').value).toBe('59.7')
    expect(page.description.length).toBeLessThanOrEqual(160)
  })
})

describe('lineup and shot pages', () => {
  test('On/Off ranks players with enough minutes by net difference', () => {
    const players = [
      { id: 1, name: 'A', onMin: 100, netDiff: 5, onNetRtg: 10, offNetRtg: 5 },
      { id: 2, name: 'B', onMin: 90, netDiff: 12, onNetRtg: 15, offNetRtg: 3 },
      { id: 3, name: 'C', onMin: 5, netDiff: 40, onNetRtg: 50, offNetRtg: 10 },
    ]
    const page = buildLineupPage({ teamId: 'real-madrid', team: 'Real Madrid', season: 2027, players, lineups: [], nameById: { 2: { name: 'Bee', path: '/jugadores/perfil/bee' } } })
    expect(page.roster.rows.map(entry => entry.name)).toEqual(['Bee', 'A'])
    expect(page.roster.rows[0].path).toBe('/jugadores/perfil/bee')
    expect(page.summary[0]).toContain('Con Bee en pista')
    expect(page.description).toMatch(/\.$/)
    expect(page.description.length).toBeLessThanOrEqual(160)
  })

  test('shot page shares add up to the team total', () => {
    const shot = (zone, made) => ({ zone, made })
    const shots = [shot('Triple Centro', true), shot('Triple Esquina Izquierda', false), shot('Zona (Restringida)', true), shot('Media Distancia Centro', false)]
    const page = buildTeamShotPage({ teamId: 'real-madrid', team: 'Real Madrid', season: 2027, shots, leagueShots: shots })
    const shares = page.table.rows.map(cells => Number(cells[1]))
    expect(shares.reduce((sum, value) => sum + value, 0)).toBeCloseTo(100, 5)
    expect(page.summary[1]).toContain('El 50.0% de sus intentos son triples')
    expect(seasonLabel(2027)).toBe('2026-27')
  })
})

test('every exported player gets a profile URL the app treats as canonical', () => {
  const read = file => JSON.parse(readFileSync(new URL(`../../public/data/${file}`, import.meta.url), 'utf8'))
  const index = read('player-index.json')
  const names = read('player-names.json')
  const slugs = buildProfileSlugs(index, names)
  const context = { players: addPlayerNames(index.map(entry => ({ ...entry })), names, slugs) }
  const problems = Object.values(slugs)
    .map(slug => `/jugadores/perfil/${slug}`)
    .filter(path => resolveCanonicalEntityLocation({ pathname: path, search: '' }, context) !== null)
  expect(problems).toEqual([])
})
