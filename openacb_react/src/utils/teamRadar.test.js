import { readFileSync } from 'node:fs'
import { expect, test } from 'vitest'
import { getTeamRadarAxes, midrankPercentile, offensiveAxes } from './teamRadar'
import { readRouteQuery, serializeRouteQuery } from '../routing/query'

test('percentiles use midpoint ties, direction and the full uncapped range', () => {
  expect(midrankPercentile(20, [10, 20, 20, 30])).toBe(50)
  expect(midrankPercentile(10, [10, 20, 30, 40])).toBe(12.5)
  expect(midrankPercentile(10, [10, 20, 30, 40], true)).toBe(87.5)
  expect(midrankPercentile(0, [1, 2, 3])).toBe(0)
  expect(midrankPercentile(4, [1, 2, 3])).toBe(100)
  expect(midrankPercentile(1, [1, 1, 1])).toBe(50)
})

test('missing values are excluded from the cohort, never converted into average scores', () => {
  expect(midrankPercentile(2, [null, 1, 2, 3, NaN, Infinity])).toBe(50)
  expect(midrankPercentile(null, [1, 2])).toBeNull()
  expect(midrankPercentile(1, [null, NaN])).toBeNull()
})

test('landing team labels and values match its linked profile and season reference', () => {
  const teams = JSON.parse(readFileSync(new URL('../../public/data/teams.json', import.meta.url)))
  const { team: preview } = JSON.parse(readFileSync(new URL('../data/home-tool-examples.json', import.meta.url)))
  const url = new URL(preview.path, 'http://openacb.local')
  const selected = teams.find(row => row.teamId === url.pathname.split('/').at(-1) && row.season === Number(url.searchParams.get('temporada')))
  expect(preview.name).toBe(selected.team)
  const reference = teams.filter(row => row.season === selected.season)
  expect(preview.axes).toEqual(getTeamRadarAxes(selected, reference))
  expect(preview.axes.map(axis => axis.label)).toEqual(offensiveAxes.map(axis => axis.label))
  expect(preview.reference.type).toBe('season')
  expect(preview.reference.observations).toBe(reference.length)
  expect(getTeamRadarAxes(selected, teams)).not.toEqual(preview.axes)
})

test('team reference defaults to season and the historical option survives public URL serialization', () => {
  expect(readRouteQuery('teamProfile', new URLSearchParams()).referencia).toBe('season')
  const search = serializeRouteQuery('teamProfile', { temporada: 2026, referencia: 'historical' })
  expect(search).toContain('referencia=historico')
  expect(readRouteQuery('teamProfile', new URLSearchParams(search)).referencia).toBe('historical')
})
