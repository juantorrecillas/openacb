import test from 'node:test'
import assert from 'node:assert/strict'
import { readFile } from 'node:fs/promises'

import {
  buildEntitySegment,
  buildGameSegment,
  buildPlayerSegment,
  parseEntitySegment,
  slugify,
} from './identifiers.js'
import {
  buildPlayerComparisonPath,
  buildPlayerProfilePath,
  buildTeamComparisonPath,
  buildTeamProfilePath,
} from './paths.js'
import {
  parseRouteQuery,
  serializeRouteQuery,
  withQuery,
} from './query.js'
import {
  createTeamIdentityIndex,
  createTeamIdentityIndexFromRows,
  getTeamName,
  resolveTeamId,
  resolveTeamIdentity,
} from './teamIdentities.js'
import {
  ROUTE_MANIFEST,
  buildRoutePath,
  buildRouteUrl,
  getActiveNavigationGroup,
  matchCanonicalRoute,
  matchLegacyRoute,
} from './manifest.js'
import {
  resolveCanonicalEntityLocation,
  resolveLegacyLocation,
  resolveSectionLocation,
} from './legacy.js'

const registry = {
  teams: [
    {
      teamId: 'baskonia',
      aliases: [
        { name: 'Kirolbet Baskonia', seasons: [2019, 2020] },
        { name: 'Baskonia', seasons: [2021, 2022, 2023, 2024, 2025, 2026] },
      ],
    },
    {
      teamId: 'real-madrid',
      aliases: [{ name: 'Real Madrid', seasons: [2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025, 2026] }],
    },
  ],
}

test('slug and numeric entity segments are deterministic', () => {
  assert.equal(slugify('Álex Abrines & Compañía'), 'alex-abrines-y-compania')
  assert.equal(buildEntitySegment('Sergio Llull', 20201774), 'sergio-llull--20201774')
  assert.deepEqual(parseEntitySegment('sergio-llull--20201774'), {
    segment: 'sergio-llull--20201774',
    slug: 'sergio-llull',
    id: '20201774',
  })
  assert.equal(buildPlayerSegment({ licenseId: 12, playerDisplay: 'Álex Pérez' }), 'alex-perez--12')
  assert.equal(buildGameSegment({ id: 104459, local: 'Unicaja', visitor: 'Surne Bilbao Basket' }), 'unicaja-surne-bilbao-basket--104459')
})

test('canonical path builders use stable ids', () => {
  assert.equal(buildTeamProfilePath('baskonia'), '/equipos/perfil/baskonia')
  assert.equal(buildTeamComparisonPath('baskonia', 'real-madrid'), '/equipos/comparar/baskonia/real-madrid')
  assert.equal(buildPlayerProfilePath({ licenseId: 8, playerDisplay: 'Mario Hezonja' }), '/jugadores/perfil/mario-hezonja')
  assert.equal(
    buildPlayerComparisonPath(
      { licenseId: 8, playerDisplay: 'Mario Hezonja' },
      { licenseId: 9, playerDisplay: 'Sergio Llull' },
    ),
    '/jugadores/comparar/mario-hezonja--8/sergio-llull--9',
  )
  assert.throws(() => buildPlayerProfilePath('Mario--123'), /Segmento de jugador/)
  assert.throws(() => buildPlayerProfilePath('foo--123/extra'), /Segmento de jugador/)
})

test('route queries use Spanish public values and omit defaults', () => {
  const search = serializeRouteQuery('teamStats', {
    temporada: 2026,
    fase: 'playoffs',
    vista: 'oppAdvanced',
    x: 'ortg',
    y: 'pace',
  })
  assert.equal(search, 'temporada=2026&fase=playoffs&vista=rival-avanzado&y=pace')
  assert.deepEqual(parseRouteQuery('teamStats', search).values, {
    temporada: 2026,
    fase: 'playoffs',
    vista: 'oppAdvanced',
    x: 'ortg',
    y: 'pace',
  })
})

const spanishQueryRoundTrips = [
  [
    'teamClutch',
    { temporada: 2026, vista: 'opponentAdvanced' },
    'temporada=2026&vista=rival-avanzado',
  ],
  [
    'teamQuarters',
    { temporada: 2026, equipo: 'baskonia', vista: 'allowed' },
    'temporada=2026&equipo=baskonia&vista=recibido',
  ],
  [
    'playerProfile',
    {
      temporada: 2026,
      equipo: 'real-madrid',
      fase: 'playoffs',
      radar: 'position',
      percentiles: 'position',
      tiro: 'rival',
    },
    'temporada=2026&equipo=real-madrid&fase=playoffs&radar=posicion&percentiles=posicion&tiro=rival',
  ],
  [
    'lineupRankings',
    {
      temporada: 2026,
      equipo: 'baskonia',
      categoria: 'lineups',
      metrica: 'offense',
      extremo: 'bottom',
    },
    'temporada=2026&equipo=baskonia&categoria=quintetos&metrica=eficiencia&extremo=peores',
  ],
  [
    'shotCharts',
    {
      temporada: 2026,
      resultado: 'missed',
      vista: 'heatmap',
      mapa: 'density',
      zonas: 'frequency',
      equipo: 'real-madrid',
    },
    'temporada=2026&resultado=fallados&vista=mapa-calor&mapa=densidad&zonas=frecuencia&equipo=real-madrid',
  ],
  [
    'zoneLeaders',
    { temporada: 2026, metrica: 'fgPct', equipo: 'baskonia', 'min-intentos': 25 },
    'temporada=2026&metrica=porcentaje&equipo=baskonia&min-intentos=25',
  ],
]

test('Spanish public query values round-trip to internal state', () => {
  spanishQueryRoundTrips.forEach(([routeId, values, expectedSearch]) => {
    const search = serializeRouteQuery(routeId, values)
    assert.equal(search, expectedSearch, routeId)
    const parsed = parseRouteQuery(routeId, search)
    Object.entries(values).forEach(([key, value]) => {
      assert.deepEqual(parsed.values[key], value, `${routeId}.${key}`)
    })
    assert.equal(parsed.invalidKeys.length, 0, routeId)
    assert.equal(parsed.unknownKeys.length, 0, routeId)
    assert.equal(parsed.needsCanonicalization, false, routeId)
  })
})

test('lineup ids are unique and numerically sorted', () => {
  const search = serializeRouteQuery('lineupAnalysis', {
    temporada: 2026,
    con: ['20', '3', '20', '11'],
    sin: ['9'],
  })
  assert.equal(search, 'temporada=2026&con=3,11,20&sin=9')
})

test('invalid and unknown query values normalize to defaults', () => {
  const result = parseRouteQuery('zoneLeaders', '?temporada=no&min-intentos=2&foo=bar')
  assert.deepEqual(result.invalidKeys, ['temporada', 'min-intentos'])
  assert.deepEqual(result.unknownKeys, ['foo'])
  assert.equal(result.values.metrica, 'points')
  assert.equal(result.values['min-intentos'], 15)
  assert.equal(result.canonicalSearch, '')
})

test('all seasons is accepted only by tools that support it', () => {
  assert.equal(parseRouteQuery('teamStats', '?temporada=todas').values.temporada, 'all')
  assert.equal(parseRouteQuery('fourFactors', '?temporada=all').values.temporada, 'all')
  assert.equal(parseRouteQuery('playerStats', '?temporada=todas').values.temporada, 'all')

  const games = parseRouteQuery('games', '?temporada=todas&ronda=regular%3A1')
  assert.deepEqual(games.invalidKeys, ['temporada'])
  assert.equal(games.values.temporada, undefined)
  assert.equal(games.values.ronda, 'regular:1')
  assert.throws(() => serializeRouteQuery('games', { temporada: 'all' }), /temporada/)
})

test('shot chart selector mode uses a stable public query value', () => {
  const search = serializeRouteQuery('shotCharts', { temporada: 2026, tipo: 'player' })
  assert.equal(search, 'temporada=2026&tipo=jugador')
  assert.equal(parseRouteQuery('shotCharts', search).values.tipo, 'player')
})

test('invalid query shapes are rejected and canonicalized deterministically', () => {
  const result = parseRouteQuery(
    'playerStats',
    '?temporada=1999&fase=final&fase=regular&vista=desconocida&equipo=Real%20Madrid&posicion=pivot-base&minimos=quizas&foo=bar',
  )
  assert.deepEqual(result.invalidKeys, ['temporada', 'fase', 'vista', 'equipo', 'posicion', 'minimos'])
  assert.deepEqual(result.duplicateKeys, ['fase'])
  assert.deepEqual(result.unknownKeys, ['foo'])
  assert.equal(result.values.fase, 'regular')
  assert.equal(result.values.vista, 'basic')
  assert.equal(result.values.referencia, 'league')
  assert.equal(result.values.minimos, true)
  assert.equal(result.canonicalSearch, '')
  assert.equal(result.needsCanonicalization, true)

  const lineup = parseRouteQuery('lineupAnalysis', '?temporada=2026&con=12,nope&sin=8,9')
  assert.deepEqual(lineup.invalidKeys, ['con', 'sin'])
  assert.deepEqual(lineup.values, { temporada: 2026 })
  assert.equal(lineup.canonicalSearch, 'temporada=2026')

  assert.throws(
    () => serializeRouteQuery('playerProfile', { temporada: 2026, equipo: 'Real Madrid' }),
    /equipo/,
  )
  assert.throws(
    () => serializeRouteQuery('lineupAnalysis', { temporada: 2026, con: ['12', 'nope'] }),
    /con/,
  )
})

test('invalid and stale entity segments return explicit resolution results', () => {
  assert.equal(parseEntitySegment('20201774'), null)
  assert.equal(parseEntitySegment('sergio-llull-20201774'), null)
  assert.equal(parseEntitySegment('sergio-llull--0'), null)
  assert.throws(() => buildTeamProfilePath('Real Madrid'), /teamId/)
  assert.throws(() => buildTeamComparisonPath('baskonia', ''), /dos teamId/)
  assert.equal(matchCanonicalRoute('/ruta-que-no-existe'), null)
  assert.equal(resolveCanonicalEntityLocation('/jugadores/comparar', { players: [{}] }), null)

  const players = [
    {
      licenseId: 8,
      season: 2026,
      team: 'Real Madrid',
      teamId: 'real-madrid',
      playerDisplay: 'Mario Hezonja',
    },
  ]
  const cleanPlayer = resolveCanonicalEntityLocation(
    '/jugadores/perfil/mario-hezonja?temporada=2026',
    { players },
  )
  assert.equal(cleanPlayer, null)

  const missingSlug = resolveCanonicalEntityLocation(
    '/jugadores/perfil/jugador-fantasma?temporada=2026',
    { players },
  )
  assert.equal(missingSlug.status, 'not-found')

  const missingPlayer = resolveCanonicalEntityLocation(
    '/jugadores/perfil/jugador-fantasma--999?temporada=2026',
    { players },
  )
  assert.equal(missingPlayer.status, 'not-found')
  assert.equal(missingPlayer.value, '999')

  const staleSlug = resolveCanonicalEntityLocation(
    '/jugadores/perfil/slug-antiguo--8?temporada=2026&equipo=real-madrid',
    { players },
  )
  assert.equal(staleSlug.status, 'redirect')
  assert.equal(staleSlug.reason, 'profile-slug')
  assert.equal(staleSlug.to, '/jugadores/perfil/mario-hezonja?temporada=2026&equipo=real-madrid')

  const ambiguousSlug = resolveCanonicalEntityLocation('/jugadores/perfil/alex-smith', {
    players: [
      { licenseId: 21, season: 2026, teamId: 'baskonia', playerDisplay: 'Alex Smith' },
      { licenseId: 22, season: 2026, teamId: 'real-madrid', playerDisplay: 'Alex Smith' },
    ],
  })
  assert.equal(ambiguousSlug.status, 'ambiguous')

  const unavailableContext = resolveCanonicalEntityLocation(
    '/jugadores/similitud/mario-hezonja--8?temporada=2026&equipo=baskonia',
    { players },
  )
  assert.equal(unavailableContext, null)

  const missingGame = resolveCanonicalEntityLocation(
    '/partidos/partido-fantasma--999?temporada=2026',
    { games: [] },
  )
  assert.equal(missingGame.status, 'not-found')
  assert.equal(missingGame.entity, 'game')

  const missingLegacyTeam = resolveLegacyLocation('/perfil-equipo/2026/equipo-fantasma', {
    teamIdentities: registry,
  })
  assert.equal(missingLegacyTeam.status, 'not-found')
  assert.equal(missingLegacyTeam.entity, 'team')
})

test('team identities resolve season-aware aliases', () => {
  const index = createTeamIdentityIndex(registry)
  assert.equal(resolveTeamId(index, 'Kirolbet Baskonia', 2020), 'baskonia')
  assert.equal(resolveTeamId(index, 'Baskonia', 2026), 'baskonia')
  assert.equal(getTeamName(index, 'baskonia', 2020), 'Kirolbet Baskonia')
  assert.equal(resolveTeamIdentity(index, 'Kirolbet Baskonia', 2026).status, 'out-of-season')
})

test('compact team rows can provide the lookup without fetching the registry', () => {
  const index = createTeamIdentityIndexFromRows([
    { teamId: 'baskonia', team: 'Kirolbet Baskonia', season: 2020 },
    { teamId: 'baskonia', team: 'Baskonia', season: 2026 },
  ])
  assert.equal(resolveTeamId(index, 'Kirolbet Baskonia', 2020), 'baskonia')
  assert.equal(getTeamName(index, 'baskonia', 2026), 'Baskonia')
})

test('manifest matches canonical routes and builds complete urls', () => {
  const match = matchCanonicalRoute('/jugadores/perfil/sergio-llull--20201774')
  assert.equal(match.routeId, 'playerProfile')
  assert.equal(match.params.player, 'sergio-llull--20201774')
  assert.equal(getActiveNavigationGroup('/jugadores/similitud/sergio-llull--20201774'), 'herramientas')
  assert.equal(
    buildRouteUrl('teamProfile', { teamId: 'baskonia' }, { temporada: 2026 }),
    '/equipos/perfil/baskonia?temporada=2026',
  )
  assert.equal(withQuery('/partidos', ''), '/partidos')
  assert.equal(matchCanonicalRoute('/EQUIPOS//ESTADISTICAS/')?.routeId, 'teamStats')
})

const canonicalBuildCases = [
  ['home', {}, '/'],
  ['teamStats', {}, '/equipos/estadisticas'],
  ['teamProfile', { teamId: 'baskonia' }, '/equipos/perfil/baskonia'],
  ['teamComparison', { teamAId: 'baskonia', teamBId: 'real-madrid' }, '/equipos/comparar/baskonia/real-madrid'],
  ['fourFactors', {}, '/equipos/cuatro-factores'],
  ['teamQuarters', {}, '/equipos/cuartos'],
  ['teamClutch', {}, '/equipos/clutch'],
  ['games', { gameId: 104459, name: 'Unicaja Surne Bilbao Basket' }, '/partidos/unicaja-surne-bilbao-basket--104459'],
  ['playerStats', {}, '/jugadores/estadisticas'],
  ['playerProfile', { licenseId: 8, name: 'Mario Hezonja' }, '/jugadores/perfil/mario-hezonja'],
  ['playerSimilarity', { licenseId: 8, name: 'Mario Hezonja' }, '/jugadores/similitud/mario-hezonja--8'],
  [
    'playerComparison',
    {
      playerA: { licenseId: 8, playerDisplay: 'Mario Hezonja' },
      playerB: { licenseId: 9, playerDisplay: 'Sergio Llull' },
    },
    '/jugadores/comparar/mario-hezonja--8/sergio-llull--9',
  ],
  ['playerClutch', {}, '/jugadores/clutch'],
  ['lineupAnalysis', { teamId: 'baskonia' }, '/alineaciones/analisis/baskonia'],
  ['lineupRankings', {}, '/alineaciones/rankings'],
  ['shotCharts', {}, '/tiro/cartas'],
  ['shotCharts', { teamId: 'baskonia' }, '/tiro/cartas/equipo/baskonia'],
  ['shotCharts', { player: { licenseId: 8, playerDisplay: 'Mario Hezonja' } }, '/tiro/cartas/jugador/mario-hezonja--8'],
  ['zoneLeaders', {}, '/tiro/lideres'],
  ['about', {}, '/info'],
]

test('every canonical manifest route has a matching public builder', () => {
  const testedRouteIds = new Set()
  canonicalBuildCases.forEach(([routeId, params, expectedPath]) => {
    const pathname = buildRoutePath(routeId, params)
    assert.equal(pathname, expectedPath, routeId)
    assert.equal(matchCanonicalRoute(pathname)?.routeId, routeId, pathname)
    testedRouteIds.add(routeId)
  })
  assert.deepEqual(testedRouteIds, new Set(ROUTE_MANIFEST.map(route => route.id)))
})

test('every declared canonical pattern is structurally reachable', () => {
  const values = {
    teamId: 'baskonia',
    teamAId: 'baskonia',
    teamBId: 'real-madrid',
    game: 'unicaja-real-madrid--104459',
    player: 'mario-hezonja--8',
    playerA: 'mario-hezonja--8',
    playerB: 'sergio-llull--9',
  }

  ROUTE_MANIFEST.forEach(route => {
    route.patterns.forEach(pattern => {
      const pathname = pattern.replace(/:([a-zA-Z]+)/g, (_match, key) => values[key])
      const match = matchCanonicalRoute(pathname)
      assert.equal(match?.routeId, route.id, pathname)
      assert.equal(match?.pattern, pattern, pathname)
    })
  })
})

const bareLegacyCases = [
  ['/equipos', '/equipos/estadisticas', 'teamStats'],
  ['/perfil-equipo', '/equipos/perfil', 'teamProfile'],
  ['/matchup-equipos', '/equipos/comparar', 'teamComparison'],
  ['/flujo-partido', '/partidos', 'games'],
  ['/cuatro-factores', '/equipos/cuatro-factores', 'fourFactors'],
  ['/jugadores', '/jugadores/estadisticas', 'playerStats'],
  ['/jugador', '/jugadores/perfil', 'playerProfile'],
  ['/similitud', '/jugadores/similitud', 'playerSimilarity'],
  ['/comparar', '/jugadores/comparar', 'playerComparison'],
  ['/estadisticas-clutch', '/jugadores/clutch', 'playerClutch'],
  ['/alineaciones', '/alineaciones/analisis', 'lineupAnalysis'],
  ['/mejores-alineaciones', '/alineaciones/rankings', 'lineupRankings'],
  ['/cartas-tiro', '/tiro/cartas', 'shotCharts'],
  ['/lideres-zona', '/tiro/lideres', 'zoneLeaders'],
]

test('every bare legacy tool route redirects to its canonical selector', () => {
  const declaredBarePaths = new Set(
    ROUTE_MANIFEST.flatMap(route => route.legacyPatterns.filter(pattern => !pattern.includes(':'))),
  )
  assert.deepEqual(new Set(bareLegacyCases.map(([legacyPath]) => legacyPath)), declaredBarePaths)

  bareLegacyCases.forEach(([legacyPath, canonicalPath, routeId]) => {
    assert.equal(matchLegacyRoute(legacyPath)?.routeId, routeId, legacyPath)
    const result = resolveLegacyLocation(legacyPath)
    assert.equal(result?.status, 'redirect', legacyPath)
    assert.equal(result?.to, canonicalPath, legacyPath)
    assert.equal(result?.replace, true, legacyPath)
  })
})

test('bare legacy selectors preserve valid query state', () => {
  const cases = [
    ['/perfil-equipo?temporada=2024&lado=defensa', '/equipos/perfil?temporada=2024&lado=defensa'],
    ['/matchup-equipos?temporada=2024&metrica=frecuencia', '/equipos/comparar?temporada=2024&metrica=frecuencia'],
    ['/jugador?temporada=2024&fase=playoffs', '/jugadores/perfil?temporada=2024&fase=playoffs'],
    ['/similitud?temporada=2024&equipo=baskonia', '/jugadores/similitud?temporada=2024&equipo=baskonia'],
    ['/comparar?temporada-a=2024&equipo-a=baskonia', '/jugadores/comparar?temporada-a=2024&equipo-a=baskonia'],
    ['/alineaciones?temporada=2024&con=3,7', '/alineaciones/analisis?temporada=2024&con=3,7'],
  ]

  cases.forEach(([source, target]) => {
    assert.equal(resolveLegacyLocation(source)?.to, target, source)
  })
})

test('unknown legacy zone metrics return an explicit not-found result', () => {
  const result = resolveLegacyLocation('/lideres-zona/2026/desconocida')
  assert.equal(result?.status, 'not-found')
  assert.equal(result?.entity, 'metric')
})

test('invalid seasons in legacy path segments are not silently replaced', () => {
  const malformed = resolveLegacyLocation('/perfil-equipo/nope/real-madrid', {
    teamIdentities: registry,
  })
  const outOfRange = resolveLegacyLocation('/alineaciones/9999/real-madrid', {
    teamIdentities: registry,
  })
  assert.equal(malformed?.status, 'not-found')
  assert.equal(malformed?.entity, 'season')
  assert.equal(outOfRange?.status, 'not-found')
  assert.equal(outOfRange?.entity, 'season')
})

test('bare section paths resolve to deterministic default tools', () => {
  const sections = {
    '/equipos': '/equipos/estadisticas',
    '/jugadores': '/jugadores/estadisticas',
    '/alineaciones': '/alineaciones/analisis',
    '/tiro': '/tiro/cartas',
  }
  Object.entries(sections).forEach(([source, target]) => {
    const result = resolveSectionLocation(`${source}?temporada=2026`)
    assert.equal(result?.to, `${target}?temporada=2026`)
    assert.equal(result?.reason, 'section-default')
  })
})

test('legacy team links resolve through the identity registry', () => {
  const result = resolveLegacyLocation('/perfil-equipo/2020/kirolbet-baskonia', {
    teamIdentities: registry,
  })
  assert.equal(result.status, 'redirect')
  assert.equal(result.to, '/equipos/perfil/baskonia?temporada=2020')
})

test('legacy transferred-player links report ambiguity instead of guessing', () => {
  const result = resolveLegacyLocation('/similitud/123/2026', {
    players: [
      { licenseId: 123, season: 2026, team: 'Baskonia', teamId: 'baskonia', playerDisplay: 'Jugador Uno' },
      { licenseId: 123, season: 2026, team: 'Real Madrid', teamId: 'real-madrid', playerDisplay: 'Jugador Uno' },
    ],
  })
  assert.equal(result.status, 'ambiguous')
  assert.equal(result.candidates.length, 2)
})

test('exported team registry covers every compact team row season by season', async () => {
  const identitiesUrl = new URL('../../public/data/team-identities.json', import.meta.url)
  const teamsUrl = new URL('../../public/data/teams.json', import.meta.url)
  const actualRegistry = JSON.parse(await readFile(identitiesUrl, 'utf8'))
  const teamRows = JSON.parse(await readFile(teamsUrl, 'utf8'))
  const aliases = actualRegistry.teams.flatMap(team => team.aliases)

  assert.equal(actualRegistry.teams.length, 25)
  assert.equal(aliases.length, 51)
  assert.equal(new Set(actualRegistry.teams.map(team => team.teamId)).size, actualRegistry.teams.length)

  const index = createTeamIdentityIndex(actualRegistry)
  actualRegistry.teams.forEach(team => {
    assert.equal(index.byId.has(team.teamId), true, team.teamId)
    team.aliases.forEach(alias => {
      alias.seasons.forEach(season => {
        assert.equal(resolveTeamId(index, alias.name, season), team.teamId, `${alias.name} ${season}`)
      })
    })
  })

  teamRows.forEach(row => {
    assert.equal(index.byId.has(row.teamId), true, `${row.team} ${row.season}`)
    assert.equal(resolveTeamId(index, row.team, row.season), row.teamId, `${row.team} ${row.season}`)
  })
})
