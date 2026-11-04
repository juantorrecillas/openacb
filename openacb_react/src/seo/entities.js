// Content for the prerendered player and team pages: title, description,
// the visible summary and the share-card fields. Pure functions over the
// exported JSON rows, so the build script and the tests use the same code.
import {
  buildLineupAnalysisPath,
  buildPlayerProfilePath,
  buildPlayerShotChartPath,
  buildPlayerSimilarityPath,
  buildPlayerStatsPath,
  buildTeamProfilePath,
  buildTeamShotChartPath,
  buildTeamStatsPath,
} from '../routing/paths.js'

// Shot locations exist from 2020-21 (season 2021) onwards.
export const SHOT_DATA_FIRST_SEASON = 2021
const DESCRIPTION_MAX = 160

export function seasonLabel(season) {
  return `${season - 1}-${String(season).slice(-2)}`
}

const isNumber = value => typeof value === 'number' && Number.isFinite(value)
const fixed = (value, digits = 1) => (isNumber(value) ? value.toFixed(digits) : null)
// teams.json stores rates (efg, ts, threeRate...) as fractions.
const percentOfFraction = value => (isNumber(value) ? (value * 100).toFixed(1) : null)
const signed = (value, digits = 1) => (isNumber(value) ? `${value > 0 ? '+' : ''}${value.toFixed(digits)}` : null)
const plural = (count, one, many) => `${count} ${count === 1 ? one : many}`

function listJoin(items) {
  if (items.length <= 1) return items.join('')
  return `${items.slice(0, -1).join(', ')} y ${items.at(-1)}`
}

function withSeason(path, season) {
  return `${path}?temporada=${season}`
}

// Joins description parts, dropping trailing parts that would push it past the
// limit, and always ends the sentence.
function fitDescription(parts) {
  let text = parts[0]
  for (const part of parts.slice(1)) {
    if (`${text}${part}`.length > DESCRIPTION_MAX - 1) break
    text += part
  }
  return /[.!?]$/.test(text) ? text : `${text}.`
}

// Rank of a value among the season's teams (1 = best).
function rankOf(rows, key, value, lowerIsBetter = false) {
  if (!isNumber(value)) return null
  const better = rows.filter(row => isNumber(row[key]) && (lowerIsBetter ? row[key] < value : row[key] > value))
  return better.length + 1
}

function mainRow(rows) {
  return [...rows].sort((a, b) => (b.games ?? 0) - (a.games ?? 0) || (b.totalMinutes ?? 0) - (a.totalMinutes ?? 0))[0]
}

/**
 * Player profile page.
 * profileSlug: the app's slug for the player (see utils/profileSlugs.js).
 * rows: the player's regular-season rows (one per season and team).
 * similar: up to three { name, season, team, path } from the similarity export.
 */
export function buildPlayerPage({ licenseId, name, fullName, profileSlug, rows, bio = {}, currentSeason, teamPathById = {}, similar = [], hasSimilarity = false }) {
  const records = [...rows].sort((a, b) => b.season - a.season || (b.games ?? 0) - (a.games ?? 0))
  const latestSeason = records[0].season
  const latest = mainRow(records.filter(row => row.season === latestSeason))
  const position = bio.position || latest.position || null
  const heightM = isNumber(bio.heightM) ? bio.heightM : (isNumber(latest.heightM) ? latest.heightM : null)
  const isCurrent = latestSeason === currentSeason
  const path = buildPlayerProfilePath({ profileSlug })
  const latestTeams = [...new Set(records.filter(row => row.season === latestSeason).map(row => row.team))]
  const season = seasonLabel(latestSeason)

  const facts = [position, latestTeams.join(' / '), heightM ? `${heightM.toFixed(2)} m` : null].filter(Boolean)

  const verb = isCurrent ? 'promedia' : 'promedió'
  const counting = [
    fixed(latest.ppg) && `${fixed(latest.ppg)} puntos`,
    fixed(latest.rpg) && `${fixed(latest.rpg)} rebotes`,
    fixed(latest.apg) && `${fixed(latest.apg)} asistencias`,
  ].filter(Boolean)
  const summary = []
  if (counting.length) {
    const minutes = fixed(latest.mpg) ? ` en ${fixed(latest.mpg)} minutos por partido` : ''
    const games = isNumber(latest.games) ? ` (${plural(latest.games, 'partido', 'partidos')})` : ''
    summary.push(`En la temporada regular ${season} de la Liga Endesa (${latest.team}) ${verb} ${listJoin(counting)}${minutes}${games}.`)
  }
  const efficiency = [
    fixed(latest.ts) && `un tiro verdadero (TS%) del ${fixed(latest.ts)}%`,
    fixed(latest.usg) && `un uso del ${fixed(latest.usg)}%`,
    fixed(latest.per) && `un PER de ${fixed(latest.per)}`,
  ].filter(Boolean)
  if (efficiency.length) summary.push(`${isCurrent ? 'Registra' : 'Registró'} ${listJoin(efficiency)}.`)

  const seasons = [...new Set(records.map(row => row.season))].sort((a, b) => a - b)
  if (seasons.length > 1) {
    const teamsOldestFirst = [...new Set([...records].reverse().map(row => row.team))]
    summary.push(`openACB recoge ${seasons.length} temporadas suyas en la Liga Endesa, de ${seasonLabel(seasons[0])} a ${seasonLabel(seasons.at(-1))}, con ${listJoin(teamsOldestFirst)}.`)
    const qualified = records.filter(row => (row.games ?? 0) >= 10 && isNumber(row.per))
    if (qualified.length > 1) {
      const best = [...qualified].sort((a, b) => b.per - a.per)[0]
      summary.push(`Su mejor PER es ${fixed(best.per)}, en ${seasonLabel(best.season)} (${best.team}).`)
    }
  }

  const descriptionStats = [
    fixed(latest.ppg) && `${fixed(latest.ppg)} puntos`,
    fixed(latest.rpg) && `${fixed(latest.rpg)} rebotes`,
    fixed(latest.apg) && `${fixed(latest.apg)} asistencias`,
  ].filter(Boolean)
  const description = descriptionStats.length
    ? fitDescription([
      `${name} (${latest.team}) en la Liga Endesa ${season}: ${listJoin(descriptionStats)} por partido`,
      fixed(latest.ts) ? `, ${fixed(latest.ts)}% TS` : '',
      fixed(latest.per) ? ` y PER ${fixed(latest.per)}` : '',
      '. Trayectoria, tiro y On/Off.',
    ].filter(Boolean))
    : `Estadísticas avanzadas de ${name} en la Liga Endesa: trayectoria, tiro y On/Off.`

  const links = []
  if (latestSeason >= SHOT_DATA_FIRST_SEASON) {
    links.push({ href: withSeason(buildPlayerShotChartPath({ licenseId }, name), latestSeason), label: `Carta de tiro ${season}` })
  }
  if (hasSimilarity) {
    links.push({ href: withSeason(buildPlayerSimilarityPath({ licenseId }, name), latestSeason), label: 'Jugadores con un perfil parecido' })
  }
  const teamPath = teamPathById[latest.teamId]
  if (teamPath) links.push({ href: teamPath, label: `Perfil de equipo: ${latest.team}` })

  return {
    path,
    title: `${name}: estadísticas avanzadas en la ACB | openACB`,
    description,
    heading: name,
    fullName: fullName && fullName !== name ? fullName : null,
    facts,
    summary,
    table: {
      caption: 'Trayectoria en temporada regular',
      columns: ['Temporada', 'Equipo', 'PJ', 'MIN', 'PTS', 'REB', 'AST', 'TS%', 'PER'],
      numeric: [false, false, true, true, true, true, true, true, true],
      rows: records.map(row => [
        seasonLabel(row.season), row.team, row.games ?? '—', fixed(row.mpg) ?? '—', fixed(row.ppg) ?? '—',
        fixed(row.rpg) ?? '—', fixed(row.apg) ?? '—', fixed(row.ts) ?? '—', fixed(row.per) ?? '—',
      ]),
    },
    similar,
    links,
    breadcrumbs: [
      { name: 'Inicio', path: '/' },
      { name: 'Jugadores', path: buildPlayerStatsPath() },
      { name, path },
    ],
    card: {
      kicker: 'Ficha de jugador · Liga Endesa',
      title: name,
      meta: [latest.team, position, season, isNumber(latest.games) ? plural(latest.games, 'partido', 'partidos') : null].filter(Boolean).join(' · '),
      stats: [
        ['PTS', fixed(latest.ppg)], ['REB', fixed(latest.rpg)], ['AST', fixed(latest.apg)],
        ['TS%', fixed(latest.ts)], ['PER', fixed(latest.per)],
      ].filter(([, value]) => value != null).map(([label, value]) => ({ label, value })),
    },
  }
}

/**
 * Team profile page for a team in the current season.
 * rows: the team's rows in teams.json; seasonRows: every team in the latest season.
 * roster: [{ name, path, position, games, ppg, rpg, apg }] for the latest season.
 */
export function buildTeamPage({ teamId, rows, seasonRows, roster = [], currentSeason }) {
  const records = [...rows].sort((a, b) => b.season - a.season)
  const latest = records[0]
  const name = latest.team
  const season = seasonLabel(latest.season)
  const path = buildTeamProfilePath(teamId)
  const teamCount = seasonRows.length
  const ortgRank = rankOf(seasonRows, 'ortg', latest.ortg)
  const drtgRank = rankOf(seasonRows, 'drtg', latest.drtg, true)
  const paceRank = rankOf(seasonRows, 'pace', latest.pace)
  const isCurrent = latest.season === currentSeason
  const record = isNumber(latest.wins) && isNumber(latest.losses) ? `${latest.wins}-${latest.losses}` : null

  const summary = []
  if (record) {
    summary.push(`En la temporada ${season} de la Liga Endesa, ${name} ${isCurrent ? 'suma' : 'sumó'} ${plural(latest.wins, 'victoria', 'victorias')} y ${plural(latest.losses, 'derrota', 'derrotas')}.`)
  }
  if (isNumber(latest.ortg) && isNumber(latest.drtg)) {
    summary.push(`${isCurrent ? 'Anota' : 'Anotó'} ${fixed(latest.ortg)} puntos por 100 posesiones (${ortgRank}.º de ${teamCount}) y ${isCurrent ? 'recibe' : 'recibió'} ${fixed(latest.drtg)} (${drtgRank}.º), para un rating neto de ${signed(latest.netRtg)}.`)
  }
  if (isNumber(latest.pace)) {
    const shooting = [
      percentOfFraction(latest.efg) && `un ${percentOfFraction(latest.efg)}% de tiro efectivo (eFG%)`,
      percentOfFraction(latest.threeRate) && `el ${percentOfFraction(latest.threeRate)}% de sus tiros desde el triple`,
    ].filter(Boolean)
    summary.push(`Su ritmo es de ${fixed(latest.pace)} posesiones por partido (${paceRank}.º)${shooting.length ? `, con ${listJoin(shooting)}` : ''}.`)
  }

  const description = fitDescription([
    `${name} en la Liga Endesa ${season}`,
    record ? `: ${record}` : '',
    isNumber(latest.ortg) ? `, rating ofensivo ${fixed(latest.ortg)} (${ortgRank}.º)` : '',
    isNumber(latest.drtg) ? `, defensivo ${fixed(latest.drtg)} (${drtgRank}.º)` : '',
    isNumber(latest.netRtg) ? ` y neto ${signed(latest.netRtg)}` : '',
    '. Plantilla, tiro y quintetos.',
  ])

  const links = [
    { href: withSeason(buildLineupAnalysisPath(teamId), latest.season), label: `Análisis On/Off y quintetos ${season}` },
  ]
  if (latest.season >= SHOT_DATA_FIRST_SEASON) {
    links.push({ href: withSeason(buildTeamShotChartPath(teamId), latest.season), label: `Carta de tiro ${season}` })
  }

  return {
    path,
    title: `${name}: estadísticas avanzadas y perfil de juego | openACB`,
    description,
    heading: name,
    facts: [season, record ? `${latest.wins} V · ${latest.losses} D` : null, isNumber(latest.netRtg) ? `Neto ${signed(latest.netRtg)}` : null].filter(Boolean),
    summary,
    roster: roster.length ? {
      caption: `Plantilla ${season} (temporada regular)`,
      columns: ['Jugador', 'Posición', 'PJ', 'PTS', 'REB', 'AST'],
      rows: roster.map(player => ({
        name: player.name,
        path: player.path,
        cells: [player.position || '—', player.games ?? '—', fixed(player.ppg) ?? '—', fixed(player.rpg) ?? '—', fixed(player.apg) ?? '—'],
      })),
    } : null,
    table: {
      caption: 'Temporadas en la Liga Endesa',
      columns: ['Temporada', 'Equipo', 'V-D', 'ORtg', 'DRtg', 'Neto', 'Ritmo'],
      numeric: [false, false, true, true, true, true, true],
      rows: records.map(row => [
        seasonLabel(row.season), row.team,
        isNumber(row.wins) && isNumber(row.losses) ? `${row.wins}-${row.losses}` : '—',
        fixed(row.ortg) ?? '—', fixed(row.drtg) ?? '—', signed(row.netRtg) ?? '—', fixed(row.pace) ?? '—',
      ]),
    },
    links,
    breadcrumbs: [
      { name: 'Inicio', path: '/' },
      { name: 'Equipos', path: buildTeamStatsPath() },
      { name, path },
    ],
    card: {
      kicker: 'Perfil de equipo · Liga Endesa',
      title: name,
      meta: [season, record ? `${latest.wins} victorias, ${latest.losses} derrotas` : null].filter(Boolean).join(' · '),
      stats: [
        ['ORtg', fixed(latest.ortg)], ['DRtg', fixed(latest.drtg)], ['NETO', signed(latest.netRtg)],
        ['RITMO', fixed(latest.pace)], ['eFG%', percentOfFraction(latest.efg)],
      ].filter(([, value]) => value != null).map(([label, value]) => ({ label, value })),
    },
  }
}

/**
 * On/Off and lineups page for a team in the current season.
 * players: rows from the lineup export's players object; lineups: its five-man lineups.
 * nameById: licenseId -> { name, path } for links to player pages.
 */
export function buildLineupPage({ teamId, team, season, players, lineups, nameById = {} }) {
  const label = seasonLabel(season)
  const path = buildLineupAnalysisPath(teamId)
  const playerName = player => nameById[String(player.id)]?.name || player.name || player.nickname
  const minMinutes = Math.max(20, Math.round(Math.max(0, ...players.map(player => player.onMin ?? 0)) * 0.25))
  const impact = players
    .filter(player => (player.onMin ?? 0) >= minMinutes && isNumber(player.netDiff))
    .sort((a, b) => b.netDiff - a.netDiff)
  const topLineups = [...lineups]
    .filter(lineup => isNumber(lineup.onMin))
    .sort((a, b) => b.onMin - a.onMin)
    .slice(0, 5)

  const summary = []
  if (impact.length) {
    const best = impact[0]
    summary.push(`Con ${playerName(best)} en pista, ${team} rinde ${fixed(best.netDiff)} puntos por 100 posesiones mejor que sin él (${fixed(best.onNetRtg)} frente a ${fixed(best.offNetRtg)} de rating neto), el mayor impacto On/Off de la plantilla en ${label}.`)
  }
  if (topLineups.length) {
    const lineup = topLineups[0]
    summary.push(`Su quinteto más utilizado (${lineup.players.split(' | ').join(', ')}) acumula ${fixed(lineup.onMin)} minutos con un rating neto de ${signed(lineup.onNetRtg)}.`)
  }

  const description = fitDescription([
    `Análisis On/Off y quintetos de ${team} en la Liga Endesa ${label}`,
    impact.length ? `: impacto de cada jugador en pista y en el banquillo` : '',
    topLineups.length ? ` y rendimiento de sus quintetos más utilizados.` : '.',
  ])

  return {
    path,
    title: `${team}: análisis On/Off y quintetos ${label} | openACB`,
    description,
    heading: `${team}: On/Off y quintetos`,
    facts: [label, `${plural(players.length, 'jugador', 'jugadores')} con minutos`],
    summary,
    roster: impact.length ? {
      caption: `Impacto On/Off (mínimo ${minMinutes} minutos en pista)`,
      columns: ['Jugador', 'Min', 'Neto On', 'Neto Off', 'Impacto'],
      rows: impact.map(player => ({
        name: playerName(player),
        path: nameById[String(player.id)]?.path || null,
        cells: [fixed(player.onMin, 0), signed(player.onNetRtg), signed(player.offNetRtg), signed(player.netDiff)],
      })),
    } : null,
    table: topLineups.length ? {
      caption: 'Quintetos más utilizados',
      columns: ['Quinteto', 'Min', 'ORtg', 'DRtg', 'Neto'],
      numeric: [false, true, true, true, true],
      rows: topLineups.map(lineup => [
        lineup.players.split(' | ').join(', '), fixed(lineup.onMin), fixed(lineup.onORtg) ?? '—', fixed(lineup.onDRtg) ?? '—', signed(lineup.onNetRtg) ?? '—',
      ]),
    } : null,
    links: [{ href: buildTeamProfilePath(teamId), label: `Perfil de equipo: ${team}` }],
    breadcrumbs: [
      { name: 'Inicio', path: '/' },
      { name: 'Alineaciones', path: buildLineupAnalysisPath() },
      { name: team, path },
    ],
  }
}

const SHOT_GROUPS = [
  { label: 'Zona restringida', zones: ['Zona (Restringida)'] },
  { label: 'Resto de la pintura', zones: ['Zona no restringida'] },
  { label: 'Media distancia', match: zone => zone.startsWith('Media Distancia') },
  { label: 'Triple de esquina', match: zone => zone.startsWith('Triple Esquina') },
  { label: 'Triple frontal', match: zone => zone === 'Triple Centro' || zone.startsWith('Triple Codo') },
]

function shotGroup(zone) {
  return SHOT_GROUPS.find(group => group.zones?.includes(zone) || group.match?.(zone))?.label || null
}

function shotProfile(shots) {
  const groups = Object.fromEntries(SHOT_GROUPS.map(group => [group.label, { attempts: 0, makes: 0 }]))
  let total = 0
  shots.forEach(shot => {
    const label = shotGroup(shot.zone || '')
    if (!label) return
    total += 1
    groups[label].attempts += 1
    if (shot.made === true || shot.made === 1 || shot.made === 'true') groups[label].makes += 1
  })
  return { total, groups }
}

/**
 * Team shot chart page: zone mix and accuracy against the league.
 * shots: the team's shots in the season; leagueShots: every shot in the season.
 */
export function buildTeamShotPage({ teamId, team, season, shots, leagueShots }) {
  const label = seasonLabel(season)
  const path = buildTeamShotChartPath(teamId)
  const own = shotProfile(shots)
  const league = shotProfile(leagueShots)
  const share = (profile, key) => (profile.total ? (profile.groups[key].attempts / profile.total) * 100 : null)
  const accuracy = (profile, key) => (profile.groups[key].attempts ? (profile.groups[key].makes / profile.groups[key].attempts) * 100 : null)

  const rows = SHOT_GROUPS.map(group => [
    group.label,
    fixed(share(own, group.label)) ?? '—',
    fixed(share(league, group.label)) ?? '—',
    fixed(accuracy(own, group.label)) ?? '—',
    fixed(accuracy(league, group.label)) ?? '—',
  ])
  const threeShare = ['Triple de esquina', 'Triple frontal'].reduce((sum, key) => sum + (share(own, key) ?? 0), 0)
  const leagueThreeShare = ['Triple de esquina', 'Triple frontal'].reduce((sum, key) => sum + (share(league, key) ?? 0), 0)
  const rimShare = share(own, 'Zona restringida')
  const leagueRimShare = share(league, 'Zona restringida')

  const summary = [
    `${team} ha lanzado ${plural(own.total, 'tiro de campo', 'tiros de campo')} con localización en la temporada ${label}.`,
    `El ${fixed(threeShare)}% de sus intentos son triples (media de la liga: ${fixed(leagueThreeShare)}%) y el ${fixed(rimShare)}% llegan desde la zona restringida (liga: ${fixed(leagueRimShare)}%).`,
  ]

  return {
    path,
    title: `${team}: carta de tiro y zonas de lanzamiento ${label} | openACB`,
    description: fitDescription([
      `Carta de tiro de ${team} en la Liga Endesa ${label}: ${fixed(threeShare)}% de triples y ${fixed(rimShare)}% de tiros cerca del aro`,
      ', con porcentajes por zona frente a la media de la liga.',
    ]),
    heading: `${team}: carta de tiro`,
    facts: [label, plural(own.total, 'tiro', 'tiros')],
    summary,
    table: {
      caption: 'Distribución y acierto por zona (%)',
      columns: ['Zona', 'Frecuencia', 'Liga', 'Acierto', 'Liga'],
      numeric: [false, true, true, true, true],
      rows,
    },
    links: [{ href: buildTeamProfilePath(teamId), label: `Perfil de equipo: ${team}` }],
    breadcrumbs: [
      { name: 'Inicio', path: '/' },
      { name: 'Cartas de tiro', path: '/tiro/cartas' },
      { name: team, path },
    ],
  }
}
