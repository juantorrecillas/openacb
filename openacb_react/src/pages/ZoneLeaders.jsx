import { useMemo, useEffect } from 'react'
import { useSearchParams } from 'react-router-dom'
import Court, { COURT_LINE_COLOR, COURT_SURFACE_FILL } from '../components/Court'
import { getPlayerPhoto } from '../utils/playerPhotos'
import { getPlayerCompactName, getPlayerDisplayName } from '../utils/playerNames'
import ShotToolShell, { shotToolStyles } from '../components/ShotToolShell'
import { parseRouteQuery, serializeRouteQuery } from '../routing'

// ─── Court constants (same as ZoneHeatmap.jsx) ────────────────────────────────
const BASKET_Y = -12.425
const FREE_THROW_Y = -8.2
const THREE_PT_R = 6.75
const CORNER_3_X = 6.6
const RESTRICTED_R = 1.25
const BASELINE_Y = -14
const SIDELINE_X = 7.5
const PAINT_HALF_WIDTH = 2.4
const CORNER_BOUNDARY_Y = -11
const ANGLE_BOUNDARY = 29.7 * (Math.PI / 180)
const THREE_PT_AT_ANGLE_X = THREE_PT_R * Math.sin(ANGLE_BOUNDARY)
const THREE_PT_AT_ANGLE_Y = BASKET_Y + THREE_PT_R * Math.cos(ANGLE_BOUNDARY)

function generateArcPoints(centerX, centerY, radius, startAngle, endAngle, numPoints = 20) {
  const points = []
  for (let i = 0; i <= numPoints; i++) {
    const angle = startAngle + (endAngle - startAngle) * (i / numPoints)
    points.push({
      x: centerX + radius * Math.sin(angle),
      y: centerY + radius * Math.cos(angle),
    })
  }
  return points
}

function getZonePolygons() {
  const zones = {}

  const restrictedArc = generateArcPoints(0, BASKET_Y, RESTRICTED_R, -Math.PI / 2, Math.PI / 2, 20)
  zones['Zona (Restringida)'] = [
    { x: -RESTRICTED_R, y: BASKET_Y },
    ...restrictedArc.slice(1, -1),
    { x: RESTRICTED_R, y: BASKET_Y },
  ]

  const restrictedArcReverse = generateArcPoints(0, BASKET_Y, RESTRICTED_R, Math.PI / 2, -Math.PI / 2, 20)
  zones['Zona no restringida'] = [
    { x: -PAINT_HALF_WIDTH, y: FREE_THROW_Y },
    { x: PAINT_HALF_WIDTH, y: FREE_THROW_Y },
    { x: PAINT_HALF_WIDTH, y: BASKET_Y },
    { x: RESTRICTED_R, y: BASKET_Y },
    ...restrictedArcReverse,
    { x: -RESTRICTED_R, y: BASKET_Y },
    { x: -PAINT_HALF_WIDTH, y: BASKET_Y },
  ]

  zones['Triple Esquina Derecha'] = [
    { x: -SIDELINE_X, y: BASELINE_Y },
    { x: -CORNER_3_X, y: BASELINE_Y },
    { x: -CORNER_3_X, y: CORNER_BOUNDARY_Y },
    { x: -SIDELINE_X, y: CORNER_BOUNDARY_Y },
  ]

  zones['Triple Esquina Izquierda'] = [
    { x: CORNER_3_X, y: BASELINE_Y },
    { x: SIDELINE_X, y: BASELINE_Y },
    { x: SIDELINE_X, y: CORNER_BOUNDARY_Y },
    { x: CORNER_3_X, y: CORNER_BOUNDARY_Y },
  ]

  zones['Media Distancia Esquina Derecha'] = [
    { x: -CORNER_3_X, y: BASELINE_Y },
    { x: -PAINT_HALF_WIDTH, y: BASELINE_Y },
    { x: -PAINT_HALF_WIDTH, y: CORNER_BOUNDARY_Y },
    { x: -CORNER_3_X, y: CORNER_BOUNDARY_Y },
  ]

  zones['Media Distancia Esquina Izquierda'] = [
    { x: PAINT_HALF_WIDTH, y: BASELINE_Y },
    { x: CORNER_3_X, y: BASELINE_Y },
    { x: CORNER_3_X, y: CORNER_BOUNDARY_Y },
    { x: PAINT_HALF_WIDTH, y: CORNER_BOUNDARY_Y },
  ]

  const tripleElbowRightArc = generateArcPoints(0, BASKET_Y, THREE_PT_R, -ANGLE_BOUNDARY, -Math.asin(CORNER_3_X / THREE_PT_R), 15)
  zones['Triple Codo Derecha'] = [
    { x: -THREE_PT_AT_ANGLE_X, y: THREE_PT_AT_ANGLE_Y },
    ...tripleElbowRightArc.slice(1),
    { x: -CORNER_3_X, y: CORNER_BOUNDARY_Y },
    { x: -SIDELINE_X, y: CORNER_BOUNDARY_Y },
    { x: -SIDELINE_X, y: 0 },
    { x: -THREE_PT_AT_ANGLE_X, y: 0 },
    { x: -THREE_PT_AT_ANGLE_X, y: THREE_PT_AT_ANGLE_Y },
  ]

  const tripleElbowLeftArc = generateArcPoints(0, BASKET_Y, THREE_PT_R, Math.asin(CORNER_3_X / THREE_PT_R), ANGLE_BOUNDARY, 15)
  zones['Triple Codo Izquierda'] = [
    { x: CORNER_3_X, y: CORNER_BOUNDARY_Y },
    ...tripleElbowLeftArc,
    { x: THREE_PT_AT_ANGLE_X, y: THREE_PT_AT_ANGLE_Y },
    { x: THREE_PT_AT_ANGLE_X, y: 0 },
    { x: SIDELINE_X, y: 0 },
    { x: SIDELINE_X, y: CORNER_BOUNDARY_Y },
  ]

  const midElbowRightOuterArc = generateArcPoints(0, BASKET_Y, THREE_PT_R, -ANGLE_BOUNDARY, -Math.asin(CORNER_3_X / THREE_PT_R), 15)
  zones['Media Distancia Codo Derecha'] = [
    { x: -PAINT_HALF_WIDTH, y: CORNER_BOUNDARY_Y },
    { x: -PAINT_HALF_WIDTH, y: FREE_THROW_Y },
    { x: -THREE_PT_AT_ANGLE_X, y: THREE_PT_AT_ANGLE_Y },
    ...midElbowRightOuterArc.slice(1),
    { x: -CORNER_3_X, y: CORNER_BOUNDARY_Y },
  ]

  const midElbowLeftOuterArc = generateArcPoints(0, BASKET_Y, THREE_PT_R, Math.asin(CORNER_3_X / THREE_PT_R), ANGLE_BOUNDARY, 15)
  zones['Media Distancia Codo Izquierda'] = [
    { x: CORNER_3_X, y: CORNER_BOUNDARY_Y },
    ...midElbowLeftOuterArc,
    { x: THREE_PT_AT_ANGLE_X, y: THREE_PT_AT_ANGLE_Y },
    { x: PAINT_HALF_WIDTH, y: FREE_THROW_Y },
    { x: PAINT_HALF_WIDTH, y: CORNER_BOUNDARY_Y },
  ]

  const midCenterOuterArc = generateArcPoints(0, BASKET_Y, THREE_PT_R, -ANGLE_BOUNDARY, ANGLE_BOUNDARY, 15)
  zones['Media Distancia Centro'] = [
    { x: -PAINT_HALF_WIDTH, y: FREE_THROW_Y },
    { x: PAINT_HALF_WIDTH, y: FREE_THROW_Y },
    { x: THREE_PT_AT_ANGLE_X, y: THREE_PT_AT_ANGLE_Y },
    ...midCenterOuterArc.reverse().slice(1),
  ]

  const tripleCenterArc = generateArcPoints(0, BASKET_Y, THREE_PT_R, -ANGLE_BOUNDARY, ANGLE_BOUNDARY, 15)
  zones['Triple Centro'] = [
    { x: -THREE_PT_AT_ANGLE_X, y: THREE_PT_AT_ANGLE_Y },
    ...tripleCenterArc,
    { x: THREE_PT_AT_ANGLE_X, y: THREE_PT_AT_ANGLE_Y },
    { x: THREE_PT_AT_ANGLE_X, y: 0 },
    { x: -THREE_PT_AT_ANGLE_X, y: 0 },
  ]

  return zones
}

const CUSTOM_LABEL_POSITIONS = {
  'Zona (Restringida)': { x: 0, y: -12.4 },
  'Zona no restringida': { x: 0, y: -10.2 },
  'Triple Codo Derecha': { x: -5.8, y: -4 },
  'Triple Codo Izquierda': { x: 5.8, y: -4 },
  'Triple Centro': { x: 0, y: -3.5 },
  'Media Distancia Centro': { x: 0, y: -6.5 },
  'Media Distancia Codo Derecha': { x: -4.2, y: -8.5 },
  'Media Distancia Codo Izquierda': { x: 4.2, y: -8.5 },
  'Media Distancia Esquina Derecha': { x: -4.5, y: -12.5 },
  'Media Distancia Esquina Izquierda': { x: 4.5, y: -12.5 },
  'Triple Esquina Derecha': { x: -7.05, y: -12.5 },
  'Triple Esquina Izquierda': { x: 7.05, y: -12.5 },
}

function polygonToPath(points, scale, offsetX) {
  if (!points || points.length === 0) return ''
  const svgPoints = points.map(p => ({
    x: (p.x + offsetX) * scale,
    y: -p.y * scale,
  }))
  return svgPoints.map((p, i) => `${i === 0 ? 'M' : 'L'} ${p.x} ${p.y}`).join(' ') + ' Z'
}

function getPolygonCentroid(points) {
  if (!points || points.length === 0) return { x: 0, y: 0 }
  const sum = points.reduce((acc, p) => ({ x: acc.x + p.x, y: acc.y + p.y }), { x: 0, y: 0 })
  return { x: sum.x / points.length, y: sum.y / points.length }
}

// ─── Player name helpers ──────────────────────────────────────────────────────

const normalizeName = (value) => String(value || '')
  .normalize('NFD')
  .replace(/[\u0300-\u036f]/g, '')
  .toLocaleLowerCase('es')

// ─── Zone fill ────────────────────────────────────────────────────────────────

const ZONE_FILL = COURT_SURFACE_FILL
const ZONE_FILL_EMPTY = COURT_SURFACE_FILL
const ZONE_STROKE = COURT_LINE_COLOR

// ─── Component ────────────────────────────────────────────────────────────────

export default function ZoneLeaders({ loadShotsForSeason, shotsCache, loadingShots, teams, players, playerPhotos = {} }) {
  const [searchParams, setSearchParams] = useSearchParams()
  const search = searchParams.toString()
  const query = useMemo(() => parseRouteQuery('zoneLeaders', search), [search])
  const availableSeasons = useMemo(() => {
    const seasons = [...new Set(teams.map(t => t.season))].filter(s => s >= 2021).sort((a, b) => b - a)
    return seasons
  }, [teams])

  const requestedSeason = Number(query.values.temporada)
  const selectedSeason = availableSeasons.includes(requestedSeason)
    ? requestedSeason
    : (availableSeasons[0] || 2026)
  const requestedAttempts = Number(query.values['min-intentos'])
  const minAttempts = Number.isInteger(requestedAttempts) && requestedAttempts >= 3 && requestedAttempts <= 50
    ? requestedAttempts
    : 15
  const metric = query.values.metrica || 'points'

  useEffect(() => {
    if (selectedSeason) loadShotsForSeason(selectedSeason)
  }, [selectedSeason, loadShotsForSeason])

  const seasonShots = useMemo(() => shotsCache[selectedSeason] || [], [shotsCache, selectedSeason])
  const hasLoadedSeason = Object.hasOwn(shotsCache, selectedSeason)
  const isLoading = loadingShots[selectedSeason] || false

  const teamList = useMemo(() =>
    [...new Set(seasonShots.map(s => s.team))].sort(),
    [seasonShots]
  )

  const seasonTeamRows = useMemo(
    () => teams
      .filter(team => (
        team.season === selectedSeason
        && (!hasLoadedSeason || teamList.includes(team.team))
      ))
      .sort((a, b) => a.team.localeCompare(b.team, 'es')),
    [hasLoadedSeason, selectedSeason, teamList, teams]
  )
  const selectedTeamId = query.values.equipo || null
  const selectedTeam = seasonTeamRows.find(team => team.teamId === selectedTeamId)?.team || ''

  useEffect(() => {
    const canonical = serializeRouteQuery('zoneLeaders', {
      temporada: selectedSeason,
      metrica: metric,
      equipo: selectedTeam ? selectedTeamId : null,
      'min-intentos': minAttempts,
    }, { strict: false })
    if (canonical !== search) {
      setSearchParams(canonical, { replace: true })
    }
  }, [metric, minAttempts, search, selectedSeason, selectedTeam, selectedTeamId, setSearchParams])

  const updateState = ({
    season = selectedSeason,
    teamId = selectedTeamId,
    nextMetric = metric,
    attempts = minAttempts,
  }) => {
    const next = serializeRouteQuery('zoneLeaders', {
      temporada: season,
      metrica: nextMetric,
      equipo: teamId,
      'min-intentos': attempts,
    }, { strict: false })
    setSearchParams(next)
  }

  const filteredShots = useMemo(() => {
    if (!selectedTeam) return seasonShots
    return seasonShots.filter(s => s.team === selectedTeam)
  }, [seasonShots, selectedTeam])

  const playerRecordsById = useMemo(() => {
    const records = new Map()
    players.forEach(player => {
      if (Number(player.season) !== Number(selectedSeason)) return
      const id = String(player.licenseId)
      if (!records.has(id)) records.set(id, player)
    })
    return records
  }, [players, selectedSeason])

  const zonePolygons = useMemo(() => getZonePolygons(), [])

  // Compute zone leaders
  const zoneLeaders = useMemo(() => {
    const zonePlayerMap = {}

    filteredShots.forEach(shot => {
      const zone = shot.zoned || shot.zone
      if (!zone) return
      if (!zonePlayerMap[zone]) zonePlayerMap[zone] = {}
      const pid = String(shot.playerId)
      if (!zonePlayerMap[zone][pid]) {
        zonePlayerMap[zone][pid] = {
          playerId: pid,
          playerName: shot.player,
          teams: new Set(),
          attempts: 0,
          makes: 0,
          points: 0,
        }
      }
      zonePlayerMap[zone][pid].teams.add(shot.team)
      zonePlayerMap[zone][pid].attempts++
      if (shot.made) zonePlayerMap[zone][pid].makes++
      zonePlayerMap[zone][pid].points += shot.points || 0
    })

    const leaders = {}
    Object.entries(zonePlayerMap).forEach(([zone, playerMap]) => {
      const eligible = Object.values(playerMap).filter(p => p.attempts >= minAttempts)
      if (eligible.length === 0) {
        leaders[zone] = null
        return
      }
      eligible.forEach(p => {
        p.fgPct = p.attempts > 0 ? (p.makes / p.attempts) * 100 : 0
      })
      const byName = (a, b) => {
        const nameA = getPlayerDisplayName(playerRecordsById.get(a.playerId), a.playerName)
        const nameB = getPlayerDisplayName(playerRecordsById.get(b.playerId), b.playerName)
        return nameA.localeCompare(nameB, 'es')
      }
      if (metric === 'points') {
        eligible.sort((a, b) =>
          b.points - a.points
          || b.fgPct - a.fgPct
          || b.attempts - a.attempts
          || byName(a, b)
        )
      } else {
        eligible.sort((a, b) =>
          b.fgPct - a.fgPct
          || b.attempts - a.attempts
          || b.makes - a.makes
          || byName(a, b)
        )
      }
      const leader = eligible[0]
      const leaderTeams = [...leader.teams].sort((a, b) => a.localeCompare(b, 'es'))
      leaders[zone] = {
        ...leader,
        team: leaderTeams.length > 1 ? 'Varios equipos' : leaderTeams[0] || '-',
        eligible: eligible.length,
      }
    })

    return leaders
  }, [filteredShots, minAttempts, metric, playerRecordsById])

  const scale = 750 / 15
  const offsetX = 7.5

  const courtToSVG = (x, y) => ({
    x: (x + offsetX) * scale,
    y: -y * scale,
  })

  const visibleLeaderNames = useMemo(() => {
    const namesById = new Map()
    const idsByCompactName = new Map()

    Object.values(zoneLeaders).filter(Boolean).forEach(leader => {
      const id = String(leader.playerId)
      if (namesById.has(id)) return
      const record = playerRecordsById.get(id)
      const full = getPlayerDisplayName(record, leader.playerName || '-')
      const compact = getPlayerCompactName(record, full)
      namesById.set(id, { full, compact })

      const compactKey = normalizeName(compact)
      if (!idsByCompactName.has(compactKey)) idsByCompactName.set(compactKey, new Set())
      idsByCompactName.get(compactKey).add(id)
    })

    namesById.forEach((names, id) => {
      const collision = idsByCompactName.get(normalizeName(names.compact))?.size > 1
      if (collision) namesById.set(id, { ...names, compact: names.full })
    })

    return namesById
  }, [playerRecordsById, zoneLeaders])

  const getLeaderNames = (leader) => {
    return visibleLeaderNames.get(String(leader.playerId)) || {
      full: leader.playerName || '-',
      compact: leader.playerName || '-',
    }
  }

  // Zone short labels for the table
  const ZONE_SHORT = {
    'Zona (Restringida)': 'Zona Restringida',
    'Zona no restringida': 'Zona',
    'Media Distancia Centro': 'MD Centro',
    'Media Distancia Codo Derecha': 'MD Codo Der.',
    'Media Distancia Codo Izquierda': 'MD Codo Izq.',
    'Media Distancia Esquina Derecha': 'MD Esq. Der.',
    'Media Distancia Esquina Izquierda': 'MD Esq. Izq.',
    'Triple Centro': '3P Centro',
    'Triple Codo Derecha': '3P Codo Der.',
    'Triple Codo Izquierda': '3P Codo Izq.',
    'Triple Esquina Derecha': '3P Esq. Der.',
    'Triple Esquina Izquierda': '3P Esq. Izq.',
  }

  // Ordered zones for the table
  const ZONE_ORDER = [
    'Zona (Restringida)',
    'Zona no restringida',
    'Media Distancia Centro',
    'Media Distancia Codo Derecha',
    'Media Distancia Codo Izquierda',
    'Media Distancia Esquina Derecha',
    'Media Distancia Esquina Izquierda',
    'Triple Centro',
    'Triple Codo Derecha',
    'Triple Codo Izquierda',
    'Triple Esquina Derecha',
    'Triple Esquina Izquierda',
  ]

  // Font sizes per zone for the court labels
  const ZONE_FONT = {
    'Zona (Restringida)': { name: 9, stat: 10, sub: 8, photo: 64 },
    'Zona no restringida': { name: 10, stat: 11, sub: 9, photo: 82 },
    'Media Distancia Esquina Derecha': { name: 8, stat: 9, sub: 7, photo: 58 },
    'Media Distancia Esquina Izquierda': { name: 8, stat: 9, sub: 7, photo: 58 },
    'Triple Esquina Derecha': { name: 8, stat: 9, sub: 7, photo: 58 },
    'Triple Esquina Izquierda': { name: 8, stat: 9, sub: 7, photo: 58 },
  }
  const defaultFont = { name: 10, stat: 11, sub: 9, photo: 82 }

  const metricLabel = metric === 'points' ? 'Máximo anotador' : 'Mejor TC%'

  return (
    <ShotToolShell
      activeTool="leaders"
      title="Líderes por zona"
      scope={isLoading
        ? 'Cargando datos…'
        : `${selectedSeason - 1}-${String(selectedSeason).slice(-2)} · ${selectedTeam || 'Toda la liga'}`}
    >
      <section className={shotToolStyles.controlBand} aria-label="Filtros de líderes por zona">
        <div className={shotToolStyles.controlsGrid}>
          <div className={shotToolStyles.controlField}>
            <label className={shotToolStyles.controlLabel} htmlFor="zone-season">Temporada</label>
            <select
              id="zone-season"
              value={selectedSeason}
              onChange={(event) => updateState({ season: parseInt(event.target.value), teamId: null })}
              className="form-control"
            >
              {availableSeasons.map(season => (
                <option key={season} value={season}>{season - 1}-{String(season).slice(-2)}</option>
              ))}
            </select>
          </div>

          <div className={shotToolStyles.controlField}>
            <label className={shotToolStyles.controlLabel} htmlFor="zone-team">Ámbito</label>
            <select
              id="zone-team"
              value={selectedTeamId || ''}
              onChange={(event) => updateState({ teamId: event.target.value || null })}
              className="form-control"
            >
              <option value="">Toda la liga</option>
              {seasonTeamRows.map(team => (
                <option key={team.teamId} value={team.teamId}>{team.team}</option>
              ))}
            </select>
          </div>

          <div className={shotToolStyles.controlField}>
            <span className={shotToolStyles.controlLabel}>Intentos mínimos por zona</span>
            <div className={shotToolStyles.rangeRow}>
              <input
                type="range"
                min={3}
                max={50}
                value={minAttempts}
                onChange={(event) => updateState({ attempts: parseInt(event.target.value) })}
                aria-label={`Intentos mínimos por zona: ${minAttempts}`}
              />
              <output className={shotToolStyles.rangeValue} aria-live="polite">{minAttempts}</output>
            </div>
          </div>

          <div className={`${shotToolStyles.controlField} ${shotToolStyles.controlFieldWide}`}>
            <span className={shotToolStyles.controlLabel}>Criterio de liderazgo</span>
            <div className={`segmented-control ${shotToolStyles.segmentFill}`} aria-label="Métrica de líderes">
              <button
                type="button"
                className="segmented-option"
                aria-pressed={metric === 'points'}
                onClick={() => updateState({ nextMetric: 'points' })}
              >
                Puntos anotados
              </button>
              <button
                type="button"
                className="segmented-option"
                aria-pressed={metric === 'fgPct'}
                onClick={() => updateState({ nextMetric: 'fgPct' })}
              >
                Porcentaje de tiro
              </button>
            </div>
          </div>
        </div>
      </section>

      <section className={shotToolStyles.analysisSheet} aria-label={`${metricLabel} por zona`}>
        <div className={shotToolStyles.courtPanel}>
          <header className={shotToolStyles.panelHeader}>
            <div>
              <h2 className={shotToolStyles.panelTitle}>{selectedTeam || 'Toda la liga'}</h2>
              <p className={shotToolStyles.panelContext}>{metricLabel} en cada zona · mínimo {minAttempts} intentos</p>
            </div>
          </header>

          <div className={shotToolStyles.courtStage}>
            <div className={shotToolStyles.courtScroller} tabIndex="0" aria-label="Líderes sobre la cancha; desplázate horizontalmente para ver toda la cancha">
              <div className={`relative ${shotToolStyles.courtFrameWide}`} style={{ aspectRatio: '750 / 705' }}>
              <Court width={750} height={705} />

              <svg
                viewBox="0 0 750 705"
                className="absolute inset-0 w-full h-full"
                role="img"
                aria-label={`${selectedTeam || 'Toda la liga'}: ${metric === 'points' ? 'máximo anotador' : 'mejor porcentaje'} por zona`}
                style={{ pointerEvents: 'none' }}
              >
              <defs>
                {Object.entries(zonePolygons).map(([zoneName], zIdx) => {
                  const leader = zoneLeaders[zoneName]
                  if (!leader) return null
                  const photoUrl = getPlayerPhoto(playerPhotos, leader.playerId, selectedSeason)
                  if (!photoUrl) return null
                  const fonts = ZONE_FONT[zoneName] || defaultFont
                  const r = fonts.photo / 2
                  const customPos = CUSTOM_LABEL_POSITIONS[zoneName]
                  const labelPos = customPos || getPolygonCentroid(zonePolygons[zoneName])
                  const { x: labelX, y: labelY } = courtToSVG(labelPos.x, labelPos.y)
                  const textBaseY = labelY + r + 2
                  const cy = textBaseY - r - fonts.name - 2
                  const size = fonts.photo
                  return (
                    <pattern key={zIdx} id={`zph${zIdx}`} patternUnits="userSpaceOnUse"
                      x={labelX - r} y={cy - r} width={size} height={size}>
                      <image href={photoUrl} x="0" y="0" width={size} height={size}
                        preserveAspectRatio="xMidYMin slice" />
                    </pattern>
                  )
                })}
              </defs>
              {Object.entries(zonePolygons).map(([zoneName, points], zIdx) => {
                const leader = zoneLeaders[zoneName]
                const pathD = polygonToPath(points, scale, offsetX)
                const customPos = CUSTOM_LABEL_POSITIONS[zoneName]
                const labelPos = customPos || getPolygonCentroid(points)
                const { x: labelX, y: labelY } = courtToSVG(labelPos.x, labelPos.y)
                const fonts = ZONE_FONT[zoneName] || defaultFont

                if (!leader) {
                  return (
                    <g key={zoneName}>
                      <path d={pathD} fill={ZONE_FILL_EMPTY} stroke={ZONE_STROKE} strokeWidth="1.2" />
                      <text
                        x={labelX} y={labelY}
                        textAnchor="middle" fontSize={fonts.sub}
                        fill="#94a3b8" fontFamily="Inter, system-ui, sans-serif"
                      >
                        Sin datos
                      </text>
                    </g>
                  )
                }

                const leaderNames = getLeaderNames(leader)
                const displayName = leaderNames.compact

                let statValue
                let subLine
                if (metric === 'points') {
                  statValue = leader.points + ' pts'
                  subLine = leader.fgPct.toFixed(1) + '% · ' + leader.makes + '/' + leader.attempts
                } else {
                  statValue = leader.fgPct.toFixed(1) + '%'
                  subLine = leader.makes + '/' + leader.attempts
                }

                const photoUrl = getPlayerPhoto(playerPhotos, leader.playerId, selectedSeason)
                const r = fonts.photo / 2
                const photoOffset = photoUrl ? r + 2 : 0
                const textBaseY = labelY + photoOffset
                const photoCy = textBaseY - r - fonts.name - 2

                return (
                  <g
                    key={zoneName}
                    role="group"
                    aria-label={`${leaderNames.full}, ${zoneName}, ${statValue}, ${leader.makes} de ${leader.attempts}`}
                  >
                    <title>{leaderNames.full} · {zoneName} · {statValue}</title>
                    <path d={pathD} fill={ZONE_FILL} stroke={ZONE_STROKE} strokeWidth="1.2" />

                    {/* Player photo as pattern-filled circle */}
                    {photoUrl && (
                      <circle
                        cx={labelX}
                        cy={photoCy}
                        r={r}
                        fill={`url(#zph${zIdx})`}
                      />
                    )}

                    {/* Player name */}
                    <text
                      x={labelX} y={textBaseY}
                      textAnchor="middle" fontSize={fonts.name}
                      fontWeight="600" fill="#0f172a"
                      fontFamily="Inter, system-ui, sans-serif"
                    >
                      {displayName}
                    </text>

                    {/* Main stat */}
                    <text
                      x={labelX} y={textBaseY + fonts.stat + 2}
                      textAnchor="middle" fontSize={fonts.stat + 1}
                      fontWeight="bold" fill="#0f172a"
                      fontFamily="JetBrains Mono, Consolas, monospace"
                    >
                      {statValue}
                    </text>

                    {/* Sub line */}
                    <text
                      x={labelX} y={textBaseY + fonts.stat + fonts.sub + 6}
                      textAnchor="middle" fontSize={fonts.sub}
                      fill="#0f172a"
                      fontFamily="JetBrains Mono, Consolas, monospace"
                    >
                      {subLine}
                    </text>
                  </g>
                )
              })}
              </svg>
            </div>
          </div>
        </div>

        </div>

        <aside className={shotToolStyles.sidePanel} aria-label="Detalle de líderes por zona">
          <header className={shotToolStyles.sideHeader}>
            <h2 className={shotToolStyles.sideTitle}>Detalle por zona</h2>
            <p className={shotToolStyles.sideSubtitle}>{metricLabel} · {selectedTeam || 'toda la liga'}</p>
          </header>
          <div className={shotToolStyles.zoneList}>
            {ZONE_ORDER.map(zone => {
              const leader = zoneLeaders[zone]
              const shortZone = ZONE_SHORT[zone] || zone
              const leaderNames = leader ? getLeaderNames(leader) : null
              return (
                <div key={zone} className={shotToolStyles.zoneRow}>
                  <div className={shotToolStyles.leaderIdentity}>
                    {leader && getPlayerPhoto(playerPhotos, leader.playerId, selectedSeason) && (
                      <img
                        src={getPlayerPhoto(playerPhotos, leader.playerId, selectedSeason)}
                        alt=""
                        className={shotToolStyles.leaderPhoto}
                      />
                    )}
                    <div className="min-w-0">
                      <div className={shotToolStyles.zoneSample}>{shortZone}</div>
                      {leader ? (
                        <>
                          <div
                            className={shotToolStyles.leaderName}
                            title={leaderNames.full}
                            aria-label={leaderNames.full}
                          >
                            {leaderNames.full}
                          </div>
                          {!selectedTeam && (
                            <div className={shotToolStyles.leaderTeam}>{leader.team}</div>
                          )}
                        </>
                      ) : (
                        <div className={shotToolStyles.emptyMessage}>Sin datos</div>
                      )}
                    </div>
                  </div>
                  {leader && (
                    <div className={shotToolStyles.zoneReading}>
                      <div>
                        {metric === 'points'
                          ? leader.points + ' pts'
                          : leader.fgPct.toFixed(1) + '%'}
                      </div>
                      <div className={shotToolStyles.zoneSecondary}>
                        {leader.makes}/{leader.attempts}
                      </div>
                    </div>
                  )}
                </div>
              )
            })}
          </div>
        </aside>
      </section>
    </ShotToolShell>
  )
}
