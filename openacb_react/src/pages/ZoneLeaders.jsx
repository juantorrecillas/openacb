import { useState, useMemo, useEffect } from 'react'
import { useParams, useNavigate } from 'react-router-dom'
import Court from '../components/Court'
import { getPlayerPhoto } from '../utils/playerPhotos'
import { getPlayerCompactName, getPlayerDisplayName } from '../utils/playerNames'
import PageHeader from '../components/PageHeader'
import { Filter } from 'lucide-react'


// Metric slug ↔ value mapping for URLs
const METRIC_SLUGS = { 'maximo-anotador': 'points', 'mejor-eficiencia': 'fgPct' }
const METRIC_TO_SLUG = { points: 'maximo-anotador', fgPct: 'mejor-eficiencia' }

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

const ZONE_FILL = 'rgba(251, 191, 128, 0.75)'     // light orange
const ZONE_FILL_EMPTY = 'rgba(251, 191, 128, 0.15)'
const ZONE_STROKE = 'rgba(234, 150, 80, 0.5)'     // softer orange

// ─── Component ────────────────────────────────────────────────────────────────

export default function ZoneLeaders({ loadShotsForSeason, shotsCache, loadingShots, teams, players, playerPhotos = {} }) {
  const { season: urlSeason, metric: urlMetricSlug } = useParams()
  const navigate = useNavigate()
  const availableSeasons = useMemo(() => {
    const seasons = [...new Set(teams.map(t => t.season))].filter(s => s >= 2021).sort((a, b) => b - a)
    return seasons
  }, [teams])

  const [selectedSeason, setSelectedSeason] = useState(availableSeasons[0] || 2026)
  const [minAttempts, setMinAttempts] = useState(15)
  const [selectedTeam, setSelectedTeam] = useState('')
  const [metric, setMetric] = useState('points') // 'fgPct' or 'points'

  // Sync from URL params
  useEffect(() => {
    if (urlSeason) setSelectedSeason(Number(urlSeason))
    if (urlMetricSlug && METRIC_SLUGS[urlMetricSlug]) setMetric(METRIC_SLUGS[urlMetricSlug])
  }, [urlSeason, urlMetricSlug])

  // Push URL when season or metric changes
  const updateUrl = (season, met) => {
    navigate(`/lideres-zona/${season}/${METRIC_TO_SLUG[met]}`, { replace: true })
  }

  useEffect(() => {
    if (selectedSeason) loadShotsForSeason(selectedSeason)
  }, [selectedSeason, loadShotsForSeason])

  const seasonShots = useMemo(() => shotsCache[selectedSeason] || [], [shotsCache, selectedSeason])
  const isLoading = loadingShots[selectedSeason] || false

  const teamList = useMemo(() =>
    [...new Set(seasonShots.map(s => s.team))].sort(),
    [seasonShots]
  )

  useEffect(() => {
    if (selectedTeam && !teamList.includes(selectedTeam)) setSelectedTeam('')
  }, [selectedTeam, teamList])

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

  return (
    <div className="app-page space-y-6">
      <PageHeader
        title="Líderes por zona"
        subtitle={metric === 'points' ? 'Jugador con más puntos en cada zona del campo' : 'Jugador con mejor porcentaje de tiro en cada zona del campo'}
        scope={isLoading
          ? 'Cargando datos…'
          : `${selectedSeason - 1}-${String(selectedSeason).slice(-2)} · ${selectedTeam || 'Toda la liga'}`}
      />

      {/* Filters */}
      <div className="filter-panel block">
        <div className="flex items-center gap-2 mb-4">
          <Filter className="w-4 h-4 text-acb-500" />
          <span className="text-sm font-medium text-acb-700">Filtros</span>
        </div>

        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4">
          {/* Season */}
          <div>
            <label className="field-label mb-1">Temporada</label>
            <select
              aria-label="Temporada"
              value={selectedSeason}
              onChange={(e) => {
                const s = parseInt(e.target.value)
                setSelectedSeason(s)
                setSelectedTeam('')
                updateUrl(s, metric)
              }}
              className="form-control"
            >
              {availableSeasons.map(season => (
                <option key={season} value={season}>{season - 1}-{String(season).slice(-2)}</option>
              ))}
            </select>
          </div>

          {/* Team */}
          <div>
            <label className="field-label mb-1">Equipo</label>
            <select
              aria-label="Equipo"
              value={selectedTeam}
              onChange={(e) => setSelectedTeam(e.target.value)}
              className="form-control"
            >
              <option value="">Toda la liga</option>
              {teamList.map(team => (
                <option key={team} value={team}>{team}</option>
              ))}
            </select>
          </div>

          {/* Min Attempts */}
          <div>
            <label className="field-label mb-1">
              Intentos mínimos: {minAttempts}
            </label>
            <input
              type="range"
              min={3}
              max={50}
              value={minAttempts}
              onChange={(e) => setMinAttempts(parseInt(e.target.value))}
              aria-label={`Intentos mínimos por zona: ${minAttempts}`}
              className="w-full accent-slate-700"
            />
            <div className="flex justify-between text-xs text-acb-400 mt-0.5">
              <span>3</span>
              <span>50</span>
            </div>
          </div>

          {/* Metric */}
          <div>
            <label className="field-label mb-1">Ordenar por</label>
            <select
              aria-label="Métrica de líderes"
              value={metric}
              onChange={(e) => {
                const m = e.target.value
                setMetric(m)
                updateUrl(selectedSeason, m)
              }}
              className="form-control"
            >
              <option value="points">Máximo anotador (puntos)</option>
              <option value="fgPct">Mejor TC% por zona</option>
            </select>
          </div>
        </div>
      </div>

      {/* Court + Table layout */}
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        {/* Court */}
        <div className="lg:col-span-2 bg-white rounded-lg border border-acb-200 p-6">
          <div className="flex items-center justify-between mb-4">
            <h3 className="font-medium text-acb-900">
              {selectedTeam || 'Toda la liga'} — {metric === 'points' ? 'Máximo anotador' : 'Mejor TC%'} por zona
            </h3>

          </div>

          <div className="overflow-x-auto pb-1">
            <div className="relative min-w-[660px]" style={{ width: '100%', maxWidth: 750, aspectRatio: '750 / 705' }}>
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
                      <path d={pathD} fill={ZONE_FILL_EMPTY} stroke={ZONE_STROKE} strokeWidth="1" />
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
                    <path d={pathD} fill={ZONE_FILL} stroke={ZONE_STROKE} strokeWidth="1" />

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

        {/* Table sidebar */}
        <div className="bg-white rounded-lg border border-acb-200 p-4">
          <h3 className="text-sm font-medium text-acb-700 mb-3">Detalle</h3>
          <div className="space-y-1">
            {ZONE_ORDER.map(zone => {
              const leader = zoneLeaders[zone]
              const shortZone = ZONE_SHORT[zone] || zone
              const leaderNames = leader ? getLeaderNames(leader) : null
              return (
                <div key={zone} className="flex items-center justify-between py-2 border-b border-acb-100 last:border-0">
                  <div className="flex items-center gap-2 min-w-0">
                    {leader && getPlayerPhoto(playerPhotos, leader.playerId, selectedSeason) && (
                      <img
                        src={getPlayerPhoto(playerPhotos, leader.playerId, selectedSeason)}
                        alt=""
                        className="w-8 h-8 rounded-full object-cover border border-acb-200 flex-shrink-0"
                      />
                    )}
                    <div className="min-w-0">
                      <div className="text-xs font-medium text-acb-500 truncate">{shortZone}</div>
                      {leader ? (
                        <>
                          <div
                            className="text-sm font-semibold text-acb-900 truncate"
                            title={leaderNames.full}
                            aria-label={leaderNames.full}
                          >
                            {leaderNames.full}
                          </div>
                          {!selectedTeam && (
                            <div className="text-xs text-acb-400 truncate">{leader.team}</div>
                          )}
                        </>
                      ) : (
                        <div className="text-xs text-acb-400">Sin datos</div>
                      )}
                    </div>
                  </div>
                  {leader && (
                    <div className="text-right shrink-0 ml-2">
                      <div className="text-sm font-bold font-mono text-acb-900">
                        {metric === 'points'
                          ? leader.points + ' pts'
                          : leader.fgPct.toFixed(1) + '%'}
                      </div>
                      <div className="text-xs text-acb-500 font-mono">
                        {leader.makes}/{leader.attempts}
                      </div>
                    </div>
                  )}
                </div>
              )
            })}
          </div>
        </div>
      </div>
    </div>
  )
}
