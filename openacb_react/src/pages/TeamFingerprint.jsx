import { useMemo, useEffect, useRef, useState } from 'react'
import { Link, useParams, useNavigate, useSearchParams } from 'react-router-dom'
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer } from 'recharts'
import TeamToolShell, { teamToolStyles as styles } from '../components/TeamToolShell'
import TeamRosterTable from '../components/TeamRosterTable'
import ZoneHeatmap from '../components/ZoneHeatmap'
import { buildTeamProfilePath } from '../routing/paths'
import { readRouteQuery, serializeRouteQuery } from '../routing/query'
import { createTeamIdentityIndexFromRows, resolveTeamId } from '../routing/teamIdentities'
import profileStyles from './TeamFingerprint.module.css'
import { offensiveAxes, defensiveAxes, getTeamRadarAxes } from '../utils/teamRadar'
import TeamRadar from '../components/TeamRadar'


// ─── Axis Definitions ─────────────────────────────────────────

const trendStatOptions = [
  { key: 'wins', label: 'Victorias', format: 'integer' },
  { key: 'ppg', label: 'Puntos/Partido', format: 'decimal' },
  { key: 'rpg', label: 'Rebotes/Partido', format: 'decimal' },
  { key: 'apg', label: 'Asistencias/Partido', format: 'decimal' },
  { key: 'spg', label: 'Robos/Partido', format: 'decimal' },
  { key: 'bpg', label: 'Tapones/Partido', format: 'decimal' },
  { key: 'topg', label: 'Pérdidas/Partido', format: 'decimal' },
  { key: 'fgPct', label: 'TC%', format: 'pct100' },
  { key: 'ftPct', label: 'TL%', format: 'pct100' },
  { key: 'ortg', label: 'Rating Ofensivo', format: 'decimal' },
  { key: 'drtg', label: 'Rating Defensivo', format: 'decimal' },
  { key: 'netRtg', label: 'Rating Neto', format: 'decimal' },
  { key: 'pace', label: 'Ritmo', format: 'decimal' },
  { key: 'efg', label: 'eFG%', format: 'pctDecimal' },
  { key: 'ts', label: 'TS%', format: 'pctDecimal' },
  { key: 'threePct', label: '3P%', format: 'pctDecimal' },
  { key: 'threeRate', label: 'Ratio 3P', format: 'pctDecimal' },
  { key: 'astRate', label: 'Ratio Asist.', format: 'pctDecimal' },
  { key: 'tovRate', label: 'Ratio Pérdidas', format: 'pctDecimal' },
  { key: 'orbPct', label: 'RO%', format: 'pctDecimal' },
  { key: 'drbPct', label: 'RD%', format: 'pctDecimal' },
  { key: 'ftRate', label: 'Ratio TL', format: 'pctDecimal' },
  { key: 'stlRate', label: 'Ratio Robos', format: 'pctDecimal' },
  { key: 'blkRate', label: 'Ratio Tapones', format: 'pctDecimal' },
  { key: 'astToRatio', label: 'Ratio AST/PER', format: 'decimal', digits: 2 },
]

const franchiseMatchers = [
  { key: 'real madrid', includes: ['real madrid'] },
  { key: 'barcelona', includes: ['barca', 'barcelona'] },
  { key: 'baskonia', includes: ['baskonia'] },
  { key: 'gran canaria', includes: ['gran canaria'] },
  { key: 'joventut', includes: ['joventut'] },
  { key: 'tenerife', includes: ['tenerife'] },
  { key: 'manresa', includes: ['manresa'] },
  { key: 'zaragoza', includes: ['zaragoza'] },
  { key: 'betis', includes: ['betis'] },
  { key: 'breogan', includes: ['breogan'] },
  { key: 'bilbao basket', includes: ['bilbao basket'] },
  { key: 'obradoiro', includes: ['obradoiro'] },
  { key: 'fuenlabrada', includes: ['fuenlabrada'] },
  { key: 'san pablo burgos', includes: ['san pablo burgos'] },
  { key: 'gbc', includes: ['gbc', 'guipuzcoa'] },
  { key: 'andorra', includes: ['andorra'] },
  { key: 'estudiantes', includes: ['estudiantes'] },
  { key: 'unicaja', includes: ['unicaja'] },
  { key: 'valencia basket', includes: ['valencia basket'] },
  { key: 'ucam murcia', includes: ['ucam murcia'] },
  { key: 'basquet girona', includes: ['basquet girona'] },
  { key: 'coviran granada', includes: ['coviran granada'] },
  { key: 'hiopos lleida', includes: ['hiopos lleida'] },
  { key: 'leyma coruna', includes: ['leyma coruna'] },
  { key: 'zunder palencia', includes: ['zunder palencia'] },
]

// ─── Narrative Templates ──────────────────────────────────────
// A variant is picked based on z-score intensity (moderate vs extreme).

const offensivePhrases = {
  ortg: {
    pos: [
      'Muy buen rendimiento ofensivo',
      'Uno de los ataques más eficientes: genera puntos con una consistencia excelente',
    ],
    neg: [
      'Le cuesta rendir en ataque en general',
      'El rendimiento ofensivo está muy por debajo de la media; necesita mejorar su ataque',
    ],
  },
  pace: {
    pos: [
      'Juega a un ritmo alto, generando más posesiones que la mayoría de equipos',
      'Uno de los equipos más rápidos de la liga; busca el contraataque y el juego en transición',
    ],
    neg: [
      'Prefiere el juego lento y controlado, reduciendo el número de posesiones',
      'Ritmo de juego muy bajo; domina el balón y controla los tiempos de manera posicional',
    ],
  },
  threeRate: {
    pos: [
      'Busca el triple con insistencia y lo convierte en el eje de su ataque',
      'El tiro exterior domina su juego ofensivo',
    ],
    neg: [
      'Apenas recurre al triple, concentrando todo su ataque cerca del aro',
      'Prescinde casi por completo del tiro exterior',
    ],
  },
  threePct: {
    pos: [
      'Anota de tres con fiabilidad',
      'Gran acierto desde el triple; castiga cualquier descuido defensivo',
    ],
    neg: [
      'El tiro de tres no está funcionando y pierde una fuente clave de eficiencia ofensiva',
      'Falla más triples de lo esperado, limitando severamente su juego exterior',
    ],
  },
  ts: {
    pos: [
      'Elige bien sus tiros y los convierte con frecuencia; muy buen porcentaje real',
      'Ataque eficiente y genial selección de tiro',
    ],
    neg: [
      'Le cuesta convertir sus lanzamientos',
      'Demasiados tiros fallados; necesita mejorar la selección o la precisión',
    ],
  },
  astRate: {
    pos: [
      'Mueve bien el balón y encuentra al compañero mejor posicionado con criterio',
      'Juego asociativo fluido: las canastas vienen muy frecuentemente de asistencias más que de acciones individuales',
    ],
    neg: [
      'Tiende al juego individual y le falta circulación de balón',
      'Pocas asistencias por posesión; depende demasiado de soluciones individuales',
    ],
  },
  orbPct: {
    pos: [
      'Domina el rebote ofensivo y consigue segundas oportunidades con frecuencia',
      'Muy activo en el rebote ofensivo; es capaz de generar segundas oportunidades frecuentemente',
    ],
    neg: [
      'No compite en el rebote ofensivo y cada fallo se convierte en una posesión perdida',
      'Rara vez captura su propio rebote; pierde muchas segundas oportunidades',
    ],
  },
  tovRate: {
    pos: [
      'Cuida el balón con mimo, comete muy pocas pérdidas por posesión',
      'Equipo ordenado que casi nunca regala posesiones al rival',
    ],
    neg: [
      'Pierde el balón con demasiada frecuencia, regalando posesiones fáciles al rival',
      'Las pérdidas de balón le están lastrando; demasiadas posesiones desperdiciadas',
    ],
  },
}

const defensivePhrases = {
  drtg: {
    pos: [
      'Defensa sólida que complica enormemente al rival en cada posesión',
      'Plantea un muro difícil de superar; uno de los mejores ratings defensivos',
    ],
    neg: [
      'Sufre atrás y concede puntos con demasiada facilidad',
      'La defensa no consigue contener al rival, que anota con comodidad',
    ],
  },
  opp_threePct: {
    pos: [
      'Cierra bien el perímetro y los rivales fallan más triples de lo habitual',
      'Los contrarios se frustran desde el arco contra este equipo',
    ],
    neg: [
      'Deja líneas de tiro abiertas desde fuera y los rivales lo aprovechan',
      'El triple rival le hace mucho daño; los contrarios tiran cómodos y aciertan',
    ],
  },
  opp_ts: {
    pos: [
      'Obliga al rival a lanzar en malas condiciones y baja su eficiencia real',
      'Hace que cada punto del contrario cueste un esfuerzo extra',
    ],
    neg: [
      'Permite que el rival anote con porcentajes altos y sin demasiada oposición',
      'No genera incomodidad suficiente al tirador; el rival convierte con soltura',
    ],
  },
  opp_tovRate: {
    pos: [
      'Presiona al rival hasta provocar errores y genera muchas pérdidas',
      'Su intensidad defensiva desestabiliza al contrario, que pierde balones con frecuencia',
    ],
    neg: [
      'No consigue incomodar al rival con el balón',
      'La defensa no genera presión suficiente para forzar errores del contrario',
    ],
  },
  stlRate: {
    pos: [
      'Roba balones a un ritmo alto y convierte esas recuperaciones en transiciones peligrosas',
      'Muy activo en las líneas de pase; intercepta con frecuencia',
    ],
    neg: [
      'Apenas roba balones en juego; defensa más posicional que agresiva',
      'Poca actividad en las líneas de pase; no genera robos directos',
    ],
  },
  blkRate: {
    pos: [
      'Protege el aro de forma eficiente. Tapona con frecuencia tiros del rival',
      'Protección del aro muy eficiente. Disuade al rival de tirar cerca del aro',
    ],
    neg: [
      'No destaca particularmente por su protección del aro',
      'Mala protección de la zona. No tapona apenas lanzamientos',
    ],
  },
  drbPct: {
    pos: [
      'El equipo es buen reboteador defensivo',
      'Rebote defensivo de élite; limita las segundas oportunidades del rival',
    ],
    neg: [
      'Concede demasiados rebotes ofensivos al rival',
      'Sufre especial debilidad en el rebote defensivo; concede excesivas segundas oportunidades',
    ],
  },
}

// Pick phrase variant based on z-score intensity
function pickPhrase(phrases, z) {
  const list = z > 0 ? phrases.pos : phrases.neg
  const absZ = Math.abs(z)
  // Extreme z-scores (>1.5) use the second, more emphatic variant
  return absZ > 1.5 ? list[1] : list[0]
}

// ─── Team Narrative Summary ─────────────────────────────────

function buildNarrative(teamName, strengths, weaknesses) {
  if (strengths.length === 0 && weaknesses.length === 0) {
    return `${teamName} presenta un perfil equilibrado, sin grandes desviaciones respecto a la media de la liga.`
  }

  const listLabels = (items) => {
    const labels = items.map(item => item.label.toLowerCase())
    if (labels.length <= 1) return labels[0] || ''
    return `${labels.slice(0, -1).join(', ')} y ${labels.at(-1)}`
  }

  const advantages = listLabels(strengths.slice(0, 3))
  const risks = listLabels(weaknesses.slice(0, 2))

  if (!risks) return `${teamName} obtiene sus mayores ventajas relativas en ${advantages}.`
  if (!advantages) return `${teamName} encuentra sus principales desventajas en ${risks}.`

  return `${teamName} obtiene sus mayores ventajas relativas en ${advantages}. Sus principales desventajas aparecen en ${risks}.`
}

// ─── Radar Chart ──────────────────────────────────────────────

function RadarChart({ axes, values, strokeColor, title }) {
  return (
    <div className={profileStyles.radarFigure}>
      <h3 className={profileStyles.radarTitle}>{title}</h3>
      <TeamRadar axes={axes.map((axis, index) => ({ ...axis, value: values[index] }))} name={title} color={strokeColor} />
    </div>
  )
}

// ─── Z-Score Badge ────────────────────────────────────────────

function ZBadge({ z }) {
  const colorClass = z > 0
    ? profileStyles.deltaPositive
    : z < 0
      ? profileStyles.deltaNegative
      : profileStyles.deltaNeutral

  return (
    <span className={`${profileStyles.delta} ${colorClass}`}>
      {z >= 0 ? '+' : ''}{z.toFixed(2)}
    </span>
  )
}

// ─── Scouting traits ──────────────────────────────────────────

function TraitList({ title, items, emptyMsg }) {
  return (
    <section className={profileStyles.traitColumn}>
      <div className={profileStyles.traitHeader}>
        <h3>{title}</h3>
        <span>{items.length} {items.length === 1 ? 'rasgo' : 'rasgos'}</span>
      </div>
      {items.length === 0 ? (
        <p className={profileStyles.traitEmpty}>{emptyMsg}</p>
      ) : (
        <div>
          {items.map((item) => (
            <article key={`${item.category}-${item.axisKey}`} className={profileStyles.traitItem}>
              <div className={profileStyles.traitTopline}>
                <span className={`${profileStyles.traitCategory} ${item.category === 'Defensivo' ? profileStyles.traitCategoryDefense : ''}`}>
                  {item.category}
                </span>
                <strong className={profileStyles.traitLabel}>{item.label}</strong>
                <ZBadge z={item.z} />
              </div>
              <p className={profileStyles.traitPhrase}>{item.phrase}</p>
              <div className={profileStyles.traitComparison}>
                <span className={profileStyles.traitValue}>{item.value}</span>
                <span className={profileStyles.traitAverage}>Media de liga {item.avg}</span>
              </div>
            </article>
          ))}
        </div>
      )}
    </section>
  )
}

function MetricTable({ title, axes, team, leagueStats, zScores }) {
  return (
    <section className={profileStyles.metricPane}>
      <h3 className={profileStyles.metricTitle}>{title}</h3>
      <table className={profileStyles.metricTable}>
        <thead>
          <tr>
            <th scope="col">Métrica</th>
            <th scope="col">Equipo</th>
            <th scope="col">Liga</th>
            <th scope="col">Δ</th>
          </tr>
        </thead>
        <tbody>
          {axes.map(axis => (
            <tr key={axis.key}>
              <th scope="row">{axis.label}</th>
              <td>{axis.format(team[axis.key])}</td>
              <td>{axis.format(leagueStats[axis.key].mean)}</td>
              <td><ZBadge z={zScores[axis.key]} /></td>
            </tr>
          ))}
        </tbody>
      </table>
    </section>
  )
}

function normalizeTeamName(name) {
  return name
    .normalize('NFD').replace(/[\u0300-\u036f]/g, '')
    .toLowerCase()
    .replace(/\s+/g, ' ')
    .trim()
}

function franchiseKey(name) {
  const normalized = normalizeTeamName(name)
  const match = franchiseMatchers.find(item =>
    item.includes.some(part => normalized.includes(part))
  )
  return match?.key || normalized
}

function formatTrendValue(value, stat) {
  if (value == null || Number.isNaN(Number(value))) return '-'
  const numeric = Number(value)

  if (stat.format === 'integer') return Math.round(numeric).toString()
  if (stat.format === 'pctDecimal') return `${(numeric * 100).toFixed(1)}%`
  if (stat.format === 'pct100') return `${numeric.toFixed(1)}%`

  return numeric.toFixed(stat.digits ?? 1)
}

function getTrendAxisValue(value, stat) {
  if (stat.format === 'pctDecimal') return value * 100
  return value
}

function getTrendDataValue(value, stat) {
  if (stat.format === 'pctDecimal') return value / 100
  return value
}

function formatTrendAxisValue(value, stat) {
  const axisValue = getTrendAxisValue(Number(value), stat)
  if (!Number.isFinite(axisValue)) return ''
  if (axisValue < 5 && axisValue > -5 && stat.format === 'decimal') return axisValue.toFixed(1)
  return Math.round(axisValue).toString()
}

function buildNiceTrendAxis(values, stat) {
  if (!values.length) return { domain: ['auto', 'auto'], ticks: undefined }

  const axisValues = values.map(value => getTrendAxisValue(value, stat))
  const min = Math.min(...axisValues)
  const max = Math.max(...axisValues)
  const spread = max - min
  const pad = spread === 0 ? 1 : Math.max(spread * 0.12, 0.5)
  const low = min - pad
  const high = max + pad
  const maxAbs = Math.max(Math.abs(low), Math.abs(high))
  const step = maxAbs < 5 && stat.format === 'decimal' ? 0.5 : 5
  const start = Math.floor(low / step) * step
  const end = Math.ceil(high / step) * step
  const count = Math.round((end - start) / step) + 1
  const axisTicks = Array.from({ length: count }, (_, index) => start + index * step)
  const dataTicks = axisTicks.map(value => getTrendDataValue(value, stat))

  return {
    domain: [getTrendDataValue(start, stat), getTrendDataValue(end, stat)],
    ticks: dataTicks,
  }
}

function TrendTooltip({ active, payload, stat }) {
  if (!active || !payload?.length) return null

  const row = payload[0].payload

  return (
    <div className="rounded-lg border border-acb-200 bg-white/95 px-3 py-2 shadow-sm">
      <p className="text-xs font-semibold text-acb-900">{row.seasonText}</p>
      <p className="text-[11px] text-acb-500">{row.team}</p>
      <p className="mt-1 text-xs text-acb-700">
        {stat.label}: <span className="font-mono font-semibold text-accent-700">{formatTrendValue(row.value, stat)}</span>
      </p>
    </div>
  )
}

function TeamTrendChart({ teams, selectedTeam, selectedSeason, trendStat, onTrendStatChange }) {
  const selectedStat = useMemo(
    () => trendStatOptions.find(stat => stat.key === trendStat) || trendStatOptions[0],
    [trendStat],
  )

  const history = useMemo(() => {
    if (!selectedTeam) return []

    const key = franchiseKey(selectedTeam)

    return teams
      .filter(row => franchiseKey(row.team) === key)
      .filter(row => row[selectedStat.key] != null && Number.isFinite(Number(row[selectedStat.key])))
      .sort((a, b) => a.season - b.season)
      .map(row => ({
        season: row.season,
        seasonText: seasonLabel(row.season),
        team: row.team,
        value: Number(row[selectedStat.key]),
      }))
  }, [teams, selectedTeam, selectedStat])

  const yAxis = useMemo(() => {
    return buildNiceTrendAxis(history.map(row => row.value), selectedStat)
  }, [history, selectedStat])

  return (
    <section className={profileStyles.trendPane} aria-labelledby="team-trend-title">
      <div className={profileStyles.trendHeader}>
        <div>
          <h3 id="team-trend-title" className={profileStyles.trendTitle}>Trayectoria</h3>
          <p className={profileStyles.trendMeta}>{selectedStat.label} · evolución por temporada</p>
        </div>
        <label className="sr-only" htmlFor="team-trend-stat">Métrica</label>
        <select
          id="team-trend-stat"
          value={trendStat}
          onChange={(e) => onTrendStatChange(e.target.value)}
          className={profileStyles.trendSelect}
        >
          {trendStatOptions.map(stat => (
            <option key={stat.key} value={stat.key}>{stat.label}</option>
          ))}
        </select>
      </div>

      {history.length < 2 ? (
        <div className={profileStyles.trendEmpty}>
          Historial insuficiente
        </div>
      ) : (
        <div className={profileStyles.trendChart}>
          <ResponsiveContainer width="100%" height="100%">
            <LineChart data={history} margin={{ top: 8, right: 12, bottom: 2, left: 0 }}>
              <CartesianGrid stroke="#e2e8f0" strokeDasharray="3 3" vertical={false} />
              <XAxis
                dataKey="seasonText"
                axisLine={false}
                tickLine={false}
                tick={{ fontSize: 11, fill: '#627d98' }}
                minTickGap={8}
              />
              <YAxis
                width={48}
                domain={yAxis.domain}
                ticks={yAxis.ticks}
                axisLine={false}
                tickLine={false}
                tick={{ fontSize: 11, fill: '#627d98' }}
                tickFormatter={(value) => formatTrendAxisValue(value, selectedStat)}
              />
              <Tooltip
                cursor={{ stroke: '#f0845e', strokeWidth: 1, strokeDasharray: '4 4' }}
                content={({ active, payload }) => <TrendTooltip active={active} payload={payload} stat={selectedStat} />}
              />
              <Line
                type="monotone"
                dataKey="value"
                stroke="#fe5917"
                strokeWidth={2.5}
                dot={(props) => {
                  const isSelected = props.payload.season === selectedSeason
                  return (
                    <circle
                      key={`trend-dot-${props.payload.season}`}
                      cx={props.cx}
                      cy={props.cy}
                      r={isSelected ? 4.5 : 3.2}
                      fill={isSelected ? '#fe5917' : '#ffffff'}
                      stroke="#fe5917"
                      strokeWidth={isSelected ? 2.2 : 1.6}
                    />
                  )
                }}
                activeDot={{ r: 5, fill: '#fe5917', stroke: '#ffffff', strokeWidth: 2 }}
              />
            </LineChart>
          </ResponsiveContainer>
        </div>
      )}
    </section>
  )
}

// ─── perfil de tiro por zonas ─────────────────────────────────

function TeamShotProfile({ team, shots, isLoading, isAvailable, mode, metric, onModeChange, onMetricChange }) {
  const teamShots = useMemo(() => {
    if (!team) return []
    return mode === 'attack'
      ? shots.filter(shot => shot.team === team)
      : shots.filter(shot => shot.opponent === team)
  }, [mode, shots, team])

  const toggleBtn = (active, onClick, label) => (
    <button
      type="button"
      onClick={onClick}
      aria-pressed={active}
      className={`${profileStyles.toggleButton} ${active ? profileStyles.toggleButtonActive : ''}`}
    >
      {label}
    </button>
  )

  if (!isAvailable) {
    return (
      <section className={profileStyles.shotSection} aria-labelledby="shot-profile-title">
        <header className={profileStyles.shotHeader}>
          <div>
            <h2 id="shot-profile-title" className={profileStyles.sectionTitle}>Mapa de tiro</h2>
            <p className={profileStyles.sectionSubtitle}>Eficiencia y volumen comparados con la media de la liga.</p>
          </div>
        </header>
        <p className={profileStyles.shotUnavailable}>Datos disponibles desde la temporada 2020-21.</p>
      </section>
    )
  }

  return (
    <section className={profileStyles.shotSection} aria-labelledby="shot-profile-title">
      <header className={profileStyles.shotHeader}>
        <div>
          <h2 id="shot-profile-title" className={profileStyles.sectionTitle}>Mapa de tiro</h2>
          <p className={profileStyles.sectionSubtitle}>Eficiencia y volumen comparados con la media de la liga en cada zona.</p>
        </div>
        <div className={profileStyles.shotControls}>
          <div className={profileStyles.toggleGroup}>
            {toggleBtn(mode === 'attack', () => onModeChange('attack'), 'Ataque')}
            {toggleBtn(mode === 'defense', () => onModeChange('defense'), 'Defensa')}
          </div>
          <div className={profileStyles.toggleGroup}>
            {toggleBtn(metric === 'efficiency', () => onMetricChange('efficiency'), 'Eficiencia')}
            {toggleBtn(metric === 'frequency', () => onMetricChange('frequency'), 'Frecuencia')}
          </div>
        </div>
      </header>
      {isLoading ? (
        <div className={profileStyles.shotLoading} role="status">Cargando tiros...</div>
      ) : (
        <div className={profileStyles.shotCanvas}>
          <ZoneHeatmap
            shots={teamShots}
            leagueShots={shots}
            metric={metric}
            higherIsBetter={mode !== 'defense'}
            width={430}
            height={405}
          />
        </div>
      )}
    </section>
  )
}

// etiqueta de temporada
function seasonLabel(s) {
  return `${s - 1}-${String(s).slice(-2)}`
}

function formatSignedMetric(value) {
  if (value == null || !Number.isFinite(Number(value))) return '-'
  const numeric = Number(value)
  return `${numeric > 0 ? '+' : ''}${numeric.toFixed(1)}`
}

function parseSeasonParam(value, availableSeasons) {
  const season = Number(value)
  return availableSeasons.includes(season) ? season : (availableSeasons[0] || 2025)
}

function buildTeamProfileSearch({ season, trendStat, mode, metric, reference = 'season' }) {
  return new URLSearchParams(serializeRouteQuery('teamProfile', {
    temporada: season,
    referencia: reference,
    tendencia: trendStat,
    lado: mode,
    metrica: metric,
  }))
}

export default function TeamFingerprint({
  teams,
  players = [],
  playerBio = {},
  teamLogos = {},
  loadShotsForSeason,
  shotsCache = {},
  loadingShots = {},
}) {
  const routeParams = useParams()
  const navigate = useNavigate()
  const [searchParams, setSearchParams] = useSearchParams()
  const [showFloatingTeamHeader, setShowFloatingTeamHeader] = useState(false)
  const teamHeaderRef = useRef(null)
  const urlTeamId = routeParams.teamId || routeParams.team || ''

  const availableSeasons = useMemo(() => {
    return [...new Set(teams.map(t => t.season))].sort((a, b) => b - a)
  }, [teams])

  const teamIdentityIndex = useMemo(() => createTeamIdentityIndexFromRows(teams), [teams])
  const queryState = readRouteQuery('teamProfile', searchParams, {
    defaults: { temporada: Number(routeParams.season) || availableSeasons[0] || 2025 },
  })
  const selectedSeason = parseSeasonParam(queryState.temporada, availableSeasons)
  const trendStat = trendStatOptions.some(stat => stat.key === queryState.tendencia)
    ? queryState.tendencia
    : 'ortg'
  const mode = queryState.lado
  const metric = queryState.metrica
  const reference = queryState.referencia
  const resolvedUrlTeamId = resolveTeamId(teamIdentityIndex, urlTeamId, selectedSeason)

  const seasonTeams = useMemo(() => {
    return teams.filter(t => t.season === selectedSeason).sort((a, b) => a.team.localeCompare(b.team))
  }, [teams, selectedSeason])

  const teamOptions = useMemo(() => {
    const options = new Map()
    seasonTeams.forEach(row => {
      const teamId = resolveTeamId(teamIdentityIndex, row.teamId || row.team, selectedSeason)
      if (teamId && !options.has(teamId)) options.set(teamId, { teamId, name: row.team })
    })
    return [...options.values()]
  }, [seasonTeams, selectedSeason, teamIdentityIndex])

  const selectedRecord = useMemo(() => {
    if (!resolvedUrlTeamId) return null
    return seasonTeams.find(row => (
      resolveTeamId(teamIdentityIndex, row.teamId || row.team, selectedSeason) === resolvedUrlTeamId
    )) || null
  }, [resolvedUrlTeamId, seasonTeams, selectedSeason, teamIdentityIndex])

  const selectedTeam = selectedRecord?.team || ''

  const rosterPlayers = useMemo(() => {
    if (!resolvedUrlTeamId) return []
    return players.filter(player => (
      Number(player.season) === Number(selectedSeason)
      && resolveTeamId(teamIdentityIndex, player.teamId || player.team, selectedSeason) === resolvedUrlTeamId
    ))
  }, [players, resolvedUrlTeamId, selectedSeason, teamIdentityIndex])

  const updateUrlState = (changes) => {
    setSearchParams(buildTeamProfileSearch({
      season: selectedSeason,
      trendStat,
      mode,
      metric,
      reference,
      ...changes,
    }))
  }

  useEffect(() => {
    if (!availableSeasons.length) return
    const canonical = buildTeamProfileSearch({ season: selectedSeason, trendStat, mode, metric, reference })
    if (canonical.toString() !== searchParams.toString()) {
      setSearchParams(canonical, { replace: true })
    }
  }, [availableSeasons.length, metric, mode, reference, searchParams, selectedSeason, setSearchParams, trendStat])

  useEffect(() => {
    if (!urlTeamId || !resolvedUrlTeamId) return
    if (urlTeamId === resolvedUrlTeamId && !routeParams.season) return
    const search = buildTeamProfileSearch({ season: selectedSeason, trendStat, mode, metric, reference }).toString()
    navigate({ pathname: buildTeamProfilePath(resolvedUrlTeamId), search: `?${search}` }, { replace: true })
  }, [metric, mode, reference, navigate, resolvedUrlTeamId, routeParams.season, selectedSeason, trendStat, urlTeamId])

  useEffect(() => {
    if (selectedSeason >= 2021) loadShotsForSeason?.(selectedSeason)
  }, [loadShotsForSeason, selectedSeason])

  const shotRows = shotsCache[selectedSeason] || []
  const hasLoadedShots = Object.prototype.hasOwnProperty.call(shotsCache, selectedSeason)
  const isShotsLoading = selectedSeason >= 2021 && (!hasLoadedShots || Boolean(loadingShots[selectedSeason]))

  // Current-season means and z-scores for the report and metric tables.
  const leagueStats = useMemo(() => {
    const allAxes = [...offensiveAxes, ...defensiveAxes]
    const stats = {}
    allAxes.forEach(axis => {
      const vals = seasonTeams.map(t => t[axis.key]).filter(v => v != null)
      const mean = vals.reduce((a, b) => a + b, 0) / vals.length
      const std = Math.sqrt(vals.reduce((a, v) => a + (v - mean) ** 2, 0) / vals.length)
      stats[axis.key] = { mean, std }
    })
    return stats
  }, [seasonTeams])

  // compute z-scores for the selected team
  const teamData = useMemo(() => {
    if (!selectedTeam) return null
    const team = seasonTeams.find(t => t.team === selectedTeam)
    if (!team) return null

    const computeZ = (axes) => {
      const zScores = {}
      axes.forEach(axis => {
        const { mean, std } = leagueStats[axis.key]
        const raw = (team[axis.key] - mean) / (std || 1)
        zScores[axis.key] = axis.inverted ? -raw : raw
      })
      return zScores
    }

    const offZScores = computeZ(offensiveAxes)
    const defZScores = computeZ(defensiveAxes)

    const referenceTeams = reference === 'season' ? seasonTeams : teams
    const offValues = getTeamRadarAxes(team, referenceTeams, offensiveAxes).map(axis => axis.value)
    const defValues = getTeamRadarAxes(team, referenceTeams, defensiveAxes).map(axis => axis.value)

    const strengths = []
    const weaknesses = []

    offensiveAxes.forEach(axis => {
      const z = offZScores[axis.key]
      const val = team[axis.key]
      const avg = leagueStats[axis.key].mean
      const item = {
        category: 'Ofensivo',
        axisKey: axis.key,
        label: axis.label,
        phrase: pickPhrase(offensivePhrases[axis.key], z),
        z,
        value: axis.format(val),
        avg: axis.format(avg),
      }
      if (z > 0.75) strengths.push(item)
      else if (z < -0.75) weaknesses.push(item)
    })

    defensiveAxes.forEach(axis => {
      const z = defZScores[axis.key]
      const val = team[axis.key]
      const avg = leagueStats[axis.key].mean
      const item = {
        category: 'Defensivo',
        axisKey: axis.key,
        label: axis.label,
        phrase: pickPhrase(defensivePhrases[axis.key], z),
        z,
        value: axis.format(val),
        avg: axis.format(avg),
      }
      if (z > 0.75) strengths.push(item)
      else if (z < -0.75) weaknesses.push(item)
    })

    strengths.sort((a, b) => b.z - a.z)
    weaknesses.sort((a, b) => a.z - b.z)

    const narrative = buildNarrative(selectedTeam, strengths, weaknesses)

    return { team, offZScores, defZScores, offValues, defValues, strengths, weaknesses, narrative }
  }, [selectedTeam, seasonTeams, leagueStats, teams, reference])

  useEffect(() => {
    const header = teamHeaderRef.current
    if (!header || !selectedTeam) {
      setShowFloatingTeamHeader(false)
      return undefined
    }

    const updateFloatingHeader = () => {
      const topOffset = window.innerWidth >= 1280 ? 80 : window.innerWidth >= 640 ? 64 : 48
      setShowFloatingTeamHeader(header.getBoundingClientRect().bottom <= topOffset)
    }

    updateFloatingHeader()
    window.addEventListener('scroll', updateFloatingHeader, { passive: true })
    window.addEventListener('resize', updateFloatingHeader)

    return () => {
      window.removeEventListener('scroll', updateFloatingHeader)
      window.removeEventListener('resize', updateFloatingHeader)
    }
  }, [selectedTeam])

  return (
    <TeamToolShell
      activeTool="profile"
      title="Perfil de equipo"
    >

      {/* selectors */}
      <div className={styles.controlBand}>
        <div className="flex flex-col gap-1">
          <label htmlFor="fingerprint-season" className="field-label">Temporada</label>
          <select
            id="fingerprint-season"
            value={selectedSeason}
            onChange={(e) => updateUrlState({ season: Number(e.target.value) })}
            className="form-control"
          >
            {availableSeasons.map(s => (
              <option key={s} value={s}>{seasonLabel(s)}</option>
            ))}
          </select>
        </div>

        <div className="flex min-w-[240px] flex-col gap-1">
          <label htmlFor="fingerprint-team" className="field-label">Equipo</label>
          <select
            id="fingerprint-team"
            value={selectedRecord ? resolvedUrlTeamId : ''}
            onChange={(e) => {
              const search = buildTeamProfileSearch({ season: selectedSeason, trendStat, mode, metric, reference }).toString()
              navigate({ pathname: buildTeamProfilePath(e.target.value), search: `?${search}` })
            }}
            className="form-control"
          >
            <option value="">Selecciona un equipo</option>
            {teamOptions.map(option => (
              <option key={option.teamId} value={option.teamId}>{option.name}</option>
            ))}
          </select>
        </div>
      </div>

      {/* content */}
      {!selectedTeam && (
        <section className={styles.dataSection} aria-labelledby="team-directory-title">
          <div className={styles.sectionHeader}>
            <div>
              <h2 id="team-directory-title">Elige un equipo</h2>
              <p>{urlTeamId
                ? 'Ese equipo no está disponible en esta temporada. Elige otro para abrir su perfil.'
                : 'Abre su informe de juego, evolución, tiro y plantilla.'}</p>
            </div>
            <span className={styles.panelMeta}>{teamOptions.length} equipos · {seasonLabel(selectedSeason)}</span>
          </div>
          <div className={styles.teamDirectory}>
            {teamOptions.map(option => (
              <Link
                key={option.teamId}
                className={styles.directoryTeam}
                to={{
                  pathname: buildTeamProfilePath(option.teamId),
                  search: `?${buildTeamProfileSearch({ season: selectedSeason, trendStat, mode, metric, reference }).toString()}`,
                }}
              >
                {teamLogos[option.name]
                  ? <img src={teamLogos[option.name]} alt="" loading="lazy" />
                  : <span className={styles.directoryInitial} aria-hidden="true">{option.name.slice(0, 1)}</span>}
                <span>{option.name}</span>
                <span className={styles.directoryAction}>Ver perfil</span>
              </Link>
            ))}
          </div>
          {teamOptions.length === 0 && <p className={styles.emptyState}>No hay equipos disponibles. Selecciona otra temporada.</p>}
        </section>
      )}

      {teamData && (
        <div className={profileStyles.profileFlow}>
          <section ref={teamHeaderRef} className={profileStyles.dossierHero} aria-labelledby="selected-team-title">
            <div className={profileStyles.identityPane}>
              <div className={profileStyles.identityTop}>
                {teamLogos[selectedTeam] && (
                  <img
                    src={teamLogos[selectedTeam]}
                    alt={selectedTeam}
                    className={profileStyles.identityLogo}
                  />
                )}
                <div>
                  <h2 id="selected-team-title" className={profileStyles.identityName}>{selectedTeam}</h2>
                  <p className={profileStyles.identitySeason}>{seasonLabel(selectedSeason)}</p>
                </div>
              </div>

              {teamData.team.wins != null && (
                <div className={profileStyles.recordBlock}>
                  <strong
                    className={profileStyles.recordValue}
                    aria-label={`${teamData.team.wins} victorias, ${teamData.team.losses} derrotas`}
                  >
                    {teamData.team.wins}–{teamData.team.losses}
                  </strong>
                </div>
              )}

              <div className={profileStyles.identityStats}>
                <div className={profileStyles.identityStat}>
                  <span className={profileStyles.identityStatLabel}>Rating neto</span>
                  <strong className={profileStyles.identityStatValue}>{formatSignedMetric(teamData.team.netRtg)}</strong>
                </div>
                <div className={profileStyles.identityStat}>
                  <span className={profileStyles.identityStatLabel}>Ataque</span>
                  <strong className={profileStyles.identityStatValue}>{teamData.team.ortg?.toFixed(1) ?? '-'}</strong>
                </div>
                <div className={profileStyles.identityStat}>
                  <span className={profileStyles.identityStatLabel}>Defensa</span>
                  <strong className={profileStyles.identityStatValue}>{teamData.team.drtg?.toFixed(1) ?? '-'}</strong>
                </div>
                <div className={profileStyles.identityStat}>
                  <span className={profileStyles.identityStatLabel}>Ritmo</span>
                  <strong className={profileStyles.identityStatValue}>{teamData.team.pace?.toFixed(1) ?? '-'}</strong>
                </div>
              </div>
            </div>

            <TeamTrendChart
              teams={teams}
              selectedTeam={selectedTeam}
              selectedSeason={selectedSeason}
              trendStat={trendStat}
              onTrendStatChange={(value) => updateUrlState({ trendStat: value })}
            />
          </section>

          <div
            aria-hidden={!showFloatingTeamHeader}
            className={`${profileStyles.floatingHeader} ${showFloatingTeamHeader ? profileStyles.floatingHeaderVisible : ''}`}
          >
            {teamLogos[selectedTeam] && (
              <img
                src={teamLogos[selectedTeam]}
                alt=""
                className={profileStyles.floatingLogo}
              />
            )}
            <div className={profileStyles.floatingCopy}>
              <strong>{selectedTeam}</strong>
              <span>
                {seasonLabel(selectedSeason)}
                {teamData.team.wins != null && (
                  <>
                    {' · '}PJ {teamData.team.games} · {teamData.team.wins}V-{teamData.team.losses}D
                  </>
                )}
              </span>
            </div>
          </div>

          <section className={profileStyles.scoutReport} aria-labelledby="traits-title">
            <div className={profileStyles.reportIntro}>
              <h2 id="traits-title" className={profileStyles.reportTitle}>Informe de juego</h2>
              <p className={profileStyles.narrative}>{teamData.narrative}</p>
            </div>

            <div className={profileStyles.traitLedger}>
              <TraitList
                title="Fortalezas"
                items={teamData.strengths}
                emptyMsg="Sin fortalezas destacadas"
              />
              <TraitList
                title="Debilidades"
                items={teamData.weaknesses}
                emptyMsg="Sin debilidades destacadas"
              />
            </div>
          </section>

          <section className={profileStyles.fingerprintSheet} aria-labelledby="radar-title">
            <header className={profileStyles.sectionHeader}>
              <div>
                <h2 id="radar-title" className={profileStyles.sectionTitle}>Huella estadística</h2>
                <p className={profileStyles.sectionSubtitle}>Percentiles · {reference === 'season' ? seasonLabel(selectedSeason) : `${seasonLabel(availableSeasons.at(-1))} a ${seasonLabel(availableSeasons[0])}`} · {reference === 'season' ? `${seasonTeams.length} equipos` : `${teams.length} equipos-temporada`}</p>
              </div>
              <div className="segmented-control" role="group" aria-label="Referencia de los percentiles de equipo">
                <button className="segmented-option" aria-pressed={reference === 'season'} onClick={() => updateUrlState({ reference: 'season' })}>Temporada</button>
                <button className="segmented-option" aria-pressed={reference === 'historical'} onClick={() => updateUrlState({ reference: 'historical' })}>Histórico</button>
              </div>
            </header>

            <div className={profileStyles.radarSpread}>
              <RadarChart
                axes={offensiveAxes}
                values={teamData.offValues}
                strokeColor="#d04313"
                title="Perfil Ofensivo"
              />
              <RadarChart
                axes={defensiveAxes}
                values={teamData.defValues}
                strokeColor="#334e68"
                title="Perfil Defensivo"
              />
            </div>

            <div className={profileStyles.metricHeading}>
              <h3>Detalle por métrica · liga {seasonLabel(selectedSeason)}</h3>
            </div>

            <div className={profileStyles.metricTables}>
              <MetricTable
                title="Ataque"
                axes={offensiveAxes}
                team={teamData.team}
                leagueStats={leagueStats}
                zScores={teamData.offZScores}
              />
              <MetricTable
                title="Defensa"
                axes={defensiveAxes}
                team={teamData.team}
                leagueStats={leagueStats}
                zScores={teamData.defZScores}
              />
            </div>
          </section>

          <TeamShotProfile
            team={selectedTeam}
            shots={shotRows}
            isLoading={isShotsLoading}
            isAvailable={selectedSeason >= 2021}
            mode={mode}
            metric={metric}
            onModeChange={(value) => updateUrlState({ mode: value })}
            onMetricChange={(value) => updateUrlState({ metric: value })}
          />

          <TeamRosterTable
            players={rosterPlayers}
            playerBio={playerBio}
            className={profileStyles.rosterSection}
          />
        </div>
      )}
    </TeamToolShell>
  )
}
