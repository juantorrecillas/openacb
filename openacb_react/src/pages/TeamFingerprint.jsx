import { useMemo, useEffect, useRef, useState } from 'react'
import { useParams, useNavigate, useSearchParams } from 'react-router-dom'
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer } from 'recharts'
import PageHeader from '../components/PageHeader'
import TeamRosterTable from '../components/TeamRosterTable'
import ZoneHeatmap from '../components/ZoneHeatmap'
import { buildTeamProfilePath } from '../routing/paths'
import { readRouteQuery, serializeRouteQuery } from '../routing/query'
import { createTeamIdentityIndexFromRows, resolveTeamId } from '../routing/teamIdentities'


// ─── Axis Definitions ─────────────────────────────────────────

const offensiveAxes = [
  { key: 'ortg',      label: 'Rating Ofensivo',      inverted: false, format: v => v.toFixed(1) },
  { key: 'pace',      label: 'Ritmo',                inverted: false, format: v => v.toFixed(1) },
  { key: 'threeRate', label: 'Dependencia 3P',       inverted: false, format: v => `${(v * 100).toFixed(1)}%` },
  { key: 'threePct',  label: 'Eficiencia 3P',        inverted: false, format: v => `${(v * 100).toFixed(1)}%` },
  { key: 'ts',        label: 'Eficiencia de Tiro',   inverted: false, format: v => `${(v * 100).toFixed(1)}%` },
  { key: 'astRate',   label: 'Ratio de Asistencias', inverted: false, format: v => `${(v * 100).toFixed(1)}%` },
  { key: 'orbPct',    label: 'Rebote Ofensivo',      inverted: false, format: v => `${(v * 100).toFixed(1)}%` },
  { key: 'tovRate',   label: 'Cuidado de Balón',     inverted: true,  format: v => `${(v * 100).toFixed(1)}%` },
]

const defensiveAxes = [
  { key: 'drtg',         label: 'Rating Defensivo',      inverted: true,  format: v => v.toFixed(1) },
  { key: 'opp_threePct', label: 'Riv. Eficiencia 3P',    inverted: true,  format: v => `${(v * 100).toFixed(1)}%` },
  { key: 'opp_ts',       label: 'Riv. Eficiencia Tiro',  inverted: true,  format: v => `${(v * 100).toFixed(1)}%` },
  { key: 'opp_tovRate',  label: 'Pérdidas Forzadas',     inverted: false, format: v => `${(v * 100).toFixed(1)}%` },
  { key: 'stlRate',      label: 'Ratio de Robos',        inverted: false, format: v => `${(v * 100).toFixed(1)}%` },
  { key: 'blkRate',      label: 'Ratio de Tapones',      inverted: false, format: v => `${(v * 100).toFixed(1)}%` },
  { key: 'drbPct',       label: 'Rebote Defensivo',      inverted: false, format: v => `${(v * 100).toFixed(1)}%` },
]

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
    return `${teamName} es un equipo sin rasgos especialmente marcados: se mueve en valores cercanos a la media de la liga en todas las métricas, sin fortalezas ni debilidades que destaquen sobre el resto.`
  }

  const parts = []

  if (strengths.length > 0) {
    const offStr = strengths.filter(s => s.category === 'Ofensivo')
    const defStr = strengths.filter(s => s.category === 'Defensivo')

    if (offStr.length > 0 && defStr.length > 0) {
      parts.push(`${teamName} destaca tanto en ataque como en defensa.`)
      parts.push(`En ataque, ${offStr.map(s => s.phrase).join(', y ')}.`)
      parts.push(`Defensivamente, ${defStr.map(s => s.phrase).join(', y ')}.`)
    } else if (offStr.length > 0) {
      parts.push(`${teamName} tiene un perfil claramente ofensivo: ${offStr.map(s => s.phrase).join(', y ')}.`)
    } else {
      parts.push(`${teamName} se apoya en su defensa como seña de identidad: ${defStr.map(s => s.phrase).join(', y ')}.`)
    }
  }

  if (weaknesses.length > 0) {
    const offWk = weaknesses.filter(s => s.category === 'Ofensivo')
    const defWk = weaknesses.filter(s => s.category === 'Defensivo')

    if (offWk.length > 0 && defWk.length > 0) {
      parts.push(`Sin embargo, tiene lagunas en ambas facetas: en ataque ${offWk.map(s => s.phrase).join(', y ')}; en defensa ${defWk.map(s => s.phrase).join(', y ')}.`)
    } else if (offWk.length > 0) {
      parts.push(`Por contra, en ataque ${offWk.map(s => s.phrase).join(', y ')}.`)
    } else {
      parts.push(`Como punto débil, defensivamente ${defWk.map(s => s.phrase).join(', y ')}.`)
    }
  }

  return parts.join(' ')
}

// ─── Radar Chart ──────────────────────────────────────────────

function RadarChart({ axes, values, fillColor, strokeColor, title }) {
  const size = 360
  const cx = size / 2
  const cy = size / 2
  const radius = 100
  const levels = [25, 50, 75, 100]
  const n = axes.length

  const angle = (i) => (Math.PI * 2 * i) / n - Math.PI / 2

  const point = (i, pct) => {
    const r = (pct / 100) * radius
    return [cx + r * Math.cos(angle(i)), cy + r * Math.sin(angle(i))]
  }

  const gridPath = (level) =>
    Array.from({ length: n }, (_, i) => point(i, level))
      .map((p, i) => `${i === 0 ? 'M' : 'L'}${p[0].toFixed(1)},${p[1].toFixed(1)}`)
      .join(' ') + ' Z'

  const dataPath = values
    .map((v, i) => point(i, Math.max(5, Math.min(v, 95))))
    .map((p, i) => `${i === 0 ? 'M' : 'L'}${p[0].toFixed(1)},${p[1].toFixed(1)}`)
    .join(' ') + ' Z'

  const labelAnchor = (i) => {
    const x = Math.cos(angle(i))
    if (x < -0.3) return 'end'
    if (x > 0.3) return 'start'
    return 'middle'
  }

  return (
    <div>
      <h3 className="text-center font-semibold text-acb-700 mb-2">{title}</h3>
      <svg viewBox={`0 0 ${size} ${size}`} className="w-full max-w-[320px] mx-auto" overflow="visible">
        {/* Grid levels */}
        {levels.map(level => (
          <path
            key={level}
            d={gridPath(level)}
            fill="none"
            stroke={level === 50 ? '#94a3b8' : '#e2e8f0'}
            strokeWidth={level === 50 ? 1.2 : 0.7}
            strokeDasharray={level === 50 ? '' : '2,2'}
          />
        ))}
        {/* Axis lines */}
        {axes.map((_, i) => {
          const [ex, ey] = point(i, 100)
          return <line key={i} x1={cx} y1={cy} x2={ex} y2={ey} stroke="#e2e8f0" strokeWidth={0.7} />
        })}
        {/* Data fill */}
        <path d={dataPath} fill={fillColor} stroke={strokeColor} strokeWidth={2} />
        {/* Data dots */}
        {values.map((v, i) => {
          const [px, py] = point(i, Math.max(5, Math.min(v, 95)))
          return <circle key={i} cx={px} cy={py} r={3.5} fill={strokeColor} stroke="white" strokeWidth={1.5} />
        })}
        {/* Labels */}
        {axes.map((axis, i) => {
          const [lx, ly] = point(i, 125)
          return (
            <text
              key={axis.key}
              x={lx}
              y={ly}
              textAnchor={labelAnchor(i)}
              dominantBaseline="central"
              className="fill-acb-600 text-[11px] font-medium"
            >
              {axis.label}
            </text>
          )
        })}
        {/* Percentile values near dots */}
        {values.map((v, i) => {
          const clamped = Math.max(5, Math.min(v, 95))
          const [px, py] = point(i, clamped + (clamped > 85 ? -12 : 12))
          return (
            <text
              key={`val-${i}`}
              x={px}
              y={py}
              textAnchor="middle"
              dominantBaseline="central"
              className="fill-acb-500 text-[9px] font-mono"
            >
              {Math.round(v)}
            </text>
          )
        })}
      </svg>
    </div>
  )
}

// ─── Z-Score Badge ────────────────────────────────────────────

function ZBadge({ z }) {
  const colorClass = z > 0.75
    ? 'bg-accent-200 text-accent-900'
    : z > 0
      ? 'bg-accent-100 text-accent-800'
      : z < -0.75
        ? 'bg-info-200 text-info-900'
        : z < 0
          ? 'bg-info-100 text-info-800'
          : 'bg-acb-100 text-acb-600'

  return (
    <span className={`inline-block px-2 py-0.5 rounded text-xs font-medium ${colorClass}`}>
      {z >= 0 ? '+' : ''}{z.toFixed(2)}
    </span>
  )
}

// ─── Trait Card ───────────────────────────────────────────────

function TraitCard({ title, items, emptyMsg }) {
  return (
    <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
      <div className="px-4 py-2 text-center">
        <h3 className="font-semibold text-sm text-acb-700">{title}</h3>
      </div>
      <div className="p-4">
        {items.length === 0 ? (
          <p className="text-acb-400 text-sm text-center py-4">{emptyMsg}</p>
        ) : (
          <div className="space-y-3">
            {items.map((item, i) => (
              <div key={i} className="flex flex-col gap-0.5">
                <div className="flex items-center gap-2 flex-wrap">
                  {item.category === 'Ofensivo'
                    ? <span className="inline-block px-1.5 py-0.5 rounded text-[10px] font-medium bg-accent-100 text-accent-700">Ofensivo</span>
                    : <span className="inline-block px-1.5 py-0.5 rounded text-[10px] font-medium bg-acb-100 text-acb-700">Defensivo</span>
                  }
                  <span className="text-sm font-medium text-acb-700">{item.label}</span>
                  <ZBadge z={item.z} />
                </div>
                <div className="flex items-center justify-between text-xs text-acb-500 pl-1">
                  <span className="italic">{item.phrase}</span>
                  <span className="font-mono shrink-0 ml-2">{item.value} <span className="text-acb-400">media: {item.avg}</span></span>
                </div>
              </div>
            ))}
          </div>
        )}
      </div>
    </div>
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
    <div className="bg-white rounded-lg border border-acb-200 p-4 h-full">
      <div className="flex flex-wrap items-start justify-between gap-3 mb-3">
        <div>
          <h3 className="font-semibold text-acb-700">Evolución</h3>
          <p className="text-xs text-acb-500">{selectedStat.label}</p>
        </div>
        <label className="sr-only" htmlFor="team-trend-stat">Métrica</label>
        <select
          id="team-trend-stat"
          value={trendStat}
          onChange={(e) => onTrendStatChange(e.target.value)}
          className="px-2.5 py-1.5 border border-acb-200 rounded text-xs bg-white text-acb-700"
        >
          {trendStatOptions.map(stat => (
            <option key={stat.key} value={stat.key}>{stat.label}</option>
          ))}
        </select>
      </div>

      {history.length < 2 ? (
        <div className="h-48 flex items-center justify-center rounded bg-acb-50 text-sm text-acb-400">
          Historial insuficiente
        </div>
      ) : (
        <div className="h-48">
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
    </div>
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
      className={`px-2.5 py-1 text-xs rounded transition-colors ${active ? 'bg-acb-800 text-white font-medium' : 'bg-acb-100 text-acb-600 hover:bg-acb-200'}`}
    >
      {label}
    </button>
  )

  if (!isAvailable) {
    return (
      <div className="bg-white rounded-lg border border-acb-200 p-5">
        <h3 className="font-semibold text-acb-900">Perfil de Tiro</h3>
        <p className="text-xs text-acb-500">Datos disponibles desde la temporada 2020-21</p>
      </div>
    )
  }

  return (
    <div className="bg-white rounded-lg border border-acb-200 p-5">
      <div className="flex flex-wrap items-start justify-between gap-3 mb-4">
        <div>
          <h3 className="font-semibold text-acb-900">Perfil de Tiro</h3>
          <p className="text-xs text-acb-500">Comparado con la media de la liga en cada zona</p>
        </div>
        <div className="flex flex-wrap gap-2">
          <div className="flex gap-1">
            {toggleBtn(mode === 'attack', () => onModeChange('attack'), 'Ataque')}
            {toggleBtn(mode === 'defense', () => onModeChange('defense'), 'Defensa')}
          </div>
          <div className="flex gap-1">
            {toggleBtn(metric === 'efficiency', () => onMetricChange('efficiency'), 'Eficiencia')}
            {toggleBtn(metric === 'frequency', () => onMetricChange('frequency'), 'Frecuencia')}
          </div>
        </div>
      </div>
      {isLoading ? (
        <div className="text-center py-10 text-acb-400" role="status">Cargando tiros...</div>
      ) : (
        <div className="mx-auto min-w-0 max-w-[430px]">
          <div className="overflow-x-auto">
            <ZoneHeatmap
              shots={teamShots}
              leagueShots={shots}
              metric={metric}
              higherIsBetter={mode !== 'defense'}
              width={430}
              height={405}
            />
          </div>
        </div>
      )}
    </div>
  )
}

// etiqueta de temporada
function seasonLabel(s) {
  return `${s - 1}-${String(s).slice(-2)}`
}

function parseSeasonParam(value, availableSeasons) {
  const season = Number(value)
  return availableSeasons.includes(season) ? season : (availableSeasons[0] || 2025)
}

function buildTeamProfileSearch({ season, trendStat, mode, metric }) {
  return new URLSearchParams(serializeRouteQuery('teamProfile', {
    temporada: season,
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
      ...changes,
    }))
  }

  useEffect(() => {
    if (!availableSeasons.length) return
    const canonical = buildTeamProfileSearch({ season: selectedSeason, trendStat, mode, metric })
    if (canonical.toString() !== searchParams.toString()) {
      setSearchParams(canonical, { replace: true })
    }
  }, [availableSeasons.length, metric, mode, searchParams, selectedSeason, setSearchParams, trendStat])

  useEffect(() => {
    if (!urlTeamId || !resolvedUrlTeamId) return
    if (urlTeamId === resolvedUrlTeamId && !routeParams.season) return
    const search = buildTeamProfileSearch({ season: selectedSeason, trendStat, mode, metric }).toString()
    navigate({ pathname: buildTeamProfilePath(resolvedUrlTeamId), search: `?${search}` }, { replace: true })
  }, [metric, mode, navigate, resolvedUrlTeamId, routeParams.season, selectedSeason, trendStat, urlTeamId])

  useEffect(() => {
    if (selectedSeason >= 2021) loadShotsForSeason?.(selectedSeason)
  }, [loadShotsForSeason, selectedSeason])

  const shotRows = shotsCache[selectedSeason] || []
  const hasLoadedShots = Object.prototype.hasOwnProperty.call(shotsCache, selectedSeason)
  const isShotsLoading = selectedSeason >= 2021 && (!hasLoadedShots || Boolean(loadingShots[selectedSeason]))

  // compute league stats for all 14 axes
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

    // map z-scores to a percentile scale and clamp to [5, 95]
    const toPercentile = (z) => Math.max(5, Math.min(95, 50 + z * 15))

    const offValues = offensiveAxes.map(a => toPercentile(offZScores[a.key]))
    const defValues = defensiveAxes.map(a => toPercentile(defZScores[a.key]))

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
  }, [selectedTeam, seasonTeams, leagueStats])

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
    <div className="app-page space-y-6">
      <PageHeader
        title="Estilo de equipo"
        subtitle="Perfil de juego de cada equipo: fortalezas, debilidades y características destacables"
        scope="Temporada completa · Liga regular y playoffs"
      />

      {/* selectors */}
      <div className="filter-panel flex flex-wrap gap-4">
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
              const search = buildTeamProfileSearch({ season: selectedSeason, trendStat, mode, metric }).toString()
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
        <div className="bg-white rounded-lg border border-acb-200 p-12 text-center text-acb-400">
          {urlTeamId
            ? 'No se ha encontrado ese equipo en la temporada seleccionada.'
            : 'Selecciona un equipo para ver su estilo de juego.'}
        </div>
      )}

      {teamData && (
        <div className="space-y-6">
          {/* team header */}
          <div ref={teamHeaderRef} className="grid lg:grid-cols-[minmax(0,0.85fr)_minmax(0,1.15fr)] gap-6">
            <div className="bg-white rounded-lg border border-acb-200 p-5 flex items-center gap-4">
            {teamLogos[selectedTeam] && (
              <img
                src={teamLogos[selectedTeam]}
                alt={selectedTeam}
                className="w-16 h-16 object-contain"
              />
            )}
            <div>
              <h3 className="text-xl font-bold text-acb-900">{selectedTeam}</h3>
              <p className="text-sm text-acb-500">{seasonLabel(selectedSeason)}</p>
              {teamData.team.wins != null && (
                <p className="text-sm text-acb-600 mt-0.5">
                  PJ {teamData.team.games} &nbsp;·&nbsp;
                  <span className="text-positive font-medium">{teamData.team.wins}V</span>
                  {' - '}
                  <span className="text-negative font-medium">{teamData.team.losses}D</span>
                </p>
              )}
            </div>
            </div>

            <TeamTrendChart
              teams={teams}
              selectedTeam={selectedTeam}
              selectedSeason={selectedSeason}
              trendStat={trendStat}
              onTrendStatChange={(value) => updateUrlState({ trendStat: value })}
            />
          </div>

          <div
            aria-hidden={!showFloatingTeamHeader}
            className={`pointer-events-none fixed left-1/2 top-8 z-40 flex w-[calc(100%-2rem)] max-w-xs -translate-x-1/2 items-center gap-2 rounded-md border border-acb-200 bg-white/95 px-3 py-1.5 shadow-md backdrop-blur-sm transition-[opacity,transform] duration-200 ease-out sm:top-10 xl:top-14 ${
              showFloatingTeamHeader ? 'translate-y-0 opacity-100' : '-translate-y-2 opacity-0'
            }`}
          >
            {teamLogos[selectedTeam] && (
              <img
                src={teamLogos[selectedTeam]}
                alt=""
                className="h-7 w-7 flex-shrink-0 object-contain"
              />
            )}
            <div className="min-w-0 flex-1">
              <h3 className="truncate text-sm font-bold leading-tight text-acb-900">{selectedTeam}</h3>
              <p className="truncate text-[10px] leading-tight text-acb-500">
                {seasonLabel(selectedSeason)}
                {teamData.team.wins != null && (
                  <>
                    {' · '}PJ {teamData.team.games} · {teamData.team.wins}V-{teamData.team.losses}D
                  </>
                )}
              </p>
            </div>
          </div>

          {/* Fortalezas / Debilidades */}
          <div className="grid md:grid-cols-2 gap-6">
            <TraitCard
              title="Fortalezas"
              items={teamData.strengths}
              emptyMsg="Sin fortalezas destacadas"
            />
            <TraitCard
              title="Debilidades"
              items={teamData.weaknesses}
              emptyMsg="Sin debilidades destacadas"
            />
          </div>

          {/* Radar Charts */}
          <div className="grid md:grid-cols-2 gap-6">
            <div className="bg-white rounded-lg border border-acb-200 p-4">
              <RadarChart
                axes={offensiveAxes}
                values={teamData.offValues}
                fillColor="rgba(249,115,22,0.18)"
                strokeColor="#f97316"
                title="Perfil Ofensivo"
              />
            </div>
            <div className="bg-white rounded-lg border border-acb-200 p-4">
              <RadarChart
                axes={defensiveAxes}
                values={teamData.defValues}
                fillColor="rgba(59,130,246,0.18)"
                strokeColor="#3b82f6"
                title="Perfil Defensivo"
              />
            </div>
          </div>

          <TeamRosterTable players={rosterPlayers} playerBio={playerBio} />

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

          {/* Stat Breakdown */}
          <div className="grid md:grid-cols-2 gap-6">
            {/* Offensive Stats */}
            <div className="bg-white rounded-lg border border-acb-200 p-4">
              <h3 className="font-semibold text-acb-700 text-center mb-3">Ofensivo</h3>
              <div className="space-y-2">
                {offensiveAxes.map(axis => {
                  const val = teamData.team[axis.key]
                  const avg = leagueStats[axis.key].mean
                  const z = teamData.offZScores[axis.key]
                  return (
                    <div key={axis.key} className="grid grid-cols-[minmax(0,1fr)_4.5rem_7rem_3.25rem] items-center gap-3 text-sm">
                      <span className="text-acb-600 truncate">{axis.label}</span>
                      <span className="font-mono text-acb-900 text-right">{axis.format(val)}</span>
                      <span className="font-mono text-acb-400 text-xs text-right">media: {axis.format(avg)}</span>
                      <span className="justify-self-end"><ZBadge z={z} /></span>
                    </div>
                  )
                })}
              </div>
            </div>

            {/* Defensive Stats */}
            <div className="bg-white rounded-lg border border-acb-200 p-4">
              <h3 className="font-semibold text-acb-700 text-center mb-3">Defensivo</h3>
              <div className="space-y-2">
                {defensiveAxes.map(axis => {
                  const val = teamData.team[axis.key]
                  const avg = leagueStats[axis.key].mean
                  const z = teamData.defZScores[axis.key]
                  return (
                    <div key={axis.key} className="grid grid-cols-[minmax(0,1fr)_4.5rem_7rem_3.25rem] items-center gap-3 text-sm">
                      <span className="text-acb-600 truncate">{axis.label}</span>
                      <span className="font-mono text-acb-900 text-right">{axis.format(val)}</span>
                      <span className="font-mono text-acb-400 text-xs text-right">media: {axis.format(avg)}</span>
                      <span className="justify-self-end"><ZBadge z={z} /></span>
                    </div>
                  )
                })}
              </div>
            </div>
          </div>
        </div>
      )}
    </div>
  )
}
