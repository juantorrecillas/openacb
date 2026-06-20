import { useState, useMemo, useEffect } from 'react'
import { useParams, useNavigate } from 'react-router-dom'


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
  if (z > 0.75) return <span className="inline-block px-2 py-0.5 rounded text-xs font-medium bg-positive-100 text-positive-700">+{z.toFixed(2)}</span>
  if (z < -0.75) return <span className="inline-block px-2 py-0.5 rounded text-xs font-medium bg-negative-100 text-negative-700">{z.toFixed(2)}</span>
  return <span className="inline-block px-2 py-0.5 rounded text-xs font-medium bg-acb-100 text-acb-600">{z >= 0 ? '+' : ''}{z.toFixed(2)}</span>
}

// ─── Trait Card ───────────────────────────────────────────────

function TraitCard({ title, items, headerBg, headerText, borderColor, emptyMsg }) {
  return (
    <div className={`bg-white rounded-lg border border-acb-200 border-l-4 ${borderColor} overflow-hidden`}>
      <div className={`${headerBg} px-4 py-2`}>
        <h3 className={`font-semibold text-sm ${headerText}`}>{title}</h3>
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

// ─── Season Label Helper ──────────────────────────────────────

function seasonLabel(s) {
  return `${s - 1}-${String(s).slice(-2)}`
}

// ─── Main Component ───────────────────────────────────────────

// Convert team name to URL-friendly slug: lowercase, spaces→hyphens, strip accents
function toSlug(name) {
  return name
    .normalize('NFD').replace(/[\u0300-\u036f]/g, '') // strip accents
    .toLowerCase()
    .replace(/\s+/g, '-')
    .replace(/[^a-z0-9-]/g, '')
}

export default function TeamFingerprint({ teams, teamLogos = {} }) {
  const { season: urlSeason, team: urlTeamSlug } = useParams()
  const navigate = useNavigate()

  const availableSeasons = useMemo(() => {
    return [...new Set(teams.map(t => t.season))].sort((a, b) => b - a)
  }, [teams])

  const [selectedSeason, setSelectedSeason] = useState(availableSeasons[0] || 2025)
  const [selectedTeam, setSelectedTeam] = useState('')

  // Sync from URL params on mount / URL change
  useEffect(() => {
    if (urlSeason) setSelectedSeason(Number(urlSeason))
  }, [urlSeason])

  // Resolve team slug to actual team name once we know the season's teams
  const seasonTeamsForSlug = useMemo(() => {
    const s = urlSeason ? Number(urlSeason) : selectedSeason
    return teams.filter(t => t.season === s)
  }, [teams, urlSeason, selectedSeason])

  useEffect(() => {
    if (urlTeamSlug && seasonTeamsForSlug.length > 0) {
      const match = seasonTeamsForSlug.find(t => toSlug(t.team) === urlTeamSlug)
      if (match) setSelectedTeam(match.team)
    }
  }, [urlTeamSlug, seasonTeamsForSlug])

  const seasonTeams = useMemo(() => {
    return teams.filter(t => t.season === selectedSeason).sort((a, b) => a.team.localeCompare(b.team))
  }, [teams, selectedSeason])

  const teamNames = useMemo(() => seasonTeams.map(t => t.team), [seasonTeams])

  // Compute league stats (mean & std) for all 14 axes
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

  // Compute z-scores for the selected team
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

    // Map z-scores to percentile scale for radar: 50 + z*15, clamped [5, 95]
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

  return (
    <div className="app-page space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-semibold text-acb-900">Estilo de Equipo</h2>
        <p className="text-acb-500 mt-1">Perfil de juego de cada equipo: fortalezas, debilidades y características destacables.</p>
      </div>

      {/* Selectors */}
      <div className="flex flex-wrap gap-3">
        <select
          value={selectedSeason}
          onChange={(e) => {
            const newSeason = Number(e.target.value)
            setSelectedSeason(newSeason)
            setSelectedTeam('')
            navigate('/perfil-equipo', { replace: true })
          }}
          className="px-3 py-2 border border-acb-300 rounded-lg text-sm bg-white"
        >
          {availableSeasons.map(s => (
            <option key={s} value={s}>{seasonLabel(s)}</option>
          ))}
        </select>

        <select
          value={selectedTeam}
          onChange={(e) => {
            const team = e.target.value
            setSelectedTeam(team)
            if (team) {
              navigate(`/perfil-equipo/${selectedSeason}/${toSlug(team)}`, { replace: true })
            } else {
              navigate('/perfil-equipo', { replace: true })
            }
          }}
          className="px-3 py-2 border border-acb-300 rounded-lg text-sm bg-white"
        >
          <option value="">Seleccionar equipo...</option>
          {teamNames.map(name => (
            <option key={name} value={name}>{name}</option>
          ))}
        </select>
      </div>

      {/* Content */}
      {!selectedTeam && (
        <div className="bg-white rounded-lg border border-acb-200 p-12 text-center text-acb-400">
          Selecciona un equipo para ver su estilo de juego.
        </div>
      )}

      {teamData && (
        <div className="space-y-6">
          {/* Team Header */}
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

          {/* Fortalezas / Debilidades */}
          <div className="grid md:grid-cols-2 gap-6">
            <TraitCard
              title="Fortalezas"
              items={teamData.strengths}
              headerBg="bg-positive-50"
              headerText="text-positive-700"
              borderColor="border-l-positive-500"
              emptyMsg="Sin fortalezas destacadas"
            />
            <TraitCard
              title="Debilidades"
              items={teamData.weaknesses}
              headerBg="bg-negative-50"
              headerText="text-negative-700"
              borderColor="border-l-negative-500"
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

          {/* Stat Breakdown */}
          <div className="grid md:grid-cols-2 gap-6">
            {/* Offensive Stats */}
            <div className="bg-white rounded-lg border border-acb-200 p-4">
              <h3 className="font-semibold text-acb-700 mb-3">Ofensivo</h3>
              <div className="space-y-2">
                {offensiveAxes.map(axis => {
                  const val = teamData.team[axis.key]
                  const avg = leagueStats[axis.key].mean
                  const z = teamData.offZScores[axis.key]
                  return (
                    <div key={axis.key} className="flex items-center justify-between text-sm">
                      <span className="text-acb-600 w-40 shrink-0">{axis.label}</span>
                      <span className="font-mono text-acb-900">{axis.format(val)}</span>
                      <span className="font-mono text-acb-400 text-xs">media: {axis.format(avg)}</span>
                      <ZBadge z={z} />
                    </div>
                  )
                })}
              </div>
            </div>

            {/* Defensive Stats */}
            <div className="bg-white rounded-lg border border-acb-200 p-4">
              <h3 className="font-semibold text-acb-700 mb-3">Defensivo</h3>
              <div className="space-y-2">
                {defensiveAxes.map(axis => {
                  const val = teamData.team[axis.key]
                  const avg = leagueStats[axis.key].mean
                  const z = teamData.defZScores[axis.key]
                  return (
                    <div key={axis.key} className="flex items-center justify-between text-sm">
                      <span className="text-acb-600 w-40 shrink-0">{axis.label}</span>
                      <span className="font-mono text-acb-900">{axis.format(val)}</span>
                      <span className="font-mono text-acb-400 text-xs">media: {axis.format(avg)}</span>
                      <ZBadge z={z} />
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
