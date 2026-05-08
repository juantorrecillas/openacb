import { useState, useMemo, useEffect, useRef } from 'react'
import { Link, useNavigate, useParams } from 'react-router-dom'
import { Search, Filter, GitCompareArrows, Loader2, ArrowRight } from 'lucide-react'
import { getPlayerPhoto } from '../utils/playerPhotos'

function seasonLabel(s) {
  return `${s - 1}-${String(s).slice(-2)}`
}

function ageAtSeasonStart(birthDate, season) {
  if (!birthDate || !season) return null
  const birth = new Date(String(birthDate))
  const ref = new Date(season - 1, 9, 1)
  let age = ref.getFullYear() - birth.getFullYear()
  const m = ref.getMonth() - birth.getMonth()
  if (m < 0 || (m === 0 && ref.getDate() < birth.getDate())) age--
  return age
}

function fmt(v, key) {
  if (v == null || Number.isNaN(Number(v))) return '-'
  const n = Number(v)
  if (key === 'games') return String(n)
  if (['fgPct', 'fg2Pct', 'fg3Pct', 'ftPct', 'efg', 'ts', 'usg', 'threeRate',
       'orbPct', 'drbPct', 'trbPct', 'astPct', 'stlPct', 'blkPct', 'tovPct',
       'freqRim', 'freqShortMid', 'freqLongMid', 'freqAllMid', 'freqCornerThree',
       'freqNcThree', 'freqAllThree', 'fgpctRim', 'fgpctShortMid',
       'fgpctLongMid', 'fgpctAllMid', 'fgpctCornerThree', 'fgpctNcThree',
       'fgpctAllThree'].includes(key)) {
    return `${n.toFixed(1)}%`
  }
  if (['offTo', 'secondChance', 'assistedFgm', 'assistedFgm2', 'assistedFgm3'].includes(key)) {
    return `${(n * 100).toFixed(1)}%`
  }
  if (['points', 'rebounds', 'assists', 'steals', 'blocks', 'turnovers', 'fouls'].includes(key)) {
    return n.toFixed(0)
  }
  return n.toFixed(1)
}

function pctColor(pct, inverse = false) {
  if (pct == null || Number.isNaN(Number(pct))) return 'bg-acb-100 text-acb-500'
  const adjusted = inverse ? 100 - pct : pct
  if (adjusted >= 75) return 'bg-positive-100 text-positive-700'
  if (adjusted >= 50) return 'bg-info-100 text-info-700'
  if (adjusted >= 25) return 'bg-info-100 text-info-600'
  return 'bg-negative-100 text-negative-700'
}

function valueColor(a, b, metric, side) {
  if (a == null || b == null || Number.isNaN(Number(a)) || Number.isNaN(Number(b))) return 'text-acb-700'
  if (Number(a) === Number(b)) return 'text-acb-700'
  const aWins = metric.lowerIsBetter ? Number(a) < Number(b) : Number(a) > Number(b)
  const sideWins = side === 'a' ? aWins : !aWins
  return sideWins ? 'text-positive font-semibold' : 'text-acb-500'
}

function PlayerSelector({ players, label, selectedLicenseId, onSelect }) {
  const [query, setQuery] = useState('')
  const [open, setOpen] = useState(false)
  const [teamFilter, setTeamFilter] = useState('')
  const [seasonFilter, setSeasonFilter] = useState('')
  const ref = useRef(null)

  useEffect(() => {
    const handler = (e) => {
      if (ref.current && !ref.current.contains(e.target)) setOpen(false)
    }
    document.addEventListener('mousedown', handler)
    return () => document.removeEventListener('mousedown', handler)
  }, [])

  const availableSeasons = useMemo(() => {
    return [...new Set(players.map(p => p.season))].sort((a, b) => b - a)
  }, [players])

  const availableTeams = useMemo(() => {
    let filtered = players
    if (seasonFilter) filtered = filtered.filter(p => p.season === Number(seasonFilter))
    return [...new Set(filtered.map(p => p.team))].sort()
  }, [players, seasonFilter])

  const uniquePlayers = useMemo(() => {
    let filtered = players
    if (seasonFilter) filtered = filtered.filter(p => p.season === Number(seasonFilter))
    if (teamFilter) filtered = filtered.filter(p => p.team === teamFilter)

    const map = new Map()
    filtered.forEach(p => {
      const key = String(p.licenseId)
      if (!map.has(key)) {
        map.set(key, {
          licenseId: p.licenseId,
          name: p.playerFull?.trim() || p.player,
          abbrev: p.playerAbbrev,
          team: p.team,
          season: p.season,
        })
      }
    })
    return [...map.values()].sort((a, b) => (a.abbrev || a.name).localeCompare(b.abbrev || b.name))
  }, [players, seasonFilter, teamFilter])

  const filtered = useMemo(() => {
    if (!query.trim()) return uniquePlayers.slice(0, 50)
    const q = query.toLowerCase()
    return uniquePlayers.filter(p =>
      p.name?.toLowerCase().includes(q) || p.abbrev?.toLowerCase().includes(q)
    ).slice(0, 50)
  }, [uniquePlayers, query])

  return (
    <div className="bg-white rounded-lg border border-acb-200 p-4">
      <div className="flex items-center gap-2 mb-3">
        <GitCompareArrows className="w-4 h-4 text-accent-500" />
        <h3 className="font-semibold text-acb-900">{label}</h3>
      </div>
      <div className="grid sm:grid-cols-[140px_180px_1fr] gap-3 items-end">
        <div>
          <label className="text-xs text-acb-500 font-medium">Temporada</label>
          <select
            value={seasonFilter}
            onChange={(e) => { setSeasonFilter(e.target.value); setTeamFilter('') }}
            className="w-full mt-1 px-3 py-2.5 border border-acb-200 rounded-lg text-sm bg-white"
          >
            <option value="">Todas</option>
            {availableSeasons.map(s => (
              <option key={s} value={s}>{seasonLabel(s)}</option>
            ))}
          </select>
        </div>
        <div>
          <label className="text-xs text-acb-500 font-medium">Equipo</label>
          <div className="relative mt-1">
            <Filter className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-acb-400" />
            <select
              value={teamFilter}
              onChange={(e) => setTeamFilter(e.target.value)}
              className="w-full pl-9 pr-3 py-2.5 border border-acb-200 rounded-lg text-sm bg-white"
            >
              <option value="">Todos</option>
              {availableTeams.map(team => (
                <option key={team} value={team}>{team}</option>
              ))}
            </select>
          </div>
        </div>
        <div ref={ref} className="relative">
          <label className="text-xs text-acb-500 font-medium">Jugador</label>
          <div className="relative mt-1">
            <Search className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-acb-400" />
            <input
              type="text"
              value={query}
              onFocus={() => setOpen(true)}
              onChange={e => { setQuery(e.target.value); setOpen(true) }}
              placeholder="Buscar jugador..."
              className="w-full pl-10 pr-4 py-2.5 border border-acb-200 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-accent-300 focus:border-accent-400"
            />
          </div>
          {open && filtered.length > 0 && (
            <ul className="absolute z-50 mt-1 w-full bg-white border border-acb-200 rounded-lg shadow-lg max-h-64 overflow-y-auto">
              {filtered.map(p => (
                <li
                  key={p.licenseId}
                  onClick={() => {
                    onSelect(p.licenseId)
                    setQuery(p.abbrev || p.name)
                    setOpen(false)
                  }}
                  className={`px-4 py-2 text-sm cursor-pointer hover:bg-accent-50 flex items-center justify-between ${
                    String(selectedLicenseId) === String(p.licenseId) ? 'bg-accent-50 font-medium' : ''
                  }`}
                >
                  <span>{p.abbrev || p.name}</span>
                  <span className="text-xs text-acb-400 ml-2">{p.team}</span>
                </li>
              ))}
            </ul>
          )}
        </div>
      </div>
    </div>
  )
}

function PlayerSummary({ record, bio, photoUrl, colorClass, season, onSeasonChange, seasons }) {
  if (!record) return null
  const age = bio?.birthDate ? ageAtSeasonStart(bio.birthDate, season) : null
  return (
    <div className="bg-white rounded-lg border border-acb-200 p-5">
      <div className="flex gap-4 items-start">
        {photoUrl ? (
          <img
            src={photoUrl}
            alt={record.playerFull?.trim()}
            className={`w-20 h-20 rounded-full object-cover object-top border-2 ${colorClass}`}
          />
        ) : (
          <div className={`w-20 h-20 rounded-full border-2 ${colorClass} bg-acb-100`} />
        )}
        <div className="min-w-0 flex-1">
          <div className="flex items-start justify-between gap-3">
            <div>
              <h3 className="text-xl font-bold text-acb-900 truncate">{record.playerFull?.trim()}</h3>
              <p className="text-sm text-acb-500">{record.team} - {seasonLabel(record.season)}</p>
            </div>
            <select
              value={season || ''}
              onChange={(e) => onSeasonChange(Number(e.target.value))}
              className="px-3 py-1.5 border border-acb-200 rounded-md text-sm bg-white shrink-0"
            >
              {seasons.map(s => (
                <option key={s} value={s}>{seasonLabel(s)}</option>
              ))}
            </select>
          </div>
          <div className="mt-3 flex flex-wrap gap-x-4 gap-y-1 text-sm text-acb-600">
            {(record.position || bio?.position) && (
              <span className="font-medium text-accent-700">{record.position || bio.position}</span>
            )}
            {bio?.heightM && <span>{parseFloat(bio.heightM).toFixed(2).replace('.', ',')} m</span>}
            {age != null && <span>{age} años</span>}
            <span>{record.games} PJ</span>
            <span>{fmt(record.mpg, 'mpg')} MPP</span>
          </div>
          <div className="grid grid-cols-4 gap-2 mt-4">
            {[
              ['PPP', record.ppg],
              ['TS%', record.ts, 'ts'],
              ['USG%', record.usg, 'usg'],
              ['ORtg', record.ortg],
            ].map(([label, value, key]) => (
              <div key={label} className="bg-acb-50 rounded-md px-2 py-2 text-center">
                <div className="font-mono text-sm font-semibold text-acb-900">{fmt(value, key || label)}</div>
                <div className="text-[11px] text-acb-500">{label}</div>
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>
  )
}

const radarAxes = [
  { key: 'ppgPct', posKey: 'ppgPosPct', label: 'Anotación' },
  { key: 'tsPct', posKey: 'tsPosPct', label: 'Eficiencia' },
  { key: 'usgPct', posKey: 'usgPosPct', label: 'Volumen' },
  { key: 'threeRatePct', posKey: 'threeRatePosPct', label: 'Freq. 3P' },
  { key: 'astPctPct', posKey: 'astPctPosPct', label: 'Creación' },
  { key: 'trbPctPct', posKey: 'trbPctPosPct', label: 'Rebote' },
  { key: 'blkPctPct', posKey: 'blkPctPosPct', label: 'Def. Interior' },
  { key: 'stlPctPct', posKey: 'stlPctPosPct', label: 'Def. Perímetro' },
]

function getRadarValues(player, usePos) {
  return radarAxes.map(axis => {
    const k = usePos && axis.posKey ? axis.posKey : axis.key
    return player?.[k] ?? 50
  })
}

function RadarOverlay({ playerA, playerB }) {
  const [usePos, setUsePos] = useState(false)
  const canUsePos = playerA?.ppgPosPct != null && playerB?.ppgPosPct != null
  const size = 360
  const cx = size / 2
  const cy = size / 2
  const radius = 100
  const levels = [25, 50, 75, 100]
  const n = radarAxes.length
  const valuesA = getRadarValues(playerA, usePos)
  const valuesB = getRadarValues(playerB, usePos)

  const angle = (i) => (Math.PI * 2 * i) / n - Math.PI / 2
  const point = (i, pct) => {
    const r = (pct / 100) * radius
    return [cx + r * Math.cos(angle(i)), cy + r * Math.sin(angle(i))]
  }
  const pathFor = (values) =>
    values
      .map((v, i) => point(i, Math.min(v, 100)))
      .map((p, i) => `${i === 0 ? 'M' : 'L'}${p[0].toFixed(1)},${p[1].toFixed(1)}`)
      .join(' ') + ' Z'
  const gridPath = (level) =>
    Array.from({ length: n }, (_, i) => point(i, level))
      .map((p, i) => `${i === 0 ? 'M' : 'L'}${p[0].toFixed(1)},${p[1].toFixed(1)}`)
      .join(' ') + ' Z'
  const labelAnchor = (i) => {
    const x = Math.cos(angle(i))
    if (x < -0.3) return 'end'
    if (x > 0.3) return 'start'
    return 'middle'
  }

  return (
    <div className="bg-white rounded-lg border border-acb-200 p-5">
      <div className="flex items-start justify-between gap-3 mb-2">
        <div>
          <h3 className="font-semibold text-acb-900">Radar de Percentiles</h3>
          <p className="text-xs text-acb-500">Comparación visual del perfil estadístico</p>
        </div>
        {canUsePos && (
          <div className="flex rounded-md border border-acb-200 text-xs overflow-hidden shrink-0">
            <button
              onClick={() => setUsePos(false)}
              className={`px-3 py-1.5 font-medium transition-colors ${!usePos ? 'bg-acb-800 text-white' : 'bg-white text-acb-600 hover:bg-acb-50'}`}
            >
              Liga
            </button>
            <button
              onClick={() => setUsePos(true)}
              className={`px-3 py-1.5 font-medium transition-colors ${usePos ? 'bg-acb-800 text-white' : 'bg-white text-acb-600 hover:bg-acb-50'}`}
            >
              Posición
            </button>
          </div>
        )}
      </div>
      <svg viewBox={`0 0 ${size} ${size}`} className="w-full max-w-[360px] mx-auto" overflow="visible">
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
        {radarAxes.map((_, i) => {
          const [ex, ey] = point(i, 100)
          return <line key={i} x1={cx} y1={cy} x2={ex} y2={ey} stroke="#e2e8f0" strokeWidth={0.7} />
        })}
        <path d={pathFor(valuesA)} fill="rgba(240,132,94,0.18)" stroke="#f0845e" strokeWidth={2.2} />
        <path d={pathFor(valuesB)} fill="rgba(59,130,246,0.12)" stroke="#3b82f6" strokeWidth={2.2} />
        {valuesA.map((v, i) => {
          const [px, py] = point(i, Math.min(v, 100))
          return <circle key={`a-${i}`} cx={px} cy={py} r={3.5} fill="#f0845e" stroke="white" strokeWidth={1.5} />
        })}
        {valuesB.map((v, i) => {
          const [px, py] = point(i, Math.min(v, 100))
          return <circle key={`b-${i}`} cx={px} cy={py} r={3.5} fill="#3b82f6" stroke="white" strokeWidth={1.5} />
        })}
        {radarAxes.map((axis, i) => {
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
      </svg>
      <div className="flex flex-wrap justify-center gap-4 text-xs text-acb-600 mt-2">
        <span className="flex items-center gap-1.5">
          <span className="w-3 h-3 rounded-full bg-accent-500" />
          {playerA?.playerAbbrev || playerA?.playerFull?.trim()}
        </span>
        <span className="flex items-center gap-1.5">
          <span className="w-3 h-3 rounded-full bg-info-500" />
          {playerB?.playerAbbrev || playerB?.playerFull?.trim()}
        </span>
      </div>
    </div>
  )
}

const metricGroups = [
  {
    title: 'Producción',
    metrics: [
      { key: 'mpg', label: 'MPP' },
      { key: 'ppg', label: 'PPP' },
      { key: 'rpg', label: 'RPP' },
      { key: 'apg', label: 'APP' },
      { key: 'spg', label: 'RBP' },
      { key: 'bpg', label: 'TAPP' },
      { key: 'topg', label: 'PER', lowerIsBetter: true },
    ],
  },
  {
    title: 'Eficiencia y volumen',
    metrics: [
      { key: 'ortg', label: 'ORtg' },
      { key: 'usg', label: 'USG%' },
      { key: 'ts', label: 'TS%' },
      { key: 'efg', label: 'eFG%' },
      { key: 'fg2Pct', label: '2P%' },
      { key: 'fg3Pct', label: '3P%' },
      { key: 'ftPct', label: 'TL%' },
      { key: 'threeRate', label: '3PAr' },
    ],
  },
  {
    title: 'Creación y posesión',
    metrics: [
      { key: 'possPg', label: 'Pos/PJ' },
      { key: 'astPct', label: 'AST%' },
      { key: 'tovPct', label: 'TOV%', lowerIsBetter: true },
      { key: 'astToRatio', label: 'AST/PER' },
      { key: 'assistedFgm', label: '% asistidos' },
      { key: 'offTo', label: 'Pts Robo%' },
      { key: 'secondChance', label: '2a Op%' },
    ],
  },
  {
    title: 'Rebote y defensa',
    metrics: [
      { key: 'orbPct', label: 'RO%' },
      { key: 'drbPct', label: 'RD%' },
      { key: 'trbPct', label: 'REB%' },
      { key: 'stlPct', label: 'ROB%' },
      { key: 'blkPct', label: 'TAP%' },
      { key: 'oppDiffRim', label: 'Diff aro', lowerIsBetter: true },
      { key: 'oppDiffAllThree', label: 'Diff 3P', lowerIsBetter: true },
    ],
  },
  {
    title: 'Perfil de tiro',
    metrics: [
      { key: 'freqRim', label: 'Freq aro' },
      { key: 'freqAllMid', label: 'Freq media' },
      { key: 'freqCornerThree', label: 'Freq esq. 3' },
      { key: 'freqNcThree', label: 'Freq no esq. 3' },
      { key: 'fgpctRim', label: 'FG% aro' },
      { key: 'fgpctAllMid', label: 'FG% media' },
      { key: 'fgpctAllThree', label: 'FG% 3P' },
    ],
  },
]

function MetricComparison({ playerA, playerB }) {
  return (
    <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
      <div className="px-5 py-3 border-b border-acb-200">
        <h3 className="font-semibold text-acb-900">Comparación Estadística</h3>
      </div>
      <div className="overflow-x-auto">
        <table className="w-full text-sm">
          <thead>
            <tr className="bg-acb-50 border-b border-acb-200">
              <th className="px-4 py-2 text-left text-xs font-semibold text-acb-600 uppercase">Métrica</th>
              <th className="px-4 py-2 text-right text-xs font-semibold text-accent-600 uppercase">{playerA.playerAbbrev || 'Jugador A'}</th>
              <th className="px-4 py-2 text-center text-xs font-semibold text-acb-500 uppercase">Dif.</th>
              <th className="px-4 py-2 text-right text-xs font-semibold text-info-600 uppercase">{playerB.playerAbbrev || 'Jugador B'}</th>
            </tr>
          </thead>
          <tbody>
            {metricGroups.map(group => (
              <tr key={group.title}>
                <td colSpan={4} className="p-0">
                  <table className="w-full text-sm">
                    <tbody>
                      <tr className="bg-acb-50/70">
                        <td colSpan={4} className="px-4 py-2 text-xs font-semibold text-acb-500 uppercase tracking-wider border-t border-acb-100">
                          {group.title}
                        </td>
                      </tr>
                      {group.metrics.map(metric => {
                        const a = playerA[metric.key]
                        const b = playerB[metric.key]
                        const diff = a != null && b != null && !Number.isNaN(Number(a)) && !Number.isNaN(Number(b))
                          ? Number(a) - Number(b)
                          : null
                        return (
                          <tr key={metric.key} className="border-t border-acb-100 hover:bg-acb-50">
                            <td className="px-4 py-2 text-acb-700 w-1/4">{metric.label}</td>
                            <td className={`px-4 py-2 text-right font-mono w-1/4 ${valueColor(a, b, metric, 'a')}`}>
                              {fmt(a, metric.key)}
                            </td>
                            <td className={`px-4 py-2 text-center font-mono w-1/4 ${
                              diff == null || diff === 0
                                ? 'text-acb-400'
                                : (metric.lowerIsBetter ? diff < 0 : diff > 0)
                                  ? 'text-positive font-semibold'
                                  : 'text-info-600 font-medium'
                            }`}>
                              {diff != null ? `${diff > 0 ? '+' : ''}${fmt(diff, metric.key)}` : '-'}
                            </td>
                            <td className={`px-4 py-2 text-right font-mono w-1/4 ${valueColor(a, b, metric, 'b')}`}>
                              {fmt(b, metric.key)}
                            </td>
                          </tr>
                        )
                      })}
                    </tbody>
                  </table>
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  )
}

function resolveOnOff(record, lineupsCache, loadingLineups) {
  if (!record) return null
  const lineupData = lineupsCache[record.season]
  if (!lineupData?.data?.[record.team]?.players) {
    return { loading: loadingLineups[record.season] || false, found: false }
  }
  const playersObj = lineupData.data[record.team].players
  let playerData = null
  for (const [key, val] of Object.entries(playersObj)) {
    if (
      key.includes(String(record.licenseId)) ||
      String(val.licenseId) === String(record.licenseId) ||
      val.nickname === record.player ||
      val.name?.includes(record.player)
    ) {
      playerData = val
      break
    }
  }
  if (!playerData) return { loading: false, found: false }
  return { loading: false, found: true, ...playerData }
}

function onOffDiff(value, inverse = false) {
  if (value == null || Number.isNaN(Number(value))) return 'text-acb-400'
  if (inverse) return value < 0 ? 'text-positive font-semibold' : value > 0 ? 'text-negative font-semibold' : 'text-acb-600'
  return value > 0 ? 'text-positive font-semibold' : value < 0 ? 'text-negative font-semibold' : 'text-acb-600'
}

function OnOffComparison({ playerA, playerB, loadLineupsForSeason, lineupsCache, loadingLineups }) {
  useEffect(() => {
    ;[playerA, playerB].filter(Boolean).forEach(p => {
      if (!lineupsCache[p.season] && !loadingLineups[p.season]) loadLineupsForSeason(p.season)
    })
  }, [playerA, playerB, lineupsCache, loadingLineups, loadLineupsForSeason])

  const rowA = useMemo(() => resolveOnOff(playerA, lineupsCache, loadingLineups), [playerA, lineupsCache, loadingLineups])
  const rowB = useMemo(() => resolveOnOff(playerB, lineupsCache, loadingLineups), [playerB, lineupsCache, loadingLineups])
  const rows = [
    { id: 'a', record: playerA, row: rowA, color: 'text-accent-600' },
    { id: 'b', record: playerB, row: rowB, color: 'text-info-600' },
  ]
  const anyLoading = rows.some(r => r.row?.loading)
  const hasAny = rows.some(r => r.row?.found)

  return (
    <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
      <div className="px-5 py-3 border-b border-acb-200 flex items-center justify-between gap-3">
        <div>
          <h3 className="font-semibold text-acb-900">Impacto On/Off Court</h3>
          <p className="text-xs text-acb-500">Ratings del equipo con y sin el jugador en pista</p>
        </div>
        {anyLoading && (
          <div className="flex items-center gap-2 text-sm text-acb-500">
            <Loader2 className="w-4 h-4 animate-spin" />
            Cargando
          </div>
        )}
      </div>
      {!hasAny && !anyLoading && (
        <div className="px-5 py-6 text-sm text-acb-500">No se encontraron datos de alineaciones para estos jugadores.</div>
      )}
      {hasAny && (
        <div className="overflow-x-auto">
          <table className="w-full text-sm">
            <thead>
              <tr className="text-xs font-semibold text-acb-600 uppercase border-b border-acb-200 bg-acb-50">
                <th className="text-left px-4 py-2">Jugador</th>
                <th className="text-left px-4 py-2">Temp.</th>
                <th className="text-right px-4 py-2">Min</th>
                <th className="text-right px-4 py-2">ORtg On</th>
                <th className="text-right px-4 py-2">ORtg Off</th>
                <th className="text-right px-4 py-2">Delta O</th>
                <th className="text-right px-4 py-2">DRtg On</th>
                <th className="text-right px-4 py-2">DRtg Off</th>
                <th className="text-right px-4 py-2">Delta D</th>
                <th className="text-right px-4 py-2">Net On</th>
                <th className="text-right px-4 py-2">Impacto</th>
              </tr>
            </thead>
            <tbody className="divide-y divide-acb-100">
              {rows.filter(r => r.row?.found).map(({ id, record, row, color }) => {
                const ortgD = row.onORtg != null && row.offORtg != null ? row.onORtg - row.offORtg : null
                const drtgD = row.onDRtg != null && row.offDRtg != null ? row.onDRtg - row.offDRtg : null
                return (
                  <tr key={id} className="hover:bg-acb-50">
                    <td className={`px-4 py-2 font-semibold ${color}`}>{record.playerAbbrev || record.playerFull?.trim()}</td>
                    <td className="px-4 py-2 text-acb-600">{seasonLabel(record.season)}</td>
                    <td className="px-4 py-2 text-right font-mono text-acb-600">{row.onMin?.toFixed(0) ?? '-'}</td>
                    <td className="px-4 py-2 text-right font-mono text-acb-700">{row.onORtg?.toFixed(1) ?? '-'}</td>
                    <td className="px-4 py-2 text-right font-mono text-acb-500">{row.offORtg?.toFixed(1) ?? '-'}</td>
                    <td className={`px-4 py-2 text-right font-mono ${onOffDiff(ortgD)}`}>
                      {ortgD != null ? `${ortgD > 0 ? '+' : ''}${ortgD.toFixed(1)}` : '-'}
                    </td>
                    <td className="px-4 py-2 text-right font-mono text-acb-700">{row.onDRtg?.toFixed(1) ?? '-'}</td>
                    <td className="px-4 py-2 text-right font-mono text-acb-500">{row.offDRtg?.toFixed(1) ?? '-'}</td>
                    <td className={`px-4 py-2 text-right font-mono ${onOffDiff(drtgD, true)}`}>
                      {drtgD != null ? `${drtgD > 0 ? '+' : ''}${drtgD.toFixed(1)}` : '-'}
                    </td>
                    <td className="px-4 py-2 text-right font-mono text-acb-700">
                      {row.onNetRtg != null ? `${row.onNetRtg > 0 ? '+' : ''}${row.onNetRtg.toFixed(1)}` : '-'}
                    </td>
                    <td className={`px-4 py-2 text-right font-mono ${onOffDiff(row.netDiff)}`}>
                      {row.netDiff != null ? `${row.netDiff > 0 ? '+' : ''}${row.netDiff.toFixed(1)}` : '-'}
                    </td>
                  </tr>
                )
              })}
            </tbody>
          </table>
        </div>
      )}
    </div>
  )
}

function PercentileDeltas({ playerA, playerB }) {
  const rows = radarAxes.map(axis => {
    const a = playerA[axis.key]
    const b = playerB[axis.key]
    return { ...axis, a, b, diff: a != null && b != null ? a - b : null }
  })

  return (
    <div className="bg-white rounded-lg border border-acb-200 p-5">
      <h3 className="font-semibold text-acb-900 mb-3">Diferencias de Perfil</h3>
      <div className="space-y-2">
        {rows.map(r => (
          <div key={r.key} className="grid grid-cols-[92px_1fr_48px] gap-3 items-center text-sm">
            <span className="text-xs font-medium text-acb-600">{r.label}</span>
            <div className="h-4 rounded-full bg-acb-100 overflow-hidden relative">
              <div className="absolute left-1/2 top-0 bottom-0 w-px bg-acb-300" />
              {r.diff != null && (
                <div
                  className={`absolute top-0 bottom-0 ${r.diff >= 0 ? 'left-1/2 bg-accent-500' : 'right-1/2 bg-info-500'}`}
                  style={{ width: `${Math.min(Math.abs(r.diff), 50)}%` }}
                />
              )}
            </div>
            <span className={`font-mono text-xs text-right px-1.5 py-0.5 rounded ${pctColor(Math.abs(r.diff || 0) + 50)}`}>
              {r.diff != null ? `${r.diff > 0 ? '+' : ''}${r.diff.toFixed(0)}` : '-'}
            </span>
          </div>
        ))}
      </div>
    </div>
  )
}

export default function PlayerComparison({
  players,
  playerPhotos = {},
  playerBio = {},
  loadLineupsForSeason,
  lineupsCache,
  loadingLineups,
}) {
  const params = useParams()
  const navigate = useNavigate()

  const latestSeason = useMemo(() => Math.max(...players.map(p => p.season).filter(Boolean)), [players])
  const defaults = useMemo(() => {
    const pool = players
      .filter(p => p.season === latestSeason && p.games >= 5 && p.mpg >= 10)
      .sort((a, b) => (b.ppg || 0) - (a.ppg || 0))
    return [pool[0], pool[1]]
  }, [players, latestSeason])

  const [aId, setAId] = useState(params.aId || defaults[0]?.licenseId || null)
  const [bId, setBId] = useState(params.bId || defaults[1]?.licenseId || null)
  const [aSeason, setASeason] = useState(params.aSeason ? Number(params.aSeason) : defaults[0]?.season || latestSeason)
  const [bSeason, setBSeason] = useState(params.bSeason ? Number(params.bSeason) : defaults[1]?.season || latestSeason)

  const recordsA = useMemo(() => players.filter(p => String(p.licenseId) === String(aId)).sort((a, b) => b.season - a.season), [players, aId])
  const recordsB = useMemo(() => players.filter(p => String(p.licenseId) === String(bId)).sort((a, b) => b.season - a.season), [players, bId])
  const seasonsA = useMemo(() => recordsA.map(r => r.season), [recordsA])
  const seasonsB = useMemo(() => recordsB.map(r => r.season), [recordsB])

  useEffect(() => {
    if (recordsA.length > 0 && !seasonsA.includes(aSeason)) setASeason(recordsA[0].season)
  }, [recordsA, seasonsA, aSeason])

  useEffect(() => {
    if (recordsB.length > 0 && !seasonsB.includes(bSeason)) setBSeason(recordsB[0].season)
  }, [recordsB, seasonsB, bSeason])

  const playerA = useMemo(() => recordsA.find(r => r.season === aSeason) || recordsA[0] || null, [recordsA, aSeason])
  const playerB = useMemo(() => recordsB.find(r => r.season === bSeason) || recordsB[0] || null, [recordsB, bSeason])

  useEffect(() => {
    if (playerA && playerB) {
      navigate(`/comparar/${playerA.licenseId}/${playerA.season}/${playerB.licenseId}/${playerB.season}`, { replace: true })
    }
  }, [playerA, playerB, navigate])

  const bioA = playerA ? playerBio[String(playerA.licenseId)] : null
  const bioB = playerB ? playerBio[String(playerB.licenseId)] : null
  const photoA = playerA ? getPlayerPhoto(playerPhotos, playerA.licenseId, playerA.season) : null
  const photoB = playerB ? getPlayerPhoto(playerPhotos, playerB.licenseId, playerB.season) : null

  const handleSelectA = (licenseId) => {
    setAId(licenseId)
    const latest = players.filter(p => String(p.licenseId) === String(licenseId)).sort((a, b) => b.season - a.season)[0]
    if (latest) setASeason(latest.season)
  }

  const handleSelectB = (licenseId) => {
    setBId(licenseId)
    const latest = players.filter(p => String(p.licenseId) === String(licenseId)).sort((a, b) => b.season - a.season)[0]
    if (latest) setBSeason(latest.season)
  }

  return (
    <div className="space-y-6">
      <div className="flex flex-col sm:flex-row sm:items-start justify-between gap-3">
        <div>
          <h2 className="text-2xl font-semibold text-acb-900">Comparación de Jugadores</h2>
          <p className="text-acb-500 text-sm mt-1">
            Compara dos jugadores por producción, eficiencia, perfil de tiro, percentiles y on/off
          </p>
        </div>
        {playerA && playerB && (
          <div className="flex gap-2">
            <Link
              to={`/jugador/${playerA.licenseId}`}
              className="inline-flex items-center gap-1.5 px-3 py-2 text-xs font-medium rounded-md border border-acb-200 text-acb-600 hover:bg-acb-50"
            >
              Perfil A <ArrowRight className="w-3.5 h-3.5" />
            </Link>
            <Link
              to={`/jugador/${playerB.licenseId}`}
              className="inline-flex items-center gap-1.5 px-3 py-2 text-xs font-medium rounded-md border border-acb-200 text-acb-600 hover:bg-acb-50"
            >
              Perfil B <ArrowRight className="w-3.5 h-3.5" />
            </Link>
          </div>
        )}
      </div>

      <div className="grid lg:grid-cols-2 gap-4">
        <PlayerSelector players={players} label="Jugador A" selectedLicenseId={aId} onSelect={handleSelectA} />
        <PlayerSelector players={players} label="Jugador B" selectedLicenseId={bId} onSelect={handleSelectB} />
      </div>

      {playerA && playerB ? (
        <>
          <div className="grid lg:grid-cols-2 gap-4">
            <PlayerSummary
              record={playerA}
              bio={bioA}
              photoUrl={photoA}
              colorClass="border-accent-300"
              season={aSeason}
              seasons={seasonsA}
              onSeasonChange={setASeason}
            />
            <PlayerSummary
              record={playerB}
              bio={bioB}
              photoUrl={photoB}
              colorClass="border-info-300"
              season={bSeason}
              seasons={seasonsB}
              onSeasonChange={setBSeason}
            />
          </div>

          <div className="grid lg:grid-cols-[minmax(0,1fr)_360px] gap-4">
            <MetricComparison playerA={playerA} playerB={playerB} />
            <div className="space-y-4">
              <RadarOverlay playerA={playerA} playerB={playerB} />
              <PercentileDeltas playerA={playerA} playerB={playerB} />
            </div>
          </div>

          <OnOffComparison
            playerA={playerA}
            playerB={playerB}
            loadLineupsForSeason={loadLineupsForSeason}
            lineupsCache={lineupsCache}
            loadingLineups={loadingLineups}
          />
        </>
      ) : (
        <div className="text-center py-12 text-acb-400">
          Selecciona dos jugadores para iniciar la comparación
        </div>
      )}
    </div>
  )
}
