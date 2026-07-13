import { useState, useMemo, useEffect, useRef } from 'react'
import { Link, useLocation, useNavigate, useParams } from 'react-router-dom'
import { Loader2, ArrowRight } from 'lucide-react'
import { getPlayerPhoto } from '../utils/playerPhotos'
import { statTitle } from '../utils/statLabels'
import { getPlayerDisplayName, getPlayerSearchText } from '../utils/playerNames'
import PageHeader from '../components/PageHeader'
import PlayerCombobox from '../components/PlayerCombobox'

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

const PERCENT_POINT_KEYS = new Set([
  'fgPct', 'fg2Pct', 'fg3Pct', 'ftPct', 'efg', 'ts', 'usg', 'threeRate',
  'orbPct', 'drbPct', 'trbPct', 'astPct', 'stlPct', 'blkPct', 'tovPct',
  'freqRim', 'freqShortMid', 'freqLongMid', 'freqAllMid', 'freqCornerThree',
  'freqNcThree', 'freqAllThree', 'fgpctRim', 'fgpctShortMid', 'fgpctLongMid',
  'fgpctAllMid', 'fgpctCornerThree', 'fgpctNcThree', 'fgpctAllThree',
  'oppDiffRim', 'oppDiffAllThree',
])

const FRACTION_POINT_KEYS = new Set(['offTo', 'secondChance', 'assistedFgm', 'assistedFgm2', 'assistedFgm3'])

function fmtComparisonDiff(value, key) {
  const n = Number(value)
  if (!Number.isFinite(n)) return '—'
  const sign = n > 0 ? '+' : ''
  if (PERCENT_POINT_KEYS.has(key)) return `${sign}${n.toFixed(1)} pp`
  if (FRACTION_POINT_KEYS.has(key)) return `${sign}${(n * 100).toFixed(1)} pp`
  return `${sign}${fmt(n, key)}`
}

function recordKey(record) {
  if (!record) return ''
  return `${record.licenseId}::${record.season}::${record.team}`
}

function compareRecordIdentity(a, b) {
  const nameOrder = getPlayerDisplayName(a).localeCompare(getPlayerDisplayName(b), 'es')
  if (nameOrder !== 0) return nameOrder
  if (a.season !== b.season) return b.season - a.season
  const teamOrder = String(a.team).localeCompare(String(b.team), 'es')
  if (teamOrder !== 0) return teamOrder
  return String(a.licenseId).localeCompare(String(b.licenseId), 'es')
}

function valueColor(a, b, metric, side) {
  if (a == null || b == null || Number.isNaN(Number(a)) || Number.isNaN(Number(b))) return 'text-acb-700'
  if (Number(a) === Number(b)) return 'text-acb-700'
  const aWins = metric.lowerIsBetter ? Number(a) < Number(b) : Number(a) > Number(b)
  const sideWins = side === 'a' ? aWins : !aWins
  return sideWins ? 'text-acb-900 font-semibold' : 'text-acb-500'
}

function PlayerSummary({ record, bio, photoUrl, colorClass }) {
  if (!record) return null
  const age = bio?.birthDate ? ageAtSeasonStart(bio.birthDate, record.season) : null
  return (
    <div className="bg-white rounded-lg border border-acb-200 p-5">
      <div className="flex flex-col sm:flex-row gap-4 items-center sm:items-start">
        {photoUrl ? (
          <img
            src={photoUrl}
            alt={getPlayerDisplayName(record)}
            className={`w-20 h-20 rounded-full object-cover object-top border-2 ${colorClass}`}
          />
        ) : (
          <div className={`w-20 h-20 rounded-full border-2 ${colorClass} bg-acb-100`} />
        )}
          <div className="min-w-0 flex-1 w-full">
            <div className="flex flex-col sm:flex-row sm:items-start sm:justify-between gap-3">
            <div className="min-w-0">
              <h3 className="text-xl font-bold text-acb-900 sm:truncate">{getPlayerDisplayName(record)}</h3>
              <p className="text-sm text-acb-500">{record.team} - {seasonLabel(record.season)}</p>
            </div>
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
          <div className="grid grid-cols-2 sm:grid-cols-4 gap-2 mt-4">
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
          <h3 className="font-semibold text-acb-900">Radar de percentiles</h3>
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
          {getPlayerDisplayName(playerA)}
        </span>
        <span className="flex items-center gap-1.5">
          <span className="w-3 h-3 rounded-full bg-info-500" />
          {getPlayerDisplayName(playerB)}
        </span>
      </div>
    </div>
  )
}

const metricGroups = [
  {
    title: 'Producción',
    metrics: [
      { key: 'mpg', label: 'MPP', scale: 15 },
      { key: 'ppg', label: 'PPP', scale: 10 },
      { key: 'rpg', label: 'RPP', scale: 5 },
      { key: 'apg', label: 'APP', scale: 5 },
      { key: 'spg', label: 'RBP', scale: 2 },
      { key: 'bpg', label: 'TAPP', scale: 1.5 },
      { key: 'topg', label: 'PER', lowerIsBetter: true, scale: 3 },
    ],
  },
  {
    title: 'Eficiencia y volumen',
    metrics: [
      { key: 'ortg', label: 'ORtg', scale: 20 },
      { key: 'usg', label: 'USG%', scale: 15 },
      { key: 'ts', label: 'TS%', scale: 10 },
      { key: 'efg', label: 'eFG%', scale: 10 },
      { key: 'fg2Pct', label: '2P%', scale: 15 },
      { key: 'fg3Pct', label: '3P%', scale: 15 },
      { key: 'ftPct', label: 'TL%', scale: 15 },
      { key: 'threeRate', label: 'Vol. 3P', scale: 20 },
    ],
  },
  {
    title: 'Creación y posesión',
    metrics: [
      { key: 'possPg', label: 'Pos/PJ', scale: 30 },
      { key: 'astPct', label: 'AST%', scale: 25 },
      { key: 'tovPct', label: 'PER%', lowerIsBetter: true, scale: 10 },
      { key: 'astToRatio', label: 'AST/PER', scale: 3 },
      { key: 'assistedFgm', label: '% asistidos', scale: 0.3 },
      { key: 'offTo', label: 'Pts Robo%', scale: 0.2 },
      { key: 'secondChance', label: '2a Op%', scale: 0.15 },
    ],
  },
  {
    title: 'Rebote y defensa',
    metrics: [
      { key: 'orbPct', label: 'RO%', scale: 10 },
      { key: 'drbPct', label: 'RD%', scale: 15 },
      { key: 'trbPct', label: 'REB%', scale: 20 },
      { key: 'stlPct', label: 'ROB%', scale: 3 },
      { key: 'blkPct', label: 'TAP%', scale: 5 },
      { key: 'oppDiffRim', label: 'Diff aro vs equipo', lowerIsBetter: true, scale: 15 },
      { key: 'oppDiffAllThree', label: 'Diff 3P vs equipo', lowerIsBetter: true, scale: 10 },
    ],
  },
  {
    title: 'Perfil de tiro',
    metrics: [
      { key: 'freqRim', label: 'Freq. aro', scale: 20 },
      { key: 'freqAllMid', label: 'Freq. media', scale: 20 },
      { key: 'freqCornerThree', label: 'Freq. esq. 3', scale: 15 },
      { key: 'freqNcThree', label: 'Freq. no esq. 3', scale: 20 },
      { key: 'fgpctRim', label: 'TC% aro', scale: 15 },
      { key: 'fgpctAllMid', label: 'TC% media', scale: 15 },
      { key: 'fgpctAllThree', label: 'TC% 3P', scale: 15 },
    ],
  },
]

function MetricComparison({ playerA, playerB }) {
  return (
    <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
      <div className="px-5 py-3 border-b border-acb-200">
        <h3 className="font-semibold text-acb-900">Comparación estadística</h3>
        <p className="text-xs text-acb-500 mt-0.5">La barra central indica dirección y magnitud de la ventaja</p>
      </div>
      <div className="overflow-x-auto">
        <table className="data-table">
          <thead>
            <tr className="bg-acb-50 border-b border-acb-200">
              <th className="data-table-head text-left w-[38%]">Métrica</th>
              <th className="data-table-head text-right text-accent-600 w-[22%]">
                <span className="flex items-center justify-end gap-1">
                  <span className="w-2 h-2 rounded-full bg-accent-500 inline-block" />
                  {getPlayerDisplayName(playerA, 'Jugador A')}
                </span>
              </th>
              <th className="data-table-head text-center text-acb-500 w-[18%]" title="Jugador A menos jugador B">Diff A−B</th>
              <th className="data-table-head text-right text-info-600 w-[22%]">
                <span className="flex items-center justify-end gap-1">
                  <span className="w-2 h-2 rounded-full bg-info-500 inline-block" />
                  {getPlayerDisplayName(playerB, 'Jugador B')}
                </span>
              </th>
            </tr>
          </thead>
          {metricGroups.map(group => (
            <tbody key={group.title}>
              <tr className="bg-acb-50/70">
                <th colSpan={4} className="data-table-group text-left border-t border-acb-100">
                  {group.title}
                </th>
              </tr>
              {group.metrics.map(metric => {
                const a = playerA[metric.key]
                const b = playerB[metric.key]
                const diff = a != null && b != null && !Number.isNaN(Number(a)) && !Number.isNaN(Number(b))
                  ? Number(a) - Number(b)
                  : null
                const aWins = diff != null && (metric.lowerIsBetter ? diff < 0 : diff > 0)
                const barPct = diff != null && metric.scale ? Math.min(100, (Math.abs(diff) / metric.scale) * 100) : 0
                return (
                  <tr key={metric.key} className="data-table-row border-t border-acb-100">
                    <td className="data-table-cell w-[38%]" title={statTitle(metric.label)}>{metric.label}</td>
                    <td className={`data-table-cell data-table-number w-[22%] ${valueColor(a, b, metric, 'a')}`}>
                      {fmt(a, metric.key)}
                    </td>
                    <td className="data-table-cell text-center w-[18%]">
                      <div className="flex flex-col items-center gap-0.5">
                        <span className="font-mono text-[10px] text-acb-500">
                          {diff != null ? fmtComparisonDiff(diff, metric.key) : '—'}
                        </span>
                        {diff != null && (
                          <div className="flex w-full h-[3px]">
                            <div className="flex-1 flex justify-end overflow-hidden">
                              {aWins && <div className="h-full bg-accent-400 rounded-l-full" style={{ width: `${barPct}%` }} />}
                            </div>
                            <div className="w-px bg-acb-200 shrink-0" />
                            <div className="flex-1 overflow-hidden">
                              {!aWins && diff !== 0 && <div className="h-full bg-info-400 rounded-r-full" style={{ width: `${barPct}%` }} />}
                            </div>
                          </div>
                        )}
                      </div>
                    </td>
                    <td className={`data-table-cell data-table-number w-[22%] ${valueColor(a, b, metric, 'b')}`}>
                      {fmt(b, metric.key)}
                    </td>
                  </tr>
                )
              })}
            </tbody>
          ))}
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
          <h3 className="font-semibold text-acb-900">Impacto en pista</h3>
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
          <table className="data-table">
            <thead>
              <tr className="text-xs font-semibold text-acb-600 uppercase border-b border-acb-200 bg-acb-50">
                <th className="data-table-head data-table-identity data-table-sticky data-table-sticky-head data-col-player bg-acb-50">Jugador</th>
                <th className="data-table-head text-left">Temp.</th>
                <th className="data-table-head data-table-number" title={statTitle('Min')}>Min</th>
                <th className="data-table-head data-table-number">ORtg On</th>
                <th className="data-table-head data-table-number">ORtg Off</th>
                <th className="data-table-head data-table-number">Diff ORtg (On−Off)</th>
                <th className="data-table-head data-table-number">DRtg On</th>
                <th className="data-table-head data-table-number">DRtg Off</th>
                <th className="data-table-head data-table-number">Diff DRtg (On−Off)</th>
                <th className="data-table-head data-table-number">Neto</th>
                <th className="data-table-head data-table-number">Diff Neto (On−Off)</th>
              </tr>
            </thead>
            <tbody className="divide-y divide-acb-100">
              {rows.filter(r => r.row?.found).map(({ id, record, row, color }) => {
                const ortgD = row.onORtg != null && row.offORtg != null ? row.onORtg - row.offORtg : null
                const drtgD = row.onDRtg != null && row.offDRtg != null ? row.onDRtg - row.offDRtg : null
                return (
                  <tr key={id} className="data-table-row">
                    <td className={`data-table-cell data-table-identity data-table-sticky data-col-player ${color}`}>{getPlayerDisplayName(record)}</td>
                    <td className="data-table-cell text-acb-600">{seasonLabel(record.season)}</td>
                    <td className="data-table-cell data-table-number text-acb-600">{row.onMin?.toFixed(0) ?? '-'}</td>
                    <td className="data-table-cell data-table-number">{row.onORtg?.toFixed(1) ?? '-'}</td>
                    <td className="data-table-cell data-table-number text-acb-500">{row.offORtg?.toFixed(1) ?? '-'}</td>
                    <td className={`data-table-cell data-table-number ${onOffDiff(ortgD)}`}>
                      {ortgD != null ? `${ortgD > 0 ? '+' : ''}${ortgD.toFixed(1)}` : '-'}
                    </td>
                    <td className="data-table-cell data-table-number">{row.onDRtg?.toFixed(1) ?? '-'}</td>
                    <td className="data-table-cell data-table-number text-acb-500">{row.offDRtg?.toFixed(1) ?? '-'}</td>
                    <td className={`data-table-cell data-table-number ${onOffDiff(drtgD, true)}`}>
                      {drtgD != null ? `${drtgD > 0 ? '+' : ''}${drtgD.toFixed(1)}` : '-'}
                    </td>
                    <td className="data-table-cell data-table-number">
                      {row.onNetRtg != null ? `${row.onNetRtg > 0 ? '+' : ''}${row.onNetRtg.toFixed(1)}` : '-'}
                    </td>
                    <td className={`data-table-cell data-table-number ${onOffDiff(row.netDiff)}`}>
                      {row.netDiff != null ? `${row.netDiff > 0 ? '+' : ''}${row.netDiff.toFixed(1)}` : '—'}
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
      <h3 className="font-semibold text-acb-900">Diferencias de perfil</h3>
      <p className="text-xs text-acb-500 mb-3">Diff A−B en puntos percentiles</p>
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
            <span className={`font-mono text-xs text-right px-1.5 py-0.5 rounded ${
              r.diff == null ? 'text-acb-400'
                : r.diff > 0 ? 'bg-accent-100 text-accent-700'
                : r.diff < 0 ? 'bg-info-100 text-info-700'
                : 'bg-acb-100 text-acb-500'
            }`}>
              {r.diff != null ? `${r.diff > 0 ? '+' : ''}${r.diff.toFixed(0)} pts pct.` : '—'}
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
  const location = useLocation()
  const navigate = useNavigate()

  const records = useMemo(() => {
    const unique = new Map()
    players.forEach(record => {
      const key = recordKey(record)
      if (key && !unique.has(key)) unique.set(key, record)
    })
    return [...unique.values()].sort(compareRecordIdentity)
  }, [players])

  const recordMap = useMemo(() => new Map(records.map(record => [recordKey(record), record])), [records])
  const latestSeason = useMemo(() => {
    const seasons = records.map(p => Number(p.season)).filter(Number.isFinite)
    return seasons.length ? Math.max(...seasons) : null
  }, [records])
  const defaults = useMemo(() => {
    const qualified = records
      .filter(p => p.season === latestSeason && p.games >= 5 && p.mpg >= 10)
    const pool = qualified.length >= 2 ? qualified : records
    return [...pool]
      .sort((a, b) => {
        const ppgOrder = (Number(b.ppg) || 0) - (Number(a.ppg) || 0)
        return ppgOrder || compareRecordIdentity(a, b)
      })
      .slice(0, 2)
  }, [records, latestSeason])

  const queryParams = useMemo(() => new URLSearchParams(location.search), [location.search])
  const resolveLegacyRecord = (id, season, team) => {
    if (id == null || season == null) return null
    const candidates = records
      .filter(record => String(record.licenseId) === String(id) && Number(record.season) === Number(season))
      .sort((a, b) => String(a.team).localeCompare(String(b.team), 'es'))
    return candidates.find(record => team && record.team === team) || candidates[0] || null
  }
  const legacyA = resolveLegacyRecord(params.aId, params.aSeason, queryParams.get('teamA'))
  const legacyB = resolveLegacyRecord(params.bId, params.bSeason, queryParams.get('teamB'))
  const preferredAKey = recordKey(legacyA || defaults[0])
  const preferredBKey = recordKey(legacyB || defaults.find(record => recordKey(record) !== preferredAKey))

  const [aKey, setAKey] = useState(preferredAKey)
  const [bKey, setBKey] = useState(preferredBKey)
  const syncedLocationRef = useRef(`${location.pathname}${location.search}`)

  useEffect(() => {
    const locationKey = `${location.pathname}${location.search}`
    if (syncedLocationRef.current === locationKey) return
    syncedLocationRef.current = locationKey
    const urlAKey = recordKey(legacyA)
    const urlBKey = recordKey(legacyB)
    if (urlAKey && recordMap.has(urlAKey)) setAKey(urlAKey)
    if (urlBKey && urlBKey !== urlAKey && recordMap.has(urlBKey)) setBKey(urlBKey)
  }, [legacyA, legacyB, location.pathname, location.search, recordMap])

  useEffect(() => {
    const nextA = recordMap.has(aKey) ? aKey : preferredAKey
    let nextB = recordMap.has(bKey) ? bKey : preferredBKey
    if (nextA && nextB === nextA) {
      nextB = recordKey(defaults.find(record => recordKey(record) !== nextA) || records.find(record => recordKey(record) !== nextA))
    }
    if (nextA !== aKey) setAKey(nextA)
    if (nextB !== bKey) setBKey(nextB)
  }, [aKey, bKey, defaults, preferredAKey, preferredBKey, recordMap, records])

  const playerA = recordMap.get(aKey) || null
  const playerB = recordMap.get(bKey) || null

  const recordOptions = useMemo(() => records.map(record => ({
    value: recordKey(record),
    label: getPlayerDisplayName(record),
    searchText: `${getPlayerSearchText(record)} ${record.team} ${seasonLabel(record.season)}`,
    meta: `${seasonLabel(record.season)} · ${record.team}`,
  })), [records])

  useEffect(() => {
    if (playerA && playerB) {
      const targetParams = new URLSearchParams()
      const aIsAmbiguous = records.filter(record => String(record.licenseId) === String(playerA.licenseId) && record.season === playerA.season).length > 1
      const bIsAmbiguous = records.filter(record => String(record.licenseId) === String(playerB.licenseId) && record.season === playerB.season).length > 1
      if (aIsAmbiguous) targetParams.set('teamA', playerA.team)
      if (bIsAmbiguous) targetParams.set('teamB', playerB.team)
      const search = targetParams.toString()
      const target = `/comparar/${playerA.licenseId}/${playerA.season}/${playerB.licenseId}/${playerB.season}${search ? `?${search}` : ''}`
      if (`${location.pathname}${location.search}` !== target) navigate(target, { replace: true })
    }
  }, [location.pathname, location.search, navigate, playerA, playerB, records])

  const bioA = playerA ? playerBio[String(playerA.licenseId)] : null
  const bioB = playerB ? playerBio[String(playerB.licenseId)] : null
  const photoA = playerA ? getPlayerPhoto(playerPhotos, playerA.licenseId, playerA.season) : null
  const photoB = playerB ? getPlayerPhoto(playerPhotos, playerB.licenseId, playerB.season) : null

  const handleSwap = () => {
    setAKey(bKey)
    setBKey(aKey)
  }

  return (
    <div className="app-page space-y-6">
      <PageHeader
        title="Comparación de jugadores"
        subtitle="Compara dos jugadores por producción, eficiencia, perfil de tiro, percentiles y on/off"
        scope="Cada selección conserva temporada y equipo"
        actions={playerA && playerB ? (
          <div className="flex gap-2 flex-wrap">
            <Link
              to={`/jugador/${playerA.licenseId}`}
              className="inline-flex items-center gap-1.5 px-3 py-2 text-xs font-medium rounded-md border border-acb-200 text-acb-600 hover:bg-acb-50"
            >
              <span className="w-2 h-2 rounded-full bg-accent-500 shrink-0" />
              {getPlayerDisplayName(playerA)} <ArrowRight className="w-3.5 h-3.5" />
            </Link>
            <Link
              to={`/jugador/${playerB.licenseId}`}
              className="inline-flex items-center gap-1.5 px-3 py-2 text-xs font-medium rounded-md border border-acb-200 text-acb-600 hover:bg-acb-50"
            >
              <span className="w-2 h-2 rounded-full bg-info-500 shrink-0" />
              {getPlayerDisplayName(playerB)} <ArrowRight className="w-3.5 h-3.5" />
            </Link>
          </div>
        ) : null}
      />

      <div className="grid lg:grid-cols-[1fr_auto_1fr] gap-4 items-center">
        <div className="bg-white rounded-lg border border-acb-200 border-t-2 border-t-accent-400 p-4">
          <PlayerCombobox
            id="compare-player-a"
            label="Jugador A"
            options={recordOptions.filter(option => option.value !== bKey)}
            value={aKey}
            onChange={option => option?.value && setAKey(option.value)}
            placeholder="Buscar jugador, equipo o temporada..."
          />
        </div>
        <button
          type="button"
          onClick={handleSwap}
          aria-label="Intercambiar jugadores"
          className="flex h-10 w-10 items-center justify-center justify-self-center self-center rounded-md border border-acb-200 bg-white text-xl leading-none text-acb-500 transition-colors hover:bg-acb-50 hover:text-acb-900"
          title="Intercambiar jugadores"
        >
          ⇄
        </button>
        <div className="bg-white rounded-lg border border-acb-200 border-t-2 border-t-info-400 p-4">
          <PlayerCombobox
            id="compare-player-b"
            label="Jugador B"
            options={recordOptions.filter(option => option.value !== aKey)}
            value={bKey}
            onChange={option => option?.value && setBKey(option.value)}
            placeholder="Buscar jugador, equipo o temporada..."
          />
        </div>
      </div>

      {playerA && playerB ? (
        <>
          <div className="grid lg:grid-cols-2 gap-4">
            <PlayerSummary
              record={playerA}
              bio={bioA}
              photoUrl={photoA}
              colorClass="border-accent-300"
            />
            <PlayerSummary
              record={playerB}
              bio={bioB}
              photoUrl={photoB}
              colorClass="border-info-300"
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
