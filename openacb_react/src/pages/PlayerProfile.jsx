import { useState, useMemo, useEffect, useRef } from 'react'
import { useParams, useNavigate } from 'react-router-dom'
import { Search, Loader2, Filter, GitCompareArrows } from 'lucide-react'
import { getPlayerPhoto } from '../utils/playerPhotos'


// ─── Helpers ───────────────────────────────────────────────────
function pctBadge(p) {
  if (p == null || isNaN(p)) return 'bg-acb-100 text-acb-600'
  if (p >= 75) return 'bg-positive-100 text-positive-700'
  if (p >= 50) return 'bg-info-100 text-info-700'
  if (p >= 25) return 'bg-info-100 text-info-600'
  return 'bg-negative-100 text-negative-700'
}

function fmt(v, key) {
  if (v == null) return '-'
  if (key === 'games') return v
  if (['fgPct', 'fg2Pct', 'fg3Pct', 'ftPct', 'efg', 'ts', 'usg', 'threeRate',
       'orbPct', 'drbPct', 'trbPct', 'astPct', 'stlPct', 'blkPct', 'tovPct'].includes(key))
    return `${v.toFixed(1)}%`
  if (key === 'ortg') return v.toFixed(1)
  if (key.startsWith('freq') || key.startsWith('fgpct')) return `${v.toFixed(1)}%`
  if (['offTo', 'secondChance', 'assistedFgm', 'assistedFgm2', 'assistedFgm3'].includes(key))
    return `${(v * 100).toFixed(1)}%`
  return v.toFixed(1)
}

function seasonLabel(s) {
  return `${s - 1}-${String(s).slice(-2)}`
}

// Age on October 1st of the season start year (e.g. season 2025 → Oct 1 2024)
function ageAtSeasonStart(birthDate, season) {
  if (!birthDate || !season) return null
  const birth = new Date(String(birthDate))
  const ref = new Date(season - 1, 9, 1) // Oct 1
  let age = ref.getFullYear() - birth.getFullYear()
  const m = ref.getMonth() - birth.getMonth()
  if (m < 0 || (m === 0 && ref.getDate() < birth.getDate())) age--
  return age
}

// ─── Player Selector (search with autocomplete + team/season filters) ────────────────
function PlayerSelector({ players, onSelect, selectedLicenseId }) {
  const [query, setQuery] = useState('')
  const [open, setOpen] = useState(false)
  const [teamFilter, setTeamFilter] = useState('')
  const [seasonFilter, setSeasonFilter] = useState('')
  const ref = useRef(null)

  // Close on outside click
  useEffect(() => {
    const handler = (e) => { if (ref.current && !ref.current.contains(e.target)) setOpen(false) }
    document.addEventListener('mousedown', handler)
    return () => document.removeEventListener('mousedown', handler)
  }, [])

  // Available seasons and teams for filters
  const availableSeasons = useMemo(() => {
    return [...new Set(players.map(p => p.season))].sort((a, b) => b - a)
  }, [players])

  const availableTeams = useMemo(() => {
    let filtered = players
    if (seasonFilter) filtered = filtered.filter(p => p.season === Number(seasonFilter))
    return [...new Set(filtered.map(p => p.team))].sort()
  }, [players, seasonFilter])

  const uniquePlayers = useMemo(() => {
    // First apply team/season filters to narrow the player pool
    let filtered = players
    if (seasonFilter) filtered = filtered.filter(p => p.season === Number(seasonFilter))
    if (teamFilter) filtered = filtered.filter(p => p.team === teamFilter)

    const map = new Map()
    filtered.forEach(p => {
      const key = p.licenseId
      if (!map.has(key)) {
        map.set(key, { licenseId: key, name: p.playerFull?.trim(), abbrev: p.playerAbbrev, team: p.team, season: p.season })
      }
    })
    return [...map.values()].sort((a, b) =>
      (a.abbrev || a.name).localeCompare(b.abbrev || b.name)
    )
  }, [players, teamFilter, seasonFilter])

  const filtered = useMemo(() => {
    if (!query.trim()) return uniquePlayers.slice(0, 50)
    const q = query.toLowerCase()
    return uniquePlayers.filter(p =>
      p.name.toLowerCase().includes(q) || p.abbrev?.toLowerCase().includes(q)
    ).slice(0, 50)
  }, [uniquePlayers, query])

  return (
    <div className="flex flex-wrap items-end gap-3">
      {/* Season Filter */}
      <div className="flex flex-col gap-1">
        <label className="text-xs text-acb-500 font-medium">Temporada</label>
        <select
          value={seasonFilter}
          onChange={(e) => { setSeasonFilter(e.target.value); setTeamFilter('') }}
          className="px-3 py-2.5 border border-acb-200 rounded-lg text-sm bg-white"
        >
          <option value="">Todas</option>
          {availableSeasons.map(s => (
            <option key={s} value={s}>{seasonLabel(s)}</option>
          ))}
        </select>
      </div>

      {/* Team Filter */}
      <div className="flex flex-col gap-1">
        <label className="text-xs text-acb-500 font-medium">Equipo</label>
        <div className="flex items-center gap-1.5">
          <Filter className="w-4 h-4 text-acb-400" />
          <select
            value={teamFilter}
            onChange={(e) => setTeamFilter(e.target.value)}
            className="px-3 py-2.5 border border-acb-200 rounded-lg text-sm bg-white"
          >
            <option value="">Todos</option>
            {availableTeams.map(team => (
              <option key={team} value={team}>{team}</option>
            ))}
          </select>
        </div>
      </div>

      {/* Player Search */}
      <div ref={ref} className="relative flex-1 min-w-[200px]">
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
                onClick={() => { onSelect(p.licenseId); setQuery(p.abbrev || p.name); setOpen(false) }}
                className={`px-4 py-2 text-sm cursor-pointer hover:bg-accent-50 flex items-center justify-between ${
                  String(selectedLicenseId) === String(p.licenseId) ? 'bg-accent-50 font-medium' : ''
                }`}
              >
                <span>{p.abbrev || p.name}</span>
                {(teamFilter || seasonFilter) && (
                  <span className="text-xs text-acb-400 ml-2">{p.team}</span>
                )}
              </li>
            ))}
          </ul>
        )}
      </div>
    </div>
  )
}

// ─── Season Picker (shared) ────────────────────────────────────
function SeasonPicker({ seasons, selected, onChange }) {
  return (
    <select
      value={selected ?? ''}
      onChange={e => onChange(Number(e.target.value))}
      className="px-3 py-1.5 border border-acb-200 rounded-md text-sm bg-white"
    >
      {seasons.map(s => (
        <option key={s} value={s}>{seasonLabel(s)}</option>
      ))}
    </select>
  )
}

// ─── Player Header Card ────────────────────────────────────────
function PlayerHeader({ records, photoUrl, bio, selectedSeason }) {
  const latest = records[0]
  const teams = [...new Set(records.map(r => r.team))]
  const seasons = [...new Set(records.map(r => r.season))].sort()
  const age = bio?.birthDate ? ageAtSeasonStart(bio.birthDate, selectedSeason || latest.season) : null

  return (
    <div className="bg-white rounded-lg border border-acb-200 p-6 flex gap-5 items-start">
      {photoUrl && (
        <img
          src={photoUrl}
          alt={latest.playerFull?.trim()}
          className="w-20 h-20 rounded-full object-cover object-top border-2 border-acb-200 flex-shrink-0"
        />
      )}
      <div className="min-w-0 flex-1">
        <div className="flex flex-wrap items-center gap-2">
          <h2 className="text-2xl font-bold text-acb-900">{latest.playerFull?.trim()}</h2>
          {bio?.position && (
            <span className="px-2 py-0.5 text-xs font-semibold rounded-full bg-accent-100 text-accent-700 border border-accent-200">
              {bio.position}
            </span>
          )}
        </div>
        <div className="mt-2 flex flex-wrap gap-x-5 gap-y-1 text-sm text-acb-600">
          {bio?.heightM && (
            <div>
              <span className="font-medium text-acb-700">Altura:</span>{' '}
              {parseFloat(bio.heightM).toFixed(2).replace('.', ',')} m
            </div>
          )}
          {bio?.birthDate && (
            <div>
              <span className="font-medium text-acb-700">Nacimiento:</span>{' '}
              {String(bio.birthDate).split('-').reverse().join('/')}
              {age != null && <span className="text-acb-400 ml-1">({age} años)</span>}
            </div>
          )}
          <div>
            <span className="font-medium text-acb-700">Equipos:</span>{' '}
            {teams.join(', ')}
          </div>
          <div>
            <span className="font-medium text-acb-700">Temporadas:</span>{' '}
            {seasons.map(s => seasonLabel(s)).join(', ')}
          </div>
          <div>
            <span className="font-medium text-acb-700">Partidos totales:</span>{' '}
            {records.reduce((sum, r) => sum + (r.games || 0), 0)}
          </div>
        </div>
      </div>
    </div>
  )
}

// ─── Career Table (with Basic / Advanced tabs) ─────────────────
const careerBasicCols = [
  { key: 'season', label: 'Temp.', fmtFn: v => seasonLabel(v), left: true },
  { key: 'team', label: 'Equipo', left: true },
  { key: 'games', label: 'PJ', integer: true },
  { key: 'mpg', label: 'MPP' },
  { key: 'ppg', label: 'PPP' },
  { key: 'rpg', label: 'RPP' },
  { key: 'orebpg', label: 'RO' },
  { key: 'drebpg', label: 'RD' },
  { key: 'apg', label: 'APP' },
  { key: 'spg', label: 'RBP' },
  { key: 'bpg', label: 'TPP' },
  { key: 'topg', label: 'PER' },
  { key: 'fpg', label: 'FPP' },
  { key: 'fgPct', label: 'TC%', suffix: '%' },
  { key: 'fg3Pct', label: '3P%', suffix: '%' },
  { key: 'ftPct', label: 'TL%', suffix: '%' },
]

const careerAdvancedCols = [
  { key: 'season', label: 'Temp.', fmtFn: v => seasonLabel(v), left: true },
  { key: 'team', label: 'Equipo', left: true },
  { key: 'games', label: 'PJ', integer: true },
  { key: 'ppg', label: 'PPP' },
  { key: 'ortg', label: 'ORtg' },
  { key: 'usg', label: 'USG%', suffix: '%' },
  { key: 'efg', label: 'eFG%', suffix: '%' },
  { key: 'ts', label: 'TS%', suffix: '%' },
  { key: 'threeRate', label: '3PAr', suffix: '%' },
  { key: 'orbPct', label: 'RO%', suffix: '%' },
  { key: 'drbPct', label: 'RD%', suffix: '%' },
  { key: 'trbPct', label: 'REB%', suffix: '%' },
  { key: 'astPct', label: 'AST%', suffix: '%' },
  { key: 'assistedFgm', label: 'Pts Ast%', fmtFn: v => `${(v * 100).toFixed(1)}%` },
  { key: 'assistedFgm2', label: 'Ast 2P%', fmtFn: v => `${(v * 100).toFixed(1)}%` },
  { key: 'assistedFgm3', label: 'Ast 3P%', fmtFn: v => `${(v * 100).toFixed(1)}%` },
  { key: 'stlPct', label: 'ROB%', suffix: '%' },
  { key: 'blkPct', label: 'TAP%', suffix: '%' },
  { key: 'tovPct', label: 'PER%', suffix: '%' },
]

function CareerTable({ records }) {
  const [tab, setTab] = useState('basic')
  const cols = tab === 'basic' ? careerBasicCols : careerAdvancedCols
  const sorted = [...records].sort((a, b) => a.season - b.season)

  return (
    <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
      <div className="px-4 py-3 border-b border-acb-200 flex items-center gap-3 flex-wrap">
        <h3 className="font-semibold text-acb-900">Trayectoria</h3>
        <div className="flex items-center gap-1 bg-acb-100 rounded-md p-0.5 ml-auto">
          <button
            onClick={() => setTab('basic')}
            className={`px-3 py-1 text-xs font-medium rounded transition-colors ${
              tab === 'basic' ? 'bg-white text-acb-900 shadow-sm' : 'text-acb-600 hover:text-acb-900'
            }`}
          >
            Básico
          </button>
          <button
            onClick={() => setTab('advanced')}
            className={`px-3 py-1 text-xs font-medium rounded transition-colors ${
              tab === 'advanced' ? 'bg-white text-acb-900 shadow-sm' : 'text-acb-600 hover:text-acb-900'
            }`}
          >
            Avanzado
          </button>
        </div>
      </div>
      <div className="overflow-x-auto">
        <table className="w-full text-sm">
          <thead>
            <tr className="bg-acb-50 border-b border-acb-200">
              {cols.map(c => (
                <th key={c.key} className={`px-3 py-2 text-xs font-semibold text-acb-600 uppercase tracking-wider whitespace-nowrap ${c.left ? 'text-left' : 'text-right'}`}>
                  {c.label}
                </th>
              ))}
            </tr>
          </thead>
          <tbody className="divide-y divide-acb-100">
            {sorted.map(r => (
              <tr key={`${r.season}-${r.team}`} className="hover:bg-acb-50">
                {cols.map(c => {
                  const v = r[c.key]
                  let display = '-'
                  if (v != null) {
                    if (c.fmtFn) display = c.fmtFn(v)
                    else if (c.left || c.integer) display = v
                    else display = `${Number(v).toFixed(1)}${c.suffix || ''}`
                  }
                  return (
                    <td key={c.key} className={`px-3 py-2 font-mono whitespace-nowrap ${
                      c.left ? (c.key === 'season' ? 'text-left font-medium text-acb-900' : 'text-left text-acb-700') : 'text-right text-acb-700'
                    }`}>
                      {display}
                    </td>
                  )
                })}
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  )
}

// ─── Percentile Bar ────────────────────────────────────────────
function PctBar({ label, value, pctKey, posPctKey, player, fmtKey, inverse, usePos }) {
  const v = player[value]
  const activePctKey = usePos && posPctKey ? posPctKey : pctKey
  const pct = activePctKey ? player[activePctKey] : null
  // For inverse stats (turnovers, fouls), high percentile = bad, so flip the color
  const colorPct = inverse ? (pct != null ? 100 - pct : null) : pct

  const barColor = colorPct == null ? 'bg-acb-200'
    : colorPct >= 75 ? 'bg-positive-500'
    : colorPct >= 50 ? 'bg-info-500'
    : colorPct >= 25 ? 'bg-info-400'
    : 'bg-negative-400'

  return (
    <div className="flex items-center gap-2 py-1.5">
      <span className="text-xs text-acb-600 w-16 shrink-0 text-right">{label}</span>
      <div className="flex-1 flex items-center gap-2">
        <div className="flex-1 h-4 bg-acb-100 rounded-full overflow-hidden relative">
          {/* 50th percentile marker */}
          <div className="absolute left-1/2 top-0 bottom-0 w-px bg-acb-300 z-10" />
          {pct != null && (
            <div
              className={`h-full rounded-full transition-all duration-500 ${barColor}`}
              style={{ width: `${Math.max(pct, 2)}%` }}
            />
          )}
        </div>
        <span className="font-mono text-xs text-acb-900 w-14 text-right shrink-0">{fmt(v, fmtKey || value)}</span>
        <span className={`text-xs w-9 text-right shrink-0 font-medium ${pctBadge(colorPct)} px-1 py-0.5 rounded`}>
          {pct != null ? `${Math.round(pct)}` : '-'}
        </span>
      </div>
    </div>
  )
}

// ─── Percentile Profile Card ───────────────────────────────────
const profileSections = [
  {
    title: 'Anotación',
    stats: [
      { label: 'PPP', value: 'ppg', pctKey: 'ppgPct', posPctKey: 'ppgPosPct' },
      { label: 'ORtg', value: 'ortg', pctKey: 'ortgPct', posPctKey: 'ortgPosPct', fmtKey: 'ortg' },
      { label: 'USG%', value: 'usg', pctKey: 'usgPct', posPctKey: 'usgPosPct', fmtKey: 'usg' },
      { label: 'TS%', value: 'ts', pctKey: 'tsPct', posPctKey: 'tsPosPct', fmtKey: 'ts' },
      { label: 'eFG%', value: 'efg', pctKey: 'efgPct', posPctKey: 'efgPosPct', fmtKey: 'efg' },
    ],
  },
  {
    title: 'Tiro',
    stats: [
      { label: 'TC%', value: 'fgPct', pctKey: 'fgPctPct', posPctKey: 'fgPctPosPct', fmtKey: 'fgPct' },
      { label: '3P%', value: 'fg3Pct', pctKey: 'fg3PctPct', posPctKey: 'fg3PctPosPct', fmtKey: 'fg3Pct' },
      { label: 'TL%', value: 'ftPct', pctKey: 'ftPctPct', posPctKey: 'ftPctPosPct', fmtKey: 'ftPct' },
      { label: '3PAr', value: 'threeRate', pctKey: 'threeRatePct', posPctKey: 'threeRatePosPct', fmtKey: 'threeRate' },
    ],
  },
  {
    title: 'Creación',
    stats: [
      { label: 'APP', value: 'apg', pctKey: 'apgPct', posPctKey: 'apgPosPct' },
      { label: 'AST%', value: 'astPct', pctKey: 'astPctPct', posPctKey: 'astPctPosPct', fmtKey: 'astPct' },
      { label: 'PER', value: 'topg', pctKey: 'topgPct', posPctKey: 'topgPosPct', inverse: true },
      { label: 'TOV%', value: 'tovPct', pctKey: 'tovPctPct', posPctKey: 'tovPctPosPct', fmtKey: 'tovPct' },
    ],
  },
  {
    title: 'Rebote',
    stats: [
      { label: 'RPP', value: 'rpg', pctKey: 'rpgPct', posPctKey: 'rpgPosPct' },
      { label: 'RO%', value: 'orbPct', pctKey: 'orbPctPct', posPctKey: 'orbPctPosPct', fmtKey: 'orbPct' },
      { label: 'RD%', value: 'drbPct', pctKey: 'drbPctPct', posPctKey: 'drbPctPosPct', fmtKey: 'drbPct' },
      { label: 'REB%', value: 'trbPct', pctKey: 'trbPctPct', posPctKey: 'trbPctPosPct', fmtKey: 'trbPct' },
    ],
  },
  {
    title: 'Defensa',
    stats: [
      { label: 'RBP', value: 'spg', pctKey: 'spgPct', posPctKey: 'spgPosPct' },
      { label: 'ROB%', value: 'stlPct', pctKey: 'stlPctPct', posPctKey: 'stlPctPosPct', fmtKey: 'stlPct' },
      { label: 'TPP', value: 'bpg', pctKey: 'bpgPct', posPctKey: 'bpgPosPct' },
      { label: 'TAP%', value: 'blkPct', pctKey: 'blkPctPct', posPctKey: 'blkPctPosPct', fmtKey: 'blkPct' },
    ],
  },
]

function PercentileProfile({ player }) {
  const [usePos, setUsePos] = useState(false)
  const hasPosPct = player.ppgPosPct != null

  return (
    <div className="bg-white rounded-lg border border-acb-200 p-5">
      <div className="mb-4 flex items-start justify-between">
        <div>
          <h3 className="font-semibold text-acb-900">Perfil de Rendimiento</h3>
          <p className="text-xs text-acb-500">{player.team} - {seasonLabel(player.season)} - {player.games} partidos - Percentiles {usePos ? `(vs. ${player.position || 'posición'})` : '(vs. liga)'}</p>
        </div>
        {hasPosPct && (
          <div className="flex rounded-md border border-acb-200 text-xs overflow-hidden shrink-0">
            <button
              onClick={() => setUsePos(false)}
              className={`px-3 py-1.5 font-medium transition-colors ${!usePos ? 'bg-acb-800 text-white' : 'bg-white text-acb-600 hover:bg-acb-50'}`}
            >Liga</button>
            <button
              onClick={() => setUsePos(true)}
              className={`px-3 py-1.5 font-medium transition-colors ${usePos ? 'bg-acb-800 text-white' : 'bg-white text-acb-600 hover:bg-acb-50'}`}
            >Posición</button>
          </div>
        )}
      </div>
      <div className="grid md:grid-cols-2 gap-x-8 gap-y-5">
        {profileSections.map(section => (
          <div key={section.title}>
            <h4 className="text-xs font-semibold text-acb-500 uppercase tracking-wider mb-1 border-b border-acb-100 pb-1">{section.title}</h4>
            {section.stats.map(s => (
              <PctBar key={s.value} {...s} player={player} usePos={usePos} />
            ))}
          </div>
        ))}
      </div>
      {/* Legend */}
      <div className="mt-4 pt-3 border-t border-acb-100 flex flex-wrap gap-3 text-xs text-acb-500">
        <span>Percentil:</span>
        <span className="flex items-center gap-1"><span className="inline-block w-3 h-3 rounded bg-positive-500" /> 75+</span>
        <span className="flex items-center gap-1"><span className="inline-block w-3 h-3 rounded bg-info-500" /> 50-74</span>
        <span className="flex items-center gap-1"><span className="inline-block w-3 h-3 rounded bg-info-400" /> 25-49</span>
        <span className="flex items-center gap-1"><span className="inline-block w-3 h-3 rounded bg-negative-400" /> 0-24</span>
        <span className="ml-auto">La línea central marca el percentil 50</span>
      </div>
    </div>
  )
}

// ─── Radar Chart (SVG) ─────────────────────────────────────────
const radarAxes = [
  { key: 'ppgPct', posKey: 'ppgPosPct', label: 'Anotación' },
  { key: 'tsPct', posKey: 'tsPosPct', label: 'Eficiencia' },
  { key: 'usgPct', posKey: 'usgPosPct', label: 'Volumen' },
  { key: 'threeRatePct', posKey: 'threeRatePosPct', label: 'Vol 3%' },
  { key: 'astPctPct', posKey: 'astPctPosPct', label: 'Creación' },
  { key: 'trbPctPct', posKey: 'trbPctPosPct', label: 'Rebote' },
  { key: 'blkPctPct', posKey: 'blkPctPosPct', label: 'Def. Interior' },
  { key: 'stlPctPct', posKey: 'stlPctPosPct', label: 'Def. Perímetro' },
]

function getRadarValues(player, usePos) {
  return radarAxes.map(axis => {
    const k = usePos && axis.posKey ? axis.posKey : axis.key
    return player[k] ?? 50
  })
}

function RadarChart({ player, usePos }) {
  const size = 360
  const cx = size / 2
  const cy = size / 2
  const radius = 100
  const levels = [25, 50, 75, 100]
  const n = radarAxes.length
  const values = getRadarValues(player, usePos)

  // Angle for each axis (start from top, go clockwise)
  const angle = (i) => (Math.PI * 2 * i) / n - Math.PI / 2

  // Point on the chart at a given axis index and percentile value (0-100)
  const point = (i, pct) => {
    const r = (pct / 100) * radius
    return [cx + r * Math.cos(angle(i)), cy + r * Math.sin(angle(i))]
  }

  // Grid polygon for a given level
  const gridPath = (level) =>
    Array.from({ length: n }, (_, i) => point(i, level))
      .map((p, i) => `${i === 0 ? 'M' : 'L'}${p[0].toFixed(1)},${p[1].toFixed(1)}`)
      .join(' ') + ' Z'

  // Player polygon
  const playerPath = values
    .map((v, i) => point(i, Math.min(v, 100)))
    .map((p, i) => `${i === 0 ? 'M' : 'L'}${p[0].toFixed(1)},${p[1].toFixed(1)}`)
    .join(' ') + ' Z'

  // Dynamic text-anchor based on position relative to center
  const labelAnchor = (i) => {
    const x = Math.cos(angle(i))
    if (x < -0.3) return 'end'
    if (x > 0.3) return 'start'
    return 'middle'
  }

  return (
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
      {radarAxes.map((_, i) => {
        const [ex, ey] = point(i, 100)
        return <line key={i} x1={cx} y1={cy} x2={ex} y2={ey} stroke="#e2e8f0" strokeWidth={0.7} />
      })}
      {/* Player fill */}
      <path d={playerPath} fill="rgba(240,132,94,0.18)" stroke="#f0845e" strokeWidth={2} />
      {/* Player dots */}
      {values.map((v, i) => {
        const [px, py] = point(i, Math.min(v, 100))
        return <circle key={i} cx={px} cy={py} r={3.5} fill="#f0845e" stroke="white" strokeWidth={1.5} />
      })}
      {/* Labels */}
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
      {/* Percentile values near dots */}
      {values.map((v, i) => {
        const [px, py] = point(i, Math.min(v, 100) + (v > 85 ? -12 : 12))
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
  )
}

// ─── Archetype Classifier ──────────────────────────────────────
function classifyArchetype(player, bio) {
  if (player.qualified === false)
    return { name: 'Datos insuficientes', desc: 'No cumple el mínimo de partidos o minutos para calcular el arquetipo', color: 'text-acb-400 bg-acb-50 border-acb-200' }

  const ppg = player.ppgPct ?? 50
  const ts  = player.tsPct ?? 50
  const usg = player.usgPct ?? 50
  const ast = player.astPctPct ?? 50
  const trb = player.trbPctPct ?? 50
  const stl = player.stlPctPct ?? 50
  const blk = player.blkPctPct ?? 50
  const orb = player.orbPctPct ?? 50
  const thr = player.threeRatePct ?? 50
  const mpg = player.mpg

  // Position & height (graceful fallback to null when missing)
  const rawPos = player.position || (bio && bio.position) || null
  const position = rawPos && rawPos.trim() ? rawPos.trim() : null
  const height = (() => {
    const h = player.heightM ?? (bio && bio.heightM)
    return h != null && isFinite(h) ? h : null
  })()

  // Position group flags (all false when position is unknown → pure stat fallback)
  const isGuardPos  = position === 'Base' || position === 'Escolta'
  const isSecondGuard = position == 'Escolta'
  const isWingPos   = position === 'Alero'
  const isBigPos    = position === 'Ala-pívot' || position === 'Pívot'
  const isCenterPos = position === 'Pívot'
  const isPFPos     = position === 'Ala-pívot'

  // Height flags
  const isTall     = height != null && height >= 2.00
  const isVeryTall = height != null && height >= 2.08

  // Shot creation: share of assisted scoring (0-1 scale, null when missing)
  const astdFgm  = player.assistedFgm  != null && isFinite(player.assistedFgm)  ? player.assistedFgm  : null
  const astdFgm3 = player.assistedFgm3 != null && isFinite(player.assistedFgm3) ? player.assistedFgm3 : null

  // Shot creation flags
  const isSelfCreator   = astdFgm != null && astdFgm < 0.40   // mostly creates own shot
  const isOffDribble3   = astdFgm3 != null && astdFgm3 < 0.60 // significant off-dribble 3PT ability
  const isCatchAndShoot = astdFgm3 != null && astdFgm3 >= 0.75 // mostly catch-and-shoot from 3

  // Trait flags
  const isHighVolume = usg >= 80
  const isScorer = ppg >= 80 && usg >= 70
  const isEfficient = ts >= 75
  const isPlaymaker = ast >= 75
  const isGoodPasser = ast >= 55
  const isRebounder = trb >= 80
  const isRimProtector = blk >= 75
  const isPerimDefender = stl >= 80
  const isShooter = thr >= 75
  const isAllAround = ppg >= 50 && ast >= 50 && blk >= 50 && stl >= 50 && trb >= 40

  // ── Guard archetypes (playmaker-first logic) ──

  // Passing big man: high-vision frontcourt player (Gasol/Jokic type)
  if (isBigPos && ast >= 75 && !isScorer && usg < 80 && trb >= 50)
    return { name: 'Interior Creador', desc: 'Interior con visión de juego excepcional que facilita el juego ofensivo', color: 'text-info-700 bg-info-50 border-info-200' }

  // Brick-layer: Players with high usage and very low efficiency
  if (usg >= 80 && ts < 10)
    return { name: 'Mandarinas', desc: 'Mandarinero de élite. Tira tanto como falla.', color: 'text-negative-700 bg-negative-50 border-negative-200' }

  // All-around elite guard: scores, creates, efficient AND elite defense
  if (isScorer && isPlaymaker && isEfficient && isPerimDefender && thr > 20 && mpg >= 20 && !isRebounder && !isCenterPos)
    return { name: 'Base Estrella', desc: 'Anota, crea, defiende y lo hace todo con eficiencia de elite', color: 'text-accent-700 bg-accent-50 border-accent-200' }
  
    // elite creator + elite defender: high usage playmaker with lockdown defense and elite scorer w/ limited efficiecny
  if (isPlaymaker && isPerimDefender && usg >= 75 && ppg >= 85 && thr > 20 && mpg >= 20 && !isRebounder && !isCenterPos && isSecondGuard)
    return { name: 'Combo Guard Todoterreno Élite', desc: 'Escolta con gran defensa perimetral y capaz de anotar con gran volumen', color: 'text-accent-700 bg-accent-50 border-accent-200' }

  // elite creator + elite defender: high usage playmaker with lockdown defense and elite scorer w/ limited efficiecny
  if (isPlaymaker && isPerimDefender && usg >= 75 && ppg >= 85 && thr > 20 && mpg >= 20 && !isRebounder && !isCenterPos && isGuardPos)
    return { name: 'Base Todoterreno Élite', desc: 'Anotador y creador estrella con defensa perimetral de alto nivel', color: 'text-accent-700 bg-accent-50 border-accent-200' }
  

  // Volume scorer: high usage + high scoring, regardless of efficiency
  if (ppg >= 90 && usg >= 90 && thr >= 35 && ast >= 50 && ast < 70 && ts >= 70 && mpg >= 20)
    return { name: 'Estrella Anotadora', desc: 'Anotador élite de alto volumen y eficiencia', color: 'text-accent-700 bg-accent-50 border-accent-200' }

  // Complete guard: scores a lot AND creates a lot (without elite defense)
  if (isScorer && isPlaymaker && isEfficient && trb < 50 && mpg >= 20 && !isCenterPos)
    return { name: 'Base Dominador', desc: 'Anota, crea para otros y lo hace con eficiencia', color: 'text-info-700 bg-info-50 border-info-200' }

  // good creator + elite defender: high usage playmaker with lockdown defense (not elite scorer)
  if (isPlaymaker && isPerimDefender && usg >= 75 && ppg >= 75 && blk < 50 && !isRebounder && isSelfCreator)
    return { name: 'Creador de Tiros Polivalente', desc: 'Creador de juego y anotador con defensa perimetral de alto nivel', color: 'text-sage-700 bg-sage-50 border-sage-200' }
  // High-octane creator: elite assists + high volume (may sacrifice efficiency)
  if (ast >= 95 && isHighVolume && ppg >= 70 && trb < 80)
    return { name: 'General en la Pista', desc: 'Anotador de alto volumen y alta capacidad de encontrar a sus compañeros', color: 'text-gold-700 bg-gold-50 border-gold-200' }


  // High-octane creator: elite assists + high volume (may sacrifice efficiency)
  if (ast >= 80 && usg >= 75 && ppg >= 80 && trb < 80 && mpg >= 20 && isSecondGuard)
    return { name: 'Combo Guard Anotador', desc: 'Escolta con buena capacidad de organizar el juego y alto volumen anotador', color: 'text-gold-700 bg-gold-50 border-gold-200' }

  // High-octane creator: elite assists + high volume (may sacrifice efficiency)
  if (ast >= 80 && isHighVolume && ppg >= 75 && trb < 80 && mpg >= 20 && isGuardPos)
    return { name: 'Creador de Tiros-Organizador', desc: 'Creador de alto octanaje que también habilita a sus compañeros', color: 'text-gold-700 bg-gold-50 border-gold-200' }
  
  // Efficient scorer who creates own shot
  if (isScorer && thr >= 40 && ast >= 50 && ast < 70 && stl < 75 && isEfficient && isSelfCreator)
    return { name: 'Anotador Autosuficiente', desc: 'Genera y convierte su propio tiro con alto volumen y eficiencia', color: 'text-gold-700 bg-gold-50 border-gold-200' }
  // Efficient off-ball scorer
  if (isScorer && thr >= 40 && ast >= 50 && ast < 70 && stl < 75 && isEfficient)
    return { name: 'Anotador Eficiente', desc: 'Anotador de alto volumen y alta eficiencia', color: 'text-gold-700 bg-gold-50 border-gold-200' }

  // Volume scorer: high usage + high scoring, regardless of efficiency
  if (isScorer && thr >= 30 && ast >= 40 && stl < 75 && mpg >= 20)
    return { name: 'Anotador Compulsivo', desc: 'Anotador de gran volumen con eficiencia limitada', color: 'text-gold-700 bg-gold-50 border-gold-200' }

  // Pure Scorer: self-creating or off-ball
  if (ppg > 85 && thr >= 30 && ast < 50 && stl < 75 && ts >= 70 && isSelfCreator)
    return { name: 'Anotador Puro', desc: 'Anotador eficiente capaz de generar sus propios tiros', color: 'text-gold-700 bg-gold-50 border-gold-200' }
  if (ppg > 85  && thr >= 30 && ast < 50 && stl < 75 && ts >= 70)
    return { name: 'Finalizador Eficiente', desc: 'Anotador eficiente que convierte oportunidades sin responsabilidades de creación', color: 'text-gold-700 bg-gold-50 border-gold-200' }

  // Volume scorer: high usage + high scoring, regardless of efficiency
  if (isScorer && thr >= 40 && ast <= 40 && stl < 75 && mpg >= 20 && !isGuardPos)
    return { name: 'Alero Anotador', desc: 'Alero con gran volumen y capacidad de anotar y sin responsabilidades en la creación de juego', color: 'text-gold-700 bg-gold-50 border-gold-200' }

  // Defensive floor general: elite playmaker + elite perimeter defense, not a scorer
  if (isPlaymaker && isPerimDefender && !isScorer && ppg < 80 && isGuardPos)
    return { name: 'Creador de Juego Defensivo', desc: 'Organiza el ataque y lidera la defensa perimetral', color: 'text-sage-700 bg-sage-50 border-sage-200' }

    // Defensive floor general: elite playmaker + elite perimeter defense, not a scorer
  if (ast>60 && isPerimDefender && !isScorer && ppg < 80 && isWingPos)
    return { name: 'Point-Forward Defensivo', desc: 'Organiza el ataque y lidera la defensa perimetral', color: 'text-sage-700 bg-sage-50 border-sage-200' }

  // Pass-first guard: elite playmaker, not high volume
  if (ast >= 75 && usg < 80 && !isScorer)
    return { name: 'Organizador Puro', desc: 'Prioriza la asistencia y la organización del ataque', color: 'text-info-700 bg-info-50 border-info-200' }

    // Pass-first guard: elite playmaker, not high volume
  if (ast >= 75 && usg < 80 && !isScorer && !isPerimDefender && !isGuardPos)
    return { name: 'Point Forward', desc: 'Prioriza la asistencia y la organización del ataque', color: 'text-info-700 bg-info-50 border-info-200' }

  // Two-way guard: scorer with elite defense
  if (isScorer && (stl >= 75 || blk >= 65) && ast >= 50 && ast < 70 && trb < 75)
    return { name: 'Escolta Two-Way', desc: 'Anotador con impacto defensivo elite', color: 'text-sage-700 bg-sage-50 border-sage-200' }

  // Pure scoring guard: efficient scorer with limited playmaking
  if (isScorer && isEfficient && ast < 65 && ast >= 50 && trb < 75)
    return { name: 'Combo Guard Anotador', desc: 'Anotador eficiente puro, genera poco para los demás', color: 'text-gold-700 bg-gold-50 border-gold-200' }
  
  // Pass-first guard: elite playmaker, not high volume
  if (ast >= 85 && usg > 70 && ppg > 65 && trb < 50 && blk < 30 && !isBigPos)
    return { name: 'Base Completo', desc: 'Organiza y anota eficientemente con buen volumen', color: 'text-info-700 bg-info-50 border-info-200' }

  
  // ── Wing / scorer archetypes ──

    // All-around Wing
  if (ppg  >= 70 && trb >= 70 && stl >= 70 && thr > 20 && ppg > 90)
    return { name: 'Alero Dominante', desc: 'Estrella Anotadora que también contribuye en defensa y rebote', color: 'text-accent-700 bg-accent-50 border-accent-200'}

  // All-around Wing
  if (ppg  >= 70 && trb >= 70 && stl >= 70 && thr > 20 && ppg < 90 && trb < 90 && stl < 90)
    return { name: 'Alero Completo', desc: 'Contribuye de forma equilibrada en ataque, rebote y defensa', color: 'text-sage-700 bg-sage-50 border-sage-200' }

        // Ala Pívot de Rol
  if (trb >= 70 && ppg >= 60 && ppg < 75 && blk < 70 && isPerimDefender && stl < 90 && thr > 20)
    return { name: 'Alero Defensivo Completo', desc: 'Rebotea y anota sin ser el foco de atención', color: 'text-sage-700 bg-sage-50 border-sage-200' }


  // Slasher: scores inside, low three-point volume
  if (isScorer && thr < 20 && !isRimProtector && stl  > 30)
    return { name: 'Finalizador Interior', desc: 'Anotador agresivo atacando el aro', color: 'text-gold-700 bg-gold-50 border-gold-200' }

  // 3&D Wing vs defensive shot creator: shooter + elite defense
  if (thr >= 75 && (isPerimDefender || isRimProtector) && isOffDribble3)
    return { name: 'Alero Creador Defensivo', desc: 'Crea su propio tiro exterior y defiende a alto nivel', color: 'text-sage-700 bg-sage-50 border-sage-200' }
  if (thr >= 75 && (isPerimDefender || isRimProtector))
    return { name: 'Alero 3&D', desc: 'Tirador exterior con defensa de élite', color: 'text-sage-700 bg-sage-50 border-sage-200' }

  // Shot creator vs catch-and-shoot specialist: shooter without elite defense
  if (isShooter && !isScorer && ast < 65 && !isPerimDefender && isOffDribble3)
    return { name: 'Tirador tras Bote', desc: 'Genera su propio tiro exterior con capacidad para anotar desde el bote', color: 'text-sand-700 bg-sand-50 border-sand-200' }
  if (isShooter && !isScorer && ast < 65 && !isPerimDefender)
    return { name: 'Francotirador', desc: 'Especialista en tiro exterior', color: 'text-sand-700 bg-sand-50 border-sand-200' }



  // ── Multi-skill archetypes ──

    // Perimeter lockdown
  if (isPerimDefender && !isScorer && !isRimProtector)
    return { name: 'Especialista Defensivo', desc: 'Especialista en robos y presión en el perímetro', color: 'text-positive-700 bg-positive-50 border-positive-200' }

      // Interior + perimeter defender  - elite
  if (blk >= 85 && stl > 85)
    return { name: 'Defensor Total', desc: 'Defensor de élite tanto en la zona como en el perímetro', color: 'text-positive-700 bg-positive-50 border-positive-200' }

    // Interior + perimeter defender - solid
  if (isRimProtector && isPerimDefender)
    return { name: 'Defensor Polivalente', desc: 'Impacto defensivo interior y perimetral', color: 'text-positive-700 bg-positive-50 border-positive-200' }


  // Triple-double threat
  if (ppg >= 90 && ast >= 90 && trb >= 90 )
    return { name: 'Amenaza de Triple-Doble', desc: 'Amenaza en anotación, asistencias y rebotes', color: 'text-accent-700 bg-accent-50 border-accent-200' }

  // Two-way star: elite scoring + elite defense
  if (ppg >= 85 && usg >= 85 && (blk >= 80 || stl >= 80) && thr >= 25 && mpg >= 20)
    return { name: 'Estrella Two-Way', desc: 'Dominante en ataque y defensa', color: 'text-accent-700 bg-accent-50 border-accent-200' }

  // Versatile guard: high usage + creation + defense, but not a star
  if (usg >= 65 && ast >= 70 && (stl >= 70) && ppg >= 70 && usg >= 70 && thr > 20 && !isRebounder && !isBigPos)
    return { name: 'Base Polivalente', desc: 'Base versátil con anotación, creación y defensa', color: 'text-info-700 bg-info-50 border-info-200' }

      // Point Forward
  if ((trb >= 70 && ast > 75 && blk > 60 && stl > 60 && thr > 50) ||
      ((isWingPos || isPFPos) && trb >= 65 && ast > 70 && blk > 40 && stl > 40 && thr > 40))
    return { name: 'Point Forward', desc: 'Ala-pívot con gran manejo de balón y capacidad de generar para sus compañeros', color:'text-info-700 bg-info-50 border-info-200'  }

  // ── Big man archetypes ──

  // Shooting big who doesn't protect the rim
  if (isBigPos && isShooter && trb >= 50 && blk < 60 && !isScorer)
    return { name: 'Interior con Tiro', desc: 'Interior que abre el campo con su amenaza exterior', color: 'text-plum-700 bg-plum-50 border-plum-200' }

  // Elite Center: All around
  if (isScorer && (trb >= 85 || (isCenterPos && trb >= 78)) && blk >70 && ast < 75 && mpg >= 20 && !isWingPos && !isGuardPos)
    return { name: 'Pívot Estrella', desc: 'Domina la zona, intimida y lleva el peso ofensivo del equipo.', color: 'text-accent-700 bg-accent-50 border-accent-200' }
  
  // Versatile Center
  if (isScorer && trb >= 80 && ast >= 70 && mpg >= 20 && !isWingPos && !isGuardPos)
    return { name: 'Pívot Moderno Estrella', desc: 'Anota en la pintura, rebotea, protege el aro y habilita a sus compañeros con facilidad', color: 'text-accent-700 bg-accent-50 border-accent-200' }

  // Versatile Center
  if (isScorer && trb >= 80 && ast >= 70 && !isWingPos && !isGuardPos)
    return { name: 'Pívot Moderno', desc: 'Anota en la pintura, rebotea y habilita a sus compañeros con facilidad', color: 'text-plum-700 bg-plum-50 border-plum-200' }

  // Post Scorer: self-creating vs finisher
  if (isScorer && isRebounder && blk < 70 && thr < 40 && !isWingPos && !isGuardPos && isSelfCreator)
    return { name: 'Creador de Tiros Interior', desc: 'Crea sus propios tiros en la zona con eficiencia y volumen', color: 'text-gold-700 bg-gold-50 border-gold-200' }
  if (isScorer && isRebounder && blk < 70 && thr < 40 && !isWingPos && !isGuardPos)
    return { name: 'Coche Escoba', desc: 'Finalizador interior con poca capacidad de generar sus propios tiros', color: 'text-gold-700 bg-gold-50 border-gold-200' }

  // Paint Beast: rebounds + blocks
  if (isRebounder && blk >= 80 && ts >= 85 && ppg  > 75 && !isWingPos && !isGuardPos)
    return { name: 'Bestia en la Zona', desc: 'Domina la zona con rebotes y protección de aro, anotando con eficiencia', color: 'text-gold-700 bg-gold-50 border-gold-200' }
    // Pívot Completo
  if (blk >= 70 && trb >= 70 && ppg >= 85 && !isWingPos && !isGuardPos)
    return { name: 'Interior Anotador', desc: 'Rebotea, protege el aro y anota en alto volumen', color: 'text-gold-700 bg-gold-50 border-gold-200' }

  // Rim Protector
  if ((isRebounder || (isCenterPos && trb >= 72)) && blk >= 80 && usg < 60 && !isWingPos && !isGuardPos)
    return {name: 'Protector del Aro', desc: 'Protector interior eficaz sin responsabilidades ofensivas', color: 'text-plum-700 bg-plum-50 border-plum-200' }

  // Interior defender (not rim protector archetype - lower rebounds)
  if (blk >= 90 && !isScorer && !isWingPos && !isGuardPos)
    return { name: 'Intimidador Interior', desc: 'Presencia defensiva cerca del aro con tapones', color: 'text-plum-700 bg-plum-50 border-plum-200' }

  // Stretch Big: rebounds + shoots threes
  if (trb >= 75 && blk >= 60 && thr >= 75 && (isBigPos || isVeryTall))
    return { name: 'Pívot Abierto', desc: 'Grande que abre el campo con tiro exterior', color: 'text-plum-700 bg-plum-50 border-plum-200' }

      // Ala Pívot: self-creating scorer vs finisher
  if (trb >= 80 && ppg >= 75 && usg > 65 && ts >80 && !isGuardPos && isSelfCreator)
    return { name: 'Ala-Pívot Anotador', desc: 'Rebotea y crea su propia anotación con gran eficiencia', color: 'text-plum-700 bg-plum-50 border-plum-200' }
  if (trb >= 80 && ppg >= 75 && usg > 65 && ts >80 && !isGuardPos)
    return { name: 'Ala Pívot Finalizador', desc: 'Rebotea y finaliza tras pase con gran eficiencia', color: 'text-plum-700 bg-plum-50 border-plum-200' }


    // Pívot Completo
  if (blk >= 70 && trb >= 70 && ppg >= 70 && !isWingPos && !isGuardPos)
    return { name: 'Interior de Rol Completo', desc: 'Rebotea, protege el aro y anota sin ser el foco de atención', color: 'text-plum-700 bg-plum-50 border-plum-200' }

      // Ala Pívot de Rol
  if (trb >= 70 && ppg >= 70 && usg < 60 && !isGuardPos)
    return { name: 'Ala Pívot de Rol', desc: 'Rebotea y anota sin ser el foco de atención', color: 'text-plum-700 bg-plum-50 border-plum-200' }

  // Glass Cleaner
  if ((isRebounder || (isCenterPos && trb >= 72)) && orb >= 70 && !isScorer && !isWingPos && !isGuardPos)
    return { name: 'Aspiradora', desc: 'Dominador del rebote ofensivo y defensivo', color: 'text-sand-700 bg-sand-50 border-sand-200' }

  // Rim Protector
  if (blk >= 70 && trb >= 70 && !isScorer && !isWingPos && !isGuardPos)
    return { name: 'Pívot de Rol', desc: 'Cumple su función de protector del aro y reboteador', color: 'text-plum-700 bg-plum-50 border-plum-200' }





  // Versatile PF catch-all before role archetypes
  if ((isPFPos || (isWingPos && isTall)) && ppg >= 65 && trb >= 65 && thr > 25)
    return { name: 'Ala-Pívot Versátil', desc: 'Ala-pívot con capacidad de anotar, rebotear y abrir el campo', color: 'text-plum-700 bg-plum-50 border-plum-200' }

  // Wing with interior impact: rebounds and/or blocks from the wing position
  if (isWingPos && trb >= 60 && (blk >= 50 || trb >= 75))
    return { name: 'Alero Reboteador', desc: 'Alero con impacto en el rebote y la defensa interior', color: 'text-sage-700 bg-sage-50 border-sage-200' }

  // ── Role archetypes ──

  // Second Unit Leader: High Volume and low minutes
  if (usg >= 85 && mpg < 20 && ts >= 75)
    return { name: 'Microondas', desc: 'Anota eficientemente y en alto volumen en minutos limitados', color: 'text-sand-700 bg-sand-50 border-sand-200'}


  // Second Unit Leader: High Volume and low minutes
  if (usg >= 80 && mpg < 20)
    return { name: 'Sexto Hombre', desc: 'Foco principal de la segunda unidad con alto volumen y minutos limitados', color: 'text-sand-700 bg-sand-50 border-sand-200'}

  // All-around
  if (isAllAround)
    return { name: 'Todoterreno', desc: 'Contribuye en todas las facetas del juego', color: 'text-info-700 bg-info-50 border-info-200' }

  // Glue Guy
  if ((stl >= 50 || blk >= 50) && trb >= 40 && mpg <20)
    return { name: 'Pegamento', desc: 'Jugador de equipo que contribuye con esfuerzo defensivo en minutos limitados', color: 'text-acb-700 bg-acb-50 border-acb-200' }

  // Fallback
  return { name: 'Jugador de Rol', desc: 'Cumple una función limitada en el esquema del equipo', color: 'text-acb-700 bg-acb-50 border-acb-200' }
}

// ─── Radar + Archetype Card ───────────────────────────────────
function RadarArchetypeCard({ player, bio }) {
  const archetype = classifyArchetype(player, bio)
  const [usePos, setUsePos] = useState(false)

  return (
    <div className="bg-white rounded-lg border border-acb-200 p-5">
      <div className="flex flex-col md:flex-row md:items-start gap-6">
        {/* Radar */}
        <div className="flex-1 min-w-0">
          <div className="flex items-center justify-between mb-1">
            <h3 className="font-semibold text-acb-900">Radar de Rendimiento</h3>
            <div className="flex rounded-lg overflow-hidden border border-acb-200 text-xs">
              <button
                onClick={() => setUsePos(false)}
                className={`px-3 py-1.5 font-medium transition-colors ${!usePos ? 'bg-acb-800 text-white' : 'bg-white text-acb-600 hover:bg-acb-50'}`}
              >Liga</button>
              <button
                onClick={() => setUsePos(true)}
                className={`px-3 py-1.5 font-medium transition-colors ${usePos ? 'bg-acb-800 text-white' : 'bg-white text-acb-600 hover:bg-acb-50'}`}
              >Posición</button>
            </div>
          </div>
          <p className="text-xs text-acb-500 mb-3">{player.team} - {seasonLabel(player.season)} - Percentiles {usePos ? `(vs. ${player.position || 'posición'})` : '(vs. liga)'}</p>
          <RadarChart player={player} usePos={usePos} />
        </div>
        {/* Archetype */}
        <div className="md:w-64 shrink-0 flex flex-col items-center md:items-start md:pt-10">
          <span className="text-xs font-semibold text-acb-500 uppercase tracking-wider mb-2">Arquetipo</span>
          <div className={`rounded-lg border px-4 py-3 text-center md:text-left ${archetype.color}`}>
            <div className="text-lg font-bold">{archetype.name}</div>
            <div className="text-xs mt-1 opacity-80">{archetype.desc}</div>
          </div>
          {/* Mini stats reference */}
          <div className="mt-4 text-xs text-acb-500 space-y-1 w-full">
            {radarAxes.map(axis => {
              const k = usePos && axis.posKey ? axis.posKey : axis.key
              const v = player[k]
              return (
                <div key={axis.key} className="flex justify-between">
                  <span>{axis.label}</span>
                  <span className="font-mono">{v != null ? Math.round(v) : '-'}p</span>
                </div>
              )
            })}
          </div>
        </div>
      </div>
    </div>
  )
}

// ─── Shooting Stats Card ───────────────────────────────────────
const zones = [
  { key: 'Rim', label: 'Zona Restringida' },
  { key: 'ShortMid', label: 'Zona No Restringida' },
  { key: 'LongMid', label: 'Media Distancia' },
  { key: 'CornerThree', label: 'Esquina 3' },
  { key: 'NcThree', label: 'Centro 3' },
  { key: 'AllThree', label: 'Total 3PT' },
]

function ShootingStatsCard({ player }) {
  const [shotTab, setShotTab] = useState('own')
  const hasRivalData = zones.some(z => player[`oppOnFgpct${z.key}`] != null)

  return (
    <div className="bg-white rounded-lg border border-acb-200 p-5">
      <div className="flex items-center justify-between mb-1">
        <h3 className="font-semibold text-acb-900">Tiro por Zona</h3>
        {hasRivalData && (
          <div className="flex rounded-md border border-acb-200 overflow-hidden text-xs">
            <button
              onClick={() => setShotTab('own')}
              className={`px-3 py-1 transition-colors ${shotTab === 'own' ? 'bg-acb-900 text-white' : 'bg-white text-acb-600 hover:bg-acb-50'}`}
            >
              Tiro Propio
            </button>
            <button
              onClick={() => setShotTab('rival')}
              className={`px-3 py-1 transition-colors ${shotTab === 'rival' ? 'bg-acb-900 text-white' : 'bg-white text-acb-600 hover:bg-acb-50'}`}
            >
              Tiro Rival
            </button>
          </div>
        )}
      </div>
      <p className="text-xs text-acb-500 mb-3">{player.team} - {player.games} partidos</p>
      <div className="overflow-x-auto">
        {shotTab === 'own' ? (
          <table className="w-full text-sm">
            <thead>
              <tr className="border-b border-acb-200">
                <th className="text-left py-2 text-xs font-semibold text-acb-600 uppercase">Zona</th>
                <th className="text-right py-2 text-xs font-semibold text-acb-600 uppercase">Freq%</th>
                <th className="text-right py-2 text-xs font-semibold text-acb-600 uppercase">FG%</th>
                <th className="text-right py-2 text-xs font-semibold text-acb-600 uppercase">FGA</th>
              </tr>
            </thead>
            <tbody className="divide-y divide-acb-100">
              {zones.map(z => {
                const freq = player[`freq${z.key}`]
                const fgpct = player[`fgpct${z.key}`]
                const fga = player[`fga${z.key}`]
                return (
                  <tr key={z.key} className="hover:bg-acb-50">
                    <td className="py-2 text-acb-700">{z.label}</td>
                    <td className="py-2 text-right font-mono text-acb-900">{freq != null ? `${freq.toFixed(1)}%` : '-'}</td>
                    <td className="py-2 text-right font-mono text-acb-900">{fgpct != null ? `${fgpct.toFixed(1)}%` : '-'}</td>
                    <td className="py-2 text-right font-mono text-acb-500">{fga ?? '-'}</td>
                  </tr>
                )
              })}
            </tbody>
          </table>
        ) : (
          <table className="w-full text-sm">
            <thead>
              <tr className="border-b border-acb-200">
                <th className="text-left py-2 text-xs font-semibold text-acb-600 uppercase">Zona</th>
                <th className="text-right py-2 text-xs font-semibold text-acb-600 uppercase">Diff</th>
                <th className="text-right py-2 text-xs font-semibold text-acb-600 uppercase">FG% Rival</th>
                <th className="text-right py-2 text-xs font-semibold text-acb-600 uppercase">FGA Rival</th>
              </tr>
            </thead>
            <tbody className="divide-y divide-acb-100">
              {zones.map(z => {
                const diff = player[`oppDiff${z.key}`]
                const fgpct = player[`oppOnFgpct${z.key}`]
                const fga = player[`oppFga${z.key}`]
                return (
                  <tr key={z.key} className="hover:bg-acb-50">
                    <td className="py-2 text-acb-700">{z.label}</td>
                    <td className={`py-2 text-right font-mono font-medium ${diff == null ? 'text-acb-400' : diff < 0 ? 'text-positive' : diff > 0 ? 'text-negative' : 'text-acb-700'}`}>
                      {diff != null ? `${diff > 0 ? '+' : ''}${diff.toFixed(1)}` : '-'}
                    </td>
                    <td className="py-2 text-right font-mono text-acb-900">{fgpct != null ? `${fgpct.toFixed(1)}%` : '-'}</td>
                    <td className="py-2 text-right font-mono text-acb-500">{fga ?? '-'}</td>
                  </tr>
                )
              })}
            </tbody>
          </table>
        )}
      </div>
    </div>
  )
}

// ─── On/Off Impact Card ────────────────────────────────────────
function OnOffCard({ records, loadLineupsForSeason, lineupsCache, loadingLineups }) {
  const seasonTeams = useMemo(() => {
    return records.map(r => ({ season: r.season, team: r.team, player: r.player, licenseId: r.licenseId }))
      .sort((a, b) => b.season - a.season)
  }, [records])

  useEffect(() => {
    seasonTeams.forEach(st => {
      if (!lineupsCache[st.season] && !loadingLineups[st.season]) {
        loadLineupsForSeason(st.season)
      }
    })
  }, [seasonTeams, lineupsCache, loadingLineups, loadLineupsForSeason])

  const rows = useMemo(() => {
    return seasonTeams.map(st => {
      const lineupData = lineupsCache[st.season]
      if (!lineupData?.data?.[st.team]?.players) {
        return { ...st, loading: loadingLineups[st.season] || false, found: false }
      }
      const playersObj = lineupData.data[st.team].players
      let playerData = null
      for (const [key, val] of Object.entries(playersObj)) {
        if (key.includes(String(st.licenseId)) || val.nickname === st.player || val.name?.includes(st.player)) {
          playerData = val
          break
        }
      }
      if (!playerData) return { ...st, loading: false, found: false }
      return { ...st, loading: false, found: true, ...playerData }
    })
  }, [seasonTeams, lineupsCache, loadingLineups])

  const ratingColor = (v, defensive = false) => {
    if (v == null) return 'text-acb-400'
    if (defensive) return v < 100 ? 'text-positive' : v > 110 ? 'text-negative' : 'text-acb-700'
    return v > 110 ? 'text-positive' : v < 100 ? 'text-negative' : 'text-acb-700'
  }

  const diffColor = (v) => {
    if (v == null) return 'text-acb-400'
    if (v > 2) return 'text-positive font-medium'
    if (v < -2) return 'text-negative font-medium'
    return 'text-acb-600'
  }

  const hasAnyData = rows.some(r => r.found)
  const anyLoading = rows.some(r => r.loading)

  return (
    <div className="bg-white rounded-lg border border-acb-200 p-5">
      <h3 className="font-semibold text-acb-900 mb-3">Impacto On/Off Court</h3>
      {anyLoading && (
        <div className="flex items-center gap-2 text-sm text-acb-500 mb-3">
          <Loader2 className="w-4 h-4 animate-spin" />
          Cargando datos de alineaciones...
        </div>
      )}
      {!hasAnyData && !anyLoading && (
        <p className="text-sm text-acb-500">No se encontraron datos de alineaciones para este jugador.</p>
      )}
      {hasAnyData && (
        <div className="overflow-x-auto">
          <table className="w-full text-sm">
            <thead>
              <tr className="text-xs font-semibold text-acb-600 uppercase border-b border-acb-200">
                <th className="text-left py-1" rowSpan={2}>Temp.</th>
                <th className="text-left py-1" rowSpan={2}>Equipo</th>
                <th colSpan={3} className="text-center py-1 border-b border-acb-100">Ataque</th>
                <th colSpan={3} className="text-center py-1 border-b border-acb-100">Defensa</th>
                <th colSpan={2} className="text-center py-1 border-b border-acb-100">Neto</th>
                <th className="text-right py-1" rowSpan={2}>Impacto</th>
                <th className="text-right py-1" rowSpan={2}>Min</th>
              </tr>
              <tr className="text-xs font-semibold text-acb-500 uppercase">
                <th className="text-right py-1">On</th>
                <th className="text-right py-1">Off</th>
                <th className="text-right py-1">Δ</th>
                <th className="text-right py-1">On</th>
                <th className="text-right py-1">Off</th>
                <th className="text-right py-1">Δ</th>
                <th className="text-right py-1">On</th>
                <th className="text-right py-1">Off</th>
              </tr>
            </thead>
            <tbody className="divide-y divide-acb-100">
              {rows.filter(r => r.found).map(r => {
                const ortgD = r.onORtg != null && r.offORtg != null ? r.onORtg - r.offORtg : null
                const drtgD = r.onDRtg != null && r.offDRtg != null ? r.onDRtg - r.offDRtg : null
                return (
                  <tr key={`${r.season}-${r.team}`} className="hover:bg-acb-50">
                    <td className="py-2 font-medium text-acb-900">{seasonLabel(r.season)}</td>
                    <td className="py-2 text-acb-700">{r.team}</td>
                    <td className="py-2 text-right font-mono text-acb-700">{r.onORtg?.toFixed(1) ?? '-'}</td>
                    <td className="py-2 text-right font-mono text-acb-500">{r.offORtg?.toFixed(1) ?? '-'}</td>
                    <td className={`py-2 text-right font-mono font-medium ${ortgD == null ? 'text-acb-400' : ortgD > 0 ? 'text-positive' : ortgD < 0 ? 'text-negative' : 'text-acb-500'}`}>
                      {ortgD != null ? `${ortgD > 0 ? '+' : ''}${ortgD.toFixed(1)}` : '-'}
                    </td>
                    <td className="py-2 text-right font-mono text-acb-700">{r.onDRtg?.toFixed(1) ?? '-'}</td>
                    <td className="py-2 text-right font-mono text-acb-500">{r.offDRtg?.toFixed(1) ?? '-'}</td>
                    <td className={`py-2 text-right font-mono font-medium ${drtgD == null ? 'text-acb-400' : drtgD < 0 ? 'text-positive' : drtgD > 0 ? 'text-negative' : 'text-acb-500'}`}>
                      {drtgD != null ? `${drtgD > 0 ? '+' : ''}${drtgD.toFixed(1)}` : '-'}
                    </td>
                    <td className="py-2 text-right font-mono text-acb-700">
                      {r.onNetRtg != null ? `${r.onNetRtg > 0 ? '+' : ''}${r.onNetRtg.toFixed(1)}` : '-'}
                    </td>
                    <td className="py-2 text-right font-mono text-acb-500">
                      {r.offNetRtg != null ? `${r.offNetRtg > 0 ? '+' : ''}${r.offNetRtg.toFixed(1)}` : '-'}
                    </td>
                    <td className={`py-2 text-right font-mono ${diffColor(r.netDiff)}`}>
                      {r.netDiff != null ? `${r.netDiff > 0 ? '+' : ''}${r.netDiff.toFixed(1)}` : '-'}
                    </td>
                    <td className="py-2 text-right font-mono text-acb-500">{r.onMin?.toFixed(0) ?? '-'}</td>
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

// ─── Main Page ─────────────────────────────────────────────────
export default function PlayerProfile({ players, playerPhotos = {}, playerBio = {}, loadLineupsForSeason, lineupsCache, loadingLineups }) {
  const { licenseId: urlLicenseId } = useParams()
  const navigate = useNavigate()
  const [selectedLicenseId, setSelectedLicenseId] = useState(null)
  const [selectedSeason, setSelectedSeason] = useState(null)

  // Sync from URL param when navigating via /jugador/:licenseId
  useEffect(() => {
    if (urlLicenseId != null) {
      // URL params are strings; coerce to number if the data uses numeric IDs
      const parsed = Number(urlLicenseId)
      setSelectedLicenseId(isNaN(parsed) ? urlLicenseId : parsed)
    }
  }, [urlLicenseId])

  // All records for the selected player, newest first
  const playerRecords = useMemo(() => {
    if (selectedLicenseId == null) return []
    // Compare with loose equality to handle string/number mismatch
    return players
      .filter(p => String(p.licenseId) === String(selectedLicenseId))
      .sort((a, b) => b.season - a.season)
  }, [players, selectedLicenseId])

  // Available seasons for this player
  const availableSeasons = useMemo(() => {
    return playerRecords.map(r => r.season)
  }, [playerRecords])

  // Default to latest season when player changes
  useEffect(() => {
    if (availableSeasons.length > 0) {
      setSelectedSeason(availableSeasons[0])
    } else {
      setSelectedSeason(null)
    }
  }, [selectedLicenseId, availableSeasons.length])

  // Record for the currently selected season
  const seasonRecord = useMemo(() => {
    if (!selectedSeason) return null
    return playerRecords.find(r => r.season === selectedSeason) || null
  }, [playerRecords, selectedSeason])

  // Bio: prefer fields embedded in the player record (from re-exported players.json),
  // fall back to the separate playerBio lookup for backwards compatibility.
  // Sanitize every value to a primitive — R's write_json can emit {} for some edge cases.
  const bio = useMemo(() => {
    if (!playerRecords.length) return null
    const str = v => (typeof v === 'string' && v) ? v : null
    const num = v => { const n = parseFloat(v); return isFinite(n) ? n : null }
    const latest = playerRecords[0]
    const ext = playerBio[String(selectedLicenseId)] || {}
    return {
      position:  str(latest.position)  ?? str(ext.position),
      heightM:   num(latest.heightM)   ?? num(ext.heightM),
      birthDate: str(latest.birthDate) ?? str(ext.birthDate),
    }
  }, [playerRecords, playerBio, selectedLicenseId])

  return (
    <div className="space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-semibold text-acb-900">Perfil de Jugador</h2>
        <p className="text-acb-500 text-sm mt-1">
          Selecciona un jugador para ver su perfil completo con estadísticas históricas
        </p>
      </div>

      {/* Player Selector */}
      <PlayerSelector
        players={players}
        onSelect={(id) => navigate(`/jugador/${id}`, { replace: true })}
        selectedLicenseId={selectedLicenseId}
      />

      {selectedLicenseId && playerRecords.length > 0 && (
        <>
          {/* Player Header */}
          <PlayerHeader
            records={playerRecords}
            photoUrl={getPlayerPhoto(playerPhotos, selectedLicenseId, selectedSeason)}
            bio={bio}
            selectedSeason={selectedSeason}
          />

          {/* Find Similar Players button */}
          {selectedSeason && (
            <button
              onClick={() => navigate(`/similitud/${selectedLicenseId}/${selectedSeason}`)}
              className="inline-flex items-center gap-2 px-4 py-2 text-sm font-medium text-accent-700 bg-accent-50 border border-accent-200 rounded-lg hover:bg-accent-100 transition-colors"
            >
              <GitCompareArrows className="w-4 h-4" />
              Buscar jugadores similares
              <span className="text-accent-500">→</span>
            </button>
          )}

          {/* Career Overview */}
          <CareerTable records={playerRecords} />

          {/* Season picker for detail cards */}
          {availableSeasons.length > 0 && (
            <div className="flex items-center gap-3">
              <span className="text-sm font-medium text-acb-700">Temporada detallada:</span>
              <SeasonPicker seasons={availableSeasons} selected={selectedSeason} onChange={setSelectedSeason} />
            </div>
          )}

          {seasonRecord && (
            <>
              <RadarArchetypeCard player={seasonRecord} bio={bio} />
              <PercentileProfile player={seasonRecord} />
            </>
          )}

          {/* Shooting Stats */}
          {seasonRecord && <ShootingStatsCard player={seasonRecord} />}

          {/* On/Off Impact */}
          <OnOffCard
            records={playerRecords}
            loadLineupsForSeason={loadLineupsForSeason}
            lineupsCache={lineupsCache}
            loadingLineups={loadingLineups}
          />
        </>
      )}

      {selectedLicenseId && playerRecords.length === 0 && (
        <div className="text-center py-12 text-acb-500">
          No se encontraron datos para este jugador.
        </div>
      )}
    </div>
  )
}
