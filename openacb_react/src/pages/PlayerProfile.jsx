import { useState, useMemo, useEffect } from 'react'
import { useLocation, useParams, useNavigate, useSearchParams } from 'react-router-dom'
import { Loader2, GitCompareArrows, Flame, Download } from 'lucide-react'
import PageHeader from '../components/PageHeader'
import PlayerCombobox from '../components/PlayerCombobox'
import { getPlayerPhoto } from '../utils/playerPhotos'
import { downloadTableAsCsv } from '../utils/csvDownload'
import { getPlayerDisplayName, getPlayerSearchText } from '../utils/playerNames'
import { getPercentileBadgeClass, getPercentileBarClass } from '../utils/percentileColors'
import { classifyArchetype, getSeasonArchetypePlayer } from '../utils/playerArchetypes'
import {
  formatPlayerProfileTableValue,
  playerProfileAdvancedStatColumns,
  playerProfileBasicStatColumns,
} from '../utils/playerProfileTableColumns'
import {
  buildPlayerProfilePath,
  buildPlayerSimilarityPath,
  getPlayerProfileSlug,
  parsePlayerSegment,
  parseRouteQuery,
  serializeRouteQuery,
  withQuery,
} from '../routing'


// ─── Helpers ───────────────────────────────────────────────────
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
  if (key === 'astToRatio') return v.toFixed(2)
  return v.toFixed(1)
}

function slugify(s) {
  return (s || 'jugador')
    .normalize('NFD')
    .replace(/\p{Diacritic}/gu, '')
    .replace(/[^a-zA-Z0-9]+/g, '-')
    .toLowerCase()
    .replace(/^-|-$/g, '')
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

// ─── player selector ───────────────────────────────────────────
function PlayerSelector({ players, onSelect, selectedLicenseId }) {
  const [teamFilter, setTeamFilter] = useState('')
  const [seasonFilter, setSeasonFilter] = useState('')

  const availableSeasons = useMemo(() => {
    return [...new Set(players.map(p => p.season))].sort((a, b) => b - a)
  }, [players])

  const availableTeams = useMemo(() => {
    let filtered = players
    if (seasonFilter) filtered = filtered.filter(p => p.season === Number(seasonFilter))
    return [...new Set(filtered.map(p => p.team))].sort()
  }, [players, seasonFilter])

  const playerOptions = useMemo(() => {
    let filtered = players
    if (seasonFilter) filtered = filtered.filter(p => p.season === Number(seasonFilter))
    if (teamFilter) filtered = filtered.filter(p => p.team === teamFilter)

    const map = new Map()
    filtered.forEach(p => {
      const key = p.licenseId
      map.set(key, {
        value: key,
        label: getPlayerDisplayName(p),
        searchText: getPlayerSearchText(p),
        meta: p.team,
      })
    })
    return [...map.values()].sort((a, b) =>
      a.label.localeCompare(b.label, 'es')
    )
  }, [players, teamFilter, seasonFilter])

  return (
    <div className="filter-panel">
      <div className="flex flex-col gap-1">
        <label htmlFor="profile-season-filter" className="field-label">Temporada</label>
        <select
          id="profile-season-filter"
          value={seasonFilter}
          onChange={(e) => { setSeasonFilter(e.target.value); setTeamFilter('') }}
          className="form-control"
        >
          <option value="">Todas</option>
          {availableSeasons.map(s => (
            <option key={s} value={s}>{seasonLabel(s)}</option>
          ))}
        </select>
      </div>

      <div className="flex flex-col gap-1">
        <label htmlFor="profile-team-filter" className="field-label">Equipo</label>
        <select
          id="profile-team-filter"
          value={teamFilter}
          onChange={(e) => setTeamFilter(e.target.value)}
          className="form-control"
        >
          <option value="">Todos</option>
          {availableTeams.map(team => (
            <option key={team} value={team}>{team}</option>
          ))}
        </select>
      </div>

      <PlayerCombobox
        id="profile-player-search"
        options={playerOptions}
        value={selectedLicenseId}
        onChange={option => onSelect(option.value)}
      />
    </div>
  )
}

// ─── Season Picker (shared) ────────────────────────────────────
function playerRecordKey(record) {
  return `${record.season}::${record.team}`
}

function SeasonPicker({ records, selected, onChange }) {
  return (
    <select
      id="player-profile-record"
      value={selected ?? ''}
      onChange={e => onChange(records.find(record => playerRecordKey(record) === e.target.value))}
      className="form-control-compact min-w-0 w-full sm:w-auto"
    >
      {!selected && <option value="" disabled>Selecciona temporada y equipo</option>}
      {records.map(record => (
        <option key={playerRecordKey(record)} value={playerRecordKey(record)}>
          {seasonLabel(record.season)} · {record.team}
        </option>
      ))}
    </select>
  )
}

// ─── Player Header Card ────────────────────────────────────────
function PlayerHeader({ records, photoUrl, bio, selectedSeason, selectedRecord }) {
  const latest = records[0]
  const activeRecord = selectedRecord || records.find(record => record.season === selectedSeason) || latest
  const teams = [...new Set(records.map(r => r.team))]
  const seasons = [...new Set(records.map(r => r.season))].sort()
  const age = bio?.birthDate ? ageAtSeasonStart(bio.birthDate, selectedSeason || latest.season) : null

  return (
    <div className="sticky top-12 z-30 isolate flex items-center gap-3 rounded-lg border border-acb-200 bg-white/95 p-3 shadow-md backdrop-blur-sm sm:top-16 sm:gap-4 sm:p-4 lg:items-start lg:gap-5 lg:p-6 xl:top-20">
      {photoUrl && (
        <img
          src={photoUrl}
          alt={getPlayerDisplayName(latest)}
          className="h-12 w-12 flex-shrink-0 rounded-full border-2 border-acb-200 object-cover object-top sm:h-14 sm:w-14 lg:h-20 lg:w-20"
        />
      )}
      <div className="min-w-0 flex-1">
        <div className="flex min-w-0 flex-wrap items-center gap-1.5 sm:gap-2">
          <h2 className="text-lg font-bold leading-tight text-acb-900 sm:text-xl lg:text-2xl">{getPlayerDisplayName(latest)}</h2>
          {bio?.position && (
            <span className="rounded-full border border-acb-200 bg-acb-100 px-1.5 py-0.5 text-[10px] font-semibold text-acb-800 sm:px-2 sm:text-xs">
              {bio.position}
            </span>
          )}
        </div>
        <p className="mt-1 truncate text-xs text-acb-500 sm:text-sm lg:hidden">
          {selectedRecord
            ? `${activeRecord.team} · ${seasonLabel(activeRecord.season)}`
            : 'Selecciona temporada y equipo'}
        </p>
        <div className="mt-2 hidden flex-wrap gap-x-5 gap-y-1 text-sm text-acb-600 lg:flex">
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
  { key: 'careerRole', label: 'Rol', fmtFn: v => v.name, left: true },
  ...playerProfileBasicStatColumns,
]

const careerAdvancedCols = [
  { key: 'season', label: 'Temp.', fmtFn: v => seasonLabel(v), left: true },
  { key: 'team', label: 'Equipo', left: true },
  { key: 'careerRole', label: 'Rol', fmtFn: v => v.name, left: true },
  ...playerProfileAdvancedStatColumns,
]

function CareerTable({ records, archetypeRecords = records, bio, tab = 'basic', onTabChange }) {
  const cols = tab === 'basic' ? careerBasicCols : careerAdvancedCols
  const sorted = [...records]
    .sort((a, b) => b.season - a.season)
    .map(record => ({
      ...record,
      careerRole: classifyArchetype(getSeasonArchetypePlayer(record, archetypeRecords), bio),
    }))

  const handleDownload = () => {
    const slug = slugify(records[0]?.player || records[0]?.playerFull)
    const filename = `${slug}_trayectoria_${tab === 'basic' ? 'basico' : 'avanzado'}.csv`
    const exportRows = sorted.map(r => {
      const row = {}
      cols.forEach(c => {
        const v = r[c.key]
        if (v == null) {
          row[c.key] = ''
        } else if (c.fmtFn) {
          row[c.key] = String(c.fmtFn(v)).replace('%', '')
        } else if (c.left || c.integer) {
          row[c.key] = v
        } else {
          row[c.key] = Number(v).toFixed(1)
        }
      })
      return row
    })
    downloadTableAsCsv(filename, exportRows, cols.map(c => ({ key: c.key, label: c.label })))
  }

  return (
    <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
      <div className="px-4 py-3 border-b border-acb-200 flex items-center gap-3 flex-wrap">
        <h3 className="font-semibold text-acb-900">Trayectoria</h3>
        <div className="flex items-center gap-2 ml-auto">
          <div className="flex items-center gap-1 bg-acb-100 rounded-md p-0.5">
            <button
              onClick={() => onTabChange?.('basic')}
              className={`px-3 py-1 text-xs font-medium rounded transition-colors ${
                tab === 'basic' ? 'bg-white text-acb-900 shadow-sm' : 'text-acb-600 hover:text-acb-900'
              }`}
            >
              Básico
            </button>
            <button
              onClick={() => onTabChange?.('advanced')}
              className={`px-3 py-1 text-xs font-medium rounded transition-colors ${
                tab === 'advanced' ? 'bg-white text-acb-900 shadow-sm' : 'text-acb-600 hover:text-acb-900'
              }`}
            >
              Avanzado
            </button>
          </div>
          <button
            onClick={handleDownload}
            className="inline-flex items-center gap-1.5 px-2.5 py-1 border border-acb-200 rounded text-xs bg-white text-acb-700 hover:bg-acb-50"
            title="Descargar CSV"
          >
            <Download className="w-3.5 h-3.5" />
            CSV
          </button>
        </div>
      </div>
      <div className="overflow-x-auto" tabIndex={0} aria-label="Trayectoria del jugador">
        <table className="data-table">
          <thead>
            <tr className="bg-acb-50 border-b border-acb-200">
              {cols.map(c => {
                const stickyClass = c.key === 'season'
                  ? 'career-sticky-season data-table-sticky-head data-col-season bg-acb-50'
                  : c.key === 'team'
                    ? 'career-sticky-team data-table-sticky-head data-col-team bg-acb-50'
                    : c.key === 'careerRole'
                      ? 'career-sticky-role data-table-sticky-head career-col-role bg-acb-50'
                      : ''
                return (
                  <th key={c.key} className={`px-3 py-2 text-xs font-semibold text-acb-600 uppercase tracking-wider whitespace-nowrap ${c.left ? 'text-left' : 'text-right'} ${stickyClass}`}>
                    {c.label}
                  </th>
                )
              })}
            </tr>
          </thead>
          <tbody className="divide-y divide-acb-100">
            {sorted.map(r => (
              <tr key={`${r.season}-${r.team}`} className="hover:bg-acb-50">
                {cols.map(c => {
                  const display = formatPlayerProfileTableValue(r, c)
                  const stickyClass = c.key === 'season'
                    ? 'career-sticky-season data-col-season'
                    : c.key === 'team'
                      ? 'career-sticky-team data-col-team'
                      : c.key === 'careerRole'
                        ? 'career-sticky-role career-col-role'
                        : ''
                  return (
                    <td key={c.key} className={`px-3 py-2 whitespace-nowrap ${
                      c.key === 'careerRole'
                        ? 'text-left text-acb-700'
                        : `font-mono ${c.left ? (c.key === 'season' ? 'text-left font-medium text-acb-900' : 'text-left text-acb-700') : 'text-right text-acb-700'}`
                    } ${stickyClass}`}>
                      {c.key === 'careerRole' ? (
                        <span
                          className={`inline-flex max-w-[14rem] truncate rounded border px-2 py-0.5 text-[11px] font-medium ${r.careerRole.color}`}
                          title={`${r.careerRole.name}: ${r.careerRole.desc}`}
                        >
                          {display}
                        </span>
                      ) : display}
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
function PctBar({ label, value, pctKey, posPctKey, player, fmtKey, usePos }) {
  const v = player[value]
  const activePctKey = usePos && posPctKey ? posPctKey : pctKey
  const pct = activePctKey ? player[activePctKey] : null

  return (
    <div className="flex items-center gap-2 py-1.5">
      <span className="text-xs text-acb-600 w-16 shrink-0 text-right">{label}</span>
      <div className="flex-1 flex items-center gap-2">
        <div className="flex-1 h-4 bg-acb-100 rounded-full overflow-hidden relative">
          {/* 50th percentile marker */}
          <div className="absolute left-1/2 top-0 bottom-0 w-px bg-acb-300 z-10" />
          {pct != null && (
            <div
              className={`h-full rounded-full transition-all duration-500 ${getPercentileBarClass(pct)}`}
              style={{ width: `${Math.max(pct, 2)}%` }}
            />
          )}
        </div>
        <span className="font-mono text-xs text-acb-900 w-14 text-right shrink-0">{fmt(v, fmtKey || value)}</span>
        <span className={`text-xs w-9 text-right shrink-0 font-medium ${getPercentileBadgeClass(pct)} px-1 py-0.5 rounded`}>
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
      { label: 'AST:TOV', value: 'astToRatio', pctKey: 'astToRatioPct', posPctKey: 'astToRatioPosPct' },
      { label: 'PER', value: 'topg', pctKey: 'topgPct', posPctKey: 'topgPosPct' },
      { label: 'PER%', value: 'tovPct', pctKey: 'tovPctPct', posPctKey: 'tovPctPosPct', fmtKey: 'tovPct' },
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
      { label: 'TAPP', value: 'bpg', pctKey: 'bpgPct', posPctKey: 'bpgPosPct' },
      { label: 'TAP%', value: 'blkPct', pctKey: 'blkPctPct', posPctKey: 'blkPctPosPct', fmtKey: 'blkPct' },
    ],
  },
]

function PercentileProfile({ player, reference = 'league', onReferenceChange }) {
  const usePos = reference === 'position'
  const hasPosPct = player.ppgPosPct != null

  return (
    <div className="bg-white rounded-lg border border-acb-200 p-5">
      <div className="mb-4 flex items-start justify-between">
        <div>
          <h3 className="font-semibold text-acb-900">Rendimiento</h3>
          <p className="text-xs text-acb-500">{player.team} - {seasonLabel(player.season)} - {player.games} partidos</p>
        </div>
        {hasPosPct && (
          <div className="segmented-control shrink-0">
            <button
              type="button"
              onClick={() => onReferenceChange?.('league')}
              aria-pressed={!usePos}
              className="segmented-option"
            >Liga</button>
            <button
              type="button"
              onClick={() => onReferenceChange?.('position')}
              aria-pressed={usePos}
              className="segmented-option"
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
        <span className="flex items-center gap-1"><span className="inline-block w-3 h-3 rounded bg-accent-300" /> 75+</span>
        <span className="flex items-center gap-1"><span className="inline-block w-3 h-3 rounded bg-accent-200" /> 50-74</span>
        <span className="flex items-center gap-1"><span className="inline-block w-3 h-3 rounded bg-info-200" /> 25-49</span>
        <span className="flex items-center gap-1"><span className="inline-block w-3 h-3 rounded bg-info-300" /> 0-24</span>
      </div>
    </div>
  )
}

// ─── Radar Chart (SVG) ─────────────────────────────────────────
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

// ─── Radar + Archetype Card ───────────────────────────────────
function RadarArchetypeCard({ player, archetypePlayer = player, bio, reference = 'league', onReferenceChange }) {
  const archetype = classifyArchetype(archetypePlayer, bio)
  const usePos = reference === 'position'

  return (
    <div className="bg-white rounded-lg border border-acb-200 p-5">
      <div className="flex flex-col md:flex-row md:items-start gap-6">
        {/* Radar */}
        <div className="flex-1 min-w-0">
          <div className="flex items-center justify-between mb-1">
            <h3 className="font-semibold text-acb-900">Radar de Rendimiento</h3>
            <div className="flex rounded-lg overflow-hidden border border-acb-200 text-xs">
              <button
                onClick={() => onReferenceChange?.('league')}
                className={`px-3 py-1.5 font-medium transition-colors ${!usePos ? 'bg-acb-800 text-white' : 'bg-white text-acb-600 hover:bg-acb-50'}`}
              >Liga</button>
              <button
                onClick={() => onReferenceChange?.('position')}
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
  { key: 'ShortMid', label: 'Pintura' },
  { key: 'LongMid', label: 'Media distancia' },
  { key: 'CornerThree', label: '3P esquina' },
  { key: 'NcThree', label: '3P frontal' },
  { key: 'AllThree', label: '3P total' },
]

function ShootingStatsCard({ player, shotTab = 'own', onShotTabChange }) {
  const hasRivalData = zones.some(z => player[`oppOnFgpct${z.key}`] != null)

  const handleDownload = () => {
    const slug = slugify(player.player || player.playerFull)
    const seasonStr = `${player.season - 1}-${String(player.season).slice(-2)}`
    const fmtNum = v => v == null ? '' : Number(v).toFixed(1)
    if (shotTab === 'own') {
      const cols = [
        { key: 'zone', label: 'Zona' },
        { key: 'freq', label: 'Frec.' },
        { key: 'fgpct', label: 'TC%' },
        { key: 'fga', label: 'Tiros' },
      ]
      const rows = zones.map(z => ({
        zone: z.label,
        freq: fmtNum(player[`freq${z.key}`]),
        fgpct: fmtNum(player[`fgpct${z.key}`]),
        fga: player[`fga${z.key}`] ?? '',
      }))
      downloadTableAsCsv(`${slug}_tiro-zona_propio_${seasonStr}.csv`, rows, cols)
    } else {
      const cols = [
        { key: 'zone', label: 'Zona' },
        { key: 'diff', label: 'Diff equipo (pp)' },
        { key: 'fgpct', label: 'TC% riv.' },
        { key: 'fga', label: 'Tiros riv.' },
      ]
      const rows = zones.map(z => ({
        zone: z.label,
        diff: fmtNum(player[`oppDiff${z.key}`]),
        fgpct: fmtNum(player[`oppOnFgpct${z.key}`]),
        fga: player[`oppFga${z.key}`] ?? '',
      }))
      downloadTableAsCsv(`${slug}_tiro-zona_rival_${seasonStr}.csv`, rows, cols)
    }
  }

  return (
    <div className="bg-white rounded-lg border border-acb-200 p-5">
      <div className="flex items-center justify-between mb-1 gap-2 flex-wrap">
        <h3 className="font-semibold text-acb-900">Tiro por Zona</h3>
        <div className="flex items-center gap-2">
          {hasRivalData && (
            <div className="flex rounded-md border border-acb-200 overflow-hidden text-xs">
              <button
                onClick={() => onShotTabChange?.('own')}
                className={`px-3 py-1 transition-colors ${shotTab === 'own' ? 'bg-acb-900 text-white' : 'bg-white text-acb-600 hover:bg-acb-50'}`}
              >
                Tiro Propio
              </button>
              <button
                onClick={() => onShotTabChange?.('rival')}
                className={`px-3 py-1 transition-colors ${shotTab === 'rival' ? 'bg-acb-900 text-white' : 'bg-white text-acb-600 hover:bg-acb-50'}`}
              >
                Tiro Rival
              </button>
            </div>
          )}
          <button
            onClick={handleDownload}
            className="inline-flex items-center gap-1.5 px-2.5 py-1 border border-acb-200 rounded text-xs bg-white text-acb-700 hover:bg-acb-50"
            title="Descargar CSV"
          >
            <Download className="w-3.5 h-3.5" />
            CSV
          </button>
        </div>
      </div>
      <p className="text-xs text-acb-500 mb-3">
        {player.team} - {player.games} partidos
        {shotTab === 'rival' && (
          <> · Dif. = tiro rival con el jugador en pista vs. media del equipo (negativo = mejor defensa)</>
        )}
      </p>
      <div className="overflow-x-auto">
        {shotTab === 'own' ? (
          <table className="data-table">
            <thead>
              <tr className="border-b border-acb-200">
                <th className="px-3 py-2 text-left text-xs font-semibold text-acb-600 uppercase whitespace-nowrap">Zona</th>
                <th className="px-3 py-2 text-right text-xs font-semibold text-acb-600 uppercase whitespace-nowrap" title="Frecuencia de tiro">Frec.</th>
                <th className="px-3 py-2 text-right text-xs font-semibold text-acb-600 uppercase whitespace-nowrap" title="Porcentaje de tiro de campo">TC%</th>
                <th className="px-3 py-2 text-right text-xs font-semibold text-acb-600 uppercase whitespace-nowrap">Tiros</th>
              </tr>
            </thead>
            <tbody className="divide-y divide-acb-100">
              {zones.map(z => {
                const freq = player[`freq${z.key}`]
                const fgpct = player[`fgpct${z.key}`]
                const fga = player[`fga${z.key}`]
                return (
                  <tr key={z.key} className="hover:bg-acb-50">
                    <td className="px-3 py-2 text-acb-700 whitespace-nowrap">{z.label}</td>
                    <td className="px-3 py-2 text-right font-mono text-acb-900 whitespace-nowrap">{freq != null ? `${freq.toFixed(1)}%` : '-'}</td>
                    <td className="px-3 py-2 text-right font-mono text-acb-900 whitespace-nowrap">{fgpct != null ? `${fgpct.toFixed(1)}%` : '-'}</td>
                    <td className="px-3 py-2 text-right font-mono text-acb-500 whitespace-nowrap">{fga ?? '-'}</td>
                  </tr>
                )
              })}
            </tbody>
          </table>
        ) : (
          <table className="data-table">
            <thead>
              <tr className="border-b border-acb-200">
                <th className="px-3 py-2 text-left text-xs font-semibold text-acb-600 uppercase whitespace-nowrap">Zona</th>
                <th className="px-3 py-2 text-right text-xs font-semibold text-acb-600 uppercase whitespace-nowrap" title="Diferencia en puntos porcentuales respecto al equipo">Diff equipo (pp)</th>
                <th className="px-3 py-2 text-right text-xs font-semibold text-acb-600 uppercase whitespace-nowrap" title="Porcentaje de tiro permitido al rival">TC% riv.</th>
                <th className="px-3 py-2 text-right text-xs font-semibold text-acb-600 uppercase whitespace-nowrap">Tiros riv.</th>
              </tr>
            </thead>
            <tbody className="divide-y divide-acb-100">
              {zones.map(z => {
                const diff = player[`oppDiff${z.key}`]
                const fgpct = player[`oppOnFgpct${z.key}`]
                const fga = player[`oppFga${z.key}`]
                return (
                  <tr key={z.key} className="hover:bg-acb-50">
                    <td className="px-3 py-2 text-acb-700 whitespace-nowrap">{z.label}</td>
                    <td className={`px-3 py-2 text-right font-mono font-medium whitespace-nowrap ${diff == null ? 'text-acb-400' : diff < 0 ? 'text-positive' : diff > 0 ? 'text-negative' : 'text-acb-700'}`}>
                      {diff != null ? `${diff > 0 ? '+' : ''}${diff.toFixed(1)} pp` : '-'}
                    </td>
                    <td className="px-3 py-2 text-right font-mono text-acb-900 whitespace-nowrap">{fgpct != null ? `${fgpct.toFixed(1)}%` : '-'}</td>
                    <td className="px-3 py-2 text-right font-mono text-acb-500 whitespace-nowrap">{fga ?? '-'}</td>
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

  const diffColor = (v) => {
    if (v == null) return 'text-acb-400'
    if (v > 2) return 'text-positive font-medium'
    if (v < -2) return 'text-negative font-medium'
    return 'text-acb-600'
  }

  const hasAnyData = rows.some(r => r.found)
  const anyLoading = rows.some(r => r.loading)

  const handleDownload = () => {
    const slug = slugify(records[0]?.player || records[0]?.playerFull)
    const cols = [
      { key: 'season', label: 'Temporada' },
      { key: 'team', label: 'Equipo' },
      { key: 'onORtg', label: 'On ORtg' },
      { key: 'offORtg', label: 'Off ORtg' },
      { key: 'ortgD', label: 'Diff ORtg' },
      { key: 'onDRtg', label: 'On DRtg' },
      { key: 'offDRtg', label: 'Off DRtg' },
      { key: 'drtgD', label: 'Diff DRtg' },
      { key: 'onNetRtg', label: 'On Neto' },
      { key: 'offNetRtg', label: 'Off Neto' },
      { key: 'netDiff', label: 'Impacto' },
      { key: 'onMin', label: 'Min' },
    ]
    const fmtNum = v => v == null ? '' : Number(v).toFixed(1)
    const exportRows = rows.filter(r => r.found).map(r => {
      const ortgD = r.onORtg != null && r.offORtg != null ? r.onORtg - r.offORtg : null
      const drtgD = r.onDRtg != null && r.offDRtg != null ? r.onDRtg - r.offDRtg : null
      return {
        season: seasonLabel(r.season),
        team: r.team,
        onORtg: fmtNum(r.onORtg),
        offORtg: fmtNum(r.offORtg),
        ortgD: fmtNum(ortgD),
        onDRtg: fmtNum(r.onDRtg),
        offDRtg: fmtNum(r.offDRtg),
        drtgD: fmtNum(drtgD),
        onNetRtg: fmtNum(r.onNetRtg),
        offNetRtg: fmtNum(r.offNetRtg),
        netDiff: fmtNum(r.netDiff),
        onMin: r.onMin == null ? '' : Math.round(r.onMin),
      }
    })
    downloadTableAsCsv(`${slug}_onoff.csv`, exportRows, cols)
  }

  return (
    <div className="bg-white rounded-lg border border-acb-200 p-5">
      <div className="flex items-center justify-between mb-3 gap-2 flex-wrap">
        <div className="flex items-center gap-2">
          <h3 className="text-sm font-semibold text-acb-900">Impacto On/Off Court</h3>
        </div>
        {hasAnyData && (
          <button
            onClick={handleDownload}
            className="inline-flex items-center gap-1.5 px-2.5 py-1 border border-acb-200 rounded text-xs bg-white text-acb-700 hover:bg-acb-50"
            title="Descargar CSV"
          >
            <Download className="w-3.5 h-3.5" />
            CSV
          </button>
        )}
      </div>
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
          <table className="data-table border-collapse">
            <thead>
              <tr className="text-xs font-semibold text-acb-600 uppercase border-b border-acb-200">
                <th className="data-table-sticky data-table-sticky-head data-col-season bg-acb-50 px-2 py-1.5 text-left whitespace-nowrap" rowSpan={2}>Temp.</th>
                <th className="data-table-sticky-after-season data-table-sticky-head data-col-team bg-acb-50 px-2 py-1.5 text-left whitespace-nowrap" rowSpan={2}>Equipo</th>
                <th colSpan={3} className="px-2 py-1.5 text-center whitespace-nowrap border-b border-l border-acb-100">Ataque</th>
                <th colSpan={3} className="px-2 py-1.5 text-center whitespace-nowrap border-b border-l border-acb-100">Defensa</th>
                <th colSpan={2} className="px-2 py-1.5 text-center whitespace-nowrap border-b border-l border-acb-100">Neto</th>
                <th className="px-2 py-1.5 text-right whitespace-nowrap border-l border-acb-100" rowSpan={2}>Impacto</th>
                <th className="px-2 py-1.5 text-right whitespace-nowrap" rowSpan={2}>Min</th>
              </tr>
              <tr className="text-xs font-semibold text-acb-500 uppercase">
                <th className="px-2 py-1.5 text-right whitespace-nowrap border-l border-acb-100">On</th>
                <th className="px-2 py-1.5 text-right whitespace-nowrap">Off</th>
                <th className="px-2 py-1.5 text-right whitespace-nowrap">Diff</th>
                <th className="px-2 py-1.5 text-right whitespace-nowrap border-l border-acb-100">On</th>
                <th className="px-2 py-1.5 text-right whitespace-nowrap">Off</th>
                <th className="px-2 py-1.5 text-right whitespace-nowrap">Diff</th>
                <th className="px-2 py-1.5 text-right whitespace-nowrap border-l border-acb-100">On</th>
                <th className="px-2 py-1.5 text-right whitespace-nowrap">Off</th>
              </tr>
            </thead>
            <tbody className="divide-y divide-acb-100">
              {rows.filter(r => r.found).map(r => {
                const ortgD = r.onORtg != null && r.offORtg != null ? r.onORtg - r.offORtg : null
                const drtgD = r.onDRtg != null && r.offDRtg != null ? r.onDRtg - r.offDRtg : null
                return (
                  <tr key={`${r.season}-${r.team}`} className="hover:bg-acb-50">
                    <td className="data-table-sticky data-col-season bg-white px-2 py-2 font-medium text-acb-900 whitespace-nowrap">{seasonLabel(r.season)}</td>
                    <td className="profile-history-team data-table-sticky-after-season data-col-team bg-white">{r.team}</td>
                    <td className="px-2 py-2 text-right font-mono text-acb-700 whitespace-nowrap border-l border-acb-100">{r.onORtg?.toFixed(1) ?? '-'}</td>
                    <td className="px-2 py-2 text-right font-mono text-acb-500 whitespace-nowrap">{r.offORtg?.toFixed(1) ?? '-'}</td>
                    <td className={`px-2 py-2 text-right font-mono font-medium whitespace-nowrap ${ortgD == null ? 'text-acb-400' : ortgD > 0 ? 'text-positive' : ortgD < 0 ? 'text-negative' : 'text-acb-500'}`}>
                      {ortgD != null ? `${ortgD > 0 ? '+' : ''}${ortgD.toFixed(1)}` : '-'}
                    </td>
                    <td className="px-2 py-2 text-right font-mono text-acb-700 whitespace-nowrap border-l border-acb-100">{r.onDRtg?.toFixed(1) ?? '-'}</td>
                    <td className="px-2 py-2 text-right font-mono text-acb-500 whitespace-nowrap">{r.offDRtg?.toFixed(1) ?? '-'}</td>
                    <td className={`px-2 py-2 text-right font-mono font-medium whitespace-nowrap ${drtgD == null ? 'text-acb-400' : drtgD < 0 ? 'text-positive' : drtgD > 0 ? 'text-negative' : 'text-acb-500'}`}>
                      {drtgD != null ? `${drtgD > 0 ? '+' : ''}${drtgD.toFixed(1)}` : '-'}
                    </td>
                    <td className="px-2 py-2 text-right font-mono text-acb-700 whitespace-nowrap border-l border-acb-100">
                      {r.onNetRtg != null ? `${r.onNetRtg > 0 ? '+' : ''}${r.onNetRtg.toFixed(1)}` : '-'}
                    </td>
                    <td className="px-2 py-2 text-right font-mono text-acb-500 whitespace-nowrap">
                      {r.offNetRtg != null ? `${r.offNetRtg > 0 ? '+' : ''}${r.offNetRtg.toFixed(1)}` : '-'}
                    </td>
                    <td className={`px-2 py-2 text-right font-mono whitespace-nowrap border-l border-acb-100 ${diffColor(r.netDiff)}`}>
                      {r.netDiff != null ? `${r.netDiff > 0 ? '+' : ''}${r.netDiff.toFixed(1)}` : '—'}
                    </td>
                    <td className="px-2 py-2 text-right font-mono text-acb-500 whitespace-nowrap">{r.onMin?.toFixed(0) ?? '-'}</td>
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

// ─── Clutch Card ───────────────────────────────────────────────
function ClutchCard({ records, loadClutchForSeason, clutchCache, loadingClutch }) {
  const seasonTeams = useMemo(() => {
    return [...new Map(records.map(record => [
      `${record.season}::${record.teamId || record.team}`,
      { season: record.season, team: record.team, teamId: record.teamId },
    ])).values()]
  }, [records])
  const seasons = useMemo(() => [...new Set(seasonTeams.map(record => record.season))], [seasonTeams])
  const licenseId = records[0]?.licenseId

  // Load clutch data for all seasons this player played in
  useEffect(() => {
    seasons.forEach(s => loadClutchForSeason(s))
  }, [seasons, loadClutchForSeason])

  const clutchRows = useMemo(() => {
    return seasonTeams
      .map(({ season, team, teamId }) => {
        const data = clutchCache[season]
        if (!data) return null
        const playerEntries = data.players?.filter(p => String(p.licenseId) === String(licenseId)) || []
        const entry = playerEntries.find(p => (teamId && p.teamId === teamId) || p.team === team)
        if (!entry) return null
        return { ...entry, season, team: entry.team || team }
      })
      .filter(Boolean)
      .sort((a, b) => b.season - a.season)
  }, [seasonTeams, clutchCache, licenseId])

  const anyLoading = seasons.some(s => loadingClutch[s])
  const seasonLabel = s => `${s - 1}-${String(s).slice(-2)}`
  const fmt = (v, pct) => {
    if (v == null || isNaN(v)) return '-'
    return pct ? `${Number(v).toFixed(1)}%` : Number(v).toFixed(1)
  }

  if (!anyLoading && clutchRows.length === 0) return null

  const handleDownload = () => {
    const slug = slugify(records[0]?.player || records[0]?.playerFull)
    const cols = [
      { key: 'season', label: 'Temporada' },
      { key: 'team', label: 'Equipo' },
      { key: 'games', label: 'PJ' },
      { key: 'pts', label: 'Pts' },
      { key: 'reb', label: 'Reb' },
      { key: 'ast', label: 'Ast' },
      { key: 'stl', label: 'Rob' },
      { key: 'blk', label: 'Tap' },
      { key: 'tov', label: 'Pér' },
      { key: 'fg2Pct', label: 'T2%' },
      { key: 'fg3Pct', label: '3P%' },
      { key: 'ftPct', label: 'TL%' },
      { key: 'efgPct', label: 'eFG%' },
      { key: 'tsPct', label: 'TS%' },
      { key: 'fg3Rate', label: '3PAr' },
    ]
    const fmtNum = v => v == null || isNaN(v) ? '' : Number(v).toFixed(1)
    const exportRows = clutchRows.map(r => {
      const fga = (r.fg2A || 0) + (r.fg3A || 0)
      const tsPct = r.ptsT != null && fga + (r.ftA || 0) > 0
        ? r.ptsT / (2 * (fga + 0.44 * (r.ftA || 0))) * 100
        : null
      const fg3Rate = fga > 0 ? (r.fg3A || 0) / fga * 100 : null
      return {
        season: seasonLabel(r.season),
        team: r.team,
        games: r.games ?? '',
        pts: fmtNum(r.pts),
        reb: fmtNum(r.reb),
        ast: fmtNum(r.ast),
        stl: fmtNum(r.stl),
        blk: fmtNum(r.blk),
        tov: fmtNum(r.tov),
        fg2Pct: fmtNum(r.fg2Pct),
        fg3Pct: fmtNum(r.fg3Pct),
        ftPct: fmtNum(r.ftPct),
        efgPct: fmtNum(r.efgPct),
        tsPct: fmtNum(tsPct),
        fg3Rate: fmtNum(fg3Rate),
      }
    })
    downloadTableAsCsv(`${slug}_clutch.csv`, exportRows, cols)
  }

  return (
    <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
      <div className="px-5 py-3 border-b border-acb-100 flex items-center gap-2 flex-wrap">
        <Flame className="w-4 h-4 text-orange-500" />
        <h3 className="font-semibold text-acb-900 text-sm">Estadísticas clutch</h3>
        <span className="text-xs text-acb-400 ml-1">Últimos 5 min con diferencia de ≤ 5 pts</span>
        {clutchRows.length > 0 && (
          <button
            onClick={handleDownload}
            className="ml-auto inline-flex items-center gap-1.5 px-2.5 py-1 border border-acb-200 rounded text-xs bg-white text-acb-700 hover:bg-acb-50"
            title="Descargar CSV"
          >
            <Download className="w-3.5 h-3.5" />
            CSV
          </button>
        )}
      </div>
      {anyLoading && clutchRows.length === 0 ? (
        <div className="flex items-center justify-center py-6 text-acb-400">
          <Loader2 className="w-4 h-4 animate-spin mr-2" />Cargando…
        </div>
      ) : (
        <div className="overflow-x-auto">
          <table className="data-table">
            <thead>
              <tr className="bg-acb-50 border-b border-acb-200">
                {['Temporada','Equipo','PJ','Pts','Reb','Ast','Rob','Tap','Pér','T2%','3P%','TL%','eFG%','TS%','3PAr'].map(h => (
                  <th key={h} className={`px-3 py-2 text-xs font-semibold text-acb-500 uppercase tracking-wider whitespace-nowrap ${h === 'Temporada' || h === 'Equipo' ? 'text-left' : 'text-right'} ${h === 'Temporada' ? 'data-table-sticky data-table-sticky-head data-col-season bg-acb-50' : h === 'Equipo' ? 'data-table-sticky-after-season data-table-sticky-head data-col-team bg-acb-50' : ''}`}>{h}</th>
                ))}
              </tr>
            </thead>
            <tbody className="divide-y divide-acb-100">
              {clutchRows.map(r => {
                const fga    = (r.fg2A || 0) + (r.fg3A || 0)
                const tsPct  = r.ptsT != null && fga + (r.ftA || 0) > 0
                  ? r.ptsT / (2 * (fga + 0.44 * (r.ftA || 0))) * 100
                  : null
                const fg3Rate = fga > 0 ? (r.fg3A || 0) / fga * 100 : null
                return (
                  <tr key={`${r.season}-${r.team}`} className="hover:bg-acb-50">
                    <td className="data-table-sticky data-col-season bg-white px-3 py-2 text-left font-medium text-acb-900 whitespace-nowrap">{seasonLabel(r.season)}</td>
                    <td className="profile-history-team data-table-sticky-after-season data-col-team bg-white">{r.team}</td>
                    <td className="px-3 py-2 text-right font-mono text-acb-700">{r.games}</td>
                    <td className="px-3 py-2 text-right font-mono text-acb-700">{fmt(r.pts)}</td>
                    <td className="px-3 py-2 text-right font-mono text-acb-700">{fmt(r.reb)}</td>
                    <td className="px-3 py-2 text-right font-mono text-acb-700">{fmt(r.ast)}</td>
                    <td className="px-3 py-2 text-right font-mono text-acb-700">{fmt(r.stl)}</td>
                    <td className="px-3 py-2 text-right font-mono text-acb-700">{fmt(r.blk)}</td>
                    <td className="px-3 py-2 text-right font-mono text-acb-700">{fmt(r.tov)}</td>
                    <td className="px-3 py-2 text-right font-mono text-acb-700">{fmt(r.fg2Pct, true)}</td>
                    <td className="px-3 py-2 text-right font-mono text-acb-700">{fmt(r.fg3Pct, true)}</td>
                    <td className="px-3 py-2 text-right font-mono text-acb-700">{fmt(r.ftPct, true)}</td>
                    <td className="px-3 py-2 text-right font-mono text-acb-700">{fmt(r.efgPct, true)}</td>
                    <td className="px-3 py-2 text-right font-mono text-acb-700">{fmt(tsPct, true)}</td>
                    <td className="px-3 py-2 text-right font-mono text-acb-700">{fmt(fg3Rate, true)}</td>
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
export default function PlayerProfile({ players, allPlayers = players, playerPhotos = {}, playerBio = {}, loadLineupsForSeason, lineupsCache, loadingLineups, loadClutchForSeason, clutchCache, loadingClutch }) {
  const { player: urlPlayer } = useParams()
  const location = useLocation()
  const navigate = useNavigate()
  const [searchParams] = useSearchParams()
  const parsedPlayer = parsePlayerSegment(urlPlayer)
  const parsedPlayerId = parsedPlayer?.id || null
  const slugLicenseIds = useMemo(() => {
    if (!urlPlayer || parsedPlayerId) return []
    return [...new Set(allPlayers
      .filter(player => getPlayerProfileSlug(player, getPlayerDisplayName(player)) === urlPlayer)
      .map(player => String(player.licenseId)))]
  }, [allPlayers, parsedPlayerId, urlPlayer])
  const selectedLicenseId = parsedPlayerId || (slugLicenseIds.length === 1 ? slugLicenseIds[0] : null)
  const search = searchParams.toString()
  const query = useMemo(() => parseRouteQuery('playerProfile', search), [search])
  const selectedStage = query.values.fase || 'regular'
  const careerTab = query.values.tabla === 'avanzado' ? 'advanced' : 'basic'
  const radarReference = query.values.radar || 'league'
  const percentileReference = query.values.percentiles || 'league'
  const shootingView = query.values.tiro || 'own'

  const scopedPlayers = useMemo(() => {
    return players.filter(p => (p.competitionStage || 'regular') === selectedStage)
  }, [players, selectedStage])

  // all records for the selected player, newest first
  const playerRecords = useMemo(() => {
    if (selectedLicenseId == null) return []
    return scopedPlayers
      .filter(p => String(p.licenseId) === String(selectedLicenseId))
      .sort((a, b) => b.season - a.season)
  }, [scopedPlayers, selectedLicenseId])

  const allPlayerRecords = useMemo(() => {
    if (selectedLicenseId == null) return []
    return allPlayers
      .filter(p => String(p.licenseId) === String(selectedLicenseId))
      .sort((a, b) => b.season - a.season)
  }, [allPlayers, selectedLicenseId])

  const requestedSeasonRecords = useMemo(() => {
    if (query.values.temporada == null) return []
    return playerRecords.filter(record => Number(record.season) === Number(query.values.temporada))
  }, [playerRecords, query.values.temporada])
  const hasAmbiguousSeason = query.values.equipo == null && requestedSeasonRecords.length > 1

  const seasonRecord = useMemo(() => {
    if (playerRecords.length === 0) return null
    const requestedSeason = query.values.temporada
    const requestedTeamId = query.values.equipo
    if (requestedSeason == null && requestedTeamId == null) return playerRecords[0]
    const matches = playerRecords.filter(record => (
      (requestedSeason == null || Number(record.season) === Number(requestedSeason))
      && (requestedTeamId == null || record.teamId === requestedTeamId)
    ))
    if (requestedSeason != null && requestedTeamId == null && matches.length !== 1) return null
    return matches[0] || null
  }, [playerRecords, query.values.equipo, query.values.temporada])

  const archetypeRecord = useMemo(() => {
    return getSeasonArchetypePlayer(seasonRecord, allPlayerRecords)
  }, [allPlayerRecords, seasonRecord])

  const selectedSeason = seasonRecord?.season ?? null
  const defaultRecord = playerRecords[0] || null
  const isDefaultRecord = Boolean(seasonRecord && defaultRecord
    && Number(seasonRecord.season) === Number(defaultRecord.season)
    && seasonRecord.teamId === defaultRecord.teamId)

  const currentValues = useMemo(() => ({
    ...query.values,
    fase: selectedStage,
    tabla: careerTab === 'advanced' ? 'avanzado' : 'basico',
    radar: radarReference,
    percentiles: percentileReference,
    tiro: shootingView,
    ...(seasonRecord
      ? isDefaultRecord
        ? { temporada: undefined, equipo: undefined }
        : { temporada: seasonRecord.season, equipo: seasonRecord.teamId }
      : {}),
  }), [careerTab, isDefaultRecord, percentileReference, query.values, radarReference, seasonRecord, selectedStage, shootingView])

  useEffect(() => {
    const canonicalRecord = seasonRecord || allPlayerRecords[0]
    if (!canonicalRecord || !urlPlayer) return
    const canonicalPath = buildPlayerProfilePath(canonicalRecord)
    const canonicalSearch = serializeRouteQuery('playerProfile', currentValues, { strict: false })
    const target = withQuery(canonicalPath, canonicalSearch)
    const current = withQuery(location.pathname, search)
    if (target !== current) navigate(target, { replace: true })
  }, [allPlayerRecords, currentValues, location.pathname, navigate, search, seasonRecord, urlPlayer])

  const updateRoute = (updates) => {
    const record = seasonRecord || allPlayerRecords[0]
    if (!record) return
    const values = { ...currentValues, ...updates }
    navigate(withQuery(
      buildPlayerProfilePath(record),
      serializeRouteQuery('playerProfile', values, { strict: false })
    ))
  }

  const selectPlayer = (id) => {
    const records = players
      .filter(record => String(record.licenseId) === String(id) && (record.competitionStage || 'regular') === 'regular')
      .sort((a, b) => b.season - a.season)
    const record = records[0] || allPlayers.find(candidate => String(candidate.licenseId) === String(id))
    if (!record) return
    const values = {
      fase: 'regular',
      tabla: 'basico',
      radar: 'league',
      percentiles: 'league',
      tiro: 'own',
    }
    navigate(withQuery(
      buildPlayerProfilePath(record),
      serializeRouteQuery('playerProfile', values, { strict: false })
    ))
  }

  // Bio: prefer fields embedded in the player record (from re-exported players.json),
  // fall back to the separate playerBio lookup for backwards compatibility.
  // Sanitize every value to a primitive — R's write_json can emit {} for some edge cases.
  const bio = useMemo(() => {
    if (!allPlayerRecords.length) return null
    const str = v => (typeof v === 'string' && v) ? v : null
    const num = v => { const n = parseFloat(v); return isFinite(n) ? n : null }
    const latest = allPlayerRecords[0]
    const ext = playerBio[String(selectedLicenseId)] || {}
    return {
      position:  str(latest.position)  ?? str(ext.position),
      heightM:   num(latest.heightM)   ?? num(ext.heightM),
      birthDate: str(latest.birthDate) ?? str(ext.birthDate),
    }
  }, [allPlayerRecords, playerBio, selectedLicenseId])

  return (
    <div className="app-page space-y-6">
      <PageHeader
        title="Perfil de jugador"
        subtitle="Selecciona un jugador para ver sus estadísticas históricas, estilo de juego y arquetipo"
      />

      <PlayerSelector
        players={players}
        onSelect={selectPlayer}
        selectedLicenseId={selectedLicenseId}
      />

      {selectedLicenseId && allPlayerRecords.length > 0 && (
        <>
          {/* Player Header */}
          <PlayerHeader
            records={allPlayerRecords}
            photoUrl={getPlayerPhoto(playerPhotos, selectedLicenseId, selectedSeason || allPlayerRecords[0]?.season)}
            bio={bio}
            selectedSeason={selectedSeason || allPlayerRecords[0]?.season}
            selectedRecord={seasonRecord}
          />

          <div className="flex items-center gap-2 flex-wrap">
            <span className="field-label">Estadísticas</span>
            <div className="segmented-control">
              <button
                type="button"
                onClick={() => updateRoute({ fase: 'regular' })}
                aria-pressed={selectedStage === 'regular'}
                className="segmented-option"
              >
                Temporada regular
              </button>
              <button
                type="button"
                onClick={() => updateRoute({ fase: 'playoffs' })}
                aria-pressed={selectedStage === 'playoffs'}
                className="segmented-option"
              >
                Playoffs
              </button>
            </div>
          </div>

          {playerRecords.length > 0 ? (
            <>
          {/* Find Similar Players button */}
          {selectedSeason && selectedStage === 'regular' && (
            <button
              type="button"
              onClick={() => navigate(withQuery(
                buildPlayerSimilarityPath(seasonRecord),
                serializeRouteQuery('playerSimilarity', {
                  temporada: selectedSeason,
                  equipo: seasonRecord.teamId,
                }, { strict: false })
              ))}
              className="inline-flex items-center gap-2 px-4 py-2 text-sm font-medium text-accent-700 bg-accent-50 border border-accent-200 rounded-lg hover:bg-accent-100 transition-colors"
            >
              <GitCompareArrows className="w-4 h-4" />
              Buscar jugadores similares
              <span className="text-accent-500">→</span>
            </button>
          )}

          {/* Career Overview */}
          <CareerTable
            records={playerRecords}
            archetypeRecords={allPlayerRecords}
            bio={bio}
            tab={careerTab}
            onTabChange={tab => updateRoute({ tabla: tab === 'advanced' ? 'avanzado' : 'basico' })}
          />

          {/* Season picker for detail cards */}
          {playerRecords.length > 0 && (
            <div className="flex flex-col items-stretch gap-1.5 sm:flex-row sm:items-center sm:gap-3">
              <label htmlFor="player-profile-record" className="field-label">Temporada y equipo</label>
              <SeasonPicker
                records={playerRecords}
                selected={seasonRecord ? playerRecordKey(seasonRecord) : ''}
                onChange={(record) => {
                  if (!record) return
                  updateRoute({ temporada: record.season, equipo: record.teamId })
                }}
              />
            </div>
          )}

          {seasonRecord && (
            <>
              <RadarArchetypeCard
                player={seasonRecord}
                archetypePlayer={archetypeRecord}
                bio={bio}
                reference={radarReference}
                onReferenceChange={reference => updateRoute({ radar: reference })}
              />
              <PercentileProfile
                player={seasonRecord}
                reference={percentileReference}
                onReferenceChange={reference => updateRoute({ percentiles: reference })}
              />
            </>
          )}

          {/* Shooting Stats */}
          {seasonRecord && (
            <ShootingStatsCard
              player={seasonRecord}
              shotTab={shootingView}
              onShotTabChange={view => updateRoute({ tiro: view })}
            />
          )}

          {!seasonRecord && (
            <div className="text-center py-8 text-acb-500">
              {hasAmbiguousSeason
                ? 'Este jugador estuvo en más de un equipo esa temporada. Selecciona la temporada y el equipo correctos.'
                : 'No hay datos para la temporada y el equipo indicados en el enlace.'}
            </div>
          )}

          {/* On/Off Impact */}
          <OnOffCard
            records={allPlayerRecords}
            loadLineupsForSeason={loadLineupsForSeason}
            lineupsCache={lineupsCache}
            loadingLineups={loadingLineups}
          />

          {/* Clutch Stats */}
          {loadClutchForSeason && (
            <ClutchCard
              records={allPlayerRecords}
              loadClutchForSeason={loadClutchForSeason}
              clutchCache={clutchCache}
              loadingClutch={loadingClutch}
            />
          )}
            </>
          ) : (
            <div className="text-center py-12 text-acb-500">
              No hay datos de {selectedStage === 'playoffs' ? 'playoffs' : 'temporada regular'} para este jugador.
            </div>
          )}
        </>
      )}

      {selectedLicenseId && allPlayerRecords.length === 0 && (
        <div className="text-center py-12 text-acb-500">
          No se encontraron datos para este jugador.
        </div>
      )}
    </div>
  )
}
