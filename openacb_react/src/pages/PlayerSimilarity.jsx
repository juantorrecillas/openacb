import { useState, useMemo, useEffect, useRef } from 'react'
import { useParams, useNavigate } from 'react-router-dom'
import { Search, Filter, ArrowRight } from 'lucide-react'
import { statTitle } from '../utils/statLabels'
import { getPlayerDisplayName, getPlayerSearchText } from '../utils/playerNames'


function seasonLabel(s) {
  return `${s - 1}-${String(s).slice(-2)}`
}

// ─── Player Selector (reused pattern from PlayerProfile) ────────────────
function PlayerSelector({ players, onSelect, selectedLicenseId }) {
  const [query, setQuery] = useState('')
  const [open, setOpen] = useState(false)
  const [teamFilter, setTeamFilter] = useState('')
  const [seasonFilter, setSeasonFilter] = useState('')
  const ref = useRef(null)

  useEffect(() => {
    const handler = (e) => { if (ref.current && !ref.current.contains(e.target)) setOpen(false) }
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
      const key = p.licenseId
      if (!map.has(key)) {
        map.set(key, {
          licenseId: key,
          name: getPlayerDisplayName(p),
          abbrev: p.playerAbbrev,
          searchText: getPlayerSearchText(p),
          team: p.team,
          season: p.season,
        })
      }
    })
    return [...map.values()].sort((a, b) =>
      (a.name || a.abbrev || '').localeCompare(b.name || b.abbrev || '', 'es')
    )
  }, [players, teamFilter, seasonFilter])

  const filtered = useMemo(() => {
    if (!query.trim()) return uniquePlayers.slice(0, 50)
    const q = query.toLowerCase()
    return uniquePlayers.filter(p =>
      p.searchText.includes(q)
    ).slice(0, 50)
  }, [uniquePlayers, query])

  return (
    <div className="flex flex-wrap items-end gap-3">
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
                onClick={() => { onSelect(p.licenseId); setQuery(p.name || p.abbrev); setOpen(false) }}
                className={`px-4 py-2 text-sm cursor-pointer hover:bg-accent-50 flex items-center justify-between ${
                  String(selectedLicenseId) === String(p.licenseId) ? 'bg-accent-50 font-medium' : ''
                }`}
              >
                <span>{p.name || p.abbrev}</span>
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

// ─── Score bar ────────────────────────────────────────────────
function ScoreBar({ score }) {
  const color = score >= 90 ? 'bg-positive-500' : score >= 80 ? 'bg-info-500' : score >= 70 ? 'bg-info-400' : 'bg-acb-300'
  return (
    <div className="flex items-center gap-2">
      <div className="w-20 h-2 bg-acb-100 rounded-full overflow-hidden">
        <div className={`h-full rounded-full ${color}`} style={{ width: `${score}%` }} />
      </div>
      <span className="font-mono text-sm text-acb-900 w-12 text-right">{score.toFixed(1)}</span>
    </div>
  )
}

// ─── Similarity stat definitions (13 features used in similarity vector) ──
const SIMILARITY_STATS = [
  { key: 'mpg',         label: 'MPP',    fmt: v => v?.toFixed(1) ?? '-' },
  { key: 'ts',          label: 'TS%',    fmt: v => v != null ? `${v.toFixed(1)}%` : '-' },
  { key: 'usg',         label: 'USG%',   fmt: v => v != null ? `${v.toFixed(1)}%` : '-' },
  { key: 'astPct',      label: 'AST%',   fmt: v => v != null ? `${v.toFixed(1)}%` : '-' },
  { key: 'tovPct',      label: 'PER%',   fmt: v => v != null ? `${v.toFixed(1)}%` : '-' },
  { key: 'orbPct',      label: 'RO%',    fmt: v => v != null ? `${v.toFixed(1)}%` : '-' },
  { key: 'drbPct',      label: 'RD%',    fmt: v => v != null ? `${v.toFixed(1)}%` : '-' },
  { key: 'stlPct',      label: 'ROB%',   fmt: v => v != null ? `${v.toFixed(1)}%` : '-' },
  { key: 'blkPct',      label: 'TAP%',   fmt: v => v != null ? `${v.toFixed(1)}%` : '-' },
  { key: 'threeRate',   label: '3PAr',   fmt: v => v != null ? `${v.toFixed(1)}%` : '-' },
  { key: 'freqRim',     label: 'Freq Rim',  fmt: v => v != null ? `${v.toFixed(1)}%` : '-' },
  { key: 'freqAllMid',  label: 'Freq Mid',  fmt: v => v != null ? `${v.toFixed(1)}%` : '-' },
  { key: 'assistedFgm', label: '% Asist.',  fmt: v => v != null ? `${(v * 100).toFixed(1)}%` : '-' },
]

// Compute mean & std for each stat across qualified players (games>=10, mpg>=10)
function computeStatDistributions(players) {
  const qualified = players.filter(p => p.games >= 10 && p.mpg >= 10)
  const stats = {}
  for (const { key } of SIMILARITY_STATS) {
    const vals = qualified.map(p => p[key]).filter(v => v != null && !isNaN(v))
    const n = vals.length
    if (n === 0) { stats[key] = { mean: 0, std: 1 }; continue }
    const mean = vals.reduce((a, b) => a + b, 0) / n
    const variance = vals.reduce((a, b) => a + (b - mean) ** 2, 0) / n
    stats[key] = { mean, std: Math.sqrt(variance) || 1 }
  }
  return stats
}

// Pick top N stats by absolute z-score for a player
function getStandoutStats(player, distributions, n = 5) {
  const scored = SIMILARITY_STATS.map(stat => {
    const val = player[stat.key]
    if (val == null || isNaN(val)) return { ...stat, z: -Infinity }
    const z = (val - distributions[stat.key].mean) / distributions[stat.key].std
    return { ...stat, z }
  })
  return scored.filter(s => s.z > 0).sort((a, b) => b.z - a.z).slice(0, n)
}

// ─── Main Page ─────────────────────────────────────────────────
export default function PlayerSimilarity({ players, similarity }) {
  const { licenseId: urlLicenseId, season: urlSeason } = useParams()
  const navigate = useNavigate()
  const [selectedLicenseId, setSelectedLicenseId] = useState(null)
  const [selectedSeason, setSelectedSeason] = useState(null)

  // Sync from URL params when navigating via /similitud/:licenseId/:season
  useEffect(() => {
    if (urlLicenseId) {
      const parsed = Number(urlLicenseId)
      setSelectedLicenseId(isNaN(parsed) ? urlLicenseId : parsed)
      if (urlSeason) setSelectedSeason(Number(urlSeason))
    }
  }, [urlLicenseId, urlSeason])

  // Build lookup: players by licenseId_season
  const playerLookup = useMemo(() => {
    const map = new Map()
    players.forEach(p => {
      const key = `${p.licenseId}_${p.season}`
      if (!map.has(key)) map.set(key, p)
    })
    return map
  }, [players])

  // Compute stat distributions once across all qualified players
  const distributions = useMemo(() => computeStatDistributions(players), [players])

  // Available seasons for the selected player
  const playerRecords = useMemo(() => {
    if (!selectedLicenseId) return []
    return players
      .filter(p => String(p.licenseId) === String(selectedLicenseId))
      .sort((a, b) => b.season - a.season)
  }, [players, selectedLicenseId])

  const availableSeasons = useMemo(() => {
    return playerRecords.map(r => r.season)
  }, [playerRecords])

  // Default to latest season when player changes (unless URL provided a season)
  useEffect(() => {
    if (urlSeason) return // don't override if navigated with target season in URL
    if (availableSeasons.length > 0 && !availableSeasons.includes(selectedSeason)) {
      setSelectedSeason(availableSeasons[0])
    }
  }, [selectedLicenseId, availableSeasons])

  // Current player record
  const currentRecord = useMemo(() => {
    if (!selectedLicenseId || !selectedSeason) return null
    return playerRecords.find(r => r.season === selectedSeason) || null
  }, [playerRecords, selectedSeason])

  // Standout stats for the selected player (top 5 by |z-score|)
  const standoutCols = useMemo(() => {
    if (!currentRecord) return []
    return getStandoutStats(currentRecord, distributions, 5)
  }, [currentRecord, distributions])

  // Find similarity entry
  const similarityEntry = useMemo(() => {
    if (!selectedLicenseId || !selectedSeason) return null
    const id = `${selectedLicenseId}_${selectedSeason}`
    return similarity.find(s => s.id === id) || null
  }, [similarity, selectedLicenseId, selectedSeason])

  // Resolve similar players to full records
  const similarPlayers = useMemo(() => {
    if (!similarityEntry?.similar) return []
    return similarityEntry.similar.map((s, idx) => {
      const key = `${s.licenseId}_${s.season}`
      const player = playerLookup.get(key)
      return {
        rank: idx + 1,
        licenseId: s.licenseId,
        season: s.season,
        score: s.score,
        name: getPlayerDisplayName(player, `ID ${s.licenseId}`),
        team: player?.team || '-',
        _record: player, // keep full record for dynamic stat access
      }
    })
  }, [similarityEntry, playerLookup])

  const handlePlayerSelect = (licenseId) => {
    setSelectedLicenseId(licenseId)
    setSelectedSeason(null) // will auto-pick latest
  }

  return (
    <div className="app-page space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-semibold text-acb-900">Similitud de Jugadores</h2>
        <p className="text-acb-500 text-sm mt-1">
          Encuentra jugadores con perfiles estadísticos similares a lo largo de todas las temporadas
        </p>
      </div>

      {/* Player Selector */}
      <PlayerSelector
        players={players}
        onSelect={handlePlayerSelect}
        selectedLicenseId={selectedLicenseId}
      />

      {/* Season picker */}
      {selectedLicenseId && availableSeasons.length > 0 && (
        <div className="flex items-center gap-3">
          <span className="text-sm font-medium text-acb-700">Temporada:</span>
          <select
            value={selectedSeason || ''}
            onChange={e => setSelectedSeason(Number(e.target.value))}
            className="px-3 py-1.5 border border-acb-200 rounded-md text-sm bg-white"
          >
            {availableSeasons.map(s => (
              <option key={s} value={s}>{seasonLabel(s)}</option>
            ))}
          </select>
        </div>
      )}

      {/* Selected player info + results */}
      {currentRecord && standoutCols.length > 0 && (
        <div className="bg-white rounded-lg border border-acb-200 p-4">
          <div className="flex flex-col sm:flex-row sm:items-center justify-between gap-3">
            <div>
              <h3 className="font-bold text-acb-900 text-lg">{getPlayerDisplayName(currentRecord)}</h3>
              <p className="text-sm text-acb-500">{currentRecord.team} - {seasonLabel(currentRecord.season)} - {currentRecord.games} partidos</p>
            </div>
            <div className="flex gap-4 text-xs text-acb-500">
              {standoutCols.map(col => (
                <div key={col.key} className="text-center">
                  <div className="font-mono text-acb-900 text-sm">{col.fmt(currentRecord[col.key])}</div>
                  <div className="text-accent-600 font-medium">{col.label}</div>
                </div>
              ))}
            </div>
          </div>
          <p className="text-xs text-acb-400 mt-2">Estadísticas destacadas con respecto a la media de la liga</p>
        </div>
      )}

      {/* Results */}
      {currentRecord && standoutCols.length > 0 && similarPlayers.length > 0 && (
        <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
          <div className="px-5 py-3 border-b border-acb-200">
            <h3 className="font-semibold text-acb-900">20 perfiles más similares</h3>
          </div>
          <div className="overflow-x-auto">
            <table className="data-table">
              <thead>
                <tr className="border-b border-acb-200 bg-acb-50">
                  <th className="data-table-head data-table-number data-table-sticky data-table-sticky-head data-col-rank bg-acb-50">#</th>
                  <th className="data-table-head data-table-identity data-table-sticky-after-rank data-table-sticky-head data-col-player bg-acb-50">Jugador</th>
                  <th className="data-table-head text-left data-col-team">Equipo</th>
                  <th className="data-table-head text-left">Temp.</th>
                  {standoutCols.map(col => (
                    <th key={col.key} title={statTitle(col.label)} className="data-table-head data-table-number data-col-number text-accent-600">{col.label}</th>
                  ))}
                  <th className="data-table-head data-table-number">Similitud</th>
                  <th className="data-table-head w-8"></th>
                </tr>
              </thead>
              <tbody className="divide-y divide-acb-100">
                {similarPlayers.map(p => (
                  <tr
                    key={`${p.licenseId}_${p.season}`}
                    onClick={() => navigate(`/jugador/${p.licenseId}`)}
                    className="data-table-row cursor-pointer hover:bg-accent-50"
                  >
                    <td className="data-table-cell data-table-number data-table-sticky data-col-rank text-acb-400">{p.rank}</td>
                    <td className="data-table-cell data-table-identity data-table-sticky-after-rank data-col-player">{p.name}</td>
                    <td className="data-table-cell data-col-team text-acb-600">{p.team}</td>
                    <td className="data-table-cell text-acb-600">{seasonLabel(p.season)}</td>
                    {standoutCols.map(col => (
                      <td key={col.key} className="data-table-cell data-table-number data-col-number">
                        {col.fmt(p._record?.[col.key])}
                      </td>
                    ))}
                    <td className="data-table-cell text-right"><ScoreBar score={p.score} /></td>
                    <td className="data-table-cell text-acb-400"><ArrowRight className="w-4 h-4" /></td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      )}

      {selectedLicenseId && selectedSeason && !similarityEntry && (
        <div className="text-center py-12 text-acb-500">
          No se encontraron datos de similitud para esta temporada. El jugador puede no cumplir los requisitos mínimos (10 partidos, 10+ minutos por partido).
        </div>
      )}

      {!selectedLicenseId && (
        <div className="text-center py-12 text-acb-400">
          Selecciona un jugador para ver sus jugadores más similares
        </div>
      )}
    </div>
  )
}
