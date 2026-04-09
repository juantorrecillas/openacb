import { useState, useMemo, useEffect } from 'react'
import { ArrowUp, ArrowDown, Search } from 'lucide-react'
import { useNavigate } from 'react-router-dom'

function seasonLabel(s) {
  return `${s - 1}-${String(s).slice(-2)}`
}

function fmtVal(v, key) {
  if (v == null) return '-'
  if (typeof v === 'string') return v
  if (isNaN(v)) return '-'
  if (key === 'games') return String(v)
  if (['fg2Pct','fg3Pct','ftPct','efgPct','tsPct','fg3Rate'].includes(key))
    return `${Number(v).toFixed(1)}%`
  return Number(v).toFixed(1)
}

function getRankColor(rank, total) {
  if (!rank || !total) return 'bg-acb-100 text-acb-600'
  const pct = rank / total
  if (pct <= 0.25) return 'bg-positive-100 text-positive-700'
  if (pct <= 0.5)  return 'bg-info-100 text-info-700'
  if (pct <= 0.75) return 'bg-info-100 text-info-600'
  return 'bg-negative-100 text-negative-700'
}

// ─── Column sets ───────────────────────────────────────────────
const basicCols = [
  { key: 'playerName', label: 'Jugador', left: true,  rank: false },
  { key: 'team',       label: 'Equipo',  left: true,  rank: false },
  { key: 'games',      label: 'PJ',      left: false, rank: false },
  { key: 'pts',        label: 'Pts',     left: false, rank: true, inverse: false },
  { key: 'reb',        label: 'Reb',     left: false, rank: true, inverse: false },
  { key: 'ast',        label: 'Ast',     left: false, rank: true, inverse: false },
  { key: 'stl',        label: 'Rob',     left: false, rank: true, inverse: false },
  { key: 'blk',        label: 'Tap',     left: false, rank: true, inverse: false },
  { key: 'tov',        label: 'Pér',     left: false, rank: true, inverse: true  },
]

const efficiencyCols = [
  { key: 'playerName', label: 'Jugador', left: true,  rank: false },
  { key: 'team',       label: 'Equipo',  left: true,  rank: false },
  { key: 'games',      label: 'PJ',      left: false, rank: false },
  { key: 'efgPct',     label: 'eFG%',    left: false, rank: true, inverse: false },
  { key: 'tsPct',      label: 'TS%',     left: false, rank: true, inverse: false },
  { key: 'fg2Pct',     label: 'T2%',     left: false, rank: true, inverse: false },
  { key: 'fg3Pct',     label: '3P%',     left: false, rank: true, inverse: false },
  { key: 'ftPct',      label: 'TL%',     left: false, rank: true, inverse: false },
  { key: 'fg3Rate',    label: '3PAr',    left: false, rank: false },
  { key: 'fg2Apg',     label: 'T2Int/G', left: false, rank: false },
  { key: 'fg3Apg',     label: '3PInt/G', left: false, rank: false },
  { key: 'ftApg',      label: 'TLInt/G', left: false, rank: false },
]

export default function ClutchStats({ teams, loadClutchForSeason, clutchCache, loadingClutch }) {
  const navigate = useNavigate()

  const availableSeasons = useMemo(
    () => [...new Set(teams.map(t => t.season))].sort((a, b) => b - a),
    [teams]
  )

  const [selectedSeason, setSelectedSeason] = useState(availableSeasons[0] || 2026)
  const [minGames, setMinGames]             = useState(3)
  const [sortKey, setSortKey]               = useState('pts')
  const [sortDir, setSortDir]               = useState('desc')
  const [search, setSearch]                 = useState('')
  const [teamFilter, setTeamFilter]         = useState('')
  const [viewMode, setViewMode]             = useState('basic') // 'basic' | 'efficiency'

  useEffect(() => {
    if (selectedSeason) loadClutchForSeason(selectedSeason)
  }, [selectedSeason, loadClutchForSeason])

  const isLoading = loadingClutch[selectedSeason] || false
  const rawPlayers = useMemo(() => clutchCache[selectedSeason]?.players || [], [clutchCache, selectedSeason])

  const allTeams = useMemo(
    () => [...new Set(rawPlayers.map(p => p.team))].filter(Boolean).sort(),
    [rawPlayers]
  )

  // Enrich with derived stats
  const enriched = useMemo(() => {
    const num = v => (v == null || v === 'NA') ? null : Number(v)
    return rawPlayers.map(p => {
      const fg2Pct = num(p.fg2Pct)
      const fg3Pct = num(p.fg3Pct)
      const ftPct  = num(p.ftPct)
      const efgPct = num(p.efgPct)
      const fga    = (p.fg2A || 0) + (p.fg3A || 0)
      const totalPts = (p.pts || 0) * (p.games || 1)
      const tsPct  = fga + (p.ftA || 0) > 0
        ? Math.round(totalPts / (2 * (fga + 0.44 * (p.ftA || 0))) * 1000) / 10
        : null
      const fg3Rate = fga > 0 ? Math.round((p.fg3A || 0) / fga * 1000) / 10 : null
      const fg2Apg  = p.games > 0 ? Math.round((p.fg2A || 0) / p.games * 10) / 10 : null
      const fg3Apg  = p.games > 0 ? Math.round((p.fg3A || 0) / p.games * 10) / 10 : null
      const ftApg   = p.games > 0 ? Math.round((p.ftA  || 0) / p.games * 10) / 10 : null
      return { ...p, playerName: p.nick, fg2Pct, fg3Pct, ftPct, efgPct, tsPct, fg3Rate, fg2Apg, fg3Apg, ftApg }
    })
  }, [rawPlayers])

  // Filter
  const filtered = useMemo(() => {
    return enriched.filter(p => {
      if ((p.games || 0) < minGames) return false
      if (teamFilter && p.team !== teamFilter) return false
      if (search) {
        const q = search.toLowerCase()
        if (!p.nick?.toLowerCase().includes(q) && !p.team?.toLowerCase().includes(q)) return false
      }
      return true
    })
  }, [enriched, minGames, teamFilter, search])

  // Ranks within filtered set (for rank badges)
  const rankKeys = ['pts','reb','ast','stl','blk','efgPct','tsPct','fg2Pct','fg3Pct','ftPct']
  const withRanks = useMemo(() => {
    const copy = filtered.map(p => ({ ...p }))
    rankKeys.forEach(key => {
      const sorted = [...copy]
        .filter(p => p[key] != null)
        .sort((a, b) => (b[key] || 0) - (a[key] || 0))
      sorted.forEach((p, i) => {
        const orig = copy.find(x => x.licenseId === p.licenseId && x.team === p.team)
        if (orig) orig[`${key}Rank`] = i + 1
      })
    })
    // tov: lower is better → rank ascending
    const tovSorted = [...copy].filter(p => p.tov != null).sort((a, b) => (a.tov || 0) - (b.tov || 0))
    tovSorted.forEach((p, i) => {
      const orig = copy.find(x => x.licenseId === p.licenseId && x.team === p.team)
      if (orig) orig.tovRank = i + 1
    })
    return copy
  }, [filtered])

  // Sort
  const sorted = useMemo(() => {
    return [...withRanks].sort((a, b) => {
      const av = a[sortKey] ?? (sortDir === 'desc' ? -Infinity : Infinity)
      const bv = b[sortKey] ?? (sortDir === 'desc' ? -Infinity : Infinity)
      if (typeof av === 'string') return sortDir === 'desc' ? bv.localeCompare(av) : av.localeCompare(bv)
      return sortDir === 'desc' ? bv - av : av - bv
    })
  }, [withRanks, sortKey, sortDir])

  const cols = viewMode === 'basic' ? basicCols : efficiencyCols
  const n = sorted.length

  const handleSort = (key) => {
    if (sortKey === key) setSortDir(d => d === 'desc' ? 'asc' : 'desc')
    else { setSortKey(key); setSortDir('desc') }
  }

  return (
    <div className="space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-semibold text-acb-900">Estadísticas Clutch</h2>
        <p className="text-acb-500 text-sm mt-1">
          Últimos 5 minutos del 4º cuarto o prórroga con diferencia de puntos ≤ 5
        </p>
      </div>

      {/* Controls */}
      <div className="bg-white rounded-lg border border-acb-200 p-4">
        <div className="flex flex-wrap items-center gap-4 mb-4">
          {/* Season */}
          <div className="flex items-center gap-2">
            <span className="text-sm text-acb-600">Temporada:</span>
            <select
              value={selectedSeason}
              onChange={e => setSelectedSeason(Number(e.target.value))}
              className="px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
            >
              {availableSeasons.map(s => <option key={s} value={s}>{seasonLabel(s)}</option>)}
            </select>
          </div>

          {/* View mode */}
          <div className="flex items-center gap-1 bg-acb-100 rounded-md p-1">
            {[['basic','Básico'],['efficiency','Eficiencia']].map(([mode, label]) => (
              <button
                key={mode}
                onClick={() => { setViewMode(mode); setSortKey(mode === 'basic' ? 'pts' : 'efgPct'); setSortDir('desc') }}
                className={`px-3 py-1.5 text-sm font-medium rounded transition-colors ${
                  viewMode === mode ? 'bg-white text-acb-900 shadow-sm' : 'text-acb-600 hover:text-acb-900'
                }`}
              >{label}</button>
            ))}
          </div>

          {/* Team filter */}
          <select
            value={teamFilter}
            onChange={e => setTeamFilter(e.target.value)}
            className="px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
          >
            <option value="">Todos los equipos</option>
            {allTeams.map(t => <option key={t} value={t}>{t}</option>)}
          </select>

          {/* Min games */}
          <div className="flex items-center gap-2">
            <span className="text-sm text-acb-600">Mín. PJ clutch:</span>
            <select
              value={minGames}
              onChange={e => setMinGames(Number(e.target.value))}
              className="px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
            >
              {[1,3,5,10].map(v => <option key={v} value={v}>{v}+</option>)}
            </select>
          </div>
        </div>

        {/* Search */}
        <div className="relative max-w-xs">
          <Search className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-acb-400" />
          <input
            type="text"
            value={search}
            onChange={e => setSearch(e.target.value)}
            placeholder="Buscar jugador..."
            className="w-full pl-9 pr-4 py-2 border border-acb-200 rounded-md text-sm focus:outline-none focus:ring-2 focus:ring-accent-300"
          />
        </div>
      </div>

      <div className="text-sm text-acb-500">
        Mostrando {n} jugador{n !== 1 ? 'es' : ''} · Mín. {minGames} partido{minGames !== 1 ? 's' : ''} clutch · {seasonLabel(selectedSeason)}
      </div>

      {/* Table */}
      <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
        <div className="overflow-x-auto">
          <table className="min-w-full">
            <thead>
              <tr className="bg-acb-50 border-b border-acb-200">
                <th className="px-4 py-3 text-left text-xs font-semibold text-acb-600 uppercase tracking-wider w-10">#</th>
                {cols.map(col => (
                  <th
                    key={col.key}
                    onClick={() => col.key !== 'playerName' && col.key !== 'team' && handleSort(col.key)}
                    className={`px-4 py-3 text-xs font-semibold text-acb-600 uppercase tracking-wider whitespace-nowrap
                      ${col.left ? 'text-left' : 'text-right'}
                      ${col.key !== 'playerName' && col.key !== 'team' ? 'cursor-pointer hover:bg-acb-100' : ''}
                      ${sortKey === col.key ? 'bg-acb-100' : ''}`}
                  >
                    <span className="inline-flex items-center gap-1">
                      {col.label}
                      {sortKey === col.key && (
                        sortDir === 'desc'
                          ? <ArrowDown className="w-3 h-3" />
                          : <ArrowUp className="w-3 h-3" />
                      )}
                    </span>
                  </th>
                ))}
              </tr>
            </thead>
            <tbody>
              {isLoading ? (
                <tr><td colSpan={cols.length + 1} className="py-12 text-center text-acb-400">Cargando datos...</td></tr>
              ) : sorted.length === 0 ? (
                <tr><td colSpan={cols.length + 1} className="py-12 text-center text-acb-400">No hay jugadores con {minGames}+ partidos clutch.</td></tr>
              ) : sorted.map((p, i) => (
                <tr
                  key={`${p.licenseId}-${p.team}`}
                  onClick={() => p.licenseId && navigate(`/jugador/${p.licenseId}`)}
                  className="border-b border-acb-100 hover:bg-acb-50 transition-colors cursor-pointer"
                >
                  <td className="px-4 py-3 text-sm text-acb-400 font-mono">{i + 1}</td>
                  {cols.map(col => {
                    const v = p[col.key]
                    const rankKey = col.inverse ? 'tovRank' : col.rank ? `${col.key}Rank` : null
                    const rank = rankKey ? p[rankKey] : null

                    return (
                      <td
                        key={col.key}
                        className={`px-4 py-3 text-sm whitespace-nowrap
                          ${col.left ? '' : 'text-right'}
                          ${col.key === 'playerName' ? 'font-medium text-acb-900' : ''}
                          ${col.key === 'team' ? 'text-acb-600' : ''}`}
                      >
                        {rank != null ? (
                          <div className="flex flex-col items-end gap-1">
                            <span className="font-mono text-acb-700">{fmtVal(v, col.key)}</span>
                            <span className={`text-xs px-1.5 py-0.5 rounded ${getRankColor(rank, n)}`}>
                              #{rank}
                            </span>
                          </div>
                        ) : (
                          <span className={col.left ? 'text-acb-700' : 'font-mono text-acb-700'}>
                            {fmtVal(v, col.key)}
                          </span>
                        )}
                      </td>
                    )
                  })}
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>
    </div>
  )
}
