import { useState, useMemo, useEffect } from 'react'
import { ArrowUp, ArrowDown, Search, Filter } from 'lucide-react'
import { useNavigate } from 'react-router-dom'
import { statTitle } from '../utils/statLabels'
import { getPlayerDisplayName, getPlayerSearchText } from '../utils/playerNames'

function seasonLabel(s) {
  return `${s - 1}-${String(s).slice(-2)}`
}

function fmtVal(v, key) {
  if (v == null) return '-'
  if (typeof v === 'string') return v
  if (isNaN(v)) return '-'
  if (key === 'games' || key === 'wins' || key === 'losses') return String(v)
  if (['ptsT','rebT','orebT','drebT','astT','stlT','blkT','tovT','foulsT','fg3M','fgmTot','ftM'].includes(key))
    return String(Math.round(v))
  if (['fgPct','fg2Pct','fg3Pct','ftPct','efgPct','tsPct','fg3Rate'].includes(key))
    return `${Number(v).toFixed(1)}%`
  return Number(v).toFixed(1)
}

function getPercentileColor(pct) {
  if (pct == null || isNaN(pct)) return 'bg-acb-100 text-acb-600'
  if (pct >= 75) return 'bg-positive-100 text-positive-700'
  if (pct >= 50) return 'bg-info-100 text-info-700'
  if (pct >= 25) return 'bg-info-100 text-info-600'
  return 'bg-negative-100 text-negative-700'
}

// rank → percentile (rank 1 → 100, rank n → 0). undefined for n <= 1.
function rankToPercentile(rank, n) {
  if (rank == null || n == null || n <= 1) return null
  return Math.round(((n - rank) / (n - 1)) * 100)
}

const POSITION_ORDER = ['Base', 'Escolta', 'Alero', 'Ala-pívot', 'Pívot']
const DEFAULT_SHOW = 25

// ─── Column sets ───────────────────────────────────────────────
const basicCols = [
  { key: 'playerDisplay', label: 'Jugador', left: true,  rank: false },
  { key: 'team',          label: 'Equipo',  left: true,  rank: false },
  { key: 'position',      label: 'Pos',     left: true,  rank: false,  title: 'Posición' },
  { key: 'games',         label: 'PJ',      left: false, rank: false,  title: 'Partidos jugados' },
  { key: 'wins',          label: 'V',       left: false, rank: false,  title: 'Victorias' },
  { key: 'losses',        label: 'D',       left: false, rank: false,  title: 'Derrotas' },
  { key: 'clutchMin',     label: 'Min',     left: false, rank: false,  title: 'Minutos totales en clutch' },
  { key: 'clutchMpg',     label: 'MPP',     left: false, rank: false,  title: 'Minutos por partido' },
  { key: 'pts',           label: 'Pts',     left: false, rank: true,   inverse: false, title: 'Puntos por partido' },
  { key: 'fgmPg',         label: 'TCA',     left: false, rank: false,  title: 'Tiros de campo anotados por partido' },
  { key: 'fgaPg',         label: 'TCI',     left: false, rank: false,  title: 'Tiros de campo intentados por partido' },
  { key: 'fgPct',         label: 'TC%',     left: false, rank: true,   inverse: false, title: 'Porcentaje de tiro de campo' },
  { key: 'fg3mPg',        label: '3PA',     left: false, rank: false,  title: 'Triples anotados por partido' },
  { key: 'fg3aPg',        label: '3PI',     left: false, rank: false,  title: 'Triples intentados por partido' },
  { key: 'fg3Pct',        label: '3P%',     left: false, rank: true,   inverse: false, title: 'Porcentaje de triple' },
  { key: 'ftmPg',         label: 'TLA',     left: false, rank: false,  title: 'Tiros libres anotados por partido' },
  { key: 'ftaPg',         label: 'TLI',     left: false, rank: false,  title: 'Tiros libres intentados por partido' },
  { key: 'ftPct',         label: 'TL%',     left: false, rank: true,   inverse: false, title: 'Porcentaje de tiro libre' },
  { key: 'oreb',          label: 'RO',      left: false, rank: true,   inverse: false, title: 'Rebotes ofensivos por partido' },
  { key: 'dreb',          label: 'RD',      left: false, rank: true,   inverse: false, title: 'Rebotes defensivos por partido' },
  { key: 'reb',           label: 'Reb',     left: false, rank: true,   inverse: false, title: 'Rebotes totales por partido' },
  { key: 'ast',           label: 'Ast',     left: false, rank: true,   inverse: false, title: 'Asistencias por partido' },
  { key: 'stl',           label: 'Rob',     left: false, rank: true,   inverse: false, title: 'Robos por partido' },
  { key: 'blk',           label: 'Tap',     left: false, rank: true,   inverse: false, title: 'Tapones por partido' },
  { key: 'tov',           label: 'Pér',     left: false, rank: true,   inverse: true,  title: 'Pérdidas por partido' },
  { key: 'fouls',         label: 'Fal',     left: false, rank: true,   inverse: true,  title: 'Faltas por partido' },
]

const advancedCols = [
  { key: 'playerDisplay', label: 'Jugador',   left: true,  rank: false },
  { key: 'team',          label: 'Equipo',    left: true,  rank: false },
  { key: 'position',      label: 'Pos',       left: true,  rank: false,  title: 'Posición' },
  { key: 'games',         label: 'PJ',        left: false, rank: false,  title: 'Partidos jugados' },
  { key: 'efgPct',        label: 'eFG%',      left: false, rank: true,   inverse: false, title: 'Porcentaje de tiro efectivo' },
  { key: 'tsPct',         label: 'TS%',       left: false, rank: true,   inverse: false, title: 'Eficiencia global de tiro (True Shooting)' },
  { key: 'fg2Pct',        label: '2P%',       left: false, rank: true,   inverse: false, title: 'Porcentaje de tiro de dos puntos' },
  { key: 'fg3Pct',        label: '3P%',       left: false, rank: true,   inverse: false, title: 'Porcentaje de triple' },
  { key: 'ftPct',         label: 'TL%',       left: false, rank: true,   inverse: false, title: 'Porcentaje de tiro libre' },
  { key: 'fg3Rate',       label: '3PAr',      left: false, rank: false,  title: 'Tasa de triple (triples intentados / tiros intentados)' },
  { key: 'fg2Apg',        label: 'T2Int/G',   left: false, rank: false,  title: 'Intentos de dos puntos por partido' },
  { key: 'fg3Apg',        label: '3PInt/G',   left: false, rank: false,  title: 'Intentos de triple por partido' },
  { key: 'ftApg',         label: 'TLInt/G',   left: false, rank: false,  title: 'Intentos de tiro libre por partido' },
]

const absolutesCols = [
  { key: 'playerDisplay', label: 'Jugador',   left: true,  rank: false },
  { key: 'team',          label: 'Equipo',    left: true,  rank: false },
  { key: 'position',      label: 'Pos',       left: true,  rank: false,  title: 'Posición' },
  { key: 'games',         label: 'PJ',        left: false, rank: false,  title: 'Partidos jugados' },
  { key: 'clutchMin',     label: 'Min',       left: false, rank: false,  title: 'Minutos totales en clutch' },
  { key: 'ptsT',          label: 'Pts',       left: false, rank: true,   inverse: false, title: 'Puntos totales en clutch' },
  { key: 'orebT',         label: 'RO',        left: false, rank: true,   inverse: false, title: 'Rebotes ofensivos totales' },
  { key: 'drebT',         label: 'RD',        left: false, rank: true,   inverse: false, title: 'Rebotes defensivos totales' },
  { key: 'rebT',          label: 'Reb',       left: false, rank: true,   inverse: false, title: 'Rebotes totales' },
  { key: 'astT',          label: 'Ast',       left: false, rank: true,   inverse: false, title: 'Asistencias totales' },
  { key: 'stlT',          label: 'Rob',       left: false, rank: true,   inverse: false, title: 'Robos totales' },
  { key: 'blkT',          label: 'Tap',       left: false, rank: true,   inverse: false, title: 'Tapones totales' },
  { key: 'tovT',          label: 'Pér',       left: false, rank: true,   inverse: true,  title: 'Pérdidas totales' },
  { key: 'foulsT',        label: 'Fal',       left: false, rank: true,   inverse: true,  title: 'Faltas totales' },
  { key: 'fg3M',          label: '3PM',       left: false, rank: false,  title: 'Triples anotados (total temporada)' },
  { key: 'fgmTot',        label: 'TC tot.',   left: false, rank: false,  title: 'Tiros de campo anotados (total temporada)' },
  { key: 'ftM',           label: 'TLA',       left: false, rank: false,  title: 'Tiros libres anotados (total temporada)' },
]

export default function ClutchStats({ teams, players = [], playerBio = {}, loadClutchForSeason, clutchCache, loadingClutch }) {
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
  const [positionFilter, setPositionFilter] = useState('')
  const [viewMode, setViewMode]             = useState('basic')
  const [showAll, setShowAll]               = useState(false)

  useEffect(() => {
    if (selectedSeason) loadClutchForSeason(selectedSeason)
  }, [selectedSeason, loadClutchForSeason])

  useEffect(() => { setShowAll(false) }, [selectedSeason, minGames, teamFilter, positionFilter, search, viewMode])

  const isLoading = loadingClutch[selectedSeason] || false
  const rawPlayers = useMemo(() => clutchCache[selectedSeason]?.players || [], [clutchCache, selectedSeason])

  const playerLookup = useMemo(() => {
    const map = {}
    players.forEach(p => {
      const id = String(p.licenseId)
      if (!map[id] || p.season === selectedSeason) map[id] = p
    })
    return map
  }, [players, selectedSeason])

  const allTeams = useMemo(
    () => [...new Set(rawPlayers.map(p => p.team))].filter(Boolean).sort(),
    [rawPlayers]
  )

  const enriched = useMemo(() => {
    const num = v => (v == null || v === 'NA') ? null : Number(v)
    return rawPlayers.map(p => {
      const bio = playerLookup[String(p.licenseId)] || playerBio[String(p.licenseId)]
      const playerAbbrev = bio?.playerAbbrev || null
      const playerFull   = bio?.playerFull   || p.nick || null
      const position     = bio?.position     || null

      const g = p.games || 1

      // derived per-game shooting stats
      const fgmPg  = num((p.fg2M + p.fg3M) / g)
      const fgaPg  = num((p.fg2A + p.fg3A) / g)
      const fg3mPg = num(p.fg3M / g)
      const fg3aPg = num(p.fg3A / g)
      const fg2mPg = num(p.fg2M / g)
      const fg2aPg = num(p.fg2A / g)
      const ftmPg  = num(p.ftM / g)
      const ftaPg  = num(p.ftA / g)

      // fgPct from R, or derive if missing
      const fgPct = p.fgPct != null
        ? num(p.fgPct)
        : (p.fg2A + p.fg3A) > 0
          ? Math.round((p.fg2M + p.fg3M) / (p.fg2A + p.fg3A) * 1000) / 10
          : null

      // tsPct (requires total pts, not per-game)
      const totalPts = (p.pts || 0) * g
      const fga = (p.fg2A || 0) + (p.fg3A || 0)
      const tsPct = fga + (p.ftA || 0) > 0
        ? Math.round(totalPts / (2 * (fga + 0.44 * (p.ftA || 0))) * 1000) / 10
        : null

      const fg3Rate = fga > 0 ? Math.round((p.fg3A || 0) / fga * 1000) / 10 : null
      // legacy efficiency keys used by advancedCols
      const fg2Apg = num((p.fg2A || 0) / g)
      const fg3Apg = num((p.fg3A || 0) / g)
      const ftApg  = num((p.ftA  || 0) / g)

      // total FGM for Absolutos tab
      const fgmTot = (p.fg2M || 0) + (p.fg3M || 0)

      return {
        ...p,
        playerFull,
        playerAbbrev,
        playerDisplay: getPlayerDisplayName(bio || { playerFull, playerAbbrev, nick: p.nick }),
        position,
        fgmPg, fgaPg, fgPct,
        fg3mPg, fg3aPg,
        fg2mPg, fg2aPg,
        ftmPg, ftaPg,
        tsPct, fg3Rate,
        fg2Apg, fg3Apg, ftApg,
        fgmTot,
      }
    })
  }, [rawPlayers, playerLookup, playerBio])

  const allPositions = useMemo(() => {
    const present = new Set(enriched.map(p => p.position).filter(Boolean))
    return POSITION_ORDER.filter(pos => present.has(pos))
  }, [enriched])

  const filtered = useMemo(() => {
    return enriched.filter(p => {
      if ((p.games || 0) < minGames) return false
      if (teamFilter && p.team !== teamFilter) return false
      if (positionFilter && p.position !== positionFilter) return false
      if (search) {
        const q = search.toLowerCase()
        if (!getPlayerSearchText(p).includes(q) && !p.team?.toLowerCase().includes(q)) return false
      }
      return true
    })
  }, [enriched, minGames, teamFilter, positionFilter, search])

  const rankKeys = ['pts','reb','oreb','dreb','ast','stl','blk','fgPct','efgPct','tsPct','fg2Pct','fg3Pct','ftPct','ptsT','rebT','orebT','drebT','astT','stlT','blkT']
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
    // inverse: lower is better
    ;['tov', 'fouls', 'tovT', 'foulsT'].forEach(key => {
      const sorted = [...copy].filter(p => p[key] != null).sort((a, b) => (a[key] || 0) - (b[key] || 0))
      sorted.forEach((p, i) => {
        const orig = copy.find(x => x.licenseId === p.licenseId && x.team === p.team)
        if (orig) orig[`${key}Rank`] = i + 1
      })
    })
    return copy
  }, [filtered])

  const sorted = useMemo(() => {
    return [...withRanks].sort((a, b) => {
      const av = a[sortKey] ?? (sortDir === 'desc' ? -Infinity : Infinity)
      const bv = b[sortKey] ?? (sortDir === 'desc' ? -Infinity : Infinity)
      if (typeof av === 'string') return sortDir === 'desc' ? bv.localeCompare(av) : av.localeCompare(bv)
      return sortDir === 'desc' ? bv - av : av - bv
    })
  }, [withRanks, sortKey, sortDir])

  const cols = viewMode === 'basic' ? basicCols : viewMode === 'advanced' ? advancedCols : absolutesCols
  const n = sorted.length
  const displayed = showAll ? sorted : sorted.slice(0, DEFAULT_SHOW)

  const handleSort = (key) => {
    if (sortKey === key) setSortDir(d => d === 'desc' ? 'asc' : 'desc')
    else { setSortKey(key); setSortDir('desc') }
  }

  const nonSortable = new Set(['playerDisplay', 'team', 'position'])

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
            {[['basic','Básico'],['advanced','Avanzado'],['absolutos','Absolutos']].map(([mode, label]) => (
              <button
                key={mode}
                onClick={() => {
                  setViewMode(mode)
                  setSortKey(mode === 'basic' ? 'pts' : mode === 'advanced' ? 'efgPct' : 'ptsT')
                  setSortDir('desc')
                }}
                className={`px-3 py-1.5 text-sm font-medium rounded transition-colors ${
                  viewMode === mode ? 'bg-white text-acb-900 shadow-sm' : 'text-acb-600 hover:text-acb-900'
                }`}
              >{label}</button>
            ))}
          </div>

          {/* Team filter */}
          <div className="flex items-center gap-2">
            <Filter className="w-4 h-4 text-acb-400" />
            <select
              value={teamFilter}
              onChange={e => setTeamFilter(e.target.value)}
              className="px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
            >
              <option value="">Todos los equipos</option>
              {allTeams.map(t => <option key={t} value={t}>{t}</option>)}
            </select>
          </div>

          {/* Position filter */}
          {allPositions.length > 0 && (
            <select
              value={positionFilter}
              onChange={e => setPositionFilter(e.target.value)}
              className="px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
            >
              <option value="">Todas las posiciones</option>
              {allPositions.map(pos => <option key={pos} value={pos}>{pos}</option>)}
            </select>
          )}

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
        Mostrando {displayed.length} de {n} jugador{n !== 1 ? 'es' : ''} · Mín. {minGames} partido{minGames !== 1 ? 's' : ''} clutch · {seasonLabel(selectedSeason)}
      </div>

      {/* Table */}
      <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
        <div className="overflow-x-auto">
          <table className="data-table min-w-full">
            <thead>
              <tr className="bg-acb-50 border-b border-acb-200">
                <th className="data-table-head data-table-number data-table-sticky data-table-sticky-head data-col-rank bg-acb-50">#</th>
                {cols.map(col => (
                  <th
                    key={col.key}
                    onClick={() => !nonSortable.has(col.key) && handleSort(col.key)}
                    title={col.title || statTitle(col.label)}
                    className={`data-table-head
                      ${col.left ? 'text-left' : 'data-table-number'}
                      ${col.key === 'playerDisplay' ? 'data-table-sticky-after-rank data-table-sticky-head data-col-player bg-acb-50' : col.key === 'team' ? 'data-col-team' : col.key === 'position' ? 'data-col-position' : col.key === 'games' ? 'data-col-games' : 'data-col-number'}
                      ${!nonSortable.has(col.key) ? 'cursor-pointer hover:bg-acb-100' : ''}
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
              ) : displayed.map((p, i) => (
                <tr
                  key={`${p.licenseId}-${p.team}`}
                  onClick={() => p.licenseId && navigate(`/jugador/${p.licenseId}`)}
                  className="data-table-row border-b border-acb-100 cursor-pointer"
                >
                  <td className="data-table-cell data-table-number data-table-sticky data-col-rank text-acb-400">{i + 1}</td>
                  {cols.map(col => {
                    const v = p[col.key]
                    const rankKey = col.rank ? `${col.key}Rank` : null
                    const rank = rankKey ? p[rankKey] : null
                    const percentile = rankToPercentile(rank, n)

                    return (
                      <td
                        key={col.key}
                        className={`data-table-cell
                          ${col.left ? '' : 'data-table-number'}
                          ${col.key === 'playerDisplay' ? 'data-table-identity data-table-sticky-after-rank data-col-player' : col.key === 'team' ? 'data-col-team' : col.key === 'position' ? 'data-col-position' : col.key === 'games' ? 'data-col-games' : 'data-col-number'}
                          ${col.key === 'team' ? 'text-acb-600' : ''}
                          ${col.key === 'position' ? 'text-acb-500 text-xs' : ''}`}
                      >
                        {percentile != null ? (
                          <div className="data-table-value">
                            <span className="text-acb-700">{fmtVal(v, col.key)}</span>
                            <span className={`data-table-badge ${getPercentileColor(percentile)}`}>
                              {percentile}%
                            </span>
                          </div>
                        ) : (
                          <span className={col.left ? 'text-acb-700' : 'font-mono text-acb-700'}>
                            {v != null ? fmtVal(v, col.key) : '-'}
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

        {!isLoading && n > DEFAULT_SHOW && (
          <div className="px-4 py-3 bg-acb-50 border-t border-acb-200 text-sm text-acb-500 text-center">
            {showAll ? (
              <button onClick={() => setShowAll(false)} className="text-acb-600 hover:text-acb-900 underline">
                Mostrar menos
              </button>
            ) : (
              <>
                Mostrando los primeros {DEFAULT_SHOW} jugadores.{' '}
                <button onClick={() => setShowAll(true)} className="text-acb-600 hover:text-acb-900 underline">
                  Mostrar todos ({n})
                </button>
              </>
            )}
          </div>
        )}
      </div>
    </div>
  )
}
