import { useState, useMemo, useEffect } from 'react'
import { useParams, useNavigate, useSearchParams } from 'react-router-dom'
import { ArrowRight } from 'lucide-react'
import PageHeader from '../components/PageHeader'
import PlayerCombobox from '../components/PlayerCombobox'
import { statTitle } from '../utils/statLabels'
import { getPlayerDisplayName, getPlayerSearchText } from '../utils/playerNames'
import { buildPlayerProfilePath, buildPlayerSimilarityPath } from '../routing/paths'
import { normalizeNumericId, parsePlayerSegment } from '../routing/identifiers'
import { readRouteQuery, serializeRouteQuery, withQuery } from '../routing/query'


function seasonLabel(s) {
  return `${s - 1}-${String(s).slice(-2)}`
}

// selector de jugador
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
      if (!map.has(key)) {
        map.set(key, {
          value: key,
          label: getPlayerDisplayName(p),
          searchText: getPlayerSearchText(p),
          meta: p.team,
        })
      }
    })
    return [...map.values()].sort((a, b) =>
      a.label.localeCompare(b.label, 'es')
    )
  }, [players, teamFilter, seasonFilter])

  return (
    <div className="filter-panel">
      <div className="flex flex-col gap-1">
        <label htmlFor="similarity-season-filter" className="field-label">Temporada</label>
        <select
          id="similarity-season-filter"
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
        <label htmlFor="similarity-team-filter" className="field-label">Equipo</label>
        <select
          id="similarity-team-filter"
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
        id="similarity-player-search"
        options={playerOptions}
        value={selectedLicenseId}
        onChange={option => onSelect(option.value)}
      />
    </div>
  )
}

// ─── Score bar ────────────────────────────────────────────────
function ScoreBar({ score }) {
  const color = score >= 90 ? 'bg-acb-700' : score >= 80 ? 'bg-acb-600' : score >= 70 ? 'bg-acb-500' : 'bg-acb-300'
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

// compute mean and standard deviation across qualified players
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

// pick the top statistics by absolute z-score for a player
function getStandoutStats(player, distributions, n = 5) {
  const scored = SIMILARITY_STATS.map(stat => {
    const val = player[stat.key]
    if (val == null || isNaN(val)) return { ...stat, z: -Infinity }
    const z = (val - distributions[stat.key].mean) / distributions[stat.key].std
    return { ...stat, z }
  })
  return scored.filter(s => s.z > 0).sort((a, b) => b.z - a.z).slice(0, n)
}

function playerSeasonTeamKey(licenseId, season, teamId) {
  return `${licenseId}_${season}_${teamId}`
}

function buildSimilaritySearch(season, teamId) {
  return serializeRouteQuery('playerSimilarity', {
    temporada: season,
    equipo: teamId,
  })
}

function buildProfileUrl(record) {
  const search = serializeRouteQuery('playerProfile', {
    temporada: record.season,
    equipo: record.teamId,
    fase: 'regular',
    tabla: 'basico',
    radar: 'league',
    percentiles: 'league',
    tiro: 'own',
  })
  return withQuery(buildPlayerProfilePath(record), search)
}

// ─── Main Page ─────────────────────────────────────────────────
export default function PlayerSimilarity({ players, similarity }) {
  const routeParams = useParams()
  const navigate = useNavigate()
  const [searchParams, setSearchParams] = useSearchParams()
  const parsedPlayer = parsePlayerSegment(routeParams.player)
  const selectedLicenseId = parsedPlayer?.id || normalizeNumericId(routeParams.licenseId)
  const hasExplicitPlayer = Boolean(routeParams.player || routeParams.licenseId)

  // build lookup by player, season, and team
  const playerLookup = useMemo(() => {
    const map = new Map()
    players.forEach(p => {
      const key = playerSeasonTeamKey(p.licenseId, p.season, p.teamId)
      if (!map.has(key)) map.set(key, p)
    })
    return map
  }, [players])

  // compute distributions once across all qualified players
  const distributions = useMemo(() => computeStatDistributions(players), [players])

  const pageScope = useMemo(() => {
    const seasons = [...new Set(players.map(player => player.season))]
      .filter(Boolean)
      .sort((a, b) => a - b)
    if (seasons.length === 0) return 'Liga Endesa'
    if (seasons.length === 1) return `Liga Endesa · ${seasonLabel(seasons[0])}`
    return `Liga Endesa · ${seasonLabel(seasons[0])} a ${seasonLabel(seasons.at(-1))}`
  }, [players])

  const playerRecords = useMemo(() => {
    if (!selectedLicenseId) return []
    return players
      .filter(p => String(p.licenseId) === String(selectedLicenseId))
      .sort((a, b) => b.season - a.season || String(a.team).localeCompare(String(b.team), 'es'))
  }, [players, selectedLicenseId])

  const availableSeasons = useMemo(() => {
    return [...new Set(playerRecords.map(r => r.season))]
  }, [playerRecords])

  const latestSeason = useMemo(() => {
    const seasons = players.map(player => Number(player.season)).filter(Number.isFinite)
    return seasons.length ? Math.max(...seasons) : 2026
  }, [players])

  const queryState = readRouteQuery('playerSimilarity', searchParams, {
    defaults: { temporada: Number(routeParams.season) || availableSeasons[0] || latestSeason },
  })
  const selectedSeason = Number.isFinite(Number(queryState.temporada))
    ? Number(queryState.temporada)
    : (availableSeasons[0] || latestSeason)
  const selectedTeamId = queryState.equipo || null
  const seasonRecords = useMemo(() => (
    playerRecords.filter(record => Number(record.season) === selectedSeason)
  ), [playerRecords, selectedSeason])

  const currentRecord = useMemo(() => {
    if (!selectedLicenseId || !selectedSeason) return null
    if (selectedTeamId) return seasonRecords.find(record => record.teamId === selectedTeamId) || null
    return seasonRecords.length === 1 ? seasonRecords[0] : null
  }, [seasonRecords, selectedLicenseId, selectedSeason, selectedTeamId])
  const hasInvalidTeamSelection = Boolean(
    selectedTeamId && seasonRecords.length > 0 && !currentRecord
  )

  useEffect(() => {
    if (!hasExplicitPlayer) {
      if (searchParams.toString()) setSearchParams(new URLSearchParams(), { replace: true })
      return
    }
    if (!selectedLicenseId || playerRecords.length === 0) return
    const representative = currentRecord || playerRecords[0]
    const pathname = buildPlayerSimilarityPath(representative)
    const search = buildSimilaritySearch(selectedSeason, currentRecord?.teamId || selectedTeamId)
    const canonicalSegment = pathname.split('/').at(-1)
    const pathNeedsNormalization = routeParams.player !== canonicalSegment || Boolean(routeParams.licenseId)
    if (pathNeedsNormalization || searchParams.toString() !== search) {
      navigate({ pathname, search: search ? `?${search}` : '' }, { replace: true })
    }
  }, [currentRecord, hasExplicitPlayer, navigate, playerRecords, routeParams.licenseId, routeParams.player, searchParams, selectedLicenseId, selectedSeason, selectedTeamId, setSearchParams])

  // standout statistics for the selected player
  const standoutCols = useMemo(() => {
    if (!currentRecord) return []
    return getStandoutStats(currentRecord, distributions, 5)
  }, [currentRecord, distributions])

  // find the similarity entry
  const similarityEntry = useMemo(() => {
    if (!currentRecord) return null
    return similarity.find(entry => (
      String(entry.licenseId) === String(currentRecord.licenseId)
      && Number(entry.season) === Number(currentRecord.season)
      && entry.teamId === currentRecord.teamId
    )) || null
  }, [currentRecord, similarity])

  // resolve similar players to full records
  const similarPlayers = useMemo(() => {
    if (!similarityEntry?.similar) return []
    return similarityEntry.similar.map((s, idx) => {
      const key = playerSeasonTeamKey(s.licenseId, s.season, s.teamId)
      const player = playerLookup.get(key)
      return {
        rank: idx + 1,
        licenseId: s.licenseId,
        season: s.season,
        teamId: s.teamId,
        score: s.score,
        name: getPlayerDisplayName(player, `ID ${s.licenseId}`),
        team: player?.team || '-',
        _record: player,
      }
    })
  }, [similarityEntry, playerLookup])

  const handlePlayerSelect = (licenseId) => {
    const records = players
      .filter(player => String(player.licenseId) === String(licenseId))
      .sort((a, b) => b.season - a.season || String(a.team).localeCompare(String(b.team), 'es'))
    if (!records.length) return
    const season = records[0].season
    const latestRecords = records.filter(record => record.season === season)
    const teamId = latestRecords.length === 1 ? latestRecords[0].teamId : null
    const search = buildSimilaritySearch(season, teamId)
    navigate({ pathname: buildPlayerSimilarityPath(records[0]), search: `?${search}` })
  }

  const handleSeasonSelect = (season) => {
    const records = playerRecords.filter(record => record.season === season)
    const teamId = records.length === 1 ? records[0].teamId : null
    const search = buildSimilaritySearch(season, teamId)
    navigate({ pathname: buildPlayerSimilarityPath(records[0] || playerRecords[0]), search: `?${search}` })
  }

  const handleTeamSelect = (teamId) => {
    const record = seasonRecords.find(candidate => candidate.teamId === teamId)
    if (!record) return
    const search = buildSimilaritySearch(selectedSeason, teamId)
    navigate({ pathname: buildPlayerSimilarityPath(record), search: `?${search}` })
  }

  return (
    <div className="app-page space-y-6">
      <PageHeader
        title="Similitud de jugadores"
        subtitle="Encuentra jugadores con perfiles estadísticos similares a lo largo de todas las temporadas"
        scope={pageScope}
      />

      {/* player selector */}
      <PlayerSelector
        players={players}
        onSelect={handlePlayerSelect}
        selectedLicenseId={selectedLicenseId}
      />

      {selectedLicenseId && availableSeasons.length > 0 && (
        <div className="flex flex-wrap items-center gap-4">
          <div className="flex items-center gap-3">
            <label htmlFor="similarity-result-season" className="field-label">Temporada</label>
            <select
              id="similarity-result-season"
              value={selectedSeason || ''}
              onChange={e => handleSeasonSelect(Number(e.target.value))}
              className="form-control-compact"
            >
              {availableSeasons.map(s => (
                <option key={s} value={s}>{seasonLabel(s)}</option>
              ))}
            </select>
          </div>
          {(seasonRecords.length > 1 || hasInvalidTeamSelection) && (
            <div className="flex items-center gap-3">
              <label htmlFor="similarity-result-team" className="field-label">Equipo</label>
              <select
                id="similarity-result-team"
                value={currentRecord?.teamId || ''}
                onChange={event => handleTeamSelect(event.target.value)}
                className="form-control-compact"
              >
                <option value="">Selecciona un equipo</option>
                {seasonRecords.map(record => (
                  <option key={record.teamId} value={record.teamId}>{record.team}</option>
                ))}
              </select>
            </div>
          )}
        </div>
      )}

      {/* selected player info and results */}
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

      {/* results */}
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
                    key={`${p.licenseId}_${p.season}_${p.teamId}`}
                    onClick={() => p._record && navigate(buildProfileUrl(p._record))}
                    onKeyDown={(event) => {
                      if ((event.key === 'Enter' || event.key === ' ') && p._record) navigate(buildProfileUrl(p._record))
                    }}
                    role="link"
                    tabIndex={0}
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

      {hasExplicitPlayer && (!selectedLicenseId || playerRecords.length === 0) && (
        <div className="text-center py-12 text-acb-500">
          No se ha encontrado ningún jugador con ese identificador.
        </div>
      )}

      {selectedLicenseId && playerRecords.length > 0 && seasonRecords.length === 0 && (
        <div className="text-center py-12 text-acb-500">
          El jugador no tiene datos en la temporada indicada. Selecciona otra temporada.
        </div>
      )}

      {hasInvalidTeamSelection && (
        <div className="text-center py-12 text-acb-500">
          El equipo indicado no corresponde a este jugador en la temporada seleccionada. Selecciona un equipo disponible.
        </div>
      )}

      {seasonRecords.length > 1 && !selectedTeamId && !currentRecord && (
        <div className="text-center py-12 text-acb-500">
          El jugador cambió de equipo durante esta temporada. Selecciona la etapa que quieres analizar.
        </div>
      )}

      {currentRecord && !similarityEntry && (
        <div className="text-center py-12 text-acb-500">
          No se encontraron datos de similitud para esta temporada. El jugador puede no cumplir los requisitos mínimos (10 partidos, 10+ minutos por partido).
        </div>
      )}

      {!hasExplicitPlayer && (
        <div className="text-center py-12 text-acb-400">
          Selecciona un jugador para ver sus jugadores más similares
        </div>
      )}
    </div>
  )
}
