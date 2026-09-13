import React, { useMemo, useEffect } from 'react'
import { useNavigate, useSearchParams } from 'react-router-dom'
import { Trophy, TrendingDown, Filter } from 'lucide-react'
import { statTitle } from '../utils/statLabels'
import { getPlayerDisplayName } from '../utils/playerNames'
import LineupToolShell, { lineupToolStyles as styles } from '../components/LineupToolShell'
import { buildPlayerProfilePath } from '../routing/paths'
import { parseRouteQuery, serializeRouteQuery, withQuery } from '../routing/query'

// Extract licenseId from player key format "Name_12345"
const getIdFromKey = (key) => key?.split('_').pop() || ''

const getPlayerName = (key, player, playerNameById) => {
  const licenseId = player.id || player.licenseId || getIdFromKey(key)
  return playerNameById.get(String(licenseId)) || player.name || player.nickname || key
}

const getCombinationName = (ids, fallback, playerNameById) => {
  if (!Array.isArray(ids) || ids.length === 0) return fallback
  const names = ids.map(id => playerNameById.get(String(id))).filter(Boolean)
  return names.length === ids.length ? names.join(' · ') : fallback
}

/**
 * Lineup Rankings Page
 *
 * Shows top and bottom performers across players, pairs, trios, and 5-man lineups.
 * Two views: League-wide and Team-wise.
 */

// minimum minutes thresholds
const MIN_MINUTES = {
  players: 200,
  pairs: 150,
  trios: 120,
  lineups: 25
}

export default function LineupRankings({ teams, loadLineupsForSeason, lineupsCache, loadingLineups, playerRecords = [] }) {
  const navigate = useNavigate()
  const [searchParams, setSearchParams] = useSearchParams()

  const parsedQuery = parseRouteQuery('lineupRankings', searchParams)
  const selectedCategory = parsedQuery.values.categoria || 'players'
  const sortByImpact = parsedQuery.values.metrica !== 'offense'
  const showBottom = parsedQuery.values.extremo === 'bottom'

  const playerNameById = useMemo(() => {
    const names = new Map()
    playerRecords.forEach(player => {
      if (player.licenseId != null) {
        names.set(String(player.licenseId), getPlayerDisplayName(player))
      }
    })
    return names
  }, [playerRecords])

  // Available seasons
  const availableSeasons = useMemo(() => {
    const seasons = [...new Set(teams.map(t => t.season))].sort((a, b) => b - a)
    return seasons
  }, [teams])

  const requestedSeason = parsedQuery.values.temporada
  const selectedSeason = availableSeasons.includes(requestedSeason)
    ? requestedSeason
    : (availableSeasons[0] || 2026)

  const minMinutes = MIN_MINUTES
  const seasonTeamRows = useMemo(
    () => teams.filter(team => team.season === selectedSeason).sort((a, b) => a.team.localeCompare(b.team, 'es')),
    [selectedSeason, teams]
  )
  const selectedTeamId = parsedQuery.values.equipo || ''
  const selectedTeam = seasonTeamRows.find(team => team.teamId === selectedTeamId)?.team || ''
  const teamIdByName = useMemo(
    () => new Map(seasonTeamRows.map(team => [team.team, team.teamId])),
    [seasonTeamRows]
  )

  // Load lineups when season changes
  useEffect(() => {
    if (selectedSeason) {
      loadLineupsForSeason(selectedSeason)
    }
  }, [selectedSeason, loadLineupsForSeason])

  // Get lineups data for current season
  const lineupData = useMemo(() => {
    return lineupsCache[selectedSeason] || null
  }, [lineupsCache, selectedSeason])

  const loading = loadingLineups[selectedSeason] || false

  const canonicalSearch = serializeRouteQuery('lineupRankings', {
    temporada: selectedSeason,
    equipo: selectedTeam ? selectedTeamId : undefined,
    categoria: selectedCategory,
    metrica: sortByImpact ? 'impact' : 'offense',
    extremo: showBottom ? 'bottom' : 'top',
  })
  const currentSearch = searchParams.toString()

  useEffect(() => {
    if (canonicalSearch !== currentSearch) {
      setSearchParams(canonicalSearch, { replace: true })
    }
  }, [canonicalSearch, currentSearch, setSearchParams])

  const updateState = ({
    season = selectedSeason,
    teamId = selectedTeamId,
    category = selectedCategory,
    impact = sortByImpact,
    bottom = showBottom,
  }) => {
    setSearchParams(serializeRouteQuery('lineupRankings', {
      temporada: season,
      equipo: teamId || undefined,
      categoria: category,
      metrica: impact ? 'impact' : 'offense',
      extremo: bottom ? 'bottom' : 'top',
    }))
  }

  const playerProfileUrl = item => {
    const licenseId = getIdFromKey(item.key)
    const teamId = selectedTeam ? selectedTeamId : (teamIdByName.get(item.team) || item.teamId)
    const profileRecord = playerRecords.find(player => (
      String(player.licenseId) === String(licenseId)
      && Number(player.season) === Number(selectedSeason)
      && (!teamId || player.teamId === teamId)
    )) || { licenseId, playerDisplay: item.displayName }
    const search = serializeRouteQuery('playerProfile', {
      temporada: selectedSeason,
      equipo: teamId,
    }, { strict: false })
    return withQuery(buildPlayerProfilePath(profileRecord, item.displayName), search)
  }

  // Extract and process all data from all teams
  const allData = useMemo(() => {
    if (!lineupData?.data) return { players: [], pairs: [], trios: [], lineups: [] }

    const players = []
    const pairs = []
    const trios = []
    const lineups = []

    Object.entries(lineupData.data).forEach(([teamName, teamData]) => {
      // Players
      if (teamData.players) {
        Object.entries(teamData.players).forEach(([key, player]) => {
          if (player.onMin >= minMinutes.players) {
            players.push({
              ...player,
              key,
              team: teamName,
              teamId: teamData.teamId,
              displayName: getPlayerName(key, player, playerNameById)
            })
          }
        })
      }

      // Pairs
      if (teamData.pairs) {
        Object.entries(teamData.pairs).forEach(([key, pair]) => {
          if (pair.onMin >= minMinutes.pairs) {
            pairs.push({
              ...pair,
              key,
              team: teamName,
              displayName: getCombinationName([pair.player1Id, pair.player2Id], pair.players || key, playerNameById)
            })
          }
        })
      }

      // Trios
      if (teamData.trios) {
        Object.entries(teamData.trios).forEach(([key, trio]) => {
          if (trio.onMin >= minMinutes.trios) {
            trios.push({
              ...trio,
              key,
              team: teamName,
              displayName: getCombinationName(trio.playerIds, trio.players || key, playerNameById)
            })
          }
        })
      }

      // 5-man Lineups
      if (teamData.lineups) {
        Object.entries(teamData.lineups).forEach(([key, lineup]) => {
          if (lineup.onMin >= minMinutes.lineups) {
            lineups.push({
              ...lineup,
              key,
              team: teamName,
              displayName: getCombinationName(lineup.playerIds, lineup.players || key, playerNameById)
            })
          }
        })
      }
    })

    return { players, pairs, trios, lineups }
  }, [lineupData, minMinutes, playerNameById])

  // Get data for selected team only
  const teamFilteredData = useMemo(() => {
    if (!lineupData?.data || !selectedTeam) return { players: [], pairs: [], trios: [], lineups: [] }

    const teamInfo = lineupData.data[selectedTeam]
    if (!teamInfo) return { players: [], pairs: [], trios: [], lineups: [] }

    const players = []
    const pairs = []
    const trios = []
    const lineups = []

    // Players
    if (teamInfo.players) {
      Object.entries(teamInfo.players).forEach(([key, player]) => {
        if (player.onMin >= minMinutes.players) {
          players.push({
            ...player,
            key,
            team: selectedTeam,
            teamId: teamInfo.teamId || selectedTeamId,
            displayName: getPlayerName(key, player, playerNameById)
          })
        }
      })
    }

    // Pairs
    if (teamInfo.pairs) {
      Object.entries(teamInfo.pairs).forEach(([key, pair]) => {
        if (pair.onMin >= minMinutes.pairs) {
          pairs.push({
            ...pair,
            key,
            team: selectedTeam,
            displayName: getCombinationName([pair.player1Id, pair.player2Id], pair.players || key, playerNameById)
          })
        }
      })
    }

    // Trios
    if (teamInfo.trios) {
      Object.entries(teamInfo.trios).forEach(([key, trio]) => {
        if (trio.onMin >= minMinutes.trios) {
          trios.push({
            ...trio,
            key,
            team: selectedTeam,
            displayName: getCombinationName(trio.playerIds, trio.players || key, playerNameById)
          })
        }
      })
    }

    // 5-man Lineups
    if (teamInfo.lineups) {
      Object.entries(teamInfo.lineups).forEach(([key, lineup]) => {
        if (lineup.onMin >= minMinutes.lineups) {
          lineups.push({
            ...lineup,
            key,
            team: selectedTeam,
            displayName: getCombinationName(lineup.playerIds, lineup.players || key, playerNameById)
          })
        }
      })
    }

    return { players, pairs, trios, lineups }
  }, [lineupData, selectedTeam, selectedTeamId, minMinutes, playerNameById])

  const hasTeamFilter = selectedTeam !== ''
  const currentData = hasTeamFilter ? teamFilteredData : allData

  // Sort and get top/bottom items
  const getRankedData = (items, category) => {
    if (!items || items.length === 0) return []

    // Determine sort key
    let sortKey
    if (category === 'lineups') {
      // 5-man lineups don't have netDiff, always sort by onNetRtg
      sortKey = 'onNetRtg'
    } else if (!hasTeamFilter && !sortByImpact) {
      sortKey = 'onNetRtg'
    } else {
      sortKey = 'netDiff'
    }

    // Sort
    const sorted = [...items].sort((a, b) => {
      const aVal = a[sortKey] ?? -999
      const bVal = b[sortKey] ?? -999
      return showBottom ? aVal - bVal : bVal - aVal
    })

    // Return top/bottom 10
    return sorted.slice(0, 10)
  }

  const rankedData = getRankedData(currentData[selectedCategory], selectedCategory)
  const isImpactRanking = selectedCategory !== 'lineups' && (hasTeamFilter || sortByImpact)

  // Category labels
  const categoryLabels = {
    players: 'Jugadores',
    pairs: 'Dúos',
    trios: 'Tríos',
    lineups: 'Quintetos'
  }

  if (loading) {
    return (
      <LineupToolShell
        activeTool="rankings"
        title="Rankings de alineaciones"
      >
        <div className={styles.loadingState} role="status">Cargando rankings…</div>
      </LineupToolShell>
    )
  }

  return (
    <LineupToolShell
      activeTool="rankings"
      title="Rankings de alineaciones"
    >

      {/* Controls */}
      <div className={styles.controlBand}>
        <div className={styles.scopeControls}>
            {/* Season */}
            <div className={styles.controlField}>
              <label htmlFor="lineup-rankings-season" className="field-label">Temporada</label>
              <select
                id="lineup-rankings-season"
                value={selectedSeason}
                onChange={(e) => updateState({ season: parseInt(e.target.value), teamId: null })}
                className="form-control"
              >
                {availableSeasons.map(season => (
                  <option key={season} value={season}>{season-1}-{String(season).slice(-2)}</option>
                ))}
              </select>
            </div>

            <div className={`${styles.controlField} ${styles.controlFieldWide}`}>
              <label htmlFor="lineup-rankings-team" className="field-label">Equipo</label>
              <select
                id="lineup-rankings-team"
                value={selectedTeamId || ''}
                onChange={(e) => updateState({ teamId: e.target.value || null })}
                className="form-control min-w-[220px]"
              >
                <option value="">Toda la liga</option>
                {seasonTeamRows.map(team => (
                  <option key={team.teamId} value={team.teamId}>{team.team}</option>
                ))}
              </select>
            </div>
          <div className={styles.selectionContext}>
            <Filter className="w-4 h-4 text-acb-400" />
            <span>
              {hasTeamFilter ? selectedTeam : 'Toda la liga'}
            </span>
          </div>
        </div>

        {/* Category Tabs */}
        <div className={styles.categoryRail} role="group" aria-label="Tipo de combinación">
          {Object.entries(categoryLabels).map(([key, label]) => (
            <button
              key={key}
              onClick={() => updateState({ category: key })}
              aria-pressed={selectedCategory === key}
              className={`${styles.categoryButton} ${selectedCategory === key ? styles.categoryButtonActive : ''}`}
            >
              {label}
            </button>
          ))}
        </div>

        {/* Sort & Filter Options */}
        <div className={styles.rankingOptions}>
          {/* Top/Bottom Toggle */}
          <div className={styles.segmentedGroup} role="group" aria-label="Extremo del ranking">
            <button
              onClick={() => updateState({ bottom: false })}
              aria-pressed={!showBottom}
              className={`${styles.segmentButton} ${!showBottom ? styles.segmentButtonActive : ''}`}
            >
              <Trophy className="w-4 h-4" />
              Mejores 10
            </button>
            <button
              onClick={() => updateState({ bottom: true })}
              aria-pressed={showBottom}
              className={`${styles.segmentButton} ${showBottom ? styles.segmentButtonActive : ''}`}
            >
              <TrendingDown className="w-4 h-4" />
              Peores 10
            </button>
          </div>

          {/* Sort Toggle (only for league rankings and non-lineup categories) */}
          {!hasTeamFilter && selectedCategory !== 'lineups' && (
            <div className={styles.segmentedGroup} role="group" aria-label="Métrica del ranking">
              <button
                onClick={() => updateState({ impact: true })}
                aria-pressed={sortByImpact}
                className={`${styles.segmentButton} ${sortByImpact ? styles.segmentButtonActive : ''}`}
              >
                Impacto
              </button>
              <button
                onClick={() => updateState({ impact: false })}
                aria-pressed={!sortByImpact}
                className={`${styles.segmentButton} ${!sortByImpact ? styles.segmentButtonActive : ''}`}
              >
                Ef. Neta
              </button>
            </div>
          )}

          {/* minutes threshold info */}
          <div className={styles.minutesNote}>
            Min. {minMinutes[selectedCategory]} min
          </div>
        </div>
      </div>

      {/* Results Table */}
      <section className={styles.sheet} aria-labelledby="lineup-ranking-title">
        <div className={styles.sectionHeader}>
          <div>
          <h2 id="lineup-ranking-title" className="flex items-center gap-2">
            {showBottom ? <TrendingDown className="w-5 h-5 text-accent-500" /> : <Trophy className="w-5 h-5 text-accent-500" />}
            {showBottom ? 'Peores' : 'Mejores'} 10 {categoryLabels[selectedCategory]}
          </h2>
          <p>{isImpactRanking ? 'Ordenado por impacto On/Off' : 'Ordenado por eficiencia neta en cancha'}</p>
          </div>
          <span className={styles.panelMeta}>
            {hasTeamFilter ? selectedTeam : 'Toda la liga'}
          </span>
        </div>

        {rankedData.length > 0 ? (
          <div className={styles.tableFrame} tabIndex={0} aria-label="Ranking de alineaciones">
            <table className="data-table">
              <thead>
                <tr className="bg-acb-50 text-left text-xs text-acb-600 uppercase tracking-wider">
                  <th className="data-table-head data-table-number data-table-sticky data-table-sticky-head data-col-rank bg-acb-50">#</th>
                  <th className="data-table-head data-table-identity data-table-sticky-after-rank data-table-sticky-head data-col-player bg-acb-50">
                    {selectedCategory === 'players' ? 'Jugador' : 'Combinación'}
                  </th>
                  {!hasTeamFilter && (
                    <th className="data-table-head text-left data-col-team">Equipo</th>
                  )}
                  <th className="data-table-head data-table-number data-col-games" title={statTitle('Min')}>Min</th>
                  <th className="data-table-head data-table-number data-col-number" title={statTitle('ORtg')}>ORtg</th>
                  <th className="data-table-head data-table-number data-col-number" title={statTitle('DRtg')}>DRtg</th>
                  <th className="data-table-head data-table-number data-col-number" title={statTitle('Neto')}>Neto</th>
                  {selectedCategory !== 'lineups' && (
                    <th className={`data-table-head data-table-number data-col-number ${
                      (hasTeamFilter || sortByImpact) ? 'bg-accent-50' : ''
                    }`}>
                      Impacto
                    </th>
                  )}
                </tr>
              </thead>
              <tbody className="divide-y divide-acb-100">
                {rankedData.map((item, index) => (
                  <tr key={`${item.team || selectedTeam || 'liga'}-${item.key}`} className="data-table-row">
                    <td className="data-table-cell data-table-number data-table-sticky data-col-rank text-acb-400">
                      {index + 1}
                    </td>
                    <td className="data-table-cell data-table-identity data-table-sticky-after-rank data-col-player">
                      {selectedCategory === 'players' ? (
                        <button
                          type="button"
                          className="text-left hover:text-accent-600 hover:underline"
                          onClick={() => navigate(playerProfileUrl(item))}
                        >
                          {item.displayName}
                        </button>
                      ) : item.displayName}
                    </td>
                    {!hasTeamFilter && (
                      <td className="data-table-cell data-col-team text-acb-600">{item.team}</td>
                    )}
                    <td className="data-table-cell data-table-number data-col-games text-acb-500">
                      {item.onMin?.toFixed(0)}
                    </td>
                    <td className="data-table-cell data-table-number data-col-number text-acb-700">
                      {item.onORtg?.toFixed(1)}
                    </td>
                    <td className="data-table-cell data-table-number data-col-number text-acb-700">
                      {item.onDRtg?.toFixed(1)}
                    </td>
                    <td className={`data-table-cell data-table-number data-col-number font-semibold ${
                      item.onNetRtg > 5 ? 'text-positive' : item.onNetRtg < -5 ? 'text-negative' : 'text-acb-700'
                    }`}>
                      {item.onNetRtg > 0 ? '+' : ''}{item.onNetRtg?.toFixed(1)}
                    </td>
                    {selectedCategory !== 'lineups' && (
                      <td className={`data-table-cell data-table-number data-col-number font-semibold ${
                        (hasTeamFilter || sortByImpact) ? 'bg-accent-50' : ''
                      } ${
                        item.netDiff > 5 ? 'text-positive' : item.netDiff < -5 ? 'text-negative' : 'text-acb-700'
                      }`}>
                        {item.netDiff > 0 ? '+' : ''}{item.netDiff?.toFixed(1)}
                      </td>
                    )}
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        ) : (
          <div className={styles.emptyState}>
            <strong>No hay suficientes datos para este ranking.</strong>
            <span>Se requieren al menos {minMinutes[selectedCategory]} minutos.</span>
          </div>
        )}
      </section>

      {/* Legend/Info */}
      <aside className={styles.methodPanel} aria-labelledby="lineup-ranking-notes">
        <h2 id="lineup-ranking-notes">Cómo leer el ranking</h2>
        <ul className={styles.noteList}>
          <li>
            <strong>Impacto:</strong> Diferencia entre el Net Rating del equipo con el jugador/combinación en pista vs. fuera de pista.
          </li>
          <li>
            <strong>ORtg/DRtg/NetRtg:</strong> Eficiencia ofensiva, defensiva y neta por 100 posesiones cuando la combinación está en pista.
          </li>
          <li>
            <strong>Quintetos:</strong> No tienen Impacto calculado, se ordenan por Net Rating en pista.
          </li>
          <li>
            <strong>Filtros de minutos:</strong> Jugadores {MIN_MINUTES.players}+ min, Dúos {MIN_MINUTES.pairs}+ min, Tríos {MIN_MINUTES.trios}+ min, Quintetos {MIN_MINUTES.lineups}+ min.
          </li>
        </ul>
      </aside>
    </LineupToolShell>
  )
}
