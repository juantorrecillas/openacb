import React, { useState, useMemo, useEffect } from 'react'
import { useNavigate } from 'react-router-dom'
import { Trophy, TrendingDown, Filter } from 'lucide-react'
import { statTitle } from '../utils/statLabels'
import { getPlayerDisplayName } from '../utils/playerNames'
import PageHeader from '../components/PageHeader'

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

  // State
  const [selectedCategory, setSelectedCategory] = useState('players') // players, pairs, trios, lineups
  const [sortByImpact, setSortByImpact] = useState(true) // true = Impact (netDiff), false = ORtg
  const [showBottom, setShowBottom] = useState(false) // Show bottom instead of top

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

  const [selectedSeason, setSelectedSeason] = useState(availableSeasons[0] || 2025)

  const minMinutes = MIN_MINUTES
  const [selectedTeam, setSelectedTeam] = useState('')

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

  // Get teams for current season
  const seasonTeams = useMemo(() => {
    return teams.filter(t => t.season === selectedSeason).map(t => t.team).sort()
  }, [teams, selectedSeason])

  // keep the selected team valid when the season changes
  useEffect(() => {
    if (selectedTeam && !seasonTeams.includes(selectedTeam)) {
      setSelectedTeam('')
    }
  }, [seasonTeams, selectedTeam])

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
  }, [lineupData, selectedTeam, minMinutes, playerNameById])

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

  // Category labels
  const categoryLabels = {
    players: 'Jugadores',
    pairs: 'Dúos',
    trios: 'Tríos',
    lineups: 'Quintetos'
  }

  if (loading) {
    return (
      <div className="app-page space-y-6">
        <PageHeader title="Rankings de alineaciones" subtitle="Mejores y peores jugadores, dúos, tríos y quintetos por impacto y eficiencia" />
        <div className="bg-acb-50 rounded-lg p-8 text-center">
          <div className="animate-pulse text-acb-600">Cargando rankings...</div>
        </div>
      </div>
    )
  }

  return (
    <div className="app-page space-y-6">
      <PageHeader title="Rankings de alineaciones" subtitle="Mejores y peores jugadores, dúos, tríos y quintetos por impacto y eficiencia" />

      {/* Controls */}
      <div className="filter-panel space-y-4">
        <div className="flex flex-wrap items-end justify-between gap-4">
          <div className="flex flex-wrap items-center gap-4">
            {/* Season */}
            <div className="flex items-center gap-2">
              <span className="field-label">Temporada</span>
              <select
                aria-label="Temporada"
                value={selectedSeason}
                onChange={(e) => setSelectedSeason(parseInt(e.target.value))}
                className="form-control"
              >
                {availableSeasons.map(season => (
                  <option key={season} value={season}>{season-1}-{String(season).slice(-2)}</option>
                ))}
              </select>
            </div>

            <div className="flex items-center gap-2">
              <span className="field-label">Equipo</span>
              <select
                aria-label="Equipo"
                value={selectedTeam}
                onChange={(e) => setSelectedTeam(e.target.value)}
                className="form-control min-w-[220px]"
              >
                <option value="">Toda la liga</option>
                {seasonTeams.map(team => (
                  <option key={team} value={team}>{team}</option>
                ))}
              </select>
            </div>
          </div>

          <div className="flex items-center gap-2 rounded-md bg-acb-50 px-3 py-2 text-sm text-acb-600">
            <Filter className="w-4 h-4 text-acb-400" />
            <span className="font-medium">
              {hasTeamFilter ? selectedTeam : 'Toda la liga'}
            </span>
          </div>
        </div>

        {/* Category Tabs */}
        <div className="flex flex-wrap gap-2">
          {Object.entries(categoryLabels).map(([key, label]) => (
            <button
              key={key}
              onClick={() => setSelectedCategory(key)}
              className={`px-4 py-2 rounded-md text-sm font-medium transition-colors ${
                selectedCategory === key
                  ? 'bg-accent-100 text-accent-700 border border-accent-200'
                  : 'bg-acb-50 text-acb-600 hover:bg-acb-100 border border-transparent'
              }`}
            >
              {label}
            </button>
          ))}
        </div>

        {/* Sort & Filter Options */}
        <div className="flex flex-wrap items-center gap-4 pt-2 border-t border-acb-100">
          {/* Top/Bottom Toggle */}
          <div className="flex items-center gap-2">
            <button
              onClick={() => setShowBottom(false)}
              className={`flex items-center gap-1 px-3 py-1.5 rounded text-sm font-medium transition-colors ${
                !showBottom
                  ? 'bg-accent-100 text-accent-700'
                  : 'bg-acb-50 text-acb-500 hover:bg-acb-100'
              }`}
            >
              <Trophy className="w-4 h-4" />
              Mejores 10
            </button>
            <button
              onClick={() => setShowBottom(true)}
              className={`flex items-center gap-1 px-3 py-1.5 rounded text-sm font-medium transition-colors ${
                showBottom
                  ? 'bg-accent-100 text-accent-700'
                  : 'bg-acb-50 text-acb-500 hover:bg-acb-100'
              }`}
            >
              <TrendingDown className="w-4 h-4" />
              Peores 10
            </button>
          </div>

          {/* Sort Toggle (only for league rankings and non-lineup categories) */}
          {!hasTeamFilter && selectedCategory !== 'lineups' && (
            <div className="flex items-center gap-2">
              <Filter className="w-4 h-4 text-acb-400" />
              <span className="text-sm text-acb-600">Ordenar por:</span>
              <button
                onClick={() => setSortByImpact(true)}
                className={`px-3 py-1.5 rounded text-sm font-medium transition-colors ${
                  sortByImpact
                    ? 'bg-acb-700 text-white'
                    : 'bg-acb-50 text-acb-600 hover:bg-acb-100'
                }`}
              >
                Impacto
              </button>
              <button
                onClick={() => setSortByImpact(false)}
                className={`px-3 py-1.5 rounded text-sm font-medium transition-colors ${
                  !sortByImpact
                    ? 'bg-acb-700 text-white'
                    : 'bg-acb-50 text-acb-600 hover:bg-acb-100'
                }`}
              >
                Ef. Neta
              </button>
            </div>
          )}

          {/* minutes threshold info */}
          <div className="ml-auto text-xs text-acb-400">
            Min. {minMinutes[selectedCategory]} min
          </div>
        </div>
      </div>

      {/* Results Table */}
      <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
        <div className="px-4 py-3 border-b border-acb-200 bg-accent-50 flex flex-wrap items-center justify-between gap-2">
          <h3 className="font-semibold text-acb-900 flex items-center gap-2">
            {showBottom ? <TrendingDown className="w-5 h-5 text-accent-500" /> : <Trophy className="w-5 h-5 text-accent-500" />}
            {showBottom ? 'Peores' : 'Mejores'} 10 {categoryLabels[selectedCategory]}
          </h3>
          <span className="text-xs text-acb-500">
            {hasTeamFilter ? selectedTeam : 'Toda la liga'}
          </span>
        </div>

        {rankedData.length > 0 ? (
          <div className="overflow-x-auto">
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
                          onClick={() => navigate(`/jugador/${getIdFromKey(item.key)}`)}
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
          <div className="p-8 text-center text-acb-500">
            <p>No hay suficientes datos para mostrar el ranking.</p>
            <p className="text-sm mt-1">
              Se requieren al menos {minMinutes[selectedCategory]} minutos.
            </p>
          </div>
        )}
      </div>

      {/* Legend/Info */}
      <div className="bg-acb-50 rounded-lg border border-acb-200 p-4">
        <h4 className="text-sm font-semibold text-acb-900 mb-2">Notas</h4>
        <ul className="text-xs text-acb-600 space-y-1">
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
      </div>
    </div>
  )
}
