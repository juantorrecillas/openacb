import React, { useState, useMemo, useEffect } from 'react'
import { Trophy, TrendingDown, ChevronDown, ChevronUp, Filter } from 'lucide-react'

/**
 * Lineup Rankings Page
 *
 * Shows top and bottom performers across players, pairs, trios, and 5-man lineups.
 * Two views: League-wide and Team-wise.
 */

// Minimum minutes thresholds - different for current (unfinished) vs past seasons
const MIN_MINUTES_CURRENT = {
  players: 150,
  pairs: 120,
  trios: 80,
  lineups: 15
}

const MIN_MINUTES_PAST = {
  players: 200,
  pairs: 150,
  trios: 120,
  lineups: 25
}

export default function LineupRankings({ teams, loadLineupsForSeason, lineupsCache, loadingLineups }) {
  // State
  const [activeView, setActiveView] = useState('league') // 'league' or 'team'
  const [selectedCategory, setSelectedCategory] = useState('players') // players, pairs, trios, lineups
  const [sortByImpact, setSortByImpact] = useState(true) // true = Impact (netDiff), false = ORtg
  const [showBottom, setShowBottom] = useState(false) // Show bottom instead of top

  // Available seasons
  const availableSeasons = useMemo(() => {
    const seasons = [...new Set(teams.map(t => t.season))].sort((a, b) => b - a)
    return seasons
  }, [teams])

  const [selectedSeason, setSelectedSeason] = useState(availableSeasons[0] || 2025)

  // Determine if current season is the most recent (unfinished)
  const mostRecentSeason = availableSeasons[0]
  const isCurrentSeason = selectedSeason === mostRecentSeason

  // Get appropriate minute thresholds
  const minMinutes = isCurrentSeason ? MIN_MINUTES_CURRENT : MIN_MINUTES_PAST
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

  // Set default team when season changes
  useEffect(() => {
    if (seasonTeams.length > 0 && !seasonTeams.includes(selectedTeam)) {
      setSelectedTeam(seasonTeams[0])
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
              displayName: player.name || player.nickname || key
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
              displayName: pair.players || key
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
              displayName: trio.players || key
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
              displayName: lineup.players || key
            })
          }
        })
      }
    })

    return { players, pairs, trios, lineups }
  }, [lineupData, minMinutes])

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
            displayName: player.name || player.nickname || key
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
            displayName: pair.players || key
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
            displayName: trio.players || key
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
            displayName: lineup.players || key
          })
        }
      })
    }

    return { players, pairs, trios, lineups }
  }, [lineupData, selectedTeam, minMinutes])

  // Get current dataset based on view
  const currentData = activeView === 'league' ? allData : teamFilteredData

  // Sort and get top/bottom items
  const getRankedData = (items, category) => {
    if (!items || items.length === 0) return []

    // Determine sort key
    let sortKey
    if (category === 'lineups') {
      // 5-man lineups don't have netDiff, always sort by onNetRtg
      sortKey = 'onNetRtg'
    } else if (activeView === 'league' && !sortByImpact) {
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
      <div className="space-y-6">
        <div>
          <h2 className="text-2xl font-semibold text-acb-900">Rankings de Alineaciones</h2>
          <p className="text-acb-500 text-sm mt-1">Cargando datos...</p>
        </div>
        <div className="bg-acb-50 rounded-lg p-8 text-center">
          <div className="animate-pulse text-acb-600">Cargando rankings...</div>
        </div>
      </div>
    )
  }

  return (
    <div className="space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-semibold text-acb-900">Rankings de Alineaciones</h2>
        <p className="text-acb-500 text-sm mt-1">
          Top y Bottom performers por impacto y eficiencia
        </p>
      </div>

      {/* Controls */}
      <div className="bg-white rounded-lg border border-acb-200 p-4 space-y-4">
        {/* Season & View Toggle */}
        <div className="flex flex-wrap items-center justify-between gap-4">
          <div className="flex items-center gap-4">
            {/* Season */}
            <div className="flex items-center gap-2">
              <span className="text-sm text-acb-600 font-medium">Temporada:</span>
              <select
                value={selectedSeason}
                onChange={(e) => setSelectedSeason(parseInt(e.target.value))}
                className="px-3 py-2 border border-acb-200 rounded-md text-sm bg-white font-medium"
              >
                {availableSeasons.map(season => (
                  <option key={season} value={season}>{season-1}-{String(season).slice(-2)}</option>
                ))}
              </select>
            </div>

            {/* Team selector (only for team view) */}
            {activeView === 'team' && (
              <div className="flex items-center gap-2">
                <span className="text-sm text-acb-600 font-medium">Equipo:</span>
                <select
                  value={selectedTeam}
                  onChange={(e) => setSelectedTeam(e.target.value)}
                  className="px-3 py-2 border border-acb-200 rounded-md text-sm bg-white font-medium min-w-[180px]"
                >
                  {seasonTeams.map(team => (
                    <option key={team} value={team}>{team}</option>
                  ))}
                </select>
              </div>
            )}
          </div>

          {/* View Toggle */}
          <div className="flex rounded-lg border border-acb-200 overflow-hidden">
            <button
              onClick={() => setActiveView('league')}
              className={`px-4 py-2 text-sm font-medium transition-colors ${
                activeView === 'league'
                  ? 'bg-acb-700 text-white'
                  : 'bg-white text-acb-600 hover:bg-acb-50'
              }`}
            >
              Liga
            </button>
            <button
              onClick={() => setActiveView('team')}
              className={`px-4 py-2 text-sm font-medium transition-colors ${
                activeView === 'team'
                  ? 'bg-acb-700 text-white'
                  : 'bg-white text-acb-600 hover:bg-acb-50'
              }`}
            >
              Por Equipo
            </button>
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
                  ? 'bg-orange-100 text-orange-700 border border-orange-200'
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
                  ? 'bg-green-100 text-green-700'
                  : 'bg-acb-50 text-acb-500 hover:bg-acb-100'
              }`}
            >
              <Trophy className="w-4 h-4" />
              Top 10
            </button>
            <button
              onClick={() => setShowBottom(true)}
              className={`flex items-center gap-1 px-3 py-1.5 rounded text-sm font-medium transition-colors ${
                showBottom
                  ? 'bg-red-100 text-red-700'
                  : 'bg-acb-50 text-acb-500 hover:bg-acb-100'
              }`}
            >
              <TrendingDown className="w-4 h-4" />
              Bottom 10
            </button>
          </div>

          {/* Sort Toggle (only for league view and non-lineup categories) */}
          {activeView === 'league' && selectedCategory !== 'lineups' && (
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
                NetRtg
              </button>
            </div>
          )}

          {/* Minutes threshold info */}
          <div className="ml-auto text-xs text-acb-400">
            Min. {minMinutes[selectedCategory]} min {isCurrentSeason && '(temp. actual)'}
          </div>
        </div>
      </div>

      {/* Results Table */}
      <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
        <div className={`px-4 py-3 border-b border-acb-200 ${showBottom ? 'bg-red-50' : 'bg-green-50'}`}>
          <h3 className="font-semibold text-acb-900 flex items-center gap-2">
            {showBottom ? (
              <>
                <TrendingDown className="w-5 h-5 text-red-500" />
                Bottom 10 {categoryLabels[selectedCategory]}
              </>
            ) : (
              <>
                <Trophy className="w-5 h-5 text-amber-500" />
                Top 10 {categoryLabels[selectedCategory]}
              </>
            )}
            <span className="text-sm font-normal text-acb-500">
              {activeView === 'league' ? '(Liga)' : `(${selectedTeam})`}
            </span>
          </h3>
        </div>

        {rankedData.length > 0 ? (
          <div className="overflow-x-auto">
            <table className="w-full text-sm">
              <thead>
                <tr className="bg-acb-50 text-left text-xs text-acb-600 uppercase tracking-wider">
                  <th className="px-4 py-3 font-semibold w-12">#</th>
                  <th className="px-4 py-3 font-semibold">
                    {selectedCategory === 'players' ? 'Jugador' : 'Combinación'}
                  </th>
                  {activeView === 'league' && (
                    <th className="px-4 py-3 font-semibold">Equipo</th>
                  )}
                  <th className="px-4 py-3 font-semibold text-center">Min</th>
                  <th className="px-4 py-3 font-semibold text-center">ORtg</th>
                  <th className="px-4 py-3 font-semibold text-center">DRtg</th>
                  <th className="px-4 py-3 font-semibold text-center">NetRtg</th>
                  {selectedCategory !== 'lineups' && (
                    <th className={`px-4 py-3 font-semibold text-center ${
                      (activeView === 'team' || sortByImpact) ? 'bg-orange-50' : ''
                    }`}>
                      Impacto
                    </th>
                  )}
                </tr>
              </thead>
              <tbody className="divide-y divide-acb-100">
                {rankedData.map((item, index) => (
                  <tr key={item.key} className="hover:bg-acb-50 transition-colors">
                    <td className="px-4 py-3 font-mono text-acb-400">
                      {showBottom ? rankedData.length - index : index + 1}
                    </td>
                    <td className="px-4 py-3 font-medium text-acb-900">
                      {item.displayName}
                    </td>
                    {activeView === 'league' && (
                      <td className="px-4 py-3 text-acb-600">{item.team}</td>
                    )}
                    <td className="px-4 py-3 text-center font-mono text-acb-500">
                      {item.onMin?.toFixed(0)}
                    </td>
                    <td className={`px-4 py-3 text-center font-mono font-medium ${
                      item.onORtg > 110 ? 'text-green-600' : item.onORtg < 100 ? 'text-red-500' : 'text-acb-700'
                    }`}>
                      {item.onORtg?.toFixed(1)}
                    </td>
                    <td className={`px-4 py-3 text-center font-mono font-medium ${
                      item.onDRtg < 105 ? 'text-green-600' : item.onDRtg > 115 ? 'text-red-500' : 'text-acb-700'
                    }`}>
                      {item.onDRtg?.toFixed(1)}
                    </td>
                    <td className={`px-4 py-3 text-center font-mono font-semibold ${
                      item.onNetRtg > 5 ? 'text-green-600' : item.onNetRtg < -5 ? 'text-red-500' : 'text-acb-700'
                    }`}>
                      {item.onNetRtg > 0 ? '+' : ''}{item.onNetRtg?.toFixed(1)}
                    </td>
                    {selectedCategory !== 'lineups' && (
                      <td className={`px-4 py-3 text-center font-mono font-bold ${
                        (activeView === 'team' || sortByImpact) ? 'bg-orange-50' : ''
                      } ${
                        item.netDiff > 5 ? 'text-green-600' : item.netDiff < -5 ? 'text-red-500' : 'text-acb-700'
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
            <strong>Filtros de minutos (temporada actual):</strong> Jugadores {MIN_MINUTES_CURRENT.players}+ min, Dúos {MIN_MINUTES_CURRENT.pairs}+ min, Tríos {MIN_MINUTES_CURRENT.trios}+ min, Quintetos {MIN_MINUTES_CURRENT.lineups}+ min.
          </li>
          <li>
            <strong>Filtros de minutos (temporadas pasadas):</strong> Jugadores {MIN_MINUTES_PAST.players}+ min, Dúos {MIN_MINUTES_PAST.pairs}+ min, Tríos {MIN_MINUTES_PAST.trios}+ min, Quintetos {MIN_MINUTES_PAST.lineups}+ min.
          </li>
        </ul>
      </div>
    </div>
  )
}
