import React, { useState, useMemo, useEffect } from 'react'
import { Users, Info, Plus, X, Search, ChevronDown, ChevronUp } from 'lucide-react'

/**
 * Lineup Analysis Page - Cleaning the Glass Style
 *
 * Allows users to select specific player combinations and see their on/off court impact.
 * Uses pre-calculated data from R for instant performance.
 */

export default function LineupAnalysis({ teams, players }) {
  // State for data and UI
  const [lineupData, setLineupData] = useState(null)
  const [loading, setLoading] = useState(true)
  const [useMockData, setUseMockData] = useState(false)
  const [showAllPlayers, setShowAllPlayers] = useState(false)
  const [sortConfig, setSortConfig] = useState({ key: 'netDiff', direction: 'desc' })

  // Available seasons and teams
  const availableSeasons = useMemo(() => {
    const seasons = [...new Set(teams.map(t => t.season))].sort((a, b) => b - a)
    return seasons
  }, [teams])

  const [selectedSeason, setSelectedSeason] = useState(availableSeasons[0] || 2025)
  const [selectedTeam, setSelectedTeam] = useState(teams[0]?.team || '')

  // Player selection state
  const [selectedPlayers, setSelectedPlayers] = useState([])
  const [searchQuery, setSearchQuery] = useState('')

  // Load lineup data
  useEffect(() => {
    async function loadLineupData() {
      try {
        const response = await fetch('/data/lineups.json')
        if (!response.ok) throw new Error('Lineup data not found')

        const data = await response.json()
        setLineupData(data)
        setUseMockData(false)
      } catch (err) {
        console.warn('Using mock data:', err.message)
        setUseMockData(true)
      } finally {
        setLoading(false)
      }
    }

    loadLineupData()
  }, [])

  // Filter teams by season
  const seasonFilteredTeams = useMemo(() => {
    if (selectedSeason === 'all') return teams
    return teams.filter(t => t.season === selectedSeason)
  }, [teams, selectedSeason])

  const teamList = useMemo(() => seasonFilteredTeams.map(t => t.team).sort(), [seasonFilteredTeams])

  // Get current team data key
  const currentDataKey = useMemo(() => {
    if (!lineupData?.data) return null
    return Object.keys(lineupData.data).find(key =>
      key.startsWith(`${selectedSeason}_`) && lineupData.data[key].team === selectedTeam
    )
  }, [lineupData, selectedSeason, selectedTeam])

  // Get available players for current team
  const availablePlayers = useMemo(() => {
    if (!lineupData?.data || useMockData || !currentDataKey) return []

    if (lineupData.data[currentDataKey]?.players) {
      return Object.keys(lineupData.data[currentDataKey].players).sort()
    }
    return []
  }, [lineupData, useMockData, currentDataKey])

  // Get all players data for the table
  const allPlayersData = useMemo(() => {
    if (!lineupData?.data || !currentDataKey) return []

    const playersObj = lineupData.data[currentDataKey]?.players || {}
    return Object.entries(playersObj).map(([key, player]) => ({
      key,
      name: player.name || key,
      ...player
    }))
  }, [lineupData, currentDataKey])

  // Sorted players data
  const sortedPlayersData = useMemo(() => {
    const sorted = [...allPlayersData]
    sorted.sort((a, b) => {
      const aVal = a[sortConfig.key] ?? 0
      const bVal = b[sortConfig.key] ?? 0
      return sortConfig.direction === 'asc' ? aVal - bVal : bVal - aVal
    })
    return sorted
  }, [allPlayersData, sortConfig])

  // Filter players by search query
  const filteredPlayers = useMemo(() => {
    if (searchQuery.trim() === '') return availablePlayers
    return availablePlayers.filter(player =>
      player.toLowerCase().includes(searchQuery.toLowerCase())
    )
  }, [availablePlayers, searchQuery])

  // Get data for selected players
  const getLineupDataForPlayers = () => {
    if (!lineupData?.data || useMockData || selectedPlayers.length === 0 || !currentDataKey) return null

    const teamData = lineupData.data[currentDataKey]
    const sortedPlayers = [...selectedPlayers].sort()
    const playerKey = sortedPlayers.join('_')

    if (selectedPlayers.length === 1) {
      return teamData.players?.[selectedPlayers[0]] || null
    } else if (selectedPlayers.length === 2) {
      return teamData.pairs?.[playerKey] || null
    } else if (selectedPlayers.length >= 3) {
      return teamData.trios?.[playerKey] || null
    }
    return null
  }

  const currentLineupData = getLineupDataForPlayers()

  // Player selection handlers
  const addPlayer = (player) => {
    if (selectedPlayers.includes(player) || selectedPlayers.length >= 5) return
    setSelectedPlayers([...selectedPlayers, player])
  }

  const removePlayer = (player) => {
    setSelectedPlayers(selectedPlayers.filter(p => p !== player))
  }

  const clearPlayers = () => setSelectedPlayers([])

  // Sort handler
  const handleSort = (key) => {
    setSortConfig(prev => ({
      key,
      direction: prev.key === key && prev.direction === 'desc' ? 'asc' : 'desc'
    }))
  }

  // Performance indicator helper
  const getPerformanceIndicator = (value, threshold = 0, inverse = false) => {
    if (value == null) return { emoji: '➖', label: 'N/A', color: 'text-acb-400' }

    const adjusted = inverse ? -value : value
    if (adjusted > threshold + 5) return { emoji: '🔥', label: 'Elite', color: 'text-green-600' }
    if (adjusted > threshold + 2) return { emoji: '✅', label: 'Good', color: 'text-green-500' }
    if (adjusted > threshold - 2) return { emoji: '➖', label: 'Average', color: 'text-acb-500' }
    if (adjusted > threshold - 5) return { emoji: '⚠️', label: 'Below Avg', color: 'text-amber-500' }
    return { emoji: '🔻', label: 'Poor', color: 'text-red-500' }
  }

  // Rating color helper
  const getRatingColor = (value, isDefensive = false) => {
    if (value == null) return 'text-acb-400'
    const threshold = isDefensive ? 105 : 110
    const good = isDefensive ? value < threshold : value > threshold
    const great = isDefensive ? value < threshold - 5 : value > threshold + 5

    if (great) return isDefensive ? 'text-green-600' : 'text-green-600'
    if (good) return isDefensive ? 'text-green-500' : 'text-green-500'
    return isDefensive ? 'text-red-500' : 'text-red-500'
  }

  if (loading) {
    return (
      <div className="space-y-6">
        <div>
          <h2 className="text-2xl font-semibold text-acb-900">Lineup Analysis</h2>
          <p className="text-acb-500 text-sm mt-1">Loading player combination data...</p>
        </div>
        <div className="bg-acb-50 rounded-lg p-8 text-center">
          <div className="animate-pulse">
            <div className="text-acb-600">Loading lineup analysis data...</div>
          </div>
        </div>
      </div>
    )
  }

  return (
    <div className="space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-semibold text-acb-900">Lineup Analysis</h2>
        <p className="text-acb-500 text-sm mt-1">
          Analyze player on/off court impact and combination performance
        </p>
      </div>

      {/* Controls */}
      <div className="bg-white rounded-lg border border-acb-200 p-4 space-y-4">
        <div className="flex flex-wrap items-center gap-4">
          {/* Season Filter */}
          <div className="flex items-center gap-2">
            <span className="text-sm text-acb-600 font-medium">Season:</span>
            <select
              value={selectedSeason}
              onChange={(e) => {
                setSelectedSeason(e.target.value === 'all' ? 'all' : parseInt(e.target.value))
                clearPlayers()
              }}
              className="px-3 py-2 border border-acb-200 rounded-md text-sm bg-white font-medium"
            >
              {availableSeasons.map(season => (
                <option key={season} value={season}>{season-1}-{String(season).slice(-2)}</option>
              ))}
            </select>
          </div>

          {/* Team Filter */}
          <div className="flex items-center gap-2">
            <Users className="w-4 h-4 text-acb-400" />
            <select
              value={selectedTeam}
              onChange={(e) => {
                setSelectedTeam(e.target.value)
                clearPlayers()
              }}
              className="px-3 py-2 border border-acb-200 rounded-md text-sm bg-white font-medium min-w-[200px]"
            >
              {teamList.map(team => (
                <option key={team} value={team}>{team}</option>
              ))}
            </select>
          </div>
        </div>

        {/* Player Selection */}
        <div className="space-y-3">
          <div className="flex items-center gap-2">
            <Search className="w-4 h-4 text-acb-400" />
            <input
              type="text"
              placeholder="Search players..."
              value={searchQuery}
              onChange={(e) => setSearchQuery(e.target.value)}
              className="flex-1 px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
            />
          </div>

          {/* Selected Players Chips */}
          {selectedPlayers.length > 0 && (
            <div className="flex flex-wrap gap-2 items-center">
              <span className="text-sm text-acb-600 font-medium">Analyzing:</span>
              {selectedPlayers.map(player => (
                <div key={player} className="flex items-center gap-1 bg-orange-100 text-orange-800 rounded-full px-3 py-1">
                  <span className="text-sm font-medium">{player}</span>
                  <button onClick={() => removePlayer(player)} className="hover:text-orange-600">
                    <X className="w-3 h-3" />
                  </button>
                </div>
              ))}
              <button onClick={clearPlayers} className="text-sm text-acb-500 hover:text-acb-700">
                Clear all
              </button>
            </div>
          )}

          {/* Player Grid */}
          <div className="max-h-48 overflow-y-auto border border-acb-100 rounded-md bg-acb-50/50">
            {filteredPlayers.length > 0 ? (
              <div className="grid grid-cols-2 sm:grid-cols-3 md:grid-cols-4 lg:grid-cols-5 gap-1 p-2">
                {filteredPlayers.map(player => (
                  <button
                    key={player}
                    onClick={() => addPlayer(player)}
                    disabled={selectedPlayers.includes(player) || selectedPlayers.length >= 5}
                    className={`px-2 py-1.5 text-sm rounded transition-all ${
                      selectedPlayers.includes(player)
                        ? 'bg-orange-100 text-orange-700 font-medium'
                        : selectedPlayers.length >= 5
                          ? 'bg-acb-100 text-acb-400 cursor-not-allowed'
                          : 'bg-white hover:bg-orange-50 text-acb-700 hover:text-orange-700 border border-acb-200'
                    }`}
                  >
                    {player}
                  </button>
                ))}
              </div>
            ) : (
              <div className="p-4 text-center text-acb-400 text-sm">
                No players found{searchQuery && ` for "${searchQuery}"`}
              </div>
            )}
          </div>
        </div>
      </div>

      {/* Individual Player Analysis Results */}
      {selectedPlayers.length === 1 && currentLineupData && (
        <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
          <div className="bg-gradient-to-r from-acb-700 to-acb-800 px-4 py-3">
            <h3 className="font-semibold text-white text-lg">
              {currentLineupData.name || selectedPlayers[0]}
            </h3>
            <p className="text-acb-200 text-sm">
              {currentLineupData.onPoss} possessions on court • {currentLineupData.offPoss} off court
            </p>
          </div>

          {/* Stats Table */}
          <div className="overflow-x-auto">
            <table className="w-full">
              <thead>
                <tr className="bg-acb-50 text-left text-xs text-acb-600 uppercase tracking-wider">
                  <th className="px-4 py-3 font-semibold">Metric</th>
                  <th className="px-4 py-3 font-semibold text-center">On Court</th>
                  <th className="px-4 py-3 font-semibold text-center">Off Court</th>
                  <th className="px-4 py-3 font-semibold text-center">Difference</th>
                  <th className="px-4 py-3 font-semibold text-center">Impact</th>
                </tr>
              </thead>
              <tbody className="divide-y divide-acb-100">
                {/* Ratings */}
                <StatRow
                  label="Offensive Rating"
                  onValue={currentLineupData.onORtg}
                  offValue={currentLineupData.offORtg}
                  unit="pts/100"
                  goodThreshold={110}
                />
                <StatRow
                  label="Defensive Rating"
                  onValue={currentLineupData.onDRtg}
                  offValue={currentLineupData.offDRtg}
                  unit="pts/100"
                  goodThreshold={105}
                  inverse
                />
                <StatRow
                  label="Net Rating"
                  onValue={currentLineupData.onNetRtg}
                  offValue={currentLineupData.offNetRtg}
                  unit="pts/100"
                  goodThreshold={0}
                  highlight
                />
                {/* Four Factors - Shooting */}
                <StatRow
                  label="eFG%"
                  onValue={currentLineupData.onEFG}
                  offValue={currentLineupData.offEFG}
                  unit="%"
                  goodThreshold={50}
                />
                {/* Four Factors - Turnovers */}
                <StatRow
                  label="TOV%"
                  onValue={currentLineupData.onTOV}
                  offValue={currentLineupData.offTOV}
                  unit="%"
                  goodThreshold={15}
                  inverse
                />
                {/* Four Factors - Rebounding */}
                <StatRow
                  label="DRB%"
                  onValue={currentLineupData.onDRB}
                  offValue={currentLineupData.offDRB}
                  unit="%"
                  goodThreshold={70}
                />
                {/* Assist Rate */}
                <StatRow
                  label="AST%"
                  onValue={currentLineupData.onAST}
                  offValue={currentLineupData.offAST}
                  unit="%"
                  goodThreshold={50}
                />
              </tbody>
            </table>
          </div>

          {/* Impact Summary */}
          <div className="p-4 bg-acb-50 border-t border-acb-200">
            <div className="flex items-center justify-center gap-4">
              <div className="text-center">
                <div className="text-3xl mb-1">
                  {getPerformanceIndicator(currentLineupData.netDiff).emoji}
                </div>
                <div className={`text-2xl font-bold font-mono ${
                  currentLineupData.netDiff > 0 ? 'text-green-600' :
                  currentLineupData.netDiff < 0 ? 'text-red-500' : 'text-acb-500'
                }`}>
                  {currentLineupData.netDiff > 0 ? '+' : ''}{currentLineupData.netDiff?.toFixed(1)}
                </div>
                <div className="text-sm text-acb-600 mt-1">Net Rating Impact</div>
              </div>
            </div>
          </div>
        </div>
      )}

      {/* Pair/Trio Analysis Results */}
      {selectedPlayers.length > 1 && currentLineupData && (
        <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
          <div className="bg-gradient-to-r from-acb-700 to-acb-800 px-4 py-3">
            <h3 className="font-semibold text-white text-lg">
              {selectedPlayers.length === 2 ? 'Duo' : 'Trio'} Analysis
            </h3>
            <p className="text-acb-200 text-sm">
              {selectedPlayers.join(' + ')} • {currentLineupData.onPoss} possessions together
            </p>
          </div>

          {/* Main Ratings */}
          <div className="grid grid-cols-3 divide-x divide-acb-200">
            <div className="p-4 text-center">
              <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">Offensive Rtg</div>
              <div className={`text-2xl font-bold font-mono ${getRatingColor(currentLineupData.onORtg)}`}>
                {currentLineupData.onORtg?.toFixed(1)}
              </div>
            </div>
            <div className="p-4 text-center">
              <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">Defensive Rtg</div>
              <div className={`text-2xl font-bold font-mono ${getRatingColor(currentLineupData.onDRtg, true)}`}>
                {currentLineupData.onDRtg?.toFixed(1)}
              </div>
            </div>
            <div className="p-4 text-center bg-acb-50">
              <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">Net Rating</div>
              <div className={`text-2xl font-bold font-mono ${
                currentLineupData.onNetRtg > 0 ? 'text-green-600' :
                currentLineupData.onNetRtg < 0 ? 'text-red-500' : 'text-acb-500'
              }`}>
                {currentLineupData.onNetRtg > 0 ? '+' : ''}{currentLineupData.onNetRtg?.toFixed(1)}
              </div>
              <div className="text-lg mt-1">
                {getPerformanceIndicator(currentLineupData.onNetRtg).emoji}
              </div>
            </div>
          </div>

          {/* Four Factors */}
          <div className="grid grid-cols-4 divide-x divide-acb-200 border-t border-acb-200 bg-acb-50">
            <div className="p-3 text-center">
              <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">eFG%</div>
              <div className="text-lg font-semibold font-mono text-acb-700">
                {currentLineupData.onEFG?.toFixed(1)}%
              </div>
            </div>
            <div className="p-3 text-center">
              <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">TOV%</div>
              <div className="text-lg font-semibold font-mono text-acb-700">
                {currentLineupData.onTOV?.toFixed(1)}%
              </div>
            </div>
            <div className="p-3 text-center">
              <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">DRB%</div>
              <div className="text-lg font-semibold font-mono text-acb-700">
                {currentLineupData.onDRB?.toFixed(1)}%
              </div>
            </div>
            <div className="p-3 text-center">
              <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">AST%</div>
              <div className="text-lg font-semibold font-mono text-acb-700">
                {currentLineupData.onAST?.toFixed(1)}%
              </div>
            </div>
          </div>
        </div>
      )}

      {/* No Data Found */}
      {selectedPlayers.length > 0 && !currentLineupData && !loading && (
        <div className="bg-amber-50 border border-amber-200 rounded-lg p-4 flex gap-3">
          <span className="text-xl">⚠️</span>
          <div>
            <p className="font-medium text-amber-800">No data available</p>
            <p className="text-sm text-amber-700">
              {selectedPlayers.length > 2
                ? "This trio combination may not have played enough possessions together."
                : "This player combination may not have sufficient sample size."}
            </p>
          </div>
        </div>
      )}

      {/* Team Overview Table */}
      {allPlayersData.length > 0 && (
        <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
          <div className="px-4 py-3 border-b border-acb-200 flex items-center justify-between">
            <h3 className="font-semibold text-acb-900">Team On/Off Overview</h3>
            <button
              onClick={() => setShowAllPlayers(!showAllPlayers)}
              className="text-sm text-acb-600 hover:text-acb-800 flex items-center gap-1"
            >
              {showAllPlayers ? 'Show Less' : 'Show All'}
              {showAllPlayers ? <ChevronUp className="w-4 h-4" /> : <ChevronDown className="w-4 h-4" />}
            </button>
          </div>

          <div className="overflow-x-auto">
            <table className="w-full text-sm">
              <thead>
                <tr className="bg-acb-50 text-left text-xs text-acb-600 uppercase tracking-wider">
                  <th className="px-4 py-3 font-semibold">Player</th>
                  <SortableHeader label="ORtg On" sortKey="onORtg" current={sortConfig} onSort={handleSort} />
                  <SortableHeader label="ORtg Off" sortKey="offORtg" current={sortConfig} onSort={handleSort} />
                  <SortableHeader label="DRtg On" sortKey="onDRtg" current={sortConfig} onSort={handleSort} />
                  <SortableHeader label="DRtg Off" sortKey="offDRtg" current={sortConfig} onSort={handleSort} />
                  <SortableHeader label="Net On" sortKey="onNetRtg" current={sortConfig} onSort={handleSort} />
                  <SortableHeader label="Net Off" sortKey="offNetRtg" current={sortConfig} onSort={handleSort} />
                  <SortableHeader label="Impact" sortKey="netDiff" current={sortConfig} onSort={handleSort} highlight />
                  <SortableHeader label="eFG%" sortKey="onEFG" current={sortConfig} onSort={handleSort} />
                  <SortableHeader label="TOV%" sortKey="onTOV" current={sortConfig} onSort={handleSort} />
                  <SortableHeader label="DRB%" sortKey="onDRB" current={sortConfig} onSort={handleSort} />
                  <SortableHeader label="AST%" sortKey="onAST" current={sortConfig} onSort={handleSort} />
                  <th className="px-4 py-3 font-semibold text-center">Poss</th>
                </tr>
              </thead>
              <tbody className="divide-y divide-acb-100">
                {(showAllPlayers ? sortedPlayersData : sortedPlayersData.slice(0, 8)).map((player) => (
                  <tr
                    key={player.key}
                    className={`hover:bg-acb-50 cursor-pointer transition-colors ${
                      selectedPlayers.includes(player.key) ? 'bg-orange-50' : ''
                    }`}
                    onClick={() => {
                      if (selectedPlayers.includes(player.key)) {
                        removePlayer(player.key)
                      } else {
                        setSelectedPlayers([player.key])
                      }
                    }}
                  >
                    <td className="px-4 py-2 font-medium text-acb-900">
                      {player.name}
                    </td>
                    <td className={`px-4 py-2 text-center font-mono ${getRatingColor(player.onORtg)}`}>
                      {player.onORtg?.toFixed(1)}
                    </td>
                    <td className="px-4 py-2 text-center font-mono text-acb-500">
                      {player.offORtg?.toFixed(1)}
                    </td>
                    <td className={`px-4 py-2 text-center font-mono ${getRatingColor(player.onDRtg, true)}`}>
                      {player.onDRtg?.toFixed(1)}
                    </td>
                    <td className="px-4 py-2 text-center font-mono text-acb-500">
                      {player.offDRtg?.toFixed(1)}
                    </td>
                    <td className={`px-4 py-2 text-center font-mono ${
                      player.onNetRtg > 0 ? 'text-green-600' : 'text-red-500'
                    }`}>
                      {player.onNetRtg > 0 ? '+' : ''}{player.onNetRtg?.toFixed(1)}
                    </td>
                    <td className="px-4 py-2 text-center font-mono text-acb-500">
                      {player.offNetRtg > 0 ? '+' : ''}{player.offNetRtg?.toFixed(1)}
                    </td>
                    <td className={`px-4 py-2 text-center font-mono font-semibold ${
                      player.netDiff > 2 ? 'text-green-600' :
                      player.netDiff < -2 ? 'text-red-500' : 'text-acb-500'
                    }`}>
                      <span className="mr-1">{getPerformanceIndicator(player.netDiff).emoji}</span>
                      {player.netDiff > 0 ? '+' : ''}{player.netDiff?.toFixed(1)}
                    </td>
                    <td className="px-4 py-2 text-center font-mono text-acb-600">
                      {player.onEFG?.toFixed(1)}
                    </td>
                    <td className="px-4 py-2 text-center font-mono text-acb-600">
                      {player.onTOV?.toFixed(1)}
                    </td>
                    <td className="px-4 py-2 text-center font-mono text-acb-600">
                      {player.onDRB?.toFixed(1)}
                    </td>
                    <td className="px-4 py-2 text-center font-mono text-acb-600">
                      {player.onAST?.toFixed(1)}
                    </td>
                    <td className="px-4 py-2 text-center font-mono text-acb-400 text-xs">
                      {player.onPoss}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      )}

      {/* Legend */}
      <div className="flex items-center justify-center gap-6 text-xs text-acb-500 flex-wrap bg-acb-50 rounded-lg p-3">
        <span className="flex items-center gap-1">🔥 Elite (top tier)</span>
        <span className="flex items-center gap-1">✅ Good (above avg)</span>
        <span className="flex items-center gap-1">➖ Average</span>
        <span className="flex items-center gap-1">⚠️ Below average</span>
        <span className="flex items-center gap-1">🔻 Poor</span>
      </div>
    </div>
  )
}

// Stat Row Component for the individual player table
const StatRow = ({ label, onValue, offValue, unit, goodThreshold, inverse = false, highlight = false }) => {
  if (onValue == null || offValue == null) return null

  // Always calculate difference as (on - off)
  const diff = onValue - offValue

  // For inverse stats (TOV%, DRtg), negative diff is good (player reduces the bad stat)
  // For normal stats (eFG%, ORtg), positive diff is good (player increases the good stat)
  const isGood = inverse ? diff < 0 : diff > 0

  const getIndicator = (val, threshold, inv) => {
    const adjusted = inv ? threshold - val : val - threshold
    if (adjusted > 5) return { emoji: '🔥', color: 'text-green-600' }
    if (adjusted > 2) return { emoji: '✅', color: 'text-green-500' }
    if (adjusted > -2) return { emoji: '➖', color: 'text-acb-500' }
    if (adjusted > -5) return { emoji: '⚠️', color: 'text-amber-500' }
    return { emoji: '🔻', color: 'text-red-500' }
  }

  const indicator = getIndicator(diff, 0, inverse)

  return (
    <tr className={highlight ? 'bg-acb-50' : ''}>
      <td className={`px-4 py-3 ${highlight ? 'font-semibold' : ''} text-acb-700`}>
        {label}
      </td>
      <td className="px-4 py-3 text-center">
        <span className={`font-mono font-medium ${
          inverse
            ? (onValue < goodThreshold ? 'text-green-600' : 'text-red-500')
            : (onValue > goodThreshold ? 'text-green-600' : 'text-red-500')
        }`}>
          {onValue?.toFixed(1)}
        </span>
      </td>
      <td className="px-4 py-3 text-center font-mono text-acb-500">
        {offValue?.toFixed(1)}
      </td>
      <td className="px-4 py-3 text-center">
        <span className={`font-mono font-medium ${isGood ? 'text-green-600' : 'text-red-500'}`}>
          {diff > 0 ? '+' : ''}{diff.toFixed(1)}
        </span>
      </td>
      <td className="px-4 py-3 text-center text-lg">
        {indicator.emoji}
      </td>
    </tr>
  )
}

// Sortable Header Component
const SortableHeader = ({ label, sortKey, current, onSort, highlight = false }) => {
  const isActive = current.key === sortKey

  return (
    <th
      className={`px-4 py-3 font-semibold text-center cursor-pointer hover:bg-acb-100 transition-colors ${
        highlight ? 'bg-orange-50' : ''
      } ${isActive ? 'text-orange-600' : ''}`}
      onClick={() => onSort(sortKey)}
    >
      <div className="flex items-center justify-center gap-1">
        {label}
        {isActive && (
          current.direction === 'desc'
            ? <ChevronDown className="w-3 h-3" />
            : <ChevronUp className="w-3 h-3" />
        )}
      </div>
    </th>
  )
}
