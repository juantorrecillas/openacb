import { useState, useMemo } from 'react'
import { Search, ArrowUp, ArrowDown, Filter } from 'lucide-react'

// Basic boxscore stats columns - with percentile key for inline display
const basicColumns = [
  { key: 'playerFull', label: 'Player', align: 'left', sortable: true },
  { key: 'team', label: 'Team', align: 'left', sortable: true },
  { key: 'games', label: 'GP', align: 'right', sortable: true },
  { key: 'mpg', label: 'MPG', align: 'right', sortable: true, pctKey: 'mpgPct' },
  { key: 'ppg', label: 'PPG', align: 'right', sortable: true, highlight: true, pctKey: 'ppgPct' },
  { key: 'rpg', label: 'RPG', align: 'right', sortable: true, pctKey: 'rpgPct' },
  { key: 'orebpg', label: 'OREB', align: 'right', sortable: true, pctKey: 'orebpgPct' },
  { key: 'drebpg', label: 'DREB', align: 'right', sortable: true, pctKey: 'drebpgPct' },
  { key: 'apg', label: 'APG', align: 'right', sortable: true, pctKey: 'apgPct' },
  { key: 'spg', label: 'SPG', align: 'right', sortable: true, pctKey: 'spgPct' },
  { key: 'bpg', label: 'BPG', align: 'right', sortable: true, pctKey: 'bpgPct' },
  { key: 'topg', label: 'TOPG', align: 'right', sortable: true, inverse: true, pctKey: 'topgPct' },
  { key: 'fpg', label: 'FPG', align: 'right', sortable: true, inverse: true, pctKey: 'fpgPct' },
  { key: 'fgPct', label: 'FG%', align: 'right', sortable: true, pctKey: 'fgPctPct' },
  { key: 'fg3Pct', label: '3P%', align: 'right', sortable: true, pctKey: 'fg3PctPct' },
  { key: 'ftPct', label: 'FT%', align: 'right', sortable: true, pctKey: 'ftPctPct' },
]

// Advanced stats - shooting efficiency and rate statistics
const advancedColumns = [
  { key: 'playerFull', label: 'Player', align: 'left', sortable: true },
  { key: 'team', label: 'Team', align: 'left', sortable: true },
  { key: 'games', label: 'GP', align: 'right', sortable: true },
  { key: 'ortg', label: 'ORtg', align: 'right', sortable: true, highlight: true, pctKey: 'ortgPct' },
  { key: 'usg', label: 'USG%', align: 'right', sortable: true, pctKey: 'usgPct' },
  { key: 'efg', label: 'eFG%', align: 'right', sortable: true, pctKey: 'efgPct' },
  { key: 'ts', label: 'TS%', align: 'right', sortable: true, pctKey: 'tsPct' },
  { key: 'threeRate', label: '3PAr', align: 'right', sortable: true, pctKey: 'threeRatePct' },
  { key: 'orbPct', label: 'ORB%', align: 'right', sortable: true, pctKey: 'orbPctPct' },
  { key: 'drbPct', label: 'DRB%', align: 'right', sortable: true, pctKey: 'drbPctPct' },
  { key: 'trbPct', label: 'TRB%', align: 'right', sortable: true, pctKey: 'trbPctPct' },
  { key: 'astPct', label: 'AST%', align: 'right', sortable: true, pctKey: 'astPctPct' },
  { key: 'stlPct', label: 'STL%', align: 'right', sortable: true, pctKey: 'stlPctPct' },
  { key: 'blkPct', label: 'BLK%', align: 'right', sortable: true, pctKey: 'blkPctPct' },
  { key: 'tovPct', label: 'TOV%', align: 'right', sortable: true, inverse: true, pctKey: 'tovPctPct' },
]

// Zone shooting frequency columns (% of shots from each zone)
const frequencyColumns = [
  { key: 'playerFull', label: 'Player', align: 'left', sortable: true },
  { key: 'team', label: 'Team', align: 'left', sortable: true },
  { key: 'games', label: 'GP', align: 'right', sortable: true },
  { key: 'mpg', label: 'MPG', align: 'right', sortable: true },
  { key: 'efg', label: 'eFG%', align: 'right', sortable: true },
  { key: 'freqRim', label: 'Rim', align: 'right', sortable: true, zone: true, fgaKey: 'fgaRim' },
  { key: 'freqShortMid', label: 'Short Mid', align: 'right', sortable: true, zone: true, fgaKey: 'fgaShortMid' },
  { key: 'freqLongMid', label: 'Long Mid', align: 'right', sortable: true, zone: true, fgaKey: 'fgaLongMid' },
  { key: 'freqAllMid', label: 'All Mid', align: 'right', sortable: true, zone: true, fgaKey: 'fgaAllMid' },
  { key: 'freqCornerThree', label: 'Corner 3', align: 'right', sortable: true, zone: true, fgaKey: 'fgaCornerThree' },
  { key: 'freqNcThree', label: 'Non-Corner 3', align: 'right', sortable: true, zone: true, fgaKey: 'fgaNcThree' },
  { key: 'freqAllThree', label: 'All 3', align: 'right', sortable: true, zone: true, fgaKey: 'fgaAllThree' },
]

// Zone shooting accuracy columns (FG% per zone)
const accuracyColumns = [
  { key: 'playerFull', label: 'Player', align: 'left', sortable: true },
  { key: 'team', label: 'Team', align: 'left', sortable: true },
  { key: 'games', label: 'GP', align: 'right', sortable: true },
  { key: 'mpg', label: 'MPG', align: 'right', sortable: true },
  { key: 'efg', label: 'eFG%', align: 'right', sortable: true },
  { key: 'fgpctRim', label: 'Rim', align: 'right', sortable: true, zone: true, fgaKey: 'fgaRim' },
  { key: 'fgpctShortMid', label: 'Short Mid', align: 'right', sortable: true, zone: true, fgaKey: 'fgaShortMid' },
  { key: 'fgpctLongMid', label: 'Long Mid', align: 'right', sortable: true, zone: true, fgaKey: 'fgaLongMid' },
  { key: 'fgpctAllMid', label: 'All Mid', align: 'right', sortable: true, zone: true, fgaKey: 'fgaAllMid' },
  { key: 'fgpctCornerThree', label: 'Corner 3', align: 'right', sortable: true, zone: true, fgaKey: 'fgaCornerThree' },
  { key: 'fgpctNcThree', label: 'Non-Corner 3', align: 'right', sortable: true, zone: true, fgaKey: 'fgaNcThree' },
  { key: 'fgpctAllThree', label: 'All 3', align: 'right', sortable: true, zone: true, fgaKey: 'fgaAllThree' },
]

// Opponent zone shooting columns (defensive impact - showing FG% allowed and differential)
const defenseColumns = [
  { key: 'playerFull', label: 'Player', align: 'left', sortable: true },
  { key: 'team', label: 'Team', align: 'left', sortable: true },
  { key: 'games', label: 'GP', align: 'right', sortable: true },
  { key: 'mpg', label: 'MPG', align: 'right', sortable: true },
  { key: 'drbPct', label: 'DRB%', align: 'right', sortable: true },
  { key: 'oppDiffRim', label: 'Rim', align: 'right', sortable: true, defense: true, fgpctKey: 'oppOnFgpctRim', fgaKey: 'oppFgaRim' },
  { key: 'oppDiffShortMid', label: 'Short Mid', align: 'right', sortable: true, defense: true, fgpctKey: 'oppOnFgpctShortMid', fgaKey: 'oppFgaShortMid' },
  { key: 'oppDiffLongMid', label: 'Long Mid', align: 'right', sortable: true, defense: true, fgpctKey: 'oppOnFgpctLongMid', fgaKey: 'oppFgaLongMid' },
  { key: 'oppDiffAllMid', label: 'All Mid', align: 'right', sortable: true, defense: true, fgpctKey: 'oppOnFgpctAllMid', fgaKey: 'oppFgaAllMid' },
  { key: 'oppDiffCornerThree', label: 'Corner 3', align: 'right', sortable: true, defense: true, fgpctKey: 'oppOnFgpctCornerThree', fgaKey: 'oppFgaCornerThree' },
  { key: 'oppDiffNcThree', label: 'Non-Corner 3', align: 'right', sortable: true, defense: true, fgpctKey: 'oppOnFgpctNcThree', fgaKey: 'oppFgaNcThree' },
  { key: 'oppDiffAllThree', label: 'All 3', align: 'right', sortable: true, defense: true, fgpctKey: 'oppOnFgpctAllThree', fgaKey: 'oppFgaAllThree' },
]

export default function PlayerStats({ players }) {
  // Get available seasons and default to most recent
  const availableSeasons = useMemo(() => {
    const seasons = [...new Set(players.map(p => p.season))].sort((a, b) => b - a)
    return seasons
  }, [players])

  const [selectedSeason, setSelectedSeason] = useState(availableSeasons[0] || 2025)
  const [viewMode, setViewMode] = useState('basic') // 'basic', 'advanced', 'frequency', 'accuracy', 'defense'
  const [search, setSearch] = useState('')
  const [sortKey, setSortKey] = useState('playerFull')
  const [sortDir, setSortDir] = useState('asc')
  const [teamFilter, setTeamFilter] = useState('')
  const [showFilteredPlayers, setShowFilteredPlayers] = useState(false)

  // Use the 'qualified' field from R data (pre-calculated with correct thresholds)
  // Falls back to local calculation if field not present
  const mostRecentSeason = availableSeasons[0]

  const meetsMinimumThreshold = (player) => {
    // Use pre-calculated qualified field from R if available
    if (player.qualified !== undefined) {
      return player.qualified
    }
    // Fallback: calculate locally
    // Most recent season: must have BOTH 5+ games AND 50+ minutes
    // Previous seasons: must have BOTH 10+ games AND 150+ minutes
    if (player.season === mostRecentSeason) {
      return player.games >= 5 && player.totalMinutes >= 50
    } else {
      return player.games >= 10 && player.totalMinutes >= 150
    }
  }

  const columns = viewMode === 'basic' ? basicColumns
    : viewMode === 'advanced' ? advancedColumns
    : viewMode === 'frequency' ? frequencyColumns
    : viewMode === 'accuracy' ? accuracyColumns
    : defenseColumns

  // Filter players by season
  const seasonFilteredPlayers = useMemo(() => {
    if (selectedSeason === 'all') return players
    return players.filter(p => p.season === selectedSeason)
  }, [players, selectedSeason])

  const teams = useMemo(() =>
    [...new Set(seasonFilteredPlayers.map(p => p.team))].sort(),
    [seasonFilteredPlayers]
  )

  // Players that meet the minimum threshold (for percentile calculation)
  const qualifiedPlayers = useMemo(() => {
    return seasonFilteredPlayers.filter(meetsMinimumThreshold)
  }, [seasonFilteredPlayers, mostRecentSeason])

  // Count of filtered out players
  const filteredOutCount = useMemo(() => {
    return seasonFilteredPlayers.length - qualifiedPlayers.length
  }, [seasonFilteredPlayers, qualifiedPlayers])

  const filteredPlayers = useMemo(() => {
    // Start with either all players or only qualified players
    const basePlayers = showFilteredPlayers ? seasonFilteredPlayers : qualifiedPlayers

    return basePlayers
      .filter(p => {
        if (search && !p.playerFull?.toLowerCase().includes(search.toLowerCase())) {
          return false
        }
        if (teamFilter && p.team !== teamFilter && teamFilter !== '') return false
        return true
      })
      .sort((a, b) => {
        const aVal = a[sortKey] || 0
        const bVal = b[sortKey] || 0
        if (typeof aVal === 'string') {
          return sortDir === 'desc'
            ? bVal.localeCompare(aVal)
            : aVal.localeCompare(bVal)
        }
        return sortDir === 'desc' ? bVal - aVal : aVal - bVal
      })
  }, [seasonFilteredPlayers, qualifiedPlayers, showFilteredPlayers, search, sortKey, sortDir, teamFilter])

  const handleSort = (key) => {
    if (sortKey === key) {
      setSortDir(sortDir === 'desc' ? 'asc' : 'desc')
    } else {
      setSortKey(key)
      setSortDir('desc')
    }
  }

  const formatValue = (value, key) => {
    if (value === undefined || value === null) return '-'
    if (key === 'playerFull' || key === 'team') return value

    // Integer values
    if (key === 'games') return value

    // Shooting percentages (contain Pct in name)
    if (key === 'fgPct' || key === 'fg2Pct' || key === 'fg3Pct' || key === 'ftPct' ||
        key === 'efg' || key === 'ts') {
      return `${value.toFixed(1)}%`
    }

    // Rate stats (end with Pct but are percentages)
    if (key === 'orbPct' || key === 'drbPct' || key === 'trbPct' ||
        key === 'astPct' || key === 'stlPct' || key === 'blkPct' || key === 'tovPct') {
      return `${value.toFixed(1)}%`
    }

    // Usage and 3PAr
    if (key === 'usg' || key === 'threeRate') {
      return `${value.toFixed(1)}%`
    }

    // Zone frequency stats (% of shots from zone)
    if (key.startsWith('freq')) {
      return `${value.toFixed(1)}%`
    }

    // Zone accuracy stats (FG% per zone)
    if (key.startsWith('fgpct')) {
      return `${value.toFixed(1)}%`
    }

    // Opponent zone shooting (defensive stats)
    if (key.startsWith('oppOnFgpct') || key.startsWith('oppDiff')) {
      const sign = value > 0 ? '+' : ''
      return `${sign}${value.toFixed(1)}%`
    }

    // Offensive Rating (points per 100 possessions)
    if (key === 'ortg') {
      return value.toFixed(1)
    }

    // Per-game stats
    return value.toFixed(1)
  }

  // Calculate league averages for percentile coloring (always based on qualified players only)
  const avgStats = useMemo(() => {
    const numericKeys = columns.filter(c => c.align === 'right' && c.key !== 'games').map(c => c.key)
    const avgs = {}
    numericKeys.forEach(key => {
      const values = qualifiedPlayers.map(p => p[key]).filter(v => v != null)
      avgs[key] = values.reduce((sum, v) => sum + v, 0) / values.length
    })
    return avgs
  }, [qualifiedPlayers, columns])

  const getPercentileColor = (value, key) => {
    if (!avgStats[key]) return ''
    const col = columns.find(c => c.key === key)
    if (!col?.highlight && !col?.inverse) return ''

    const diff = value - avgStats[key]
    const isGood = col.inverse ? diff < 0 : diff > 0

    if (Math.abs(diff) < avgStats[key] * 0.1) return 'text-acb-700'
    return isGood ? 'text-positive font-medium' : 'text-negative'
  }

  // Get percentile badge color based on percentile value (0-100)
  const getPercentileBadgeColor = (percentile) => {
    if (percentile == null || isNaN(percentile)) return 'bg-acb-100 text-acb-600'
    if (percentile >= 75) return 'bg-green-100 text-green-700'   // Top 25%
    if (percentile >= 50) return 'bg-blue-100 text-blue-700'    // Top 50%
    if (percentile >= 25) return 'bg-orange-100 text-orange-700' // Top 75%
    return 'bg-red-100 text-red-700'                            // Bottom 25%
  }

  return (
    <div className="space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-semibold text-acb-900">Player Statistics</h2>
        <p className="text-acb-500 text-sm mt-1">
          Individual player performance metrics - Switch between basic and advanced views
        </p>
      </div>

      {/* Filters */}
      <div className="bg-white rounded-lg border border-acb-200 p-4">
        <div className="flex flex-wrap items-center gap-4 mb-4">
          {/* Season Filter */}
          <div className="flex items-center gap-2">
            <span className="text-sm text-acb-600">Season:</span>
            <select
              value={selectedSeason}
              onChange={(e) => setSelectedSeason(e.target.value === 'all' ? 'all' : parseInt(e.target.value))}
              className="px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
            >
              {availableSeasons.map(season => (
                <option key={season} value={season}>{season-1}-{String(season).slice(-2)}</option>
              ))}
              <option value="all">All Seasons</option>
            </select>
          </div>

          {/* View Mode Toggle */}
          <div className="flex items-center gap-1 bg-acb-100 rounded-md p-1">
            <button
              onClick={() => setViewMode('basic')}
              className={`px-3 py-1.5 text-sm font-medium rounded transition-colors
                ${viewMode === 'basic'
                  ? 'bg-white text-acb-900 shadow-sm'
                  : 'text-acb-600 hover:text-acb-900'}`}
            >
              Basic
            </button>
            <button
              onClick={() => setViewMode('advanced')}
              className={`px-3 py-1.5 text-sm font-medium rounded transition-colors
                ${viewMode === 'advanced'
                  ? 'bg-white text-acb-900 shadow-sm'
                  : 'text-acb-600 hover:text-acb-900'}`}
            >
              Advanced
            </button>
            <button
              onClick={() => setViewMode('frequency')}
              className={`px-3 py-1.5 text-sm font-medium rounded transition-colors
                ${viewMode === 'frequency'
                  ? 'bg-white text-acb-900 shadow-sm'
                  : 'text-acb-600 hover:text-acb-900'}`}
            >
              Frequency
            </button>
            <button
              onClick={() => setViewMode('accuracy')}
              className={`px-3 py-1.5 text-sm font-medium rounded transition-colors
                ${viewMode === 'accuracy'
                  ? 'bg-white text-acb-900 shadow-sm'
                  : 'text-acb-600 hover:text-acb-900'}`}
            >
              Accuracy
            </button>
            <button
              onClick={() => setViewMode('defense')}
              className={`px-3 py-1.5 text-sm font-medium rounded transition-colors
                ${viewMode === 'defense'
                  ? 'bg-white text-acb-900 shadow-sm'
                  : 'text-acb-600 hover:text-acb-900'}`}
            >
              Opp. Shooting
            </button>
          </div>

          {/* Search */}
          <div className="relative flex-1 min-w-[200px]">
            <Search className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-acb-400" />
            <input
              type="text"
              value={search}
              onChange={(e) => setSearch(e.target.value)}
              placeholder="Search players..."
              className="w-full pl-10 pr-4 py-2 border border-acb-200 rounded-md text-sm"
            />
          </div>

          {/* Team Filter */}
          <div className="flex items-center gap-2">
            <Filter className="w-4 h-4 text-acb-400" />
            <select
              value={teamFilter}
              onChange={(e) => setTeamFilter(e.target.value)}
              className="px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
            >
              <option value="">All Teams</option>
              {teams.map(team => (
                <option key={team} value={team}>{team}</option>
              ))}
            </select>
          </div>

          {/* Show Filtered Players Toggle */}
          {filteredOutCount > 0 && (
            <button
              onClick={() => setShowFilteredPlayers(!showFilteredPlayers)}
              className={`px-3 py-2 text-sm border rounded-md transition-colors
                ${showFilteredPlayers
                  ? 'bg-acb-100 border-acb-300 text-acb-700'
                  : 'bg-white border-acb-200 text-acb-500 hover:bg-acb-50'}`}
            >
              {showFilteredPlayers ? 'Hide' : 'Show'} {filteredOutCount} filtered
            </button>
          )}
        </div>
      </div>

      {/* Results count */}
      <div className="text-sm text-acb-500">
        Showing {filteredPlayers.length} of {qualifiedPlayers.length} qualified players
        {filteredOutCount > 0 && !showFilteredPlayers && (
          <span className="text-acb-400"> ({filteredOutCount} below threshold hidden)</span>
        )}
      </div>

      {/* Table */}
      <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
        <div className="overflow-x-auto">
          <table className="w-full">
            <thead>
              <tr className="bg-acb-50 border-b border-acb-200">
                <th className="px-4 py-3 text-left text-xs font-semibold text-acb-600 uppercase tracking-wider w-8">
                  #
                </th>
                {columns.map(col => (
                  <th
                    key={col.key}
                    onClick={() => col.sortable && handleSort(col.key)}
                    className={`px-4 py-3 text-xs font-semibold text-acb-600 uppercase tracking-wider
                      ${col.align === 'right' ? 'text-right' : 'text-left'}
                      ${col.sortable ? 'cursor-pointer hover:bg-acb-100' : ''}`}
                  >
                    <span className="inline-flex items-center gap-1">
                      {col.label}
                      {sortKey === col.key && (
                        sortDir === 'desc' ? <ArrowDown className="w-3 h-3" /> : <ArrowUp className="w-3 h-3" />
                      )}
                    </span>
                  </th>
                ))}
              </tr>
            </thead>
            <tbody>
              {filteredPlayers.slice(0, 100).map((player, i) => (
                <tr
                  key={player.playerId || `${player.player}-${player.team}-${player.season}`}
                  className="border-b border-acb-100 hover:bg-acb-50 transition-colors"
                >
                  <td className="px-4 py-3 text-sm text-acb-400 font-mono">
                    {i + 1}
                  </td>
                  {columns.map(col => {
                    const hasPercentile = col.pctKey && player[col.pctKey] != null
                    const percentileValue = hasPercentile ? player[col.pctKey] : null
                    const hasZoneFga = col.zone && col.fgaKey && player[col.fgaKey] != null
                    const fgaValue = hasZoneFga ? player[col.fgaKey] : null
                    const hasDefense = col.defense && col.fgpctKey && col.fgaKey && player[col.fgpctKey] != null
                    const defenseFgpct = hasDefense ? player[col.fgpctKey] : null
                    const defenseFga = hasDefense ? player[col.fgaKey] : null
                    const diffValue = hasDefense ? player[col.key] : null

                    return (
                      <td
                        key={col.key}
                        className={`px-4 py-3 text-sm whitespace-nowrap
                          ${col.align === 'right' ? 'text-right' : ''}
                          ${col.key === 'playerFull' ? 'font-medium text-acb-900' : ''}
                          ${col.key === 'team' ? 'text-acb-600' : ''}`}
                      >
                        {hasDefense ? (
                          <div className="flex flex-col items-end gap-0.5">
                            <span className={`font-mono ${diffValue < 0 ? 'text-green-700 font-medium' : diffValue > 0 ? 'text-red-700' : 'text-acb-700'}`}>
                              {formatValue(diffValue, col.key)}
                            </span>
                            <span className="text-xs text-acb-400">
                              {defenseFgpct.toFixed(1)}% / {defenseFga}
                            </span>
                          </div>
                        ) : hasZoneFga ? (
                          <div className="flex flex-col items-end gap-0.5">
                            <span className="font-mono text-acb-900">
                              {formatValue(player[col.key], col.key)}
                            </span>
                            <span className="text-xs text-acb-400">
                              {fgaValue}
                            </span>
                          </div>
                        ) : hasPercentile ? (
                          <div className="flex flex-col items-end gap-1">
                            <span className={`font-mono ${getPercentileColor(player[col.key], col.key)}`}>
                              {formatValue(player[col.key], col.key)}
                            </span>
                            <span className={`text-xs px-1.5 py-0.5 rounded ${getPercentileBadgeColor(percentileValue)}`}>
                              {Math.round(percentileValue)}%
                            </span>
                          </div>
                        ) : (
                          <span className={`${col.align === 'right' ? 'font-mono' : ''} ${col.align === 'right' ? getPercentileColor(player[col.key], col.key) : 'text-acb-700'}`}>
                            {formatValue(player[col.key], col.key)}
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

        {filteredPlayers.length > 100 && (
          <div className="px-4 py-3 bg-acb-50 border-t border-acb-200 text-sm text-acb-500 text-center">
            Showing first 100 players. Use filters to narrow results.
          </div>
        )}
      </div>
    </div>
  )
}
