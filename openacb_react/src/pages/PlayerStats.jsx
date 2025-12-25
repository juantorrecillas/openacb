import { useState, useMemo } from 'react'
import { Search, ArrowUp, ArrowDown, Filter } from 'lucide-react'

// Basic boxscore stats columns
const basicColumns = [
  { key: 'player', label: 'Player', align: 'left', sortable: true },
  { key: 'team', label: 'Team', align: 'left', sortable: true },
  { key: 'games', label: 'GP', align: 'right', sortable: true },
  { key: 'mpg', label: 'MPG', align: 'right', sortable: true },
  { key: 'ppg', label: 'PPG', align: 'right', sortable: true, highlight: true },
  { key: 'rpg', label: 'RPG', align: 'right', sortable: true },
  { key: 'apg', label: 'APG', align: 'right', sortable: true },
  { key: 'spg', label: 'SPG', align: 'right', sortable: true },
  { key: 'bpg', label: 'BPG', align: 'right', sortable: true },
  { key: 'topg', label: 'TOPG', align: 'right', sortable: true, inverse: true },
]

// Shooting and efficiency stats
const advancedColumns = [
  { key: 'player', label: 'Player', align: 'left', sortable: true },
  { key: 'team', label: 'Team', align: 'left', sortable: true },
  { key: 'games', label: 'GP', align: 'right', sortable: true },
  { key: 'fgPct', label: 'FG%', align: 'right', sortable: true },
  { key: 'fg2Pct', label: '2P%', align: 'right', sortable: true },
  { key: 'fg3Pct', label: '3P%', align: 'right', sortable: true },
  { key: 'ftPct', label: 'FT%', align: 'right', sortable: true },
  { key: 'efg', label: 'eFG%', align: 'right', sortable: true, highlight: true },
  { key: 'ts', label: 'TS%', align: 'right', sortable: true },
  { key: 'threeRate', label: '3PAr', align: 'right', sortable: true },
  { key: 'possPg', label: 'POSS', align: 'right', sortable: true },
]

// Percentile rankings
const percentileColumns = [
  { key: 'player', label: 'Player', align: 'left', sortable: true },
  { key: 'team', label: 'Team', align: 'left', sortable: true },
  { key: 'ppgPct', label: 'PPG', align: 'right', sortable: true, highlight: true },
  { key: 'rpgPct', label: 'RPG', align: 'right', sortable: true },
  { key: 'apgPct', label: 'APG', align: 'right', sortable: true },
  { key: 'spgPct', label: 'SPG', align: 'right', sortable: true },
  { key: 'bpgPct', label: 'BPG', align: 'right', sortable: true },
  { key: 'topgPct', label: 'TOV', align: 'right', sortable: true },
  { key: 'efgPct', label: 'eFG%', align: 'right', sortable: true },
  { key: 'fg3PctPct', label: '3P%', align: 'right', sortable: true },
  { key: 'mpgPct', label: 'MPG', align: 'right', sortable: true },
  { key: 'possPgPct', label: 'POSS', align: 'right', sortable: true },
]

export default function PlayerStats({ players }) {
  // Get available seasons and default to most recent
  const availableSeasons = useMemo(() => {
    const seasons = [...new Set(players.map(p => p.season))].sort((a, b) => b - a)
    return seasons
  }, [players])

  const [selectedSeason, setSelectedSeason] = useState(availableSeasons[0] || 2025)
  const [viewMode, setViewMode] = useState('basic') // 'basic', 'advanced', or 'percentiles'
  const [search, setSearch] = useState('')
  const [sortKey, setSortKey] = useState('ppg')
  const [sortDir, setSortDir] = useState('desc')
  const [teamFilter, setTeamFilter] = useState('')

  const columns = viewMode === 'basic' ? basicColumns :
                  viewMode === 'advanced' ? advancedColumns :
                  percentileColumns

  // Filter players by season
  const seasonFilteredPlayers = useMemo(() => {
    if (selectedSeason === 'all') return players
    return players.filter(p => p.season === selectedSeason)
  }, [players, selectedSeason])

  const teams = useMemo(() =>
    [...new Set(seasonFilteredPlayers.map(p => p.team))].sort(),
    [seasonFilteredPlayers]
  )
  
  const filteredPlayers = useMemo(() => {
    return seasonFilteredPlayers
      .filter(p => {
        if (search && !p.player.toLowerCase().includes(search.toLowerCase()) &&
            !p.playerFull?.toLowerCase().includes(search.toLowerCase())) {
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
  }, [seasonFilteredPlayers, search, sortKey, sortDir, teamFilter])
  
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
    if (key === 'player' || key === 'team') return value

    // Integer values
    if (key === 'games') return value

    // Percentile columns (end with Pct suffix in percentile view)
    if (key.endsWith('Pct') && viewMode === 'percentiles') {
      return `${Math.round(value)}`
    }

    // Shooting percentages (contain Pct in name but not percentile view)
    if (key === 'fgPct' || key === 'fg2Pct' || key === 'fg3Pct' || key === 'ftPct' ||
        key === 'efg' || key === 'ts' || key === 'threeRate') {
      return `${value.toFixed(1)}%`
    }

    // Per-game stats
    return value.toFixed(1)
  }
  
  // Calculate league averages for percentile coloring
  const avgStats = useMemo(() => {
    const numericKeys = columns.filter(c => c.align === 'right' && c.key !== 'games').map(c => c.key)
    const avgs = {}
    numericKeys.forEach(key => {
      const values = filteredPlayers.map(p => p[key]).filter(v => v != null)
      avgs[key] = values.reduce((sum, v) => sum + v, 0) / values.length
    })
    return avgs
  }, [filteredPlayers])
  
  const getPercentileColor = (value, key) => {
    if (!avgStats[key]) return ''
    const col = columns.find(c => c.key === key)
    if (!col?.highlight && !col?.inverse) return ''
    
    const diff = value - avgStats[key]
    const isGood = col.inverse ? diff < 0 : diff > 0
    
    if (Math.abs(diff) < avgStats[key] * 0.1) return 'text-acb-700'
    return isGood ? 'text-positive font-medium' : 'text-negative'
  }

  return (
    <div className="space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-semibold text-acb-900">Player Statistics</h2>
        <p className="text-acb-500 text-sm mt-1">
          Individual player performance metrics - Switch between basic, advanced, and percentile views
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
              onClick={() => {
                setViewMode('basic')
                setSortKey('ppg')
              }}
              className={`px-3 py-1.5 text-sm font-medium rounded transition-colors
                ${viewMode === 'basic'
                  ? 'bg-white text-acb-900 shadow-sm'
                  : 'text-acb-600 hover:text-acb-900'}`}
            >
              Basic
            </button>
            <button
              onClick={() => {
                setViewMode('advanced')
                setSortKey('efg')
              }}
              className={`px-3 py-1.5 text-sm font-medium rounded transition-colors
                ${viewMode === 'advanced'
                  ? 'bg-white text-acb-900 shadow-sm'
                  : 'text-acb-600 hover:text-acb-900'}`}
            >
              Shooting
            </button>
            <button
              onClick={() => {
                setViewMode('percentiles')
                setSortKey('ppgPct')
              }}
              className={`px-3 py-1.5 text-sm font-medium rounded transition-colors
                ${viewMode === 'percentiles'
                  ? 'bg-white text-acb-900 shadow-sm'
                  : 'text-acb-600 hover:text-acb-900'}`}
            >
              Percentiles
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
        </div>
      </div>

      {/* Results count */}
      <div className="text-sm text-acb-500">
        Showing {filteredPlayers.length} of {seasonFilteredPlayers.length} players
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
                  key={`${player.player}-${player.team}`}
                  className="border-b border-acb-100 hover:bg-acb-50 transition-colors"
                >
                  <td className="px-4 py-3 text-sm text-acb-400 font-mono">
                    {i + 1}
                  </td>
                  {columns.map(col => (
                    <td
                      key={col.key}
                      className={`px-4 py-3 text-sm whitespace-nowrap
                        ${col.align === 'right' ? 'text-right font-mono' : ''}
                        ${col.key === 'player' ? 'font-medium text-acb-900' : ''}
                        ${col.key === 'team' ? 'text-acb-600' : ''}
                        ${col.align === 'right' ? getPercentileColor(player[col.key], col.key) : 'text-acb-700'}`}
                    >
                      {formatValue(player[col.key], col.key)}
                    </td>
                  ))}
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
