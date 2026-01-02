import { useState, useMemo } from 'react'
import { ScatterChart, Scatter, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, ReferenceLine, LabelList } from 'recharts'
import { ArrowUpDown, ArrowUp, ArrowDown } from 'lucide-react'

// Add a color palette for teams (or generate colors dynamically)
const TEAM_COLORS = [
  '#3B82F6', // blue
  '#EF4444', // red
  '#10B981', // green
  '#F59E0B', // yellow
  '#8B5CF6', // purple
  '#EC4899', // pink
  '#14B8A6', // teal
  '#F97316', // orange
  '#6366F1', // indigo
  '#84CC16', // lime
  '#06B6D4', // cyan
  '#D946EF', // fuchsia
  '#0EA5E9', // sky blue
  '#22C55E', // emerald
  '#A855F7', // violet
  '#EAB308', // amber
  '#F43F5E', // rose
  '#06D6A0', // sea green
  '#FF6B6B', // coral
  '#4ECDC4', // turquoise
]

const statOptions = [
  // Team boxscore stats
  { value: 'ppg', label: 'Points/Game', format: 'decimal' },
  { value: 'rpg', label: 'Rebounds/Game', format: 'decimal' },
  { value: 'apg', label: 'Assists/Game', format: 'decimal' },
  { value: 'topg', label: 'Turnovers/Game', format: 'decimal' },
  { value: 'spg', label: 'Steals/Game', format: 'decimal' },
  { value: 'bpg', label: 'Blocks/Game', format: 'decimal' },
  { value: 'fg3mPg', label: '3PM/Game', format: 'decimal' },
  { value: 'fgPct', label: 'FG%', format: 'decimal' },
  { value: 'ftPct', label: 'FT%', format: 'decimal' },
  // Team advanced stats
  { value: 'ortg', label: 'Offensive Rating', format: 'decimal' },
  { value: 'drtg', label: 'Defensive Rating', format: 'decimal' },
  { value: 'netRtg', label: 'Net Rating', format: 'decimal' },
  { value: 'pace', label: 'Pace', format: 'decimal' },
  { value: 'efg', label: 'eFG%', format: 'pct'},
  { value: 'ts', label: 'True Shooting %', format: 'pct' },
  { value: 'threePct', label: '3P%', format: 'pct' },
  { value: 'threeRate', label: '3P Rate', format: 'pct' },
  { value: 'astRate', label: 'Assist Rate', format: 'pct' },
  { value: 'tovRate', label: 'Turnover Rate', format: 'pct' },
  { value: 'orbPct', label: 'ORB%', format: 'pct' },
  { value: 'drbPct', label: 'DRB%', format: 'pct' },
  { value: 'ftRate', label: 'FT Rate', format: 'decimal' },
  { value: 'stlRate', label: 'Steal Rate', format: 'pct' },
  { value: 'blkRate', label: 'Block Rate', format: 'pct' },
  // Opponent boxscore stats
  { value: 'opp_ppg', label: 'Opp PPG', format: 'decimal' },
  { value: 'opp_rpg', label: 'Opp RPG', format: 'decimal' },
  { value: 'opp_apg', label: 'Opp APG', format: 'decimal' },
  { value: 'opp_fgPct', label: 'Opp FG%', format: 'decimal' },
  // Opponent advanced stats
  { value: 'opp_ortg', label: 'Opp Offensive Rating', format: 'decimal' },
  { value: 'opp_drtg', label: 'Opp Defensive Rating', format: 'decimal' },
  { value: 'opp_efg', label: 'Opp eFG%', format: 'pct' },
  { value: 'opp_ts', label: 'Opp True Shooting %', format: 'pct' },
  { value: 'opp_threePct', label: 'Opp 3P%', format: 'pct' },
  { value: 'opp_threeRate', label: 'Opp 3P Rate', format: 'pct' },
  { value: 'opp_astRate', label: 'Opp Assist Rate', format: 'pct' },
  { value: 'opp_tovRate', label: 'Opp Turnover Rate', format: 'pct' },
  { value: 'opp_orbPct', label: 'Opp ORB%', format: 'pct' },
  { value: 'opp_drbPct', label: 'Opp DRB%', format: 'pct' },
  { value: 'opp_ftRate', label: 'Opp FT Rate', format: 'decimal' },
  { value: 'opp_stlRate', label: 'Opp Steal Rate', format: 'pct' },
  { value: 'opp_blkRate', label: 'Opp Block Rate', format: 'pct' },
]

// Basic team stats columns - Boxscore stats like player stats
const basicColumns = [
  { key: 'team', label: 'Team', align: 'left' },
  { key: 'games', label: 'G', align: 'right' },
  { key: 'ppg', label: 'PPG', align: 'right', highlight: true },
  { key: 'rpg', label: 'RPG', align: 'right' },
  { key: 'apg', label: 'APG', align: 'right' },
  { key: 'topg', label: 'TOV', align: 'right', inverse: true },
  { key: 'spg', label: 'SPG', align: 'right' },
  { key: 'bpg', label: 'BPG', align: 'right' },
  { key: 'fg3mPg', label: '3PM', align: 'right' },
  { key: 'fgPct', label: 'FG%', align: 'right' },
  { key: 'threePct', label: '3P%', align: 'right' },
  { key: 'ftPct', label: 'FT%', align: 'right' },
]

// Advanced team stats columns - Ratings, pace, and rate stats
const advancedColumns = [
  { key: 'team', label: 'Team', align: 'left' },
  { key: 'games', label: 'G', align: 'right' },
  { key: 'ortg', label: 'ORtg', align: 'right', highlight: true },
  { key: 'drtg', label: 'DRtg', align: 'right', highlight: true, inverse: true },
  { key: 'netRtg', label: 'Net', align: 'right', highlight: true },
  { key: 'pace', label: 'Pace', align: 'right' },
  { key: 'efg', label: 'eFG%', align: 'right' },
  { key: 'ts', label: 'TS%', align: 'right' },
  { key: 'threeRate', label: '3PAr', align: 'right' },
  { key: 'astRate', label: 'AST%', align: 'right' },
  { key: 'tovRate', label: 'TOV%', align: 'right', inverse: true },
  { key: 'orbPct', label: 'ORB%', align: 'right' },
  { key: 'drbPct', label: 'DRB%', align: 'right' },
]

// Opponent basic stats columns - Boxscore stats allowed
const oppBasicColumns = [
  { key: 'team', label: 'Team', align: 'left' },
  { key: 'games', label: 'G', align: 'right' },
  { key: 'opp_ppg', label: 'PPG', align: 'right', highlight: true, inverse: true },
  { key: 'opp_rpg', label: 'RPG', align: 'right', inverse: true },
  { key: 'opp_apg', label: 'APG', align: 'right', inverse: true },
  { key: 'opp_topg', label: 'TOV', align: 'right' },
  { key: 'opp_spg', label: 'SPG', align: 'right', inverse: true },
  { key: 'opp_bpg', label: 'BPG', align: 'right', inverse: true },
  { key: 'opp_fg3mPg', label: '3PM', align: 'right', inverse: true },
  { key: 'opp_fgPct', label: 'FG%', align: 'right', inverse: true },
  { key: 'opp_threePct', label: '3P%', align: 'right', inverse: true },
  { key: 'opp_ftPct', label: 'FT%', align: 'right', inverse: true },
]

// Opponent advanced stats columns - Ratings and rate stats allowed
const oppAdvancedColumns = [
  { key: 'team', label: 'Team', align: 'left' },
  { key: 'games', label: 'G', align: 'right' },
  { key: 'opp_ortg', label: 'ORtg', align: 'right', highlight: true, inverse: true },
  { key: 'opp_drtg', label: 'DRtg', align: 'right', highlight: true },
  { key: 'pace', label: 'Pace', align: 'right' },
  { key: 'opp_efg', label: 'eFG%', align: 'right', inverse: true },
  { key: 'opp_ts', label: 'TS%', align: 'right', inverse: true },
  { key: 'opp_threeRate', label: '3PAr', align: 'right' },
  { key: 'opp_astRate', label: 'AST%', align: 'right', inverse: true },
  { key: 'opp_tovRate', label: 'TOV%', align: 'right' },
  { key: 'opp_orbPct', label: 'ORB%', align: 'right', inverse: true },
  { key: 'opp_drbPct', label: 'DRB%', align: 'right', inverse: true },
]

export default function TeamStats({ teams }) {
  // Get available seasons and default to most recent
  const availableSeasons = useMemo(() => {
    const seasons = [...new Set(teams.map(t => t.season))].sort((a, b) => b - a)
    return seasons
  }, [teams])

  const [selectedSeason, setSelectedSeason] = useState(availableSeasons[0] || 2025)
  const [viewMode, setViewMode] = useState('basic') // 'basic', 'advanced', 'oppBasic', 'oppAdvanced'
  const [xAxis, setXAxis] = useState('ortg')
  const [yAxis, setYAxis] = useState('drtg')
  const [sortKey, setSortKey] = useState('team')
  const [sortDir, setSortDir] = useState('asc')
  const [highlightTeam, setHighlightTeam] = useState(null)
  const [showLabels, setShowLabels] = useState(false) // State for toggling labels

  // Select columns based on viewMode
  const tableColumns = viewMode === 'basic' ? basicColumns
    : viewMode === 'advanced' ? advancedColumns
    : viewMode === 'oppBasic' ? oppBasicColumns
    : oppAdvancedColumns

  // Filter teams by season
  const seasonFilteredTeams = useMemo(() => {
    if (selectedSeason === 'all') return teams
    return teams.filter(t => t.season === selectedSeason)
  }, [teams, selectedSeason])

  // Assign colors to teams
  const teamsWithColors = useMemo(() => {
    // Get unique team names across all seasons or just current season
    const uniqueTeams = [...new Set(teams.map(t => t.team))].sort()
    
    return seasonFilteredTeams.map(team => ({
      ...team,
      // Assign color based on team name (consistent across seasons)
      color: TEAM_COLORS[uniqueTeams.indexOf(team.team) % TEAM_COLORS.length],
      // Or use a fixed size for all teams
      // size: 16
    }))
  }, [seasonFilteredTeams, teams])

  const sortedTeams = useMemo(() => {
    return [...seasonFilteredTeams].sort((a, b) => {
      const aVal = a[sortKey] || 0
      const bVal = b[sortKey] || 0
      return sortDir === 'desc' ? bVal - aVal : aVal - bVal
    })
  }, [seasonFilteredTeams, sortKey, sortDir])
  
  const avgX = useMemo(() =>
    seasonFilteredTeams.reduce((sum, t) => sum + (t[xAxis] || 0), 0) / seasonFilteredTeams.length,
    [seasonFilteredTeams, xAxis]
  )

  const avgY = useMemo(() =>
    seasonFilteredTeams.reduce((sum, t) => sum + (t[yAxis] || 0), 0) / seasonFilteredTeams.length,
    [seasonFilteredTeams, yAxis]
  )

  // Calculate domains centered around the mean
  const xDomain = useMemo(() => {
    const values = seasonFilteredTeams.map(t => t[xAxis] || 0)
    const min = Math.min(...values)
    const max = Math.max(...values)
    const range = max - min

    const padding = range * 0.2
    const distFromMeanToMin = avgX - min
    const distFromMeanToMax = max - avgX
    const maxDist = Math.max(distFromMeanToMin, distFromMeanToMax)

    return [
      avgX - maxDist - padding,
      avgX + maxDist + padding
    ]
  }, [seasonFilteredTeams, xAxis, avgX])

  const yDomain = useMemo(() => {
    const values = seasonFilteredTeams.map(t => t[yAxis] || 0)
    const min = Math.min(...values)
    const max = Math.max(...values)
    const range = max - min

    const padding = range * 0.2
    const distFromMeanToMin = avgY - min
    const distFromMeanToMax = max - avgY
    const maxDist = Math.max(distFromMeanToMin, distFromMeanToMax)

    return [
      avgY - maxDist - padding,
      avgY + maxDist + padding
    ]
  }, [seasonFilteredTeams, yAxis, avgY])
  
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
    const col = tableColumns.find(c => c.key === key) || statOptions.find(s => s.value === key)

    // Integer values
    if (key === 'games') return Math.round(value).toString()

    // Percentages stored as decimals (0-1)
    if (key === 'efg' || key === 'ts' || key === 'threePct' || key === 'threeRate' ||
        key === 'astRate' || key === 'tovRate' || key === 'orbPct' || key === 'drbPct' ||
        key === 'ftRate' || key === 'stlRate' || key === 'blkRate' ||
        key === 'opp_efg' || key === 'opp_ts' || key === 'opp_threePct' || key === 'opp_threeRate' ||
        key === 'opp_astRate' || key === 'opp_tovRate' || key === 'opp_orbPct' || key === 'opp_drbPct' ||
        key === 'opp_ftRate' || key === 'opp_stlRate' || key === 'opp_blkRate') {
      return `${(value * 100).toFixed(1)}%`
    }

    // Percentages already as 0-100
    if (key === 'fgPct' || key === 'ftPct' || key === 'opp_fgPct' || key === 'opp_ftPct') {
      return `${value.toFixed(1)}%`
    }

    if (col?.format === 'pct') {
      return `${(value * 100).toFixed(1)}%`
    }
    if (col?.format === 'integer') {
      return Math.round(value).toString()
    }
    return value.toFixed(1)
  }

  // Calculate rankings for each stat (1 = best)
  const rankings = useMemo(() => {
    const rankMap = {}
    const numericCols = tableColumns.filter(c => c.key !== 'team' && c.key !== 'games')

    numericCols.forEach(col => {
      const values = seasonFilteredTeams
        .map(t => ({ team: t.team, value: t[col.key] }))
        .filter(v => v.value != null && !isNaN(v.value))

      // Sort: for inverse stats, lower is better; otherwise higher is better
      values.sort((a, b) => col.inverse ? a.value - b.value : b.value - a.value)

      values.forEach((v, idx) => {
        if (!rankMap[v.team]) rankMap[v.team] = {}
        rankMap[v.team][col.key] = idx + 1
      })
    })

    return rankMap
  }, [seasonFilteredTeams, tableColumns])

  // Get ranking badge color based on rank (1 = best)
  const getRankBadgeColor = (rank, total) => {
    if (rank == null || isNaN(rank)) return 'bg-acb-100 text-acb-600'
    const pct = ((total - rank) / (total - 1)) * 100 // Convert rank to percentile
    if (pct >= 75) return 'bg-green-100 text-green-700'   // Top 25%
    if (pct >= 50) return 'bg-blue-100 text-blue-700'     // Top 50%
    if (pct >= 25) return 'bg-orange-100 text-orange-700' // Top 75%
    return 'bg-red-100 text-red-700'                       // Bottom 25%
  }
  
  const getValueColor = (value, key, inverse = false) => {
    const col = tableColumns.find(c => c.key === key)
    if (!col?.highlight) return ''

    const avg = seasonFilteredTeams.reduce((sum, t) => sum + (t[key] || 0), 0) / seasonFilteredTeams.length
    const isGood = inverse ? value < avg : value > avg
    return isGood ? 'text-positive' : 'text-negative'
  }

  // Create helper function for axis formatting
  const formatAxisValue = (value, statKey) => {
    const stat = statOptions.find(s => s.value === statKey)
    if (stat?.format === 'pct') {
      return (value * 100).toFixed(1)
    }
    if (stat?.format === 'integer') {
      return Math.round(value).toString()
    }
    return value.toFixed(1)
  }

  return (
    <div className="space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-semibold text-acb-900">Team Statistics</h2>
        <p className="text-acb-500 text-sm mt-1">
          Compare team performance across offensive and defensive metrics
        </p>
      </div>
      
      {/* Scatter Plot */}
      <div className="bg-white rounded-lg border border-acb-200 p-6">
        <div className="flex flex-wrap items-center gap-4 mb-6">
          <div className="flex items-center gap-2">
            <label className="text-sm text-acb-600">Season:</label>
            <select
              value={selectedSeason}
              onChange={(e) => setSelectedSeason(e.target.value === 'all' ? 'all' : parseInt(e.target.value))}
              className="px-3 py-1.5 border border-acb-200 rounded text-sm bg-white"
            >
              {availableSeasons.map(season => (
                <option key={season} value={season}>{season-1}-{String(season).slice(-2)}</option>
              ))}
              <option value="all">All Seasons</option>
            </select>
          </div>
          <div className="flex items-center gap-2">
            <label className="text-sm text-acb-600">X-Axis:</label>
            <select
              value={xAxis}
              onChange={(e) => setXAxis(e.target.value)}
              className="px-3 py-1.5 border border-acb-200 rounded text-sm bg-white"
            >
              {statOptions.map(opt => (
                <option key={opt.value} value={opt.value}>{opt.label}</option>
              ))}
            </select>
          </div>
          <div className="flex items-center gap-2">
            <label className="text-sm text-acb-600">Y-Axis:</label>
            <select
              value={yAxis}
              onChange={(e) => setYAxis(e.target.value)}
              className="px-3 py-1.5 border border-acb-200 rounded text-sm bg-white"
            >
              {statOptions.map(opt => (
                <option key={opt.value} value={opt.value}>{opt.label}</option>
              ))}
            </select>
          </div>
          <div className="flex items-center gap-2">
            <button
              onClick={() => setShowLabels(!showLabels)}
              className={`px-3 py-1.5 border border-acb-200 rounded text-sm bg-white hover:bg-acb-50 ${showLabels ? 'bg-acb-100' : ''}`}
            >
              {showLabels ? 'Hide Labels' : 'Show Labels'}
            </button>
          </div>
        </div>
        
        <div className="h-96">
          <ResponsiveContainer width="100%" height="100%">
            <ScatterChart margin={{ top: 20, right: 20, bottom: 40, left: 40 }}>
              <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
              <XAxis
                type="number"
                dataKey={xAxis}
                name={statOptions.find(s => s.value === xAxis)?.label}
                domain={xDomain}
                stroke="#64748b"
                fontSize={12}
                tickFormatter={(v) => formatAxisValue(v, xAxis)}
                label={{
                  value: statOptions.find(s => s.value === xAxis)?.label,
                  position: 'bottom',
                  style: { fill: '#64748b', fontSize: 12 }
                }}
              />
              <YAxis
                type="number"
                dataKey={yAxis}
                name={statOptions.find(s => s.value === yAxis)?.label}
                domain={yDomain}
                stroke="#64748b"
                fontSize={12}
                tickFormatter={(v) => formatAxisValue(v, yAxis)}
                label={{
                  value: statOptions.find(s => s.value === yAxis)?.label,
                  angle: -90,
                  position: 'left',
                  style: { fill: '#64748b', fontSize: 12 }
                }}
              />
              <ReferenceLine x={avgX} stroke="#94a3b8" strokeDasharray="5 5" />
              <ReferenceLine y={avgY} stroke="#94a3b8" strokeDasharray="5 5" />
              <Tooltip 
                content={({ payload }) => {
                  if (!payload?.[0]) return null
                  const team = payload[0].payload
                  return (
                    <div className="bg-white border border-acb-200 rounded-lg p-3 shadow-lg">
                      <div className="font-medium text-acb-900 mb-1">{team.team}</div>
                      <div className="text-sm text-acb-600">
                        {statOptions.find(s => s.value === xAxis)?.label}: {formatValue(team[xAxis], xAxis)}
                      </div>
                      <div className="text-sm text-acb-600">
                        {statOptions.find(s => s.value === yAxis)?.label}: {formatValue(team[yAxis], yAxis)}
                      </div>
                    </div>
                  )
                }}
              />
              
              {/* Render each team as a separate Scatter for unique colors */}
              {teamsWithColors.map((team, index) => (
                <Scatter
                  key={team.team}
                  data={[team]}
                  fill={highlightTeam === team.team ? '#f97316' : team.color}
                  fillOpacity={highlightTeam === team.team ? 1 : 0.8}
                  onMouseEnter={() => setHighlightTeam(team.team)}
                  onMouseLeave={() => setHighlightTeam(null)}
                >                  
                  {/* Add team labels if enabled */}
                  {showLabels && (
                    <LabelList
                      dataKey="team"
                      position="right"
                      offset={10}
                      style={{
                        fontSize: '10px',
                        fontWeight: 'bold',
                        fill: '#374151',
                      }}
                    />
                  )}
                </Scatter>
              ))}
            </ScatterChart>
          </ResponsiveContainer>
        </div>
      </div>
      
      {/* Table View Mode Toggle */}
      <div className="flex items-center gap-1 bg-acb-100 rounded-md p-1 w-fit">
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
          onClick={() => setViewMode('oppBasic')}
          className={`px-3 py-1.5 text-sm font-medium rounded transition-colors
            ${viewMode === 'oppBasic'
              ? 'bg-white text-acb-900 shadow-sm'
              : 'text-acb-600 hover:text-acb-900'}`}
        >
          Opp. Basic
        </button>
        <button
          onClick={() => setViewMode('oppAdvanced')}
          className={`px-3 py-1.5 text-sm font-medium rounded transition-colors
            ${viewMode === 'oppAdvanced'
              ? 'bg-white text-acb-900 shadow-sm'
              : 'text-acb-600 hover:text-acb-900'}`}
        >
          Opp. Advanced
        </button>
      </div>

      {/* Table */}
      <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
        <div className="overflow-x-auto">
          <table className="w-full">
            <thead>
              <tr className="bg-acb-50 border-b border-acb-200">
                {tableColumns.map(col => (
                  <th
                    key={col.key}
                    onClick={() => col.key !== 'team' && handleSort(col.key)}
                    className={`px-4 py-3 text-xs font-semibold text-acb-600 uppercase tracking-wider
                      ${col.align === 'right' ? 'text-right' : 'text-left'}
                      ${col.key !== 'team' ? 'cursor-pointer hover:bg-acb-100' : ''}`}
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
              {sortedTeams.map((team, i) => (
                <tr
                  key={team.team}
                  className={`border-b border-acb-100 hover:bg-acb-50 transition-colors
                    ${highlightTeam === team.team ? 'bg-orange-50' : ''}`}
                  onMouseEnter={() => setHighlightTeam(team.team)}
                  onMouseLeave={() => setHighlightTeam(null)}
                >
                  {tableColumns.map(col => {
                    const rank = rankings[team.team]?.[col.key]
                    const showRank = col.key !== 'team' && col.key !== 'games' && rank != null
                    const totalTeams = seasonFilteredTeams.length

                    return (
                      <td
                        key={col.key}
                        className={`px-4 py-3 text-sm whitespace-nowrap
                          ${col.align === 'right' ? 'text-right' : ''}
                          ${col.key === 'team' ? 'font-medium text-acb-900' : ''}`}
                      >
                        {col.key === 'team' ? (
                          team.team
                        ) : showRank ? (
                          <div className="flex flex-col items-end gap-0.5">
                            <span className={`font-mono ${col.highlight ? getValueColor(team[col.key], col.key, col.inverse) : 'text-acb-700'}`}>
                              {formatValue(team[col.key], col.key)}
                            </span>
                            <span className={`text-xs px-1.5 py-0.5 rounded ${getRankBadgeColor(rank, totalTeams)}`}>
                              #{rank}
                            </span>
                          </div>
                        ) : (
                          <span className={`font-mono ${col.highlight ? getValueColor(team[col.key], col.key, col.inverse) : 'text-acb-700'}`}>
                            {formatValue(team[col.key], col.key)}
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
      
      {/* Legend for team colors (optional) */}
      {selectedSeason !== 'all' && (
        <div className="bg-white rounded-lg border border-acb-200 p-4">
          <h3 className="text-sm font-semibold text-acb-700 mb-2">Team Colors</h3>
          <div className="flex flex-wrap gap-2">
            {teamsWithColors.map((team, index) => (
              <div
                key={team.team}
                className="flex items-center gap-1.5 px-2 py-1 rounded text-xs"
                style={{ backgroundColor: `${team.color}20` }}
                onMouseEnter={() => setHighlightTeam(team.team)}
                onMouseLeave={() => setHighlightTeam(null)}
              >
                <div
                  className="w-3 h-3 rounded-full"
                  style={{ backgroundColor: team.color }}
                />
                <span className="text-acb-700">{team.team}</span>
              </div>
            ))}
          </div>
        </div>
      )}
    </div>
  )
}