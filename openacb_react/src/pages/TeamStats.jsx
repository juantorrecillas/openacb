import { useState, useMemo } from 'react'
import { ScatterChart, Scatter, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, ReferenceLine, LabelList } from 'recharts'
import { ArrowUpDown, ArrowUp, ArrowDown, Download } from 'lucide-react'
import { downloadTableAsCsv } from '../utils/csvDownload'
import { statTitle } from '../utils/statLabels'


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
  { value: 'ppg', label: 'Puntos/Partido', format: 'decimal' },
  { value: 'rpg', label: 'Rebotes/Partido', format: 'decimal' },
  { value: 'apg', label: 'Asistencias/Partido', format: 'decimal' },
  { value: 'topg', label: 'Pérdidas/Partido', format: 'decimal' },
  { value: 'spg', label: 'Robos/Partido', format: 'decimal' },
  { value: 'bpg', label: 'Tapones/Partido', format: 'decimal' },
  { value: 'fg3mPg', label: 'Triples/Partido', format: 'decimal' },
  { value: 'fgPct', label: 'TC%', format: 'decimal' },
  { value: 'ftPct', label: 'TL%', format: 'decimal' },
  // Team advanced stats
  { value: 'ortg', label: 'Eficiencia Ofensiva', format: 'decimal' },
  { value: 'drtg', label: 'Eficiencia Defensiva', format: 'decimal' },
  { value: 'netRtg', label: 'Eficiencia Neta', format: 'decimal' },
  { value: 'pace', label: 'Ritmo', format: 'decimal' },
  { value: 'efg', label: 'eFG%', format: 'pct'},
  { value: 'ts', label: 'TS%', format: 'pct' },
  { value: 'threePct', label: '3P%', format: 'pct' },
  { value: 'threeRate', label: 'Ratio 3P', format: 'pct' },
  { value: 'astRate', label: 'Ratio Asist.', format: 'pct' },
  { value: 'tovRate', label: 'Ratio Pérdidas', format: 'pct' },
  { value: 'orbPct', label: 'RO%', format: 'pct' },
  { value: 'drbPct', label: 'RD%', format: 'pct' },
  { value: 'ftRate', label: 'Ratio TL', format: 'decimal' },
  { value: 'stlRate', label: 'Ratio Robos', format: 'pct' },
  { value: 'blkRate', label: 'Ratio Tapones', format: 'pct' },
  { value: 'astToRatio', label: 'Ratio AST/PER', format: 'decimal' },
  { value: 'offTo', label: 'Pts de Robo %', format: 'pct' },
  { value: 'secondChance', label: '2da Oport. %', format: 'pct' },
  { value: 'assistedFgm', label: 'Pts Asistidos %', format: 'pct' },
  { value: 'assistedFgm2', label: '2P Asistidos %', format: 'pct' },
  { value: 'assistedFgm3', label: '3P Asistidos %', format: 'pct' },
  // Opponent boxscore stats
  { value: 'opp_ppg', label: 'Riv PPP', format: 'decimal' },
  { value: 'opp_rpg', label: 'Riv RPP', format: 'decimal' },
  { value: 'opp_apg', label: 'Riv APP', format: 'decimal' },
  { value: 'opp_fgPct', label: 'Riv TC%', format: 'decimal' },
  // Opponent advanced stats
  { value: 'opp_ortg', label: 'Riv Ef. Ofensiva', format: 'decimal' },
  { value: 'opp_drtg', label: 'Riv Ef. Defensiva', format: 'decimal' },
  { value: 'opp_efg', label: 'Riv eFG%', format: 'pct' },
  { value: 'opp_ts', label: 'Riv TS%', format: 'pct' },
  { value: 'opp_threePct', label: 'Riv 3P%', format: 'pct' },
  { value: 'opp_threeRate', label: 'Riv Ratio 3P', format: 'pct' },
  { value: 'opp_astRate', label: 'Riv Ratio Asist.', format: 'pct' },
  { value: 'opp_tovRate', label: 'Riv Ratio Pérdidas', format: 'pct' },
  { value: 'opp_orbPct', label: 'Riv RO%', format: 'pct' },
  { value: 'opp_drbPct', label: 'Riv RD%', format: 'pct' },
  { value: 'opp_ftRate', label: 'Riv Ratio TL', format: 'decimal' },
  { value: 'opp_stlRate', label: 'Riv Ratio Robos', format: 'pct' },
  { value: 'opp_blkRate', label: 'Riv Ratio Tapones', format: 'pct' },
  { value: 'opp_astToRatio', label: 'Riv AST/PER', format: 'decimal' },
  { value: 'opp_offTo', label: 'Riv Pts de Robo %', format: 'pct' },
  { value: 'opp_secondChance', label: 'Riv 2da Oport. %', format: 'pct' },
  { value: 'opp_assistedFgm', label: 'Riv Pts Asist. %', format: 'pct' },
  { value: 'opp_assistedFgm2', label: 'Riv 2PT Asist. %', format: 'pct' },
  { value: 'opp_assistedFgm3', label: 'Riv 3PT Asist. %', format: 'pct' },
]

// Basic team stats columns - Boxscore stats like player stats
const basicColumns = [
  { key: 'team', label: 'Equipo', align: 'left' },
  { key: 'games', label: 'PJ', align: 'right' },
  { key: 'wins', label: 'V', align: 'right' },
  { key: 'losses', label: 'D', align: 'right' },
  { key: 'ppg', label: 'PPP', align: 'right', highlight: true },
  { key: 'fgPct', label: 'TC%', align: 'right' },
  { key: 'threePct', label: '3P%', align: 'right' },
  { key: 'ftPct', label: 'TL%', align: 'right' },
  { key: 'fg3mPg', label: '3PA', align: 'right' },
  { key: 'orebpg', label: 'RO', align: 'right' },
  { key: 'drebpg', label: 'RD', align: 'right' },
  { key: 'rpg', label: 'RPP', align: 'right' },
  { key: 'apg', label: 'APP', align: 'right' },
  { key: 'spg', label: 'RBP', align: 'right' },
  { key: 'bpg', label: 'TAPP', align: 'right' },
  { key: 'topg', label: 'PER', align: 'right', inverse: true },
]

// Advanced team stats columns - Ratings, pace, and rate stats
const advancedColumns = [
  { key: 'team', label: 'Equipo', align: 'left' },
  { key: 'games', label: 'PJ', align: 'right' },
  { key: 'ortg', label: 'ORtg', align: 'right', highlight: true },
  { key: 'drtg', label: 'DRtg', align: 'right', highlight: true, inverse: true },
  { key: 'netRtg', label: 'Neto', align: 'right', highlight: true },
  { key: 'pace', label: 'Ritmo', align: 'right' },
  { key: 'efg', label: 'eFG%', align: 'right' },
  { key: 'ts', label: 'TS%', align: 'right' },
  { key: 'threeRate', label: '3PAr', align: 'right' },
  { key: 'orbPct', label: 'RO%', align: 'right' },
  { key: 'drbPct', label: 'RD%', align: 'right' },
  { key: 'trbPct', label: 'RT%', align: 'right' },
  { key: 'astRate', label: 'AST%', align: 'right' },
  { key: 'stlRate', label: 'ROB%', align: 'right' },
  { key: 'blkRate', label: 'TAP%', align: 'right' },
  { key: 'tovRate', label: 'PER%', align: 'right', inverse: true },
  { key: 'astToRatio', label: 'AST/PER', align: 'right' },
  { key: 'offTo', label: 'Pts Robo%', align: 'right' },
  { key: 'secondChance', label: '2da Op%', align: 'right' },
  { key: 'assistedFgm', label: 'Pts Ast%', align: 'right' },
]

// Opponent basic stats columns - Boxscore stats allowed
const oppBasicColumns = [
  { key: 'team', label: 'Equipo', align: 'left' },
  { key: 'games', label: 'PJ', align: 'right' },
  { key: 'opp_ppg', label: 'PPP', align: 'right', highlight: true, inverse: true },
  { key: 'opp_fgPct', label: 'TC%', align: 'right', inverse: true },
  { key: 'opp_threePct', label: '3P%', align: 'right', inverse: true },
  { key: 'opp_ftPct', label: 'TL%', align: 'right', inverse: true },
  { key: 'opp_fg3mPg', label: '3PA', align: 'right', inverse: true },
  { key: 'opp_orebpg', label: 'RO', align: 'right', inverse: true },
  { key: 'opp_drebpg', label: 'RD', align: 'right', inverse: true },
  { key: 'opp_rpg', label: 'RPP', align: 'right', inverse: true },
  { key: 'opp_apg', label: 'APP', align: 'right', inverse: true },
  { key: 'opp_spg', label: 'RBP', align: 'right', inverse: true },
  { key: 'opp_bpg', label: 'TAPP', align: 'right', inverse: true },
  { key: 'opp_topg', label: 'PER', align: 'right' },
]

// Opponent advanced stats columns - Ratings and rate stats allowed
const oppAdvancedColumns = [
  { key: 'team', label: 'Equipo', align: 'left' },
  { key: 'games', label: 'PJ', align: 'right' },
  { key: 'opp_ortg', label: 'ORtg', align: 'right', highlight: true, inverse: true },
  { key: 'opp_drtg', label: 'DRtg', align: 'right', highlight: true },
  { key: 'netRtg', label: 'Neto', align: 'right', highlight: true },
  { key: 'pace', label: 'Ritmo', align: 'right' },
  { key: 'opp_efg', label: 'eFG%', align: 'right', inverse: true },
  { key: 'opp_ts', label: 'TS%', align: 'right', inverse: true },
  { key: 'opp_threeRate', label: '3PAr', align: 'right' },
  { key: 'opp_orbPct', label: 'RO%', align: 'right', inverse: true },
  { key: 'opp_drbPct', label: 'RD%', align: 'right', inverse: true },
  { key: 'opp_trbPct', label: 'RT%', align: 'right', inverse: true },
  { key: 'opp_astRate', label: 'AST%', align: 'right', inverse: true },
  { key: 'opp_stlRate', label: 'ROB%', align: 'right', inverse: true },
  { key: 'opp_blkRate', label: 'TAP%', align: 'right', inverse: true },
  { key: 'opp_tovRate', label: 'PER%', align: 'right' },
  { key: 'opp_astToRatio', label: 'AST/PER', align: 'right', inverse: true },
  { key: 'opp_offTo', label: 'Pts Robo%', align: 'right', inverse: true },
  { key: 'opp_secondChance', label: '2da Op%', align: 'right', inverse: true },
  { key: 'opp_assistedFgm', label: 'Pts Ast%', align: 'right', inverse: true },
]

const basicGroups      = [{ label:'Marcador', span:3 },{ label:'Tiro', span:4 },{ label:'Rebotes', span:3 },{ label:'Otros', span:4 }]
const advancedGroups   = [{ label:'Rating', span:4 },{ label:'Tiro', span:3 },{ label:'Rebotes', span:3 },{ label:'Ratios', span:5 },{ label:'Tipo', span:3 }]
const oppBasicGroups   = [{ label:'Puntos', span:1 },{ label:'Tiro', span:4 },{ label:'Rebotes', span:3 },{ label:'Otros', span:4 }]
const oppAdvancedGroups= [{ label:'Rating', span:4 },{ label:'Tiro', span:3 },{ label:'Rebotes', span:3 },{ label:'Ratios', span:5 },{ label:'Tipo', span:3 }]

// Custom scatter shape: logo image if available, else colored circle
function TeamDot({ cx, cy, payload, teamLogos, color, highlighted }) {
  const url = teamLogos?.[payload?.team]
  const size = highlighted ? 40 : 34
  const half = size / 2
  if (url) {
    return (
      <image
        href={url}
        x={cx - half}
        y={cy - half}
        width={size}
        height={size}
        style={{ opacity: highlighted ? 1 : 0.85 }}
        preserveAspectRatio="xMidYMid meet"
      />
    )
  }
  return <circle cx={cx} cy={cy} r={highlighted ? 10 : 8} fill={color} fillOpacity={highlighted ? 1 : 0.8} />
}

export default function TeamStats({ teams, teamLogos = {} }) {
  // Get available seasons and default to most recent
  const availableSeasons = useMemo(() => {
    const seasons = [...new Set(teams.map(t => t.season))].sort((a, b) => b - a)
    return seasons
  }, [teams])

  const [selectedSeason, setSelectedSeason] = useState(availableSeasons[0] || 2025)
  const [selectedStage, setSelectedStage] = useState('regular')
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

  const columnGroups = viewMode === 'basic' ? basicGroups
    : viewMode === 'advanced' ? advancedGroups
    : viewMode === 'oppBasic' ? oppBasicGroups
    : oppAdvancedGroups

  // first key of each group in tableColumns (after identity cols) gets a left border
  const groupBorderKeys = new Set(
    columnGroups.reduce((acc, g, i) => {
      const offset = columnGroups.slice(0, i).reduce((s, x) => s + x.span, 0)
      const col = tableColumns[2 + offset]
      if (col) acc.push(col.key)
      return acc
    }, [])
  )

  const stageFilteredTeams = useMemo(() => {
    return teams.filter(t => (t.competitionStage || 'regular') === selectedStage)
  }, [teams, selectedStage])

  // Filter teams by season
  const seasonFilteredTeams = useMemo(() => {
    if (selectedSeason === 'all') return stageFilteredTeams
    return stageFilteredTeams.filter(t => t.season === selectedSeason)
  }, [stageFilteredTeams, selectedSeason])

  // Derive trbPct / opp_trbPct from per-game rebound averages (not in JSON)
  const enrichedTeams = useMemo(() => {
    return seasonFilteredTeams.map(t => {
      const totalReb = (t.orebpg || 0) + (t.drebpg || 0) + (t.opp_orebpg || 0) + (t.opp_drebpg || 0)
      return {
        ...t,
        trbPct:     totalReb > 0 ? ((t.orebpg || 0) + (t.drebpg || 0)) / totalReb : null,
        opp_trbPct: totalReb > 0 ? ((t.opp_orebpg || 0) + (t.opp_drebpg || 0)) / totalReb : null,
      }
    })
  }, [seasonFilteredTeams])

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
    return [...enrichedTeams].sort((a, b) => {
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
    if (key === 'games' || key === 'wins' || key === 'losses') return Math.round(value).toString()

    // Percentages stored as decimals (0-1)
    if (key === 'efg' || key === 'ts' || key === 'threePct' || key === 'threeRate' ||
        key === 'astRate' || key === 'tovRate' || key === 'orbPct' || key === 'drbPct' || key === 'trbPct' ||
        key === 'ftRate' || key === 'stlRate' || key === 'blkRate' ||
        key === 'opp_efg' || key === 'opp_ts' || key === 'opp_threePct' || key === 'opp_threeRate' ||
        key === 'opp_astRate' || key === 'opp_tovRate' || key === 'opp_orbPct' || key === 'opp_drbPct' || key === 'opp_trbPct' ||
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

  const handleDownloadCsv = () => {
    const seasonStr = selectedSeason === 'all'
      ? 'todas-temporadas'
      : `${selectedSeason - 1}-${String(selectedSeason).slice(-2)}`
    const viewStr = viewMode === 'basic' ? 'basico'
      : viewMode === 'advanced' ? 'avanzado'
      : viewMode === 'oppBasic' ? 'rival-basico'
      : 'rival-avanzado'
    const filename = `equipos_${seasonStr}_${selectedStage}_${viewStr}.csv`

    const exportColumns = [
      { key: 'season', label: 'Temporada' },
      ...tableColumns.map(c => ({ key: c.key, label: c.label })),
    ]

    const exportRows = sortedTeams.map(t => {
      const row = { season: `${t.season - 1}-${String(t.season).slice(-2)}` }
      tableColumns.forEach(col => {
        const v = t[col.key]
        if (col.key === 'team') {
          row[col.key] = v ?? ''
        } else if (v == null) {
          row[col.key] = ''
        } else {
          // mirror display semantics but strip % so values are pure numbers
          const formatted = formatValue(v, col.key)
          row[col.key] = formatted === '-' ? '' : formatted.replace('%', '')
        }
      })
      return row
    })

    downloadTableAsCsv(filename, exportRows, exportColumns)
  }

  // Calculate rankings for each stat (1 = best)
  const rankings = useMemo(() => {
    const rankMap = {}
    const numericCols = tableColumns.filter(c => c.key !== 'team' && c.key !== 'games' && c.key !== 'wins' && c.key !== 'losses')

    numericCols.forEach(col => {
      const values = enrichedTeams
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

  const getRankBadgeColor = (rank, total) => {
    if (rank == null || isNaN(rank)) return 'bg-acb-100 text-acb-600'
    const pct = ((total - rank) / (total - 1)) * 100
    if (pct >= 75) return 'bg-positive-100 text-positive-700'
    if (pct >= 50) return 'bg-info-100 text-info-700'
    if (pct >= 25) return 'bg-info-100 text-info-600'
    return 'bg-negative-100 text-negative-700'
  }

  const getValueColor = () => 'text-acb-700'

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
        <h2 className="text-2xl font-semibold text-acb-900">Estadísticas de Equipo</h2>
        <p className="text-acb-500 text-sm mt-1">
          Compara el rendimiento de los equipos en métricas ofensivas y defensivas
        </p>
      </div>
      
      {/* Scatter Plot */}
      <div className="bg-white rounded-lg border border-acb-200 p-6">
        <div className="flex flex-wrap items-center gap-4 mb-6">
          <div className="flex items-center gap-2">
            <label className="text-sm text-acb-600">Temporada:</label>
            <select
              value={selectedSeason}
              onChange={(e) => setSelectedSeason(e.target.value === 'all' ? 'all' : parseInt(e.target.value))}
              className="px-3 py-1.5 border border-acb-200 rounded text-sm bg-white"
            >
              {availableSeasons.map(season => (
                <option key={season} value={season}>{season-1}-{String(season).slice(-2)}</option>
              ))}
              <option value="all">Todas las Temporadas</option>
            </select>
          </div>
          <div className="flex items-center gap-1 bg-acb-100 rounded-md p-1">
            <button
              onClick={() => setSelectedStage('regular')}
              className={`px-3 py-1 text-sm font-medium rounded transition-colors ${
                selectedStage === 'regular' ? 'bg-white text-acb-900 shadow-sm' : 'text-acb-600 hover:text-acb-900'
              }`}
            >
              Temporada regular
            </button>
            <button
              onClick={() => setSelectedStage('playoffs')}
              className={`px-3 py-1 text-sm font-medium rounded transition-colors ${
                selectedStage === 'playoffs' ? 'bg-white text-acb-900 shadow-sm' : 'text-acb-600 hover:text-acb-900'
              }`}
            >
              Playoffs
            </button>
          </div>
          <div className="flex items-center gap-2">
            <label className="text-sm text-acb-600">Eje X:</label>
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
            <label className="text-sm text-acb-600">Eje Y:</label>
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
              {showLabels ? 'Ocultar Nombres' : 'Mostrar Nombres'}
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
              
              {/* Render each team as a separate Scatter with logo shape */}
              {teamsWithColors.map((team) => (
                <Scatter
                  key={team.team}
                  data={[team]}
                  shape={(props) => (
                    <TeamDot
                      {...props}
                      teamLogos={teamLogos}
                      color={team.color}
                      highlighted={highlightTeam === team.team}
                    />
                  )}
                  onMouseEnter={() => setHighlightTeam(team.team)}
                  onMouseLeave={() => setHighlightTeam(null)}
                >
                  {showLabels && (
                    <LabelList
                      dataKey="team"
                      position="right"
                      offset={10}
                      style={{ fontSize: '10px', fontWeight: 'bold', fill: '#374151' }}
                    />
                  )}
                </Scatter>
              ))}
            </ScatterChart>
          </ResponsiveContainer>
        </div>
      </div>
      
      {/* Table View Mode Toggle + Download */}
      <div className="flex items-center justify-between flex-wrap gap-3">
        <div className="flex items-center gap-1 bg-acb-100 rounded-md p-1 w-fit">
          <button
            onClick={() => setViewMode('basic')}
            className={`px-3 py-1.5 text-sm font-medium rounded transition-colors
              ${viewMode === 'basic'
                ? 'bg-white text-acb-900 shadow-sm'
                : 'text-acb-600 hover:text-acb-900'}`}
          >
            Básico
          </button>
          <button
            onClick={() => setViewMode('advanced')}
            className={`px-3 py-1.5 text-sm font-medium rounded transition-colors
              ${viewMode === 'advanced'
                ? 'bg-white text-acb-900 shadow-sm'
                : 'text-acb-600 hover:text-acb-900'}`}
          >
            Avanzado
          </button>
          <button
            onClick={() => setViewMode('oppBasic')}
            className={`px-3 py-1.5 text-sm font-medium rounded transition-colors
              ${viewMode === 'oppBasic'
                ? 'bg-white text-acb-900 shadow-sm'
                : 'text-acb-600 hover:text-acb-900'}`}
          >
            Riv. Básico
          </button>
          <button
            onClick={() => setViewMode('oppAdvanced')}
            className={`px-3 py-1.5 text-sm font-medium rounded transition-colors
              ${viewMode === 'oppAdvanced'
                ? 'bg-white text-acb-900 shadow-sm'
                : 'text-acb-600 hover:text-acb-900'}`}
          >
            Riv. Avanzado
          </button>
        </div>
        <button
          onClick={handleDownloadCsv}
          className="inline-flex items-center gap-1.5 px-3 py-1.5 border border-acb-200 rounded text-sm bg-white text-acb-700 hover:bg-acb-50"
          title="Descargar la tabla actual como CSV"
        >
          <Download className="w-4 h-4" />
          Descargar CSV
        </button>
      </div>

      {/* Table */}
      <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
        <div className="overflow-x-auto">
          <table className="data-table min-w-full">
            <thead>
              <tr className="bg-acb-100 border-b border-acb-300">
                <th rowSpan={2} className="data-table-head data-table-identity data-table-sticky data-table-sticky-head data-col-team bg-acb-100">Equipo</th>
                <th rowSpan={2} onClick={() => handleSort('games')} title={statTitle('PJ')} className="data-table-head data-table-number data-col-games cursor-pointer hover:bg-acb-100">
                  <span className="inline-flex items-center gap-1">PJ {sortKey === 'games' && (sortDir === 'desc' ? <ArrowDown className="w-3 h-3"/> : <ArrowUp className="w-3 h-3"/>)}</span>
                </th>
                {columnGroups.map(g => (
                  <th key={g.label} colSpan={g.span} className="data-table-group border-l border-acb-300">{g.label}</th>
                ))}
              </tr>
              <tr className="bg-acb-50 border-b border-acb-200">
                {tableColumns.slice(2).map(col => (
                  <th
                    key={col.key}
                    onClick={() => handleSort(col.key)}
                    title={statTitle(col.label)}
                    className={`data-table-head data-table-number data-col-number cursor-pointer hover:bg-acb-100
                      ${groupBorderKeys.has(col.key) ? 'border-l border-acb-200' : ''}`}
                  >
                    <span className="inline-flex items-center gap-1">
                      {col.label}
                      {sortKey === col.key && (sortDir === 'desc' ? <ArrowDown className="w-3 h-3"/> : <ArrowUp className="w-3 h-3"/>)}
                    </span>
                  </th>
                ))}
              </tr>
            </thead>
            <tbody>
              {sortedTeams.map((team, i) => (
                <tr
                  key={team.team}
                  className={`data-table-row border-b border-acb-100
                    ${highlightTeam === team.team ? 'bg-accent-50' : ''}`}
                  onMouseEnter={() => setHighlightTeam(team.team)}
                  onMouseLeave={() => setHighlightTeam(null)}
                >
                  {tableColumns.map(col => {
                    const rank = rankings[team.team]?.[col.key]
                    const showRank = col.key !== 'team' && col.key !== 'games' && col.key !== 'wins' && col.key !== 'losses' && rank != null
                    const totalTeams = seasonFilteredTeams.length

                    return (
                      <td
                        key={col.key}
                        className={`data-table-cell
                          ${col.align === 'right' ? 'data-table-number' : ''}
                          ${col.key === 'team' ? 'data-table-identity data-table-sticky data-col-team' : col.key === 'games' ? 'data-col-games' : 'data-col-number'}`}
                      >
                        {col.key === 'team' ? (
                          team.team
                        ) : showRank ? (
                          <div className="data-table-value">
                            <span className={col.highlight ? getValueColor(team[col.key], col.key, col.inverse) : 'text-acb-700'}>
                              {formatValue(team[col.key], col.key)}
                            </span>
                            <span className={`data-table-badge ${getRankBadgeColor(rank, totalTeams)}`}>
                              #{rank}
                            </span>
                          </div>
                        ) : (
                          <span className={col.highlight ? getValueColor(team[col.key], col.key, col.inverse) : 'text-acb-700'}>
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
      
    </div>
  )
}
