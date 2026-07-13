import { useState, useMemo } from 'react'
import { ScatterChart, Scatter, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, ReferenceLine, LabelList } from 'recharts'
import { ArrowUpDown, ArrowUp, ArrowDown, Download } from 'lucide-react'
import { downloadTableAsCsv } from '../utils/csvDownload'
import { statTitle } from '../utils/statLabels'
import PageHeader from '../components/PageHeader'
import { getPercentileBadgeClass } from '../utils/percentileColors'


// Add a color palette for teams (or generate colors dynamically)
const TEAM_COLORS = [
  '#fe5917',
  '#3b82f6',
  '#8b5cf6',
  '#f59e0b',
  '#0ea5e9',
  '#d946ef',
  '#6366f1',
  '#ec4899',
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
  { value: 'offTo', label: 'Pts de Robo %', format: 'pct100' },
  { value: 'secondChance', label: '2da Oport. %', format: 'pct100' },
  { value: 'assistedFgm', label: 'Pts Asistidos %', format: 'pct100' },
  { value: 'assistedFgm2', label: '2P Asistidos %', format: 'pct100' },
  { value: 'assistedFgm3', label: '3P Asistidos %', format: 'pct100' },
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
  { value: 'opp_offTo', label: 'Riv Pts de Robo %', format: 'pct100' },
  { value: 'opp_secondChance', label: 'Riv 2da Oport. %', format: 'pct100' },
  { value: 'opp_assistedFgm', label: 'Riv Pts Asist. %', format: 'pct100' },
  { value: 'opp_assistedFgm2', label: 'Riv 2PT Asist. %', format: 'pct100' },
  { value: 'opp_assistedFgm3', label: 'Riv 3PT Asist. %', format: 'pct100' },
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

function getAxisStat(statKey) {
  return statOptions.find(s => s.value === statKey)
}

function teamRecordKey(team) {
  return `${team.team}-${team.season}-${team.competitionStage || 'regular'}`
}

function getAxisDisplayValue(value, statKey) {
  const stat = getAxisStat(statKey)
  return stat?.format === 'pct' ? value * 100 : value
}

function getAxisDataValue(value, statKey) {
  const stat = getAxisStat(statKey)
  return stat?.format === 'pct' ? value / 100 : value
}

function buildNiceScatterAxis(rows, statKey) {
  const values = rows
    .map(row => row[statKey])
    .filter(value => value != null && Number.isFinite(Number(value)))
    .map(Number)

  if (!values.length) return { domain: ['auto', 'auto'], ticks: undefined }

  const displayValues = values.map(value => getAxisDisplayValue(value, statKey))
  const min = Math.min(...displayValues)
  const max = Math.max(...displayValues)
  const range = max - min
  const padding = range === 0 ? 1 : Math.max(range * 0.06, 0.4)
  const low = min - padding
  const high = max + padding
  const maxAbs = Math.max(Math.abs(low), Math.abs(high))
  const stat = getAxisStat(statKey)
  const step = maxAbs < 5 && stat?.format === 'decimal' ? 0.5 : 5
  const start = Math.floor(low / step) * step
  const end = Math.ceil(high / step) * step
  const count = Math.round((end - start) / step) + 1
  const displayTicks = Array.from({ length: count }, (_, index) => start + index * step)
  const dataTicks = displayTicks.map(value => getAxisDataValue(value, statKey))

  return {
    domain: [getAxisDataValue(start, statKey), getAxisDataValue(end, statKey)],
    ticks: dataTicks,
  }
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
    
    return enrichedTeams.map(team => ({
      ...team,
      // Assign color based on team name (consistent across seasons)
      color: TEAM_COLORS[uniqueTeams.indexOf(team.team) % TEAM_COLORS.length],
      displayLabel: selectedSeason === 'all'
        ? `${team.team} (${team.season - 1}-${String(team.season).slice(-2)})`
        : team.team,
      // Or use a fixed size for all teams
      // size: 16
    }))
  }, [enrichedTeams, teams, selectedSeason])

  const sortedTeams = useMemo(() => {
    return [...enrichedTeams].sort((a, b) => {
      const aVal = a[sortKey] ?? ''
      const bVal = b[sortKey] ?? ''
      if (typeof aVal === 'string' || typeof bVal === 'string') {
        const result = String(aVal).localeCompare(String(bVal), 'es')
        return sortDir === 'desc' ? -result : result
      }
      return sortDir === 'desc' ? bVal - aVal : aVal - bVal
    })
  }, [enrichedTeams, sortKey, sortDir])
  
  const avgX = useMemo(() =>
    enrichedTeams.reduce((sum, t) => sum + (t[xAxis] || 0), 0) / enrichedTeams.length,
    [enrichedTeams, xAxis]
  )

  const avgY = useMemo(() =>
    enrichedTeams.reduce((sum, t) => sum + (t[yAxis] || 0), 0) / enrichedTeams.length,
    [enrichedTeams, yAxis]
  )

  // calculate compact domains with readable ticks
  const xAxisScale = useMemo(() => {
    return buildNiceScatterAxis(enrichedTeams, xAxis)
  }, [enrichedTeams, xAxis])

  const yAxisScale = useMemo(() => {
    return buildNiceScatterAxis(enrichedTeams, yAxis)
  }, [enrichedTeams, yAxis])
  
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
    if (key === 'fgPct' || key === 'ftPct' || key === 'opp_fgPct' || key === 'opp_ftPct' || col?.format === 'pct100') {
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
        .map(t => ({ key: teamRecordKey(t), value: t[col.key] }))
        .filter(v => v.value != null && !isNaN(v.value))

      // Sort: for inverse stats, lower is better; otherwise higher is better
      values.sort((a, b) => col.inverse ? a.value - b.value : b.value - a.value)

      values.forEach((v, idx) => {
        if (!rankMap[v.key]) rankMap[v.key] = {}
        rankMap[v.key][col.key] = idx + 1
      })
    })

    return rankMap
  }, [enrichedTeams, tableColumns])

  const getRankBadgeColor = (rank, total) => {
    if (rank == null || isNaN(rank) || total <= 1) return getPercentileBadgeClass(null)
    const pct = ((total - rank) / (total - 1)) * 100
    return getPercentileBadgeClass(pct)
  }

  const getValueColor = () => 'text-acb-700'

  // Create helper function for axis formatting
  const formatAxisValue = (value, statKey) => {
    const stat = statOptions.find(s => s.value === statKey)
    const displayValue = getAxisDisplayValue(value, statKey)
    if (stat?.format === 'pct' || stat?.format === 'pct100') {
      return Math.round(displayValue).toString()
    }
    if (stat?.format === 'integer') {
      return Math.round(displayValue).toString()
    }
    if (displayValue < 5 && displayValue > -5 && stat?.format === 'decimal') {
      return displayValue.toFixed(1)
    }
    return Math.round(displayValue).toString()
  }

  return (
    <div className="app-page space-y-6">
      <PageHeader
        title="Estadísticas de equipos"
        subtitle="Compara el rendimiento de los equipos en métricas ofensivas y defensivas"
      />
      
      {/* Scatter Plot */}
      <div className="rounded-md border border-acb-200 bg-white p-4">
        <div className="mb-4 flex flex-wrap items-center gap-x-3 gap-y-2">
          <div className="flex min-w-0 items-center gap-1.5">
            <label htmlFor="team-stats-season" className="field-label whitespace-nowrap text-xs">Temporada</label>
            <select
              id="team-stats-season"
              value={selectedSeason}
              onChange={(e) => setSelectedSeason(e.target.value === 'all' ? 'all' : parseInt(e.target.value))}
              className="form-control h-8 w-auto min-w-[7rem] px-2 text-xs"
            >
              {availableSeasons.map(season => (
                <option key={season} value={season}>{season-1}-{String(season).slice(-2)}</option>
              ))}
              <option value="all">Todas las temporadas</option>
            </select>
          </div>
          <div className="segmented-control h-8" role="group" aria-label="Fase de la competición">
            <button
              onClick={() => setSelectedStage('regular')}
              aria-pressed={selectedStage === 'regular'}
              className="segmented-option px-2 py-1 text-xs"
            >
              Temporada regular
            </button>
            <button
              onClick={() => setSelectedStage('playoffs')}
              aria-pressed={selectedStage === 'playoffs'}
              className="segmented-option px-2 py-1 text-xs"
            >
              Playoffs
            </button>
          </div>
          <div className="flex min-w-0 items-center gap-1.5">
            <label htmlFor="team-stats-x-axis" className="field-label whitespace-nowrap text-xs">Eje X</label>
            <select
              id="team-stats-x-axis"
              value={xAxis}
              onChange={(e) => setXAxis(e.target.value)}
              className="form-control h-8 w-auto min-w-[9rem] max-w-[12rem] px-2 text-xs"
            >
              {statOptions.map(opt => (
                <option key={opt.value} value={opt.value}>{opt.label}</option>
              ))}
            </select>
          </div>
          <div className="flex min-w-0 items-center gap-1.5">
            <label htmlFor="team-stats-y-axis" className="field-label whitespace-nowrap text-xs">Eje Y</label>
            <select
              id="team-stats-y-axis"
              value={yAxis}
              onChange={(e) => setYAxis(e.target.value)}
              className="form-control h-8 w-auto min-w-[9rem] max-w-[12rem] px-2 text-xs"
            >
              {statOptions.map(opt => (
                <option key={opt.value} value={opt.value}>{opt.label}</option>
              ))}
            </select>
          </div>
          <div className="flex items-center">
            <button
              onClick={() => setShowLabels(!showLabels)}
              className={`form-control h-8 w-auto px-2 text-xs hover:bg-acb-50 ${showLabels ? 'bg-acb-100' : ''}`}
            >
              {showLabels ? 'Ocultar nombres' : 'Mostrar nombres'}
            </button>

          </div>
        </div>
        
        <div className="h-96 min-w-0 w-full">
          <ResponsiveContainer width="100%" height="100%">
            <ScatterChart margin={{ top: 20, right: 20, bottom: 40, left: 40 }}>
              <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
              <XAxis
                type="number"
                dataKey={xAxis}
                name={statOptions.find(s => s.value === xAxis)?.label}
                domain={xAxisScale.domain}
                ticks={xAxisScale.ticks}
                stroke="#64748b"
                fontSize={11}
                tickFormatter={(v) => formatAxisValue(v, xAxis)}
                label={{
                  value: statOptions.find(s => s.value === xAxis)?.label,
                  position: 'bottom',
                  style: { fill: '#64748b', fontSize: 11 }
                }}
              />
              <YAxis
                type="number"
                dataKey={yAxis}
                name={statOptions.find(s => s.value === yAxis)?.label}
                domain={yAxisScale.domain}
                ticks={yAxisScale.ticks}
                stroke="#64748b"
                fontSize={11}
                tickFormatter={(v) => formatAxisValue(v, yAxis)}
                label={{
                  value: statOptions.find(s => s.value === yAxis)?.label,
                  angle: -90,
                  position: 'left',
                  style: { fill: '#64748b', fontSize: 11 }
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
                      {selectedSeason === 'all' && (
                        <div className="text-xs text-acb-500 mb-1">{team.season - 1}-{String(team.season).slice(-2)}</div>
                      )}
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
                  key={teamRecordKey(team)}
                  data={[team]}
                  shape={(props) => (
                    <TeamDot
                      {...props}
                      teamLogos={teamLogos}
                      color={team.color}
                      highlighted={highlightTeam === teamRecordKey(team)}
                    />
                  )}
                  onMouseEnter={() => setHighlightTeam(teamRecordKey(team))}
                  onMouseLeave={() => setHighlightTeam(null)}
                >
                  {showLabels && (
                    <LabelList
                      dataKey="displayLabel"
                      position="right"
                      offset={10}
                      style={{ fontSize: '9px', fontWeight: 'bold', fill: '#374151' }}
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
                {selectedSeason === 'all' && (
                  <th
                    rowSpan={2}
                    onClick={() => handleSort('season')}
                    onKeyDown={(e) => (e.key === 'Enter' || e.key === ' ') && handleSort('season')}
                    tabIndex={0}
                    aria-sort={sortKey === 'season' ? (sortDir === 'desc' ? 'descending' : 'ascending') : 'none'}
                    className="data-table-head text-left cursor-pointer hover:bg-acb-100"
                  >
                    Temp.
                  </th>
                )}
                <th rowSpan={2} onClick={() => handleSort('games')} onKeyDown={(e) => (e.key === 'Enter' || e.key === ' ') && handleSort('games')} tabIndex={0} aria-sort={sortKey === 'games' ? (sortDir === 'desc' ? 'descending' : 'ascending') : 'none'} title={statTitle('PJ')} className="data-table-head data-table-number data-col-games cursor-pointer hover:bg-acb-100">
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
                    onKeyDown={(e) => (e.key === 'Enter' || e.key === ' ') && handleSort(col.key)}
                    tabIndex={0}
                    aria-sort={sortKey === col.key ? (sortDir === 'desc' ? 'descending' : 'ascending') : 'none'}
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
              {sortedTeams.map((team) => (
                <tr
                  key={teamRecordKey(team)}
                  className={`data-table-row border-b border-acb-100
                    ${highlightTeam === teamRecordKey(team) ? 'bg-accent-50' : ''}`}
                  onMouseEnter={() => setHighlightTeam(teamRecordKey(team))}
                  onMouseLeave={() => setHighlightTeam(null)}
                >
                  <td className="data-table-cell data-table-identity data-table-sticky data-col-team">
                    {team.team}
                  </td>
                  {selectedSeason === 'all' && (
                    <td className="data-table-cell whitespace-nowrap text-acb-600">
                      {team.season - 1}-{String(team.season).slice(-2)}
                    </td>
                  )}
                  {tableColumns.slice(1).map(col => {
                    const rank = rankings[teamRecordKey(team)]?.[col.key]
                    const showRank = col.key !== 'team' && col.key !== 'games' && col.key !== 'wins' && col.key !== 'losses' && rank != null
                    const totalTeams = enrichedTeams.length

                    return (
                      <td
                        key={col.key}
                        className={`data-table-cell
                          ${col.align === 'right' ? 'data-table-number' : ''}
                          ${col.key === 'games' ? 'data-col-games' : 'data-col-number'}`}
                      >
                        {showRank ? (
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
