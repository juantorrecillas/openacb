import { useState, useMemo } from 'react'
import { useNavigate } from 'react-router-dom'
import { Search, ArrowUp, ArrowDown, Filter, Download } from 'lucide-react'
import { downloadTableAsCsv } from '../utils/csvDownload'
import { statTitle } from '../utils/statLabels'
import { getPlayerDisplayName, getPlayerSearchText } from '../utils/playerNames'

const positionCol = { key: 'position', label: 'Pos', align: 'left', sortable: true }

// Basic boxscore stats columns - with percentile key for inline display
const basicColumns = [
  { key: 'playerFull', label: 'Jugador', align: 'left', sortable: true },
  { key: 'team', label: 'Equipo', align: 'left', sortable: true },
  positionCol,
  { key: 'games', label: 'PJ', align: 'right', sortable: true },
  { key: 'mpg', label: 'MPP', align: 'right', sortable: true, pctKey: 'mpgPct', posPctKey: 'mpgPosPct' },
  { key: 'ppg', label: 'PPP', align: 'right', sortable: true, highlight: true, pctKey: 'ppgPct', posPctKey: 'ppgPosPct' },
  { key: 'fgmpg', label: 'TCA', align: 'right', sortable: true },
  { key: 'fgapg', label: 'TCI', align: 'right', sortable: true },
  { key: 'fgPct', label: 'TC%', align: 'right', sortable: true, pctKey: 'fgPctPct', posPctKey: 'fgPctPosPct' },
  { key: 'fg3mpg', label: '3PA', align: 'right', sortable: true },
  { key: 'fg3apg', label: '3PI', align: 'right', sortable: true },
  { key: 'fg3Pct', label: '3P%', align: 'right', sortable: true, pctKey: 'fg3PctPct', posPctKey: 'fg3PctPosPct' },
  { key: 'ftmpg', label: 'TLA', align: 'right', sortable: true },
  { key: 'ftapg', label: 'TLI', align: 'right', sortable: true },
  { key: 'ftPct', label: 'TL%', align: 'right', sortable: true, pctKey: 'ftPctPct', posPctKey: 'ftPctPosPct' },
  { key: 'orebpg', label: 'RO', align: 'right', sortable: true, pctKey: 'orebpgPct', posPctKey: 'orebpgPosPct' },
  { key: 'drebpg', label: 'RD', align: 'right', sortable: true, pctKey: 'drebpgPct', posPctKey: 'drebpgPosPct' },
  { key: 'rpg', label: 'RPP', align: 'right', sortable: true, pctKey: 'rpgPct', posPctKey: 'rpgPosPct' },
  { key: 'apg', label: 'APP', align: 'right', sortable: true, pctKey: 'apgPct', posPctKey: 'apgPosPct' },
  { key: 'spg', label: 'RBP', align: 'right', sortable: true, pctKey: 'spgPct', posPctKey: 'spgPosPct' },
  { key: 'bpg', label: 'TAPP', align: 'right', sortable: true, pctKey: 'bpgPct', posPctKey: 'bpgPosPct' },
  { key: 'topg', label: 'PER', align: 'right', sortable: true, inverse: true, pctKey: 'topgPct', posPctKey: 'topgPosPct' },
  { key: 'fpg', label: 'FPP', align: 'right', sortable: true, inverse: true, pctKey: 'fpgPct', posPctKey: 'fpgPosPct' },
]

// Advanced stats - shooting efficiency and rate statistics
const advancedColumns = [
  { key: 'playerFull', label: 'Jugador', align: 'left', sortable: true },
  { key: 'team', label: 'Equipo', align: 'left', sortable: true },
  positionCol,
  { key: 'games', label: 'PJ', align: 'right', sortable: true },
  { key: 'ortg', label: 'ORtg', align: 'right', sortable: true, highlight: true, pctKey: 'ortgPct', posPctKey: 'ortgPosPct' },
  { key: 'usg', label: 'USG%', align: 'right', sortable: true, pctKey: 'usgPct', posPctKey: 'usgPosPct' },
  { key: 'efg', label: 'eFG%', align: 'right', sortable: true, pctKey: 'efgPct', posPctKey: 'efgPosPct' },
  { key: 'ts', label: 'TS%', align: 'right', sortable: true, pctKey: 'tsPct', posPctKey: 'tsPosPct' },
  { key: 'threeRate', label: '3PAr', align: 'right', sortable: true, pctKey: 'threeRatePct', posPctKey: 'threeRatePosPct' },
  { key: 'orbPct', label: 'RO%', align: 'right', sortable: true, pctKey: 'orbPctPct', posPctKey: 'orbPctPosPct' },
  { key: 'drbPct', label: 'RD%', align: 'right', sortable: true, pctKey: 'drbPctPct', posPctKey: 'drbPctPosPct' },
  { key: 'trbPct', label: 'REB%', align: 'right', sortable: true, pctKey: 'trbPctPct', posPctKey: 'trbPctPosPct' },
  { key: 'astPct', label: 'AST%', align: 'right', sortable: true, pctKey: 'astPctPct', posPctKey: 'astPctPosPct' },
  { key: 'stlPct', label: 'ROB%', align: 'right', sortable: true, pctKey: 'stlPctPct', posPctKey: 'stlPctPosPct' },
  { key: 'blkPct', label: 'TAP%', align: 'right', sortable: true, pctKey: 'blkPctPct', posPctKey: 'blkPctPosPct' },
  { key: 'tovPct', label: 'PER%', align: 'right', sortable: true, inverse: true, pctKey: 'tovPctPct', posPctKey: 'tovPctPosPct' },
]

// Miscellaneous stats - contextual variables and play type percentages
const miscColumns = [
  { key: 'playerFull', label: 'Jugador', align: 'left', sortable: true },
  { key: 'team', label: 'Equipo', align: 'left', sortable: true },
  positionCol,
  { key: 'games', label: 'PJ', align: 'right', sortable: true },
  { key: 'mpg', label: 'MPP', align: 'right', sortable: true },
  { key: 'ppg', label: 'PPP', align: 'right', sortable: true },
  { key: 'offTo', label: 'Pts Robo%', align: 'right', sortable: true },
  { key: 'secondChance', label: '2da Op%', align: 'right', sortable: true },
  { key: 'assistedFgm', label: '% asistidos', align: 'right', sortable: true },
  { key: 'assistedFgm2', label: 'Ast 2P%', align: 'right', sortable: true },
  { key: 'assistedFgm3', label: 'Ast 3P%', align: 'right', sortable: true },
]

// Zone shooting frequency columns (% of shots from each zone)
const frequencyColumns = [
  { key: 'playerFull', label: 'Jugador', align: 'left', sortable: true },
  { key: 'team', label: 'Equipo', align: 'left', sortable: true },
  positionCol,
  { key: 'games', label: 'PJ', align: 'right', sortable: true },
  { key: 'mpg', label: 'MPP', align: 'right', sortable: true },
  { key: 'freqRim', label: 'Aro', title: 'Zona restringida', align: 'right', sortable: true, zone: true, fgaKey: 'fgaRim' },
  { key: 'freqShortMid', label: 'Pintura', title: 'Zona no restringida', align: 'right', sortable: true, zone: true, fgaKey: 'fgaShortMid' },
  { key: 'freqLongMid', label: 'Media', title: 'Media distancia', align: 'right', sortable: true, zone: true, fgaKey: 'fgaLongMid' },
  { key: 'freqCornerThree', label: '3P esq.', title: 'Triple desde la esquina', align: 'right', sortable: true, zone: true, fgaKey: 'fgaCornerThree' },
  { key: 'freqNcThree', label: '3P frontal', title: 'Triple fuera de la esquina', align: 'right', sortable: true, zone: true, fgaKey: 'fgaNcThree' },
  { key: 'freqAllThree', label: '3P total', title: 'Total de triples', align: 'right', sortable: true, zone: true, fgaKey: 'fgaAllThree' },
]

// Zone shooting accuracy columns (FG% per zone)
const accuracyColumns = [
  { key: 'playerFull', label: 'Jugador', align: 'left', sortable: true },
  { key: 'team', label: 'Equipo', align: 'left', sortable: true },
  positionCol,
  { key: 'games', label: 'PJ', align: 'right', sortable: true },
  { key: 'mpg', label: 'MPP', align: 'right', sortable: true },
  { key: 'fgpctRim', label: 'Aro', title: 'Zona restringida', align: 'right', sortable: true, zone: true, fgaKey: 'fgaRim' },
  { key: 'fgpctShortMid', label: 'Pintura', title: 'Zona no restringida', align: 'right', sortable: true, zone: true, fgaKey: 'fgaShortMid' },
  { key: 'fgpctLongMid', label: 'Media', title: 'Media distancia', align: 'right', sortable: true, zone: true, fgaKey: 'fgaLongMid' },
  { key: 'fgpctCornerThree', label: '3P esq.', title: 'Triple desde la esquina', align: 'right', sortable: true, zone: true, fgaKey: 'fgaCornerThree' },
  { key: 'fgpctNcThree', label: '3P frontal', title: 'Triple fuera de la esquina', align: 'right', sortable: true, zone: true, fgaKey: 'fgaNcThree' },
  { key: 'fgpctAllThree', label: '3P total', title: 'Total de triples', align: 'right', sortable: true, zone: true, fgaKey: 'fgaAllThree' },
]

// Raw counting totals — no per-game normalization
const absolutesColumns = [
  { key: 'playerFull', label: 'Jugador', align: 'left', sortable: true },
  { key: 'team', label: 'Equipo', align: 'left', sortable: true },
  positionCol,
  { key: 'games', label: 'PJ', align: 'right', sortable: true },
  { key: 'points', label: 'Pts', align: 'right', sortable: true },
  { key: 'oreb', label: 'RO', align: 'right', sortable: true },
  { key: 'dreb', label: 'RD', align: 'right', sortable: true },
  { key: 'rebounds', label: 'Reb', align: 'right', sortable: true },
  { key: 'assists', label: 'Ast', align: 'right', sortable: true },
  { key: 'steals', label: 'Rob', align: 'right', sortable: true },
  { key: 'blocks', label: 'Tap', align: 'right', sortable: true },
  { key: 'turnovers', label: 'Pér', align: 'right', sortable: true },
  { key: 'fouls', label: 'Fal', align: 'right', sortable: true },
  { key: 'fgm3', label: '3PM', align: 'right', sortable: true },
  { key: 'fgm', label: 'TCA', align: 'right', sortable: true },
  { key: 'ftm', label: 'TLA', align: 'right', sortable: true },
]

// Opponent zone shooting columns (defensive impact - showing FG% allowed and differential)
const defenseColumns = [
  { key: 'playerFull', label: 'Jugador', align: 'left', sortable: true },
  { key: 'team', label: 'Equipo', align: 'left', sortable: true },
  positionCol,
  { key: 'games', label: 'PJ', align: 'right', sortable: true },
  { key: 'mpg', label: 'MPP', align: 'right', sortable: true },
  { key: 'oppDiffRim', label: 'Aro', title: 'Zona restringida', align: 'right', sortable: true, defense: true, fgpctKey: 'oppOnFgpctRim', fgaKey: 'oppFgaRim' },
  { key: 'oppDiffShortMid', label: 'Pintura', title: 'Zona no restringida', align: 'right', sortable: true, defense: true, fgpctKey: 'oppOnFgpctShortMid', fgaKey: 'oppFgaShortMid' },
  { key: 'oppDiffLongMid', label: 'Media', title: 'Media distancia', align: 'right', sortable: true, defense: true, fgpctKey: 'oppOnFgpctLongMid', fgaKey: 'oppFgaLongMid' },
  { key: 'oppDiffCornerThree', label: '3P esq.', title: 'Triple desde la esquina', align: 'right', sortable: true, defense: true, fgpctKey: 'oppOnFgpctCornerThree', fgaKey: 'oppFgaCornerThree' },
  { key: 'oppDiffNcThree', label: '3P frontal', title: 'Triple fuera de la esquina', align: 'right', sortable: true, defense: true, fgpctKey: 'oppOnFgpctNcThree', fgaKey: 'oppFgaNcThree' },
  { key: 'oppDiffAllThree', label: '3P total', title: 'Total de triples', align: 'right', sortable: true, defense: true, fgpctKey: 'oppOnFgpctAllThree', fgaKey: 'oppFgaAllThree' },
]

export default function PlayerStats({ players, playerBio = {} }) {
  const navigate = useNavigate()
  // Enrich players with position from playerBio lookup and derived per-game shooting fields
  const enrichedPlayers = useMemo(() => {
    return players.map(p => {
      const pos = typeof p.position === 'string' && p.position
        ? p.position
        : (playerBio[String(p.licenseId)]?.position || null)
      const g = p.games || 1
      const fgmpg  = p.fgm  != null ? Math.round(p.fgm  / g * 10) / 10 : null
      const fgapg  = p.fga  != null ? Math.round(p.fga  / g * 10) / 10 : null
      const fg3mpg = p.fgm3 != null ? Math.round(p.fgm3 / g * 10) / 10 : null
      const fg3apg = p.fga3 != null ? Math.round(p.fga3 / g * 10) / 10 : null
      const ftmpg  = p.ftm  != null ? Math.round(p.ftm  / g * 10) / 10 : null
      const ftapg  = p.fta  != null ? Math.round(p.fta  / g * 10) / 10 : null
      return { ...p, position: pos, fgmpg, fgapg, fg3mpg, fg3apg, ftmpg, ftapg }
    })
  }, [players, playerBio])

  // Get available seasons and default to most recent
  const availableSeasons = useMemo(() => {
    const seasons = [...new Set(enrichedPlayers.map(p => p.season))].sort((a, b) => b - a)
    return seasons
  }, [enrichedPlayers])

  const [selectedSeason, setSelectedSeason] = useState(availableSeasons[0] || 2025)
  const [selectedStage, setSelectedStage] = useState('regular')
  const [viewMode, setViewMode] = useState('basic') // 'basic', 'advanced', 'misc', 'frequency', 'accuracy', 'defense'
  const [search, setSearch] = useState('')
  const [sortKey, setSortKey] = useState('playerFull')
  const [sortDir, setSortDir] = useState('asc')
  const [teamFilter, setTeamFilter] = useState('')
  const [positionFilter, setPositionFilter] = useState('')
  const [showFilteredPlayers, setShowFilteredPlayers] = useState(false)
  const [pctMode, setPctMode] = useState('league') // 'league' or 'position'

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
    if (selectedStage === 'playoffs') {
      return player.games >= 2 && player.totalMinutes >= 20
    } else if (player.season === mostRecentSeason) {
      return player.games >= 5 && player.totalMinutes >= 50
    } else {
      return player.games >= 10 && player.totalMinutes >= 150
    }
  }

  const columns = viewMode === 'basic' ? basicColumns
    : viewMode === 'advanced' ? advancedColumns
    : viewMode === 'absolutos' ? absolutesColumns
    : viewMode === 'misc' ? miscColumns
    : viewMode === 'frequency' ? frequencyColumns
    : viewMode === 'accuracy' ? accuracyColumns
    : defenseColumns

  const stageFilteredPlayers = useMemo(() => {
    return enrichedPlayers.filter(p => (p.competitionStage || 'regular') === selectedStage)
  }, [enrichedPlayers, selectedStage])

  // Filter players by season
  const seasonFilteredPlayers = useMemo(() => {
    if (selectedSeason === 'all') return stageFilteredPlayers
    return stageFilteredPlayers.filter(p => p.season === selectedSeason)
  }, [stageFilteredPlayers, selectedSeason])

  const teams = useMemo(() =>
    [...new Set(seasonFilteredPlayers.map(p => p.team))].sort(),
    [seasonFilteredPlayers]
  )

  const POSITION_ORDER = ['Base', 'Escolta', 'Alero', 'Ala-pívot', 'Pívot']
  const positions = useMemo(() => {
    const present = new Set(seasonFilteredPlayers.map(p => p.position).filter(v => typeof v === 'string' && v.trim()))
    return POSITION_ORDER.filter(pos => present.has(pos))
  }, [seasonFilteredPlayers])

  // Players that meet the minimum threshold (for percentile calculation)
  const qualifiedPlayers = useMemo(() => {
    return seasonFilteredPlayers.filter(meetsMinimumThreshold)
  }, [seasonFilteredPlayers, mostRecentSeason])

  // Count of filtered out players
  const filteredOutCount = useMemo(() => {
    return seasonFilteredPlayers.length - qualifiedPlayers.length
  }, [seasonFilteredPlayers, qualifiedPlayers])

  // Helper function to extract surname for sorting
  // Handles: "J. Rubio" -> "Rubio", "M. A. Gasol" -> "A. Gasol", "Luwawu-Cabarrot" -> "Luwawu-Cabarrot"
  const getSortKey = (name) => {
    if (!name || typeof name !== 'string') return ''
    
    // Normalize: trim and replace multiple spaces
    const normalized = name.trim().replace(/\s+/g, ' ')
    
    // Split by dot to handle initials
    const dotParts = normalized.split('.')
    if (dotParts.length >= 2) {
      // Take everything after the last dot
      const afterLastDot = dotParts.slice(1).join('.').trim()
      if (afterLastDot) {
        return afterLastDot
      }
    }
    
    // No dots - use full name
    return normalized
  }

  const filteredPlayers = useMemo(() => {
    // Start with either all players or only qualified players
    const basePlayers = showFilteredPlayers ? seasonFilteredPlayers : qualifiedPlayers

    return basePlayers
      .filter(p => {
        if (search && !getPlayerSearchText(p).includes(search.toLocaleLowerCase('es'))) {
          return false
        }
        if (teamFilter && p.team !== teamFilter && teamFilter !== '') return false
        if (positionFilter && (typeof p.position !== 'string' || p.position !== positionFilter)) return false
        return true
      })
      .sort((a, b) => {
        let aVal = (typeof a[sortKey] === 'string' || typeof a[sortKey] === 'number') ? a[sortKey] : 0
        let bVal = (typeof b[sortKey] === 'string' || typeof b[sortKey] === 'number') ? b[sortKey] : 0

        // When sorting by player name, use playerAbbrev if available
        if (sortKey === 'playerFull') {
          aVal = getPlayerDisplayName(a)
          bVal = getPlayerDisplayName(b)
        }

        if (typeof aVal === 'string') {
          // For player names, sort by surname instead of full name
          if (sortKey === 'playerFull') {
            const aSortKey = getSortKey(aVal)
            const bSortKey = getSortKey(bVal)
            return sortDir === 'desc'
              ? bSortKey.localeCompare(aSortKey)
              : aSortKey.localeCompare(bSortKey)
          }
          return sortDir === 'desc'
            ? bVal.localeCompare(aVal)
            : aVal.localeCompare(bVal)
        }
        return sortDir === 'desc' ? bVal - aVal : aVal - bVal
      })
  }, [seasonFilteredPlayers, qualifiedPlayers, showFilteredPlayers, search, sortKey, sortDir, teamFilter, positionFilter])

  const handleSort = (key) => {
    if (sortKey === key) {
      setSortDir(sortDir === 'desc' ? 'asc' : 'desc')
    } else {
      setSortKey(key)
      setSortDir('desc')
    }
  }

  const formatValue = (value, key, player) => {
    if (key === 'playerFull') return getPlayerDisplayName(player)
    if (value === undefined || value === null) {
      const isZoneStat = key.startsWith('freq') || key.startsWith('fgpct') ||
        (key.startsWith('fga') && key.length > 3) ||
        key.startsWith('oppOnFgpct') || key.startsWith('oppDiff') || key.startsWith('oppFga')
      return isZoneStat ? 'N/D' : '-'
    }
    if (key === 'team') return typeof value === 'string' ? value : '-'
    if (key === 'position') return typeof value === 'string' && value ? value : '-'

    // Integer values
    if (key === 'games') return value

    // Absolutos raw totals
    if (['points','rebounds','oreb','dreb','assists','steals','blocks','turnovers','fouls',
         'fgm','fga','fgm2','fga2','fgm3','fga3','ftm','fta'].includes(key))
      return String(Math.round(value))

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

    // Contextual variables - points from different play types
    // These are stored as decimals (0-1) and need to be multiplied by 100
    if (key === 'offTo' || key === 'secondChance' || key === 'assistedFgm' ||
        key === 'assistedFgm2' || key === 'assistedFgm3') {
      return `${(value * 100).toFixed(1)}%`
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

  const getPercentileColor = () => 'text-acb-700'

  const handleDownloadCsv = () => {
    const seasonStr = selectedSeason === 'all'
      ? 'todas-temporadas'
      : `${selectedSeason - 1}-${String(selectedSeason).slice(-2)}`
    const viewStr = viewMode === 'basic' ? 'basico'
      : viewMode === 'advanced' ? 'avanzado'
      : viewMode === 'absolutos' ? 'absolutos'
      : viewMode === 'misc' ? 'otros'
      : viewMode === 'frequency' ? 'tiro-frecuencia'
      : viewMode === 'accuracy' ? 'tiro-precision'
      : 'tiro-rival'
    const filename = `jugadores_${seasonStr}_${selectedStage}_${viewStr}.csv`

    const exportColumns = [
      { key: 'season', label: 'Temporada' },
      ...columns.map(c => ({ key: c.key, label: c.label })),
    ]

    const exportRows = filteredPlayers.map(p => {
      const row = { season: `${p.season - 1}-${String(p.season).slice(-2)}` }
      columns.forEach(col => {
        const v = p[col.key]
        if (col.key === 'playerFull') {
          row[col.key] = getPlayerDisplayName(p, '')
        } else if (col.key === 'team' || col.key === 'position') {
          row[col.key] = (typeof v === 'string' && v) ? v : ''
        } else if (v == null) {
          row[col.key] = ''
        } else {
          // mirror display semantics but strip % / leading + so values are spreadsheet-friendly
          const formatted = formatValue(v, col.key, p)
          if (formatted === '-' || formatted === 'N/D') {
            row[col.key] = ''
          } else {
            row[col.key] = String(formatted).replace('%', '').replace(/^\+/, '')
          }
        }
      })
      return row
    })

    downloadTableAsCsv(filename, exportRows, exportColumns)
  }

  // Get percentile badge color based on percentile value (0-100)
  const getPercentileBadgeColor = (percentile) => {
    if (percentile == null || isNaN(percentile)) return 'bg-acb-100 text-acb-600'
    if (percentile >= 75) return 'bg-positive-100 text-positive-700'
    if (percentile >= 50) return 'bg-info-100 text-info-700'
    if (percentile >= 25) return 'bg-info-100 text-info-600'
    return 'bg-negative-100 text-negative-700'
  }

  return (
    <div className="app-page space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-semibold text-acb-900">Estadísticas de Jugador</h2>
        <p className="text-acb-500 text-sm mt-1">
          Estadísticas básicas y avanzadas de jugadores
        </p>
      </div>

      {/* Filters */}
      <div className="bg-white rounded-lg border border-acb-200 p-4">
        <div className="flex flex-wrap items-center gap-4 mb-4">
          {/* Season Filter */}
          <div className="flex items-center gap-2">
            <span className="text-sm text-acb-600">Temporada:</span>
            <select
              value={selectedSeason}
              onChange={(e) => setSelectedSeason(e.target.value === 'all' ? 'all' : parseInt(e.target.value))}
              className="px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
            >
              {availableSeasons.map(season => (
                <option key={season} value={season}>{season-1}-{String(season).slice(-2)}</option>
              ))}
              <option value="all">Todas las Temporadas</option>
            </select>
          </div>

          <div className="flex items-center gap-1 bg-acb-100 rounded-md p-1">
            <button
              onClick={() => { setSelectedStage('regular'); setTeamFilter('') }}
              className={`px-3 py-1.5 text-sm font-medium rounded transition-colors ${
                selectedStage === 'regular' ? 'bg-white text-acb-900 shadow-sm' : 'text-acb-600 hover:text-acb-900'
              }`}
            >
              Temporada regular
            </button>
            <button
              onClick={() => { setSelectedStage('playoffs'); setTeamFilter('') }}
              className={`px-3 py-1.5 text-sm font-medium rounded transition-colors ${
                selectedStage === 'playoffs' ? 'bg-white text-acb-900 shadow-sm' : 'text-acb-600 hover:text-acb-900'
              }`}
            >
              Playoffs
            </button>
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
              onClick={() => { setViewMode('absolutos'); setSortKey('points'); setSortDir('desc') }}
              className={`px-3 py-1.5 text-sm font-medium rounded transition-colors
                ${viewMode === 'absolutos'
                  ? 'bg-white text-acb-900 shadow-sm'
                  : 'text-acb-600 hover:text-acb-900'}`}
            >
              Absolutos
            </button>
            <button
              onClick={() => setViewMode('misc')}
              className={`px-3 py-1.5 text-sm font-medium rounded transition-colors
                ${viewMode === 'misc'
                  ? 'bg-white text-acb-900 shadow-sm'
                  : 'text-acb-600 hover:text-acb-900'}`}
            >
              Otros
            </button>
            <button
              onClick={() => setViewMode('frequency')}
              className={`px-3 py-1.5 text-sm font-medium rounded transition-colors
                ${viewMode === 'frequency'
                  ? 'bg-white text-acb-900 shadow-sm'
                  : 'text-acb-600 hover:text-acb-900'}`}
            >
              Tiro: Frecuencia
            </button>
            <button
              onClick={() => setViewMode('accuracy')}
              className={`px-3 py-1.5 text-sm font-medium rounded transition-colors
                ${viewMode === 'accuracy'
                  ? 'bg-white text-acb-900 shadow-sm'
                  : 'text-acb-600 hover:text-acb-900'}`}
            >
              Tiro: Precisión
            </button>
            <button
              onClick={() => setViewMode('defense')}
              className={`px-3 py-1.5 text-sm font-medium rounded transition-colors
                ${viewMode === 'defense'
                  ? 'bg-white text-acb-900 shadow-sm'
                  : 'text-acb-600 hover:text-acb-900'}`}
            >
              Tiro Rival
            </button>
          </div>

          {/* Percentile Mode Toggle - only for views with percentiles */}
          {(viewMode === 'basic' || viewMode === 'advanced' || viewMode === 'misc') && (
            <div className="flex items-center gap-1.5">
              <span className="text-xs text-acb-500">Percentil:</span>
              <div className="flex rounded-md border border-acb-200 text-xs overflow-hidden">
                <button
                  onClick={() => setPctMode('league')}
                  className={`px-2.5 py-1 font-medium transition-colors ${pctMode === 'league' ? 'bg-acb-800 text-white' : 'bg-white text-acb-600 hover:bg-acb-50'}`}
                >Liga</button>
                <button
                  onClick={() => setPctMode('position')}
                  className={`px-2.5 py-1 font-medium transition-colors ${pctMode === 'position' ? 'bg-acb-800 text-white' : 'bg-white text-acb-600 hover:bg-acb-50'}`}
                >Posición</button>
              </div>
            </div>
          )}

          {/* Search */}
          <div className="relative flex-1 min-w-[200px]">
            <Search className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-acb-400" />
            <input
              type="text"
              value={search}
              onChange={(e) => setSearch(e.target.value)}
              placeholder="Buscar jugadores..."
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
              <option value="">Todos los Equipos</option>
              {teams.map(team => (
                <option key={team} value={team}>{team}</option>
              ))}
            </select>
          </div>

          {/* Position Filter */}
          {positions.length > 0 && (
            <select
              value={positionFilter}
              onChange={(e) => setPositionFilter(e.target.value)}
              className="px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
            >
              <option value="">Todas las Posiciones</option>
              {positions.map(pos => (
                <option key={pos} value={pos}>{pos}</option>
              ))}
            </select>
          )}

          {/* Show Filtered Players Toggle */}
          {filteredOutCount > 0 && (
            <button
              onClick={() => setShowFilteredPlayers(!showFilteredPlayers)}
              className={`px-3 py-2 text-sm border rounded-md transition-colors
                ${showFilteredPlayers
                  ? 'bg-acb-100 border-acb-300 text-acb-700'
                  : 'bg-white border-acb-200 text-acb-500 hover:bg-acb-50'}`}
            >
              {showFilteredPlayers ? 'Ocultar' : 'Mostrar'} {filteredOutCount} filtrados
            </button>
          )}
        </div>
      </div>

      {/* Results count + download */}
      <div className="flex items-center justify-between flex-wrap gap-3">
        <div className="text-sm text-acb-500">
          Mostrando {filteredPlayers.length} de {qualifiedPlayers.length + filteredOutCount} jugadores
          {filteredOutCount > 0 && !showFilteredPlayers && (
            <span className="text-acb-400"> ({filteredOutCount} filtrados al no cumplir mínimos en partidos y minutos)</span>
          )}
        </div>
        <button
          onClick={handleDownloadCsv}
          disabled={filteredPlayers.length === 0}
          className="inline-flex items-center gap-1.5 px-3 py-1.5 border border-acb-200 rounded text-sm bg-white text-acb-700 hover:bg-acb-50 disabled:opacity-50 disabled:cursor-not-allowed"
          title="Descargar la tabla actual como CSV"
        >
          <Download className="w-4 h-4" />
          Descargar CSV
        </button>
      </div>

      {/* Zone data unavailability notice */}
      {(viewMode === 'frequency' || viewMode === 'accuracy' || viewMode === 'defense') && (
        selectedSeason !== 'all' && selectedSeason < 2021
          ? <div className="text-sm text-amber-700 bg-amber-50 border border-amber-200 rounded-md px-4 py-2">
              Los datos de tiro por zonas no están disponibles para temporadas anteriores a 2020-21.
            </div>
          : selectedSeason === 'all' && (
            <div className="text-sm text-acb-500 bg-acb-50 border border-acb-200 rounded-md px-4 py-2">
              Los datos de tiro por zonas están disponibles desde la temporada 2020-21. Las temporadas anteriores muestran <span className="font-medium">N/D</span>.
            </div>
          )
      )}

      {/* Table */}
      <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
        <div className="overflow-x-auto">
          <table className="data-table min-w-full">
            <thead>
              <tr className="bg-acb-50 border-b border-acb-200">
                <th className="data-table-head data-table-number data-table-sticky data-table-sticky-head data-col-rank bg-acb-50">
                  #
                </th>
                {columns.map(col => (
                  <th
                    key={col.key}
                    onClick={() => col.sortable && handleSort(col.key)}
                    title={col.title || statTitle(col.label)}
                    className={`data-table-head
                      ${col.align === 'right' ? 'data-table-number' : 'text-left'}
                      ${col.key === 'playerFull' ? 'data-table-sticky-after-rank data-table-sticky-head data-col-player bg-acb-50' : col.key === 'team' ? 'data-col-team' : col.key === 'position' ? 'data-col-position' : col.key === 'games' ? 'data-col-games' : 'data-col-number'}
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
                  onClick={() => navigate(`/jugador/${player.licenseId}`)}
                  className="data-table-row border-b border-acb-100 cursor-pointer"
                >
                  <td className="data-table-cell data-table-number data-table-sticky data-col-rank text-acb-400">
                    {i + 1}
                  </td>
                  {columns.map(col => {
                    const activePctKey = pctMode === 'position' && col.posPctKey ? col.posPctKey : col.pctKey
                    const hasPercentile = activePctKey && player[activePctKey] != null
                    const percentileValue = hasPercentile ? player[activePctKey] : null
                    const hasZoneFga = col.zone && col.fgaKey && player[col.fgaKey] != null
                    const fgaValue = hasZoneFga ? player[col.fgaKey] : null
                    const hasDefense = col.defense && col.fgpctKey && col.fgaKey && player[col.fgpctKey] != null
                    const defenseFgpct = hasDefense ? player[col.fgpctKey] : null
                    const defenseFga = hasDefense ? player[col.fgaKey] : null
                    const diffValue = hasDefense ? player[col.key] : null

                    return (
                      <td
                        key={col.key}
                        className={`data-table-cell
                          ${col.align === 'right' ? 'data-table-number' : ''}
                          ${col.key === 'playerFull' ? 'data-table-identity data-table-sticky-after-rank data-col-player' : col.key === 'team' ? 'data-col-team' : col.key === 'position' ? 'data-col-position' : col.key === 'games' ? 'data-col-games' : 'data-col-number'}
                          ${col.key === 'team' ? 'text-acb-600' : ''}`}
                      >
                        {hasDefense ? (
                          <div className="data-table-value">
                            <span className="text-acb-700">
                              {formatValue(diffValue, col.key, player)}
                            </span>
                            <span className="text-[10px] text-acb-400">
                              {defenseFgpct.toFixed(1)}% / {defenseFga}
                            </span>
                          </div>
                        ) : hasZoneFga ? (
                          <div className="data-table-value">
                            <span className="text-acb-900">
                              {formatValue(player[col.key], col.key, player)}
                            </span>
                            <span className="text-[10px] text-acb-400">
                              {fgaValue}
                            </span>
                          </div>
                        ) : hasPercentile ? (
                          <div className="data-table-value">
                            <span className={getPercentileColor(player[col.key], col.key)}>
                              {formatValue(player[col.key], col.key, player)}
                            </span>
                            <span className={`data-table-badge ${getPercentileBadgeColor(percentileValue)}`}>
                              {Math.round(percentileValue)}%
                            </span>
                          </div>
                        ) : (
                          <span className={col.align === 'right' ? getPercentileColor(player[col.key], col.key) : 'text-acb-700'}>
                            {formatValue(player[col.key], col.key, player)}
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
            Mostrando los primeros 100 jugadores. Usa los filtros para ajustar resultados.
          </div>
        )}
      </div>
    </div>
  )
}
