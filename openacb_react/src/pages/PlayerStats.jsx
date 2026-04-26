import { useState, useMemo } from 'react'
import { useNavigate } from 'react-router-dom'
import { Search, ArrowUp, ArrowDown, Filter } from 'lucide-react'

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
  { key: 'bpg', label: 'TPP', align: 'right', sortable: true, pctKey: 'bpgPct', posPctKey: 'bpgPosPct' },
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
  { key: 'assistedFgm', label: 'Pts Ast%', align: 'right', sortable: true },
  { key: 'assistedFgm2', label: 'Ast 2PT%', align: 'right', sortable: true },
  { key: 'assistedFgm3', label: 'Ast 3PT%', align: 'right', sortable: true },
]

// Zone shooting frequency columns (% of shots from each zone)
const frequencyColumns = [
  { key: 'playerFull', label: 'Jugador', align: 'left', sortable: true },
  { key: 'team', label: 'Equipo', align: 'left', sortable: true },
  positionCol,
  { key: 'games', label: 'PJ', align: 'right', sortable: true },
  { key: 'mpg', label: 'MPP', align: 'right', sortable: true },
  { key: 'freqRim', label: 'Zona restr.', align: 'right', sortable: true, zone: true, fgaKey: 'fgaRim' },
  { key: 'freqShortMid', label: 'Zona no restr.', align: 'right', sortable: true, zone: true, fgaKey: 'fgaShortMid' },
  { key: 'freqLongMid', label: 'Media Distancia', align: 'right', sortable: true, zone: true, fgaKey: 'fgaLongMid' },
  { key: 'freqCornerThree', label: 'Esquina 3', align: 'right', sortable: true, zone: true, fgaKey: 'fgaCornerThree' },
  { key: 'freqNcThree', label: 'Centro 3', align: 'right', sortable: true, zone: true, fgaKey: 'fgaNcThree' },
  { key: 'freqAllThree', label: 'Todo 3', align: 'right', sortable: true, zone: true, fgaKey: 'fgaAllThree' },
]

// Zone shooting accuracy columns (FG% per zone)
const accuracyColumns = [
  { key: 'playerFull', label: 'Jugador', align: 'left', sortable: true },
  { key: 'team', label: 'Equipo', align: 'left', sortable: true },
  positionCol,
  { key: 'games', label: 'PJ', align: 'right', sortable: true },
  { key: 'mpg', label: 'MPP', align: 'right', sortable: true },
  { key: 'fgpctRim', label: 'Zona restr.', align: 'right', sortable: true, zone: true, fgaKey: 'fgaRim' },
  { key: 'fgpctShortMid', label: 'Zona no restr.', align: 'right', sortable: true, zone: true, fgaKey: 'fgaShortMid' },
  { key: 'fgpctLongMid', label: 'Media Distancia', align: 'right', sortable: true, zone: true, fgaKey: 'fgaLongMid' },
  { key: 'fgpctCornerThree', label: 'Esquina 3', align: 'right', sortable: true, zone: true, fgaKey: 'fgaCornerThree' },
  { key: 'fgpctNcThree', label: 'Centro 3', align: 'right', sortable: true, zone: true, fgaKey: 'fgaNcThree' },
  { key: 'fgpctAllThree', label: 'Todo 3', align: 'right', sortable: true, zone: true, fgaKey: 'fgaAllThree' },
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
  { key: 'oppDiffRim', label: 'Zona restr', align: 'right', sortable: true, defense: true, fgpctKey: 'oppOnFgpctRim', fgaKey: 'oppFgaRim' },
  { key: 'oppDiffShortMid', label: 'Zona no restr.', align: 'right', sortable: true, defense: true, fgpctKey: 'oppOnFgpctShortMid', fgaKey: 'oppFgaShortMid' },
  { key: 'oppDiffLongMid', label: 'Media Distancia', align: 'right', sortable: true, defense: true, fgpctKey: 'oppOnFgpctLongMid', fgaKey: 'oppFgaLongMid' },
  { key: 'oppDiffCornerThree', label: 'Esquina 3', align: 'right', sortable: true, defense: true, fgpctKey: 'oppOnFgpctCornerThree', fgaKey: 'oppFgaCornerThree' },
  { key: 'oppDiffNcThree', label: 'Centro 3', align: 'right', sortable: true, defense: true, fgpctKey: 'oppOnFgpctNcThree', fgaKey: 'oppFgaNcThree' },
  { key: 'oppDiffAllThree', label: 'Todo 3', align: 'right', sortable: true, defense: true, fgpctKey: 'oppOnFgpctAllThree', fgaKey: 'oppFgaAllThree' },
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
    if (player.season === mostRecentSeason) {
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

  // Filter players by season
  const seasonFilteredPlayers = useMemo(() => {
    if (selectedSeason === 'all') return enrichedPlayers
    return enrichedPlayers.filter(p => p.season === selectedSeason)
  }, [enrichedPlayers, selectedSeason])

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
        if (search && !p.playerFull?.toLowerCase().includes(search.toLowerCase())) {
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
        if (sortKey === 'playerFull' && typeof aVal === 'string') {
          aVal = a.playerAbbrev || aVal
          bVal = b.playerAbbrev || bVal
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
    if (value === undefined || value === null) {
      const isZoneStat = key.startsWith('freq') || key.startsWith('fgpct') ||
        (key.startsWith('fga') && key.length > 3) ||
        key.startsWith('oppOnFgpct') || key.startsWith('oppDiff') || key.startsWith('oppFga')
      return isZoneStat ? 'N/D' : '-'
    }
    if (key === 'playerFull') return player?.playerAbbrev || value
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

  // Get percentile badge color based on percentile value (0-100)
  const getPercentileBadgeColor = (percentile) => {
    if (percentile == null || isNaN(percentile)) return 'bg-acb-100 text-acb-600'
    if (percentile >= 75) return 'bg-positive-100 text-positive-700'
    if (percentile >= 50) return 'bg-info-100 text-info-700'
    if (percentile >= 25) return 'bg-info-100 text-info-600'
    return 'bg-negative-100 text-negative-700'
  }

  return (
    <div className="space-y-6">
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

      {/* Results count */}
      <div className="text-sm text-acb-500">
        Mostrando {filteredPlayers.length} de {qualifiedPlayers.length + filteredOutCount} jugadores
        {filteredOutCount > 0 && !showFilteredPlayers && (
          <span className="text-acb-400"> ({filteredOutCount} filtrados al no cumplir mínimos en partidos y minutos)</span>
        )}
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
          <table className="min-w-full">
            <thead>
              <tr className="bg-acb-50 border-b border-acb-200">
                <th className="px-4 py-3 text-left text-xs font-semibold text-acb-600 uppercase tracking-wider" style={{ width: '2.5rem', minWidth: '2.5rem' }}>
                  #
                </th>
                {columns.map(col => (
                  <th
                    key={col.key}
                    onClick={() => col.sortable && handleSort(col.key)}
                    style={{
                      width: col.key === 'playerFull' ? '10rem' : col.key === 'team' ? '7rem' : col.key === 'position' ? '6rem' : col.key === 'games' ? '3.5rem' : '5.5rem',
                      minWidth: col.key === 'playerFull' ? '10rem' : col.key === 'team' ? '7rem' : col.key === 'position' ? '6rem' : col.key === 'games' ? '3.5rem' : '5.5rem',
                    }}
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
                  onClick={() => navigate(`/jugador/${player.licenseId}`)}
                  className="border-b border-acb-100 hover:bg-acb-50 transition-colors cursor-pointer"
                >
                  <td className="px-4 py-3 text-sm text-acb-400 font-mono">
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
                        style={{
                          width: col.key === 'playerFull' ? '10rem' : col.key === 'team' ? '7rem' : col.key === 'position' ? '6rem' : col.key === 'games' ? '3.5rem' : '5.5rem',
                          minWidth: col.key === 'playerFull' ? '10rem' : col.key === 'team' ? '7rem' : col.key === 'position' ? '6rem' : col.key === 'games' ? '3.5rem' : '5.5rem',
                        }}
                        className={`px-4 py-3 text-sm whitespace-nowrap
                          ${col.align === 'right' ? 'text-right' : ''}
                          ${col.key === 'playerFull' ? 'font-medium text-acb-900' : ''}
                          ${col.key === 'team' ? 'text-acb-600' : ''}`}
                      >
                        {hasDefense ? (
                          <div className="flex flex-col items-end gap-0.5">
                            <span className="font-mono text-acb-700">
                              {formatValue(diffValue, col.key, player)}
                            </span>
                            <span className="text-xs text-acb-400">
                              {defenseFgpct.toFixed(1)}% / {defenseFga}
                            </span>
                          </div>
                        ) : hasZoneFga ? (
                          <div className="flex flex-col items-end gap-0.5">
                            <span className="font-mono text-acb-900">
                              {formatValue(player[col.key], col.key, player)}
                            </span>
                            <span className="text-xs text-acb-400">
                              {fgaValue}
                            </span>
                          </div>
                        ) : hasPercentile ? (
                          <div className="flex flex-col items-end gap-1">
                            <span className={`font-mono ${getPercentileColor(player[col.key], col.key)}`}>
                              {formatValue(player[col.key], col.key, player)}
                            </span>
                            <span className={`text-xs px-1.5 py-0.5 rounded ${getPercentileBadgeColor(percentileValue)}`}>
                              {Math.round(percentileValue)}%
                            </span>
                          </div>
                        ) : (
                          <span className={`${col.align === 'right' ? 'font-mono' : ''} ${col.align === 'right' ? getPercentileColor(player[col.key], col.key) : 'text-acb-700'}`}>
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
