import React, { useState, useMemo, useEffect } from 'react'
import { useParams, useNavigate } from 'react-router-dom'
import { getPlayerPhoto } from '../utils/playerPhotos'
import { getPlayerDisplayName as getCanonicalPlayerName } from '../utils/playerNames'
import { Users, Info, X, Search, ChevronDown, ChevronUp } from 'lucide-react'
import PageHeader from '../components/PageHeader'

/**
 * Lineup Analysis Page - Cleaning the Glass Style
 *
 * Allows users to select specific player combinations and see their on/off court impact.
 * Uses pre-calculated data from R for instant performance.
 */

// Extract licenseId from player key format "Name_12345"
const getIdFromKey = (key) => key?.split('_').pop() || ''
const MIN_ON_OFF_MINUTES = 50
const hasNumber = (value) => value != null && !Number.isNaN(Number(value))

const normalizeSearch = (value) => String(value || '')
  .normalize('NFD')
  .replace(/[\u0300-\u036f]/g, '')
  .toLocaleLowerCase('es')

// Convert team name to URL-friendly slug
function toSlug(name) {
  return name
    .normalize('NFD').replace(/[\u0300-\u036f]/g, '')
    .toLowerCase()
    .replace(/\s+/g, '-')
    .replace(/[^a-z0-9-]/g, '')
}

export default function LineupAnalysis({ teams, loadLineupsForSeason, lineupsCache, loadingLineups, playerPhotos = {}, playerRecords = [] }) {
  const { season: urlSeason, team: urlTeamSlug } = useParams()
  const navigate = useNavigate()

  // State for UI
  const [showAllPlayers, setShowAllPlayers] = useState(false)
  const [sortConfig, setSortConfig] = useState({ key: 'netDiff', direction: 'desc' })

  // Available seasons and teams
  const availableSeasons = useMemo(() => {
    const seasons = [...new Set(teams.map(t => t.season))].sort((a, b) => b - a)
    return seasons
  }, [teams])

  const [selectedSeason, setSelectedSeason] = useState(availableSeasons[0] || 2025)
  const [selectedTeam, setSelectedTeam] = useState('')

  // Sync season from URL
  useEffect(() => {
    if (urlSeason) setSelectedSeason(Number(urlSeason))
  }, [urlSeason])

  // Resolve team slug to real name once season teams are known
  const seasonTeamsForSlug = useMemo(() => {
    const s = urlSeason ? Number(urlSeason) : selectedSeason
    return teams.filter(t => t.season === s)
  }, [teams, urlSeason, selectedSeason])

  useEffect(() => {
    if (seasonTeamsForSlug.length === 0) return
    const teamNames = [...new Set(seasonTeamsForSlug.map(t => t.team))].sort((a, b) => a.localeCompare(b, 'es'))
    const urlMatch = urlTeamSlug ? teamNames.find(team => toSlug(team) === urlTeamSlug) : null
    const nextTeam = urlTeamSlug
      ? (urlMatch || teamNames[0])
      : (teamNames.includes(selectedTeam) ? selectedTeam : teamNames[0])
    if (nextTeam !== selectedTeam) setSelectedTeam(nextTeam)
    const canonicalPath = `/alineaciones/${selectedSeason}/${toSlug(nextTeam)}`
    if (Number(urlSeason) !== selectedSeason || urlTeamSlug !== toSlug(nextTeam)) {
      navigate(canonicalPath, { replace: true })
    }
  }, [navigate, selectedSeason, selectedTeam, seasonTeamsForSlug, urlSeason, urlTeamSlug])

  // Load lineups when season changes
  useEffect(() => {
    if (selectedSeason) {
      loadLineupsForSeason(selectedSeason)
    }
  }, [selectedSeason, loadLineupsForSeason])

  // Get lineups for current season from cache
  const lineupData = useMemo(() => {
    return lineupsCache[selectedSeason] || null
  }, [lineupsCache, selectedSeason])

  // Check if lineups are currently loading
  const loading = loadingLineups[selectedSeason] || false

  // Player selection state
  const [selectedPlayers, setSelectedPlayers] = useState([])
  const [excludedPlayer, setExcludedPlayer] = useState('')
  const [searchQuery, setSearchQuery] = useState('')

  // clear exclusions whenever the analysis context changes
  useEffect(() => {
    setExcludedPlayer('')
  }, [selectedSeason, selectedTeam, selectedPlayers])

  // Filter teams by season
  const seasonFilteredTeams = useMemo(() => {
    if (selectedSeason === 'all') return teams
    return teams.filter(t => t.season === selectedSeason)
  }, [teams, selectedSeason])

  const teamList = useMemo(() => seasonFilteredTeams.map(t => t.team).sort((a, b) => a.localeCompare(b, 'es')), [seasonFilteredTeams])

  // Get current team data (per-season format: data is keyed by team name directly)
  const currentTeamData = useMemo(() => {
    if (!lineupData?.data) return null
    // In per-season files, data is keyed by team name
    return lineupData.data[selectedTeam] || null
  }, [lineupData, selectedTeam])

  // Get available players for current team (keys are now nick_id format for uniqueness)
  const availablePlayers = useMemo(() => {
    if (!currentTeamData?.players) return []
    return Object.keys(currentTeamData.players).sort()
  }, [currentTeamData])

  const playerNameById = useMemo(() => {
    const names = new Map()
    playerRecords.forEach(player => {
      if (player.licenseId != null) {
        names.set(String(player.licenseId), getCanonicalPlayerName(player))
      }
    })
    return names
  }, [playerRecords])

  // Create a mapping from player key to display info
  const playerDisplayMap = useMemo(() => {
    if (!currentTeamData?.players) return {}
    const map = {}
    Object.entries(currentTeamData.players).forEach(([key, player]) => {
      const licenseId = player.id || player.licenseId || key.split('_').pop()
      const fullName = playerNameById.get(String(licenseId))
      map[key] = {
        name: fullName || player.name || player.nickname || key,
        nickname: player.nickname || key
      }
    })
    return map
  }, [currentTeamData, playerNameById])

  // Helper to get display name for a player key
  const getPlayerDisplayName = (playerKey) => {
    return playerDisplayMap[playerKey]?.name || playerKey
  }

  // find teammates with a valid directional exclusion split
  const availableExclusions = useMemo(() => {
    if (selectedPlayers.length !== 1 || !currentTeamData?.pairs) return []

    const focalPlayer = selectedPlayers[0]
    const teammates = []

    Object.values(currentTeamData.pairs).forEach(pair => {
      if (pair.player1 === focalPlayer && pair.without?.player1) {
        teammates.push(pair.player2)
      } else if (pair.player2 === focalPlayer && pair.without?.player2) {
        teammates.push(pair.player1)
      }
    })

    return teammates.sort((a, b) => {
      const nameA = playerDisplayMap[a]?.name || a
      const nameB = playerDisplayMap[b]?.name || b
      return nameA.localeCompare(nameB, 'es')
    })
  }, [currentTeamData, selectedPlayers, playerDisplayMap])

  // Get all players data for the table
  const allPlayersData = useMemo(() => {
    if (!currentTeamData?.players) return []

    const playersObj = currentTeamData.players
    return Object.entries(playersObj).map(([key, player]) => ({
      ...player,
      key,
      name: playerDisplayMap[key]?.name || player.name || player.nickname || key,
      deltaORtg: (player.onORtg ?? 0) - (player.offORtg ?? 0),
      deltaDRtg: (player.onDRtg ?? 0) - (player.offDRtg ?? 0),
      deltaEFG: (player.onEFG ?? 0) - (player.offEFG ?? 0),
      deltaTOV: (player.onTOV ?? 0) - (player.offTOV ?? 0),
      deltaORB: (player.onORB ?? 0) - (player.offORB ?? 0),
      deltaAST: (player.onAST ?? 0) - (player.offAST ?? 0),
      deltaOppEFG: (player.onOppEFG ?? 0) - (player.offOppEFG ?? 0),
      deltaOppTOV: (player.onOppTOV ?? 0) - (player.offOppTOV ?? 0),
      deltaDRB: (player.onDRB ?? 0) - (player.offDRB ?? 0),
    }))
  }, [currentTeamData, playerDisplayMap])

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

  const rankedPlayersData = useMemo(
    () => sortedPlayersData.filter(player => (player.onMin ?? 0) >= MIN_ON_OFF_MINUTES),
    [sortedPlayersData]
  )

  // Filter players by search query (search by display name, not key)
  const filteredPlayers = useMemo(() => {
    if (searchQuery.trim() === '') return availablePlayers
    const query = normalizeSearch(searchQuery)
    return availablePlayers.filter(playerKey => {
      const displayName = playerDisplayMap[playerKey]?.name || playerKey
      const nickname = playerDisplayMap[playerKey]?.nickname || playerKey
      return normalizeSearch(displayName).includes(query) ||
             normalizeSearch(nickname).includes(query)
    })
  }, [availablePlayers, searchQuery, playerDisplayMap])

  // Get data for selected players
  const getLineupDataForPlayers = () => {
    if (!currentTeamData || selectedPlayers.length === 0) return null

    const sortedPlayers = [...selectedPlayers].sort()

    if (selectedPlayers.length === 1) {
      return currentTeamData.players?.[selectedPlayers[0]] || null
    } else if (selectedPlayers.length === 2) {
      // Pairs use underscore separator
      const playerKey = sortedPlayers.join('_')
      return currentTeamData.pairs?.[playerKey] || null
    } else if (selectedPlayers.length === 3) {
      // Trios use underscore separator
      const playerKey = sortedPlayers.join('_')
      return currentTeamData.trios?.[playerKey] || null
    } else if (selectedPlayers.length === 4) {
      // No 4-player data exists (not computed in ETL)
      return null
    } else if (selectedPlayers.length === 5) {
      // 5-man lineups use pipe separator
      const lineupKey = sortedPlayers.join('|')
      return currentTeamData.lineups?.[lineupKey] || null
    }
    return null
  }

  const currentLineupData = getLineupDataForPlayers()

  // resolve the focal player's directional split without relying on pair key order
  const currentExclusionData = useMemo(() => {
    if (selectedPlayers.length !== 1 || !excludedPlayer || !currentTeamData?.pairs) return null

    const focalPlayer = selectedPlayers[0]
    const pair = Object.values(currentTeamData.pairs).find(item => (
      (item.player1 === focalPlayer && item.player2 === excludedPlayer) ||
      (item.player2 === focalPlayer && item.player1 === excludedPlayer)
    ))

    if (!pair) return null

    const split = pair.player1 === focalPlayer
      ? pair.without?.player1 || null
      : pair.without?.player2 || null

    if (!split) return null
    return {
      without: split,
      together: {
        min: pair.onMin,
        poss: pair.onPoss,
        ORtg: pair.onORtg,
        DRtg: pair.onDRtg,
        netRtg: pair.onNetRtg,
        eFG: pair.onEFG,
        TOV: pair.onTOV,
        AST: pair.onAST,
        oppEFG: pair.onOppEFG,
        DRB: pair.onDRB
      }
    }
  }, [currentTeamData, selectedPlayers, excludedPlayer])

  // shape exclusions like the duo card, with a clear net comparison row
  const exclusionAnalysisData = useMemo(() => {
    if (!currentExclusionData) return null

    const { without, together } = currentExclusionData
    const isNumber = (value) => value != null && !Number.isNaN(Number(value))
    const diff = (a, b) => (
      !isNumber(a) || !isNumber(b)
        ? null
        : Math.round((a - b) * 10) / 10
    )

    return {
      without,
      together,
      impact: {
        netRtg: diff(without.netRtg, together.netRtg)
      },
    }
  }, [currentExclusionData])

  // Player selection handlers
  const addPlayer = (player) => {
    if (excludedPlayer || selectedPlayers.includes(player) || selectedPlayers.length >= 5) return
    setSelectedPlayers([...selectedPlayers, player])
  }

  const excludePlayer = (player) => {
    if (selectedPlayers.length !== 1 || !availableExclusions.includes(player)) return
    setExcludedPlayer(player)
  }

  const removePlayer = (player) => {
    setSelectedPlayers(selectedPlayers.filter(p => p !== player))
  }

  const toggleSinglePlayer = (player) => {
    if (selectedPlayers.includes(player)) removePlayer(player)
    else setSelectedPlayers([player])
  }

  const clearPlayers = () => {
    setSelectedPlayers([])
    setExcludedPlayer('')
  }

  // Sort handler
  const handleSort = (key) => {
    setSortConfig(prev => ({
      key,
      direction: prev.key === key && prev.direction === 'desc' ? 'asc' : 'desc'
    }))
  }

  // Performance indicator helper
  const getPerformanceIndicator = (value, threshold = 0, inverse = false, minutes = MIN_ON_OFF_MINUTES) => {
    if (value == null) return { emoji: '➖', label: 'N/A', color: 'text-acb-400' }
    if ((minutes ?? 0) < MIN_ON_OFF_MINUTES) return { emoji: '○', label: 'Muestra reducida', color: 'text-acb-400' }

    const adjusted = inverse ? -value : value
    if (adjusted > threshold + 5) return { emoji: '🔥', label: 'Elite', color: 'text-positive' }
    if (adjusted > threshold + 2) return { emoji: '✅', label: 'Bueno', color: 'text-positive' }
    if (adjusted > threshold - 2) return { emoji: '➖', label: 'Normal', color: 'text-acb-500' }
    if (adjusted > threshold - 5) return { emoji: '⚠️', label: 'Debajo de la media', color: 'text-acb-500' }
    return { emoji: '🔻', label: 'Bajo', color: 'text-negative' }
  }

  // Rating color helper
  const getRatingColor = (value, isDefensive = false) => {
    if (value == null) return 'text-acb-400'
    const threshold = isDefensive ? 105 : 110
    const good = isDefensive ? value < threshold : value > threshold
    const great = isDefensive ? value < threshold - 5 : value > threshold + 5

    if (great) return 'text-positive'
    if (good) return 'text-positive'
    return 'text-negative'
  }

  if (loading) {
    return (
      <div className="app-page space-y-6">
          <PageHeader title="Análisis de alineaciones" subtitle="Cargando datos de combinaciones..." />
        <div className="bg-acb-50 rounded-lg p-8 text-center">
          <div className="animate-pulse">
            <div className="text-acb-600">Cargando alineaciones...</div>
          </div>
        </div>
      </div>
    )
  }

  return (
    <div className="app-page space-y-6">
      <PageHeader
        title="Análisis de alineaciones"
        subtitle="Analiza el impacto on/off de jugadores y el rendimiento de combinaciones"
        scope="Temporada completa · Liga regular y playoffs"
      />

      {/* Controls */}
      <div className="filter-panel block space-y-4">
        <div className="flex flex-wrap items-center gap-4">
          {/* Season Filter */}
          <div className="flex items-center gap-2">
            <span className="field-label">Temporada</span>
            <select
              aria-label="Temporada"
              value={selectedSeason}
              onChange={(e) => {
                const s = e.target.value === 'all' ? 'all' : parseInt(e.target.value)
                setSelectedSeason(s)
                clearPlayers()
                navigate('/alineaciones', { replace: true })
              }}
              className="form-control"
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
              aria-label="Equipo"
              value={selectedTeam}
              onChange={(e) => {
                const team = e.target.value
                setSelectedTeam(team)
                clearPlayers()
                if (team) {
                  navigate(`/alineaciones/${selectedSeason}/${toSlug(team)}`, { replace: true })
                }
              }}
              className="form-control min-w-[200px]"
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
              aria-label="Buscar jugadores"
              type="text"
              placeholder="Buscar jugadores..."
              value={searchQuery}
              onChange={(e) => setSearchQuery(e.target.value)}
              disabled={Boolean(excludedPlayer)}
              className="flex-1 px-3 py-2 border border-acb-200 rounded-md text-sm bg-white disabled:bg-acb-50 disabled:text-acb-400 disabled:cursor-not-allowed"
            />
          </div>

          {/* Selected Players Chips */}
          {selectedPlayers.length > 0 && (
            <div className="flex flex-wrap gap-2 items-center">
              <span className="text-sm text-acb-600 font-medium">Analizando:</span>
              {selectedPlayers.map(playerKey => (
                <div key={playerKey} className="flex items-center gap-1 bg-accent-100 text-accent-800 rounded-full px-3 py-1">
                  {getPlayerPhoto(playerPhotos, getIdFromKey(playerKey), selectedSeason) && (
                    <img src={getPlayerPhoto(playerPhotos, getIdFromKey(playerKey), selectedSeason)} alt="" className="w-5 h-5 rounded-full object-cover object-top" />
                  )}
                  <span className="text-sm font-medium">{getPlayerDisplayName(playerKey)}</span>
                  <button
                    type="button"
                    onClick={() => removePlayer(playerKey)}
                    className="hover:text-accent-600"
                    aria-label={`Quitar ${getPlayerDisplayName(playerKey)}`}
                  >
                    <X className="w-3 h-3" />
                  </button>
                </div>
              ))}
              {excludedPlayer && (
                <button
                  type="button"
                  onClick={() => setExcludedPlayer('')}
                  className="inline-flex items-center gap-1.5 rounded-full border border-acb-300 bg-acb-100 px-3 py-1 text-sm font-medium text-acb-800 hover:bg-acb-200"
                  aria-label={`Quitar comparación sin ${getPlayerDisplayName(excludedPlayer)}`}
                >
                  Sin {getPlayerDisplayName(excludedPlayer)} <X className="h-3.5 w-3.5" />
                </button>
              )}
              <button onClick={clearPlayers} className="text-sm text-acb-500 hover:text-acb-700">
                Limpiar
              </button>
              {selectedPlayers.length === 1 && !excludedPlayer && availableExclusions.length > 0 && (
                <span className="text-xs text-acb-500">
                  <span className="hidden sm:inline">Pasa sobre otro jugador y elige </span>
                  <span className="sm:hidden">Elige </span>
                  <span className="font-semibold text-acb-700">Con</span> o <span className="font-semibold text-acb-700">Sin</span>.
                </span>
              )}
            </div>
          )}

          {/* Player Grid */}
          <div className="max-h-48 overflow-y-auto border border-acb-100 rounded-md bg-acb-50/50">
            {filteredPlayers.length > 0 ? (
              <div className="grid grid-cols-2 sm:grid-cols-3 md:grid-cols-4 lg:grid-cols-5 items-start gap-1 p-2">
                {filteredPlayers.map(playerKey => {
                  const displayName = getPlayerDisplayName(playerKey)
                  const photo = getPlayerPhoto(playerPhotos, getIdFromKey(playerKey), selectedSeason)
                  const isSelected = selectedPlayers.includes(playerKey)
                  const isExcluded = excludedPlayer === playerKey
                  const selectionClosed = Boolean(excludedPlayer) || selectedPlayers.length >= 5
                  const showRelationActions = selectedPlayers.length > 0 && !isSelected && !isExcluded && !selectionClosed
                  const canExclude = selectedPlayers.length === 1 && availableExclusions.includes(playerKey)
                  const identity = (
                    <>
                      {photo && <img src={photo} alt="" className="h-5 w-5 shrink-0 rounded-full object-cover object-top" />}
                      <span className="min-w-0 truncate" title={displayName}>{displayName}</span>
                    </>
                  )

                  return (
                    <div
                      key={playerKey}
                      className={`group relative rounded border text-sm transition-colors ${
                        isSelected
                          ? 'border-accent-200 bg-accent-100 font-medium text-accent-800'
                          : isExcluded
                            ? 'border-acb-300 bg-acb-200 font-medium text-acb-900'
                            : selectionClosed
                              ? 'border-acb-100 bg-acb-100 text-acb-400'
                              : 'border-acb-200 bg-white text-acb-700 hover:border-accent-200'
                      }`}
                    >
                      {selectedPlayers.length === 0 ? (
                        <button
                          type="button"
                          onClick={() => addPlayer(playerKey)}
                          className="flex min-h-8 w-full items-center gap-1.5 rounded px-2 py-1.5 text-left hover:bg-accent-50 hover:text-accent-700"
                          aria-label={`Seleccionar a ${displayName}`}
                        >
                          {identity}
                        </button>
                      ) : (
                        <>
                          <div
                            className="flex min-h-8 items-center gap-1.5 px-2 py-1.5"
                            aria-disabled={!isSelected && !isExcluded && selectionClosed ? 'true' : undefined}
                          >
                            {identity}
                            {isExcluded && <span className="ml-auto text-[10px] font-bold uppercase tracking-wide">Sin</span>}
                          </div>
                          {showRelationActions && (
                            <div className="flex items-center justify-end gap-1 border-t border-acb-100 px-1.5 py-1 sm:absolute sm:inset-y-0 sm:right-1 sm:justify-start sm:border-0 sm:bg-white/95 sm:pl-2 sm:opacity-0 sm:transition-opacity sm:group-hover:opacity-100 sm:group-focus-within:opacity-100">
                              <button
                                type="button"
                                onClick={() => addPlayer(playerKey)}
                                className="rounded border border-accent-200 bg-accent-50 px-2 py-1 text-xs font-semibold text-accent-800 hover:bg-accent-100 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-1 focus-visible:outline-accent-500"
                                aria-label={`Analizar con ${displayName}`}
                              >
                                Con
                              </button>
                              {selectedPlayers.length === 1 && (
                                <button
                                  type="button"
                                  onClick={() => excludePlayer(playerKey)}
                                  disabled={!canExclude}
                                  className="rounded border border-acb-300 bg-acb-100 px-2 py-1 text-xs font-semibold text-acb-800 hover:bg-acb-200 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-1 focus-visible:outline-acb-500 disabled:cursor-not-allowed disabled:opacity-35"
                                  aria-label={`Analizar sin ${displayName}`}
                                  title={canExclude ? `Analizar sin ${displayName}` : 'Sin datos suficientes para esta exclusión'}
                                >
                                  Sin
                                </button>
                              )}
                            </div>
                          )}
                        </>
                      )}
                    </div>
                  )
                })}
              </div>
            ) : (
              <div className="p-4 text-center text-acb-400 text-sm">
                No se encontraron jugadores{searchQuery && ` para "${searchQuery}"`}
              </div>
            )}
          </div>
        </div>
      </div>

      {/* Individual Player Analysis Results */}
      {selectedPlayers.length === 1 && currentLineupData && !excludedPlayer && (
        <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
          <div className="bg-gradient-to-r from-acb-700 to-acb-800 px-4 py-3">
            <h3 className="font-semibold text-white text-lg">
              {selectedPlayers.map(getPlayerDisplayName).join(' · ')}
            </h3>
            <p className="text-acb-200 text-sm">
              {currentLineupData.onMin?.toFixed(1)} min en cancha • {currentLineupData.offMin?.toFixed(1)} min fuera
            </p>
          </div>

          {/* Stats Table */}
          <div className="overflow-x-auto">
            <table className="data-table table-fixed">
              <colgroup>
                <col className="w-[27%]" />
                <col className="w-[18.25%]" />
                <col className="w-[18.25%]" />
                <col className="w-[18.25%]" />
                <col className="w-[18.25%]" />
              </colgroup>
              <thead>
                <tr className="bg-acb-50 text-left text-xs text-acb-600 uppercase tracking-wider">
                  <th className="data-table-head text-left">Métrica</th>
                  <th className="data-table-head text-center" title="Rendimiento con la selección en pista">
                    <span className="sm:hidden">On</span><span className="hidden sm:inline">En cancha</span>
                  </th>
                  <th className="data-table-head text-center" title="Rendimiento con la selección fuera de pista">
                    <span className="sm:hidden">Off</span><span className="hidden sm:inline">Fuera de cancha</span>
                  </th>
                  <th className="data-table-head text-center" title="En cancha menos fuera de cancha">
                    <span className="sm:hidden">Diff</span><span className="hidden sm:inline">Diff On−Off</span>
                  </th>
                  <th className="data-table-head text-center">Impacto</th>
                </tr>
              </thead>
              <tbody className="divide-y divide-acb-100">
                {/* ATAQUE */}
                <tr className="bg-acb-50">
                  <td colSpan={5} className="data-table-group text-left">Ataque</td>
                </tr>
                <StatRow
                  label="Ef. Ofensiva"
                  onValue={currentLineupData.onORtg}
                  offValue={currentLineupData.offORtg}
                  goodThreshold={110}
                />
                <StatRow
                  label="eFG%"
                  onValue={currentLineupData.onEFG}
                  offValue={currentLineupData.offEFG}
                  goodThreshold={50}
                  diffUnit=" pp"
                />
                <StatRow
                  label="PER%"
                  onValue={currentLineupData.onTOV}
                  offValue={currentLineupData.offTOV}
                  goodThreshold={15}
                  inverse
                  diffUnit=" pp"
                />
                <StatRow
                  label="RO%"
                  onValue={currentLineupData.onORB}
                  offValue={currentLineupData.offORB}
                  goodThreshold={30}
                  diffUnit=" pp"
                />
                <StatRow
                  label="AST%"
                  onValue={currentLineupData.onAST}
                  offValue={currentLineupData.offAST}
                  goodThreshold={50}
                  diffUnit=" pp"
                />
                {/* DEFENSA */}
                <tr className="bg-acb-50">
                  <td colSpan={5} className="data-table-group text-left" title="Estadísticas permitidas al rival">Defensa</td>
                </tr>
                <StatRow
                  label="Ef. Defensiva"
                  onValue={currentLineupData.onDRtg}
                  offValue={currentLineupData.offDRtg}
                  goodThreshold={105}
                  inverse
                />
                <StatRow
                  label="eFG%"
                  onValue={currentLineupData.onOppEFG}
                  offValue={currentLineupData.offOppEFG}
                  goodThreshold={50}
                  inverse
                  diffUnit=" pp"
                />
                <StatRow
                  label="PER%"
                  onValue={currentLineupData.onOppTOV}
                  offValue={currentLineupData.offOppTOV}
                  goodThreshold={14}
                  diffUnit=" pp"
                />
                <StatRow
                  label="RD%"
                  onValue={currentLineupData.onDRB}
                  offValue={currentLineupData.offDRB}
                  goodThreshold={70}
                  diffUnit=" pp"
                />
                {/* BALANCE */}
                <tr className="bg-acb-50">
                  <td colSpan={5} className="data-table-group text-left">Balance</td>
                </tr>
                <StatRow
                  label="Ef. Neta"
                  onValue={currentLineupData.onNetRtg}
                  offValue={currentLineupData.offNetRtg}
                  goodThreshold={0}
                  highlight
                />
              </tbody>
            </table>
          </div>

          {/* Impact Summary */}
          <div className="p-4 bg-acb-50 border-t border-acb-200">
            <div className="flex items-center justify-center gap-4">
              <div className="text-center">
                <div className="text-3xl mb-1">
                  {getPerformanceIndicator(currentLineupData.netDiff, 0, false, currentLineupData.onMin).emoji}
                </div>
                <div className={`text-2xl font-bold font-mono ${
                  currentLineupData.netDiff > 0 ? 'text-positive' :
                  currentLineupData.netDiff < 0 ? 'text-negative' : 'text-acb-500'
                }`}>
                  {currentLineupData.netDiff > 0 ? '+' : ''}{currentLineupData.netDiff?.toFixed(1)}
                </div>
                <div className="text-sm text-acb-600 mt-1">Diff Neto On−Off</div>
              </div>
            </div>
          </div>
        </div>
      )}

      {/* pair, trio, lineup, and exclusion analysis results */}
      {(() => {
        const isExclusion = Boolean(selectedPlayers.length === 1 && excludedPlayer && exclusionAnalysisData)
        if (isExclusion) {
          return (
            <ExclusionComparisonCard
              data={exclusionAnalysisData}
              focalName={getPlayerDisplayName(selectedPlayers[0])}
              excludedName={getPlayerDisplayName(excludedPlayer)}
              onClear={() => setExcludedPlayer('')}
            />
          )
        }

        const analysisData = selectedPlayers.length > 1 ? currentLineupData : null

        if (!analysisData) return null

        return (
          <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
          <div className="bg-gradient-to-r from-acb-700 to-acb-800 px-4 py-3">
            <h3 className="font-semibold text-white text-lg">
              {`Análisis de ${selectedPlayers.length === 2 ? 'Dúo' : selectedPlayers.length === 3 ? 'Trío' : 'Quinteto'}`}
            </h3>
            <p className="text-acb-200 text-sm">
              {selectedPlayers.map(k => getPlayerDisplayName(k)).join(' + ')} • {analysisData.onMin?.toFixed(1)} min juntos
              {analysisData.offMin != null && ` • ${analysisData.offMin?.toFixed(1)} min separados`}
            </p>
          </div>

          {/* main ratings */}
          <div className="grid grid-cols-4 divide-x divide-acb-200">
            <div className="p-4 text-center">
              <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">Ef. Ofensiva</div>
              <div className={`text-2xl font-bold font-mono ${getRatingColor(analysisData.onORtg)}`}>
                {analysisData.onORtg?.toFixed(1)}
              </div>
            </div>
            <div className="p-4 text-center">
              <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">Ef. Defensiva</div>
              <div className={`text-2xl font-bold font-mono ${getRatingColor(analysisData.onDRtg, true)}`}>
                {analysisData.onDRtg?.toFixed(1)}
              </div>
            </div>
            <div className="p-4 text-center">
              <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">Ef. Neta</div>
              <div className={`text-2xl font-bold font-mono ${
                analysisData.onNetRtg > 0 ? 'text-positive' :
                analysisData.onNetRtg < 0 ? 'text-negative' : 'text-acb-500'
              }`}>
                {analysisData.onNetRtg > 0 ? '+' : ''}{analysisData.onNetRtg?.toFixed(1)}
              </div>
              <div className="text-lg mt-1">
                {getPerformanceIndicator(analysisData.onNetRtg, 0, false, analysisData.onMin).emoji}
              </div>
            </div>
            {analysisData.netDiff != null ? (
              <div className="p-4 text-center bg-acb-50">
                <div
                  className="text-xs text-acb-500 uppercase tracking-wider mb-1"
                >
                  Impacto
                </div>
                <div className={`text-2xl font-bold font-mono ${
                  analysisData.netDiff > 0 ? 'text-positive' :
                  analysisData.netDiff < 0 ? 'text-negative' : 'text-acb-500'
                }`}>
                  {(analysisData.netDiff > 0 ? '+' : '') + analysisData.netDiff?.toFixed(1)}
                </div>
                <div className="text-lg mt-1">
                  {getPerformanceIndicator(analysisData.netDiff, 0, false, analysisData.onMin).emoji}
                </div>
              </div>
            ) : (
              <div className="p-4 text-center bg-acb-50">
                <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">Posesiones</div>
                <div className="text-2xl font-bold font-mono text-acb-700">
                  {analysisData.onPoss}
                </div>
              </div>
            )}
          </div>

          {/* ataque */}
          {(() => {
            const stats = [
              { label: 'eFG%',  val: analysisData.onEFG,  fmt: v => `${v.toFixed(1)}%`, color: 'text-acb-700' },
              { label: 'PER%',  val: analysisData.onTOV,  fmt: v => `${v.toFixed(1)}%`, color: 'text-acb-700' },
              { label: 'RO%',   val: analysisData.onORB,  fmt: v => `${v.toFixed(1)}%`, color: 'text-acb-700' },
              { label: 'AST%',  val: analysisData.onAST,  fmt: v => `${v.toFixed(1)}%`, color: 'text-acb-700' },
            ].filter(s => s.val != null)
            if (stats.length === 0) return null
            return (
              <div className="border-t border-acb-200">
                <div className="px-4 py-2 bg-acb-50 text-xs font-bold text-acb-600 uppercase tracking-wider">Ataque</div>
                <div className="grid divide-x divide-acb-200 bg-acb-50" style={{ gridTemplateColumns: `repeat(${stats.length}, 1fr)` }}>
                  {stats.map(s => (
                    <div key={s.label} className="p-3 text-center">
                      <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">{s.label}</div>
                      <div className={`text-lg font-semibold font-mono ${s.color}`}>{s.fmt(s.val)}</div>
                    </div>
                  ))}
                </div>
              </div>
            )
          })()}

          {/* defensa */}
          {(() => {
            const stats = [
              { label: 'DRtg', val: analysisData.onDRtg, fmt: v => v.toFixed(1), color: analysisData.onDRtg < 105 ? 'text-positive' : 'text-negative' },
              { label: 'eFG%', val: analysisData.onOppEFG, fmt: v => `${v.toFixed(1)}%`, color: analysisData.onOppEFG < 50 ? 'text-positive' : 'text-negative' },
              { label: 'PER%', val: analysisData.onOppTOV, fmt: v => `${v.toFixed(1)}%`, color: analysisData.onOppTOV > 14 ? 'text-positive' : 'text-negative' },
              { label: 'RD%',  val: analysisData.onDRB, fmt: v => `${v.toFixed(1)}%`, color: analysisData.onDRB > 70 ? 'text-positive' : 'text-negative' },
            ].filter(s => s.val != null)
            if (stats.length === 0) return null
            return (
              <div className="border-t border-acb-200">
                <div className="px-4 py-2 bg-acb-50 text-xs font-bold text-acb-600 uppercase tracking-wider">Defensa</div>
                <div className="grid divide-x divide-acb-200 bg-acb-50" style={{ gridTemplateColumns: `repeat(${stats.length}, 1fr)` }}>
                  {stats.map(s => (
                    <div key={s.label} className="p-3 text-center">
                      <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">{s.label}</div>
                      <div className={`text-lg font-semibold font-mono ${s.color}`}>{s.fmt(s.val)}</div>
                    </div>
                  ))}
                </div>
              </div>
            )
          })()}
          </div>
        )
      })()}

      {/* No Data Found */}
      {selectedPlayers.length > 0 && !currentLineupData && !loading && (
        <div className="bg-acb-50 border border-acb-200 rounded-lg p-4 flex gap-3">
          <span className="text-xl">⚠️</span>
          <div>
            <p className="font-medium text-acb-800">Sin datos disponibles</p>
            <p className="text-sm text-acb-700">
              {selectedPlayers.length === 4
                ? "Los datos para combinaciones de 4 jugadores no están calculados. Selecciona 1, 2, 3 o 5 jugadores."
                : selectedPlayers.length === 5
                  ? "Esta alineación de 5 puede no haber jugado suficientes minutos juntos."
                  : selectedPlayers.length > 2
                    ? "Esta combinación de jugadores puede no haber jugado suficientes minutos juntos."
                    : "Esta combinación de jugadores puede no tener suficiente tamaño de muestra."}
            </p>
          </div>
        </div>
      )}

      {/* Team Overview Table */}
      {allPlayersData.length > 0 && (
        <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
          <div className="px-4 py-3 border-b border-acb-200 flex items-center justify-between gap-3">
            <div>
              <h3 className="font-semibold text-acb-900">Resumen On/Off del equipo</h3>
              <p className="text-xs text-acb-400">Ranking con un mínimo de {MIN_ON_OFF_MINUTES} minutos en pista</p>
            </div>
            <button
              onClick={() => setShowAllPlayers(!showAllPlayers)}
              className="text-sm text-acb-600 hover:text-acb-800 flex items-center gap-1"
            >
              {showAllPlayers ? 'Mostrar menos' : 'Mostrar todos'}
              {showAllPlayers ? <ChevronUp className="w-4 h-4" /> : <ChevronDown className="w-4 h-4" />}
            </button>
          </div>

          <div className="overflow-x-auto">
            <table className="data-table">
              <thead>
                <tr className="text-xs uppercase tracking-wider border-b border-acb-200">
                  <th className="data-table-head data-table-identity data-table-sticky data-table-sticky-head data-col-player bg-acb-50" rowSpan={2}>Jugador</th>
                  <th colSpan={2} className="data-table-group bg-acb-50">Rating</th>
                  <th colSpan={2} className="data-table-group bg-acb-50">Neto</th>
                  <th
                    className="data-table-head data-table-number data-col-number bg-accent-50 text-accent-700 cursor-pointer hover:bg-acb-100 transition-colors"
                    rowSpan={2}
                    aria-sort={sortConfig.key === 'netDiff' ? (sortConfig.direction === 'desc' ? 'descending' : 'ascending') : 'none'}
                  >
                    <button type="button" className="w-full flex items-center justify-end gap-1" onClick={() => handleSort('netDiff')}>
                      Impacto
                      {sortConfig.key === 'netDiff' && (
                        sortConfig.direction === 'desc'
                          ? <ChevronDown className="w-3 h-3" />
                          : <ChevronUp className="w-3 h-3" />
                      )}
                    </button>
                  </th>
                  <th colSpan={4} className="data-table-group bg-acb-50">Ataque</th>
                  <th colSpan={3} className="data-table-group bg-acb-50">Defensa</th>
                  <th className="data-table-head data-table-number data-col-games bg-acb-50" rowSpan={2}>Min</th>
                </tr>
                <tr className="text-xs text-acb-600 uppercase tracking-wider">
                  <SortableHeader label="ORtg" title="Diff ORtg On−Off" sortKey="deltaORtg" current={sortConfig} onSort={handleSort} />
                  <SortableHeader label="DRtg" title="Diff DRtg On−Off" sortKey="deltaDRtg" current={sortConfig} onSort={handleSort} />
                  <SortableHeader label="On" sortKey="onNetRtg" current={sortConfig} onSort={handleSort} />
                  <SortableHeader label="Off" sortKey="offNetRtg" current={sortConfig} onSort={handleSort} />
                  <SortableHeader label="eFG" title="Diff eFG% On−Off (pp)" sortKey="deltaEFG" current={sortConfig} onSort={handleSort} thClassName="bg-acb-50" />
                  <SortableHeader label="PER" title="Diff PER% On−Off (pp)" sortKey="deltaTOV" current={sortConfig} onSort={handleSort} thClassName="bg-acb-50" />
                  <SortableHeader label="RO" title="Diff RO% On−Off (pp)" sortKey="deltaORB" current={sortConfig} onSort={handleSort} thClassName="bg-acb-50" />
                  <SortableHeader label="AST" title="Diff AST% On−Off (pp)" sortKey="deltaAST" current={sortConfig} onSort={handleSort} thClassName="bg-acb-50" />
                  <SortableHeader label="eFG" title="Diff eFG% rival On−Off (pp)" sortKey="deltaOppEFG" current={sortConfig} onSort={handleSort} thClassName="bg-acb-50" />
                  <SortableHeader label="PER" title="Diff PER% rival On−Off (pp)" sortKey="deltaOppTOV" current={sortConfig} onSort={handleSort} thClassName="bg-acb-50" />
                  <SortableHeader label="RD" title="Diff RD% On−Off (pp)" sortKey="deltaDRB" current={sortConfig} onSort={handleSort} thClassName="bg-acb-50" />
                </tr>
              </thead>
              <tbody className="divide-y divide-acb-100">
                {(showAllPlayers ? rankedPlayersData : rankedPlayersData.slice(0, 8)).map((player) => (
                  <tr
                    key={player.key}
                    className={`data-table-row cursor-pointer ${
                      selectedPlayers.includes(player.key) ? 'bg-accent-50' : ''
                    }`}
                    onClick={() => toggleSinglePlayer(player.key)}
                  >
                    <td className="data-table-cell data-table-identity data-table-sticky data-col-player">
                      <button
                        type="button"
                        className="flex items-center gap-2 text-left"
                        onClick={(event) => {
                          event.stopPropagation()
                          toggleSinglePlayer(player.key)
                        }}
                      >
                        {getPlayerPhoto(playerPhotos, player.id, selectedSeason) && (
                          <img src={getPlayerPhoto(playerPhotos, player.id, selectedSeason)} alt="" className="w-6 h-6 rounded-full object-cover object-top" />
                        )}
                        {player.name}
                      </button>
                    </td>
                    {(() => {
                      const ortgD = (player.onORtg ?? 0) - (player.offORtg ?? 0)
                      return (
                        <td className={`data-table-cell data-table-number data-col-number ${ortgD > 0 ? 'text-positive' : ortgD < 0 ? 'text-negative' : 'text-acb-500'}`}>
                          {ortgD > 0 ? '+' : ''}{ortgD.toFixed(1)}
                        </td>
                      )
                    })()}
                    {(() => {
                      const drtgD = (player.onDRtg ?? 0) - (player.offDRtg ?? 0)
                      return (
                        <td className={`data-table-cell data-table-number data-col-number ${drtgD < 0 ? 'text-positive' : drtgD > 0 ? 'text-negative' : 'text-acb-500'}`}>
                          {drtgD > 0 ? '+' : ''}{drtgD.toFixed(1)}
                        </td>
                      )
                    })()}
                    <td className={`data-table-cell data-table-number data-col-number ${
                      player.onNetRtg > 0 ? 'text-positive' : 'text-negative'
                    }`}>
                      {player.onNetRtg > 0 ? '+' : ''}{player.onNetRtg?.toFixed(1)}
                    </td>
                    <td className="data-table-cell data-table-number data-col-number text-acb-500">
                      {player.offNetRtg > 0 ? '+' : ''}{player.offNetRtg?.toFixed(1)}
                    </td>
                    <td className={`data-table-cell data-table-number data-col-number font-semibold ${
                      player.netDiff > 2 ? 'text-positive' :
                      player.netDiff < -2 ? 'text-negative' : 'text-acb-500'
                    }`}>
                      <span className="mr-1">{getPerformanceIndicator(player.netDiff, 0, false, player.onMin).emoji}</span>
                      {player.netDiff > 0 ? '+' : ''}{player.netDiff?.toFixed(1)}
                    </td>
                    {(() => {
                      const d = (player.onEFG ?? 0) - (player.offEFG ?? 0)
                      return <td className={`data-table-cell data-table-number data-col-number ${d > 0 ? 'text-positive' : d < 0 ? 'text-negative' : 'text-acb-500'}`}>{d > 0 ? '+' : ''}{d.toFixed(1)}</td>
                    })()}
                    {(() => {
                      const d = (player.onTOV ?? 0) - (player.offTOV ?? 0)
                      return <td className={`data-table-cell data-table-number data-col-number ${d < 0 ? 'text-positive' : d > 0 ? 'text-negative' : 'text-acb-500'}`}>{d > 0 ? '+' : ''}{d.toFixed(1)}</td>
                    })()}
                    {(() => {
                      const d = (player.onORB ?? 0) - (player.offORB ?? 0)
                      return <td className={`data-table-cell data-table-number data-col-number ${d > 0 ? 'text-positive' : d < 0 ? 'text-negative' : 'text-acb-500'}`}>{d > 0 ? '+' : ''}{d.toFixed(1)}</td>
                    })()}
                    {(() => {
                      const d = (player.onAST ?? 0) - (player.offAST ?? 0)
                      return <td className={`data-table-cell data-table-number data-col-number ${d > 0 ? 'text-positive' : d < 0 ? 'text-negative' : 'text-acb-500'}`}>{d > 0 ? '+' : ''}{d.toFixed(1)}</td>
                    })()}
                    {(() => {
                      const d = (player.onOppEFG ?? 0) - (player.offOppEFG ?? 0)
                      return <td className={`data-table-cell data-table-number data-col-number ${d < 0 ? 'text-positive' : d > 0 ? 'text-negative' : 'text-acb-500'}`}>{d > 0 ? '+' : ''}{d.toFixed(1)}</td>
                    })()}
                    {(() => {
                      const d = (player.onOppTOV ?? 0) - (player.offOppTOV ?? 0)
                      return <td className={`data-table-cell data-table-number data-col-number ${d > 0 ? 'text-positive' : d < 0 ? 'text-negative' : 'text-acb-500'}`}>{d > 0 ? '+' : ''}{d.toFixed(1)}</td>
                    })()}
                    {(() => {
                      const d = (player.onDRB ?? 0) - (player.offDRB ?? 0)
                      return <td className={`data-table-cell data-table-number data-col-number ${d > 0 ? 'text-positive' : d < 0 ? 'text-negative' : 'text-acb-500'}`}>{d > 0 ? '+' : ''}{d.toFixed(1)}</td>
                    })()}
                    <td className="data-table-cell data-table-number data-col-games text-xs text-acb-400">
                      {player.onMin?.toFixed(0)}
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
        <span className="flex items-center gap-1">🔥 Élite</span>
        <span className="flex items-center gap-1">✅ Bueno</span>
        <span className="flex items-center gap-1">➖ Medio</span>
        <span className="flex items-center gap-1">⚠️ Debajo de la media</span>
        <span className="flex items-center gap-1">🔻 Bajo</span>
        <span className="flex items-center gap-1">○ Muestra reducida</span>
      </div>

      {/* On/Off Explanation */}
      <div className="bg-acb-50 rounded-lg border border-acb-200 p-4">
        <h3 className="text-sm font-semibold text-acb-900 mb-3 flex items-center gap-2">
          <Info className="w-4 h-4" />
          Cómo interpretar las estadísticas On/Off
        </h3>
        <div className="text-sm text-acb-600 space-y-3">
          <p>
            El análisis <strong>On/Off</strong> describe cómo rinde el equipo cuando un jugador está en pista frente a sus minutos fuera.
          </p>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4 mt-2">
            <div className="bg-white p-3 rounded border border-acb-100">
              <div className="font-medium text-acb-900 mb-1">En cancha</div>
              <p className="text-xs">Estadísticas del equipo durante los minutos que el jugador o combinación seleccionada está jugando.</p>
            </div>
            <div className="bg-white p-3 rounded border border-acb-100">
              <div className="font-medium text-acb-900 mb-1">Fuera de cancha</div>
              <p className="text-xs">Estadísticas del equipo durante los minutos que el jugador o combinación no está en pista.</p>
            </div>
            <div className="bg-white p-3 rounded border border-acb-100">
              <div className="font-medium text-acb-900 mb-1">Impacto (Diff)</div>
              <p className="text-xs">La diferencia observada entre On y Off. No aísla el efecto individual del jugador.</p>
            </div>
          </div>
          <ul className="list-disc list-inside space-y-1 mt-2 text-xs">
            <li><strong>Eficiencia Ofensiva (ORtg):</strong> Puntos anotados por 100 posesiones. <span className="text-positive">Mayor es mejor</span>. Un Diff positivo significa que el ataque mejora con el jugador.</li>
            <li><strong>Eficiencia Defensiva (DRtg):</strong> Puntos recibidos por 100 posesiones. <span className="text-positive">Menor es mejor</span>. Un Diff negativo significa que la defensa mejora con el jugador.</li>
            <li><strong>Eficiencia Neta (NetRtg):</strong> Diferencia entre ORtg y DRtg. Muestra el margen de victoria por 100 posesiones.</li>
            <li><strong>Impacto:</strong> Diferencia descriptiva entre el Net Rating con el jugador dentro y fuera. Depende también de compañeros, rivales, contexto y tamaño de muestra.</li>
          </ul>
        </div>
      </div>
    </div>
  )
}

const formatDecimal = (value, unit = '') => {
  if (!hasNumber(value)) return '—'
  return `${Number(value).toFixed(1)}${unit}`
}

const formatSignedDecimal = (value, unit = '') => {
  if (!hasNumber(value)) return '—'
  const numericValue = Number(value)
  return `${numericValue > 0 ? '+' : ''}${numericValue.toFixed(1)}${unit}`
}

const comparisonDeltaClass = (value, inverse = false) => {
  if (!hasNumber(value)) return 'text-acb-400'
  const numericValue = Number(value)
  if (numericValue === 0) return 'text-acb-500'
  const isGood = inverse ? numericValue < 0 : numericValue > 0
  return isGood ? 'text-positive' : 'text-negative'
}

const ExclusionComparisonCard = ({ data, focalName, excludedName, onClear }) => {
  const attackStats = [
    { label: 'eFG%', value: data.without.eFG, unit: '%' },
    { label: 'PER%', value: data.without.TOV, unit: '%' },
    { label: 'AST%', value: data.without.AST, unit: '%' },
  ].filter(stat => hasNumber(stat.value))

  const defenseStats = [
    { label: 'DRtg', value: data.without.DRtg },
    { label: 'eFG%', value: data.without.oppEFG, unit: '%' },
    { label: 'RD%', value: data.without.DRB, unit: '%' },
  ].filter(stat => hasNumber(stat.value))

  return (
    <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
      <div className="bg-gradient-to-r from-acb-700 to-acb-800 px-4 py-3">
        <h3 className="font-semibold text-white text-lg">Rendimiento de {focalName} sin {excludedName}</h3>
        <p className="text-acb-200 text-sm">
          {focalName} sin {excludedName} • {formatDecimal(data.without.min)} min sin
          {hasNumber(data.together.min) && ` • ${formatDecimal(data.together.min)} min juntos`}
        </p>
      </div>

      <div className="grid grid-cols-2 sm:grid-cols-4 divide-x divide-y sm:divide-y-0 divide-acb-200">
        <div className="p-4 text-center">
          <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">Ef. Ofensiva</div>
          <div className="text-2xl font-bold font-mono text-acb-800">
            {formatDecimal(data.without.ORtg)}
          </div>
        </div>
        <div className="p-4 text-center">
          <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">Ef. Defensiva</div>
          <div className="text-2xl font-bold font-mono text-acb-800">
            {formatDecimal(data.without.DRtg)}
          </div>
        </div>
        <div className="p-4 text-center">
          <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">Ef. Neta</div>
          <div className="text-2xl font-bold font-mono text-acb-800">
            {formatSignedDecimal(data.without.netRtg)}
          </div>
        </div>
        <div className="p-4 text-center bg-acb-50">
          <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">Impacto</div>
          <div className={`text-2xl font-bold font-mono ${comparisonDeltaClass(data.impact.netRtg)}`}>
            {formatSignedDecimal(data.impact.netRtg)}
          </div>
        </div>
      </div>

      <div className="border-t border-acb-200 bg-acb-50 px-4 py-3">
        <div className="grid grid-cols-1 sm:grid-cols-3 gap-3 text-center">
          <div>
            <div className="text-xs text-acb-500 uppercase tracking-wider">Sin {excludedName}</div>
            <div className="font-mono font-semibold text-acb-700">
              {formatSignedDecimal(data.without.netRtg)}
            </div>
          </div>
          <div>
            <div className="text-xs text-acb-500 uppercase tracking-wider">Juntos</div>
            <div className="font-mono font-semibold text-acb-700">
              {formatSignedDecimal(data.together.netRtg)}
            </div>
          </div>
          <div>
            <div className="text-xs text-acb-500 uppercase tracking-wider">Diff Sin−Juntos</div>
            <div className={`font-mono font-semibold ${comparisonDeltaClass(data.impact.netRtg)}`}>
              {formatSignedDecimal(data.impact.netRtg)}
            </div>
          </div>
        </div>
      </div>

      {attackStats.length > 0 && (
        <div className="border-t border-acb-200">
          <div className="px-4 py-2 bg-acb-50 text-xs font-bold text-acb-600 uppercase tracking-wider">Ataque</div>
          <div className="grid divide-x divide-acb-200 bg-acb-50" style={{ gridTemplateColumns: `repeat(${attackStats.length}, 1fr)` }}>
            {attackStats.map(stat => (
              <div key={stat.label} className="p-3 text-center">
                <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">{stat.label}</div>
                <div className="text-lg font-semibold font-mono text-acb-700">{formatDecimal(stat.value, stat.unit)}</div>
              </div>
            ))}
          </div>
        </div>
      )}

      {defenseStats.length > 0 && (
        <div className="border-t border-acb-200">
          <div className="px-4 py-2 bg-acb-50 text-xs font-bold text-acb-600 uppercase tracking-wider">Defensa</div>
          <div className="grid divide-x divide-acb-200 bg-acb-50" style={{ gridTemplateColumns: `repeat(${defenseStats.length}, 1fr)` }}>
            {defenseStats.map(stat => (
              <div key={stat.label} className="p-3 text-center">
                <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">{stat.label}</div>
                <div className="text-lg font-semibold font-mono text-acb-700">
                  {formatDecimal(stat.value, stat.unit)}
                </div>
              </div>
            ))}
          </div>
        </div>
      )}

      <div className="border-t border-acb-200 p-3 text-center">
        <button type="button" onClick={onClear} className="text-sm font-medium text-accent-700 hover:text-accent-800">
          Volver al On/Off general
        </button>
      </div>
    </div>
  )
}

// Stat Row Component for the individual player table
const StatRow = ({ label, onValue, offValue, inverse = false, highlight = false }) => {
  const hasValues = hasNumber(onValue) && hasNumber(offValue)
  const diff = hasValues ? Number(onValue) - Number(offValue) : null

  // For inverse stats (TOV%, DRtg), negative diff is good (player reduces the bad stat)
  // For normal stats (eFG%, ORtg), positive diff is good (player increases the good stat)
  const isGood = hasValues && (inverse ? diff < 0 : diff > 0)

  const getIndicator = (val, threshold, inv) => {
    const adjusted = inv ? threshold - val : val - threshold
    if (adjusted > 5) return { emoji: '🔥', color: 'text-positive' }
    if (adjusted > 2) return { emoji: '✅', color: 'text-positive' }
    if (adjusted > -2) return { emoji: '➖', color: 'text-acb-500' }
    if (adjusted > -5) return { emoji: '⚠️', color: 'text-acb-500' }
    return { emoji: '🔻', color: 'text-negative' }
  }

  const indicator = hasValues ? getIndicator(diff, 0, inverse) : null

  return (
    <tr className={`data-table-row ${highlight ? 'bg-acb-50' : ''}`}>
      <td className={`data-table-cell ${highlight ? 'font-semibold' : ''}`}>
        {label}
      </td>
      <td className="data-table-cell text-center font-mono tabular-nums">
        <span className="font-medium text-acb-700">{hasNumber(onValue) ? Number(onValue).toFixed(1) : '—'}</span>
      </td>
      <td className="data-table-cell text-center font-mono tabular-nums text-acb-500">
        {hasNumber(offValue) ? Number(offValue).toFixed(1) : '—'}
      </td>
      <td className="data-table-cell text-center font-mono tabular-nums">
        <span className={`font-medium ${!hasValues ? 'text-acb-400' : isGood ? 'text-positive' : diff < 0 || diff > 0 ? 'text-negative' : 'text-acb-500'}`}>
          {hasValues ? `${diff > 0 ? '+' : ''}${diff.toFixed(1)}` : '—'}
        </span>
      </td>
      <td className="data-table-cell text-center text-base">
        {indicator?.emoji || '—'}
      </td>
    </tr>
  )
}

// Sortable Header Component
const SortableHeader = ({ label, title, sortKey, current, onSort, highlight = false, thClassName = '' }) => {
  const isActive = current.key === sortKey

  return (
    <th
      className={`data-table-head data-table-number data-col-number hover:bg-acb-100 transition-colors ${
        highlight ? 'bg-accent-50' : ''
      } ${isActive ? 'text-accent-600' : ''} ${thClassName}`}
      aria-sort={isActive ? (current.direction === 'desc' ? 'descending' : 'ascending') : 'none'}
      title={title}
    >
      <button
        type="button"
        className="w-full flex items-center justify-end gap-1"
        onClick={() => onSort(sortKey)}
        aria-label={title ? `${title}. Ordenar` : undefined}
      >
        {label}
        {isActive && (
          current.direction === 'desc'
            ? <ChevronDown className="w-3 h-3" />
            : <ChevronUp className="w-3 h-3" />
        )}
      </button>
    </th>
  )
}
