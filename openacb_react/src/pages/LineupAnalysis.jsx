import React, { useState, useMemo, useEffect } from 'react'
import { useParams, useNavigate } from 'react-router-dom'
import { getPlayerPhoto } from '../utils/playerPhotos'
import { Users, Info, Plus, X, Search, ChevronDown, ChevronUp } from 'lucide-react'

/**
 * Lineup Analysis Page - Cleaning the Glass Style
 *
 * Allows users to select specific player combinations and see their on/off court impact.
 * Uses pre-calculated data from R for instant performance.
 */

// Extract licenseId from player key format "Name_12345"
const getIdFromKey = (key) => key?.split('_').pop() || ''

// Convert team name to URL-friendly slug
function toSlug(name) {
  return name
    .normalize('NFD').replace(/[\u0300-\u036f]/g, '')
    .toLowerCase()
    .replace(/\s+/g, '-')
    .replace(/[^a-z0-9-]/g, '')
}

export default function LineupAnalysis({ teams, loadLineupsForSeason, lineupsCache, loadingLineups, playerPhotos = {} }) {
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
    if (urlTeamSlug && seasonTeamsForSlug.length > 0) {
      const match = seasonTeamsForSlug.find(t => toSlug(t.team) === urlTeamSlug)
      if (match) setSelectedTeam(match.team)
    }
  }, [urlTeamSlug, seasonTeamsForSlug])

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

  // Ensure selectedTeam is valid for the selected season (don't override URL on first mount)
  useEffect(() => {
    if (urlTeamSlug) return // URL will handle team selection
    const seasonTeams = teams.filter(t => t.season === selectedSeason)
    if (seasonTeams.length > 0 && !seasonTeams.find(t => t.team === selectedTeam)) {
      setSelectedTeam(seasonTeams[0].team)
    }
  }, [selectedSeason, teams, selectedTeam])

  // Player selection state
  const [selectedPlayers, setSelectedPlayers] = useState([])
  const [searchQuery, setSearchQuery] = useState('')

  // Filter teams by season
  const seasonFilteredTeams = useMemo(() => {
    if (selectedSeason === 'all') return teams
    return teams.filter(t => t.season === selectedSeason)
  }, [teams, selectedSeason])

  const teamList = useMemo(() => seasonFilteredTeams.map(t => t.team).sort(), [seasonFilteredTeams])

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

  // Create a mapping from player key to display info
  const playerDisplayMap = useMemo(() => {
    if (!currentTeamData?.players) return {}
    const map = {}
    Object.entries(currentTeamData.players).forEach(([key, player]) => {
      map[key] = {
        name: player.name || player.nickname || key,
        nickname: player.nickname || key
      }
    })
    return map
  }, [currentTeamData])

  // Helper to get display name for a player key
  const getPlayerDisplayName = (playerKey) => {
    return playerDisplayMap[playerKey]?.name || playerKey
  }

  // Get all players data for the table
  const allPlayersData = useMemo(() => {
    if (!currentTeamData?.players) return []

    const playersObj = currentTeamData.players
    return Object.entries(playersObj).map(([key, player]) => ({
      key,
      name: player.name || player.nickname || key,
      ...player
    }))
  }, [currentTeamData])

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

  // Filter players by search query (search by display name, not key)
  const filteredPlayers = useMemo(() => {
    if (searchQuery.trim() === '') return availablePlayers
    const query = searchQuery.toLowerCase()
    return availablePlayers.filter(playerKey => {
      const displayName = playerDisplayMap[playerKey]?.name || playerKey
      const nickname = playerDisplayMap[playerKey]?.nickname || playerKey
      return displayName.toLowerCase().includes(query) ||
             nickname.toLowerCase().includes(query)
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
    if (adjusted > threshold + 5) return { emoji: '🔥', label: 'Elite', color: 'text-positive' }
    if (adjusted > threshold + 2) return { emoji: '✅', label: 'Bueno', color: 'text-positive' }
    if (adjusted > threshold - 2) return { emoji: '➖', label: 'Normal', color: 'text-acb-500' }
    if (adjusted > threshold - 5) return { emoji: '⚠️', label: 'Debajo Media', color: 'text-acb-500' }
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
      <div className="space-y-6">
        <div>
          <h2 className="text-2xl font-semibold text-acb-900">Análisis de Alineaciones</h2>
          <p className="text-acb-500 text-sm mt-1">Cargando datos de combinaciones...</p>
        </div>
        <div className="bg-acb-50 rounded-lg p-8 text-center">
          <div className="animate-pulse">
            <div className="text-acb-600">Cargando alineaciones...</div>
          </div>
        </div>
      </div>
    )
  }

  return (
    <div className="space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-semibold text-acb-900">Análisis de Alineaciones</h2>
        <p className="text-acb-500 text-sm mt-1">
          Analiza el impacto on/off de jugadores y rendimiento de combinaciones
        </p>
      </div>

      {/* Controls */}
      <div className="bg-white rounded-lg border border-acb-200 p-4 space-y-4">
        <div className="flex flex-wrap items-center gap-4">
          {/* Season Filter */}
          <div className="flex items-center gap-2">
            <span className="text-sm text-acb-600 font-medium">Temporada:</span>
            <select
              value={selectedSeason}
              onChange={(e) => {
                const s = e.target.value === 'all' ? 'all' : parseInt(e.target.value)
                setSelectedSeason(s)
                clearPlayers()
                navigate('/alineaciones', { replace: true })
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
                const team = e.target.value
                setSelectedTeam(team)
                clearPlayers()
                if (team) {
                  navigate(`/alineaciones/${selectedSeason}/${toSlug(team)}`, { replace: true })
                }
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
              placeholder="Buscar jugadores..."
              value={searchQuery}
              onChange={(e) => setSearchQuery(e.target.value)}
              className="flex-1 px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
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
                  <button onClick={() => removePlayer(playerKey)} className="hover:text-accent-600">
                    <X className="w-3 h-3" />
                  </button>
                </div>
              ))}
              <button onClick={clearPlayers} className="text-sm text-acb-500 hover:text-acb-700">
                Limpiar
              </button>
            </div>
          )}

          {/* Player Grid */}
          <div className="max-h-48 overflow-y-auto border border-acb-100 rounded-md bg-acb-50/50">
            {filteredPlayers.length > 0 ? (
              <div className="grid grid-cols-2 sm:grid-cols-3 md:grid-cols-4 lg:grid-cols-5 gap-1 p-2">
                {filteredPlayers.map(playerKey => (
                  <button
                    key={playerKey}
                    onClick={() => addPlayer(playerKey)}
                    disabled={selectedPlayers.includes(playerKey) || selectedPlayers.length >= 5}
                    className={`px-2 py-1.5 text-sm rounded transition-all flex items-center gap-1.5 ${
                      selectedPlayers.includes(playerKey)
                        ? 'bg-accent-100 text-accent-700 font-medium'
                        : selectedPlayers.length >= 5
                          ? 'bg-acb-100 text-acb-400 cursor-not-allowed'
                          : 'bg-white hover:bg-accent-50 text-acb-700 hover:text-accent-700 border border-acb-200'
                    }`}
                  >
                    {getPlayerPhoto(playerPhotos, getIdFromKey(playerKey), selectedSeason) && (
                      <img src={getPlayerPhoto(playerPhotos, getIdFromKey(playerKey), selectedSeason)} alt="" className="w-5 h-5 rounded-full object-cover object-top" />
                    )}
                    {getPlayerDisplayName(playerKey)}
                  </button>
                ))}
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
      {selectedPlayers.length === 1 && currentLineupData && (
        <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
          <div className="bg-gradient-to-r from-acb-700 to-acb-800 px-4 py-3">
            <h3 className="font-semibold text-white text-lg">
              {currentLineupData.name || selectedPlayers[0]}
            </h3>
            <p className="text-acb-200 text-sm">
              {currentLineupData.onMin?.toFixed(1)} min en cancha • {currentLineupData.offMin?.toFixed(1)} min fuera
            </p>
          </div>

          {/* Stats Table */}
          <div className="overflow-x-auto">
            <table className="w-full">
              <thead>
                <tr className="bg-acb-50 text-left text-xs text-acb-600 uppercase tracking-wider">
                  <th className="px-4 py-3 font-semibold">Métrica</th>
                  <th className="px-4 py-3 font-semibold text-center">En Cancha</th>
                  <th className="px-4 py-3 font-semibold text-center">Fuera</th>
                  <th className="px-4 py-3 font-semibold text-center">Diferencia</th>
                  <th className="px-4 py-3 font-semibold text-center">Impacto</th>
                </tr>
              </thead>
              <tbody className="divide-y divide-acb-100">
                {/* ATAQUE */}
                <tr className="bg-acb-50">
                  <td colSpan={5} className="px-4 py-2 text-xs font-bold text-acb-600 uppercase tracking-wider">Ataque</td>
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
                />
                <StatRow
                  label="TOV%"
                  onValue={currentLineupData.onTOV}
                  offValue={currentLineupData.offTOV}
                  goodThreshold={15}
                  inverse
                />
                <StatRow
                  label="ORB%"
                  onValue={currentLineupData.onORB}
                  offValue={currentLineupData.offORB}
                  goodThreshold={30}
                />
                <StatRow
                  label="AST%"
                  onValue={currentLineupData.onAST}
                  offValue={currentLineupData.offAST}
                  goodThreshold={50}
                />
                {/* DEFENSA */}
                <tr className="bg-acb-50">
                  <td colSpan={5} className="px-4 py-2 text-xs font-bold text-acb-600 uppercase tracking-wider">Defensa (estadísticas del rival)</td>
                </tr>
                <StatRow
                  label="Ef. Defensiva"
                  onValue={currentLineupData.onDRtg}
                  offValue={currentLineupData.offDRtg}
                  goodThreshold={105}
                  inverse
                />
                <StatRow
                  label="eFG% Rival"
                  onValue={currentLineupData.onOppEFG}
                  offValue={currentLineupData.offOppEFG}
                  goodThreshold={50}
                  inverse
                />
                <StatRow
                  label="TOV% Rival"
                  onValue={currentLineupData.onOppTOV}
                  offValue={currentLineupData.offOppTOV}
                  goodThreshold={14}
                />
                <StatRow
                  label="DRB%"
                  onValue={currentLineupData.onDRB}
                  offValue={currentLineupData.offDRB}
                  goodThreshold={70}
                />
                {/* BALANCE */}
                <tr className="bg-acb-50">
                  <td colSpan={5} className="px-4 py-2 text-xs font-bold text-acb-600 uppercase tracking-wider">Balance</td>
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
                  {getPerformanceIndicator(currentLineupData.netDiff).emoji}
                </div>
                <div className={`text-2xl font-bold font-mono ${
                  currentLineupData.netDiff > 0 ? 'text-positive' :
                  currentLineupData.netDiff < 0 ? 'text-negative' : 'text-acb-500'
                }`}>
                  {currentLineupData.netDiff > 0 ? '+' : ''}{currentLineupData.netDiff?.toFixed(1)}
                </div>
                <div className="text-sm text-acb-600 mt-1">Impacto en Ef. Neta</div>
              </div>
            </div>
          </div>
        </div>
      )}

      {/* Pair/Trio/Lineup Analysis Results */}
      {selectedPlayers.length > 1 && currentLineupData && (
        <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
          <div className="bg-gradient-to-r from-acb-700 to-acb-800 px-4 py-3">
            <h3 className="font-semibold text-white text-lg">
              Análisis de {selectedPlayers.length === 2 ? 'Dúo' : selectedPlayers.length === 3 ? 'Trío' : 'Quinteto'}
            </h3>
            <p className="text-acb-200 text-sm">
              {selectedPlayers.map(k => getPlayerDisplayName(k)).join(' + ')} • {currentLineupData.onMin?.toFixed(1)} min juntos
              {currentLineupData.offMin != null && ` • ${currentLineupData.offMin?.toFixed(1)} min separados`}
            </p>
          </div>

          {/* Main Ratings */}
          <div className="grid grid-cols-4 divide-x divide-acb-200">
            <div className="p-4 text-center">
              <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">Ef. Ofensiva</div>
              <div className={`text-2xl font-bold font-mono ${getRatingColor(currentLineupData.onORtg)}`}>
                {currentLineupData.onORtg?.toFixed(1)}
              </div>
            </div>
            <div className="p-4 text-center">
              <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">Ef. Defensiva</div>
              <div className={`text-2xl font-bold font-mono ${getRatingColor(currentLineupData.onDRtg, true)}`}>
                {currentLineupData.onDRtg?.toFixed(1)}
              </div>
            </div>
            <div className="p-4 text-center">
              <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">Ef. Neta</div>
              <div className={`text-2xl font-bold font-mono ${
                currentLineupData.onNetRtg > 0 ? 'text-positive' :
                currentLineupData.onNetRtg < 0 ? 'text-negative' : 'text-acb-500'
              }`}>
                {currentLineupData.onNetRtg > 0 ? '+' : ''}{currentLineupData.onNetRtg?.toFixed(1)}
              </div>
              <div className="text-lg mt-1">
                {getPerformanceIndicator(currentLineupData.onNetRtg).emoji}
              </div>
            </div>
            {currentLineupData.netDiff != null ? (
              <div className="p-4 text-center bg-acb-50">
                <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">Impacto</div>
                <div className={`text-2xl font-bold font-mono ${
                  currentLineupData.netDiff > 0 ? 'text-positive' :
                  currentLineupData.netDiff < 0 ? 'text-negative' : 'text-acb-500'
                }`}>
                  {(currentLineupData.netDiff > 0 ? '+' : '') + currentLineupData.netDiff?.toFixed(1)}
                </div>
                <div className="text-lg mt-1">
                  {getPerformanceIndicator(currentLineupData.netDiff).emoji}
                </div>
              </div>
            ) : (
              <div className="p-4 text-center bg-acb-50">
                <div className="text-xs text-acb-500 uppercase tracking-wider mb-1">Posesiones</div>
                <div className="text-2xl font-bold font-mono text-acb-700">
                  {currentLineupData.onPoss}
                </div>
              </div>
            )}
          </div>

          {/* Ataque */}
          {(() => {
            const stats = [
              { label: 'eFG%',  val: currentLineupData.onEFG,  fmt: v => `${v.toFixed(1)}%`, color: 'text-acb-700' },
              { label: 'TOV%',  val: currentLineupData.onTOV,  fmt: v => `${v.toFixed(1)}%`, color: 'text-acb-700' },
              { label: 'ORB%',  val: currentLineupData.onORB,  fmt: v => `${v.toFixed(1)}%`, color: 'text-acb-700' },
              { label: 'AST%',  val: currentLineupData.onAST,  fmt: v => `${v.toFixed(1)}%`, color: 'text-acb-700' },
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

          {/* Defensa */}
          {(() => {
            const stats = [
              { label: 'DRtg',       val: currentLineupData.onDRtg,   fmt: v => v.toFixed(1),        color: currentLineupData.onDRtg   < 105 ? 'text-positive' : 'text-negative' },
              { label: 'eFG% Rival', val: currentLineupData.onOppEFG, fmt: v => `${v.toFixed(1)}%`,  color: currentLineupData.onOppEFG < 50  ? 'text-positive' : 'text-negative' },
              { label: 'TOV% Rival', val: currentLineupData.onOppTOV, fmt: v => `${v.toFixed(1)}%`,  color: currentLineupData.onOppTOV > 14  ? 'text-positive' : 'text-negative' },
              { label: 'DRB%',       val: currentLineupData.onDRB,    fmt: v => `${v.toFixed(1)}%`,  color: currentLineupData.onDRB    > 70  ? 'text-positive' : 'text-negative' },
            ].filter(s => s.val != null)
            if (stats.length === 0) return null
            return (
              <div className="border-t border-acb-200">
                <div className="px-4 py-2 bg-acb-50 text-xs font-bold text-acb-600 uppercase tracking-wider">Defensa (rival)</div>
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
      )}

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
          <div className="px-4 py-3 border-b border-acb-200 flex items-center justify-between">
            <h3 className="font-semibold text-acb-900">Resumen On/Off del Equipo</h3>
            <button
              onClick={() => setShowAllPlayers(!showAllPlayers)}
              className="text-sm text-acb-600 hover:text-acb-800 flex items-center gap-1"
            >
              {showAllPlayers ? 'Mostrar Menos' : 'Mostrar Todos'}
              {showAllPlayers ? <ChevronUp className="w-4 h-4" /> : <ChevronDown className="w-4 h-4" />}
            </button>
          </div>

          <div className="overflow-x-auto">
            <table className="w-full text-sm">
              <thead>
                <tr className="text-xs uppercase tracking-wider border-b border-acb-200">
                  <th className="px-4 py-1 bg-acb-50 text-left font-semibold text-acb-600" rowSpan={2}>Jugador</th>
                  <th colSpan={2} className="px-2 py-1 text-center bg-acb-50 text-acb-500 font-semibold">Ratings (Δ)</th>
                  <th colSpan={2} className="px-2 py-1 text-center bg-acb-50 text-acb-500 font-semibold">Ef. Neta</th>
                  <th className="px-2 py-1 bg-accent-50 text-accent-700 font-semibold text-center cursor-pointer hover:bg-acb-100 transition-colors" rowSpan={2} onClick={() => handleSort('netDiff')}>
                    <div className="flex items-center justify-center gap-1">
                      Impacto
                      {sortConfig.key === 'netDiff' && (
                        sortConfig.direction === 'desc'
                          ? <ChevronDown className="w-3 h-3" />
                          : <ChevronUp className="w-3 h-3" />
                      )}
                    </div>
                  </th>
                  <th colSpan={5} className="px-2 py-1 text-center bg-acb-50 text-acb-600 font-semibold">Ataque (Δ)</th>
                  <th colSpan={3} className="px-2 py-1 text-center bg-acb-50 text-acb-600 font-semibold">Defensa Rival (Δ)</th>
                  <th className="px-2 py-1 bg-acb-50 text-acb-500 font-semibold text-center" rowSpan={2}>Min</th>
                </tr>
                <tr className="text-xs text-acb-600 uppercase tracking-wider">
                  <SortableHeader label="ORtg" sortKey="onORtg" current={sortConfig} onSort={handleSort} />
                  <SortableHeader label="DRtg" sortKey="onDRtg" current={sortConfig} onSort={handleSort} />
                  <SortableHeader label="On" sortKey="onNetRtg" current={sortConfig} onSort={handleSort} />
                  <SortableHeader label="Off" sortKey="offNetRtg" current={sortConfig} onSort={handleSort} />
                  <SortableHeader label="eFG%" sortKey="onEFG" current={sortConfig} onSort={handleSort} thClassName="bg-acb-50" />
                  <SortableHeader label="TOV%" sortKey="onTOV" current={sortConfig} onSort={handleSort} thClassName="bg-acb-50" />
                  <SortableHeader label="ORB%" sortKey="onORB" current={sortConfig} onSort={handleSort} thClassName="bg-acb-50" />
                  <SortableHeader label="AST%" sortKey="onAST" current={sortConfig} onSort={handleSort} thClassName="bg-acb-50" />
                  <SortableHeader label="eFG% Rv" sortKey="onOppEFG" current={sortConfig} onSort={handleSort} thClassName="bg-acb-50" />
                  <SortableHeader label="TOV% Rv" sortKey="onOppTOV" current={sortConfig} onSort={handleSort} thClassName="bg-acb-50" />
                  <SortableHeader label="DRB%" sortKey="onDRB" current={sortConfig} onSort={handleSort} thClassName="bg-acb-50" />
                </tr>
              </thead>
              <tbody className="divide-y divide-acb-100">
                {(showAllPlayers ? sortedPlayersData : sortedPlayersData.slice(0, 8)).map((player) => (
                  <tr
                    key={player.key}
                    className={`hover:bg-acb-50 cursor-pointer transition-colors ${
                      selectedPlayers.includes(player.key) ? 'bg-accent-50' : ''
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
                      <span className="flex items-center gap-2">
                        {getPlayerPhoto(playerPhotos, player.id, selectedSeason) && (
                          <img src={getPlayerPhoto(playerPhotos, player.id, selectedSeason)} alt="" className="w-6 h-6 rounded-full object-cover object-top" />
                        )}
                        {player.name}
                      </span>
                    </td>
                    {(() => {
                      const ortgD = (player.onORtg ?? 0) - (player.offORtg ?? 0)
                      return (
                        <td className={`px-4 py-2 text-center font-mono ${ortgD > 0 ? 'text-positive' : ortgD < 0 ? 'text-negative' : 'text-acb-500'}`}>
                          {ortgD > 0 ? '+' : ''}{ortgD.toFixed(1)}
                        </td>
                      )
                    })()}
                    {(() => {
                      const drtgD = (player.onDRtg ?? 0) - (player.offDRtg ?? 0)
                      return (
                        <td className={`px-4 py-2 text-center font-mono ${drtgD < 0 ? 'text-positive' : drtgD > 0 ? 'text-negative' : 'text-acb-500'}`}>
                          {drtgD > 0 ? '+' : ''}{drtgD.toFixed(1)}
                        </td>
                      )
                    })()}
                    <td className={`px-4 py-2 text-center font-mono ${
                      player.onNetRtg > 0 ? 'text-positive' : 'text-negative'
                    }`}>
                      {player.onNetRtg > 0 ? '+' : ''}{player.onNetRtg?.toFixed(1)}
                    </td>
                    <td className="px-4 py-2 text-center font-mono text-acb-500">
                      {player.offNetRtg > 0 ? '+' : ''}{player.offNetRtg?.toFixed(1)}
                    </td>
                    <td className={`px-4 py-2 text-center font-mono font-semibold ${
                      player.netDiff > 2 ? 'text-positive' :
                      player.netDiff < -2 ? 'text-negative' : 'text-acb-500'
                    }`}>
                      <span className="mr-1">{getPerformanceIndicator(player.netDiff).emoji}</span>
                      {player.netDiff > 0 ? '+' : ''}{player.netDiff?.toFixed(1)}
                    </td>
                    {(() => {
                      const d = (player.onEFG ?? 0) - (player.offEFG ?? 0)
                      return <td className={`px-4 py-2 text-center font-mono ${d > 0 ? 'text-positive' : d < 0 ? 'text-negative' : 'text-acb-500'}`}>{d > 0 ? '+' : ''}{d.toFixed(1)}</td>
                    })()}
                    {(() => {
                      const d = (player.onTOV ?? 0) - (player.offTOV ?? 0)
                      return <td className={`px-4 py-2 text-center font-mono ${d < 0 ? 'text-positive' : d > 0 ? 'text-negative' : 'text-acb-500'}`}>{d > 0 ? '+' : ''}{d.toFixed(1)}</td>
                    })()}
                    {(() => {
                      const d = (player.onORB ?? 0) - (player.offORB ?? 0)
                      return <td className={`px-4 py-2 text-center font-mono ${d > 0 ? 'text-positive' : d < 0 ? 'text-negative' : 'text-acb-500'}`}>{d > 0 ? '+' : ''}{d.toFixed(1)}</td>
                    })()}
                    {(() => {
                      const d = (player.onAST ?? 0) - (player.offAST ?? 0)
                      return <td className={`px-4 py-2 text-center font-mono ${d > 0 ? 'text-positive' : d < 0 ? 'text-negative' : 'text-acb-500'}`}>{d > 0 ? '+' : ''}{d.toFixed(1)}</td>
                    })()}
                    {(() => {
                      const d = (player.onOppEFG ?? 0) - (player.offOppEFG ?? 0)
                      return <td className={`px-4 py-2 text-center font-mono ${d < 0 ? 'text-positive' : d > 0 ? 'text-negative' : 'text-acb-500'}`}>{d > 0 ? '+' : ''}{d.toFixed(1)}</td>
                    })()}
                    {(() => {
                      const d = (player.onOppTOV ?? 0) - (player.offOppTOV ?? 0)
                      return <td className={`px-4 py-2 text-center font-mono ${d > 0 ? 'text-positive' : d < 0 ? 'text-negative' : 'text-acb-500'}`}>{d > 0 ? '+' : ''}{d.toFixed(1)}</td>
                    })()}
                    {(() => {
                      const d = (player.onDRB ?? 0) - (player.offDRB ?? 0)
                      return <td className={`px-4 py-2 text-center font-mono ${d > 0 ? 'text-positive' : d < 0 ? 'text-negative' : 'text-acb-500'}`}>{d > 0 ? '+' : ''}{d.toFixed(1)}</td>
                    })()}
                    <td className="px-4 py-2 text-center font-mono text-acb-400 text-xs">
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
        <span className="flex items-center gap-1">⚠️ Debajo Media</span>
        <span className="flex items-center gap-1">🔻 Bajo</span>
      </div>

      {/* On/Off Explanation */}
      <div className="bg-acb-50 rounded-lg border border-acb-200 p-4">
        <h3 className="text-sm font-semibold text-acb-900 mb-3 flex items-center gap-2">
          <Info className="w-4 h-4" />
          Cómo interpretar las estadísticas On/Off
        </h3>
        <div className="text-sm text-acb-600 space-y-3">
          <p>
            El análisis <strong>On/Off</strong> mide el impacto de un jugador comparando cómo rinde el equipo cuando él está en pista versus cuando no.
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
              <div className="font-medium text-acb-900 mb-1">Impacto (Dif.)</div>
              <p className="text-xs">La diferencia entre On y Off. Indica cuánto mejora (o empeora) el equipo con su presencia.</p>
            </div>
          </div>
          <ul className="list-disc list-inside space-y-1 mt-2 text-xs">
            <li><strong>Eficiencia Ofensiva (ORtg):</strong> Puntos anotados por 100 posesiones. <span className="text-positive">Mayor es mejor</span>. Un Diff positivo significa que el ataque mejora con el jugador.</li>
            <li><strong>Eficiencia Defensiva (DRtg):</strong> Puntos recibidos por 100 posesiones. <span className="text-positive">Menor es mejor</span>. Un Diff negativo significa que la defensa mejora con el jugador.</li>
            <li><strong>Eficiencia Neta (NetRtg):</strong> Diferencia entre ORtg y DRtg. Muestra el margen de victoria por 100 posesiones.</li>
            <li><strong>Impacto:</strong>Diferencia entre el Net rating del equipo cuando el jugador está dentro y el Net rating del equipo cuando el jugador está fuera de la pista. Intenta medir el impacto del jugador eliminando el efecto de la dinámica general del equipo. </li>
          </ul>
        </div>
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
    if (adjusted > 5) return { emoji: '🔥', color: 'text-positive' }
    if (adjusted > 2) return { emoji: '✅', color: 'text-positive' }
    if (adjusted > -2) return { emoji: '➖', color: 'text-acb-500' }
    if (adjusted > -5) return { emoji: '⚠️', color: 'text-acb-500' }
    return { emoji: '🔻', color: 'text-negative' }
  }

  const indicator = getIndicator(diff, 0, inverse)

  return (
    <tr className={highlight ? 'bg-acb-50' : ''}>
      <td className={`px-4 py-3 ${highlight ? 'font-semibold' : ''} text-acb-700`}>
        {label}
      </td>
      <td className="px-4 py-3 text-center">
        <span className={`font-mono font-medium ${isGood ? 'text-positive' : 'text-negative'}`}>
          {onValue?.toFixed(1)}
        </span>
      </td>
      <td className="px-4 py-3 text-center font-mono text-acb-500">
        {offValue?.toFixed(1)}
      </td>
      <td className="px-4 py-3 text-center">
        <span className={`font-mono font-medium ${isGood ? 'text-positive' : 'text-negative'}`}>
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
const SortableHeader = ({ label, sortKey, current, onSort, highlight = false, thClassName = '' }) => {
  const isActive = current.key === sortKey

  return (
    <th
      className={`px-4 py-3 font-semibold text-center cursor-pointer hover:bg-acb-100 transition-colors ${
        highlight ? 'bg-accent-50' : ''
      } ${isActive ? 'text-accent-600' : ''} ${thClassName}`}
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
