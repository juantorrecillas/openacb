import { useState, useMemo, useEffect } from 'react'
import { getPlayerPhoto } from '../utils/playerPhotos'
import Court, { ShotMarker } from '../components/Court'
import ZoneHeatmap from '../components/ZoneHeatmap'
import DensityHeatmap from '../components/DensityHeatmap'
import { Filter, Circle, X } from 'lucide-react'


// Note: Zone calculation functions removed since we now use pre-calculated
// zone and zoned fields from the CSV data

// Helper to get abbreviated name from player data
// Uses pre-calculated playerAbbrev field from data (e.g., "J. Fernández")
const getPlayerAbbrev = (players, playerId) => {
  const player = players.find(p => String(p.licenseId) === String(playerId))
  return player?.playerAbbrev || player?.playerFull || '-'
}

export default function ShotCharts({ loadShotsForSeason, shotsCache, loadingShots, teams, players, playerPhotos = {} }) {
  // Get available seasons from teams data (since we don't load all shots upfront)
  const availableSeasons = useMemo(() => {
    const seasons = [...new Set(teams.map(t => t.season))].filter(s => s >= 2021).sort((a, b) => b - a)
    return seasons
  }, [teams])

  const [selectedSeason, setSelectedSeason] = useState(availableSeasons[0] || 2025)
  const [filterType, setFilterType] = useState('team') // 'team', 'player' - default to 'team' to start empty
  const [selectedTeam, setSelectedTeam] = useState('')
  const [selectedPlayer, setSelectedPlayer] = useState('')
  const [playerSearch, setPlayerSearch] = useState('') // Search input for players
  const [shotFilter, setShotFilter] = useState('all') // 'all', 'made', 'missed'
  const [displayMode, setDisplayMode] = useState('shots') // 'shots', 'zones', 'heatmap'
  const [heatmapMode, setHeatmapMode] = useState('frequency') // 'frequency', 'density'
  const [zoneMode, setZoneMode] = useState('efficiency') // 'efficiency', 'frequency'

  // Load shots when season changes
  useEffect(() => {
    if (selectedSeason) {
      loadShotsForSeason(selectedSeason)
    }
  }, [selectedSeason, loadShotsForSeason])

  // Get shots for current season from cache
  const seasonFilteredShots = useMemo(() => {
    return shotsCache[selectedSeason] || []
  }, [shotsCache, selectedSeason])

  // Check if shots are currently loading
  const isLoadingSeasonShots = loadingShots[selectedSeason] || false

  const teamList = useMemo(() =>
    [...new Set(seasonFilteredShots.map(s => s.team))].sort(),
    [seasonFilteredShots]
  )
  
  const playerList = useMemo(() => {
    // Build list of unique players using playerId as the unique key
    const shots = (filterType === 'team' || filterType === 'player') && selectedTeam
      ? seasonFilteredShots.filter(s => s.team === selectedTeam)
      : seasonFilteredShots

    // Create a map of playerId -> player name to ensure uniqueness by ID
    const playerMap = new Map()
    shots.forEach(s => {
      const id = String(s.playerId)
      if (s.playerId && !playerMap.has(id)) {
        playerMap.set(id, s.player)
      }
    })

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

  // Return array of {id, name, displayName} objects sorted by surname
    return Array.from(playerMap.entries())
      .map(([id, name]) => {
        // Get abbreviated name from players data if available
        const displayName = getPlayerAbbrev(players, id)
        return { id, name, displayName }
      })
      .sort((a, b) => {
        const aSortKey = getSortKey(a.displayName)
        const bSortKey = getSortKey(b.displayName)
        return aSortKey.localeCompare(bSortKey)
      })
  }, [seasonFilteredShots, filterType, selectedTeam, players])

  // Filtered player list based on search input (search both full name and abbreviated)
  const filteredPlayerList = useMemo(() => {
    if (!playerSearch) return playerList
    const search = playerSearch.toLowerCase()
    return playerList.filter(p =>
      p.name.toLowerCase().includes(search) ||
      p.displayName.toLowerCase().includes(search)
    )
  }, [playerList, playerSearch])

  const zones = useMemo(() => {
    // Use the pre-calculated zone fields from the CSV data
    // 'zoned' contains the detailed zone information with direction
    if (seasonFilteredShots.length > 0) {
      const dataZones = [...new Set(seasonFilteredShots.map(s => s.zoned))].sort()
      return dataZones
    }
    return []
  }, [seasonFilteredShots])
  
  const filteredShots = useMemo(() => {
    if (!seasonFilteredShots || !Array.isArray(seasonFilteredShots)) return []

    // If team filter is active but no team selected, return empty (start empty)
    if (filterType === 'team' && !selectedTeam) return []

    // If player filter is active but no player selected, return empty
    if (filterType === 'player' && !selectedPlayer) return []

    return seasonFilteredShots.filter(shot => {
      // Team/Player filtering
      if (filterType === 'team' && selectedTeam && shot.team !== selectedTeam) {
        return false
      }
      if (filterType === 'player' && selectedPlayer && String(shot.playerId) !== selectedPlayer) {
        return false
      }

      // Made/missed filtering
      if (shotFilter !== 'all') {
        const isMade = shot.made === true || shot.made === 'true' || shot.made === 1 || shot.made === '1'
        if (shotFilter === 'made' && !isMade) {
          return false
        }
        if (shotFilter === 'missed' && isMade) {
          return false
        }
      }

      return true
    })
  }, [seasonFilteredShots, filterType, selectedTeam, selectedPlayer, shotFilter])

  const heatmapReferenceShots = useMemo(() => {
    if (!seasonFilteredShots || !Array.isArray(seasonFilteredShots)) return []
    if (shotFilter === 'all') return seasonFilteredShots

    return seasonFilteredShots.filter(shot => {
      const isMade = shot.made === true || shot.made === 'true' || shot.made === 1 || shot.made === '1'
      return shotFilter === 'made' ? isMade : !isMade
    })
  }, [seasonFilteredShots, shotFilter])
  
  const stats = useMemo(() => {
    const total = filteredShots.length
    const made = filteredShots.filter(s => s.made).length
    const threes = filteredShots.filter(s => s.shotType?.includes('3'))
    const threeMade = threes.filter(s => s.made).length
    const twos = filteredShots.filter(s => !s.shotType?.includes('3'))
    const twoMade = twos.filter(s => s.made).length
    const points = filteredShots.reduce((sum, s) => sum + s.points, 0)
    
    return {
      total,
      made,
      fgPct: total > 0 ? ((made / total) * 100).toFixed(1) : '0.0',
      threePct: threes.length > 0 ? ((threeMade / threes.length) * 100).toFixed(1) : '0.0',
      twoPct: twos.length > 0 ? ((twoMade / twos.length) * 100).toFixed(1) : '0.0',
      pps: total > 0 ? (points / total).toFixed(2) : '0.00',
      efg: total > 0 ? (((twoMade + 1.5 * threeMade) / total) * 100).toFixed(1) : '0.0'
    }
  }, [filteredShots])
  
  // Zone breakdown
  const zoneStats = useMemo(() => {
    const byZone = {}
    filteredShots.forEach(shot => {
      // Use the pre-calculated zone field from CSV data
      const dataZone = shot.zoned || shot.zone
      if (!byZone[dataZone]) {
        byZone[dataZone] = { attempts: 0, makes: 0, points: 0 }
      }
      byZone[dataZone].attempts++
      if (shot.made) {
        byZone[dataZone].makes++
        byZone[dataZone].points += shot.points
      }
    })
    
    return Object.entries(byZone).map(([zone, data]) => ({
      zone,
      ...data,
      pct: ((data.makes / data.attempts) * 100).toFixed(1),
      pps: (data.points / data.attempts).toFixed(2)
    })).sort((a, b) => b.attempts - a.attempts)
  }, [filteredShots])

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex items-center justify-between">
        <div>
          <h2 className="text-2xl font-semibold text-acb-900">Carta de Tiro</h2>
          <p className="text-acb-500 text-sm mt-1">
            Visualiza las cartas de tiro de equipos y jugadores {isLoadingSeasonShots && <span className="text-info-600">- Cargando datos...</span>}
          </p>
        </div>
      </div>

      {/* Filters */}
      <div className="bg-white rounded-lg border border-acb-200 p-4">
        <div className="flex items-center gap-2 mb-4">
          <Filter className="w-4 h-4 text-acb-500" />
          <span className="text-sm font-medium text-acb-700">Filtros</span>
        </div>
        
        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-7 gap-4">
          {/* Season Filter */}
          <div>
            <label className="block text-xs font-medium text-acb-600 mb-1">Temporada</label>
            <select
              value={selectedSeason}
              onChange={(e) => setSelectedSeason(parseInt(e.target.value))}
              className="w-full px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
            >
              {availableSeasons.map(season => (
                <option key={season} value={season}>{season-1}-{String(season).slice(-2)}</option>
              ))}
            </select>
          </div>

          {/* Display Mode */}
          <div>
            <label className="block text-xs font-medium text-acb-600 mb-1">Mostrar</label>
            <select
              value={displayMode}
              onChange={(e) => setDisplayMode(e.target.value)}
              className="w-full px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
            >
              <option value="shots">Tiros Individuales</option>
              <option value="heatmap">Mapa de Calor</option>
              <option value="zones">Zona del Campo</option>
            </select>
          </div>

          {displayMode === 'heatmap' && (
            <div>
              <label className="block text-xs font-medium text-acb-600 mb-1">Tipo Mapa</label>
              <select
                value={heatmapMode}
                onChange={(e) => setHeatmapMode(e.target.value)}
                className="w-full px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
              >
                <option value="frequency">Frecuencia</option>
                <option value="density">Densidad</option>
              </select>
            </div>
          )}

          {displayMode === 'zones' && (
            <div>
              <label className="block text-xs font-medium text-acb-600 mb-1">Tipo Zona</label>
              <select
                value={zoneMode}
                onChange={(e) => setZoneMode(e.target.value)}
                className="w-full px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
              >
                <option value="efficiency">Eficiencia</option>
                <option value="frequency">Frecuencia</option>
              </select>
            </div>
          )}

          {/* Filter Type */}
          <div>
            <label className="block text-xs font-medium text-acb-600 mb-1">Vista</label>
            <select
              value={filterType}
              onChange={(e) => {
                setFilterType(e.target.value)
                setSelectedTeam('')
                setSelectedPlayer('')
              }}
              className="w-full px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
            >
              <option value="team">Por Equipo</option>
              <option value="player">Por Jugador</option>
            </select>
          </div>
          
          {/* Team Select */}
          {(filterType === 'team' || filterType === 'player') && (
            <div>
              <label className="block text-xs font-medium text-acb-600 mb-1">Equipo</label>
              <select
                value={selectedTeam}
                onChange={(e) => {
                  setSelectedTeam(e.target.value)
                  setSelectedPlayer('')
                }}
                className="w-full px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
              >
                <option value="">Selecciona equipo...</option>
                {teamList.map(team => (
                  <option key={team} value={team}>{team}</option>
                ))}
              </select>
            </div>
          )}
          
          {/* Player Select with Search */}
          {filterType === 'player' && (
            <div className="space-y-2">
              <label className="block text-xs font-medium text-acb-600">
                Jugador {filteredPlayerList.length > 0 && `(${filteredPlayerList.length})`}
              </label>
              {/* Search input */}
              <input
                type="text"
                value={playerSearch}
                onChange={(e) => setPlayerSearch(e.target.value)}
                placeholder="Buscar jugador..."
                className="w-full px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
              />
              {/* Dropdown */}
              <select
                value={selectedPlayer}
                onChange={(e) => setSelectedPlayer(e.target.value)}
                className="w-full px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
                size="4"
              >
                <option value="">Selecciona jugador...</option>
                {filteredPlayerList.map(p => (
                  <option key={p.id} value={p.id}>{p.displayName}</option>
                ))}
              </select>
            </div>
          )}
          
          {/* Shot Result */}
          <div>
            <label className="block text-xs font-medium text-acb-600 mb-1">Resultado</label>
            <select
              value={shotFilter}
              onChange={(e) => setShotFilter(e.target.value)}
              className="w-full px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
            >
              <option value="all">Todos</option>
              <option value="made">Solo Anotados</option>
              <option value="missed">Solo Fallados</option>
            </select>
          </div>
        </div>
      </div>
      
      {/* Main Content */}
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        {/* Court */}
        <div className="lg:col-span-2 bg-white rounded-lg border border-acb-200 p-6">
          <div className="flex items-center justify-between mb-4">
            <h3 className="font-medium text-acb-900 flex items-center gap-2">
              {filterType === 'player' && selectedPlayer && getPlayerPhoto(playerPhotos, selectedPlayer, selectedSeason) && (
                <img
                  src={getPlayerPhoto(playerPhotos, selectedPlayer, selectedSeason)}
                  alt=""
                  className="w-8 h-8 rounded-full object-cover object-top border border-acb-200"
                />
              )}
              {filterType === 'player' && selectedPlayer
                ? playerList.find(p => p.id === selectedPlayer)?.displayName || selectedPlayer
                : filterType === 'team' && selectedTeam
                  ? selectedTeam
                  : 'Todos los Jugadores'}
            </h3>
            <div className="flex items-center gap-4 text-xs text-acb-500">
              {displayMode === 'shots' && (
                <>
                  <span className="flex items-center gap-1">
                    <Circle className="w-3 h-3 fill-positive text-positive" /> Anotado
                  </span>
                  <span className="flex items-center gap-1">
                    <X className="w-3 h-3 text-negative" /> Fallado
                  </span>
                </>
              )}
              {displayMode === 'heatmap' && (
                <span>
                  {heatmapMode === 'frequency'
                    ? 'Frecuencia'
                    : 'Mapa de Calor'}
                </span>
              )}
              {displayMode === 'zones' && (
                <span>{zoneMode === 'frequency' ? 'Frecuencia por zona' : 'Zona del Campo'}</span>
              )}

            </div>
          </div>
          
          {displayMode === 'shots' && (
            <Court width={750} height={705}>
              {filteredShots.map((shot, i) => (
                <ShotMarker
                  key={shot.id || i}
                  x={shot.x}
                  y={shot.y}
                  made={shot.made}
                  size={5}
                  width={750}
                />
              ))}
            </Court>
          )}

          {displayMode === 'heatmap' && (
            <DensityHeatmap
              shots={filteredShots}
              referenceShots={heatmapReferenceShots}
              mode={heatmapMode}
              width={750}
              height={705}
            />
          )}

          {displayMode === 'zones' && (
            <ZoneHeatmap
              shots={filteredShots}
              leagueShots={heatmapReferenceShots}
              metric={zoneMode}
              width={750}
              height={705}
            />
          )}

          <p className="text-xs text-acb-400 text-center mt-2">
            Mostrando {filteredShots.length} tiros
          </p>
        </div>
        
        {/* Stats Sidebar */}
        <div className="space-y-4">
          {/* Summary Stats */}
          <div className="bg-white rounded-lg border border-acb-200 p-4">
            <h3 className="text-sm font-medium text-acb-700 mb-3">Resumen</h3>
            <div className="grid grid-cols-2 gap-3">
              <div>
                <div className="text-2xl font-semibold font-mono text-acb-900">
                  {stats.fgPct}%
                </div>
                <div className="text-xs text-acb-500">FG%</div>
              </div>
              <div>
                <div className="text-2xl font-semibold font-mono text-acb-900">
                  {stats.efg}%
                </div>
                <div className="text-xs text-acb-500">eFG%</div>
              </div>
              <div>
                <div className="text-2xl font-semibold font-mono text-acb-900">
                  {stats.pps}
                </div>
                <div className="text-xs text-acb-500">PPT</div>
              </div>
              <div>
                <div className="text-2xl font-semibold font-mono text-acb-900">
                  {stats.made}/{stats.total}
                </div>
                <div className="text-xs text-acb-500">Anot/Int</div>
              </div>
              <div>
                <div className="text-2xl font-semibold font-mono text-acb-900">
                  {stats.twoPct}%
                </div>
                <div className="text-xs text-acb-500">2P%</div>
              </div>
              <div>
                <div className="text-2xl font-semibold font-mono text-acb-900">
                  {stats.threePct}%
                </div>
                <div className="text-xs text-acb-500">3P%</div>
              </div>
            </div>
          </div>
          
          {/* Zone Breakdown */}
          <div className="bg-white rounded-lg border border-acb-200 p-4">
            <h3 className="text-sm font-medium text-acb-700 mb-3">Por Zona</h3>
            <div className="space-y-2">
              {zoneStats.map(zone => (
                <div key={zone.zone} className="flex items-center justify-between py-1.5 border-b border-acb-100 last:border-0">
                  <div>
                    <div className="text-sm text-acb-800">{zone.zone}</div>
                    <div className="text-xs text-acb-500">
                      {zone.makes}/{zone.attempts}
                    </div>
                  </div>
                  <div className="text-right">
                    <div className="text-sm font-mono font-medium text-acb-900">
                      {zone.pct}%
                    </div>
                    <div className="text-xs text-acb-500 font-mono">
                      {zone.pps} PPT
                    </div>
                  </div>
                </div>
              ))}
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}
