import { useState, useMemo, useEffect } from 'react'
import Court, { ShotMarker } from '../components/Court'
import ZoneHeatmap from '../components/ZoneHeatmap'
import DensityHeatmap from '../components/DensityHeatmap'
import { Filter, Circle, X } from 'lucide-react'

// Note: Zone calculation functions removed since we now use pre-calculated
// zone and zoned fields from the CSV data

export default function ShotCharts({ loadShotsForSeason, shotsCache, loadingShots, teams, players }) {
  // Get available seasons from teams data (since we don't load all shots upfront)
  const availableSeasons = useMemo(() => {
    const seasons = [...new Set(teams.map(t => t.season))].sort((a, b) => b - a)
    return seasons
  }, [teams])

  const [selectedSeason, setSelectedSeason] = useState(availableSeasons[0] || 2025)
  const [filterType, setFilterType] = useState('team') // 'team', 'player' - default to 'team' to start empty
  const [selectedTeam, setSelectedTeam] = useState('')
  const [selectedPlayer, setSelectedPlayer] = useState('')
  const [playerSearch, setPlayerSearch] = useState('') // Search input for players
  const [shotFilter, setShotFilter] = useState('all') // 'all', 'made', 'missed'
  const [displayMode, setDisplayMode] = useState('shots') // 'shots', 'zones', 'heatmap'

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

    // Return array of {id, name} objects sorted by name
    return Array.from(playerMap.entries())
      .map(([id, name]) => ({ id, name }))
      .sort((a, b) => a.name.localeCompare(b.name))
  }, [seasonFilteredShots, filterType, selectedTeam])

  // Filtered player list based on search input
  const filteredPlayerList = useMemo(() => {
    if (!playerSearch) return playerList
    return playerList.filter(p =>
      p.name.toLowerCase().includes(playerSearch.toLowerCase())
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
            Visualiza patrones de tiro y eficiencia por zona {isLoadingSeasonShots && <span className="text-blue-600">- Cargando datos...</span>}
          </p>
        </div>
      </div>

      {/* Filters */}
      <div className="bg-white rounded-lg border border-acb-200 p-4">
        <div className="flex items-center gap-2 mb-4">
          <Filter className="w-4 h-4 text-acb-500" />
          <span className="text-sm font-medium text-acb-700">Filtros</span>
        </div>
        
        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-6 gap-4">
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
              <option value="zones">Estadísticas por Zona</option>
            </select>
          </div>

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
                  <option key={p.id} value={p.id}>{p.name}</option>
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
            <h3 className="font-medium text-acb-900">
              {filterType === 'player' && selectedPlayer
                ? playerList.find(p => p.id === selectedPlayer)?.name || selectedPlayer
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
                <span>Mapa de Calor</span>
              )}
              {displayMode === 'zones' && (
                <span>Estadísticas por Zona</span>
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
            <DensityHeatmap shots={filteredShots} width={750} height={705} />
          )}

          {displayMode === 'zones' && (
            <ZoneHeatmap shots={filteredShots} leagueShots={seasonFilteredShots} width={750} height={705} />
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
