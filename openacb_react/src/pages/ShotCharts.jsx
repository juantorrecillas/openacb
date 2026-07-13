import { useState, useMemo, useEffect } from 'react'
import { getPlayerPhoto } from '../utils/playerPhotos'
import { getPlayerDisplayName } from '../utils/playerNames'
import Court, { ShotMarker } from '../components/Court'
import ZoneHeatmap from '../components/ZoneHeatmap'
import DensityHeatmap from '../components/DensityHeatmap'
import PageHeader from '../components/PageHeader'
import PlayerCombobox from '../components/PlayerCombobox'
import { Filter, Circle, X } from 'lucide-react'


// Note: Zone calculation functions removed since we now use pre-calculated
// zone and zoned fields from the CSV data

const getPlayerRecord = (players, playerId, season) => {
  return players.find(p => String(p.licenseId) === String(playerId) && Number(p.season) === Number(season))
    || players.find(p => String(p.licenseId) === String(playerId))
}

const normalizeSearch = (value) => String(value || '')
  .normalize('NFD')
  .replace(/[\u0300-\u036f]/g, '')
  .toLocaleLowerCase('es')

const isMadeShot = (shot) => shot.made === true || shot.made === 'true' || shot.made === 1 || shot.made === '1'

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
    [...new Set(seasonFilteredShots.map(s => s.team))].sort((a, b) => a.localeCompare(b, 'es')),
    [seasonFilteredShots]
  )

  useEffect(() => {
    if (filterType === 'team') {
      const nextTeam = selectedTeam && teamList.includes(selectedTeam) ? selectedTeam : teamList[0] || ''
      if (nextTeam !== selectedTeam) {
        setSelectedTeam(nextTeam)
        setSelectedPlayer('')
      }
      return
    }

    if (selectedTeam && !teamList.includes(selectedTeam)) setSelectedTeam('')
  }, [filterType, selectedTeam, teamList])
  
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
        const record = getPlayerRecord(players, id, selectedSeason)
        const displayName = record ? getPlayerDisplayName(record, name) : name
        return { id, name, displayName }
      })
      .sort((a, b) => {
        const aSortKey = getSortKey(a.displayName)
        const bSortKey = getSortKey(b.displayName)
        return aSortKey.localeCompare(bSortKey)
      })
  }, [seasonFilteredShots, filterType, selectedTeam, players, selectedSeason])

  useEffect(() => {
    if (selectedPlayer && !playerList.some(player => player.id === selectedPlayer)) setSelectedPlayer('')
  }, [playerList, selectedPlayer])

  const playerOptions = useMemo(() => playerList.map(player => ({
    value: player.id,
    label: player.displayName,
    searchText: `${normalizeSearch(player.name)} ${normalizeSearch(player.displayName)}`,
    meta: selectedTeam || 'Todos los equipos',
  })), [playerList, selectedTeam])

  // conserva todos los intentos de la selección para los denominadores
  const analysisShots = useMemo(() => {
    if (!seasonFilteredShots || !Array.isArray(seasonFilteredShots)) return []

    if (filterType === 'team' && !selectedTeam) return []
    if (filterType === 'player' && !selectedPlayer) return []

    return seasonFilteredShots.filter(shot => {
      if (filterType === 'team' && selectedTeam && shot.team !== selectedTeam) {
        return false
      }
      if (filterType === 'player' && selectedPlayer && String(shot.playerId) !== selectedPlayer) {
        return false
      }
      if (filterType === 'player' && selectedTeam && shot.team !== selectedTeam) return false

      return true
    })
  }, [seasonFilteredShots, filterType, selectedTeam, selectedPlayer])

  // el resultado del tiro solo filtra capas visuales compatibles
  const filteredShots = useMemo(() => {
    if (shotFilter === 'all') return analysisShots
    return analysisShots.filter(shot => shotFilter === 'made' ? isMadeShot(shot) : !isMadeShot(shot))
  }, [analysisShots, shotFilter])

  const heatmapReferenceShots = useMemo(() => {
    if (!seasonFilteredShots || !Array.isArray(seasonFilteredShots)) return []
    if (shotFilter === 'all') return seasonFilteredShots

    return seasonFilteredShots.filter(shot => shotFilter === 'made' ? isMadeShot(shot) : !isMadeShot(shot))
  }, [seasonFilteredShots, shotFilter])
  
  const stats = useMemo(() => {
    const total = analysisShots.length
    const made = analysisShots.filter(isMadeShot).length
    const threes = analysisShots.filter(s => s.shotType?.includes('3'))
    const threeMade = threes.filter(isMadeShot).length
    const twos = analysisShots.filter(s => !s.shotType?.includes('3'))
    const twoMade = twos.filter(isMadeShot).length
    const points = analysisShots.reduce((sum, s) => sum + (Number(s.points) || 0), 0)
    
    return {
      total,
      made,
      fgPct: total > 0 ? ((made / total) * 100).toFixed(1) : '0.0',
      threePct: threes.length > 0 ? ((threeMade / threes.length) * 100).toFixed(1) : '0.0',
      twoPct: twos.length > 0 ? ((twoMade / twos.length) * 100).toFixed(1) : '0.0',
      pps: total > 0 ? (points / total).toFixed(2) : '0.00',
      efg: total > 0 ? (((twoMade + 1.5 * threeMade) / total) * 100).toFixed(1) : '0.0'
    }
  }, [analysisShots])
  
  // Zone breakdown
  const zoneStats = useMemo(() => {
    const byZone = {}
    analysisShots.forEach(shot => {
      // Use the pre-calculated zone field from CSV data
      const dataZone = shot.zoned || shot.zone
      if (!dataZone) return
      if (!byZone[dataZone]) {
        byZone[dataZone] = { attempts: 0, makes: 0, points: 0 }
      }
      byZone[dataZone].attempts++
      if (isMadeShot(shot)) {
        byZone[dataZone].makes++
        byZone[dataZone].points += Number(shot.points) || 0
      }
    })
    
    return Object.entries(byZone).map(([zone, data]) => ({
      zone,
      ...data,
      pct: ((data.makes / data.attempts) * 100).toFixed(1),
      pps: (data.points / data.attempts).toFixed(2)
    })).sort((a, b) => b.attempts - a.attempts)
  }, [analysisShots])

  const hasSelection = filterType === 'team' ? Boolean(selectedTeam) : Boolean(selectedPlayer)
  const efficiencyMap = displayMode === 'zones' && zoneMode === 'efficiency'

  return (
    <div className="app-page space-y-6">
      <PageHeader
        title="Cartas de tiro"
        subtitle="Visualiza la distribución y eficiencia de tiro de equipos y jugadores"
        scope={isLoadingSeasonShots ? 'Cargando datos…' : `Temporada ${selectedSeason - 1}-${String(selectedSeason).slice(-2)}`}
      />

      {/* Filters */}
      <div className="filter-panel block">
        <div className="flex items-center gap-2 mb-4">
          <Filter className="w-4 h-4 text-acb-500" />
          <span className="text-sm font-medium text-acb-700">Filtros</span>
        </div>
        
        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 xl:grid-cols-6 gap-4">
          {/* Season Filter */}
          <div>
            <label className="field-label mb-1">Temporada</label>
            <select
              aria-label="Temporada"
              value={selectedSeason}
              onChange={(e) => {
                setSelectedSeason(parseInt(e.target.value))
                setSelectedTeam('')
                setSelectedPlayer('')
              }}
              className="form-control"
            >
              {availableSeasons.map(season => (
                <option key={season} value={season}>{season-1}-{String(season).slice(-2)}</option>
              ))}
            </select>
          </div>

          {/* Display Mode */}
          <div>
            <label className="field-label mb-1">Mostrar</label>
            <select
              aria-label="Modo de visualización"
              value={displayMode}
              onChange={(e) => {
                const mode = e.target.value
                setDisplayMode(mode)
                if (mode === 'zones' && zoneMode === 'efficiency') setShotFilter('all')
              }}
              className="form-control"
            >
              <option value="shots">Tiros individuales</option>
              <option value="heatmap">Mapa de calor</option>
              <option value="zones">Zonas del campo</option>
            </select>
          </div>

          {displayMode === 'heatmap' && (
            <div>
              <label className="field-label mb-1">Tipo de mapa</label>
              <select
                aria-label="Tipo de mapa de calor"
                value={heatmapMode}
                onChange={(e) => setHeatmapMode(e.target.value)}
                className="form-control"
              >
                <option value="frequency">Frecuencia</option>
                <option value="density">Densidad</option>
              </select>
            </div>
          )}

          {displayMode === 'zones' && (
            <div>
              <label className="field-label mb-1">Métrica de zona</label>
              <select
                aria-label="Métrica del mapa por zonas"
                value={zoneMode}
                onChange={(e) => {
                  const mode = e.target.value
                  setZoneMode(mode)
                  if (mode === 'efficiency') setShotFilter('all')
                }}
                className="form-control"
              >
                <option value="efficiency">Eficiencia</option>
                <option value="frequency">Frecuencia</option>
              </select>
            </div>
          )}

          {/* Filter Type */}
          <div>
            <label className="field-label mb-1">Vista</label>
            <select
              aria-label="Vista por equipo o jugador"
              value={filterType}
              onChange={(e) => {
                setFilterType(e.target.value)
                setSelectedTeam('')
                setSelectedPlayer('')
              }}
              className="form-control"
            >
              <option value="team">Por equipo</option>
              <option value="player">Por jugador</option>
            </select>
          </div>
          
          {/* Team Select */}
          {(filterType === 'team' || filterType === 'player') && (
            <div>
              <label className="field-label mb-1">
                Equipo{filterType === 'player' ? ' (opcional)' : ''}
              </label>
              <select
                aria-label="Equipo"
                value={selectedTeam}
                onChange={(e) => {
                  setSelectedTeam(e.target.value)
                  setSelectedPlayer('')
                }}
                className="form-control"
              >
                <option value="" disabled={filterType === 'team'}>
                  {filterType === 'player' ? 'Todos los equipos' : 'Selecciona equipo…'}
                </option>
                {teamList.map(team => (
                  <option key={team} value={team}>{team}</option>
                ))}
              </select>
            </div>
          )}
          
          {/* Player Select with Search */}
          {filterType === 'player' && (
            <div className="sm:col-span-2">
              <PlayerCombobox
                id="shot-chart-player"
                label={`Jugador (${playerOptions.length})`}
                options={playerOptions}
                value={selectedPlayer}
                onChange={(option) => setSelectedPlayer(String(option?.value || ''))}
                placeholder="Buscar jugador…"
              />
            </div>
          )}
          
          {/* Shot Result */}
          <div>
            <label className="block text-xs font-medium text-acb-600 mb-1">Resultado</label>
            <select
              aria-label="Resultado del tiro"
              value={shotFilter}
              onChange={(e) => setShotFilter(e.target.value)}
              disabled={efficiencyMap}
              className="w-full px-3 py-2 border border-acb-200 rounded-md text-sm bg-white disabled:opacity-50 disabled:cursor-not-allowed"
            >
              <option value="all">Todos</option>
              <option value="made">Solo Anotados</option>
              <option value="missed">Solo Fallados</option>
            </select>
            {efficiencyMap && (
              <p className="text-xs text-acb-400 mt-1">La eficiencia requiere todos los intentos.</p>
            )}
          </div>
        </div>
      </div>
      
      {!hasSelection ? (
        <div className="bg-white rounded-lg border border-acb-200 p-12 text-center text-acb-500">
          Selecciona {filterType === 'team' ? 'un equipo' : 'un jugador'} para mostrar su carta de tiro
        </div>
      ) : (
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        {/* Court */}
        <div className="lg:col-span-2 bg-white rounded-lg border border-acb-200 p-6">
          <div className="flex flex-col gap-2 sm:flex-row sm:items-center sm:justify-between mb-4">
            <h3 className="font-medium text-acb-900 flex items-center gap-2 min-w-0">
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
                  : 'Selecciona una opción'}
            </h3>
            <div className="flex items-center gap-x-4 gap-y-1 text-xs text-acb-500 flex-wrap">
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
                    ? 'Frecuencia respecto a la media de la liga'
                    : 'Mapa de densidad'}
                </span>
              )}
              {displayMode === 'zones' && (
                <span>{zoneMode === 'frequency' ? 'Frecuencia por zona' : 'Eficiencia por zona'}</span>
              )}

            </div>
          </div>
          
          {displayMode === 'shots' && (
            <div className="overflow-x-auto pb-1">
              <div className="min-w-[560px]">
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
              </div>
            </div>
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
              shots={zoneMode === 'efficiency' ? analysisShots : filteredShots}
              leagueShots={zoneMode === 'efficiency' ? seasonFilteredShots : heatmapReferenceShots}
              metric={zoneMode}
              width={750}
              height={705}
            />
          )}

          <p className="text-xs text-acb-400 text-center mt-2">
            Visualizando {filteredShots.length} de {analysisShots.length} tiros
          </p>
          {shotFilter !== 'all' && (
            <p className="text-xs text-acb-400 text-center mt-1">
              Los porcentajes y el desglose se calculan con todos los intentos de la selección.
            </p>
          )}
        </div>
        
        {/* Stats Sidebar */}
        <div className="space-y-4">
          {/* Summary Stats */}
          <div className="bg-white rounded-lg border border-acb-200 p-4">
            <h3 className="text-sm font-medium text-acb-700 mb-1">Resumen</h3>
            <p className="text-xs text-acb-400 mb-3">Todos los intentos</p>
            <div className="grid grid-cols-2 gap-3">
              <div>
                <div className="text-2xl font-semibold font-mono text-acb-900">
                  {stats.fgPct}%
                </div>
                <div className="text-xs text-acb-500">TC%</div>
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
      )}
    </div>
  )
}
