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
    if ((filterType === 'team' || filterType === 'player') && selectedTeam) {
      return [...new Set(seasonFilteredShots.filter(s => s.team === selectedTeam).map(s => s.player))].sort()
    }
    return [...new Set(seasonFilteredShots.map(s => s.player))].sort()
  }, [seasonFilteredShots, filterType, selectedTeam])

  // Filtered player list based on search input
  const filteredPlayerList = useMemo(() => {
    if (!playerSearch) return playerList
    return playerList.filter(player =>
      player.toLowerCase().includes(playerSearch.toLowerCase())
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
      if (filterType === 'player' && selectedPlayer && shot.player !== selectedPlayer) {
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
          <h2 className="text-2xl font-semibold text-acb-900">Shot Charts</h2>
          <p className="text-acb-500 text-sm mt-1">
            Visualize shooting patterns and efficiency by zone {isLoadingSeasonShots && <span className="text-blue-600">- Loading season data...</span>}
          </p>
        </div>
      </div>

      {/* Filters */}
      <div className="bg-white rounded-lg border border-acb-200 p-4">
        <div className="flex items-center gap-2 mb-4">
          <Filter className="w-4 h-4 text-acb-500" />
          <span className="text-sm font-medium text-acb-700">Filters</span>
        </div>
        
        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-6 gap-4">
          {/* Season Filter */}
          <div>
            <label className="block text-xs font-medium text-acb-600 mb-1">Season</label>
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
            <label className="block text-xs font-medium text-acb-600 mb-1">Display</label>
            <select
              value={displayMode}
              onChange={(e) => setDisplayMode(e.target.value)}
              className="w-full px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
            >
              <option value="shots">Individual Shots</option>
              <option value="heatmap">Density Heatmap</option>
              <option value="zones">Zone Statistics</option>
            </select>
          </div>

          {/* Filter Type */}
          <div>
            <label className="block text-xs font-medium text-acb-600 mb-1">View</label>
            <select
              value={filterType}
              onChange={(e) => {
                setFilterType(e.target.value)
                setSelectedTeam('')
                setSelectedPlayer('')
              }}
              className="w-full px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
            >
              <option value="team">By Team</option>
              <option value="player">By Player</option>
            </select>
          </div>
          
          {/* Team Select */}
          {(filterType === 'team' || filterType === 'player') && (
            <div>
              <label className="block text-xs font-medium text-acb-600 mb-1">Team</label>
              <select
                value={selectedTeam}
                onChange={(e) => {
                  setSelectedTeam(e.target.value)
                  setSelectedPlayer('')
                }}
                className="w-full px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
              >
                <option value="">Select team...</option>
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
                Player {filteredPlayerList.length > 0 && `(${filteredPlayerList.length})`}
              </label>
              {/* Search input */}
              <input
                type="text"
                value={playerSearch}
                onChange={(e) => setPlayerSearch(e.target.value)}
                placeholder="Search player..."
                className="w-full px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
              />
              {/* Dropdown */}
              <select
                value={selectedPlayer}
                onChange={(e) => setSelectedPlayer(e.target.value)}
                className="w-full px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
                size="4"
              >
                <option value="">Select player...</option>
                {filteredPlayerList.map(player => (
                  <option key={player} value={player}>{player}</option>
                ))}
              </select>
            </div>
          )}
          
          {/* Shot Result */}
          <div>
            <label className="block text-xs font-medium text-acb-600 mb-1">Result</label>
            <select
              value={shotFilter}
              onChange={(e) => setShotFilter(e.target.value)}
              className="w-full px-3 py-2 border border-acb-200 rounded-md text-sm bg-white"
            >
              <option value="all">All Shots</option>
              <option value="made">Made Only</option>
              <option value="missed">Missed Only</option>
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
                ? selectedPlayer 
                : filterType === 'team' && selectedTeam 
                  ? selectedTeam 
                  : 'All Players'}
            </h3>
            <div className="flex items-center gap-4 text-xs text-acb-500">
              {displayMode === 'shots' && (
                <>
                  <span className="flex items-center gap-1">
                    <Circle className="w-3 h-3 fill-positive text-positive" /> Made
                  </span>
                  <span className="flex items-center gap-1">
                    <X className="w-3 h-3 text-negative" /> Missed
                  </span>
                </>
              )}
              {displayMode === 'heatmap' && (
                <span>Density Heatmap</span>
              )}
              {displayMode === 'zones' && (
                <span>Zone Statistics</span>
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
            Showing all {filteredShots.length} shots
          </p>
        </div>
        
        {/* Stats Sidebar */}
        <div className="space-y-4">
          {/* Summary Stats */}
          <div className="bg-white rounded-lg border border-acb-200 p-4">
            <h3 className="text-sm font-medium text-acb-700 mb-3">Summary</h3>
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
                <div className="text-xs text-acb-500">PPS</div>
              </div>
              <div>
                <div className="text-2xl font-semibold font-mono text-acb-900">
                  {stats.made}/{stats.total}
                </div>
                <div className="text-xs text-acb-500">Made/Att</div>
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
            <h3 className="text-sm font-medium text-acb-700 mb-3">By Zone</h3>
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
                      {zone.pps} PPS
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
