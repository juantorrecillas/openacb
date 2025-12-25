import { useState, useMemo } from 'react'
import { BarChart3, Target, Users, TrendingUp, Percent, Circle } from 'lucide-react'
import { ScatterChart, Scatter, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, ReferenceLine, BarChart, Bar, Legend, Cell } from 'recharts'

/**
 * Four Factors Analysis Page - Dean Oliver's Basketball Success Framework
 * 
 * The Four Factors of Basketball Success:
 * 1. Shooting (40%) - Effective Field Goal Percentage (eFG%)
 * 2. Turnovers (25%) - Turnover Percentage (TOV%)
 * 3. Rebounding (20%) - Offensive/Defensive Rebound Percentage (ORB%/DRB%)
 * 4. Free Throws (15%) - Free Throw Rate (FT/FGA)
 */

export default function FourFactors({ teams }) {
  // Get available seasons and default to most recent
  const availableSeasons = useMemo(() => {
    const seasons = [...new Set(teams.map(t => t.season))].sort((a, b) => b - a)
    return seasons
  }, [teams])

  const [selectedSeason, setSelectedSeason] = useState(availableSeasons[0] || 2025)
  const [viewMode, setViewMode] = useState('overview') // 'overview', 'shooting', 'turnovers', 'rebounding', 'free-throws'
  const [sortKey, setSortKey] = useState('netRating')
  const [sortDir, setSortDir] = useState('desc')
  const [highlightTeam, setHighlightTeam] = useState(null)

  // Filter teams by season
  const seasonFilteredTeams = useMemo(() => {
    if (selectedSeason === 'all') return teams
    return teams.filter(t => t.season === selectedSeason)
  }, [teams, selectedSeason])

  // Calculate Four Factors for each team using actual values (no inversions)
  // Data comes in decimal format (0-1), so multiply by 100 to get percentages
  const teamsWithFourFactors = useMemo(() => {
    return seasonFilteredTeams.map(team => {
      // Offensive Four Factors (actual values from data)
      const offensiveShooting = (team.efg || 0) * 100 // eFG%
      const offensiveTurnovers = (team.tovRate || 0) * 100 // TOV% (actual, not inverted)
      const offensiveRebounding = (team.orbPct || 0) * 100 // ORB%
      const offensiveFreeThrows = (team.ftRate || 0) * 100 // FT/FGA
      const offensiveRating = team.ortg || 0 // Offensive Rating

      // Defensive Four Factors (opponent actual values)
      const defensiveShooting = (team.opp_efg || 0) * 100 // Opponent eFG% (actual)
      const defensiveTurnovers = (team.opp_tovRate || 0) * 100 // Opponent TOV% (actual)
      const defensiveRebounding = (team.drbPct || 0) * 100 // DRB%
      const defensiveFreeThrows = (team.opp_ftRate || 0) * 100 // Opponent FT/FGA (actual)
      const defensiveRating = team.drtg || 0 // Defensive Rating

      // Net Rating
      const netRating = team.netRtg || 0

      return {
        ...team,
        offensiveShooting,
        offensiveTurnovers,
        offensiveRebounding,
        offensiveFreeThrows,
        offensiveRating,
        defensiveShooting,
        defensiveTurnovers,
        defensiveRebounding,
        defensiveFreeThrows,
        defensiveRating,
        netRating
      }
    })
  }, [seasonFilteredTeams])

  // Calculate ranks for each metric (1 = best)
  const teamsWithRanks = useMemo(() => {
    const metrics = [
      { key: 'offensiveShooting', higherIsBetter: true },
      { key: 'offensiveTurnovers', higherIsBetter: false }, // Lower TOV% is better
      { key: 'offensiveRebounding', higherIsBetter: true },
      { key: 'offensiveFreeThrows', higherIsBetter: true },
      { key: 'offensiveRating', higherIsBetter: true },
      { key: 'defensiveShooting', higherIsBetter: false }, // Lower opponent eFG% is better
      { key: 'defensiveTurnovers', higherIsBetter: true }, // Higher opponent TOV% is better
      { key: 'defensiveRebounding', higherIsBetter: true },
      { key: 'defensiveFreeThrows', higherIsBetter: false }, // Lower opponent FT rate is better
      { key: 'defensiveRating', higherIsBetter: false }, // Lower DRtg is better
      { key: 'netRating', higherIsBetter: true }
    ]

    const rankedTeams = teamsWithFourFactors.map(team => ({ ...team }))

    metrics.forEach(({ key, higherIsBetter }) => {
      const sorted = [...rankedTeams].sort((a, b) => {
        const aVal = a[key] || 0
        const bVal = b[key] || 0
        return higherIsBetter ? bVal - aVal : aVal - bVal
      })

      sorted.forEach((team, index) => {
        const originalTeam = rankedTeams.find(t => t.team === team.team && t.season === team.season)
        if (originalTeam) {
          originalTeam[`${key}Rank`] = index + 1
        }
      })
    })

    return rankedTeams
  }, [teamsWithFourFactors])

  // Sort teams
  const sortedTeams = useMemo(() => {
    return [...teamsWithRanks].sort((a, b) => {
      const aVal = a[sortKey] || 0
      const bVal = b[sortKey] || 0
      return sortDir === 'desc' ? bVal - aVal : aVal - bVal
    })
  }, [teamsWithRanks, sortKey, sortDir])

  // Calculate league averages
  const leagueAverages = useMemo(() => {
    if (teamsWithFourFactors.length === 0) return null
    
    const keys = ['offensiveShooting', 'offensiveTurnovers', 'offensiveRebounding', 'offensiveFreeThrows',
                  'defensiveShooting', 'defensiveTurnovers', 'defensiveRebounding', 'defensiveFreeThrows', 'fourFactorScore']
    
    const avgs = {}
    keys.forEach(key => {
      const values = teamsWithFourFactors.map(t => t[key]).filter(v => v != null)
      avgs[key] = values.reduce((sum, v) => sum + v, 0) / values.length
    })
    
    return avgs
  }, [teamsWithFourFactors])

  const handleSort = (key) => {
    if (sortKey === key) {
      setSortDir(sortDir === 'desc' ? 'asc' : 'desc')
    } else {
      setSortKey(key)
      setSortDir('desc')
    }
  }

  const formatValue = (value, decimals = 1) => {
    if (value === undefined || value === null) return '-'
    return value.toFixed(decimals)
  }

  const getPerformanceColor = (value, avg, inverse = false) => {
    if (value == null || avg == null) return 'text-acb-700'
    const isGood = inverse ? value < avg : value > avg
    return isGood ? 'text-positive font-medium' : 'text-negative'
  }

  const getRankColor = (rank, totalTeams) => {
    if (!rank) return 'bg-acb-100 text-acb-600'
    const percentile = rank / totalTeams
    if (percentile <= 0.25) return 'bg-green-100 text-green-700' // Top 25%
    if (percentile <= 0.5) return 'bg-blue-100 text-blue-700'   // Top 50%
    if (percentile <= 0.75) return 'bg-orange-100 text-orange-700' // Top 75%
    return 'bg-red-100 text-red-700' // Bottom 25%
  }

  // Prepare data for charts
  const chartData = useMemo(() => {
    return sortedTeams.map(team => ({
      name: team.team,
      shooting: team.offensiveShooting,
      turnovers: team.offensiveTurnovers,
      rebounding: team.offensiveRebounding,
      freeThrows: team.offensiveFreeThrows,
      offensiveScore: team.offensiveFourFactorScore,
      defensiveScore: team.defensiveFourFactorScore,
      overallScore: team.overallFourFactorScore
    }))
  }, [sortedTeams])

  return (
    <div className="space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-semibold text-acb-900">Análisis de Cuatro Factores</h2>
        <p className="text-acb-500 text-sm mt-1">
          Marco de Dean Oliver para el éxito en baloncesto - Tiro (40%), Pérdidas (25%), Rebotes (20%), Tiros Libres (15%)
        </p>
      </div>

      {/* Controls */}
      <div className="bg-white rounded-lg border border-acb-200 p-4 space-y-4">
        <div className="flex flex-wrap items-center gap-4">
          {/* Season Filter */}
          <div className="flex items-center gap-2">
            <span className="text-sm text-acb-600 font-medium">Season:</span>
            <select
              value={selectedSeason}
              onChange={(e) => setSelectedSeason(e.target.value === 'all' ? 'all' : parseInt(e.target.value))}
              className="px-3 py-2 border border-acb-200 rounded-md text-sm bg-white font-medium"
            >
              {availableSeasons.map(season => (
                <option key={season} value={season}>{season-1}-{String(season).slice(-2)}</option>
              ))}
              <option value="all">All Seasons</option>
            </select>
          </div>

          {/* View Mode Toggle */}
          <div className="flex items-center gap-1 bg-acb-100 rounded-md p-1">
            <button
              onClick={() => setViewMode('overview')}
              className={`px-3 py-1.5 text-sm font-medium rounded transition-colors ${
                viewMode === 'overview' 
                  ? 'bg-white text-acb-900 shadow-sm'
                  : 'text-acb-600 hover:text-acb-900'
              }`}
            >
              Resumen
            </button>
            <button
              onClick={() => setViewMode('shooting')}
              className={`px-3 py-1.5 text-sm font-medium rounded transition-colors ${
                viewMode === 'shooting'
                  ? 'bg-white text-acb-900 shadow-sm'
                  : 'text-acb-600 hover:text-acb-900'
              }`}
            >
              Tiro
            </button>
            <button
              onClick={() => setViewMode('turnovers')}
              className={`px-3 py-1.5 text-sm font-medium rounded transition-colors ${
                viewMode === 'turnovers'
                  ? 'bg-white text-acb-900 shadow-sm'
                  : 'text-acb-600 hover:text-acb-900'
              }`}
            >
              Pérdidas
            </button>
            <button
              onClick={() => setViewMode('rebounding')}
              className={`px-3 py-1.5 text-sm font-medium rounded transition-colors ${
                viewMode === 'rebounding'
                  ? 'bg-white text-acb-900 shadow-sm'
                  : 'text-acb-600 hover:text-acb-900'
              }`}
            >
              Rebotes
            </button>
            <button
              onClick={() => setViewMode('free-throws')}
              className={`px-3 py-1.5 text-sm font-medium rounded transition-colors ${
                viewMode === 'free-throws'
                  ? 'bg-white text-acb-900 shadow-sm'
                  : 'text-acb-600 hover:text-acb-900'
              }`}
            >
              Tiros Libres
            </button>
          </div>
        </div>

      </div>

      {/* Shooting Analysis */}
      {viewMode === 'shooting' && (
        <div className="bg-white rounded-lg border border-acb-200 p-6">
          <h3 className="text-lg font-semibold text-acb-900 mb-4">Análisis de Tiro (40% peso)</h3>
          <p className="text-acb-500 text-sm mb-4">
            Porcentaje de Tiro Efectivo (eFG%) = (Tiros 2 Anotados + 1.5 × Triples Anotados) / Tiros Campo Intentados
          </p>
          <div className="h-96">
            <ResponsiveContainer width="100%" height="100%">
              <ScatterChart margin={{ top: 20, right: 20, bottom: 40, left: 40 }}>
                <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                <XAxis 
                  type="number"
                  dataKey="offensiveShooting"
                  name="Offensive eFG%"
                  domain={[40, 60]}
                  stroke="#64748b"
                  fontSize={12}
                  label={{ value: 'Offensive Shooting Efficiency', position: 'bottom', style: { fill: '#64748b', fontSize: 12 } }}
                />
                <YAxis
                  type="number"
                  dataKey="defensiveShooting"
                  name="Defensive eFG%"
                  domain={[40, 60]}
                  stroke="#64748b"
                  fontSize={12}
                  label={{ value: 'Defensive Shooting Efficiency', angle: -90, position: 'left', style: { fill: '#64748b', fontSize: 12 } }}
                />
                {leagueAverages && (
                  <>
                    <ReferenceLine x={leagueAverages.offensiveShooting} stroke="#94a3b8" strokeDasharray="5 5" />
                    <ReferenceLine y={leagueAverages.defensiveShooting} stroke="#94a3b8" strokeDasharray="5 5" />
                  </>
                )}
                <Tooltip
                  content={({ payload }) => {
                    if (!payload?.[0]) return null
                    const team = payload[0].payload
                    return (
                      <div className="bg-white border border-acb-200 rounded-lg p-3 shadow-lg">
                        <div className="font-medium text-acb-900 mb-1">{team.team}</div>
                        <div className="text-sm text-acb-600">
                          Offensive eFG%: {formatValue(team.offensiveShooting)}%
                        </div>
                        <div className="text-sm text-acb-600">
                          Defensive eFG%: {formatValue(team.defensiveShooting)}%
                        </div>
                      </div>
                    )
                  }}
                />
                <Scatter data={sortedTeams} fill="#475569">
                  {sortedTeams.map((team, i) => (
                    <circle
                      key={team.team}
                      r={highlightTeam === team.team ? 8 : 6}
                      fill={highlightTeam === team.team ? '#f97316' : '#10b981'}
                      fillOpacity={highlightTeam === team.team ? 1 : 0.7}
                    />
                  ))}
                </Scatter>
              </ScatterChart>
            </ResponsiveContainer>
          </div>
        </div>
      )}

      {/* Turnovers Analysis */}
      {viewMode === 'turnovers' && (
        <div className="bg-white rounded-lg border border-acb-200 p-6">
          <h3 className="text-lg font-semibold text-acb-900 mb-4">Análisis de Pérdidas (25% peso)</h3>
          <p className="text-acb-500 text-sm mb-4">
            Porcentaje de Pérdidas (TOV%) = 100 × (Pérdidas / Total Jugadas)
          </p>
          <div className="h-96">
            <ResponsiveContainer width="100%" height="100%">
              <ScatterChart margin={{ top: 20, right: 20, bottom: 40, left: 40 }}>
                <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                <XAxis 
                  type="number"
                  dataKey="offensiveTurnovers"
                  name="Offensive TOV%"
                  domain={[80, 95]}
                  stroke="#64748b"
                  fontSize={12}
                  label={{ value: 'Offensive Turnover Avoidance', position: 'bottom', style: { fill: '#64748b', fontSize: 12 } }}
                />
                <YAxis
                  type="number"
                  dataKey="defensiveTurnovers"
                  name="Defensive TOV%"
                  domain={[5, 15]}
                  stroke="#64748b"
                  fontSize={12}
                  label={{ value: 'Defensive Turnover Creation', angle: -90, position: 'left', style: { fill: '#64748b', fontSize: 12 } }}
                />
                {leagueAverages && (
                  <>
                    <ReferenceLine x={leagueAverages.offensiveTurnovers} stroke="#94a3b8" strokeDasharray="5 5" />
                    <ReferenceLine y={leagueAverages.defensiveTurnovers} stroke="#94a3b8" strokeDasharray="5 5" />
                  </>
                )}
                <Tooltip
                  content={({ payload }) => {
                    if (!payload?.[0]) return null
                    const team = payload[0].payload
                    return (
                      <div className="bg-white border border-acb-200 rounded-lg p-3 shadow-lg">
                        <div className="font-medium text-acb-900 mb-1">{team.team}</div>
                        <div className="text-sm text-acb-600">
                          Offensive TOV%: {formatValue(team.offensiveTurnovers)}%
                        </div>
                        <div className="text-sm text-acb-600">
                          Defensive TOV%: {formatValue(team.defensiveTurnovers)}%
                        </div>
                      </div>
                    )
                  }}
                />
                <Scatter data={sortedTeams} fill="#475569">
                  {sortedTeams.map((team, i) => (
                    <circle
                      key={team.team}
                      r={highlightTeam === team.team ? 8 : 6}
                      fill={highlightTeam === team.team ? '#f97316' : '#3b82f6'}
                      fillOpacity={highlightTeam === team.team ? 1 : 0.7}
                    />
                  ))}
                </Scatter>
              </ScatterChart>
            </ResponsiveContainer>
          </div>
        </div>
      )}

      {/* Rebounding Analysis */}
      {viewMode === 'rebounding' && (
        <div className="bg-white rounded-lg border border-acb-200 p-6">
          <h3 className="text-lg font-semibold text-acb-900 mb-4">Análisis de Rebotes (20% peso)</h3>
          <p className="text-acb-500 text-sm mb-4">
            Porcentaje de Rebotes Ofensivos (ORB%) = 100 × (Rebotes Ofensivos) / (Rebotes Ofensivos + Rebotes Defensivos del rival)
          </p>
          <div className="h-96">
            <ResponsiveContainer width="100%" height="100%">
              <ScatterChart margin={{ top: 20, right: 20, bottom: 40, left: 40 }}>
                <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                <XAxis 
                  type="number"
                  dataKey="offensiveRebounding"
                  name="ORB%"
                  domain={[20, 40]}
                  stroke="#64748b"
                  fontSize={12}
                  label={{ value: 'Offensive Rebounding', position: 'bottom', style: { fill: '#64748b', fontSize: 12 } }}
                />
                <YAxis
                  type="number"
                  dataKey="defensiveRebounding"
                  name="DRB%"
                  domain={[60, 80]}
                  stroke="#64748b"
                  fontSize={12}
                  label={{ value: 'Defensive Rebounding', angle: -90, position: 'left', style: { fill: '#64748b', fontSize: 12 } }}
                />
                {leagueAverages && (
                  <>
                    <ReferenceLine x={leagueAverages.offensiveRebounding} stroke="#94a3b8" strokeDasharray="5 5" />
                    <ReferenceLine y={leagueAverages.defensiveRebounding} stroke="#94a3b8" strokeDasharray="5 5" />
                  </>
                )}
                <Tooltip
                  content={({ payload }) => {
                    if (!payload?.[0]) return null
                    const team = payload[0].payload
                    return (
                      <div className="bg-white border border-acb-200 rounded-lg p-3 shadow-lg">
                        <div className="font-medium text-acb-900 mb-1">{team.team}</div>
                        <div className="text-sm text-acb-600">
                          Offensive ORB%: {formatValue(team.offensiveRebounding)}%
                        </div>
                        <div className="text-sm text-acb-600">
                          Defensive DRB%: {formatValue(team.defensiveRebounding)}%
                        </div>
                      </div>
                    )
                  }}
                />
                <Scatter data={sortedTeams} fill="#475569">
                  {sortedTeams.map((team, i) => (
                    <circle
                      key={team.team}
                      r={highlightTeam === team.team ? 8 : 6}
                      fill={highlightTeam === team.team ? '#f97316' : '#8b5cf6'}
                      fillOpacity={highlightTeam === team.team ? 1 : 0.7}
                    />
                  ))}
                </Scatter>
              </ScatterChart>
            </ResponsiveContainer>
          </div>
        </div>
      )}

      {/* Free Throws Analysis */}
      {viewMode === 'free-throws' && (
        <div className="bg-white rounded-lg border border-acb-200 p-6">
          <h3 className="text-lg font-semibold text-acb-900 mb-4">Análisis de Tiros Libres (15% peso)</h3>
          <p className="text-acb-500 text-sm mb-4">
            Ratio de Tiros Libres (FT/FGA) = Tiros Libres Intentados / Tiros de Campo Intentados
          </p>
          <div className="h-96">
            <ResponsiveContainer width="100%" height="100%">
              <ScatterChart margin={{ top: 20, right: 20, bottom: 40, left: 40 }}>
                <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                <XAxis
                  type="number"
                  dataKey="offensiveFreeThrows"
                  name="FT/FGA"
                  domain={[10, 35]}
                  stroke="#64748b"
                  fontSize={12}
                  label={{ value: 'Offensive Free Throw Rate (%)', position: 'bottom', style: { fill: '#64748b', fontSize: 12 } }}
                />
                <YAxis
                  type="number"
                  dataKey="defensiveFreeThrows"
                  name="Opp FT/FGA (inverted)"
                  domain={[65, 90]}
                  stroke="#64748b"
                  fontSize={12}
                  label={{ value: 'Defensive Free Throw Prevention', angle: -90, position: 'left', style: { fill: '#64748b', fontSize: 12 } }}
                />
                {leagueAverages && (
                  <>
                    <ReferenceLine x={leagueAverages.offensiveFreeThrows} stroke="#94a3b8" strokeDasharray="5 5" />
                    <ReferenceLine y={leagueAverages.defensiveFreeThrows} stroke="#94a3b8" strokeDasharray="5 5" />
                  </>
                )}
                <Tooltip
                  content={({ payload }) => {
                    if (!payload?.[0]) return null
                    const team = payload[0].payload
                    return (
                      <div className="bg-white border border-acb-200 rounded-lg p-3 shadow-lg">
                        <div className="font-medium text-acb-900 mb-1">{team.team}</div>
                        <div className="text-sm text-acb-600">
                          Offensive FT/FGA: {formatValue(team.offensiveFreeThrows, 1)}%
                        </div>
                        <div className="text-sm text-acb-600">
                          Defensive FT/FGA: {formatValue(team.defensiveFreeThrows, 1)}%
                        </div>
                      </div>
                    )
                  }}
                />
                <Scatter data={sortedTeams} fill="#475569">
                  {sortedTeams.map((team, i) => (
                    <circle
                      key={team.team}
                      r={highlightTeam === team.team ? 8 : 6}
                      fill={highlightTeam === team.team ? '#f97316' : '#f59e0b'}
                      fillOpacity={highlightTeam === team.team ? 1 : 0.7}
                    />
                  ))}
                </Scatter>
              </ScatterChart>
            </ResponsiveContainer>
          </div>
        </div>
      )}

      {/* Detailed Table */}
      <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
        <div className="overflow-x-auto">
          <table className="w-full">
            <thead>
              {/* Header row 1: Column groups */}
              <tr className="bg-acb-100 border-b border-acb-300">
                <th className="px-4 py-2 text-left text-xs font-semibold text-acb-700 uppercase tracking-wider w-8" rowSpan="2">#</th>
                <th className="px-4 py-2 text-left text-xs font-semibold text-acb-700 uppercase tracking-wider" rowSpan="2">Team</th>
                <th className="px-2 py-2 text-center text-xs font-semibold text-acb-700 uppercase tracking-wider border-r border-acb-300" colSpan="5">Ataque (Offense)</th>
                <th className="px-2 py-2 text-center text-xs font-semibold text-acb-700 uppercase tracking-wider border-r border-acb-300" colSpan="5">Defensa (Defense)</th>
                <th className="px-2 py-2 text-center text-xs font-semibold text-acb-700 uppercase tracking-wider" rowSpan="2">Net</th>
              </tr>
              {/* Header row 2: Individual columns */}
              <tr className="bg-acb-50 border-b border-acb-200">
                <th className={`px-2 py-3 text-center text-xs font-semibold text-acb-600 uppercase tracking-wider cursor-pointer hover:bg-acb-100 ${
                  sortKey === 'offensiveShooting' ? 'bg-acb-100' : ''}`}
                    onClick={() => handleSort('offensiveShooting')} title="Effective Field Goal %">eFG%</th>
                <th className={`px-2 py-3 text-center text-xs font-semibold text-acb-600 uppercase tracking-wider cursor-pointer hover:bg-acb-100 ${
                  sortKey === 'offensiveTurnovers' ? 'bg-acb-100' : ''}`}
                    onClick={() => handleSort('offensiveTurnovers')} title="Turnover %">TOV%</th>
                <th className={`px-2 py-3 text-center text-xs font-semibold text-acb-600 uppercase tracking-wider cursor-pointer hover:bg-acb-100 ${
                  sortKey === 'offensiveRebounding' ? 'bg-acb-100' : ''}`}
                    onClick={() => handleSort('offensiveRebounding')} title="Offensive Rebound %">ORB%</th>
                <th className={`px-2 py-3 text-center text-xs font-semibold text-acb-600 uppercase tracking-wider cursor-pointer hover:bg-acb-100 ${
                  sortKey === 'offensiveFreeThrows' ? 'bg-acb-100' : ''}`}
                    onClick={() => handleSort('offensiveFreeThrows')} title="Free Throw Rate">FTr</th>
                <th className={`px-2 py-3 text-center text-xs font-semibold text-acb-600 uppercase tracking-wider cursor-pointer hover:bg-acb-100 border-r border-acb-300 ${
                  sortKey === 'offensiveRating' ? 'bg-acb-100' : ''}`}
                    onClick={() => handleSort('offensiveRating')} title="Offensive Rating">ORtg</th>
                <th className={`px-2 py-3 text-center text-xs font-semibold text-acb-600 uppercase tracking-wider cursor-pointer hover:bg-acb-100 ${
                  sortKey === 'defensiveShooting' ? 'bg-acb-100' : ''}`}
                    onClick={() => handleSort('defensiveShooting')} title="Opponent eFG%">eFG%</th>
                <th className={`px-2 py-3 text-center text-xs font-semibold text-acb-600 uppercase tracking-wider cursor-pointer hover:bg-acb-100 ${
                  sortKey === 'defensiveTurnovers' ? 'bg-acb-100' : ''}`}
                    onClick={() => handleSort('defensiveTurnovers')} title="Opponent TOV%">TOV%</th>
                <th className={`px-2 py-3 text-center text-xs font-semibold text-acb-600 uppercase tracking-wider cursor-pointer hover:bg-acb-100 ${
                  sortKey === 'defensiveRebounding' ? 'bg-acb-100' : ''}`}
                    onClick={() => handleSort('defensiveRebounding')} title="Defensive Rebound %">DRB%</th>
                <th className={`px-2 py-3 text-center text-xs font-semibold text-acb-600 uppercase tracking-wider cursor-pointer hover:bg-acb-100 ${
                  sortKey === 'defensiveFreeThrows' ? 'bg-acb-100' : ''}`}
                    onClick={() => handleSort('defensiveFreeThrows')} title="Opponent FT Rate">FTr</th>
                <th className={`px-2 py-3 text-center text-xs font-semibold text-acb-600 uppercase tracking-wider cursor-pointer hover:bg-acb-100 border-r border-acb-300 ${
                  sortKey === 'defensiveRating' ? 'bg-acb-100' : ''}`}
                    onClick={() => handleSort('defensiveRating')} title="Defensive Rating">DRtg</th>
              </tr>
            </thead>
            <tbody>
              {sortedTeams.map((team, i) => {
                const totalTeams = sortedTeams.length
                return (
                  <tr
                    key={`${team.team}-${team.season}`}
                    className={`border-b border-acb-100 hover:bg-acb-50 transition-colors ${
                      highlightTeam === team.team ? 'bg-orange-50' : ''
                    }`}
                    onMouseEnter={() => setHighlightTeam(team.team)}
                    onMouseLeave={() => setHighlightTeam(null)}
                  >
                    <td className="px-4 py-3 text-sm text-acb-400 font-mono">{i + 1}</td>
                    <td className="px-4 py-3 text-sm font-medium text-acb-900">{team.team}</td>
                    {/* Offensive Four Factors */}
                    <td className="px-2 py-3 text-sm text-center">
                      <div className="flex flex-col items-center gap-1">
                        <span className="font-mono">{formatValue(team.offensiveShooting, 1)}</span>
                        <span className={`text-xs px-1.5 py-0.5 rounded ${getRankColor(team.offensiveShootingRank, totalTeams)}`}>
                          #{team.offensiveShootingRank}
                        </span>
                      </div>
                    </td>
                    <td className="px-2 py-3 text-sm text-center">
                      <div className="flex flex-col items-center gap-1">
                        <span className="font-mono">{formatValue(team.offensiveTurnovers, 1)}</span>
                        <span className={`text-xs px-1.5 py-0.5 rounded ${getRankColor(team.offensiveTurnoversRank, totalTeams)}`}>
                          #{team.offensiveTurnoversRank}
                        </span>
                      </div>
                    </td>
                    <td className="px-2 py-3 text-sm text-center">
                      <div className="flex flex-col items-center gap-1">
                        <span className="font-mono">{formatValue(team.offensiveRebounding, 1)}</span>
                        <span className={`text-xs px-1.5 py-0.5 rounded ${getRankColor(team.offensiveReboundingRank, totalTeams)}`}>
                          #{team.offensiveReboundingRank}
                        </span>
                      </div>
                    </td>
                    <td className="px-2 py-3 text-sm text-center">
                      <div className="flex flex-col items-center gap-1">
                        <span className="font-mono">{formatValue(team.offensiveFreeThrows, 1)}</span>
                        <span className={`text-xs px-1.5 py-0.5 rounded ${getRankColor(team.offensiveFreeThrowsRank, totalTeams)}`}>
                          #{team.offensiveFreeThrowsRank}
                        </span>
                      </div>
                    </td>
                    <td className="px-2 py-3 text-sm text-center border-r border-acb-300">
                      <div className="flex flex-col items-center gap-1">
                        <span className="font-mono font-semibold">{formatValue(team.offensiveRating, 1)}</span>
                        <span className={`text-xs px-1.5 py-0.5 rounded ${getRankColor(team.offensiveRatingRank, totalTeams)}`}>
                          #{team.offensiveRatingRank}
                        </span>
                      </div>
                    </td>
                    {/* Defensive Four Factors */}
                    <td className="px-2 py-3 text-sm text-center">
                      <div className="flex flex-col items-center gap-1">
                        <span className="font-mono">{formatValue(team.defensiveShooting, 1)}</span>
                        <span className={`text-xs px-1.5 py-0.5 rounded ${getRankColor(team.defensiveShootingRank, totalTeams)}`}>
                          #{team.defensiveShootingRank}
                        </span>
                      </div>
                    </td>
                    <td className="px-2 py-3 text-sm text-center">
                      <div className="flex flex-col items-center gap-1">
                        <span className="font-mono">{formatValue(team.defensiveTurnovers, 1)}</span>
                        <span className={`text-xs px-1.5 py-0.5 rounded ${getRankColor(team.defensiveTurnoversRank, totalTeams)}`}>
                          #{team.defensiveTurnoversRank}
                        </span>
                      </div>
                    </td>
                    <td className="px-2 py-3 text-sm text-center">
                      <div className="flex flex-col items-center gap-1">
                        <span className="font-mono">{formatValue(team.defensiveRebounding, 1)}</span>
                        <span className={`text-xs px-1.5 py-0.5 rounded ${getRankColor(team.defensiveReboundingRank, totalTeams)}`}>
                          #{team.defensiveReboundingRank}
                        </span>
                      </div>
                    </td>
                    <td className="px-2 py-3 text-sm text-center">
                      <div className="flex flex-col items-center gap-1">
                        <span className="font-mono">{formatValue(team.defensiveFreeThrows, 1)}</span>
                        <span className={`text-xs px-1.5 py-0.5 rounded ${getRankColor(team.defensiveFreeThrowsRank, totalTeams)}`}>
                          #{team.defensiveFreeThrowsRank}
                        </span>
                      </div>
                    </td>
                    <td className="px-2 py-3 text-sm text-center border-r border-acb-300">
                      <div className="flex flex-col items-center gap-1">
                        <span className="font-mono font-semibold">{formatValue(team.defensiveRating, 1)}</span>
                        <span className={`text-xs px-1.5 py-0.5 rounded ${getRankColor(team.defensiveRatingRank, totalTeams)}`}>
                          #{team.defensiveRatingRank}
                        </span>
                      </div>
                    </td>
                    {/* Net Rating */}
                    <td className="px-2 py-3 text-sm text-center">
                      <div className="flex flex-col items-center gap-1">
                        <span className="font-mono font-bold">{formatValue(team.netRating, 1)}</span>
                        <span className={`text-xs px-1.5 py-0.5 rounded ${getRankColor(team.netRatingRank, totalTeams)}`}>
                          #{team.netRatingRank}
                        </span>
                      </div>
                    </td>
                  </tr>
                )
              })}
            </tbody>
          </table>
        </div>

        {/* Summary */}
        {leagueAverages && (
          <div className="px-4 py-3 bg-acb-50 border-t border-acb-200 text-sm text-acb-600">
            <div className="flex items-center justify-between">
              <span>League Average Four Factor Score: {formatValue(leagueAverages.fourFactorScore)}</span>
              <span>Teams: {sortedTeams.length}</span>
            </div>
          </div>
        )}
      </div>

      {/* Four Factors Explanation */}
      <div className="bg-acb-50 rounded-lg border border-acb-200 p-4">
        <h3 className="text-sm font-semibold text-acb-900 mb-3 flex items-center gap-2">
          <Circle className="w-4 h-4" />
          About the Four Factors
        </h3>
        <div className="text-sm text-acb-600 space-y-3">
          <p>
            <strong>Los Cuatro Factores de Dean Oliver</strong> proporcionan un marco para entender qué gana los partidos de baloncesto.
            Cada factor tiene un peso aproximado basado en su importancia:
          </p>
          <ul className="list-disc list-inside space-y-1">
            <li><strong>Tiro (40%)</strong>: Porcentaje de Tiro Efectivo (eFG%) que tiene en cuenta el valor extra de los triples</li>
            <li><strong>Pérdidas (25%)</strong>: Porcentaje de pérdidas más bajo significa mejor protección del balón</li>
            <li><strong>Rebotes (20%)</strong>: Porcentajes de rebotes ofensivos y defensivos muestran el control del tablero</li>
            <li><strong>Tiros Libres (15%)</strong>: Ratio de tiros libres mide la capacidad de llegar a la línea y convertir</li>
          </ul>
          <p>
            El <strong>Puntuación de Cuatro Factores</strong> ahora incluye tres métricas:
          </p>
          <ul className="list-disc list-inside space-y-1">
            <li><strong>Ofensiva</strong>: Ponderación de los cuatro factores ofensivos usando los pesos de Dean Oliver</li>
            <li><strong>Defensiva</strong>: Ponderación de los cuatro factores defensivos usando los mismos pesos</li>
            <li><strong>General</strong>: Promedio de las puntuaciones ofensiva y defensiva para una visión global</li>
          </ul>
          <p>
            Cada puntuación usa la fórmula ponderada: (Tiro × 40%) + (Pérdidas × 25%) + (Rebotes × 20%) + (Tiros Libres × 15%)
          </p>
        </div>
      </div>
    </div>
  )
}