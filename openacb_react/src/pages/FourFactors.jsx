import { useState, useMemo } from 'react'
import { Circle } from 'lucide-react'


export default function FourFactors({ teams }) {
  // Get available seasons and default to most recent
  const availableSeasons = useMemo(() => {
    const seasons = [...new Set(teams.map(t => t.season))].sort((a, b) => b - a)
    return seasons
  }, [teams])

  const [selectedSeason, setSelectedSeason] = useState(availableSeasons[0] || 2025)
  const [sortKey, setSortKey] = useState('netRating')
  const [sortDir, setSortDir] = useState('desc')
  const [highlightTeam, setHighlightTeam] = useState(null)

  // Filter teams by season
  const seasonFilteredTeams = useMemo(() => {
    if (selectedSeason === 'all') return teams
    return teams.filter(t => t.season === selectedSeason)
  }, [teams, selectedSeason])

  const teamsWithFourFactors = useMemo(() => {
    return seasonFilteredTeams.map(team => {
      const offensiveShooting = (team.efg || 0) * 100 // eFG%
      const offensiveTurnovers = (team.tovRate || 0) * 100 // TOV% (actual, not inverted)
      const offensiveRebounding = (team.orbPct || 0) * 100 // ORB%
      const offensiveFreeThrows = (team.ftRate || 0) * 100 // FT/FGA
      const offensiveRating = team.ortg || 0 // Offensive Rating

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

  const getRankColor = (rank, totalTeams) => {
    if (!rank) return 'bg-acb-100 text-acb-600'
    const percentile = rank / totalTeams
    if (percentile <= 0.25) return 'bg-green-100 text-green-700' // Top 25%
    if (percentile <= 0.5) return 'bg-blue-100 text-blue-700'   // Top 50%
    if (percentile <= 0.75) return 'bg-orange-100 text-orange-700' // Top 75%
    return 'bg-red-100 text-red-700' // Bottom 25%
  }

  return (
    <div className="space-y-6">
      {/* Header */}
      <div>
        <h2 className="text-2xl font-semibold text-acb-900">Análisis de Four Factors</h2>
        <p className="text-acb-500 text-sm mt-1">
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
              onChange={(e) => setSelectedSeason(e.target.value === 'all' ? 'all' : parseInt(e.target.value))}
              className="px-3 py-2 border border-acb-200 rounded-md text-sm bg-white font-medium"
            >
              {availableSeasons.map(season => (
                <option key={season} value={season}>{season-1}-{String(season).slice(-2)}</option>
              ))}
              <option value="all">All Seasons</option>
            </select>
          </div>
        </div>
      </div>

      {/* Detailed Table */}
      <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
        <div className="overflow-x-auto">
          <table className="w-full">
            <thead>
              {/* Header row 1: Column groups */}
              <tr className="bg-acb-100 border-b border-acb-300">
                <th className="px-4 py-2 text-left text-xs font-semibold text-acb-700 uppercase tracking-wider w-8" rowSpan="2">#</th>
                <th className="px-4 py-2 text-left text-xs font-semibold text-acb-700 uppercase tracking-wider" rowSpan="2">Team</th>
                <th className="px-2 py-2 text-center text-xs font-semibold text-acb-700 uppercase tracking-wider border-r border-acb-300" colSpan="5">Ataque</th>
                <th className="px-2 py-2 text-center text-xs font-semibold text-acb-700 uppercase tracking-wider border-r border-acb-300" colSpan="5">Defensa</th>
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
      </div>

      {/* Four Factors Explanation */}
      <div className="bg-acb-50 rounded-lg border border-acb-200 p-4">
        <h3 className="text-sm font-semibold text-acb-900 mb-3 flex items-center gap-2">
          <Circle className="w-4 h-4" />
          Sobre los Four Factors
        </h3>
        <div className="text-sm text-acb-600 space-y-3">
          <p>
            <strong>Los Four Factors de Dean Oliver</strong> proporcionan un marco para entender qué determina que un equipo gane partidos.
            Creado por Dean Oliver, éste le otorga un peso relativo a cada uno de los cuatro factores que identifica:
          </p>
          <ul className="list-disc list-inside space-y-1">
            <li><strong>Tiro (40%)</strong>: Medido a través del porcentaje de Tiro Efectivo (Effective Field Goal; eFG%)</li>
            <li><strong>Pérdidas (25%)</strong>: Medido como el porcentaje de posesiones que acaban en pérdida de balón tanto en ataque como en defensa</li>
            <li><strong>Rebotes (20%)</strong>: Porcentajes de rebotes ofensivos y defensivos que el equipo es capaz de capturar sobre el total de oportunidades de rebote disponibles</li>
            <li><strong>Tiros Libres (15%)</strong>: Con qué frecuencia un equipo es capaz de ir a la línea de tiros libres y anotarlos</li>
          </ul>
        </div>
      </div>
    </div>
  )
}