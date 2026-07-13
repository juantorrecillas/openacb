import { useState, useMemo } from 'react'
import { Circle, Download } from 'lucide-react'
import { downloadTableAsCsv } from '../utils/csvDownload'
import PageHeader from '../components/PageHeader'


export default function FourFactors({ teams }) {
  // get available seasons and default to most recent
  const availableSeasons = useMemo(() => {
    const seasons = [...new Set(teams.map(t => t.season))].sort((a, b) => b - a)
    return seasons
  }, [teams])

  const [selectedSeason, setSelectedSeason] = useState(availableSeasons[0] || 2025)
  const [sortKey, setSortKey] = useState('netRating')
  const [sortDir, setSortDir] = useState('desc')
  const [highlightTeam, setHighlightTeam] = useState(null)

  // filter teams by season
  const seasonFilteredTeams = useMemo(() => {
    if (selectedSeason === 'all') return teams
    return teams.filter(t => t.season === selectedSeason)
  }, [teams, selectedSeason])

  const teamsWithFourFactors = useMemo(() => {
    return seasonFilteredTeams.map(team => {
      const offensiveShooting = (team.efg || 0) * 100 // efg%
      const offensiveTurnovers = (team.tovRate || 0) * 100 // tov%
      const offensiveRebounding = (team.orbPct || 0) * 100 // orb%
      const offensiveFreeThrows = (team.ftRate || 0) * 100 // tiros libres anotados / fga
      const offensiveRating = team.ortg || 0 // offensive rating

      const defensiveShooting = (team.opp_efg || 0) * 100 // opponent efg%
      const defensiveTurnovers = (team.opp_tovRate || 0) * 100 // opponent tov%
      const defensiveRebounding = (team.drbPct || 0) * 100 // drb%
      const defensiveFreeThrows = (team.opp_ftRate || 0) * 100 // opponent free throws made / fga
      const defensiveRating = team.drtg || 0 // defensive rating

      // net rating
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
      { key: 'offensiveTurnovers', higherIsBetter: false },
      { key: 'offensiveRebounding', higherIsBetter: true },
      { key: 'offensiveFreeThrows', higherIsBetter: true },
      { key: 'offensiveRating', higherIsBetter: true },
      { key: 'defensiveShooting', higherIsBetter: false }, 
      { key: 'defensiveTurnovers', higherIsBetter: true }, 
      { key: 'defensiveRebounding', higherIsBetter: true },
      { key: 'defensiveFreeThrows', higherIsBetter: false },
      { key: 'defensiveRating', higherIsBetter: false }, 
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

  // sort teams
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

  const handleDownloadCsv = () => {
    const seasonStr = selectedSeason === 'all'
      ? 'todas-temporadas'
      : `${selectedSeason - 1}-${String(selectedSeason).slice(-2)}`
    const filename = `four-factors_${seasonStr}.csv`

    const cols = [
      { key: 'season', label: 'Temporada' },
      { key: 'team', label: 'Equipo' },
      { key: 'offensiveShooting', label: 'eFG% Atq' },
      { key: 'offensiveTurnovers', label: 'PER% Atq' },
      { key: 'offensiveRebounding', label: 'RO% Atq' },
      { key: 'offensiveFreeThrows', label: 'TLr Atq' },
      { key: 'offensiveRating', label: 'ORtg' },
      { key: 'defensiveShooting', label: 'eFG% Def' },
      { key: 'defensiveTurnovers', label: 'PER% Def' },
      { key: 'defensiveRebounding', label: 'RD% Def' },
      { key: 'defensiveFreeThrows', label: 'TLr Def' },
      { key: 'defensiveRating', label: 'DRtg' },
      { key: 'netRating', label: 'Net' },
    ]

    const fmtNum = v => v == null || isNaN(v) ? '' : Number(v).toFixed(1)
    const exportRows = sortedTeams.map(t => {
      const row = {
        season: `${t.season - 1}-${String(t.season).slice(-2)}`,
        team: t.team,
      }
      cols.slice(2).forEach(c => { row[c.key] = fmtNum(t[c.key]) })
      return row
    })

    downloadTableAsCsv(filename, exportRows, cols)
  }

  const getRankColor = (rank, totalTeams) => {
    if (!rank) return 'bg-acb-100 text-acb-600'
    const percentile = rank / totalTeams
    if (percentile <= 0.25) return 'bg-acb-800 text-white'
    if (percentile <= 0.5) return 'bg-acb-200 text-acb-800'
    if (percentile <= 0.75) return 'bg-acb-100 text-acb-700'
    return 'bg-acb-50 text-acb-500'
  }

  const rankedCell = (value, rank, totalTeams, className = '') => (
    <td className={`data-table-cell data-table-number data-col-number ${className}`}>
      <div className="data-table-value">
        <span>{formatValue(value, 1)}</span>
        <span className={`data-table-badge ${getRankColor(rank, totalTeams)}`}>#{rank}</span>
      </div>
    </td>
  )

  return (
    <div className="app-page space-y-6">
      <PageHeader
        title="Análisis de Four Factors"
        subtitle="Compara los cuatro factores ofensivos y defensivos de cada equipo"
        scope="Temporada completa · Liga regular y playoffs"
      />

      {/* controls */}
      <div className="filter-panel space-y-4">
        <div className="flex flex-wrap items-center justify-between gap-4">
          {/* season filter */}
          <div className="flex items-center gap-2">
            <label htmlFor="four-factors-season" className="field-label">Temporada</label>
            <select
              id="four-factors-season"
              value={selectedSeason}
              onChange={(e) => setSelectedSeason(e.target.value === 'all' ? 'all' : parseInt(e.target.value))}
              className="form-control font-medium"
            >
              {availableSeasons.map(season => (
                <option key={season} value={season}>{season-1}-{String(season).slice(-2)}</option>
              ))}
              <option value="all">Todas las temporadas</option>
            </select>
          </div>
          <button
            onClick={handleDownloadCsv}
            className="form-control-compact inline-flex items-center gap-1.5 text-acb-700 hover:bg-acb-50"
            title="Descargar la tabla actual como CSV"
          >
            <Download className="w-4 h-4" />
            Descargar CSV
          </button>
        </div>
      </div>

      {/* detailed table */}
      <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
        <div className="overflow-x-auto">
          <table className="data-table">
            <thead>
              {/* header row 1: column groups */}
              <tr className="bg-acb-100 border-b border-acb-300">
                <th className="data-table-head data-table-number data-table-sticky data-table-sticky-head data-col-rank bg-acb-100" rowSpan="2">#</th>
                <th className="data-table-head data-table-identity data-table-sticky-after-rank data-table-sticky-head data-col-team bg-acb-100" rowSpan="2">Equipo</th>
                {selectedSeason === 'all' && (
                  <th className="data-table-head text-left" rowSpan="2">Temporada</th>
                )}
                <th className="data-table-group border-r border-acb-300" colSpan="5">Ataque</th>
                <th className="data-table-group border-r border-acb-300" colSpan="5">Defensa</th>
                <th className="data-table-group" rowSpan="2">Neto</th>
              </tr>
              {/* header row 2: individual columns */}
              <tr className="bg-acb-50 border-b border-acb-200">
                <th className={`data-table-head data-table-number data-col-number cursor-pointer hover:bg-acb-100 ${
                  sortKey === 'offensiveShooting' ? 'bg-acb-100' : ''}`}
                    onClick={() => handleSort('offensiveShooting')} title="Effective Field Goal %">eFG%</th>
                <th className={`data-table-head data-table-number data-col-number cursor-pointer hover:bg-acb-100 ${
                  sortKey === 'offensiveTurnovers' ? 'bg-acb-100' : ''}`}
                    onClick={() => handleSort('offensiveTurnovers')} title="Pérdidas %">PER%</th>
                <th className={`data-table-head data-table-number data-col-number cursor-pointer hover:bg-acb-100 ${
                  sortKey === 'offensiveRebounding' ? 'bg-acb-100' : ''}`}
                    onClick={() => handleSort('offensiveRebounding')} title="Rebote Ofensivo %">RO%</th>
                <th className={`data-table-head data-table-number data-col-number cursor-pointer hover:bg-acb-100 ${
                  sortKey === 'offensiveFreeThrows' ? 'bg-acb-100' : ''}`}
                    onClick={() => handleSort('offensiveFreeThrows')} title="Tasa de Tiros Libres">TLr</th>
                <th className={`data-table-head data-table-number data-col-number cursor-pointer hover:bg-acb-100 border-r border-acb-300 ${
                  sortKey === 'offensiveRating' ? 'bg-acb-100' : ''}`}
                    onClick={() => handleSort('offensiveRating')} title="Offensive Rating">ORtg</th>
                <th className={`data-table-head data-table-number data-col-number cursor-pointer hover:bg-acb-100 ${
                  sortKey === 'defensiveShooting' ? 'bg-acb-100' : ''}`}
                    onClick={() => handleSort('defensiveShooting')} title="eFG% Rival">eFG%</th>
                <th className={`data-table-head data-table-number data-col-number cursor-pointer hover:bg-acb-100 ${
                  sortKey === 'defensiveTurnovers' ? 'bg-acb-100' : ''}`}
                    onClick={() => handleSort('defensiveTurnovers')} title="PER% Rival">PER%</th>
                <th className={`data-table-head data-table-number data-col-number cursor-pointer hover:bg-acb-100 ${
                  sortKey === 'defensiveRebounding' ? 'bg-acb-100' : ''}`}
                    onClick={() => handleSort('defensiveRebounding')} title="Rebote Defensivo %">RD%</th>
                <th className={`data-table-head data-table-number data-col-number cursor-pointer hover:bg-acb-100 ${
                  sortKey === 'defensiveFreeThrows' ? 'bg-acb-100' : ''}`}
                    onClick={() => handleSort('defensiveFreeThrows')} title="Tasa de Tiros Libres Rival">TLr</th>
                <th className={`data-table-head data-table-number data-col-number cursor-pointer hover:bg-acb-100 border-r border-acb-300 ${
                  sortKey === 'defensiveRating' ? 'bg-acb-100' : ''}`}
                    onClick={() => handleSort('defensiveRating')} title="Defensive Rating">DRtg</th>
              </tr>
            </thead>
            <tbody>
              {sortedTeams.map((team, i) => {
                const totalTeams = sortedTeams.length
                return (
                  <tr
                    key={`${team.team}-${team.season}-${team.competitionStage || 'all'}`}
                    className={`data-table-row border-b border-acb-100 ${
                      highlightTeam === team.team ? 'bg-accent-50' : ''
                    }`}
                    onMouseEnter={() => setHighlightTeam(team.team)}
                    onMouseLeave={() => setHighlightTeam(null)}
                  >
                    <td className="data-table-cell data-table-number data-table-sticky data-col-rank text-acb-400">{i + 1}</td>
                    <td className="data-table-cell data-table-identity data-table-sticky-after-rank data-col-team">{team.team}</td>
                    {selectedSeason === 'all' && (
                      <td className="data-table-cell text-left">{team.season - 1}-{String(team.season).slice(-2)}</td>
                    )}
                    {rankedCell(team.offensiveShooting, team.offensiveShootingRank, totalTeams)}
                    {rankedCell(team.offensiveTurnovers, team.offensiveTurnoversRank, totalTeams)}
                    {rankedCell(team.offensiveRebounding, team.offensiveReboundingRank, totalTeams)}
                    {rankedCell(team.offensiveFreeThrows, team.offensiveFreeThrowsRank, totalTeams)}
                    {rankedCell(team.offensiveRating, team.offensiveRatingRank, totalTeams, 'border-r border-acb-300 font-semibold')}
                    {rankedCell(team.defensiveShooting, team.defensiveShootingRank, totalTeams)}
                    {rankedCell(team.defensiveTurnovers, team.defensiveTurnoversRank, totalTeams)}
                    {rankedCell(team.defensiveRebounding, team.defensiveReboundingRank, totalTeams)}
                    {rankedCell(team.defensiveFreeThrows, team.defensiveFreeThrowsRank, totalTeams)}
                    {rankedCell(team.defensiveRating, team.defensiveRatingRank, totalTeams, 'border-r border-acb-300 font-semibold')}
                    {rankedCell(team.netRating, team.netRatingRank, totalTeams, 'font-semibold')}
                  </tr>
                )
              })}
            </tbody>
          </table>
        </div>
      </div>

      {/* four factors explanation */}
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
            <li><strong>Tiro (40%)</strong>: Porcentaje de Tiro Efectivo (Effective Field Goal; eFG%)</li>
            <li><strong>Pérdidas (25%)</strong>: Porcentaje de posesiones que acaban en pérdida de balón tanto en ataque como en defensa</li>
            <li><strong>Rebotes (20%)</strong>: Porcentajes de rebotes ofensivos y defensivos que el equipo captura sobre el total de rebotes potenciales disponibles</li>
            <li><strong>Tiros libres (15%)</strong>: Tiros libres anotados por tiro de campo intentado (FTM/FGA)</li>
          </ul>
        </div>
      </div>
    </div>
  )
}
