import { useState, useMemo, useEffect, useRef } from 'react'
import { ArrowUp, ArrowDown, Loader2 } from 'lucide-react'
import TeamPace from './TeamPace'

// ─── Helpers ──────────────────────────────────────────────────

function seasonLabel(s) {
  return `${s - 1}-${String(s).slice(-2)}`
}

function formatClock(seconds) {
  const q = seconds < 2400
    ? Math.floor(seconds / 600) + 1
    : 5 + Math.floor((seconds - 2400) / 300)
  let inQuarter
  if (q <= 4) {
    inQuarter = 600 - (seconds - (q - 1) * 600)
  } else {
    inQuarter = 300 - (seconds - 2400 - (q - 5) * 300)
  }
  const m = Math.floor(inQuarter / 60)
  const s = inQuarter % 60
  const qLabel = q <= 4 ? `Q${q}` : `OT${q - 4}`
  return `${qLabel} ${m}:${String(s).padStart(2, '0')}`
}

function typeLabel(type) {
  if (type === '3p') return 'Triple'
  if (type === '2p') return 'Canasta de 2'
  if (type === 'ft') return 'Tiro libre'
  if (type === 'to') return 'Tiempo muerto'
  return type
}

// ─── Clutch Teams View ────────────────────────────────────────

function getRankColor(rank, total) {
  if (!rank || !total) return 'bg-acb-100 text-acb-600'
  const pct = rank / total
  if (pct <= 0.25) return 'bg-positive-100 text-positive-700'
  if (pct <= 0.5)  return 'bg-info-100 text-info-700'
  if (pct <= 0.75) return 'bg-info-100 text-info-600'
  return 'bg-negative-100 text-negative-700'
}

function ClutchTeamsView({ tabBar, teams, selectedSeason, setSelectedSeason, availableSeasons, clutchCache, loadingClutch }) {
  const [sortCol, setSortCol] = useState('plusMinus')
  const [sortDir, setSortDir] = useState('desc')

  const clutchData = clutchCache[selectedSeason] || null
  const isLoading  = loadingClutch[selectedSeason] || false

  const enriched = useMemo(() => {
    const raw = clutchData?.teams || []
    const withDerived = raw.map(t => {
      const fga    = (t.fg2Apg || 0) + (t.fg3Apg || 0)
      const tsPct  = fga + (t.ftApg || 0) > 0
        ? Math.round(t.ptsScoredAvg / (2 * (fga + 0.44 * (t.ftApg || 0))) * 1000) / 10
        : null
      const fg3Rate = fga > 0 ? Math.round((t.fg3Apg || 0) / fga * 1000) / 10 : null
      const winPct  = (t.games || 0) > 0 ? t.wins / t.games : 0
      return { ...t, tsPct, fg3Rate, winPct }
    })
    const copy = withDerived.map(t => ({ ...t }))
    const rankDesc = ['ptsScoredAvg','efgPct','tsPct','fg2Pct','fg3Pct','ftPct','plusMinus','winPct']
    rankDesc.forEach(key => {
      const s = [...copy].filter(t => t[key] != null).sort((a, b) => (b[key] || 0) - (a[key] || 0))
      s.forEach((t, i) => { const o = copy.find(x => x.team === t.team); if (o) o[`${key}Rank`] = i + 1 })
    })
    const s = [...copy].filter(t => t.ptsAllowedAvg != null).sort((a, b) => (a.ptsAllowedAvg || 0) - (b.ptsAllowedAvg || 0))
    s.forEach((t, i) => { const o = copy.find(x => x.team === t.team); if (o) o.ptsAllowedAvgRank = i + 1 })
    return copy
  }, [clutchData])

  const sorted = useMemo(() => {
    return [...enriched].sort((a, b) => {
      const av = a[sortCol] ?? (sortDir === 'desc' ? -Infinity : Infinity)
      const bv = b[sortCol] ?? (sortDir === 'desc' ? -Infinity : Infinity)
      return sortDir === 'desc' ? bv - av : av - bv
    })
  }, [enriched, sortCol, sortDir])

  const n = sorted.length
  const handleSort = col => {
    if (sortCol === col) setSortDir(d => d === 'desc' ? 'asc' : 'desc')
    else { setSortCol(col); setSortDir('desc') }
  }

  const sl = s => `${s - 1}-${String(s).slice(-2)}`
  const fmt = (v, pct) => v == null || isNaN(v) ? '-' : pct ? `${Number(v).toFixed(1)}%` : Number(v).toFixed(1)
  const thCls = key => `px-2 py-3 text-center text-xs font-semibold uppercase tracking-wider cursor-pointer hover:bg-acb-100 whitespace-nowrap select-none ${sortCol === key ? 'bg-acb-100 text-acb-800' : 'text-acb-600'}`
  const sortIcon = key => sortCol === key
    ? (sortDir === 'desc' ? <ArrowDown className="inline w-3 h-3 ml-0.5" /> : <ArrowUp className="inline w-3 h-3 ml-0.5" />)
    : <span className="ml-0.5 opacity-20">↕</span>

  const StatCell = ({ t, k, pct = false, signed = false }) => {
    const v = t[k]
    const rank = t[`${k}Rank`]
    let display, colorClass = 'text-acb-700'
    if (v == null || isNaN(v)) display = '-'
    else if (signed) { display = v > 0 ? `+${v.toFixed(1)}` : v.toFixed(1); colorClass = v > 0 ? 'text-green-700' : v < 0 ? 'text-red-600' : 'text-acb-700' }
    else display = pct ? `${Number(v).toFixed(1)}%` : Number(v).toFixed(1)
    return (
      <td className={`px-2 py-3 text-sm text-center ${sortCol === k ? 'bg-acb-50/60' : ''}`}>
        <div className="flex flex-col items-center gap-1">
          <span className={`font-mono ${colorClass}`}>{display}</span>
          {rank != null && <span className={`text-xs px-1.5 py-0.5 rounded ${getRankColor(rank, n)}`}>#{rank}</span>}
        </div>
      </td>
    )
  }

  return (
    <div className="space-y-6">
      {tabBar('clutch')}

      <div>
        <h2 className="text-2xl font-semibold text-acb-900">Clutch por Equipo</h2>
        <p className="text-acb-500 text-sm mt-1">
          Últimos 5 minutos con diferencia ≤ 5 pts (Q4 o prórroga) · {sl(selectedSeason)}
        </p>
      </div>

      <div className="flex flex-col gap-1 w-fit">
        <label className="text-xs text-acb-500 font-medium">Temporada</label>
        <select
          value={selectedSeason}
          onChange={e => setSelectedSeason(Number(e.target.value))}
          className="px-3 py-2.5 border border-acb-200 rounded-lg text-sm bg-white"
        >
          {availableSeasons.map(s => <option key={s} value={s}>{sl(s)}</option>)}
        </select>
      </div>

      {isLoading ? (
        <div className="flex items-center justify-center py-16 text-acb-400">
          <Loader2 className="w-5 h-5 animate-spin mr-2" />Cargando…
        </div>
      ) : !clutchData ? (
        <div className="text-center py-12 text-acb-400">No hay datos disponibles.</div>
      ) : (
        <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
          <div className="overflow-x-auto">
            <table className="w-full text-sm">
              <thead>
                <tr className="bg-acb-100 border-b border-acb-300">
                  <th className="px-4 py-2 text-left text-xs font-semibold text-acb-700 uppercase tracking-wider" rowSpan="2">Equipo</th>
                  <th className="px-2 py-2 text-center text-xs font-semibold text-acb-700 uppercase tracking-wider" rowSpan="2">PJ</th>
                  <th className="px-2 py-2 text-center text-xs font-semibold text-acb-700 uppercase tracking-wider border-l border-acb-300" colSpan="4">Marcador</th>
                  <th className="px-2 py-2 text-center text-xs font-semibold text-acb-700 uppercase tracking-wider border-l border-acb-300" colSpan="6">Tiro</th>
                </tr>
                <tr className="bg-acb-50 border-b border-acb-200">
                  <th className={`${thCls('winPct')} border-l border-acb-200`} onClick={() => handleSort('winPct')}>W-L {sortIcon('winPct')}</th>
                  <th className={thCls('plusMinus')} onClick={() => handleSort('plusMinus')}>+/- {sortIcon('plusMinus')}</th>
                  <th className={thCls('ptsScoredAvg')} onClick={() => handleSort('ptsScoredAvg')}>Pts/G {sortIcon('ptsScoredAvg')}</th>
                  <th className={thCls('ptsAllowedAvg')} onClick={() => handleSort('ptsAllowedAvg')}>Pts Riv/G {sortIcon('ptsAllowedAvg')}</th>
                  <th className={`${thCls('efgPct')} border-l border-acb-200`} onClick={() => handleSort('efgPct')}>eFG% {sortIcon('efgPct')}</th>
                  <th className={thCls('tsPct')} onClick={() => handleSort('tsPct')}>TS% {sortIcon('tsPct')}</th>
                  <th className={thCls('fg2Pct')} onClick={() => handleSort('fg2Pct')}>T2% {sortIcon('fg2Pct')}</th>
                  <th className={thCls('fg3Pct')} onClick={() => handleSort('fg3Pct')}>3P% {sortIcon('fg3Pct')}</th>
                  <th className={thCls('ftPct')} onClick={() => handleSort('ftPct')}>TL% {sortIcon('ftPct')}</th>
                  <th className={thCls('fg3Rate')} onClick={() => handleSort('fg3Rate')}>3PAr {sortIcon('fg3Rate')}</th>
                </tr>
              </thead>
              <tbody className="divide-y divide-acb-100">
                {sorted.map(t => (
                  <tr key={t.team} className="border-b border-acb-100 hover:bg-acb-50 transition-colors">
                    <td className="px-4 py-3 text-sm font-medium text-acb-900 whitespace-nowrap">{t.team}</td>
                    <td className="px-2 py-3 text-sm text-center font-mono text-acb-600">{t.games}</td>
                    <td className={`px-2 py-3 text-sm text-center border-l border-acb-100 ${sortCol === 'winPct' ? 'bg-acb-50/60' : ''}`}>
                      <div className="flex flex-col items-center gap-1">
                        <span className="font-mono text-acb-700">{t.wins}-{t.losses}</span>
                        {t.winPctRank != null && <span className={`text-xs px-1.5 py-0.5 rounded ${getRankColor(t.winPctRank, n)}`}>#{t.winPctRank}</span>}
                      </div>
                    </td>
                    <StatCell t={t} k="plusMinus" signed />
                    <StatCell t={t} k="ptsScoredAvg" />
                    <StatCell t={t} k="ptsAllowedAvg" />
                    <StatCell t={t} k="efgPct" pct />
                    <StatCell t={t} k="tsPct" pct />
                    <StatCell t={t} k="fg2Pct" pct />
                    <StatCell t={t} k="fg3Pct" pct />
                    <StatCell t={t} k="ftPct" pct />
                    <td className={`px-2 py-3 text-sm text-center ${sortCol === 'fg3Rate' ? 'bg-acb-50/60' : ''}`}>
                      <span className="font-mono text-acb-700">{fmt(t.fg3Rate, true)}</span>
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
          <div className="px-4 py-2 border-t border-acb-100 bg-acb-50 text-xs text-acb-400">
            {n} equipos · Stats por partido clutch · {sl(selectedSeason)}
          </div>
        </div>
      )}
    </div>
  )
}

// ─── Main Component ───────────────────────────────────────────

export default function GameFlow({ teams, loadGameFlowForSeason, gameFlowCache, loadingGameFlow, loadTeamPaceForSeason, teamPaceCache, loadingTeamPace, loadClutchForSeason, clutchCache, loadingClutch }) {
  const [view, setView] = useState('gameflow')

  const availableSeasons = useMemo(() => {
    return [...new Set(teams.map(t => t.season))].sort((a, b) => b - a)
  }, [teams])

  const [selectedSeason, setSelectedSeason] = useState(availableSeasons[0] || 2025)
  const [selectedGame, setSelectedGame] = useState(null)
  const [hoveredEvent, setHoveredEvent] = useState(null)
  const svgRef = useRef(null)
  const tooltipRef = useRef(null)

  useEffect(() => {
    if (selectedSeason) loadGameFlowForSeason(selectedSeason)
  }, [selectedSeason, loadGameFlowForSeason])

  useEffect(() => {
    if (selectedSeason && view === 'clutch') loadClutchForSeason(selectedSeason)
  }, [selectedSeason, view, loadClutchForSeason])

  const games = useMemo(() => {
    return gameFlowCache[selectedSeason] || []
  }, [gameFlowCache, selectedSeason])

  const isLoading = loadingGameFlow[selectedSeason] || false

  // Group games by jornada
  const jornadas = useMemo(() => {
    const map = new Map()
    games.forEach(g => {
      const j = g.jornada
      if (!map.has(j)) map.set(j, [])
      map.get(j).push(g)
    })
    return [...map.entries()].sort((a, b) => a[0] - b[0])
  }, [games])

  const [selectedJornada, setSelectedJornada] = useState(null)

  // Auto-select latest jornada when games load
  useEffect(() => {
    if (jornadas.length > 0) {
      const latest = jornadas[jornadas.length - 1][0]
      setSelectedJornada(latest)
      setSelectedGame(null)
    }
  }, [jornadas])

  const jornadaGames = useMemo(() => {
    if (selectedJornada == null) return []
    const entry = jornadas.find(([j]) => j === selectedJornada)
    return entry ? entry[1] : []
  }, [jornadas, selectedJornada])

  // Current game data
  const game = selectedGame
    ? games.find(g => g.id === selectedGame)
    : null

  // ─── Chart dimensions ─────────────────────────────────────────

  const margin = { top: 24, right: 20, bottom: 40, left: 48 }
  const chartWidth = 800
  const chartHeight = 300
  const innerW = chartWidth - margin.left - margin.right
  const innerH = chartHeight - margin.top - margin.bottom

  // ─── Chart data ───────────────────────────────────────────────

  const chartData = useMemo(() => {
    if (!game) return null

    const events = game.events || []
    const maxPeriod = game.maxPeriod || 4

    // Total game time in seconds
    const totalTime = maxPeriod <= 4
      ? 2400
      : 2400 + (maxPeriod - 4) * 300

    // Build margin curve from scoring events
    const points = [{ t: 0, margin: 0, sl: 0, sv: 0 }]

    let lastSl = 0, lastSv = 0
    events.forEach(e => {
      if (e.sl != null && e.sv != null && (e.sl !== lastSl || e.sv !== lastSv)) {
        points.push({ t: e.t, margin: e.sl - e.sv, sl: e.sl, sv: e.sv })
        lastSl = e.sl
        lastSv = e.sv
      }
    })

    // Add final point at game end
    points.push({ t: totalTime, margin: game.scoreL - game.scoreV, sl: game.scoreL, sv: game.scoreV })

    // Max absolute margin for Y scale
    const maxAbsMargin = Math.max(
      5,
      Math.max(...points.map(p => Math.abs(p.margin)))
    )

    // Quarter boundaries
    const periodBreaks = []
    for (let p = 1; p <= maxPeriod; p++) {
      if (p <= 4) periodBreaks.push(p * 600)
      else periodBreaks.push(2400 + (p - 4) * 300)
    }
    periodBreaks.pop()

    // Scale functions
    const xScale = (t) => (t / totalTime) * innerW
    const yScale = (m) => innerH / 2 - (m / maxAbsMargin) * (innerH / 2)

    // Build SVG path for margin line
    let linePath = ''
    points.forEach((p, i) => {
      const x = xScale(p.t)
      const y = yScale(p.margin)
      linePath += i === 0 ? `M${x},${y}` : `L${x},${y}`
    })

    // Build area paths (positive area = green, negative area = red)
    const zeroY = yScale(0)
    let posAreaPath = `M0,${zeroY}`
    let negAreaPath = `M0,${zeroY}`

    for (let i = 0; i < points.length; i++) {
      const p = points[i]
      const x = xScale(p.t)
      const y = yScale(p.margin)

      // Insert zero-crossing point when margin changes sign
      if (i > 0) {
        const prev = points[i - 1]
        if ((prev.margin > 0 && p.margin < 0) || (prev.margin < 0 && p.margin > 0)) {
          const r = Math.abs(prev.margin) / (Math.abs(prev.margin) + Math.abs(p.margin))
          const crossX = xScale(prev.t + (p.t - prev.t) * r)
          posAreaPath += `L${crossX.toFixed(1)},${zeroY}`
          negAreaPath += `L${crossX.toFixed(1)},${zeroY}`
        }
      }

      if (p.margin >= 0) {
        posAreaPath += `L${x},${y}`
        negAreaPath += `L${x},${zeroY}`
      } else {
        posAreaPath += `L${x},${zeroY}`
        negAreaPath += `L${x},${y}`
      }
    }

    const lastX = xScale(totalTime)
    posAreaPath += `L${lastX},${zeroY}Z`
    negAreaPath += `L${lastX},${zeroY}Z`

    // Scoring events for dots
    const scoringDots = events
      .filter(e => ['2p', '3p', 'ft'].includes(e.type) && e.sl != null)
      .map(e => ({
        ...e,
        x: xScale(e.t),
        y: yScale(e.sl - e.sv),
        margin: e.sl - e.sv,
      }))

    // Timeout markers
    const timeouts = events
      .filter(e => e.type === 'to')
      .map(e => ({
        ...e,
        x: xScale(e.t),
      }))

    // Run highlight bands (vertical shaded rectangles)
    const runBands = (game.runs || []).map(run => ({
      x: xScale(run.tStart),
      width: xScale(run.tEnd) - xScale(run.tStart),
      team: run.team,
    }))

    return {
      points, totalTime, maxAbsMargin, periodBreaks,
      xScale, yScale, zeroY, linePath, posAreaPath, negAreaPath,
      scoringDots, timeouts, runBands, lastX,
    }
  }, [game, innerW, innerH])

  // ─── Tooltip positioning ──────────────────────────────────────

  const handleDotHover = (e, dot) => {
    if (!svgRef.current) return
    const rect = svgRef.current.getBoundingClientRect()
    const x = margin.left + dot.x
    const y = margin.top + dot.y
    setHoveredEvent({
      ...dot,
      screenX: rect.left + x * (rect.width / chartWidth),
      screenY: rect.top + y * (rect.height / chartHeight),
    })
  }

  // ─── Y-axis ticks ─────────────────────────────────────────────

  const yTicks = useMemo(() => {
    if (!chartData) return []
    const max = chartData.maxAbsMargin
    const step = max <= 10 ? 5 : max <= 20 ? 5 : 10
    const ticks = [0]
    for (let v = step; v <= max; v += step) {
      ticks.push(v)
      ticks.push(-v)
    }
    return ticks
  }, [chartData])

  // ─── Render ───────────────────────────────────────────────────

  const tabBar = (active) => (
    <div className="flex gap-2 flex-wrap">
      <button onClick={() => setView('gameflow')} className={`px-4 py-1.5 rounded-full text-sm font-medium ${active === 'gameflow' ? 'bg-acb-900 text-white' : 'border border-acb-200 text-acb-500 hover:bg-acb-50'}`}>Flujo de Partido</button>
      <button onClick={() => setView('teampace')} className={`px-4 py-1.5 rounded-full text-sm font-medium ${active === 'teampace' ? 'bg-acb-900 text-white' : 'border border-acb-200 text-acb-500 hover:bg-acb-50'}`}>Rendimiento por Cuarto</button>
      <button onClick={() => setView('clutch')}   className={`px-4 py-1.5 rounded-full text-sm font-medium ${active === 'clutch'   ? 'bg-acb-900 text-white' : 'border border-acb-200 text-acb-500 hover:bg-acb-50'}`}>Clutch</button>
    </div>
  )

  if (view === 'teampace') {
    return (
      <div className="space-y-6">
        {tabBar('teampace')}
        <TeamPace
          teams={teams}
          loadTeamPaceForSeason={loadTeamPaceForSeason}
          teamPaceCache={teamPaceCache}
          loadingTeamPace={loadingTeamPace}
        />
      </div>
    )
  }

  if (view === 'clutch') {
    return (
      <ClutchTeamsView
        tabBar={tabBar}
        teams={teams}
        selectedSeason={selectedSeason}
        setSelectedSeason={setSelectedSeason}
        availableSeasons={availableSeasons}
        clutchCache={clutchCache}
        loadingClutch={loadingClutch}
      />
    )
  }

  return (
    <div className="space-y-6">
      {/* Tab switcher */}
      {tabBar('gameflow')}

      {/* Header */}
      <div>
        <h2 className="text-2xl font-semibold text-acb-900">Flujo de Partido</h2>
        <p className="text-acb-500 text-sm mt-1">
          Visualiza la evolución del marcador jugada a jugada en cada partido
        </p>
      </div>

      {/* Season selector */}
      <div className="flex flex-wrap items-end gap-3">
        <div className="flex flex-col gap-1">
          <label className="text-xs text-acb-500 font-medium">Temporada</label>
          <select
            value={selectedSeason}
            onChange={e => {
              setSelectedSeason(Number(e.target.value))
              setSelectedGame(null)
              setSelectedJornada(null)
            }}
            className="px-3 py-2.5 border border-acb-200 rounded-lg text-sm bg-white"
          >
            {availableSeasons.map(s => (
              <option key={s} value={s}>{seasonLabel(s)}</option>
            ))}
          </select>
        </div>

        {jornadas.length > 0 && (
          <div className="flex flex-col gap-1">
            <label className="text-xs text-acb-500 font-medium">Jornada</label>
            <select
              value={selectedJornada ?? ''}
              onChange={e => {
                setSelectedJornada(Number(e.target.value))
                setSelectedGame(null)
              }}
              className="px-3 py-2.5 border border-acb-200 rounded-lg text-sm bg-white"
            >
              {jornadas.map(([j]) => (
                <option key={j} value={j}>Jornada {j}</option>
              ))}
            </select>
          </div>
        )}
      </div>

      {/* Loading */}
      {isLoading && (
        <div className="text-center py-12 text-acb-400">Cargando datos...</div>
      )}

      {/* Game cards */}
      {!isLoading && jornadaGames.length > 0 && (
        <div className="grid grid-cols-2 sm:grid-cols-3 md:grid-cols-4 lg:grid-cols-5 gap-3">
          {jornadaGames.map(g => {
            const isSelected = selectedGame === g.id
            const diff = g.scoreL - g.scoreV
            return (
              <button
                key={g.id}
                onClick={() => setSelectedGame(isSelected ? null : g.id)}
                className={`text-left p-3 rounded-lg border transition-all ${
                  isSelected
                    ? 'border-accent-400 bg-accent-50 ring-1 ring-accent-300'
                    : 'border-acb-200 bg-white hover:border-acb-300'
                }`}
              >
                <div className="text-xs text-acb-400 mb-1">J{g.jornada}</div>
                <div className="text-sm font-medium text-acb-900 truncate">{g.local}</div>
                <div className="text-sm text-acb-500 truncate">{g.visitor}</div>
                <div className="mt-1.5 flex items-center gap-1.5">
                  <span className={`text-base font-bold ${diff > 0 ? 'text-positive' : diff < 0 ? 'text-negative' : 'text-acb-700'}`}>
                    {g.scoreL}
                  </span>
                  <span className="text-acb-300">-</span>
                  <span className={`text-base font-bold ${diff < 0 ? 'text-positive' : diff > 0 ? 'text-negative' : 'text-acb-700'}`}>
                    {g.scoreV}
                  </span>
                </div>
              </button>
            )
          })}
        </div>
      )}

      {/* No data */}
      {!isLoading && games.length === 0 && !isLoading && (
        <div className="text-center py-12 text-acb-400">
          No hay datos de flujo de partido para esta temporada
        </div>
      )}

      {/* Chart */}
      {game && chartData && (
        <div className="bg-white rounded-lg border border-acb-200 p-4">
          {/* Game header */}
          <div className="flex items-center justify-between mb-4">
            <div>
              <h3 className="font-semibold text-acb-900">
                {game.local} {game.scoreL} - {game.scoreV} {game.visitor}
              </h3>
              <p className="text-xs text-acb-400">Jornada {game.jornada} - {seasonLabel(selectedSeason)}</p>
            </div>
            <div className="flex items-center gap-4 text-xs">
              <div className="flex items-center gap-1.5">
                <span className="w-3 h-3 rounded-full bg-green-500/30 border border-green-500"></span>
                <span className="text-acb-500">{game.local} anota</span>
              </div>
              <div className="flex items-center gap-1.5">
                <span className="w-3 h-3 rounded-full bg-red-500/30 border border-red-500"></span>
                <span className="text-acb-500">{game.visitor} anota</span>
              </div>
            </div>
          </div>

          {/* SVG Chart */}
          <div className="overflow-x-auto">
            <svg
              ref={svgRef}
              viewBox={`0 0 ${chartWidth} ${chartHeight}`}
              className="w-full min-w-[600px]"
              onMouseLeave={() => setHoveredEvent(null)}
            >
              <g transform={`translate(${margin.left},${margin.top})`}>
                {/* Areas */}
                <path d={chartData.posAreaPath} fill="rgba(34,197,94,0.12)" />
                <path d={chartData.negAreaPath} fill="rgba(239,68,68,0.12)" />

                {/* Run highlight bands */}
                {chartData.runBands.map((rb, i) => (
                  <rect
                    key={`run-${i}`}
                    x={rb.x}
                    y={0}
                    width={rb.width}
                    height={innerH}
                    fill={rb.team === 'L' ? 'rgba(22,163,74,0.12)' : 'rgba(220,38,38,0.12)'}
                    stroke="none"
                  />
                ))}

                {/* Quarter separators */}
                {chartData.periodBreaks.map((t, i) => {
                  const x = chartData.xScale(t)
                  return (
                    <g key={`q-${i}`}>
                      <line
                        x1={x} y1={0} x2={x} y2={innerH}
                        stroke="#bcccdc" strokeWidth={1} strokeDasharray="4,3"
                      />
                    </g>
                  )
                })}

                {/* Zero line */}
                <line
                  x1={0} y1={chartData.zeroY} x2={innerW} y2={chartData.zeroY}
                  stroke="#829ab1" strokeWidth={1}
                />

                {/* Y axis labels */}
                {yTicks.map(v => (
                  <text
                    key={`yl-${v}`}
                    x={-8} y={chartData.yScale(v)}
                    textAnchor="end"
                    dominantBaseline="central"
                    className="fill-acb-400 text-[10px] font-mono"
                  >
                    {v > 0 ? `+${v}` : v}
                  </text>
                ))}

                {/* Margin line */}
                <path
                  d={chartData.linePath}
                  fill="none"
                  stroke="#334e68"
                  strokeWidth={1}
                  strokeLinejoin="round"
                />

                {/* Scoring dots (interactive) */}
                {chartData.scoringDots.map((dot, i) => {
                  const color = dot.team === 'L' ? '#2aa867' : '#dd415d'
                  const isHovered = hoveredEvent && hoveredEvent.t === dot.t && hoveredEvent.sl === dot.sl
                  return (
                    <circle
                      key={`dot-${i}`}
                      cx={dot.x}
                      cy={dot.y}
                      r={isHovered ? 5 : dot.type === '3p' ? 3.5 : 2.5}
                      fill={color}
                      stroke="white"
                      strokeWidth={1}
                      opacity={isHovered ? 1 : 0.7}
                      className="cursor-pointer"
                      onMouseEnter={(e) => handleDotHover(e, dot)}
                      onMouseLeave={() => setHoveredEvent(null)}
                    />
                  )
                })}

                {/* X axis labels */}
                {(() => {
                  const maxP = game.maxPeriod || 4
                  const labels = []
                  for (let p = 1; p <= maxP; p++) {
                    let midT
                    if (p <= 4) midT = (p - 1) * 600 + 300
                    else midT = 2400 + (p - 5) * 300 + 150
                    labels.push({ t: midT, label: p <= 4 ? `${p}Q` : `OT${p - 4}` })
                  }
                  return labels.map(l => (
                    <text
                      key={l.label}
                      x={chartData.xScale(l.t)}
                      y={innerH + 28}
                      textAnchor="middle"
                      className="fill-acb-500 text-[11px] font-medium"
                    >
                      {l.label}
                    </text>
                  ))
                })()}
              </g>
            </svg>
          </div>

          {/* Tooltip */}
          {hoveredEvent && (
            <div
              ref={tooltipRef}
              className="fixed z-50 pointer-events-none bg-acb-900 text-white text-xs rounded-lg px-3 py-2 shadow-lg"
              style={{
                left: hoveredEvent.screenX,
                top: hoveredEvent.screenY - 60,
                transform: 'translateX(-50%)',
              }}
            >
              <div className="font-medium">{hoveredEvent.player} - {typeLabel(hoveredEvent.type)}</div>
              <div className="text-acb-300">
                {hoveredEvent.sl}-{hoveredEvent.sv}
                {' '}({hoveredEvent.margin > 0 ? '+' : ''}{hoveredEvent.margin})
                {' - '}{formatClock(hoveredEvent.t)}
              </div>
            </div>
          )}

          {/* Parciales destacados */}
          {game.runs && game.runs.length > 0 && (
            <div className="mt-4 border-t border-acb-100 pt-4">
              <h4 className="text-sm font-semibold text-acb-700 mb-2">Parciales destacados</h4>
              <div className="flex flex-wrap gap-2">
                {game.runs.map((run, i) => {
                  const teamName = run.team === 'L' ? game.local : game.visitor
                  const localPts = run.slEnd - run.slStart
                  const visitorPts = run.svEnd - run.svStart
                  const wonPts = run.team === 'L' ? localPts : visitorPts
                  const lostPts = run.team === 'L' ? visitorPts : localPts
                  const isLocal = run.team === 'L'
                  return (
                    <div
                      key={i}
                      className={`inline-flex items-center gap-2 px-3 py-1.5 rounded-full text-xs border ${
                        isLocal ? 'border-green-200 bg-green-50' : 'border-red-200 bg-red-50'
                      }`}
                    >
                      <span className={`font-semibold ${isLocal ? 'text-green-700' : 'text-red-700'}`}>{teamName}</span>
                      <span className={`font-bold ${isLocal ? 'text-green-800' : 'text-red-800'}`}>{wonPts}-{lostPts}</span>
                      <span className="text-acb-400">{formatClock(run.tStart)} → {formatClock(run.tEnd)}</span>
                    </div>
                  )
                })}
              </div>
            </div>
          )}

        </div>
      )}

      {/* Placeholder when no game selected */}
      {!isLoading && games.length > 0 && !game && (
        <div className="bg-white rounded-lg border border-acb-200 p-12 text-center text-acb-400">
          Selecciona un partido para ver el flujo del marcador
        </div>
      )}
    </div>
  )
}
