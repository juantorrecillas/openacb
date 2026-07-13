import { useState, useMemo, useEffect } from 'react'
import PageHeader from '../components/PageHeader'

function seasonLabel(s) {
  return `${s - 1}-${String(s).slice(-2)}`
}

// ─── Color helpers ──────────────────────────────────────────────

function diffColor(v, max) {
  if (max === 0) return 'bg-acb-50'
  const intensity = Math.min(Math.abs(v) / max, 1)
  if (v > 0) {
    if (intensity > 0.6) return 'bg-positive-200 text-positive-900'
    if (intensity > 0.3) return 'bg-positive-100 text-positive-800'
    return 'bg-positive-50 text-positive-700'
  }
  if (v < 0) {
    if (intensity > 0.6) return 'bg-negative-200 text-negative-900'
    if (intensity > 0.3) return 'bg-negative-100 text-negative-800'
    return 'bg-negative-50 text-negative-700'
  }
  return 'bg-acb-50 text-acb-600'
}

// colorValue is always a signed deviation
function cellColor(colorValue, max) {
  return diffColor(colorValue, max)
}

function diffText(v) {
  if (v > 0) return `+${v.toFixed(1)}`
  return v.toFixed(1)
}

// ─── Segment heatmap cell ───────────────────────────────────────

// value = display value, colorValue = signed deviation used for coloring
function HeatCell({ value, colorValue, max, isDiff }) {
  const cls = cellColor(colorValue ?? value, max)
  const text = isDiff ? diffText(value) : value.toFixed(1)
  return (
    <td className={`px-2 py-1.5 text-center text-xs font-mono ${cls}`}>
      {text}
    </td>
  )
}

// ─── Main Component ─────────────────────────────────────────────

export default function TeamPace({ teams, loadTeamPaceForSeason, teamPaceCache, loadingTeamPace }) {
  const availableSeasons = useMemo(() => {
    return [...new Set(teams.map(t => t.season))].sort((a, b) => b - a)
  }, [teams])

  const [selectedSeason, setSelectedSeason] = useState(availableSeasons[0] || 2025)
  const [selectedTeam, setSelectedTeam] = useState(null)
  const [sortCol, setSortCol] = useState('total')
  const [sortDir, setSortDir] = useState('desc')
  const [viewMode, setViewMode] = useState('diff') // 'diff', 'scored', 'allowed'

  useEffect(() => {
    if (selectedSeason) loadTeamPaceForSeason(selectedSeason)
  }, [selectedSeason, loadTeamPaceForSeason])

  const paceData = useMemo(() => {
    return teamPaceCache[selectedSeason] || []
  }, [teamPaceCache, selectedSeason])

  const isLoading = loadingTeamPace[selectedSeason] || false

  // per-quarter league means for scored and allowed
  const quarterMeans = useMemo(() => {
    if (paceData.length === 0) return { scored: [0,0,0,0], allowed: [0,0,0,0] }
    const scored = [0,0,0,0], allowed = [0,0,0,0]
    paceData.forEach(t => {
      if (!t.quarters) return
      for (let q = 0; q < 4; q++) {
        scored[q] += (t.quarters.scored[q] || 0) / paceData.length
        allowed[q] += (t.quarters.allowed[q] || 0) / paceData.length
      }
    })
    return { scored, allowed }
  }, [paceData])

  // max deviation from the mean or zero, depending on the view
  const maxDiff = useMemo(() => {
    if (paceData.length === 0) return 3
    let max = 0
    paceData.forEach(t => {
      if (!t.quarters) return
      if (viewMode === 'diff') {
        t.quarters.diff.forEach(v => { if (Math.abs(v) > max) max = Math.abs(v) })
      } else {
        const arr = viewMode === 'scored' ? t.quarters.scored : t.quarters.allowed
        const means = viewMode === 'scored' ? quarterMeans.scored : quarterMeans.allowed
        arr.forEach((v, q) => {
          const dev = Math.abs(v - means[q])
          if (dev > max) max = dev
        })
      }
    })
    return Math.max(max, 0.5)
  }, [paceData, viewMode, quarterMeans])

  // sort teams
  const sortedTeams = useMemo(() => {
    if (paceData.length === 0) return []
    const arr = viewMode === 'diff' ? 'diff' : viewMode === 'scored' ? 'scored' : 'allowed'
    return [...paceData].sort((a, b) => {
      let va, vb
      if (sortCol === 'team') {
        return sortDir === 'asc'
          ? a.team.localeCompare(b.team)
          : b.team.localeCompare(a.team)
      }
      if (sortCol === 'total') {
        const sum = (t) => t.quarters[arr].reduce((s, v) => s + v, 0)
        va = sum(a); vb = sum(b)
      } else {
        const qi = Number(sortCol) - 1
        va = a.quarters[arr][qi] || 0
        vb = b.quarters[arr][qi] || 0
      }
      return sortDir === 'desc' ? vb - va : va - vb
    })
  }, [paceData, sortCol, sortDir, viewMode])

  const handleSort = (col) => {
    if (sortCol === col) {
      setSortDir(d => d === 'desc' ? 'asc' : 'desc')
    } else {
      setSortCol(col)
      setSortDir('desc')
    }
  }

  const sortIcon = (col) => {
    if (sortCol !== col) return ''
    return sortDir === 'desc' ? ' \u2193' : ' \u2191'
  }

  // selected team detail data
  const teamDetail = useMemo(() => {
    if (!selectedTeam) return null
    return paceData.find(t => t.team === selectedTeam) || null
  }, [paceData, selectedTeam])

  // segment heatmap always colors by diff (good/bad is relative to the opponent)
  const maxSegDiff = useMemo(() => {
    if (!teamDetail || !teamDetail.segments) return 2
    let max = 0
    teamDetail.segments.forEach(s => { if (Math.abs(s.diff) > max) max = Math.abs(s.diff) })
    return Math.max(max, 0.5)
  }, [teamDetail])

  return (
    <div className="app-page space-y-6">
      <PageHeader title="Ritmo y parciales" subtitle="Rendimiento por cuarto, parciales por segmento y anotación tras tiempo muerto" />

      {/* controls */}
      <div className="filter-panel flex flex-wrap items-end gap-4">
        <div className="flex flex-col gap-1">
          <label htmlFor="team-pace-season" className="field-label">Temporada</label>
          <select
            id="team-pace-season"
            value={selectedSeason}
            onChange={e => {
              setSelectedSeason(Number(e.target.value))
              setSelectedTeam(null)
            }}
            className="form-control"
          >
            {availableSeasons.map(s => (
              <option key={s} value={s}>{seasonLabel(s)}</option>
            ))}
          </select>
        </div>

        <div className="flex flex-col gap-1">
          <span className="field-label">Vista</span>
          <div className="segmented-control">
            {[
              { key: 'diff', label: 'Diferencial' },
              { key: 'scored', label: 'Anotado' },
              { key: 'allowed', label: 'Recibido' },
            ].map(opt => (
              <button
                key={opt.key}
                onClick={() => setViewMode(opt.key)}
                aria-pressed={viewMode === opt.key}
                className="segmented-option"
              >
                {opt.label}
              </button>
            ))}
          </div>
        </div>
      </div>

      {isLoading && (
        <div className="text-center py-12 text-acb-400">Cargando datos...</div>
      )}

      {/* quarter performance table */}
      {!isLoading && sortedTeams.length > 0 && (
        <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
          <div className="px-5 py-3 border-b border-acb-200">
            <h3 className="font-semibold text-acb-900">
              Rendimiento por cuarto
              <span className="font-normal text-acb-400 text-sm ml-2">
                · {viewMode === 'diff' ? 'Diferencial (pts/partido)' : viewMode === 'scored' ? 'Puntos anotados (media)' : 'Puntos recibidos (media)'}
              </span>
            </h3>
          </div>
          <div className="overflow-x-auto">
            <table className="data-table">
              <thead>
                <tr className="border-b border-acb-200 bg-acb-50">
                  <th
                    onClick={() => handleSort('team')}
                    className="data-table-head data-table-identity data-table-sticky data-table-sticky-head data-col-team bg-acb-50 cursor-pointer hover:text-acb-900"
                  >
                    Equipo{sortIcon('team')}
                  </th>
                  {[1, 2, 3, 4].map(q => (
                    <th
                      key={q}
                      onClick={() => handleSort(String(q))}
                      className="text-center py-2 px-4 text-xs font-semibold text-acb-600 uppercase cursor-pointer hover:text-acb-900 w-20"
                    >
                      Q{q}{sortIcon(String(q))}
                    </th>
                  ))}
                  <th
                    onClick={() => handleSort('total')}
                    className="text-center py-2 px-4 text-xs font-semibold text-acb-600 uppercase cursor-pointer hover:text-acb-900 w-20"
                  >
                    Total{sortIcon('total')}
                  </th>
                  <th className="text-center py-2 px-3 text-xs font-semibold text-acb-600 uppercase w-16">
                    Mejor
                  </th>
                  <th className="text-center py-2 px-3 text-xs font-semibold text-acb-600 uppercase w-16">
                    Peor
                  </th>
                </tr>
              </thead>
              <tbody className="divide-y divide-acb-100">
                {sortedTeams.map(t => {
                  const arr = viewMode === 'diff' ? t.quarters.diff
                    : viewMode === 'scored' ? t.quarters.scored
                    : t.quarters.allowed
                  const total = arr.reduce((s, v) => s + v, 0)
                  const isSelected = selectedTeam === t.team
                  const quarterOrder = arr
                    .map((value, index) => ({ value, quarter: index + 1 }))
                    .sort((a, b) => viewMode === 'allowed' ? a.value - b.value : b.value - a.value)
                  const bestQuarter = quarterOrder[0]?.quarter
                  const worstQuarter = quarterOrder[quarterOrder.length - 1]?.quarter
                  return (
                    <tr
                      key={t.team}
                      onClick={() => setSelectedTeam(isSelected ? null : t.team)}
                      className={`data-table-row cursor-pointer ${
                        isSelected ? 'bg-accent-50' : 'hover:bg-acb-50'
                      }`}
                    >
                      <td className="data-table-cell data-table-identity data-table-sticky data-col-team truncate">{t.team}</td>
                      {arr.map((v, qi) => {
                        let colorValue
                        if (viewMode === 'diff') colorValue = v
                        else if (viewMode === 'scored') colorValue = v - quarterMeans.scored[qi]
                        else colorValue = quarterMeans.allowed[qi] - v
                        return <HeatCell key={qi} value={v} colorValue={colorValue} max={maxDiff} isDiff={viewMode === 'diff'} />
                      })}
                      <td className={`px-2 py-1.5 text-center text-xs font-mono font-bold ${(() => {
                        if (viewMode === 'diff') return cellColor(total, maxDiff * 4)
                        const meanTotal = viewMode === 'scored'
                          ? quarterMeans.scored.reduce((a, b) => a + b, 0)
                          : quarterMeans.allowed.reduce((a, b) => a + b, 0)
                        const dev = viewMode === 'scored' ? total - meanTotal : meanTotal - total
                        return cellColor(dev, maxDiff * 4)
                      })()}`}>
                        {viewMode === 'diff' ? diffText(total) : total.toFixed(1)}
                      </td>
                      <td className="text-center text-xs">
                        <span className="inline-block px-1.5 py-0.5 rounded bg-positive-100 text-positive-700 font-medium">
                          Q{bestQuarter}
                        </span>
                      </td>
                      <td className="text-center text-xs">
                        <span className="inline-block px-1.5 py-0.5 rounded bg-negative-100 text-negative-700 font-medium">
                          Q{worstQuarter}
                        </span>
                      </td>
                    </tr>
                  )
                })}
              </tbody>
            </table>
          </div>
        </div>
      )}

      {/* team detail panel */}
      {teamDetail && (
        <div className="grid md:grid-cols-2 gap-6">
          {/* quarter bar chart */}
          <div className="bg-white rounded-lg border border-acb-200 p-4">
            <h3 className="font-semibold text-acb-900 mb-1">{teamDetail.team}</h3>
            <p className="text-xs text-acb-400 mb-4">{teamDetail.games} partidos - Anotado vs Recibido por cuarto</p>
            <div className="space-y-3">
              {[1, 2, 3, 4].map(q => {
                const scored = teamDetail.quarters.scored[q - 1]
                const allowed = teamDetail.quarters.allowed[q - 1]
                const maxVal = Math.max(
                  ...teamDetail.quarters.scored,
                  ...teamDetail.quarters.allowed,
                  1
                )
                const diff = teamDetail.quarters.diff[q - 1]
                return (
                  <div key={q}>
                    <div className="flex items-center justify-between mb-1">
                      <span className="text-xs font-semibold text-acb-600 w-8">Q{q}</span>
                      <span className={`text-xs font-mono font-bold ${diff > 0 ? 'text-positive' : diff < 0 ? 'text-negative' : 'text-acb-500'}`}>
                        {diffText(diff)}
                      </span>
                    </div>
                    <div className="flex gap-1">
                      <div className="flex-1">
                        <div className="h-4 bg-acb-100 rounded-sm overflow-hidden">
                          <div
                            className="h-full bg-positive-400 rounded-sm"
                            style={{ width: `${(scored / maxVal) * 100}%` }}
                          />
                        </div>
                        <div className="text-[10px] text-acb-500 mt-0.5">{scored.toFixed(1)} anot.</div>
                      </div>
                      <div className="flex-1">
                        <div className="h-4 bg-acb-100 rounded-sm overflow-hidden">
                          <div
                            className="h-full bg-negative-400 rounded-sm"
                            style={{ width: `${(allowed / maxVal) * 100}%` }}
                          />
                        </div>
                        <div className="text-[10px] text-acb-500 mt-0.5">{allowed.toFixed(1)} recib.</div>
                      </div>
                    </div>
                  </div>
                )
              })}
            </div>
          </div>

          {/* segment heatmap and post-timeout scoring */}
          <div className="space-y-6">
            {/* segment heatmap */}
            <div className="bg-white rounded-lg border border-acb-200 p-4">
              <h3 className="font-semibold text-acb-900 mb-1">Desglose por segmento</h3>
              <p className="text-xs text-acb-400 mb-3">
                {viewMode === 'diff' ? 'Diferencial' : viewMode === 'scored' ? 'Anotado' : 'Recibido'} por tramo de 2 minutos
              </p>
              <div className="overflow-x-auto">
                <table className="data-table text-xs">
                  <thead>
                    <tr className="border-b border-acb-200">
                      <th className="py-1 px-2 text-left text-acb-500 font-medium"></th>
                      {['0-2', '2-4', '4-6', '6-8', '8-10'].map(s => (
                        <th key={s} className="py-1 px-2 text-center text-acb-500 font-medium">{s}</th>
                      ))}
                    </tr>
                  </thead>
                  <tbody>
                    {[1, 2, 3, 4].map(q => {
                      const qSegments = (teamDetail.segments || []).filter(s => s.q === q)
                      return (
                        <tr key={q} className="border-b border-acb-50">
                          <td className="py-1 px-2 font-semibold text-acb-600">Q{q}</td>
                          {[1, 2, 3, 4, 5].map(seg => {
                            const s = qSegments.find(x => x.seg === seg)
                            const val = s
                              ? (viewMode === 'diff' ? s.diff : viewMode === 'scored' ? s.scored : s.allowed)
                              : 0
                            const diffVal = s ? s.diff : 0
                            return <HeatCell key={seg} value={val} colorValue={diffVal} max={maxSegDiff} isDiff={viewMode === 'diff'} />
                          })}
                        </tr>
                      )
                    })}
                  </tbody>
                </table>
              </div>
            </div>

            {/* post-timeout scoring */}
            {teamDetail.afterTimeout && (
              <div className="bg-white rounded-lg border border-acb-200 p-4">
                <h3 className="font-semibold text-acb-900 mb-1">Anotación tras tiempo muerto</h3>
                <p className="text-xs text-acb-400 mb-3">Siguiente anotación del equipo durante los 30 segundos posteriores; no representa una posesión</p>
                <div className="grid grid-cols-3 gap-4 text-center">
                  <div>
                    <div className="text-2xl font-bold text-acb-900">{teamDetail.afterTimeout.ppp}</div>
                    <div className="text-xs text-acb-500">Pts por tiempo muerto</div>
                  </div>
                  <div>
                    <div className="text-2xl font-bold text-acb-900">{teamDetail.afterTimeout.scoringPct}%</div>
                    <div className="text-xs text-acb-500">% con anotación en 30 s</div>
                  </div>
                  <div>
                    <div className="text-2xl font-bold text-acb-400">{teamDetail.afterTimeout.leaguePpp}</div>
                    <div className="text-xs text-acb-500">Media liga</div>
                  </div>
                </div>
                <div className="mt-2 text-[10px] text-acb-400 text-center">
                  {teamDetail.afterTimeout.timeouts} tiempos muertos analizados
                </div>
              </div>
            )}
          </div>
        </div>
      )}

      {/* empty state */}
      {!isLoading && paceData.length === 0 && (
        <div className="text-center py-12 text-acb-400">
          No hay datos de ritmo para esta temporada
        </div>
      )}
    </div>
  )
}
