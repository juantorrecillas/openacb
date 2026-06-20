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
  const [viewTab, setViewTab] = useState('basico')

  const clutchData = clutchCache[selectedSeason] || null
  const isLoading  = loadingClutch[selectedSeason] || false

  const enriched = useMemo(() => {
    const raw = clutchData?.teams || []
    const withDerived = raw.map(t => {
      const fga     = (t.fg2Apg || 0) + (t.fg3Apg || 0)
      const tsPct   = fga + (t.ftApg || 0) > 0
        ? Math.round(t.ptsScoredAvg / (2 * (fga + 0.44 * (t.ftApg || 0))) * 1000) / 10
        : null
      const fg3Rate = fga > 0 ? Math.round((t.fg3Apg || 0) / fga * 1000) / 10 : null
      const winPct  = (t.games || 0) > 0 ? t.wins / t.games : 0
      const opp_fga = (t.opp_fg2Apg || 0) + (t.opp_fg3Apg || 0)
      const opp_tsPct = opp_fga + (t.opp_ftApg || 0) > 0
        ? Math.round((t.ptsAllowedAvg || 0) / (2 * (opp_fga + 0.44 * (t.opp_ftApg || 0))) * 1000) / 10
        : null
      const opp_fg3Rate = opp_fga > 0 ? Math.round((t.opp_fg3Apg || 0) / opp_fga * 1000) / 10 : null
      const opp_orbPct = (t.opp_orebAvg || 0) + (t.drebAvg || 0) > 0
        ? Math.round((t.opp_orebAvg || 0) / ((t.opp_orebAvg || 0) + (t.drebAvg || 0)) * 1000) / 10 : null
      const opp_drbPct = (t.opp_drebAvg || 0) + (t.orebAvg || 0) > 0
        ? Math.round((t.opp_drebAvg || 0) / ((t.opp_drebAvg || 0) + (t.orebAvg || 0)) * 1000) / 10 : null
      return { ...t, tsPct, fg3Rate, winPct, opp_tsPct, opp_fg3Rate, opp_orbPct, opp_drbPct }
    })
    const copy = withDerived.map(t => ({ ...t }))
    const rankDesc = [
      'ptsScoredAvg','fgPct','fg2Pct','fg3Pct','ftPct','efgPct','tsPct','fg3Rate',
      'plusMinus','winPct','netRtg','ortg','apg','spg','bpg',
      'orebAvg','drebAvg','rebAvg','orbPct','drbPct',
      'astRate','blkRate','stlRate','astToRatio',
    ]
    rankDesc.forEach(key => {
      const s = [...copy].filter(t => t[key] != null).sort((a, b) => (b[key] || 0) - (a[key] || 0))
      s.forEach((t, i) => { const o = copy.find(x => x.team === t.team); if (o) o[`${key}Rank`] = i + 1 })
    })
    const rankAsc = [
      'ptsAllowedAvg','drtg','topg','tovRate',
      'opp_fgPct','opp_fg2Pct','opp_fg3Pct','opp_ftPct','opp_efgPct','opp_tsPct',
      'opp_orebAvg','opp_drebAvg','opp_apg','opp_spg','opp_bpg','opp_topg',
      'opp_astRate','opp_stlRate','opp_blkRate','opp_tovRate','opp_astToRatio',
      'opp_orbPct','opp_drbPct',
    ]
    rankAsc.forEach(key => {
      const s = [...copy].filter(t => t[key] != null).sort((a, b) => (a[key] || 0) - (b[key] || 0))
      s.forEach((t, i) => { const o = copy.find(x => x.team === t.team); if (o) o[`${key}Rank`] = i + 1 })
    })
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
  const thCls = key => `data-table-head data-table-number data-col-number cursor-pointer hover:bg-acb-100 select-none ${sortCol === key ? 'bg-acb-100 text-acb-800' : 'text-acb-600'}`
  const sortIcon = key => sortCol === key
    ? (sortDir === 'desc' ? <ArrowDown className="inline w-3 h-3 ml-0.5" /> : <ArrowUp className="inline w-3 h-3 ml-0.5" />)
    : <span className="ml-0.5 opacity-20">↕</span>

  const StatCell = ({ t, k, pct = false, signed = false }) => {
    const v = t[k]
    const rank = t[`${k}Rank`]
    let display, colorClass = 'text-acb-700'
    if (v == null || isNaN(v)) display = '-'
    else if (signed) { display = v > 0 ? `+${v.toFixed(1)}` : v.toFixed(1); colorClass = v > 0 ? 'text-positive' : v < 0 ? 'text-negative' : 'text-acb-700' }
    else display = pct ? `${Number(v).toFixed(1)}%` : Number(v).toFixed(1)
    return (
      <td className={`data-table-cell data-table-number data-col-number ${sortCol === k ? 'bg-acb-50/60' : ''}`}>
        <div className="data-table-value">
          <span className={colorClass}>{display}</span>
          {rank != null && <span className={`data-table-badge ${getRankColor(rank, n)}`}>#{rank}</span>}
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

      <div className="flex items-center gap-1 bg-acb-100 rounded-md p-1 w-fit">
        {[['basico','Básico'],['avanzado','Avanzado'],['rival','Rival'],['rivalAvanzado','Riv. Avanzado']].map(([id, label]) => (
          <button key={id} onClick={() => {
            setViewTab(id)
            setSortCol(id === 'avanzado' ? 'netRtg' : id === 'rival' ? 'opp_efgPct' : id === 'rivalAvanzado' ? 'drtg' : 'plusMinus')
          }}
            className={`px-3 py-1.5 text-sm font-medium rounded transition-colors ${viewTab === id ? 'bg-white text-acb-900 shadow-sm' : 'text-acb-600 hover:text-acb-900'}`}>
            {label}
          </button>
        ))}
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
            <table className="data-table">
              <thead>
                <tr className="bg-acb-100 border-b border-acb-300">
                  <th className="data-table-head data-table-identity data-table-sticky data-table-sticky-head data-col-team bg-acb-100" rowSpan="2">Equipo</th>
                  <th className="data-table-head data-table-number data-col-games" rowSpan="2">PJ</th>
                  {viewTab === 'basico' && <>
                    <th className="data-table-group border-l border-acb-300" colSpan="4">Marcador</th>
                    <th className="data-table-group border-l border-acb-300" colSpan="5">Tiro</th>
                    <th className="data-table-group border-l border-acb-300" colSpan="3">Rebotes</th>
                    <th className="data-table-group border-l border-acb-300" colSpan="4">Otros</th>
                  </>}
                  {viewTab === 'avanzado' && <>
                    <th className="data-table-group border-l border-acb-300" colSpan="4">Rating</th>
                    <th className="data-table-group border-l border-acb-300" colSpan="3">Tiro</th>
                    <th className="data-table-group border-l border-acb-300" colSpan="2">Rebotes</th>
                    <th className="data-table-group border-l border-acb-300" colSpan="5">Ratios</th>
                  </>}
                  {viewTab === 'rival' && <>
                    <th className="data-table-group border-l border-acb-300" colSpan="1">Puntos</th>
                    <th className="data-table-group border-l border-acb-300" colSpan="5">Tiro</th>
                    <th className="data-table-group border-l border-acb-300" colSpan="2">Rebotes</th>
                    <th className="data-table-group border-l border-acb-300" colSpan="4">Otros</th>
                  </>}
                  {viewTab === 'rivalAvanzado' && <>
                    <th className="data-table-group border-l border-acb-300" colSpan="3">Rating</th>
                    <th className="data-table-group border-l border-acb-300" colSpan="3">Tiro</th>
                    <th className="data-table-group border-l border-acb-300" colSpan="2">Rebotes</th>
                    <th className="data-table-group border-l border-acb-300" colSpan="5">Ratios</th>
                  </>}
                </tr>
                <tr className="bg-acb-50 border-b border-acb-200">
                  {viewTab === 'basico' && <>
                    <th className={`${thCls('winPct')} border-l border-acb-200`} onClick={() => handleSort('winPct')}>V-D {sortIcon('winPct')}</th>
                    <th className={thCls('plusMinus')} onClick={() => handleSort('plusMinus')}>+/- {sortIcon('plusMinus')}</th>
                    <th className={thCls('ptsScoredAvg')} onClick={() => handleSort('ptsScoredAvg')}>Pts/G {sortIcon('ptsScoredAvg')}</th>
                    <th className={thCls('ptsAllowedAvg')} onClick={() => handleSort('ptsAllowedAvg')}>Pts Riv/G {sortIcon('ptsAllowedAvg')}</th>
                    <th className={`${thCls('fgPct')} border-l border-acb-200`} onClick={() => handleSort('fgPct')}>TC% {sortIcon('fgPct')}</th>
                    <th className={thCls('fg3Pct')} onClick={() => handleSort('fg3Pct')}>3P% {sortIcon('fg3Pct')}</th>
                    <th className={thCls('ftPct')} onClick={() => handleSort('ftPct')}>TL% {sortIcon('ftPct')}</th>
                    <th className={thCls('fg3Rate')} onClick={() => handleSort('fg3Rate')}>3PAr {sortIcon('fg3Rate')}</th>
                    <th className={thCls('efgPct')} onClick={() => handleSort('efgPct')}>eFG% {sortIcon('efgPct')}</th>
                    <th className={`${thCls('orebAvg')} border-l border-acb-200`} onClick={() => handleSort('orebAvg')}>RO {sortIcon('orebAvg')}</th>
                    <th className={thCls('drebAvg')} onClick={() => handleSort('drebAvg')}>RD {sortIcon('drebAvg')}</th>
                    <th className={thCls('rebAvg')} onClick={() => handleSort('rebAvg')}>RT {sortIcon('rebAvg')}</th>
                    <th className={`${thCls('apg')} border-l border-acb-200`} onClick={() => handleSort('apg')}>APP {sortIcon('apg')}</th>
                    <th className={thCls('spg')} onClick={() => handleSort('spg')}>RBP {sortIcon('spg')}</th>
                    <th className={thCls('bpg')} onClick={() => handleSort('bpg')}>TAPP {sortIcon('bpg')}</th>
                    <th className={thCls('topg')} onClick={() => handleSort('topg')}>PER {sortIcon('topg')}</th>
                  </>}
                  {viewTab === 'avanzado' && <>
                    <th className={`${thCls('ortg')} border-l border-acb-200`} onClick={() => handleSort('ortg')}>ORtg {sortIcon('ortg')}</th>
                    <th className={thCls('drtg')} onClick={() => handleSort('drtg')}>DRtg {sortIcon('drtg')}</th>
                    <th className={thCls('netRtg')} onClick={() => handleSort('netRtg')}>Neto {sortIcon('netRtg')}</th>
                    <th className={thCls('plusMinus')} onClick={() => handleSort('plusMinus')}>+/- {sortIcon('plusMinus')}</th>
                    <th className={`${thCls('efgPct')} border-l border-acb-200`} onClick={() => handleSort('efgPct')}>eFG% {sortIcon('efgPct')}</th>
                    <th className={thCls('tsPct')} onClick={() => handleSort('tsPct')}>TS% {sortIcon('tsPct')}</th>
                    <th className={thCls('fg3Rate')} onClick={() => handleSort('fg3Rate')}>3PAr {sortIcon('fg3Rate')}</th>
                    <th className={`${thCls('orbPct')} border-l border-acb-200`} onClick={() => handleSort('orbPct')}>RO% {sortIcon('orbPct')}</th>
                    <th className={thCls('drbPct')} onClick={() => handleSort('drbPct')}>RD% {sortIcon('drbPct')}</th>
                    <th className={`${thCls('astRate')} border-l border-acb-200`} onClick={() => handleSort('astRate')}>AST% {sortIcon('astRate')}</th>
                    <th className={thCls('stlRate')} onClick={() => handleSort('stlRate')}>ROB% {sortIcon('stlRate')}</th>
                    <th className={thCls('blkRate')} onClick={() => handleSort('blkRate')}>TAP% {sortIcon('blkRate')}</th>
                    <th className={thCls('tovRate')} onClick={() => handleSort('tovRate')}>PER% {sortIcon('tovRate')}</th>
                    <th className={thCls('astToRatio')} onClick={() => handleSort('astToRatio')}>AST/PER {sortIcon('astToRatio')}</th>
                  </>}
                  {viewTab === 'rivalAvanzado' && <>
                    <th className={`${thCls('drtg')} border-l border-acb-200`} onClick={() => handleSort('drtg')}>DRtg {sortIcon('drtg')}</th>
                    <th className={thCls('netRtg')} onClick={() => handleSort('netRtg')}>Neto {sortIcon('netRtg')}</th>
                    <th className={thCls('plusMinus')} onClick={() => handleSort('plusMinus')}>+/- {sortIcon('plusMinus')}</th>
                    <th className={`${thCls('opp_efgPct')} border-l border-acb-200`} onClick={() => handleSort('opp_efgPct')}>eFG% {sortIcon('opp_efgPct')}</th>
                    <th className={thCls('opp_tsPct')} onClick={() => handleSort('opp_tsPct')}>TS% {sortIcon('opp_tsPct')}</th>
                    <th className={thCls('opp_fg3Rate')} onClick={() => handleSort('opp_fg3Rate')}>3PAr {sortIcon('opp_fg3Rate')}</th>
                    <th className={`${thCls('opp_orbPct')} border-l border-acb-200`} onClick={() => handleSort('opp_orbPct')}>RO% {sortIcon('opp_orbPct')}</th>
                    <th className={thCls('opp_drbPct')} onClick={() => handleSort('opp_drbPct')}>RD% {sortIcon('opp_drbPct')}</th>
                    <th className={`${thCls('opp_astRate')} border-l border-acb-200`} onClick={() => handleSort('opp_astRate')}>AST% {sortIcon('opp_astRate')}</th>
                    <th className={thCls('opp_stlRate')} onClick={() => handleSort('opp_stlRate')}>ROB% {sortIcon('opp_stlRate')}</th>
                    <th className={thCls('opp_blkRate')} onClick={() => handleSort('opp_blkRate')}>TAP% {sortIcon('opp_blkRate')}</th>
                    <th className={thCls('opp_tovRate')} onClick={() => handleSort('opp_tovRate')}>PER% {sortIcon('opp_tovRate')}</th>
                    <th className={thCls('opp_astToRatio')} onClick={() => handleSort('opp_astToRatio')}>AST/PER {sortIcon('opp_astToRatio')}</th>
                  </>}
                  {viewTab === 'rival' && <>
                    <th className={`${thCls('ptsAllowedAvg')} border-l border-acb-200`} onClick={() => handleSort('ptsAllowedAvg')}>Pts/G {sortIcon('ptsAllowedAvg')}</th>
                    <th className={`${thCls('opp_fgPct')} border-l border-acb-200`} onClick={() => handleSort('opp_fgPct')}>TC% {sortIcon('opp_fgPct')}</th>
                    <th className={thCls('opp_fg3Pct')} onClick={() => handleSort('opp_fg3Pct')}>3P% {sortIcon('opp_fg3Pct')}</th>
                    <th className={thCls('opp_ftPct')} onClick={() => handleSort('opp_ftPct')}>TL% {sortIcon('opp_ftPct')}</th>
                    <th className={thCls('opp_fg3Rate')} onClick={() => handleSort('opp_fg3Rate')}>3PAr {sortIcon('opp_fg3Rate')}</th>
                    <th className={thCls('opp_efgPct')} onClick={() => handleSort('opp_efgPct')}>eFG% {sortIcon('opp_efgPct')}</th>
                    <th className={`${thCls('opp_orebAvg')} border-l border-acb-200`} onClick={() => handleSort('opp_orebAvg')}>RO {sortIcon('opp_orebAvg')}</th>
                    <th className={thCls('opp_drebAvg')} onClick={() => handleSort('opp_drebAvg')}>RD {sortIcon('opp_drebAvg')}</th>
                    <th className={`${thCls('opp_apg')} border-l border-acb-200`} onClick={() => handleSort('opp_apg')}>APP {sortIcon('opp_apg')}</th>
                    <th className={thCls('opp_spg')} onClick={() => handleSort('opp_spg')}>RBP {sortIcon('opp_spg')}</th>
                    <th className={thCls('opp_bpg')} onClick={() => handleSort('opp_bpg')}>TAPP {sortIcon('opp_bpg')}</th>
                    <th className={thCls('opp_topg')} onClick={() => handleSort('opp_topg')}>PER {sortIcon('opp_topg')}</th>
                  </>}
                </tr>
              </thead>
              <tbody className="divide-y divide-acb-100">
                {sorted.map(t => (
                  <tr key={t.team} className="data-table-row border-b border-acb-100">
                    <td className="data-table-cell data-table-identity data-table-sticky data-col-team">{t.team}</td>
                    <td className="data-table-cell data-table-number data-col-games text-acb-600">{t.games}</td>
                    {viewTab === 'basico' && <>
                      <td className={`data-table-cell data-table-number data-col-number border-l border-acb-100 ${sortCol === 'winPct' ? 'bg-acb-50/60' : ''}`}>
                        <div className="data-table-value">
                          <span className="text-acb-700">{t.wins}-{t.losses}</span>
                          {t.winPctRank != null && <span className={`data-table-badge ${getRankColor(t.winPctRank, n)}`}>#{t.winPctRank}</span>}
                        </div>
                      </td>
                      <StatCell t={t} k="plusMinus" signed />
                      <StatCell t={t} k="ptsScoredAvg" />
                      <StatCell t={t} k="ptsAllowedAvg" />
                      <StatCell t={t} k="fgPct" pct />
                      <StatCell t={t} k="fg3Pct" pct />
                      <StatCell t={t} k="ftPct" pct />
                      <td className={`data-table-cell data-table-number data-col-number ${sortCol === 'fg3Rate' ? 'bg-acb-50/60' : ''}`}>
                        <span className="font-mono text-acb-700">{fmt(t.fg3Rate, true)}</span>
                      </td>
                      <StatCell t={t} k="efgPct" pct />
                      <StatCell t={t} k="orebAvg" />
                      <StatCell t={t} k="drebAvg" />
                      <StatCell t={t} k="rebAvg" />
                      <StatCell t={t} k="apg" />
                      <StatCell t={t} k="spg" />
                      <StatCell t={t} k="bpg" />
                      <StatCell t={t} k="topg" />
                    </>}
                    {viewTab === 'avanzado' && <>
                      <StatCell t={t} k="ortg" />
                      <StatCell t={t} k="drtg" />
                      <StatCell t={t} k="netRtg" signed />
                      <StatCell t={t} k="plusMinus" signed />
                      <StatCell t={t} k="efgPct" pct />
                      <StatCell t={t} k="tsPct" pct />
                      <td className={`data-table-cell data-table-number data-col-number ${sortCol === 'fg3Rate' ? 'bg-acb-50/60' : ''}`}>
                        <span className="font-mono text-acb-700">{fmt(t.fg3Rate, true)}</span>
                      </td>
                      <StatCell t={t} k="orbPct" pct />
                      <StatCell t={t} k="drbPct" pct />
                      <StatCell t={t} k="astRate" pct />
                      <StatCell t={t} k="stlRate" pct />
                      <StatCell t={t} k="blkRate" pct />
                      <StatCell t={t} k="tovRate" pct />
                      <td className={`data-table-cell data-table-number data-col-number ${sortCol === 'astToRatio' ? 'bg-acb-50/60' : ''}`}>
                        <div className="data-table-value">
                          <span className="text-acb-700">{fmt(t.astToRatio, false)}</span>
                          {t.astToRatioRank != null && <span className={`data-table-badge ${getRankColor(t.astToRatioRank, n)}`}>#{t.astToRatioRank}</span>}
                        </div>
                      </td>
                    </>}
                    {viewTab === 'rivalAvanzado' && <>
                      <StatCell t={t} k="drtg" />
                      <StatCell t={t} k="netRtg" signed />
                      <StatCell t={t} k="plusMinus" signed />
                      <StatCell t={t} k="opp_efgPct" pct />
                      <StatCell t={t} k="opp_tsPct" pct />
                      <td className={`data-table-cell data-table-number data-col-number ${sortCol === 'opp_fg3Rate' ? 'bg-acb-50/60' : ''}`}>
                        <span className="font-mono text-acb-700">{fmt(t.opp_fg3Rate, true)}</span>
                      </td>
                      <StatCell t={t} k="opp_orbPct" pct />
                      <StatCell t={t} k="opp_drbPct" pct />
                      <StatCell t={t} k="opp_astRate" pct />
                      <StatCell t={t} k="opp_stlRate" pct />
                      <StatCell t={t} k="opp_blkRate" pct />
                      <StatCell t={t} k="opp_tovRate" pct />
                      <td className={`data-table-cell data-table-number data-col-number ${sortCol === 'opp_astToRatio' ? 'bg-acb-50/60' : ''}`}>
                        <div className="data-table-value">
                          <span className="text-acb-700">{fmt(t.opp_astToRatio, false)}</span>
                          {t.opp_astToRatioRank != null && <span className={`data-table-badge ${getRankColor(t.opp_astToRatioRank, n)}`}>#{t.opp_astToRatioRank}</span>}
                        </div>
                      </td>
                    </>}
                    {viewTab === 'rival' && <>
                      <StatCell t={t} k="ptsAllowedAvg" />
                      <StatCell t={t} k="opp_fgPct" pct />
                      <StatCell t={t} k="opp_fg3Pct" pct />
                      <StatCell t={t} k="opp_ftPct" pct />
                      <td className={`data-table-cell data-table-number data-col-number ${sortCol === 'opp_fg3Rate' ? 'bg-acb-50/60' : ''}`}>
                        <span className="font-mono text-acb-700">{fmt(t.opp_fg3Rate, true)}</span>
                      </td>
                      <StatCell t={t} k="opp_efgPct" pct />
                      <StatCell t={t} k="opp_orebAvg" />
                      <StatCell t={t} k="opp_drebAvg" />
                      <StatCell t={t} k="opp_apg" />
                      <StatCell t={t} k="opp_spg" />
                      <StatCell t={t} k="opp_bpg" />
                      <StatCell t={t} k="opp_topg" />
                    </>}
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
                <span className="w-3 h-3 rounded-full bg-positive-100 border border-positive-500"></span>
                <span className="text-acb-500">{game.local} anota</span>
              </div>
              <div className="flex items-center gap-1.5">
                <span className="w-3 h-3 rounded-full bg-negative-100 border border-negative-500"></span>
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
                        isLocal ? 'border-positive-200 bg-positive-50' : 'border-negative-200 bg-negative-50'
                      }`}
                    >
                      <span className={`font-semibold ${isLocal ? 'text-positive-700' : 'text-negative-700'}`}>{teamName}</span>
                      <span className={`font-semibold ${isLocal ? 'text-positive-800' : 'text-negative-800'}`}>{wonPts}-{lostPts}</span>
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
