import { useState, useMemo, useEffect, useRef } from 'react'
import { useLocation, useNavigate, useParams, useSearchParams } from 'react-router-dom'
import { getPlayerDisplayName } from '../utils/playerNames'
import GameAnalysisNav from '../components/GameAnalysisNav'
import TeamToolShell, { teamToolStyles as styles } from '../components/TeamToolShell'
import MobileNameRow from '../components/MobileNameRow'
import SortLabel from '../components/SortLabel'
import {
  buildGamesPath,
  parseGameSegment,
  parseRouteQuery,
  serializeRouteQuery,
  withQuery,
} from '../routing'

// ─── Helpers ──────────────────────────────────────────────────

function seasonLabel(s) {
  return `${s - 1}-${String(s).slice(-2)}`
}

function gameRoundLabel(game) {
  return game.competitionStage === 'playoffs'
    ? game.competitionRound || 'Playoffs'
    : `Jornada ${game.jornada}`
}

function normalizeName(value) {
  return String(value || '')
    .normalize('NFD')
    .replace(/[\u0300-\u036f]/g, '')
    .trim()
    .toLocaleLowerCase('es')
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
  if (pct <= 0.25) return 'bg-acb-800 text-white'
  if (pct <= 0.5)  return 'bg-acb-200 text-acb-800'
  if (pct <= 0.75) return 'bg-acb-100 text-acb-700'
  return 'bg-acb-50 text-acb-600'
}

function ClutchTeamsView({ selectedSeason, onSeasonChange, viewTab, onViewChange, availableSeasons, clutchCache, loadingClutch }) {
  const [sortCol, setSortCol] = useState('plusMinus')
  const [sortDir, setSortDir] = useState('desc')

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
      'astRate','blkRate','stlRate','astToRatio','opp_topg','opp_tovRate',
    ]
    rankDesc.forEach(key => {
      const s = [...copy].filter(t => t[key] != null).sort((a, b) => (b[key] || 0) - (a[key] || 0))
      s.forEach((t, i) => { const o = copy.find(x => x.team === t.team); if (o) o[`${key}Rank`] = i + 1 })
    })
    const rankAsc = [
      'ptsAllowedAvg','drtg','topg','tovRate',
      'opp_fgPct','opp_fg2Pct','opp_fg3Pct','opp_ftPct','opp_efgPct','opp_tsPct',
      'opp_orebAvg','opp_drebAvg','opp_apg','opp_spg','opp_bpg',
      'opp_astRate','opp_stlRate','opp_blkRate','opp_astToRatio',
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
  // First column of each header group gets a line down the table
  const groupStarts = new Set({
    basico: ['winPct', 'fgPct', 'orebAvg', 'apg'],
    avanzado: ['ortg', 'efgPct', 'orbPct', 'astRate'],
    rival: ['ptsAllowedAvg', 'opp_fgPct', 'opp_orebAvg', 'opp_apg'],
    rivalAvanzado: ['drtg', 'opp_efgPct', 'opp_orbPct', 'opp_astRate'],
  }[viewTab] ?? [])
  const groupStartClass = key => groupStarts.has(key) ? 'data-table-group-start' : ''
  const sortTh = (key, label) => (
    <th
      key={key}
      className={`data-table-head data-table-number ${groupStartClass(key)}`}
      onClick={() => handleSort(key)}
      aria-sort={sortCol === key ? (sortDir === 'desc' ? 'descending' : 'ascending') : 'none'}
    >
      <SortLabel direction={sortCol === key && sortDir}>{label}</SortLabel>
    </th>
  )

  const StatCell = ({ t, k, pct = false, signed = false }) => {
    const v = t[k]
    const rank = t[`${k}Rank`]
    let display, colorClass = 'text-acb-700'
    if (v == null || isNaN(v)) display = '-'
    else if (signed) { display = v > 0 ? `+${v.toFixed(1)}` : v.toFixed(1); colorClass = v > 0 ? 'text-positive' : v < 0 ? 'text-negative' : 'text-acb-700' }
    else display = pct ? `${Number(v).toFixed(1)}%` : Number(v).toFixed(1)
    return (
      <td className={`data-table-cell data-table-number ${groupStartClass(k)}`}>
        <div className="data-table-value">
          <span className={colorClass}>{display}</span>
          {rank != null && <span className={`data-table-badge ${getRankColor(rank, n)}`}>#{rank}</span>}
        </div>
      </td>
    )
  }

  return (
    <TeamToolShell
      activeTool="games"
      title="Clutch por equipo"
      scope={`Liga regular y playoffs · ${sl(selectedSeason)}`}
    >
      <GameAnalysisNav season={selectedSeason} />

      <div className={styles.controlBand}>
        <div className="flex flex-col gap-1">
          <label className="field-label">Temporada</label>
          <select
            aria-label="Temporada de estadísticas clutch"
            value={selectedSeason}
            onChange={e => onSeasonChange(Number(e.target.value))}
            className="form-control"
          >
            {availableSeasons.map(s => <option key={s} value={s}>{sl(s)}</option>)}
          </select>
        </div>

        <div className={`${styles.viewTabs} ${styles.controlSpacer}`} role="group" aria-label="Tipo de estadísticas clutch">
        {[['basico','Básico'],['avanzado','Avanzado'],['rival','Rival'],['rivalAvanzado','Riv. Avanzado']].map(([id, label]) => (
          <button key={id} onClick={() => {
            onViewChange(id)
            setSortCol(id === 'avanzado' ? 'netRtg' : id === 'rival' ? 'opp_efgPct' : id === 'rivalAvanzado' ? 'drtg' : 'plusMinus')
          }}
            aria-pressed={viewTab === id}
            className={`${styles.viewTab} ${viewTab === id ? styles.viewTabActive : ''}`}>
            {label}
          </button>
        ))}
        </div>
      </div>

      {isLoading ? (
        <div className={styles.loadingState} role="status">Cargando estadísticas clutch…</div>
      ) : !clutchData ? (
        <div className={styles.emptyState}>No hay datos disponibles para esta temporada.</div>
      ) : (
        <section className={styles.dataSection} aria-labelledby="clutch-table-title">
          <div className={styles.sectionHeader}>
            <div>
              <h2 id="clutch-table-title">Rendimiento en finales ajustados</h2>
              <p>Ordena cada indicador para identificar qué equipos resuelven mejor bajo presión.</p>
            </div>
            <span className={styles.panelMeta}>{n} equipos</span>
          </div>
          <div className={`${styles.tableFrame} mobile-name-frame`} tabIndex={0} aria-label="Estadísticas clutch por equipo">
            <table className="data-table">
              <thead>
                <tr>
                  <th className="data-table-head text-left data-table-sticky data-table-sticky-head data-col-team mobile-name-hide" rowSpan="2">Equipo</th>
                  <th className="data-table-head data-table-number" rowSpan="2">PJ</th>
                  {viewTab === 'basico' && <>
                    <th className="data-table-group data-table-group-start" colSpan="4">Marcador</th>
                    <th className="data-table-group data-table-group-start" colSpan="5">Tiro</th>
                    <th className="data-table-group data-table-group-start" colSpan="3">Rebotes</th>
                    <th className="data-table-group data-table-group-start" colSpan="4">Otros</th>
                  </>}
                  {viewTab === 'avanzado' && <>
                    <th className="data-table-group data-table-group-start" colSpan="4">Rating</th>
                    <th className="data-table-group data-table-group-start" colSpan="3">Tiro</th>
                    <th className="data-table-group data-table-group-start" colSpan="2">Rebotes</th>
                    <th className="data-table-group data-table-group-start" colSpan="5">Ratios</th>
                  </>}
                  {viewTab === 'rival' && <>
                    <th className="data-table-group data-table-group-start" colSpan="1">Puntos</th>
                    <th className="data-table-group data-table-group-start" colSpan="5">Tiro</th>
                    <th className="data-table-group data-table-group-start" colSpan="2">Rebotes</th>
                    <th className="data-table-group data-table-group-start" colSpan="4">Otros</th>
                  </>}
                  {viewTab === 'rivalAvanzado' && <>
                    <th className="data-table-group data-table-group-start" colSpan="3">Rating</th>
                    <th className="data-table-group data-table-group-start" colSpan="3">Tiro</th>
                    <th className="data-table-group data-table-group-start" colSpan="2">Rebotes</th>
                    <th className="data-table-group data-table-group-start" colSpan="5">Ratios</th>
                  </>}
                </tr>
                <tr>
                  {viewTab === 'basico' && <>
                    {sortTh('winPct', 'V-D')}
                    {sortTh('plusMinus', '+/-')}
                    {sortTh('ptsScoredAvg', 'Pts/G')}
                    {sortTh('ptsAllowedAvg', 'Pts Riv/G')}
                    {sortTh('fgPct', 'TC%')}
                    {sortTh('fg3Pct', '3P%')}
                    {sortTh('ftPct', 'TL%')}
                    {sortTh('fg3Rate', '3PAr')}
                    {sortTh('efgPct', 'eFG%')}
                    {sortTh('orebAvg', 'RO')}
                    {sortTh('drebAvg', 'RD')}
                    {sortTh('rebAvg', 'RT')}
                    {sortTh('apg', 'APP')}
                    {sortTh('spg', 'RBP')}
                    {sortTh('bpg', 'TAPP')}
                    {sortTh('topg', 'PER')}
                  </>}
                  {viewTab === 'avanzado' && <>
                    {sortTh('ortg', 'ORtg')}
                    {sortTh('drtg', 'DRtg')}
                    {sortTh('netRtg', 'Neto')}
                    {sortTh('plusMinus', '+/-')}
                    {sortTh('efgPct', 'eFG%')}
                    {sortTh('tsPct', 'TS%')}
                    {sortTh('fg3Rate', '3PAr')}
                    {sortTh('orbPct', 'RO%')}
                    {sortTh('drbPct', 'RD%')}
                    {sortTh('astRate', 'AST%')}
                    {sortTh('stlRate', 'ROB%')}
                    {sortTh('blkRate', 'TAP%')}
                    {sortTh('tovRate', 'PER%')}
                    {sortTh('astToRatio', 'AST/PER')}
                  </>}
                  {viewTab === 'rivalAvanzado' && <>
                    {sortTh('drtg', 'DRtg')}
                    {sortTh('netRtg', 'Neto')}
                    {sortTh('plusMinus', '+/-')}
                    {sortTh('opp_efgPct', 'eFG%')}
                    {sortTh('opp_tsPct', 'TS%')}
                    {sortTh('opp_fg3Rate', '3PAr')}
                    {sortTh('opp_orbPct', 'RO%')}
                    {sortTh('opp_drbPct', 'RD%')}
                    {sortTh('opp_astRate', 'AST%')}
                    {sortTh('opp_stlRate', 'ROB%')}
                    {sortTh('opp_blkRate', 'TAP%')}
                    {sortTh('opp_tovRate', 'PER%')}
                    {sortTh('opp_astToRatio', 'AST/PER')}
                  </>}
                  {viewTab === 'rival' && <>
                    {sortTh('ptsAllowedAvg', 'Pts/G')}
                    {sortTh('opp_fgPct', 'TC%')}
                    {sortTh('opp_fg3Pct', '3P%')}
                    {sortTh('opp_ftPct', 'TL%')}
                    {sortTh('opp_fg3Rate', '3PAr')}
                    {sortTh('opp_efgPct', 'eFG%')}
                    {sortTh('opp_orebAvg', 'RO')}
                    {sortTh('opp_drebAvg', 'RD')}
                    {sortTh('opp_apg', 'APP')}
                    {sortTh('opp_spg', 'RBP')}
                    {sortTh('opp_bpg', 'TAPP')}
                    {sortTh('opp_topg', 'PER')}
                  </>}
                </tr>
              </thead>
              {sorted.map(t => (
                <tbody key={t.team} className="mobile-name-group">
                  <MobileNameRow name={t.team} />
                  <tr className="data-table-row">
                    <td className="data-table-cell data-table-identity data-table-sticky data-col-team mobile-name-hide">{t.team}</td>
                    <td className="data-table-cell data-table-number text-acb-600">{t.games}</td>
                    {viewTab === 'basico' && <>
                      <td className={`data-table-cell data-table-number ${groupStartClass('winPct')}`}>
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
                      <td className={`data-table-cell data-table-number ${groupStartClass('fg3Rate')}`}>
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
                      <td className={`data-table-cell data-table-number ${groupStartClass('fg3Rate')}`}>
                        <span className="font-mono text-acb-700">{fmt(t.fg3Rate, true)}</span>
                      </td>
                      <StatCell t={t} k="orbPct" pct />
                      <StatCell t={t} k="drbPct" pct />
                      <StatCell t={t} k="astRate" pct />
                      <StatCell t={t} k="stlRate" pct />
                      <StatCell t={t} k="blkRate" pct />
                      <StatCell t={t} k="tovRate" pct />
                      <td className={`data-table-cell data-table-number ${groupStartClass('astToRatio')}`}>
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
                      <td className={`data-table-cell data-table-number ${groupStartClass('opp_fg3Rate')}`}>
                        <span className="font-mono text-acb-700">{fmt(t.opp_fg3Rate, true)}</span>
                      </td>
                      <StatCell t={t} k="opp_orbPct" pct />
                      <StatCell t={t} k="opp_drbPct" pct />
                      <StatCell t={t} k="opp_astRate" pct />
                      <StatCell t={t} k="opp_stlRate" pct />
                      <StatCell t={t} k="opp_blkRate" pct />
                      <StatCell t={t} k="opp_tovRate" pct />
                      <td className={`data-table-cell data-table-number ${groupStartClass('opp_astToRatio')}`}>
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
                      <td className={`data-table-cell data-table-number ${groupStartClass('opp_fg3Rate')}`}>
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
                </tbody>
              ))}
            </table>
          </div>
          <div className="px-4 py-2 border-t border-acb-100 bg-acb-50 text-xs text-acb-400">
            {n} equipos · Stats por partido clutch · {sl(selectedSeason)}
          </div>
        </section>
      )}
    </TeamToolShell>
  )
}

// ─── Main Component ───────────────────────────────────────────

export function TeamClutchPage({ teams, loadClutchForSeason, clutchCache, loadingClutch }) {
  const [searchParams, setSearchParams] = useSearchParams()
  const availableSeasons = useMemo(
    () => [...new Set(teams.map(team => team.season))].sort((a, b) => b - a),
    [teams]
  )
  const parsedQuery = parseRouteQuery('teamClutch', searchParams)
  const requestedSeason = Number(parsedQuery.values.temporada)
  const selectedSeason = availableSeasons.includes(requestedSeason)
    ? requestedSeason
    : (availableSeasons[0] || 2026)
  const viewTabs = {
    basic: 'basico',
    advanced: 'avanzado',
    opponent: 'rival',
    opponentAdvanced: 'rivalAvanzado',
  }
  const queryViews = Object.fromEntries(Object.entries(viewTabs).map(([queryView, tab]) => [tab, queryView]))
  const queryView = parsedQuery.values.vista || 'basic'
  const viewTab = viewTabs[queryView]

  useEffect(() => {
    if (selectedSeason) loadClutchForSeason(selectedSeason)
  }, [loadClutchForSeason, selectedSeason])

  useEffect(() => {
    const canonical = serializeRouteQuery('teamClutch', {
      temporada: selectedSeason,
      vista: queryView,
    })
    if (canonical !== searchParams.toString()) {
      setSearchParams(canonical, { replace: true })
    }
  }, [queryView, searchParams, selectedSeason, setSearchParams])

  const updateState = (season, view) => {
    setSearchParams(serializeRouteQuery('teamClutch', {
      temporada: season,
      vista: queryViews[view] || 'basic',
    }))
  }

  return (
    <ClutchTeamsView
      selectedSeason={selectedSeason}
      onSeasonChange={season => updateState(season, viewTab)}
      viewTab={viewTab}
      onViewChange={view => updateState(selectedSeason, view)}
      availableSeasons={availableSeasons}
      clutchCache={clutchCache}
      loadingClutch={loadingClutch}
    />
  )
}

export default function GameFlow({ teams, playerRecords = [], loadGameFlowForSeason, gameFlowCache, loadingGameFlow }) {
  const location = useLocation()
  const navigate = useNavigate()
  const { game: gameUrlSegment } = useParams()

  const availableSeasons = useMemo(() => {
    return [...new Set(teams.map(t => t.season))].sort((a, b) => b - a)
  }, [teams])

  const parsedQuery = parseRouteQuery('games', location.search)
  const requestedSeason = Number(parsedQuery.values.temporada)
  const selectedSeason = availableSeasons.includes(requestedSeason)
    ? requestedSeason
    : (availableSeasons[0] || 2026)
  const [hoveredEvent, setHoveredEvent] = useState(null)
  const svgRef = useRef(null)
  const tooltipRef = useRef(null)

  useEffect(() => {
    if (selectedSeason) loadGameFlowForSeason(selectedSeason)
  }, [selectedSeason, loadGameFlowForSeason])

  const games = useMemo(() => {
    return gameFlowCache[selectedSeason] || []
  }, [gameFlowCache, selectedSeason])

  const hasLoadedSeason = Object.hasOwn(gameFlowCache, selectedSeason)
  const isLoading = loadingGameFlow[selectedSeason] || false

  // agrupa la liga regular por jornada y los playoffs por ronda
  const jornadas = useMemo(() => {
    const map = new Map()
    games.forEach(g => {
      const key = g.competitionStage === 'playoffs'
        ? `playoffs:${g.competitionRound || 'Playoffs'}`
        : `regular:${g.jornada}`
      if (!map.has(key)) map.set(key, { label: gameRoundLabel(g), games: [] })
      map.get(key).games.push(g)
    })
    return [...map.entries()]
  }, [games])

  const requestedJornada = parsedQuery.values.ronda
  const requestedGameId = Number(parseGameSegment(gameUrlSegment)?.id) || null
  const game = requestedGameId == null
    ? null
    : games.find(candidate => Number(candidate.id) === requestedGameId) || null
  const gameJornada = game
    ? (game.competitionStage === 'playoffs'
        ? `playoffs:${game.competitionRound || 'Playoffs'}`
        : `regular:${game.jornada}`)
    : null
  const selectedJornada = gameJornada
    || (requestedJornada && !hasLoadedSeason ? requestedJornada : null)
    || (jornadas.some(([key]) => key === requestedJornada) ? requestedJornada : null)
    || jornadas.at(-1)?.[0]
    || null

  useEffect(() => {
    const canonicalPath = game
      ? buildGamesPath(game)
      : gameUrlSegment
        ? location.pathname
        : buildGamesPath()
    const canonicalSearch = serializeRouteQuery('games', {
      temporada: selectedSeason,
      ronda: selectedJornada,
    }, { strict: false })
    const target = withQuery(canonicalPath, canonicalSearch)
    const current = `${location.pathname}${location.search}`
    if (target !== current) navigate(target, { replace: true })
  }, [game, gameUrlSegment, location.pathname, location.search, navigate, selectedJornada, selectedSeason])

  const jornadaGames = useMemo(() => {
    if (selectedJornada == null) return []
    const entry = jornadas.find(([j]) => j === selectedJornada)
    return entry ? entry[1].games : []
  }, [jornadas, selectedJornada])

  const playerNameLookup = useMemo(() => {
    const candidates = new Map()
    playerRecords.forEach(record => {
      if (Number(record.season) !== Number(selectedSeason) || !record.team) return
      const displayName = getPlayerDisplayName(record, null)
      if (!displayName) return
      const aliases = [record.player, record.nickname, record.nick].filter(Boolean)
      aliases.forEach(alias => {
        const key = `${record.team}|${normalizeName(alias)}`
        if (!candidates.has(key)) candidates.set(key, new Set())
        candidates.get(key).add(displayName)
      })
    })

    return new Map(
      [...candidates.entries()]
        .filter(([, names]) => names.size === 1)
        .map(([key, names]) => [key, [...names][0]])
    )
  }, [playerRecords, selectedSeason])

  const resolveEventPlayer = (event) => {
    if (!game || !event?.player) return event?.player || '-'
    const team = event.team === 'L' ? game.local : game.visitor
    return playerNameLookup.get(`${team}|${normalizeName(event.player)}`) || event.player
  }

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
    const scorePoints = [{ t: 0, margin: 0, sl: 0, sv: 0 }]

    let lastSl = 0, lastSv = 0
    events.forEach(e => {
      if (e.sl != null && e.sv != null && (e.sl !== lastSl || e.sv !== lastSv)) {
        scorePoints.push({ t: e.t, margin: e.sl - e.sv, sl: e.sl, sv: e.sv })
        lastSl = e.sl
        lastSv = e.sv
      }
    })

    // Add final point at game end
    scorePoints.push({ t: totalTime, margin: game.scoreL - game.scoreV, sl: game.scoreL, sv: game.scoreV })

    // repite el valor anterior en cada instante para dibujar saltos de marcador
    const points = []
    scorePoints.forEach((point, index) => {
      if (index > 0) points.push({ ...scorePoints[index - 1], t: point.t })
      points.push(point)
    })

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
          const crossX = xScale(p.t)
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

  return (
    <TeamToolShell
      activeTool="games"
      title="Flujo de partido"
    >
      <GameAnalysisNav season={selectedSeason} />

      {/* Season selector */}
      <div className={styles.controlBand}>
        <div className="flex flex-col gap-1">
          <label className="field-label">Temporada</label>
          <select
            aria-label="Temporada del flujo de partido"
            value={selectedSeason}
            onChange={e => navigate(withQuery(
              buildGamesPath(),
              serializeRouteQuery('games', { temporada: Number(e.target.value) })
            ))}
            className="form-control"
          >
            {availableSeasons.map(s => (
              <option key={s} value={s}>{seasonLabel(s)}</option>
            ))}
          </select>
        </div>

        {jornadas.length > 0 && (
          <div className="flex flex-col gap-1">
            <label className="field-label">Jornada</label>
            <select
              aria-label="Jornada o ronda"
              value={selectedJornada ?? ''}
              onChange={e => navigate(withQuery(
                buildGamesPath(),
                serializeRouteQuery('games', { temporada: selectedSeason, ronda: e.target.value })
              ))}
              className="form-control"
            >
              {jornadas.map(([j, group]) => (
                <option key={j} value={j}>{group.label}</option>
              ))}
            </select>
          </div>
        )}
      </div>

      {/* Loading */}
      {isLoading && (
        <div className={styles.loadingState} role="status">Cargando partidos…</div>
      )}

      {/* Game cards */}
      {!isLoading && jornadaGames.length > 0 && (
        <section className={styles.profileSection} aria-labelledby="round-games-title">
          <div className={styles.profileSectionTitle}>
            <h2 id="round-games-title">Partidos de la jornada</h2>
            <p>{jornadaGames.length} {jornadaGames.length === 1 ? 'partido disponible' : 'partidos disponibles'}</p>
          </div>
          <div className={styles.gameGrid}>
          {jornadaGames.map(g => {
            const isSelected = Number(game?.id) === Number(g.id)
            return (
              <button
                key={g.id}
                onClick={() => navigate(withQuery(
                  isSelected ? buildGamesPath() : buildGamesPath(g),
                  serializeRouteQuery('games', { temporada: selectedSeason, ronda: selectedJornada })
                ))}
                className={`${styles.gameButton} ${isSelected ? styles.gameButtonActive : ''}`}
                aria-pressed={isSelected}
              >
                <div className={styles.gameMeta}>{gameRoundLabel(g)}</div>
                <div className={styles.fixtureLine}>
                  <span className={styles.homeTeam}>{g.local}</span>
                  <strong className={styles.homeScore}>{g.scoreL}</strong>
                </div>
                <div className={styles.fixtureLine}>
                  <span className={styles.awayTeam}>{g.visitor}</span>
                  <strong className={styles.awayScore}>{g.scoreV}</strong>
                </div>
              </button>
            )
          })}
          </div>
        </section>
      )}

      {/* No data */}
      {!isLoading && games.length === 0 && !isLoading && (
        <div className={styles.emptyState}>
          No hay datos de flujo de partido para esta temporada
        </div>
      )}

      {/* Chart */}
      {game && chartData && (
        <section className={`${styles.panel} ${styles.gameChart} p-4`} aria-labelledby="game-flow-chart-title">
          {/* Game header */}
          <div className={styles.gameHeading}>
            <div>
              <h2 id="game-flow-chart-title" className="font-semibold text-acb-900">
                {game.local} {game.scoreL} - {game.scoreV} {game.visitor}
              </h2>
              <p className="text-xs text-acb-400">{gameRoundLabel(game)} - {seasonLabel(selectedSeason)}</p>
            </div>
            <div className={styles.gameLegend}>
              <div className="flex items-center gap-1.5">
                <span className="w-3 h-3 rounded-full bg-accent-100 border border-accent-500"></span>
                <span className="text-acb-500">{game.local} anota</span>
              </div>
              <div className="flex items-center gap-1.5">
                <span className="w-3 h-3 rounded-full bg-info-100 border border-info-500"></span>
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
              role="img"
              aria-label={`Evolución del marcador: ${game.local} contra ${game.visitor}`}
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

                {chartData.timeouts.map((timeout, i) => (
                  <g key={`timeout-${i}`} transform={`translate(${timeout.x},0)`}>
                    <line y1={0} y2={9} stroke="#64748b" strokeWidth={1.5} />
                    <text y={18} textAnchor="middle" className="fill-acb-500 text-[8px] font-semibold">TO</text>
                  </g>
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
                  const color = dot.team === 'L' ? '#fe5917' : '#3b82f6'
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
              <div className="font-medium">{resolveEventPlayer(hoveredEvent)} - {typeLabel(hoveredEvent.type)}</div>
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
              <div className={styles.runList}>
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
                      className={styles.runRow}
                    >
                      <span className={`font-semibold ${isLocal ? 'text-accent-700' : 'text-info-700'}`}>{teamName}</span>
                      <span className={`font-semibold ${isLocal ? 'text-accent-800' : 'text-info-800'}`}>{wonPts}-{lostPts}</span>
                      <span className="text-acb-400">{formatClock(run.tStart)} → {formatClock(run.tEnd)}</span>
                    </div>
                  )
                })}
              </div>
            </div>
          )}

        </section>
      )}

      {/* estado sin partido */}
      {!isLoading && games.length > 0 && !game && (
        <div className={styles.emptyState}>
          {gameUrlSegment
            ? 'No se ha encontrado ese partido en la temporada seleccionada.'
            : 'Selecciona un partido para ver el flujo del marcador'}
        </div>
      )}
    </TeamToolShell>
  )
}
