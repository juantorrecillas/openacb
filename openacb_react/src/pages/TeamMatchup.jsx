import { useState, useMemo, useEffect, useRef } from 'react'
import { useNavigate, useParams, useSearchParams, Link } from 'react-router-dom'
import ZoneHeatmap from '../components/ZoneHeatmap'
import PageHeader from '../components/PageHeader'
import { buildTeamComparisonPath, buildTeamProfilePath, buildTeamStatsPath } from '../routing/paths'
import { readRouteQuery, serializeRouteQuery } from '../routing/query'
import { createTeamIdentityIndexFromRows, resolveTeamId } from '../routing/teamIdentities'

function seasonLabel(s) {
  return `${s - 1}-${String(s).slice(-2)}`
}

function parseSeasonParam(value, availableSeasons) {
  const season = Number(value)
  return availableSeasons.includes(season) ? season : (availableSeasons[0] || 2026)
}

function buildTeamComparisonSearch({ season, mode, metric }) {
  return new URLSearchParams(serializeRouteQuery('teamComparison', {
    temporada: season,
    lado: mode,
    metrica: metric,
  }))
}

function num(v) {
  if (v == null || v === 'NA') return null
  const n = Number(v)
  return Number.isFinite(n) ? n : null
}

function fmt(v, type = 'decimal') {
  const n = num(v)
  if (n == null) return '—'
  if (type === 'pct') return `${(n * 100).toFixed(1)}%`
  if (type === 'pct100') return `${n.toFixed(1)}%`
  if (type === 'int') return String(Math.round(n))
  if (type === 'signed') return n > 0 ? `+${n.toFixed(1)}` : n.toFixed(1)
  return n.toFixed(1)
}

function fmtAdvantage(v, type = 'decimal') {
  const n = num(v)
  if (n == null) return '—'
  if (n === 0) return 'Igual'
  if (type === 'pct') return `${(Math.abs(n) * 100).toFixed(1)} pp`
  if (type === 'pct100') return `${Math.abs(n).toFixed(1)} pp`
  return Math.abs(n).toFixed(1)
}

function percentileRank(rows, key, value, inverse = false) {
  const vals = rows.map(r => num(r[key])).filter(v => v != null).sort((a, b) => a - b)
  const v = num(value)
  if (!vals.length || v == null) return null
  const below = vals.filter(x => x < v).length
  const equal = vals.filter(x => x === v).length
  const pct = vals.length === 1 ? 50 : ((below + (equal - 1) / 2) / (vals.length - 1)) * 100
  return Math.max(0, Math.min(100, inverse ? 100 - pct : pct))
}

function edgeClass(v) {
  if (v == null || Math.abs(v) < 0.5) return 'text-acb-600'
  return v > 0 ? 'text-positive font-semibold' : 'text-negative font-semibold'
}

function TeamLogo({ team, teamLogos, size = 'lg' }) {
  const logo = teamLogos?.[team]
  const cls = size === 'sm' ? 'w-8 h-8' : 'w-14 h-14'
  if (logo) return <img src={logo} alt={team} className={`${cls} object-contain`} />
  return <div className={`${cls} rounded-full bg-acb-100 border border-acb-200`} />
}

function TeamSelector({ label, teams, selected, onChange, teamLogos, profileUrl }) {
  const selectId = label === 'Equipo A' ? 'matchup-team-a' : 'matchup-team-b'
  return (
    <div className="bg-white rounded-lg border border-acb-200 p-4">
      <div className="flex items-center justify-between gap-2 mb-3">
        <label htmlFor={selectId} className="font-semibold text-acb-900">{label}</label>
        {profileUrl && selected && (
          <Link to={profileUrl} className="text-xs text-acb-400 hover:text-acb-700 transition-colors" title="Ver perfil del equipo">
            Ver perfil →
          </Link>
        )}
      </div>
      <div className="flex items-center gap-3">
        <TeamLogo team={selected} teamLogos={teamLogos} size="sm" />
        <select
          id={selectId}
          value={selected || ''}
          onChange={e => onChange(e.target.value)}
          className="w-full px-3 py-2.5 border border-acb-200 rounded-lg text-sm bg-white"
        >
          <option value="">Selecciona un equipo</option>
          {teams.map(t => <option key={t.team} value={t.team}>{t.team}</option>)}
        </select>
      </div>
    </div>
  )
}

const radarAxes = [
  { key: 'ortg', label: 'Ataque', inverse: false },
  { key: 'drtg', label: 'Defensa', inverse: true },
  { key: 'pace', label: 'Ritmo', inverse: false },
  { key: 'ts', label: 'Ef. Tiro', inverse: false },
  { key: 'threeRate', label: 'Vol. 3P', inverse: false },
  { key: 'opp_tovRate', label: 'Presión', inverse: false },
  { key: 'orbPct', label: 'RO%', inverse: false },
  { key: 'drbPct', label: 'RD%', inverse: false },
]

function RadarOverlay({ teamA, teamB, league }) {
  const size = 360
  const cx = size / 2
  const cy = size / 2
  const radius = 100
  const levels = [25, 50, 75, 100]
  const n = radarAxes.length
  const valsA = radarAxes.map(a => percentileRank(league, a.key, teamA?.[a.key], a.inverse))
  const valsB = radarAxes.map(a => percentileRank(league, a.key, teamB?.[a.key], a.inverse))
  const hasRadarA = valsA.every(v => v != null)
  const hasRadarB = valsB.every(v => v != null)
  const angle = i => (Math.PI * 2 * i) / n - Math.PI / 2
  const point = (i, pct) => {
    const r = (Math.max(4, Math.min(100, pct)) / 100) * radius
    return [cx + r * Math.cos(angle(i)), cy + r * Math.sin(angle(i))]
  }
  const path = values => values
    .map((v, i) => point(i, v))
    .map((p, i) => `${i === 0 ? 'M' : 'L'}${p[0].toFixed(1)},${p[1].toFixed(1)}`)
    .join(' ') + ' Z'
  const gridPath = level => Array.from({ length: n }, (_, i) => point(i, level))
    .map((p, i) => `${i === 0 ? 'M' : 'L'}${p[0].toFixed(1)},${p[1].toFixed(1)}`)
    .join(' ') + ' Z'
  const anchor = i => {
    const x = Math.cos(angle(i))
    if (x < -0.3) return 'end'
    if (x > 0.3) return 'start'
    return 'middle'
  }

  return (
    <div className="min-w-0 overflow-hidden bg-white rounded-lg border border-acb-200 p-5">
      <div className="flex items-start justify-between gap-3 mb-2">
        <div>
          <h3 className="font-semibold text-acb-900">Radar de cara a cara</h3>
          <p className="text-xs text-acb-500">Percentiles de liga por temporada</p>
        </div>
        <div className="flex flex-wrap gap-3 text-xs">
          <span className="flex items-center gap-1"><span className="w-3 h-3 rounded-full bg-accent-500" />{teamA?.team}</span>
          <span className="flex items-center gap-1"><span className="w-3 h-3 rounded-full bg-info-500" />{teamB?.team}</span>
        </div>
      </div>
      <svg viewBox={`0 0 ${size} ${size}`} className="w-full max-w-[420px] mx-auto" overflow="visible">
        {levels.map(level => (
          <path key={level} d={gridPath(level)} fill="none" stroke={level === 50 ? '#94a3b8' : '#e2e8f0'} strokeWidth={level === 50 ? 1.2 : 0.7} strokeDasharray={level === 50 ? '' : '2,2'} />
        ))}
        {radarAxes.map((_, i) => {
          const [x, y] = point(i, 100)
          return <line key={i} x1={cx} y1={cy} x2={x} y2={y} stroke="#e2e8f0" strokeWidth={0.7} />
        })}
        {hasRadarA && <path d={path(valsA)} fill="rgba(254, 89, 23, 0.18)" stroke="#fe5917" strokeWidth={2.5} />}
        {hasRadarB && <path d={path(valsB)} fill="rgba(59, 130, 246, 0.16)" stroke="#3b82f6" strokeWidth={2.5} />}
        {valsA.map((v, i) => {
          if (v == null) return null
          const [x, y] = point(i, v)
          return <circle key={`a-${i}`} cx={x} cy={y} r={3.5} fill="#fe5917" stroke="white" strokeWidth={1.5} />
        })}
        {valsB.map((v, i) => {
          if (v == null) return null
          const [x, y] = point(i, v)
          return <circle key={`b-${i}`} cx={x} cy={y} r={3.5} fill="#3b82f6" stroke="white" strokeWidth={1.5} />
        })}
        {radarAxes.map((axis, i) => {
          const [x, y] = point(i, 123)
          return (
            <text key={axis.key} x={x} y={y} textAnchor={anchor(i)} dominantBaseline="central" className="fill-acb-600 text-[11px] font-medium">
              {axis.label}
            </text>
          )
        })}
      </svg>
    </div>
  )
}

function SummaryCard({ team, league, pace, clutch, teamLogos, accent }) {
  if (!team) return null
  const attack = percentileRank(league, 'ortg', team.ortg)
  const defense = percentileRank(league, 'drtg', team.drtg, true)
  const pressure = percentileRank(league, 'opp_tovRate', team.opp_tovRate)
  const offensiveRebound = percentileRank(league, 'orbPct', team.orbPct)
  const defensiveRebound = percentileRank(league, 'drbPct', team.drbPct)
  const rebound = offensiveRebound == null || defensiveRebound == null
    ? null
    : (offensiveRebound + defensiveRebound) / 2
  const clutchNet = clutch?.netRtg
  const qs = pace?.quarters?.diff || []
  const bestQ = qs.length ? qs.indexOf(Math.max(...qs)) + 1 : null
  const worstQ = qs.length ? qs.indexOf(Math.min(...qs)) + 1 : null

  return (
    <div className="bg-white rounded-lg border border-acb-200 p-5">
      <div className="flex items-start justify-between gap-4">
        <div className="flex items-center gap-4 min-w-0">
          <TeamLogo team={team.team} teamLogos={teamLogos} />
          <div className="min-w-0">
            <h3 className="text-xl font-bold text-acb-900 truncate">{team.team}</h3>
            <p className="text-sm text-acb-500">{team.wins}-{team.losses} - {team.games} PJ</p>
          </div>
        </div>
        <span className={`h-2.5 w-2.5 rounded-full mt-2 ${accent}`} />
      </div>
      <div className="grid grid-cols-2 sm:grid-cols-4 gap-2 mt-4">
        {[
          ['ORtg', team.ortg, 'decimal'],
          ['DRtg', team.drtg, 'decimal'],
          ['Neto', team.netRtg, 'signed'],
          ['Pace', team.pace, 'decimal'],
        ].map(([label, value, type]) => (
          <div key={label} className="bg-acb-50 rounded-md px-2 py-2 text-center">
            <div className="font-mono text-sm font-semibold text-acb-900">{fmt(value, type)}</div>
            <div className="text-[11px] text-acb-500">{label}</div>
          </div>
        ))}
      </div>
      <div className="mt-4 grid sm:grid-cols-2 gap-2 text-xs">
        {[
          ['Ataque', attack],
          ['Defensa', defense],
          ['Presión', pressure],
          ['Rebote', rebound],
        ].map(([label, score]) => (
          <div key={label} className="flex items-center justify-between gap-2">
            <span className="text-acb-500">{label}</span>
            <span className="px-2 py-0.5 rounded bg-acb-100 text-acb-700 font-mono font-semibold">
              {score == null ? '—' : score.toFixed(0)}
            </span>
          </div>
        ))}
        <div className="flex items-center justify-between gap-2">
          <span className="text-acb-500">Neto clutch</span>
          <span className={`font-mono ${edgeClass(clutchNet)}`}>{fmt(clutchNet, 'signed')}</span>
        </div>
        <div className="flex items-center justify-between gap-2">
          <span className="text-acb-500">Cuartos</span>
          <span className="font-mono text-acb-700">
            {bestQ ? `Mejor: Q${bestQ} (${fmt(qs[bestQ - 1], 'signed')}) · Peor: Q${worstQ} (${fmt(qs[worstQ - 1], 'signed')})` : '—'}
          </span>
        </div>
      </div>
    </div>
  )
}

const sections = [
  {
    title: 'Eficiencia',
    metrics: [
      { key: 'ortg', label: 'ORtg', type: 'decimal', scale: 12 },
      { key: 'drtg', label: 'DRtg', type: 'decimal', lower: true, scale: 12 },
      { key: 'netRtg', label: 'Neto', type: 'signed', scale: 15 },
      { key: 'efg', label: 'eFG%', type: 'pct', scale: 0.08 },
      { key: 'ts', label: 'TS%', type: 'pct', scale: 0.08 },
    ],
  },
  {
    title: 'Ritmo y tiro',
    metrics: [
      { key: 'pace', label: 'Pace', type: 'decimal', scale: 8 },
      { key: 'threeRate', label: 'Vol. 3P', type: 'pct', scale: 0.15 },
      { key: 'threePct', label: '3P%', type: 'pct', scale: 0.12 },
      { key: 'ftRate', label: 'FTr', type: 'pct', scale: 0.3 },
      { key: 'assistedFgm', label: '% asistidos', type: 'pct100', scale: 20 },
    ],
  },
  {
    title: 'Presión y rebote',
    metrics: [
      { key: 'tovRate', label: 'PER%', type: 'pct', lower: true, scale: 0.06 },
      { key: 'opp_tovRate', label: 'PER% rival', type: 'pct', scale: 0.06 },
      { key: 'stlRate', label: 'ROB%', type: 'pct', scale: 0.04 },
      { key: 'orbPct', label: 'RO%', type: 'pct', scale: 0.10 },
      { key: 'drbPct', label: 'RD%', type: 'pct', scale: 0.10 },
    ],
  },
]

function StatComparison({ a, b }) {
  return (
    <div className="bg-white rounded-lg border border-acb-200 overflow-hidden">
      <div className="px-5 py-3 border-b border-acb-200">
        <h3 className="font-semibold text-acb-900">Fortalezas y debilidades</h3>
        <p className="text-xs text-acb-500 mt-0.5">La barra central indica qué equipo es mejor y por cuánto</p>
      </div>
      <div className="grid grid-cols-[minmax(80px,1fr)_64px_56px_64px] sm:grid-cols-[minmax(110px,1fr)_minmax(80px,108px)_80px_minmax(80px,108px)] items-center gap-1 sm:gap-2 px-2 sm:px-4 py-2.5 bg-acb-50 border-b border-acb-200 text-[11px] font-semibold uppercase text-acb-500">
        <div>Métrica</div>
        <div className="text-right flex items-center justify-end gap-1">
          <span className="w-2 h-2 rounded-full bg-accent-500 shrink-0" />
          <span className="truncate" title={a?.team}>{a?.team || 'Equipo A'}</span>
        </div>
        <div className="text-center">Mejor por</div>
        <div className="text-right flex items-center justify-end gap-1">
          <span className="w-2 h-2 rounded-full bg-info-500 shrink-0" />
          <span className="truncate" title={b?.team}>{b?.team || 'Equipo B'}</span>
        </div>
      </div>
      <div className="divide-y divide-acb-100">
        {sections.map(section => {
          return (
            <div key={section.title} className="p-4">
              <div className="flex items-center gap-2 mb-3">
                <h4 className="text-sm font-semibold text-acb-800">{section.title}</h4>
              </div>
              <div className="space-y-2.5">
                {section.metrics.map(m => {
                  const av = num(a?.[m.key])
                  const bv = num(b?.[m.key])
                  const diff = av == null || bv == null ? null : (m.lower ? bv - av : av - bv)
                  const barPct = diff != null && m.scale ? Math.min(100, (Math.abs(diff) / m.scale) * 100) : 0
                  return (
                    <div key={m.key} className="grid grid-cols-[minmax(80px,1fr)_64px_56px_64px] sm:grid-cols-[minmax(110px,1fr)_minmax(80px,108px)_80px_minmax(80px,108px)] items-center gap-1 sm:gap-2 text-sm">
                      <div className="text-acb-600 truncate">{m.label}</div>
                      <div className={`font-mono text-right text-acb-700 ${diff != null && diff > 0 ? 'font-semibold' : ''}`}>{fmt(av, m.type)}</div>
                      <div className="flex flex-col items-center gap-0.5">
                        <span className="font-mono text-[10px] text-acb-500">{fmtAdvantage(diff, m.type)}</span>
                        {diff != null && (
                          <div className="flex w-full h-[3px]">
                            <div className="flex-1 flex justify-end overflow-hidden">
                              {diff > 0 && <div className="h-full bg-accent-400 rounded-l-full" style={{ width: `${barPct}%` }} />}
                            </div>
                            <div className="w-px bg-acb-200 shrink-0" />
                            <div className="flex-1 overflow-hidden">
                              {diff < 0 && <div className="h-full bg-info-400 rounded-r-full" style={{ width: `${barPct}%` }} />}
                            </div>
                          </div>
                        )}
                      </div>
                      <div className={`font-mono text-right text-acb-700 ${diff != null && diff < 0 ? 'font-semibold' : ''}`}>{fmt(bv, m.type)}</div>
                    </div>
                  )
                })}
              </div>
            </div>
          )
        })}
      </div>
    </div>
  )
}


function ShotProfile({ teamA, teamB, shots, isLoading, mode, metric, onModeChange, onMetricChange }) {
  const shotsA = useMemo(() => mode === 'attack'
    ? shots.filter(s => s.team === teamA?.team)
    : shots.filter(s => s.opponent === teamA?.team), [shots, teamA, mode])
  const shotsB = useMemo(() => mode === 'attack'
    ? shots.filter(s => s.team === teamB?.team)
    : shots.filter(s => s.opponent === teamB?.team), [shots, teamB, mode])

  const toggleBtn = (active, onClick, label) => (
    <button
      type="button"
      onClick={onClick}
      aria-pressed={active}
      className={`px-2.5 py-1 text-xs rounded transition-colors ${active ? 'bg-acb-800 text-white font-medium' : 'bg-acb-100 text-acb-600 hover:bg-acb-200'}`}
    >
      {label}
    </button>
  )

  return (
    <div className="bg-white rounded-lg border border-acb-200 p-5">
      <div className="flex flex-wrap items-start justify-between gap-3 mb-4">
        <div>
          <h3 className="font-semibold text-acb-900">Perfil de tiro por zonas</h3>
          <p className="text-xs text-acb-500">Comparado con la media de la liga en cada zona</p>
        </div>
        <div className="flex flex-wrap gap-2">
          <div className="flex gap-1">
            {toggleBtn(mode === 'attack', () => onModeChange('attack'), 'Ataque')}
            {toggleBtn(mode === 'defense', () => onModeChange('defense'), 'Defensa')}
          </div>
          <div className="flex gap-1">
            {toggleBtn(metric === 'efficiency', () => onMetricChange('efficiency'), 'Eficiencia')}
            {toggleBtn(metric === 'frequency', () => onMetricChange('frequency'), 'Frecuencia')}
          </div>
        </div>
      </div>
      {isLoading ? (
        <div className="text-center py-10 text-acb-400">Cargando tiros...</div>
      ) : (
        <div className="grid min-w-0 lg:grid-cols-2 gap-6">
          <div className="min-w-0">
            <div className="flex items-center gap-2 mb-2 text-sm font-semibold text-acb-800">
              <span className="w-2.5 h-2.5 rounded-full bg-accent-500 inline-block" />
              {teamA?.team}
            </div>
            <div className="overflow-x-auto">
              <ZoneHeatmap shots={shotsA} leagueShots={shots} metric={metric} higherIsBetter={mode !== 'defense'} width={430} height={405} />
            </div>
          </div>
          <div className="min-w-0">
            <div className="flex items-center gap-2 mb-2 text-sm font-semibold text-acb-800">
              <span className="w-2.5 h-2.5 rounded-full bg-info-500 inline-block" />
              {teamB?.team}
            </div>
            <div className="overflow-x-auto">
              <ZoneHeatmap shots={shotsB} leagueShots={shots} metric={metric} higherIsBetter={mode !== 'defense'} width={430} height={405} />
            </div>
          </div>
        </div>
      )}
    </div>
  )
}

function PaceFlow({ a, b }) {
  const maxAbs = Math.max(
    1,
    ...[...(a?.quarters?.diff || []), ...(b?.quarters?.diff || [])].map(v => Math.abs(v))
  )
  return (
    <div className="bg-white rounded-lg border border-acb-200 p-5">
      <div className="flex items-start justify-between gap-3 mb-4">
        <div>
          <h3 className="font-semibold text-acb-900">Diferencial por cuartos</h3>
          <p className="text-xs text-acb-500">Diferencial medio de anotación en cada cuarto</p>
        </div>
      </div>
      <div className="space-y-4">
        {[a, b].map((row, idx) => (
          <div key={idx}>
            <div className="mb-2">
              <span className="text-sm font-semibold text-acb-800">{row?.team || '-'}</span>
            </div>
            <div className="grid grid-cols-4 gap-2">
              {[0, 1, 2, 3].map(i => {
                const v = row?.quarters?.diff?.[i]
                const width = v == null ? 0 : Math.abs(v) / maxAbs * 100
                return (
                  <div key={i} className="bg-acb-50 rounded-md p-2">
                    <div className="flex items-center justify-between text-xs mb-1">
                      <span className="font-semibold text-acb-500">Q{i + 1}</span>
                      <span className={`font-mono ${edgeClass(v)}`}>{fmt(v, 'signed')}</span>
                    </div>
                    <div className="h-1.5 rounded-full bg-acb-100 overflow-hidden">
                      <div className={`h-full ${v >= 0 ? 'bg-positive-500' : 'bg-negative-500'}`} style={{ width: `${width}%` }} />
                    </div>
                  </div>
                )
              })}
            </div>
            {row?.afterTimeout?.ppp != null && (
              <div className="mt-2 pt-2 border-t border-acb-100 flex items-center justify-between text-xs">
                <span className="text-acb-500">
                  <span className="block font-medium text-acb-700">Anotación tras tiempo muerto</span>
                  <span>pts por tiempo muerto (30 s)</span>
                </span>
                <span className="font-mono text-acb-700">
                  {fmt(row.afterTimeout.ppp)} pts
                  {row.afterTimeout.leaguePpp != null && (
                    <span className="text-acb-400 font-normal"> (media liga: {fmt(row.afterTimeout.leaguePpp)})</span>
                  )}
                </span>
              </div>
            )}
          </div>
        ))}
      </div>
    </div>
  )
}

function ClutchPanel({ a, b, loading, teamA, teamB }) {
  const metrics = [
    ['Balance', r => r ? `${r.wins}-${r.losses}` : '-'],
    ['Ef. Neta', r => fmt(r?.netRtg, 'signed')],
    ['ORtg', r => fmt(r?.ortg)],
    ['DRtg', r => fmt(r?.drtg)],
    ['TC%/eFG%', r => `${fmt(r?.fgPct, 'pct100')} / ${fmt(r?.efgPct, 'pct100')}`],
    ['PER%', r => fmt(r?.tovRate, 'pct100')],
  ]
  return (
    <div className="bg-white rounded-lg border border-acb-200 p-5">
      <div className="flex items-start justify-between gap-3 mb-4">
        <div>
          <h3 className="font-semibold text-acb-900">Clutch</h3>
          <p className="text-xs text-acb-500">Últimos 5 minutos con diferencia de 5 puntos o menos</p>
        </div>
      </div>
      {loading ? (
        <div className="text-center py-8 text-acb-400">Cargando clutch...</div>
      ) : (
        <div className="space-y-2">
          <div className="grid grid-cols-[1fr_1fr_1fr] gap-3 pb-2 mb-1 border-b border-acb-100 text-[11px] font-semibold text-acb-500">
            <div />
            <div className="flex items-center justify-end gap-1">
              <span className="w-2 h-2 rounded-full bg-accent-500 shrink-0" />
              <span className="truncate" title={teamA?.team}>{teamA?.team || 'Equipo A'}</span>
            </div>
            <div className="flex items-center justify-end gap-1">
              <span className="w-2 h-2 rounded-full bg-info-500 shrink-0" />
              <span className="truncate" title={teamB?.team}>{teamB?.team || 'Equipo B'}</span>
            </div>
          </div>
          {metrics.map(([label, get]) => (
            <div key={label} className="grid grid-cols-[1fr_1fr_1fr] gap-3 text-sm">
              <div className="text-acb-500">{label}</div>
              <div className="font-mono text-right text-acb-800">{get(a)}</div>
              <div className="font-mono text-right text-acb-800">{get(b)}</div>
            </div>
          ))}
        </div>
      )}
    </div>
  )
}

function buildNotes(a, b, league, paceA, paceB, clutchA, clutchB) {
  if (!a || !b) return []
  const safe = v => num(v) ?? Number.NaN
  const n1 = v => num(v) == null ? '—' : Number(v).toFixed(1)
  const pRank = (key, val, inv) => percentileRank(league, key, val, inv) ?? Number.NaN
  const mk = (label, winner, isA, stat, desc, value) => ({ label, winner, isA, stat, desc, value })

  const notes = []

  // offensive rating
  const ortgDiff = safe(a.ortg) - safe(b.ortg)
  if (Math.abs(ortgDiff) > 1.5) {
    const isA = ortgDiff > 0
    const [w, l] = isA ? [a, b] : [b, a]
    notes.push(mk('Ataque', w.team, isA, `${n1(w.ortg)} vs ${n1(l.ortg)} ORtg`,
      'Mayor eficiencia ofensiva por posesión.', Math.abs(ortgDiff) * 2))
  }

  // defensive rating (lower is better)
  const drtgDiff = safe(a.drtg) - safe(b.drtg)
  if (Math.abs(drtgDiff) > 1.5) {
    const isA = drtgDiff < 0
    const [w, l] = isA ? [a, b] : [b, a]
    notes.push(mk('Defensa', w.team, isA, `${n1(w.drtg)} vs ${n1(l.drtg)} DRtg`,
      'Menos puntos concedidos por posesión.', Math.abs(drtgDiff) * 2))
  }

  // pace
  const paceDiff = safe(a.pace) - safe(b.pace)
  if (Math.abs(paceDiff) > 1.5) {
    const isA = paceDiff > 0
    const [fast, slow] = isA ? [a, b] : [b, a]
    const gap = Math.abs(paceDiff)
    notes.push(mk('Ritmo', fast.team, isA, `${n1(fast.pace)} vs ${n1(slow.pace)} pos/40`,
      gap > 4 ? 'Diferencia de ritmo marcada entre ambos equipos.' : 'Juega a mayor cadencia que el rival.',
      gap * 3))
  }

  // three-point profile (rate × pct combined)
  const score3A = safe(a.threeRate) * safe(a.threePct) * 100
  const score3B = safe(b.threeRate) * safe(b.threePct) * 100
  const diff3 = score3A - score3B
  if (Math.abs(diff3) > 0.5) {
    const isA = diff3 > 0
    const [better, worse] = isA ? [a, b] : [b, a]
    notes.push(mk('Triple', better.team, isA,
      `vol. ${(safe(better.threeRate) * 100).toFixed(1)}% · 3P ${(safe(better.threePct) * 100).toFixed(1)}% vs vol. ${(safe(worse.threeRate) * 100).toFixed(1)}% · 3P ${(safe(worse.threePct) * 100).toFixed(1)}%`,
      'Mayor producción estimada de triple al combinar volumen y acierto.', Math.abs(diff3) * 5))
  }

  // forced turnovers
  const oppTovDiff = pRank('opp_tovRate', a.opp_tovRate) - pRank('opp_tovRate', b.opp_tovRate)
  if (Math.abs(oppTovDiff) > 12) {
    const isA = oppTovDiff > 0
    const [press, noPress] = isA ? [a, b] : [b, a]
    notes.push(mk('Presión', press.team, isA,
      `${(safe(press.opp_tovRate) * 100).toFixed(1)}% vs ${(safe(noPress.opp_tovRate) * 100).toFixed(1)}% PER% rival`,
      'Fuerza más pérdidas al rival por posesión.', Math.abs(oppTovDiff)))
  }

  // own turnovers (lower is better)
  const tovA = safe(a.tovRate); const tovB = safe(b.tovRate)
  const tovDiff = tovA - tovB
  if (Math.abs(tovDiff) > 0.025) {
    const isA = tovDiff < 0
    const [clean] = isA ? [a, b] : [b, a]
    const cv = isA ? tovA : tovB; const mv = isA ? tovB : tovA
    notes.push(mk('Pérdidas', clean.team, isA, `${(cv * 100).toFixed(1)}% vs ${(mv * 100).toFixed(1)}% pérdidas`,
      'Menor tasa de pérdidas propias.', Math.abs(tovDiff) * 150))
  }

  // offensive rebounding
  const orbA = safe(a.orbPct); const orbB = safe(b.orbPct)
  const orbDiff = orbA - orbB
  if (Math.abs(orbDiff) > 0.03) {
    const isA = orbDiff > 0
    const [bOrb] = isA ? [a, b] : [b, a]
    const bv = isA ? orbA : orbB; const wv = isA ? orbB : orbA
    notes.push(mk('Reb. ofensivo', bOrb.team, isA, `${(bv * 100).toFixed(1)}% vs ${(wv * 100).toFixed(1)}% RO%`,
      'Más segundas oportunidades de ataque.', Math.abs(orbDiff) * 120))
  }

  // defensive rebounding
  const drbA = safe(a.drbPct); const drbB = safe(b.drbPct)
  const drbDiff = drbA - drbB
  if (Math.abs(drbDiff) > 0.03) {
    const isA = drbDiff > 0
    const [bDrb] = isA ? [a, b] : [b, a]
    const bv = isA ? drbA : drbB; const wv = isA ? drbB : drbA
    notes.push(mk('Reb. defensivo', bDrb.team, isA, `${(bv * 100).toFixed(1)}% vs ${(wv * 100).toFixed(1)}% RD%`,
      'Mayor control del rebote defensivo.', Math.abs(drbDiff) * 100))
  }

  // clutch
  const cnA = safe(clutchA?.netRtg); const cnB = safe(clutchB?.netRtg)
  const clutchDiff = cnA - cnB
  if (Math.abs(clutchDiff) > 3) {
    const isA = clutchDiff > 0
    const [bClutch] = isA ? [a, b] : [b, a]
    const [cb, cw] = isA ? [clutchA, clutchB] : [clutchB, clutchA]
    notes.push(mk('Clutch', bClutch.team, isA, `neto ${n1(cb?.netRtg)} vs ${n1(cw?.netRtg)}`,
      'Mejor rendimiento neto en los últimos 5 minutos.', Math.abs(clutchDiff) * 2))
  }

  // quarter clashes
  const diffA = paceA?.quarters?.diff?.map(num) || []
  const diffB = paceB?.quarters?.diff?.map(num) || []
  if (diffA.length === 4 && diffB.length === 4 && diffA.every(v => v != null) && diffB.every(v => v != null)) {
    const qNames = ['1er cuarto', '2º cuarto', '3er cuarto', '4º cuarto']
    const bestQA = diffA.indexOf(Math.max(...diffA))
    const bestQB = diffB.indexOf(Math.max(...diffB))
    const worstQA = diffA.indexOf(Math.min(...diffA))
    const worstQB = diffB.indexOf(Math.min(...diffB))

    if (bestQA === worstQB && diffA[bestQA] > 1 && diffB[worstQB] < -1) {
      notes.push(mk(qNames[bestQA], a.team, true, `${n1(diffA[bestQA])} vs ${n1(diffB[worstQB])} dif. media`,
        'Tramo fuerte de uno, flojo del otro.', diffA[bestQA] - diffB[worstQB]))
    } else if (bestQB === worstQA && diffB[bestQB] > 1 && diffA[worstQA] < -1) {
      notes.push(mk(qNames[bestQB], b.team, false, `${n1(diffB[bestQB])} vs ${n1(diffA[worstQA])} dif. media`,
        'Tramo fuerte de uno, flojo del otro.', diffB[bestQB] - diffA[worstQA]))
    }

    const sumA = diffA.reduce((s, v) => s + v, 0)
    const sumB = diffB.reduce((s, v) => s + v, 0)
    const qSumDiff = sumA - sumB
    if (Math.abs(qSumDiff) > 3) {
      const isA = qSumDiff > 0
      const [bQ] = isA ? [a, b] : [b, a]
      const bSum = isA ? sumA : sumB; const wSum = isA ? sumB : sumA
      notes.push(mk('Parciales', bQ.team, isA, `${n1(bSum)} vs ${n1(wSum)} suma dif.`,
        'Mejor diferencial agregado por cuartos.', Math.abs(qSumDiff) * 2))
    }
  }

  // free throw rate
  const ftrA = safe(a.ftRate); const ftrB = safe(b.ftRate)
  const ftrDiff = ftrA - ftrB
  if (Math.abs(ftrDiff) > 0.04) {
    const isA = ftrDiff > 0
    const [aggr] = isA ? [a, b] : [b, a]
    const av = isA ? ftrA : ftrB; const bv = isA ? ftrB : ftrA
    notes.push(mk('Tiros libres', aggr.team, isA, `FTr ${(av * 100).toFixed(1)}% vs ${(bv * 100).toFixed(1)}%`,
      'Llega más a la línea de personal.', Math.abs(ftrDiff) * 80))
  }

  // true shooting
  const tsA = safe(a.ts); const tsB = safe(b.ts)
  const tsDiff = tsA - tsB
  if (Math.abs(tsDiff) > 0.02) {
    const isA = tsDiff > 0
    const [bTS] = isA ? [a, b] : [b, a]
    const bv = isA ? tsA : tsB; const wv = isA ? tsB : tsA
    notes.push(mk('Eficiencia', bTS.team, isA, `TS% ${(bv * 100).toFixed(1)} vs ${(wv * 100).toFixed(1)}`,
      'Mayor eficiencia global de tiro.', Math.abs(tsDiff) * 100))
  }

  // team play
  const asA = safe(a.assistedFgm); const asB = safe(b.assistedFgm)
  const asDiff = asA - asB
  if (Math.abs(asDiff) > 7) {
    const isA = asDiff > 0
    const [col] = isA ? [a, b] : [b, a]
    const tv = isA ? asA : asB; const iv = isA ? asB : asA
    notes.push(mk('Juego colectivo', col.team, isA, `${n1(tv)}% vs ${n1(iv)}% tiros asistidos`,
      'Mayor porcentaje de canastas precedidas de asistencia.', Math.abs(asDiff) * 0.8))
  }

  return notes
    .filter(c => c.value > 0)
    .sort((x, y) => y.value - x.value)
    .slice(0, 7)
}

function NotesPanel({ notes }) {
  if (!notes.length) return (
    <div className="bg-white rounded-lg border border-acb-200 p-5 flex items-center justify-center">
      <span className="text-sm text-acb-400">Sin datos suficientes</span>
    </div>
  )
  return (
    <div className="bg-white rounded-lg border border-acb-200 p-5">
      <div className="mb-4">
        <h3 className="font-semibold text-acb-900">Ventajas clave</h3>
        <p className="text-xs text-acb-500 mt-0.5">Principales diferencias entre ambos equipos esta temporada</p>
      </div>
      <div className="space-y-2.5">
        {notes.map(note => (
          <div key={note.label} className={`border-l-2 pl-3 py-0.5 ${note.isA ? 'border-accent-400' : 'border-info-400'}`}>
            <div className="flex items-center gap-2 flex-wrap">
              <span className={`text-xs font-semibold px-1.5 py-0.5 rounded shrink-0 ${note.isA ? 'bg-accent-100 text-accent-700' : 'bg-info-100 text-info-700'}`}>
                {note.winner}
              </span>
              <span className="text-[10px] font-bold uppercase tracking-wider text-acb-400">{note.label}</span>
              <span className="text-[11px] font-mono text-acb-400 ml-auto shrink-0">{note.stat}</span>
            </div>
            <p className="text-xs text-acb-600 mt-0.5">{note.desc}</p>
          </div>
        ))}
      </div>
    </div>
  )
}

export default function TeamMatchup({
  teams,
  teamLogos = {},
  loadTeamPaceForSeason,
  teamPaceCache,
  loadingTeamPace,
  loadClutchForSeason,
  clutchCache,
  loadingClutch,
  loadShotsForSeason,
  shotsCache,
  loadingShots,
}) {
  const navigate = useNavigate()
  const routeParams = useParams()
  const [searchParams, setSearchParams] = useSearchParams()
  const urlTeamA = routeParams.teamAId || routeParams.teamA || ''
  const urlTeamB = routeParams.teamBId || routeParams.teamB || ''

  const availableSeasons = useMemo(() => [...new Set(teams.map(t => t.season))].sort((a, b) => b - a), [teams])
  const teamIdentityIndex = useMemo(() => createTeamIdentityIndexFromRows(teams), [teams])
  const queryState = readRouteQuery('teamComparison', searchParams, {
    defaults: { temporada: Number(routeParams.season) || availableSeasons[0] || 2026 },
  })
  const selectedSeason = parseSeasonParam(queryState.temporada, availableSeasons)
  const mode = queryState.lado
  const metric = queryState.metrica
  const seasonTeams = useMemo(() => teams.filter(t => t.season === selectedSeason).sort((a, b) => a.team.localeCompare(b.team)), [teams, selectedSeason])
  const resolvedUrlTeamA = resolveTeamId(teamIdentityIndex, urlTeamA, selectedSeason)
  const resolvedUrlTeamB = resolveTeamId(teamIdentityIndex, urlTeamB, selectedSeason)

  const recordA = useMemo(() => seasonTeams.find(row => (
    resolveTeamId(teamIdentityIndex, row.teamId || row.team, selectedSeason) === resolvedUrlTeamA
  )) || null, [resolvedUrlTeamA, seasonTeams, selectedSeason, teamIdentityIndex])
  const recordB = useMemo(() => seasonTeams.find(row => (
    resolveTeamId(teamIdentityIndex, row.teamId || row.team, selectedSeason) === resolvedUrlTeamB
  )) || null, [resolvedUrlTeamB, seasonTeams, selectedSeason, teamIdentityIndex])

  const [draftTeamA, setDraftTeamA] = useState('')
  const [draftTeamB, setDraftTeamB] = useState('')
  const hasExplicitPair = Boolean(urlTeamA || urlTeamB)
  const previousExplicitPairRef = useRef(hasExplicitPair)
  const teamA = recordA?.team || draftTeamA
  const teamB = recordB?.team || draftTeamB

  useEffect(() => {
    if (previousExplicitPairRef.current && !hasExplicitPair) {
      setDraftTeamA('')
      setDraftTeamB('')
    }
    previousExplicitPairRef.current = hasExplicitPair
  }, [hasExplicitPair])

  useEffect(() => {
    if (!availableSeasons.length) return
    const canonical = buildTeamComparisonSearch({ season: selectedSeason, mode, metric })
    if (canonical.toString() !== searchParams.toString()) {
      setSearchParams(canonical, { replace: true })
    }
  }, [availableSeasons.length, metric, mode, searchParams, selectedSeason, setSearchParams])

  useEffect(() => {
    const names = new Set(seasonTeams.map(t => t.team))
    setDraftTeamA(current => names.has(current) ? current : '')
    setDraftTeamB(current => names.has(current) ? current : '')
  }, [seasonTeams])

  useEffect(() => {
    if (!urlTeamA || !urlTeamB || !resolvedUrlTeamA || !resolvedUrlTeamB) return
    if (urlTeamA === resolvedUrlTeamA && urlTeamB === resolvedUrlTeamB && !routeParams.season) return
    const search = buildTeamComparisonSearch({ season: selectedSeason, mode, metric }).toString()
    navigate({
      pathname: buildTeamComparisonPath(resolvedUrlTeamA, resolvedUrlTeamB),
      search: `?${search}`,
    }, { replace: true })
  }, [metric, mode, navigate, resolvedUrlTeamA, resolvedUrlTeamB, routeParams.season, selectedSeason, urlTeamA, urlTeamB])

  useEffect(() => {
    if (!selectedSeason) return
    loadTeamPaceForSeason(selectedSeason)
    loadClutchForSeason(selectedSeason)
    if (selectedSeason >= 2021) loadShotsForSeason(selectedSeason)
  }, [selectedSeason, loadTeamPaceForSeason, loadClutchForSeason, loadShotsForSeason])

  const updateUrlState = (changes) => {
    setSearchParams(buildTeamComparisonSearch({
      season: selectedSeason,
      mode,
      metric,
      ...changes,
    }))
  }

  const teamIdFromName = (name) => {
    const row = seasonTeams.find(team => team.team === name)
    return row ? resolveTeamId(teamIdentityIndex, row.teamId || row.team, selectedSeason) : null
  }

  const navigateToComparison = (teamAId, teamBId) => {
    if (!teamAId || !teamBId || teamAId === teamBId) return false
    const search = buildTeamComparisonSearch({ season: selectedSeason, mode, metric }).toString()
    navigate({ pathname: buildTeamComparisonPath(teamAId, teamBId), search: `?${search}` })
    return true
  }

  const handleTeamAChange = (name) => {
    const nextTeamAId = teamIdFromName(name)
    const currentTeamBId = recordB ? resolvedUrlTeamB : teamIdFromName(draftTeamB)
    setDraftTeamA(name)
    navigateToComparison(nextTeamAId, currentTeamBId)
  }

  const handleTeamBChange = (name) => {
    const currentTeamAId = recordA ? resolvedUrlTeamA : teamIdFromName(draftTeamA)
    const nextTeamBId = teamIdFromName(name)
    setDraftTeamB(name)
    navigateToComparison(currentTeamAId, nextTeamBId)
  }

  const handleSwapTeams = () => {
    const currentTeamAId = recordA ? resolvedUrlTeamA : teamIdFromName(draftTeamA)
    const currentTeamBId = recordB ? resolvedUrlTeamB : teamIdFromName(draftTeamB)
    if (!navigateToComparison(currentTeamBId, currentTeamAId)) {
      setDraftTeamA(draftTeamB)
      setDraftTeamB(draftTeamA)
    }
  }

  const paceRows = teamPaceCache[selectedSeason] || []
  const clutchRows = clutchCache[selectedSeason]?.teams || []
  const shotRows = shotsCache[selectedSeason] || []
  const paceA = paceRows.find(t => t.team === teamA)
  const paceB = paceRows.find(t => t.team === teamB)
  const clutchA = clutchRows.find(t => t.team === teamA)
  const clutchB = clutchRows.find(t => t.team === teamB)
  const notes = useMemo(() => buildNotes(recordA, recordB, seasonTeams, paceA, paceB, clutchA, clutchB), [recordA, recordB, seasonTeams, paceA, paceB, clutchA, clutchB])
  const isPaceLoading = loadingTeamPace[selectedSeason] || false
  const isClutchLoading = loadingClutch[selectedSeason] || false
  const isShotsLoading = loadingShots[selectedSeason] || false

  return (
    <div className="app-page space-y-6">
      <PageHeader
        title="Cara a cara"
        subtitle="Compara dos equipos por estilo, eficiencia, zonas, rebote, presión y clutch"
        scope="Temporada completa · Liga regular y playoffs"
        actions={(
          <Link to={buildTeamStatsPath()} className="inline-flex items-center gap-2 px-3 py-2 rounded-lg border border-acb-200 bg-white text-sm text-acb-600 hover:bg-acb-50">
            Estadísticas de equipo
          </Link>
        )}
      />

      <div className="grid lg:grid-cols-[160px_1fr_auto_1fr] gap-4 items-center">
        <div className="bg-white rounded-lg border border-acb-200 p-4">
          <label htmlFor="matchup-season" className="text-xs text-acb-500 font-medium">Temporada</label>
          <select
            id="matchup-season"
            value={selectedSeason}
            onChange={e => updateUrlState({ season: Number(e.target.value) })}
            className="w-full mt-1 px-3 py-2.5 border border-acb-200 rounded-lg text-sm bg-white"
          >
            {availableSeasons.map(s => <option key={s} value={s}>{seasonLabel(s)}</option>)}
          </select>
        </div>
        <TeamSelector
          label="Equipo A"
          teams={seasonTeams.filter(t => t.team !== teamB)}
          selected={teamA}
          onChange={handleTeamAChange}
          teamLogos={teamLogos}
          profileUrl={recordA ? `${buildTeamProfilePath(resolvedUrlTeamA)}?temporada=${selectedSeason}` : null}
        />
        <button
          type="button"
          onClick={handleSwapTeams}
          aria-label="Intercambiar equipos"
          className="flex h-10 w-10 items-center justify-center justify-self-center self-center rounded-md border border-acb-200 bg-white text-xl leading-none text-acb-500 transition-colors hover:bg-acb-50 hover:text-acb-900"
          title="Intercambiar equipos"
        >
          ⇄
        </button>
        <TeamSelector
          label="Equipo B"
          teams={seasonTeams.filter(t => t.team !== teamA)}
          selected={teamB}
          onChange={handleTeamBChange}
          teamLogos={teamLogos}
          profileUrl={recordB ? `${buildTeamProfilePath(resolvedUrlTeamB)}?temporada=${selectedSeason}` : null}
        />
      </div>

      {!recordA || !recordB ? (
        <div className="bg-white rounded-lg border border-acb-200 p-12 text-center text-acb-400">
          {urlTeamA || urlTeamB
            ? 'Selecciona dos equipos disponibles en esta temporada para compararlos.'
            : 'Selecciona dos equipos para empezar la comparación.'}
        </div>
      ) : (
        <>
      <div className="grid lg:grid-cols-2 gap-5">
        <SummaryCard team={recordA} league={seasonTeams} pace={paceA} clutch={clutchA} teamLogos={teamLogos} accent="bg-accent-500" />
        <SummaryCard team={recordB} league={seasonTeams} pace={paceB} clutch={clutchB} teamLogos={teamLogos} accent="bg-info-500" />
      </div>

      <div className="grid xl:grid-cols-[1.05fr_0.95fr] gap-5">
        <RadarOverlay teamA={recordA} teamB={recordB} league={seasonTeams} />
        <NotesPanel notes={notes} />
      </div>

      <div className="grid xl:grid-cols-2 gap-5">
        <StatComparison a={recordA} b={recordB} />
        <ClutchPanel a={clutchA} b={clutchB} loading={isClutchLoading} teamA={recordA} teamB={recordB} />
      </div>

      {isPaceLoading ? (
        <div className="bg-white rounded-lg border border-acb-200 p-10 text-center text-acb-400">Cargando ritmo...</div>
      ) : (
        <PaceFlow a={paceA} b={paceB} />
      )}

      <ShotProfile
        teamA={recordA}
        teamB={recordB}
        shots={shotRows}
        isLoading={isShotsLoading}
        mode={mode}
        metric={metric}
        onModeChange={(value) => updateUrlState({ mode: value })}
        onMetricChange={(value) => updateUrlState({ metric: value })}
      />
        </>
      )}
    </div>
  )
}
