import { useRef, useState } from 'react'
import { Link } from 'react-router-dom'
import { ArrowRight } from 'lucide-react'
import Court from './Court'
import TeamRadar from './TeamRadar'
import { getZonePolygons, CUSTOM_LABEL_POSITIONS, getPolygonCentroid, polygonToPath } from '../utils/shotZones'
import examples from '../data/home-tool-examples.json'
import styles from './HomeToolPreview.module.css'

const slides = [
  { id: 'team', label: 'Equipo', title: 'Perfil de equipo', action: 'Abrir perfil de equipo' },
  { id: 'player', label: 'Jugador', title: 'Perfil de jugador', action: 'Abrir perfil de jugador' },
  { id: 'lineups', label: 'Alineaciones', title: 'Mejores y peores tríos', action: 'Abrir ranking de alineaciones' },
]

function RadarPreview({ axes, name }) {
  const point = (index, value) => {
    const angle = index * Math.PI / 4 - Math.PI / 2
    return [180 + Math.cos(angle) * value, 149 + Math.sin(angle) * value]
  }
  const polygon = values => values.map((value, index) => point(index, value).join(',')).join(' ')

  return (
    <svg viewBox="-30 -12 420 326" role="img" aria-label={`Radar de ${name}`}>
      <desc>{axes.map(axis => `${axis.label}: ${Math.round(axis.value)}`).join('; ')}. Percentiles de liga, de 0 a 100. Línea discontinua: percentil 50.</desc>
      {[25, 50, 75, 100].map(level => (
        <polygon key={level} points={polygon(axes.map(() => level))} className={level === 50 ? styles.referenceRing : styles.radarGrid} />
      ))}
      {axes.map((axis, index) => {
        const [x, y] = point(index, 100)
        return <line key={axis.label} x1="180" y1="149" x2={x} y2={y} className={styles.radarGrid} />
      })}
      <polygon points={polygon(axes.map(axis => axis.value))} className={styles.radarArea} />
      {axes.map((axis, index) => {
        const [x, y] = point(index, axis.value)
        const [lx, ly] = point(index, 119)
        return (
          <g key={axis.label}>
            <circle cx={x} cy={y} r="3.4" className={styles.radarPoint}><title>{axis.label}: {Math.round(axis.value)}</title></circle>
            <text x={lx} y={ly} textAnchor={index === 0 || index === 4 ? 'middle' : index < 4 ? 'start' : 'end'} dominantBaseline="middle" className={styles.radarLabel}>{axis.label}</text>
          </g>
        )
      })}
    </svg>
  )
}

function MetricRegister({ axes }) {
  return (
    <div className={styles.metricPanel}>
      <dl className={styles.metricRegister}>
        {axes.map(axis => (
          <div key={axis.label}>
            <dt>{axis.label}</dt>
            <dd className={axis.value > 85 ? styles.metricAccent : undefined}>{Math.round(axis.value)}</dd>
          </div>
        ))}
      </dl>
    </div>
  )
}

const compactCourtZones = Object.entries(getZonePolygons())

function CompactEfficiencyCourt({ data }) {
  const stats = data.compactZones.stats
  const leagueEfg = data.compactZones.leagueEfg
  return (
    <figure className={styles.compactCourt} aria-label={`Eficiencia eFG% por zona de ${data.name}`}>
      <Court width={450} height={423}>
      <desc>{`eFG% por zona de ${data.name}. Temporada regular ${data.seasonLabel}. Verde: superior a la liga; rosa: inferior.`}</desc>
      {compactCourtZones.map(([zone, points]) => {
        const stat = stats[zone]
        const hasData = stat?.att > 0 && Number.isFinite(stat.efg)
        const hasComparison = hasData && Number.isFinite(leagueEfg[zone])
        const difference = hasComparison ? stat.efg - leagueEfg[zone] : 0
        const opacity = Math.abs(difference) > 10 ? 0.72 : Math.abs(difference) > 5 ? 0.5 : 0.26
        const position = CUSTOM_LABEL_POSITIONS[zone] || getPolygonCentroid(points)
        const x = (position.x + 7.5) * 30
        const y = -position.y * 30
        const corner = zone.startsWith('Triple Esquina')
        const restricted = zone === 'Zona (Restringida)'
        return (
          <g key={zone}>
            <title>{hasData ? `${zone}: ${stat.efg.toFixed(1)}% eFG; ${stat.made}/${stat.att} tiros${hasComparison ? `; ${difference > 0 ? '+' : ''}${difference.toFixed(1)} pp frente a la liga` : ''}` : `${zone}: sin dato en este ejemplo`}</title>
            <path d={polygonToPath(points, 30, 7.5)} fill={!hasData ? '#d9e2ec' : difference > 0 ? '#2aa867' : difference < 0 ? '#dd415d' : '#ffffff'} fillOpacity={!hasData ? 0.5 : opacity} stroke="#486581" strokeOpacity="0.5" strokeWidth="1" />
            <text x={x} y={y} textAnchor="middle" dominantBaseline="middle" transform={corner ? `rotate(-90 ${x} ${y})` : undefined} className={styles.compactZoneLabel}>
              <tspan x={x}>{hasData ? `${Math.round(stat.efg)}%` : '—'}</tspan>
              {hasData && !corner && !restricted && <tspan x={x} dy="19">{stat.made}/{stat.att}</tspan>}
            </text>
          </g>
        )
      })}
      </Court>
      <figcaption className={styles.courtLegend}>
        <span><i className={styles.belowLeague} />Inferior a la liga</span>
        <span><i className={styles.aboveLeague} />Superior</span>
      </figcaption>
    </figure>
  )
}

function LineupRows({ entries, tone }) {
  return entries.map(lineup => {
    const value = lineup.impact ?? lineup.net
    const valueClass = value < 0 ? styles.lineupNetNegative : styles.lineupNet

    return (
      <div className={styles.lineupRow} key={`${tone}-${lineup.rank}`}>
        <span className={styles.lineupRank}>{String(lineup.rank).padStart(2, '0')}</span>
        <span className={styles.lineupPlayers}>
          <b>{lineup.players.join(' · ')}</b>
          <small>ORtg {lineup.ortg.toFixed(1)} · DRtg {lineup.drtg.toFixed(1)}</small>
        </span>
        <span className={styles.lineupMinutes}>{lineup.minutes.toFixed(1)}</span>
        <span className={valueClass}>{value > 0 ? '+' : ''}{value.toFixed(1)}</span>
      </div>
    )
  })
}

function LineupPreview() {
  const data = examples.lineups

  return (
    <div className={styles.lineupFocus}>
      <div className={styles.lineupList} aria-label={`Mejores y peores tríos de ${data.name}`}>
        <div className={styles.lineupListHead} aria-hidden="true">
          <span>TRÍO</span><span>MIN</span><span>{data.metricShort}</span>
        </div>
        <div className={styles.lineupGroup}>
          <div className={styles.lineupGroupHeading}>MEJORES</div>
          <LineupRows entries={data.best} tone="best" />
        </div>
        <div className={styles.lineupDivider} aria-hidden="true" />
        <div className={styles.lineupGroup}>
          <div className={styles.lineupGroupHeading}>PEORES</div>
          <LineupRows entries={data.worst} tone="worst" />
        </div>
      </div>
    </div>
  )
}

export default function HomeToolPreview() {
  const [active, setActive] = useState(0)
  const touchStart = useRef(null)
  const slide = slides[active]
  const data = examples[slide.id]
  const changeSlide = delta => setActive(index => (index + delta + slides.length) % slides.length)

  return (
    <section className={styles.preview} aria-label="Ejemplos de herramientas" aria-roledescription="carrusel">
      <header className={styles.previewHeader}>
        <div className={styles.identity}>
          <h2>{data.name}{slide.id === 'player' ? ` · ${data.team}` : ''} <span>· {data.seasonLabel}</span></h2>
          {slide.id === 'lineups' && <p className={styles.identityDetail}>{slide.title}</p>}
          {slide.id === 'player' && (
            <div className={styles.identityDetail}>
              <span>{data.position}</span>
              <span aria-hidden="true">·</span>
              <span className={styles.roleWord}>{data.role}</span>
            </div>
          )}
        </div>
        <div className={styles.slideNav} role="group" aria-label="Herramienta del ejemplo">
          {slides.map((item, index) => (
            <button key={item.id} type="button" aria-pressed={index === active} aria-controls="home-tool-example" onClick={() => setActive(index)}>{item.label}</button>
          ))}
        </div>
      </header>

      <div className={styles.slideStage} onPointerDown={event => {
        if (event.pointerType === 'touch') touchStart.current = { x: event.clientX, y: event.clientY }
      }} onPointerUp={event => {
        if (!touchStart.current) return
        const dx = event.clientX - touchStart.current.x
        const dy = event.clientY - touchStart.current.y
        if (Math.abs(dx) > 50 && Math.abs(dx) > Math.abs(dy)) changeSlide(dx < 0 ? 1 : -1)
        touchStart.current = null
      }} onPointerCancel={() => { touchStart.current = null }}>
        <div id="home-tool-example" key={slide.id} className={styles.slide} role="group" aria-roledescription="diapositiva" aria-label={`${active + 1} de ${slides.length}: ${slide.title}`}>
          {slide.id === 'lineups' ? (
            <LineupPreview />
          ) : (
            <>
              <div className={`${styles.analysisGrid} ${slide.id === 'player' ? styles.playerGrid : styles.teamGrid}`}>
                {slide.id === 'player' && <MetricRegister axes={data.axes} />}
                <figure className={styles.radarFigure}>
                  {slide.id === 'team'
                    ? <TeamRadar axes={data.axes} name={data.name} color="#fe5917" dark />
                    : <RadarPreview axes={data.axes} name={data.name} />}
                </figure>
                {slide.id === 'team' ? <div className={styles.zoneMapCompact}>
                  <p className={styles.chartTitle}>Tiro por zona · eFG%</p>
                  <CompactEfficiencyCourt data={data} />
                </div> : null}
              </div>
            </>
          )}
        </div>
      </div>

      <footer className={styles.previewFooter}>
        <span aria-live="polite" aria-atomic="true">{active + 1} / {slides.length}<span className={styles.srOnly}> · {slide.title}</span></span>
        <Link to={data.path}>{slide.action}<ArrowRight aria-hidden="true" size={18} /></Link>
      </footer>
    </section>
  )
}
