import { Link } from 'react-router-dom'
import { ArrowRight, ArrowUpRight } from 'lucide-react'
import {
  buildAboutPath,
  buildFourFactorsPath,
  buildGamesPath,
  buildLineupAnalysisPath,
  buildLineupRankingsPath,
  buildPlayerClutchPath,
  buildPlayerComparisonPath,
  buildPlayerProfilePath,
  buildPlayerSimilarityPath,
  buildPlayerStatsPath,
  buildShotChartsPath,
  buildTeamComparisonPath,
  buildTeamProfilePath,
  buildTeamStatsPath,
  buildZoneLeadersPath,
} from '../routing'

const TAB_PATHS = {
  teams: buildTeamStatsPath(),
  fingerprint: buildTeamProfilePath(),
  matchup: buildTeamComparisonPath(),
  gameflow: buildGamesPath(),
  factors: buildFourFactorsPath(),
  players: buildPlayerStatsPath(),
  profile: buildPlayerProfilePath(),
  clutch: buildPlayerClutchPath(),
  similarity: buildPlayerSimilarityPath(),
  comparison: buildPlayerComparisonPath(),
  lineups: buildLineupAnalysisPath(),
  rankings: buildLineupRankingsPath(),
  shots: buildShotChartsPath(),
  zoneleaders: buildZoneLeadersPath(),
}

const categories = [
  {
    id: 'equipos',
    title: 'Equipos',
    description: 'Rendimiento, estilo e identidad de cada equipo',
    tools: [
      { id: 'teams',       title: 'Estadísticas de Equipo', description: 'Rendimiento, ritmo y eficiencia.' },
      { id: 'fingerprint', title: 'Perfil de Equipo',       description: 'Fortalezas, debilidades y estilo.' },
      { id: 'gameflow',    title: 'Análisis de Partido',    description: 'Evolución del marcador y jugadas.' },
      { id: 'factors',     title: 'Four Factors',           description: 'Tiro, pérdidas, rebote y tiros libres.' },
    ],
  },
  {
    id: 'jugadores',
    title: 'Jugadores',
    description: 'Estadísticas individuales y perfiles de jugador',
    tools: [
      { id: 'players', title: 'Estadísticas de Jugador', description: 'Producción, eficiencia y métricas avanzadas.' },
      { id: 'profile', title: 'Perfil de Jugador',       description: 'Perfil completo, estilo y evolución.' },
      { id: 'clutch',  title: 'Estadísticas clutch',     description: 'Rendimiento en finales ajustados.' },
    ],
  },
  {
    id: 'herramientas',
    title: 'Herramientas',
    description: 'Comparativas y búsqueda de similitud entre jugadores y equipos',
    tools: [
      { id: 'similarity', title: 'Similitud',          description: 'Jugadores con perfiles similares.' },
      { id: 'comparison', title: 'Comparar Jugadores', description: 'Comparación directa entre jugadores.' },
      { id: 'matchup',    title: 'Cara a Cara',        description: 'Compara dos equipos por métricas.' },
    ],
  },
  {
    id: 'alineaciones',
    title: 'Alineaciones',
    description: 'Combinaciones y rendimiento de quintetos',
    tools: [
      { id: 'lineups',  title: 'Análisis On/Off',      description: 'Impacto de jugadores y quintetos.' },
      { id: 'rankings', title: 'Mejores Alineaciones', description: 'Quintetos con mayor rendimiento.' },
    ],
  },
  {
    id: 'tiro',
    title: 'Tiro',
    description: 'Visualizaciones de tiro y líderes por zona',
    tools: [
      { id: 'shots',       title: 'Cartas de tiro',   description: 'Mapas de tiro por jugador o equipo.' },
      { id: 'zoneleaders', title: 'Líderes por zona', description: 'Anotadores y eficiencia por zona.' },
    ],
  },
]

function CategoryBlock({ category, wide = false }) {
  return (
    <section className={`border-t-2 border-acb-800 ${wide ? 'lg:col-span-2' : ''}`}>
      {/* category header */}
      <header className="grid gap-1 border-b border-acb-200 py-4 sm:grid-cols-[9rem_1fr] sm:items-baseline">
        <h3 className="font-display text-2xl font-semibold text-acb-900">{category.title}</h3>
        <p className="text-sm text-acb-500 sm:text-right">{category.description}</p>
      </header>
      {/* tools list */}
      <div className={wide ? 'lg:grid lg:grid-cols-2 lg:gap-x-10' : ''}>
        {category.tools.map((tool) => {
          return (
            <Link
              key={tool.id}
              to={TAB_PATHS[tool.id] || '/'}
              className="group grid min-h-[76px] w-full grid-cols-[minmax(0,1fr)_auto] items-center gap-4 border-b border-acb-200 py-3.5 text-left transition-colors hover:border-accent-400"
            >
              <div className="min-w-0">
                <div className="font-display text-lg font-semibold text-acb-800 transition-colors group-hover:text-accent-700">{tool.title}</div>
                <div className="mt-0.5 text-sm leading-snug text-acb-500">{tool.description}</div>
              </div>
              <ArrowRight className="h-4 w-4 flex-shrink-0 text-acb-300 transition-[color,transform] group-hover:translate-x-1 group-hover:text-accent-500" />
            </Link>
          )
        })}
      </div>
    </section>
  )
}

function Home() {
  return (
    <div className="app-page mx-auto max-w-6xl">
      {/* hero section */}
      <section className="grid gap-10 border-b border-acb-300 pb-10 pt-2 lg:grid-cols-[minmax(0,1.45fr)_minmax(18rem,0.55fr)] lg:items-end lg:pb-12">
        <div>
          <h1 className="max-w-4xl font-display text-5xl font-semibold leading-[0.94] tracking-[-0.025em] text-acb-900 sm:text-6xl lg:text-7xl">
            Estadística avanzada de la Liga Endesa
          </h1>
          <p className="mt-6 max-w-3xl text-lg leading-relaxed text-acb-600 sm:text-xl">
            Herramientas de analítica y estadísticas avanzadas para la Liga ACB. Explora, visualiza y utiliza los recursos disponibles para entender mejor el baloncesto ACB.
          </p>
        </div>

        <aside className="border-t-4 border-accent-500 bg-acb-900 p-6 text-white sm:p-7">
          <div className="flex items-baseline justify-between gap-4 border-b border-acb-700 pb-4">
            <strong className="font-display text-5xl font-semibold leading-none">10</strong>
            <span className="max-w-40 text-right text-sm leading-snug text-acb-200">Temporadas disponibles</span>
          </div>
          <p className="py-4 font-mono text-sm text-acb-200">2016-17 — 2025-26</p>
          <a
            href="https://github.com/juantorrecillas/openacb"
            target="_blank"
            rel="noopener noreferrer"
            className="group flex items-center justify-between border-t border-acb-700 pt-4 text-sm font-semibold text-white"
          >
            100% código abierto
            <ArrowUpRight className="h-4 w-4 text-accent-400 transition-transform group-hover:-translate-y-0.5 group-hover:translate-x-0.5" />
          </a>
        </aside>
      </section>

      {/* category grid */}
      <div className="mb-12 mt-12 grid gap-x-10 gap-y-12 lg:grid-cols-2">
        {categories.map((category, index) => (
          <CategoryBlock key={category.id} category={category} wide={index === categories.length - 1} />
        ))}
      </div>

      {/* cta */}
      <div className="flex items-center justify-between gap-6 border-t border-acb-300 py-6">
        <p className="text-sm text-acb-500">Datos, metodología y código disponibles para consulta.</p>
        <Link
          to={buildAboutPath()}
          className="group inline-flex shrink-0 items-center gap-2 text-sm font-semibold text-acb-900 underline decoration-accent-500 decoration-2 underline-offset-4"
        >
          Sobre el proyecto
          <ArrowRight className="h-4 w-4 transition-transform group-hover:translate-x-1" />
        </Link>
      </div>
    </div>
  )
}

export default Home
