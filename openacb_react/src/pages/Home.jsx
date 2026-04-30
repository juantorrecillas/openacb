import { Link } from 'react-router-dom'
import { Target, BarChart3, Users, TrendingUp, Trophy, Percent, UserCircle, ArrowRight, GitCompareArrows, Fingerprint, Activity, Crown, Sparkles, Scale, Flame } from 'lucide-react'

// Tab id → URL path mapping (must match App.jsx)
const TAB_PATHS = {
  teams: '/equipos',
  fingerprint: '/perfil-equipo',
  matchup: '/matchup-equipos',
  gameflow: '/flujo-partido',
  factors: '/cuatro-factores',
  players: '/jugadores',
  profile: '/jugador',
  clutch: '/estadisticas-clutch',
  similarity: '/similitud',
  comparison: '/comparar',
  lineups: '/alineaciones',
  rankings: '/mejores-alineaciones',
  shots: '/cartas-tiro',
  zoneleaders: '/lideres-zona',
  about: '/info',
}

const categories = [
  {
    id: 'jugadores',
    title: 'Jugadores',
    description: 'Estadísticas individuales y perfiles de jugador',
    tools: [
      { id: 'players', title: 'Estadísticas de Jugador', description: 'Producción, eficiencia y métricas avanzadas.', icon: Users },
      { id: 'profile', title: 'Perfil de Jugador',       description: 'Perfil completo, estilo y evolución.', icon: UserCircle },
      { id: 'clutch',  title: 'Estadísticas Clutch',     description: 'Rendimiento en finales ajustados.', icon: Flame },
    ],
  },
  {
    id: 'equipos',
    title: 'Equipos',
    description: 'Rendimiento, estilo e identidad de cada equipo',
    tools: [
      { id: 'teams',       title: 'Estadísticas de Equipo', description: 'Rendimiento, ritmo y eficiencia.',                               icon: BarChart3 },
      { id: 'fingerprint', title: 'Perfil de Equipo',       description: 'Fortalezas, debilidades y estilo.',               icon: Fingerprint },
      { id: 'gameflow',    title: 'Análisis de Partido',    description: 'Evolución del marcador y jugadas.', icon: Activity },
      { id: 'factors',     title: 'Four Factors',           description: 'Tiro, pérdidas, rebote y tiros libres.',                         icon: Percent },
    ],
  },
  {
    id: 'herramientas',
    title: 'Herramientas',
    description: 'Comparativas y búsqueda de similitud entre jugadores y equipos',
    tools: [
      { id: 'similarity', title: 'Similitud',          description: 'Jugadores con perfiles similares.',                    icon: Sparkles },
      { id: 'comparison', title: 'Comparar Jugadores', description: 'Comparación directa entre jugadores.',          icon: GitCompareArrows },
      { id: 'matchup',    title: 'Cara a Cara',        description: 'Compara dos equipos por métricas.',      icon: Scale },
    ],
  },
  {
    id: 'alineaciones',
    title: 'Alineaciones',
    description: 'Combinaciones y rendimiento de quintetos',
    tools: [
      { id: 'lineups',  title: 'Análisis On/Off',     description: 'Impacto de jugadores y quintetos.', icon: TrendingUp },
      { id: 'rankings', title: 'Mejores Alineaciones', description: 'Quintetos con mayor rendimiento.',  icon: Trophy },
    ],
  },
  {
    id: 'tiro',
    title: 'Tiro',
    description: 'Visualizaciones de tiro y líderes por zona',
    tools: [
      { id: 'shots',       title: 'Cartas de Tiro',   description: 'Mapas de tiro por jugador o equipo.',                                             icon: Target },
      { id: 'zoneleaders', title: 'Líderes por Zona', description: 'Anotadores y eficiencia por zona.', icon: Crown },
    ],
  },
]

function CategoryBlock({ category }) {
  return (
    <div className="bg-white rounded-xl overflow-hidden border border-acb-200">
      {/* Category header */}
      <div className="bg-acb-700 px-5 py-4">
        <h3 className="text-sm font-bold text-white uppercase tracking-wide">{category.title}</h3>
        <p className="text-acb-300 text-xs mt-0.5">{category.description}</p>
      </div>
      {/* Tools list */}
      <div className="divide-y divide-acb-100">
        {category.tools.map((tool) => {
          const Icon = tool.icon
          return (
            <Link
              key={tool.id}
              to={TAB_PATHS[tool.id] || '/'}
              className="w-full flex items-center gap-3 px-5 py-3.5 text-left hover:bg-accent-100 transition-colors group"
            >
              <div className="flex-shrink-0 p-2 rounded-lg bg-acb-100">
                <Icon className="w-4 h-4 text-acb-500" />
              </div>
              <div className="min-w-0 flex-1">
                <div className="text-sm font-semibold text-acb-800 group-hover:text-accent-700">{tool.title}</div>
                <div className="text-xs text-acb-400 truncate">{tool.description}</div>
              </div>
              <ArrowRight className="w-3.5 h-3.5 text-acb-300 group-hover:text-accent-500 group-hover:translate-x-0.5 transition-all flex-shrink-0" />
            </Link>
          )
        })}
      </div>
    </div>
  )
}

function Home() {
  return (
    <div className="max-w-5xl mx-auto">
      {/* Hero Section */}
      <div className="text-center mb-10">
        <div className="flex justify-center mb-6">
          <img src="/openacb_nobckg.png" alt="OpenACB Logo" className="w-28 h-28 object-contain" />
        </div>
        <h1 className="text-4xl font-bold text-acb-900 mb-4">Bienvenido a OpenACB</h1>
        <p className="text-xl text-acb-600 max-w-3xl mx-auto leading-relaxed">
          Herramientas de analítica y estadísticas avanzadas para la Liga ACB.
          Explora, visualiza y utiliza los recursos disponibles para entender mejor el baloncesto ACB.
        </p>
      </div>

      {/* Category Grid */}
      <div className="grid md:grid-cols-2 gap-5 mb-8">
        {categories.map((category) => (
          <CategoryBlock key={category.id} category={category} />
        ))}
      </div>

      {/* Quick Stats Banner */}
      <div className="bg-gradient-to-r from-acb-900 to-acb-700 rounded-lg p-8 text-white">
        <div className="grid md:grid-cols-3 gap-8 text-center">
          <div>
            <div className="text-3xl font-bold mb-2">10</div>
            <div className="text-acb-300 text-sm">Temporadas Disponibles: 2016-2026</div>
          </div>
          <div></div>
          <div>
            <div className="text-3xl font-bold mb-2">100%</div>
            <div className="text-acb-300 text-sm">Código Abierto</div>
          </div>
        </div>
      </div>

      {/* CTA */}
      <div className="mt-8 text-center">
        <Link
          to="/info"
          className="inline-flex items-center gap-2 px-6 py-3 bg-acb-900 text-white rounded-lg hover:bg-acb-800 transition-colors"
        >
          Más información sobre el proyecto
          <ArrowRight className="w-4 h-4" />
        </Link>
      </div>
    </div>
  )
}

export default Home
