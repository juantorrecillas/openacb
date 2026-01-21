import { Target, BarChart3, Users, TrendingUp, Trophy, Percent, ArrowRight } from 'lucide-react'

function Home({ setActiveTab }) {
  const features = [
    {
      id: 'shots',
      title: 'Cartas de Tiro',
      description: 'Visualiza la distribución espacial de los tiros de jugadores y equipos con mapas de calor interactivos.',
      icon: Target,
      color: 'bg-blue-50 text-blue-600 border-blue-200'
    },
    {
      id: 'teams',
      title: 'Estadísticas de Equipo',
      description: 'Analiza métricas avanzadas de todos los equipos ACB: ORtg, DRtg, Four Factors y más.',
      icon: BarChart3,
      color: 'bg-green-50 text-green-600 border-green-200'
    },
    {
      id: 'players',
      title: 'Estadísticas de Jugador',
      description: 'Consulta estadísticas individuales detalladas, eficiencia por zona y métricas avanzadas.',
      icon: Users,
      color: 'bg-purple-50 text-purple-600 border-purple-200'
    },
    {
      id: 'lineups',
      title: 'Análisis de Alineaciones',
      description: 'Descubre qué combinaciones de jugadores funcionan mejor con el análisis On/Off Court.',
      icon: TrendingUp,
      color: 'bg-orange-50 text-orange-600 border-orange-200'
    },
    {
      id: 'rankings',
      title: 'Rankings de Alineaciones',
      description: 'Las combinaciones de jugadores con mayor impacto en la Liga.',
      icon: Trophy,
      color: 'bg-yellow-50 text-yellow-600 border-yellow-200'
    },
    {
      id: 'factors',
      title: 'Cuatro Factores',
      description: 'Analiza los Four Factors de cada equipo.',
      icon: Percent,
      color: 'bg-red-50 text-red-600 border-red-200'
    }
  ]

  return (
    <div className="max-w-6xl mx-auto">
      {/* Hero Section */}
      <div className="text-center mb-12">
        <div className="flex justify-center mb-6">
          <img
            src="/openacb_nobckg.png"
            alt="OpenACB Logo"
            className="w-32 h-32 object-contain"
          />
        </div>
        <h1 className="text-4xl font-bold text-slate-900 mb-4">
          Bienvenido a OpenACB
        </h1>
        <p className="text-xl text-slate-600 max-w-3xl mx-auto leading-relaxed">
          Estadísticas avanzadas y herramientas de análisis para la Liga Endesa.
          Explora métricas, visualizaciones y utiliza las herramientas disponibles para entender mejor el baloncesto ACB.
        </p>
      </div>

      {/* Features Grid */}
      <div className="grid md:grid-cols-2 lg:grid-cols-3 gap-6 mb-8">
        {features.map((feature) => {
          const Icon = feature.icon
          return (
            <button
              key={feature.id}
              onClick={() => setActiveTab(feature.id)}
              className="bg-white rounded-lg shadow-sm border border-slate-200 p-6 text-left hover:shadow-md transition-shadow group"
            >
              <div className={`inline-flex p-3 rounded-lg mb-4 ${feature.color} border`}>
                <Icon className="w-6 h-6" />
              </div>
              <h3 className="text-lg font-semibold text-slate-900 mb-2 group-hover:text-slate-700">
                {feature.title}
              </h3>
              <p className="text-sm text-slate-600 mb-4">
                {feature.description}
              </p>
              <div className="flex items-center text-sm font-medium text-slate-900 group-hover:text-blue-600">
                Explorar
                <ArrowRight className="w-4 h-4 ml-1 group-hover:translate-x-1 transition-transform" />
              </div>
            </button>
          )
        })}
      </div>

      {/* Quick Stats Banner */}
      <div className="bg-gradient-to-r from-slate-900 to-slate-700 rounded-lg p-8 text-white">
        <div className="grid md:grid-cols-3 gap-8 text-center">
          <div>
            <div className="text-3xl font-bold mb-2">6</div>
            <div className="text-slate-300 text-sm">Temporadas Disponibles - 2020 a 2026</div>
          </div>
          <div>

          </div>
          <div>
            <div className="text-3xl font-bold mb-2">100%</div>
            <div className="text-slate-300 text-sm">Código Abierto</div>
          </div>
        </div>
      </div>

      {/* CTA */}
      <div className="mt-8 text-center">
        <button
          onClick={() => setActiveTab('about')}
          className="inline-flex items-center gap-2 px-6 py-3 bg-slate-900 text-white rounded-lg hover:bg-slate-800 transition-colors"
        >
          Más información sobre el proyecto
          <ArrowRight className="w-4 h-4" />
        </button>
      </div>
    </div>
  )
}

export default Home
