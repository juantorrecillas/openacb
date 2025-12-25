import { useState, useEffect } from 'react'
import { BarChart3, Target, Users, TrendingUp, Percent } from 'lucide-react'
import ShotCharts from './pages/ShotCharts'
import TeamStats from './pages/TeamStats'
import PlayerStats from './pages/PlayerStats'
import LineupAnalysis from './pages/LineupAnalysis'
import FourFactors from './pages/FourFactors'

const tabs = [
  { id: 'shots', label: 'Cartas de Tiro', icon: Target },
  { id: 'teams', label: 'Estadísticas de Equipo', icon: BarChart3 },
  { id: 'players', label: 'Estadísticas de Jugador', icon: Users },
  { id: 'lineups', label: 'Alineaciones', icon: TrendingUp },
  { id: 'factors', label: 'Cuatro Factores', icon: Percent },
]

function App() {
  const [activeTab, setActiveTab] = useState('shots')
  const [data, setData] = useState({ teams: [], players: [] })
  const [loading, setLoading] = useState(true)

  // Lazy loading for shots - load on demand per season
  const [shotsCache, setShotsCache] = useState({}) // { 2021: [...], 2022: [...], ... }
  const [loadingShots, setLoadingShots] = useState({}) // { 2021: true, 2022: false, ... }

  useEffect(() => {
    async function loadData() {
      try {
        const [teamsRes, playersRes] = await Promise.all([
          fetch('/data/teams.json'),
          fetch('/data/players.json'),
        ])

        const [teams, players] = await Promise.all([
          teamsRes.json(),
          playersRes.json(),
        ])

        setData({ teams, players })
      } catch (error) {
        console.error('Error cargando datos:', error)
      } finally {
        setLoading(false)
      }
    }

    loadData()
  }, [])

  // Function to load shots for a specific season
  const loadShotsForSeason = async (season) => {
    // Return cached data if already loaded
    if (shotsCache[season]) {
      return shotsCache[season]
    }

    // Don't reload if already loading
    if (loadingShots[season]) {
      return []
    }

    try {
      setLoadingShots(prev => ({ ...prev, [season]: true }))
      const response = await fetch(`/data/shots-${season}.json`)
      const shots = await response.json()

      // Cache the loaded shots
      setShotsCache(prev => ({ ...prev, [season]: shots }))
      setLoadingShots(prev => ({ ...prev, [season]: false }))

      return shots
    } catch (error) {
      console.error(`Error loading shots for season ${season}:`, error)
      setLoadingShots(prev => ({ ...prev, [season]: false }))
      return []
    }
  }

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-slate-500">Cargando datos...</div>
      </div>
    )
  }

  return (
    <div className="min-h-screen bg-slate-50">
      {/* Header */}
      <header className="bg-white border-b border-slate-200 sticky top-0 z-50">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex items-center justify-between h-16">
            <div className="flex items-center gap-3">
              <div className="w-8 h-8 bg-slate-800 rounded-lg flex items-center justify-center">
                <span className="text-white font-bold text-sm">A</span>
              </div>
              <div>
                <h1 className="text-lg font-semibold text-slate-900">OpenACB</h1>
                <p className="text-xs text-slate-500">Datos Abiertos Liga Endesa</p>
              </div>
            </div>
            
            <nav className="flex items-center gap-1">
              {tabs.map((tab) => {
                const Icon = tab.icon
                const isActive = activeTab === tab.id
                return (
                  <button
                    key={tab.id}
                    onClick={() => setActiveTab(tab.id)}
                    className={`flex items-center gap-2 px-4 py-2 rounded-md transition-colors text-sm font-medium
                      ${isActive 
                        ? 'bg-slate-100 text-slate-900' 
                        : 'text-slate-600 hover:text-slate-900 hover:bg-slate-50'
                      }`}
                  >
                    <Icon className="w-4 h-4" />
                    <span className="hidden sm:inline">{tab.label}</span>
                  </button>
                )
              })}
            </nav>
          </div>
        </div>
      </header>

      {/* Main Content */}
      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6">
        {activeTab === 'shots' && (
          <ShotCharts
            loadShotsForSeason={loadShotsForSeason}
            shotsCache={shotsCache}
            loadingShots={loadingShots}
            teams={data.teams}
            players={data.players}
          />
        )}
        {activeTab === 'teams' && <TeamStats teams={data.teams} />}
        {activeTab === 'players' && <PlayerStats players={data.players} />}
        {activeTab === 'lineups' && <LineupAnalysis teams={data.teams} />}
        {activeTab === 'factors' && <FourFactors teams={data.teams} />}
      </main>

      {/* Footer */}
      <footer className="border-t border-slate-200 bg-white mt-12">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6">
          <div className="flex items-center justify-between text-sm text-slate-500">
            <p>hecho con cariño por <a href="https://github.com/juantorrecillas" className="text-slate-600 hover:text-slate-900 underline">juan torrecillas</a> 🍋</p>
          </div>
        </div>
      </footer>
    </div>
  )
}

export default App
"// TEST: This should trigger a reload" 
