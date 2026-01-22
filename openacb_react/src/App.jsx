import { useState, useEffect } from 'react'
import { Analytics } from '@vercel/analytics/react'
import { BarChart3, Target, Users, TrendingUp, Percent, Trophy, Info } from 'lucide-react'
import Home from './pages/Home'
import ShotCharts from './pages/ShotCharts'
import TeamStats from './pages/TeamStats'
import PlayerStats from './pages/PlayerStats'
import LineupAnalysis from './pages/LineupAnalysis'
import LineupRankings from './pages/LineupRankings'
import FourFactors from './pages/FourFactors'
import About from './pages/About'

const tabs = [
  { id: 'shots', label: 'Cartas de Tiro', icon: Target },
  { id: 'teams', label: 'Estadísticas de Equipo', icon: BarChart3 },
  { id: 'players', label: 'Estadísticas de Jugador', icon: Users },
  { id: 'lineups', label: 'Alineaciones', icon: TrendingUp },
  { id: 'rankings', label: 'Rankings', icon: Trophy },
  { id: 'factors', label: 'Cuatro Factores', icon: Percent },
]

const aboutTab = { id: 'about', label: 'Acerca de', icon: Info }

function App() {
  const [activeTab, setActiveTab] = useState('home')
  const [data, setData] = useState({ teams: [], players: [] })
  const [loading, setLoading] = useState(true)

  // Lazy loading for shots - load on demand per season
  const [shotsCache, setShotsCache] = useState({}) // { 2021: [...], 2022: [...], ... }
  const [loadingShots, setLoadingShots] = useState({}) // { 2021: true, 2022: false, ... }

  // Lazy loading for lineups - load on demand per season
  const [lineupsCache, setLineupsCache] = useState({}) // { 2021: {...}, 2022: {...}, ... }
  const [loadingLineups, setLoadingLineups] = useState({}) // { 2021: true, 2022: false, ... }

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

  // Function to load lineups for a specific season
  const loadLineupsForSeason = async (season) => {
    // Return cached data if already loaded
    if (lineupsCache[season]) {
      return lineupsCache[season]
    }

    // Don't reload if already loading
    if (loadingLineups[season]) {
      return null
    }

    try {
      setLoadingLineups(prev => ({ ...prev, [season]: true }))
      const response = await fetch(`/data/lineups-${season}.json`)
      if (!response.ok) throw new Error('Lineup data not found')
      const lineupData = await response.json()

      // Cache the loaded lineups
      setLineupsCache(prev => ({ ...prev, [season]: lineupData }))
      setLoadingLineups(prev => ({ ...prev, [season]: false }))

      return lineupData
    } catch (error) {
      console.error(`Error loading lineups for season ${season}:`, error)
      setLoadingLineups(prev => ({ ...prev, [season]: false }))
      return null
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
    <>
      <div className="min-h-screen bg-slate-50">
        {/* Header */}
        <header className="bg-white border-b border-slate-200 sticky top-0 z-50">
        <div className="relative h-20">
          <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 h-full">
            <div className="flex items-center h-full">
              {/* Logo */}
              <button
                onClick={() => setActiveTab('home')}
                className="flex items-center gap-3 hover:opacity-80 transition-opacity"
              >
                <img
                  src="/openacb_nobckg.png"
                  alt="OpenACB Logo"
                  className="w-24 h-24 object-contain"
                />
                <div>
                  <h1 className="text-xl font-semibold text-slate-900">openACB</h1>
                </div>
              </button>

              {/* Main Navigation */}
              <nav className="flex items-center gap-1 ml-6">
                {tabs.map((tab) => {
                  const Icon = tab.icon
                  const isActive = activeTab === tab.id
                  return (
                    <button
                      key={tab.id}
                      onClick={() => setActiveTab(tab.id)}
                      className={`flex items-center gap-2 px-3 py-2 rounded-md transition-colors text-sm font-medium whitespace-nowrap
                        ${isActive
                          ? 'bg-slate-100 text-slate-900'
                          : 'text-slate-600 hover:text-slate-900 hover:bg-slate-50'
                        }`}
                    >
                      <Icon className="w-4 h-4" />
                      <span className="hidden lg:inline">{tab.label}</span>
                    </button>
                  )
                })}
              </nav>
            </div>
          </div>

          {/* About - Fixed to right edge of full screen */}
          {(() => {
            const AboutIcon = aboutTab.icon
            return (
              <button
                onClick={() => setActiveTab(aboutTab.id)}
                className={`absolute right-8 top-1/2 -translate-y-1/2 flex items-center gap-2 px-4 py-2 rounded-md transition-colors text-sm font-medium whitespace-nowrap z-10
                  ${activeTab === aboutTab.id
                    ? 'bg-slate-100 text-slate-900'
                    : 'text-slate-600 hover:text-slate-900 hover:bg-slate-50'
                  }`}
              >
                <AboutIcon className="w-4 h-4" />
                <span className="hidden sm:inline">{aboutTab.label}</span>
              </button>
            )
          })()}
        </div>
      </header>

      {/* Main Content */}
      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6">
        {activeTab === 'home' && <Home setActiveTab={setActiveTab} />}
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
        {activeTab === 'lineups' && (
          <LineupAnalysis
            teams={data.teams}
            loadLineupsForSeason={loadLineupsForSeason}
            lineupsCache={lineupsCache}
            loadingLineups={loadingLineups}
          />
        )}
        {activeTab === 'rankings' && (
          <LineupRankings
            teams={data.teams}
            loadLineupsForSeason={loadLineupsForSeason}
            lineupsCache={lineupsCache}
            loadingLineups={loadingLineups}
          />
        )}
        {activeTab === 'factors' && <FourFactors teams={data.teams} />}
        {activeTab === 'about' && <About />}
      </main>

      {/* Footer */}
      <footer className="border-t border-slate-200 bg-white mt-12">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6">
          <div className="flex items-center justify-between text-sm text-slate-500">
            <p>hecho con cariño por <a href="https://juantorrecillas.es" className="text-slate-600 hover:text-slate-900 underline">juan torrecillas</a> 🍋</p>
          </div>
        </div>
      </footer>
    </div>
    <Analytics />
  </>
  )
}

export default App
