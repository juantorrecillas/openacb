import { useState, useEffect, useRef } from 'react'
import { Routes, Route, useNavigate, useLocation, Link } from 'react-router-dom'
import { Analytics } from '@vercel/analytics/react'
import { BarChart3, Target, Users, TrendingUp, Percent, Trophy, Info, UserCircle, Menu, X, GitCompareArrows, Fingerprint, ChevronDown, Activity, Crown, Flame, Scale, Sparkles } from 'lucide-react'
import Home from './pages/Home'
import ShotCharts from './pages/ShotCharts'
import TeamStats from './pages/TeamStats'
import PlayerStats from './pages/PlayerStats'
import LineupAnalysis from './pages/LineupAnalysis'
import LineupRankings from './pages/LineupRankings'
import FourFactors from './pages/FourFactors'
import PlayerProfile from './pages/PlayerProfile'
import About from './pages/About'
import PlayerSimilarity from './pages/PlayerSimilarity'
import PlayerComparison from './pages/PlayerComparison'
import TeamFingerprint from './pages/TeamFingerprint'
import TeamMatchup from './pages/TeamMatchup'
import GameFlow from './pages/GameFlow'
import TeamPace from './pages/TeamPace'
import ZoneLeaders from './pages/ZoneLeaders'
import ClutchStats from './pages/ClutchStats'

// Tab id → URL path mapping
const TAB_PATHS = {
  home: '/',
  teams: '/equipos',
  fingerprint: '/perfil-equipo',
  matchup: '/matchup-equipos',
  gameflow: '/flujo-partido',
  factors: '/cuatro-factores',
  players: '/jugadores',
  profile: '/jugador',
  similarity: '/similitud',
  comparison: '/comparar',
  clutch: '/estadisticas-clutch',
  lineups: '/alineaciones',
  rankings: '/mejores-alineaciones',
  shots: '/cartas-tiro',
  zoneleaders: '/lideres-zona',
  about: '/info',
}

// Navigation structure: single tabs and grouped dropdowns
const NAV = [
  {
    id: 'equipos', label: 'Equipos', short: 'Equipos', icon: BarChart3,
    tabs: [
      { id: 'teams',       label: 'Estadísticas de Equipo', icon: BarChart3 },
      { id: 'fingerprint', label: 'Perfil de Equipo',       icon: Fingerprint },
      { id: 'gameflow',    label: 'Análisis de Partido',    icon: Activity },
      { id: 'factors',     label: 'Four Factors',           icon: Percent },
    ],
  },
  {
    id: 'jugadores', label: 'Jugadores', short: 'Jugadores', icon: Users,
    tabs: [
      { id: 'players', label: 'Estadísticas de Jugador', icon: Users },
      { id: 'profile', label: 'Perfil de Jugador',       icon: UserCircle },
      { id: 'clutch',  label: 'Estadísticas Clutch',     icon: Flame },
    ],
  },
  {
    id: 'herramientas', label: 'Herramientas', short: 'Herramientas', icon: Sparkles,
    tabs: [
      { id: 'similarity', label: 'Similitud',          icon: Sparkles },
      { id: 'comparison', label: 'Comparar Jugadores', icon: GitCompareArrows },
      { id: 'matchup',    label: 'Cara a Cara',        icon: Scale },
    ],
  },
  {
    id: 'alineaciones', label: 'Alineaciones', short: 'Alineaciones', icon: TrendingUp,
    tabs: [
      { id: 'lineups',  label: 'Análisis On/Off',      icon: TrendingUp },
      { id: 'rankings', label: 'Mejores Alineaciones', icon: Trophy },
    ],
  },
  {
    id: 'tiro', label: 'Tiro', short: 'Tiro', icon: Target,
    tabs: [
      { id: 'shots',       label: 'Cartas de Tiro',   icon: Target },
      { id: 'zoneleaders', label: 'Líderes por Zona', icon: Crown },
    ],
  },
  { id: 'about', label: 'Info', short: 'Info', icon: Info, single: true },
]

// Derive active tab id from the current URL pathname
function getTabFromPath(pathname) {
  for (const [tabId, path] of Object.entries(TAB_PATHS)) {
    if (tabId === 'home') continue // handle home separately
    if (pathname.startsWith(path)) return tabId
  }
  return pathname === '/' ? 'home' : null
}

// Returns the group id that contains the given tab id
function getActiveGroup(tabId) {
  for (const item of NAV) {
    if (item.single && item.id === tabId) return item.id
    if (item.tabs && item.tabs.some(t => t.id === tabId)) return item.id
  }
  return null
}

function App() {
  const navigate = useNavigate()
  const location = useLocation()
  const activeTab = getTabFromPath(location.pathname)

  const [data, setData] = useState({ teams: [], teamsByStage: [], players: [], playersByStage: [], similarity: [], teamLogos: {}, playerPhotos: {}, playerBio: {} })
  const [loading, setLoading] = useState(true)
  const [menuOpen, setMenuOpen] = useState(false)
  const [openGroup, setOpenGroup] = useState(null)
  const menuRef = useRef(null)
  const dropdownRef = useRef(null)

  // Close mobile menu on outside click
  useEffect(() => {
    const handler = (e) => {
      if (menuRef.current && menuRef.current.contains(e.target)) return
      if (dropdownRef.current && dropdownRef.current.contains(e.target)) return
      setMenuOpen(false)
    }
    document.addEventListener('mousedown', handler)
    return () => document.removeEventListener('mousedown', handler)
  }, [])

  const [shotsCache, setShotsCache] = useState({})
  const [loadingShots, setLoadingShots] = useState({})
  const [lineupsCache, setLineupsCache] = useState({})
  const [loadingLineups, setLoadingLineups] = useState({})
  const [gameFlowCache, setGameFlowCache] = useState({})
  const [loadingGameFlow, setLoadingGameFlow] = useState({})
  const [teamPaceCache, setTeamPaceCache] = useState({})
  const [loadingTeamPace, setLoadingTeamPace] = useState({})
  const [clutchCache, setClutchCache] = useState({})
  const [loadingClutch, setLoadingClutch] = useState({})

  useEffect(() => {
    async function loadData() {
      try {
        const [teamsRes, teamsByStageRes, playersRes, playersByStageRes, similarityRes, teamLogosRes, playerPhotosRes, playerBioRes] = await Promise.all([
          fetch('/data/teams.json'),
          fetch('/data/teams-by-stage.json'),
          fetch('/data/players.json'),
          fetch('/data/players-by-stage.json'),
          fetch('/data/similarity.json'),
          fetch('/data/team-logos.json'),
          fetch('/data/player-photos.json'),
          fetch('/data/player-bio.json'),
        ])
        const [teams, teamsByStage, players, playersByStage, similarity, teamLogos, playerPhotos, playerBio] = await Promise.all([
          teamsRes.json(),
          teamsByStageRes.ok ? teamsByStageRes.json() : [],
          playersRes.json(),
          playersByStageRes.ok ? playersByStageRes.json() : [],
          similarityRes.ok ? similarityRes.json() : [],
          teamLogosRes.ok ? teamLogosRes.json() : {},
          playerPhotosRes.ok ? playerPhotosRes.json() : {},
          playerBioRes.ok ? playerBioRes.json() : {},
        ])
        setData({
          teams,
          teamsByStage,
          players,
          playersByStage,
          similarity,
          teamLogos,
          playerPhotos,
          playerBio,
        })
      } catch (error) {
        console.error('Error cargando datos:', error)
      } finally {
        setLoading(false)
      }
    }
    loadData()
  }, [])

  const loadShotsForSeason = async (season) => {
    if (shotsCache[season]) return shotsCache[season]
    if (loadingShots[season]) return []
    try {
      setLoadingShots(prev => ({ ...prev, [season]: true }))
      const response = await fetch(`/data/shots-${season}.json`)
      const shots = await response.json()
      setShotsCache(prev => ({ ...prev, [season]: shots }))
      setLoadingShots(prev => ({ ...prev, [season]: false }))
      return shots
    } catch (error) {
      console.error(`Error loading shots for season ${season}:`, error)
      setLoadingShots(prev => ({ ...prev, [season]: false }))
      return []
    }
  }

  const loadLineupsForSeason = async (season) => {
    if (lineupsCache[season]) return lineupsCache[season]
    if (loadingLineups[season]) return null
    try {
      setLoadingLineups(prev => ({ ...prev, [season]: true }))
      const response = await fetch(`/data/lineups-${season}.json`)
      if (!response.ok) throw new Error('Lineup data not found')
      const lineupData = await response.json()
      setLineupsCache(prev => ({ ...prev, [season]: lineupData }))
      setLoadingLineups(prev => ({ ...prev, [season]: false }))
      return lineupData
    } catch (error) {
      console.error(`Error loading lineups for season ${season}:`, error)
      setLoadingLineups(prev => ({ ...prev, [season]: false }))
      return null
    }
  }

  const loadGameFlowForSeason = async (season) => {
    if (gameFlowCache[season]) return gameFlowCache[season]
    if (loadingGameFlow[season]) return []
    try {
      setLoadingGameFlow(prev => ({ ...prev, [season]: true }))
      const response = await fetch(`/data/gameflow-${season}.json`)
      if (!response.ok) throw new Error('Game flow data not found')
      const gameFlowData = await response.json()
      setGameFlowCache(prev => ({ ...prev, [season]: gameFlowData }))
      setLoadingGameFlow(prev => ({ ...prev, [season]: false }))
      return gameFlowData
    } catch (error) {
      console.error(`Error loading game flow for season ${season}:`, error)
      setLoadingGameFlow(prev => ({ ...prev, [season]: false }))
      return []
    }
  }

  const loadTeamPaceForSeason = async (season) => {
    if (teamPaceCache[season]) return teamPaceCache[season]
    if (loadingTeamPace[season]) return []
    try {
      setLoadingTeamPace(prev => ({ ...prev, [season]: true }))
      const response = await fetch(`/data/teampace-${season}.json`)
      if (!response.ok) throw new Error('Team pace data not found')
      const teamPaceData = await response.json()
      setTeamPaceCache(prev => ({ ...prev, [season]: teamPaceData }))
      setLoadingTeamPace(prev => ({ ...prev, [season]: false }))
      return teamPaceData
    } catch (error) {
      console.error(`Error loading team pace for season ${season}:`, error)
      setLoadingTeamPace(prev => ({ ...prev, [season]: false }))
      return []
    }
  }

  const loadClutchForSeason = async (season) => {
    if (clutchCache[season]) return clutchCache[season]
    if (loadingClutch[season]) return null
    try {
      setLoadingClutch(prev => ({ ...prev, [season]: true }))
      const response = await fetch(`/data/clutch-${season}.json`)
      if (!response.ok) throw new Error('Clutch data not found')
      const clutchData = await response.json()
      setClutchCache(prev => ({ ...prev, [season]: clutchData }))
      setLoadingClutch(prev => ({ ...prev, [season]: false }))
      return clutchData
    } catch (error) {
      console.error(`Error loading clutch data for season ${season}:`, error)
      setLoadingClutch(prev => ({ ...prev, [season]: false }))
      return null
    }
  }

  // Helper: navigate to a tab by its id
  const goTo = (tabId) => navigate(TAB_PATHS[tabId] || '/')

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-acb-500">Cargando datos...</div>
      </div>
    )
  }

  const activeGroupId = getActiveGroup(activeTab)

  return (
    <>
      <div className="min-h-screen bg-acb-50">
        {/* Header */}
        <header className="bg-white border-b border-acb-200 sticky top-0 z-50 relative">
          <div className="h-12 sm:h-16 xl:h-20">
            <div className="max-w-7xl mx-auto px-3 sm:px-6 lg:px-8 h-full">
              <div className="flex items-center justify-between h-full">

                {/* Logo */}
                <Link
                  to="/"
                  onClick={() => setMenuOpen(false)}
                  className="flex items-center gap-1.5 sm:gap-2 hover:opacity-80 transition-opacity shrink-0"
                >
                  <img
                    src="/openacb_nobckg.png"
                    alt="openACB Logo"
                    className="w-10 h-10 sm:w-14 sm:h-14 xl:w-20 xl:h-20 object-contain"
                  />
                  <h1 className="text-base sm:text-lg font-semibold text-acb-900 hidden sm:block">openACB</h1>
                </Link>

                {/* Desktop Navigation */}
                <nav className="hidden xl:flex items-center gap-0.5 ml-4 flex-1 min-w-0">
                  {NAV.map((item) => {
                    const Icon = item.icon
                    const isActive = activeGroupId === item.id || (item.single && activeTab === item.id)

                    if (item.single) {
                      return (
                        <Link
                          key={item.id}
                          to={TAB_PATHS[item.id] || '/'}
                          className={`flex items-center gap-1.5 px-2.5 py-1.5 rounded-md border transition-colors text-xs font-medium whitespace-nowrap
                            ${isActive ? 'border-accent-200 bg-accent-50 text-accent-700 font-semibold' : 'border-transparent text-acb-600 hover:bg-accent-100 hover:text-accent-700'}`}
                        >
                          <Icon className="w-3.5 h-3.5" />
                          <span>{item.short}</span>
                        </Link>
                      )
                    }

                    // Grouped dropdown
                    return (
                      <div
                        key={item.id}
                        className="relative"
                        onMouseEnter={() => setOpenGroup(item.id)}
                        onMouseLeave={() => setOpenGroup(null)}
                      >
                        <button
                          className={`flex items-center gap-1.5 px-2.5 py-1.5 rounded-md border transition-colors text-xs font-medium whitespace-nowrap
                            ${isActive ? 'border-accent-200 bg-accent-50 text-accent-700 font-semibold' : 'border-transparent text-acb-600 hover:bg-accent-100 hover:text-accent-700'}`}
                        >
                          <Icon className="w-3.5 h-3.5" />
                          <span>{item.short}</span>
                          <ChevronDown className="w-3 h-3 opacity-40" />
                        </button>

                        {openGroup === item.id && (
                          <div className="absolute top-full left-0 pt-1 z-50 min-w-max">
                          <div className="bg-white border border-acb-200 rounded-lg shadow-lg py-1">
                            {item.tabs.map(tab => {
                              const TabIcon = tab.icon
                              return (
                                <Link
                                  key={tab.id}
                                  to={TAB_PATHS[tab.id] || '/'}
                                  onClick={() => setOpenGroup(null)}
                                  className={`flex items-center gap-2.5 w-full px-4 py-2 text-sm transition-colors border-l-2
                                    ${activeTab === tab.id
                                      ? 'border-accent-500 bg-accent-50 text-accent-700 font-semibold'
                                      : 'border-transparent text-acb-600 hover:bg-accent-100 hover:text-accent-700'}`}
                                >
                                  <TabIcon className="w-4 h-4" />
                                  {tab.label}
                                </Link>
                              )
                            })}
                          </div>
                          </div>
                        )}
                      </div>
                    )
                  })}
                </nav>

                {/* Hamburger button */}
                <div className="xl:hidden" ref={menuRef}>
                  <button
                    onClick={() => setMenuOpen(!menuOpen)}
                    className="p-2 rounded-md text-acb-600 hover:text-acb-900 hover:bg-acb-100 transition-colors"
                  >
                    {menuOpen ? <X className="w-5 h-5 sm:w-6 sm:h-6" /> : <Menu className="w-5 h-5 sm:w-6 sm:h-6" />}
                  </button>
                </div>
              </div>
            </div>
          </div>

          {/* Mobile dropdown */}
          {menuOpen && (
            <div ref={dropdownRef} className="xl:hidden absolute left-0 right-0 top-full bg-white border-b border-acb-200 shadow-lg z-50">
              <nav className="max-w-7xl mx-auto px-3 sm:px-4 py-2 flex flex-col gap-0.5">
                {NAV.map((item) => {
                  const Icon = item.icon

                  if (item.single) {
                    return (
                      <Link
                        key={item.id}
                        to={TAB_PATHS[item.id] || '/'}
                        onClick={() => setMenuOpen(false)}
                        className={`flex items-center gap-3 px-3 py-2.5 rounded-md transition-colors text-sm font-medium
                          ${activeTab === item.id ? 'bg-acb-100 text-acb-900' : 'text-acb-600 hover:text-acb-900 hover:bg-acb-50'}`}
                      >
                        <Icon className="w-4 h-4" />
                        {item.label}
                      </Link>
                    )
                  }

                  return (
                    <div key={item.id} className="mt-1">
                      <div className="flex items-center gap-2 px-3 py-1 text-xs font-semibold text-acb-400 uppercase tracking-wider">
                        <Icon className="w-3.5 h-3.5" />
                        {item.label}
                      </div>
                      {item.tabs.map(tab => {
                        const TabIcon = tab.icon
                        return (
                          <Link
                            key={tab.id}
                            to={TAB_PATHS[tab.id] || '/'}
                            onClick={() => setMenuOpen(false)}
                            className={`flex items-center gap-3 w-full px-3 py-2 pl-7 rounded-md transition-colors text-sm border-l-2
                              ${activeTab === tab.id ? 'border-accent-500 bg-accent-50 text-accent-700 font-semibold' : 'border-transparent text-acb-600 hover:bg-accent-100 hover:text-accent-700'}`}
                          >
                            <TabIcon className="w-4 h-4" />
                            {tab.label}
                          </Link>
                        )
                      })}
                    </div>
                  )
                })}
              </nav>
            </div>
          )}
        </header>

        {/* Main Content */}
        <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6">
          <Routes>
            <Route path="/" element={<Home />} />
            <Route path="/equipos" element={<TeamStats teams={data.teamsByStage} teamLogos={data.teamLogos} />} />
            <Route path="/perfil-equipo" element={<TeamFingerprint teams={data.teams} teamLogos={data.teamLogos} />} />
            <Route path="/perfil-equipo/:season/:team" element={<TeamFingerprint teams={data.teams} teamLogos={data.teamLogos} />} />
            <Route path="/matchup-equipos" element={
              <TeamMatchup
                teams={data.teams}
                teamLogos={data.teamLogos}
                loadTeamPaceForSeason={loadTeamPaceForSeason}
                teamPaceCache={teamPaceCache}
                loadingTeamPace={loadingTeamPace}
                loadClutchForSeason={loadClutchForSeason}
                clutchCache={clutchCache}
                loadingClutch={loadingClutch}
                loadShotsForSeason={loadShotsForSeason}
                shotsCache={shotsCache}
                loadingShots={loadingShots}
              />
            } />
            <Route path="/matchup-equipos/:season/:teamA/:teamB" element={
              <TeamMatchup
                teams={data.teams}
                teamLogos={data.teamLogos}
                loadTeamPaceForSeason={loadTeamPaceForSeason}
                teamPaceCache={teamPaceCache}
                loadingTeamPace={loadingTeamPace}
                loadClutchForSeason={loadClutchForSeason}
                clutchCache={clutchCache}
                loadingClutch={loadingClutch}
                loadShotsForSeason={loadShotsForSeason}
                shotsCache={shotsCache}
                loadingShots={loadingShots}
              />
            } />
            <Route path="/flujo-partido" element={
              <GameFlow
                teams={data.teams}
                loadGameFlowForSeason={loadGameFlowForSeason}
                gameFlowCache={gameFlowCache}
                loadingGameFlow={loadingGameFlow}
                loadTeamPaceForSeason={loadTeamPaceForSeason}
                teamPaceCache={teamPaceCache}
                loadingTeamPace={loadingTeamPace}
                loadClutchForSeason={loadClutchForSeason}
                clutchCache={clutchCache}
                loadingClutch={loadingClutch}
              />
            } />
            <Route path="/cuatro-factores" element={<FourFactors teams={data.teams} />} />
            <Route path="/jugadores" element={
              <PlayerStats
                players={data.playersByStage}
                playerBio={data.playerBio}
              />
            } />
            <Route path="/jugador" element={
              <PlayerProfile
                players={data.playersByStage}
                allPlayers={data.players}
                playerPhotos={data.playerPhotos}
                playerBio={data.playerBio}
                loadLineupsForSeason={loadLineupsForSeason}
                lineupsCache={lineupsCache}
                loadingLineups={loadingLineups}
                loadClutchForSeason={loadClutchForSeason}
                clutchCache={clutchCache}
                loadingClutch={loadingClutch}
              />
            } />
            <Route path="/jugador/:licenseId" element={
              <PlayerProfile
                players={data.playersByStage}
                allPlayers={data.players}
                playerPhotos={data.playerPhotos}
                playerBio={data.playerBio}
                loadLineupsForSeason={loadLineupsForSeason}
                lineupsCache={lineupsCache}
                loadingLineups={loadingLineups}
                loadClutchForSeason={loadClutchForSeason}
                clutchCache={clutchCache}
                loadingClutch={loadingClutch}
              />
            } />
            <Route path="/similitud" element={
              <PlayerSimilarity
                players={data.players}
                similarity={data.similarity}
              />
            } />
            <Route path="/similitud/:licenseId/:season" element={
              <PlayerSimilarity
                players={data.players}
                similarity={data.similarity}
              />
            } />
            <Route path="/comparar" element={
              <PlayerComparison
                players={data.players}
                playerPhotos={data.playerPhotos}
                playerBio={data.playerBio}
                loadLineupsForSeason={loadLineupsForSeason}
                lineupsCache={lineupsCache}
                loadingLineups={loadingLineups}
              />
            } />
            <Route path="/comparar/:aId/:aSeason/:bId/:bSeason" element={
              <PlayerComparison
                players={data.players}
                playerPhotos={data.playerPhotos}
                playerBio={data.playerBio}
                loadLineupsForSeason={loadLineupsForSeason}
                lineupsCache={lineupsCache}
                loadingLineups={loadingLineups}
              />
            } />
            <Route path="/alineaciones" element={
              <LineupAnalysis
                teams={data.teams}
                loadLineupsForSeason={loadLineupsForSeason}
                lineupsCache={lineupsCache}
                loadingLineups={loadingLineups}
                playerPhotos={data.playerPhotos}
              />
            } />
            <Route path="/alineaciones/:season/:team" element={
              <LineupAnalysis
                teams={data.teams}
                loadLineupsForSeason={loadLineupsForSeason}
                lineupsCache={lineupsCache}
                loadingLineups={loadingLineups}
                playerPhotos={data.playerPhotos}
              />
            } />
            <Route path="/mejores-alineaciones" element={
              <LineupRankings
                teams={data.teams}
                loadLineupsForSeason={loadLineupsForSeason}
                lineupsCache={lineupsCache}
                loadingLineups={loadingLineups}
              />
            } />
            <Route path="/cartas-tiro" element={
              <ShotCharts
                loadShotsForSeason={loadShotsForSeason}
                shotsCache={shotsCache}
                loadingShots={loadingShots}
                teams={data.teams}
                players={data.players}
                playerPhotos={data.playerPhotos}
              />
            } />
            <Route path="/lideres-zona" element={
              <ZoneLeaders
                loadShotsForSeason={loadShotsForSeason}
                shotsCache={shotsCache}
                loadingShots={loadingShots}
                teams={data.teams}
                players={data.players}
                playerPhotos={data.playerPhotos}
              />
            } />
            <Route path="/lideres-zona/:season/:metric" element={
              <ZoneLeaders
                loadShotsForSeason={loadShotsForSeason}
                shotsCache={shotsCache}
                loadingShots={loadingShots}
                teams={data.teams}
                players={data.players}
                playerPhotos={data.playerPhotos}
              />
            } />
            <Route path="/estadisticas-clutch" element={
              <ClutchStats
                teams={data.teams}
                players={data.players}
                playerBio={data.playerBio}
                loadClutchForSeason={loadClutchForSeason}
                clutchCache={clutchCache}
                loadingClutch={loadingClutch}
              />
            } />
            <Route path="/info" element={<About />} />
            {/* Catch-all: redirect to home */}
            <Route path="*" element={<Home />} />
          </Routes>
        </main>

        {/* Footer */}
        <footer className="border-t border-acb-200 bg-white mt-12">
          <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6">
            <div className="flex items-center justify-between text-sm text-acb-500">
              <p>hecho con cariño por <a href="https://juantorrecillas.es" className="text-acb-600 hover:text-lemon underline">juan torrecillas</a> 🍋</p>
            </div>
          </div>
        </footer>
      </div>
      <Analytics />
    </>
  )
}

export default App
