import { lazy, Suspense, useCallback, useEffect, useMemo, useRef, useState } from 'react'
import { Routes, Route, useLocation, Link, Navigate } from 'react-router-dom'
import { Analytics } from '@vercel/analytics/react'
import { BarChart3, Target, Users, TrendingUp, Percent, Trophy, Info, UserCircle, Menu, X, GitCompareArrows, Fingerprint, ChevronDown, Activity, Crown, Flame, Scale, Sparkles } from 'lucide-react'
import {
  ROUTE_MANIFEST,
  buildAboutPath,
  buildFourFactorsPath,
  buildGamesPath,
  buildPlayerSegment,
  buildLineupAnalysisPath,
  buildLineupRankingsPath,
  buildPlayerClutchPath,
  buildPlayerComparisonPath,
  buildPlayerProfilePath,
  buildPlayerSimilarityPath,
  buildPlayerStatsPath,
  buildShotChartsPath,
  buildTeamComparisonPath,
  buildTeamClutchPath,
  buildTeamProfilePath,
  buildTeamQuartersPath,
  buildTeamStatsPath,
  buildZoneLeadersPath,
  canonicalizePathname,
  getPlayerProfileSlug,
  matchCanonicalRoute,
  matchLegacyRoute,
  resolveCanonicalEntityLocation,
  resolveLegacyLocation,
} from './routing'
const Home = lazy(() => import('./pages/Home'))
const ShotCharts = lazy(() => import('./pages/ShotCharts'))
const TeamStats = lazy(() => import('./pages/TeamStats'))
const PlayerStats = lazy(() => import('./pages/PlayerStats'))
const LineupAnalysis = lazy(() => import('./pages/LineupAnalysis'))
const LineupRankings = lazy(() => import('./pages/LineupRankings'))
const FourFactors = lazy(() => import('./pages/FourFactors'))
const PlayerProfile = lazy(() => import('./pages/PlayerProfile'))
const About = lazy(() => import('./pages/About'))
const PlayerSimilarity = lazy(() => import('./pages/PlayerSimilarity'))
const PlayerComparison = lazy(() => import('./pages/PlayerComparison'))
const TeamFingerprint = lazy(() => import('./pages/TeamFingerprint'))
const TeamMatchup = lazy(() => import('./pages/TeamMatchup'))
const GameFlow = lazy(() => import('./pages/GameFlow'))
const TeamClutchPage = lazy(() => import('./pages/GameFlow').then(module => ({ default: module.TeamClutchPage })))
const TeamPace = lazy(() => import('./pages/TeamPace'))
const ZoneLeaders = lazy(() => import('./pages/ZoneLeaders'))
const ClutchStats = lazy(() => import('./pages/ClutchStats'))

const DATA_RESOURCES = {
  teams: { url: '/data/teams.json' },
  teamsByStage: { url: '/data/teams-by-stage.json' },
  players: { url: '/data/players.json' },
  playersByStage: { url: '/data/players-by-stage.json' },
  playerNames: { url: '/data/player-names.json', fallback: {} },
  similarity: { url: '/data/similarity.json' },
  teamLogos: { url: '/data/team-logos.json', fallback: {} },
  playerPhotos: { url: '/data/player-photos.json', fallback: {} },
  playerBio: { url: '/data/player-bio.json', fallback: {} },
  teamIdentities: { url: '/data/team-identities.json' },
}

const DATA_REQUIREMENTS = {
  home: [],
  about: [],
  teams: ['teamsByStage', 'teamLogos'],
  fingerprint: ['teams', 'players', 'playerNames', 'playerBio', 'teamLogos'],
  matchup: ['teams', 'teamLogos'],
  gameflow: ['teams', 'players', 'playerNames'],
  quarters: ['teams'],
  teamClutch: ['teams'],
  factors: ['teams'],
  players: ['playersByStage', 'playerNames', 'playerBio'],
  profile: ['players', 'playersByStage', 'playerNames', 'playerPhotos', 'playerBio'],
  similarity: ['players', 'playerNames', 'similarity'],
  comparison: ['players', 'playerNames', 'playerPhotos', 'playerBio'],
  clutch: ['teams', 'players', 'playerNames', 'playerBio'],
  lineups: ['teams', 'players', 'playerNames', 'playerPhotos'],
  rankings: ['teams', 'players', 'playerNames'],
  shots: ['teams', 'players', 'playerNames', 'playerPhotos'],
  zoneleaders: ['teams', 'players', 'playerNames', 'playerPhotos'],
}

const INITIAL_DATA = Object.fromEntries(Object.keys(DATA_RESOURCES).map(key => [key, null]))

const TAB_PATHS = {
  home: '/',
  teams: buildTeamStatsPath(),
  fingerprint: buildTeamProfilePath(),
  matchup: buildTeamComparisonPath(),
  gameflow: buildGamesPath(),
  factors: buildFourFactorsPath(),
  players: buildPlayerStatsPath(),
  profile: buildPlayerProfilePath(),
  similarity: buildPlayerSimilarityPath(),
  comparison: buildPlayerComparisonPath(),
  clutch: buildPlayerClutchPath(),
  lineups: buildLineupAnalysisPath(),
  rankings: buildLineupRankingsPath(),
  shots: buildShotChartsPath(),
  zoneleaders: buildZoneLeadersPath(),
  about: buildAboutPath(),
}

// navigation structure: single tabs and grouped dropdowns
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

// return the group id that contains the given tab id
function getActiveGroup(tabId) {
  for (const item of NAV) {
    if (item.single && item.id === tabId) return item.id
    if (item.tabs && item.tabs.some(t => t.id === tabId)) return item.id
  }
  return null
}

function RouteProblem({ title, message }) {
  return (
    <div className="mx-auto my-16 max-w-lg rounded-lg border border-acb-200 bg-white p-6 text-center shadow-sm" role="alert">
      <h1 className="text-xl font-semibold text-acb-900">{title}</h1>
      <p className="mt-2 text-sm text-acb-600">{message}</p>
      <Link to="/" className="mt-5 inline-flex rounded-lg bg-acb-900 px-4 py-2 text-sm font-medium text-white hover:bg-acb-800">
        Volver al inicio
      </Link>
    </div>
  )
}

function LegacyRedirect({ context }) {
  const location = useLocation()
  const resolution = resolveLegacyLocation(location, context)

  if (resolution?.status === 'redirect') return <Navigate to={resolution.to} replace />
  if (resolution?.status === 'needs-data') {
    return <div className="py-16 text-center text-acb-500" role="status">Cargando enlace...</div>
  }
  if (resolution?.status === 'ambiguous') {
    return (
      <RouteProblem
        title="El enlace necesita más información"
        message="Hay más de una entidad que coincide con este enlace antiguo. Abre la herramienta y selecciona la opción correcta."
      />
    )
  }
  return <RouteProblem title="Enlace no encontrado" message="La entidad indicada en este enlace no existe o ya no está disponible." />
}

function App() {
  const location = useLocation()
  const normalizedPathname = canonicalizePathname(location.pathname).toLocaleLowerCase('es')
  const canonicalMatch = matchCanonicalRoute(normalizedPathname)
  const legacyMatch = canonicalMatch ? null : matchLegacyRoute(normalizedPathname)
  const routeMatch = canonicalMatch || legacyMatch
  const pathNeedsNormalization = Boolean(routeMatch && normalizedPathname !== location.pathname)
  const activeTab = routeMatch?.route.tabId || null

  const [data, setData] = useState(INITIAL_DATA)
  const [loadError, setLoadError] = useState('')
  const [retryToken, setRetryToken] = useState(0)
  const [menuOpen, setMenuOpen] = useState(false)
  const [openGroup, setOpenGroup] = useState(null)
  const menuRef = useRef(null)
  const dropdownRef = useRef(null)
  const dataRequestsRef = useRef({})

  // close the mobile menu on outside click
  useEffect(() => {
    const handler = (e) => {
      if (menuRef.current && menuRef.current.contains(e.target)) return
      if (dropdownRef.current && dropdownRef.current.contains(e.target)) return
      setMenuOpen(false)
    }
    document.addEventListener('mousedown', handler)
    return () => document.removeEventListener('mousedown', handler)
  }, [])

  useEffect(() => {
    document.title = activeTab === 'home'
      ? 'openACB'
      : `${routeMatch?.route.label || 'Página no encontrada'} | openACB`
  }, [activeTab, routeMatch?.route.label])

  useEffect(() => {
    setMenuOpen(false)
    setOpenGroup(null)
    window.scrollTo({ top: 0, left: 0 })
  }, [location.pathname])

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

  const legacyNeedsTeamIdentities = Boolean(
    legacyMatch
    && ['team', 'teamA', 'teamB'].some(param => legacyMatch.params[param])
  )
  const requiredResources = legacyNeedsTeamIdentities
    ? [...(DATA_REQUIREMENTS[activeTab] || []), 'teamIdentities']
    : (DATA_REQUIREMENTS[activeTab] || [])
  const missingResources = requiredResources.filter(key => data[key] == null)
  const missingKey = missingResources.join('|')

  useEffect(() => {
    if (!missingKey) {
      setLoadError('')
      return
    }

    setLoadError('')
    const loadResource = (key) => {
      if (dataRequestsRef.current[key]) return dataRequestsRef.current[key]

      const resource = DATA_RESOURCES[key]
      const request = fetch(resource.url)
        .then(async response => {
          if (!response.ok) {
            if (Object.hasOwn(resource, 'fallback')) return resource.fallback
            throw new Error(`No se pudo cargar ${resource.url}`)
          }
          return response.json()
        })
        .then(value => {
          setData(previous => previous[key] == null ? { ...previous, [key]: value } : previous)
          return value
        })
        .finally(() => {
          delete dataRequestsRef.current[key]
        })

      dataRequestsRef.current[key] = request
      return request
    }

    Promise.all(missingKey.split('|').map(loadResource)).catch(error => {
      console.error('Error cargando datos:', error)
      setLoadError('No se han podido cargar los datos. Comprueba la conexión e inténtalo de nuevo.')
    })
  }, [missingKey, retryToken])

  const playerProfileSlugs = useMemo(() => {
    const names = data.playerNames || {}
    const rows = [...(data.players || []), ...(data.playersByStage || [])]
    const idsBySlug = new Map()
    const slugById = new Map()
    const playerById = new Map()

    rows.forEach(player => {
      const id = String(player.licenseId)
      const slug = getPlayerProfileSlug(player, names[id])
      if (!idsBySlug.has(slug)) idsBySlug.set(slug, new Set())
      idsBySlug.get(slug).add(id)
      slugById.set(id, slug)
      if (!playerById.has(id)) playerById.set(id, player)
    })

    return Object.fromEntries([...slugById].map(([id, slug]) => [
      id,
      idsBySlug.get(slug).size > 1
        ? buildPlayerSegment(playerById.get(id), names[id])
        : slug,
    ]))
  }, [data.playerNames, data.players, data.playersByStage])

  const namedPlayers = useMemo(() => {
    const names = data.playerNames || {}
    return (data.players || []).map(player => ({
      ...player,
      playerDisplay: names[String(player.licenseId)] || undefined,
      profileSlug: playerProfileSlugs[String(player.licenseId)],
    }))
  }, [data.players, data.playerNames, playerProfileSlugs])

  const namedPlayersByStage = useMemo(() => {
    const names = data.playerNames || {}
    return (data.playersByStage || []).map(player => ({
      ...player,
      playerDisplay: names[String(player.licenseId)] || undefined,
      profileSlug: playerProfileSlugs[String(player.licenseId)],
    }))
  }, [data.playersByStage, data.playerNames, playerProfileSlugs])

  const requestedGameSeason = canonicalMatch?.routeId === 'games'
    ? Number(new URLSearchParams(location.search).get('temporada'))
    : null
  const routeGames = Number.isInteger(requestedGameSeason)
    && Object.hasOwn(gameFlowCache, requestedGameSeason)
    ? gameFlowCache[requestedGameSeason]
    : undefined
  const routingContext = useMemo(() => ({
    players: namedPlayersByStage.length > 0 ? namedPlayersByStage : namedPlayers,
    teamIdentities: data.teamIdentities,
    teamRows: data.teams,
    ...(routeGames !== undefined ? { games: routeGames } : {}),
  }), [data.teamIdentities, data.teams, namedPlayers, namedPlayersByStage, routeGames])
  const canonicalResolution = canonicalMatch
    ? resolveCanonicalEntityLocation(location, routingContext)
    : null

  const loadShotsForSeason = useCallback(async (season) => {
    if (shotsCache[season]) return shotsCache[season]
    if (loadingShots[season]) return []
    try {
      setLoadingShots(prev => ({ ...prev, [season]: true }))
      const response = await fetch(`/data/shots-${season}.json`)
      if (!response.ok) throw new Error('Shot data not found')
      const shots = await response.json()
      setShotsCache(prev => ({ ...prev, [season]: shots }))
      setLoadingShots(prev => ({ ...prev, [season]: false }))
      return shots
    } catch (error) {
      console.error(`Error loading shots for season ${season}:`, error)
      setShotsCache(prev => ({ ...prev, [season]: [] }))
      setLoadingShots(prev => ({ ...prev, [season]: false }))
      return []
    }
  }, [shotsCache, loadingShots])

  const loadLineupsForSeason = useCallback(async (season) => {
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
      setLineupsCache(prev => ({ ...prev, [season]: {} }))
      setLoadingLineups(prev => ({ ...prev, [season]: false }))
      return null
    }
  }, [lineupsCache, loadingLineups])

  const loadGameFlowForSeason = useCallback(async (season) => {
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
      setGameFlowCache(prev => ({ ...prev, [season]: [] }))
      setLoadingGameFlow(prev => ({ ...prev, [season]: false }))
      return []
    }
  }, [gameFlowCache, loadingGameFlow])

  const loadTeamPaceForSeason = useCallback(async (season) => {
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
      setTeamPaceCache(prev => ({ ...prev, [season]: [] }))
      setLoadingTeamPace(prev => ({ ...prev, [season]: false }))
      return []
    }
  }, [teamPaceCache, loadingTeamPace])

  const loadClutchForSeason = useCallback(async (season) => {
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
      setClutchCache(prev => ({ ...prev, [season]: {} }))
      setLoadingClutch(prev => ({ ...prev, [season]: false }))
      return null
    }
  }, [clutchCache, loadingClutch])

  const activeGroupId = routeMatch?.route.navGroupId || getActiveGroup(activeTab)

  return (
    <>
      <div className="min-h-screen bg-acb-50">
        {/* header */}
        <header className="bg-white border-b border-acb-200 sticky top-0 z-50 relative">
          <div className="h-12 sm:h-16 xl:h-20">
            <div className="max-w-7xl mx-auto px-3 sm:px-6 lg:px-8 h-full">
              <div className="flex items-center justify-between h-full">

                {/* logo */}
                <Link
                  to="/"
                  onClick={() => setMenuOpen(false)}
                  className="flex items-center gap-1.5 sm:gap-2 hover:opacity-80 transition-opacity shrink-0"
                >
                  <img
                    src="/openacb_nobckg.png"
                    alt="Logotipo de openACB"
                    className="w-10 h-10 sm:w-14 sm:h-14 xl:w-20 xl:h-20 object-contain"
                  />
                  <span className="text-base sm:text-lg font-semibold text-acb-900 hidden sm:block">openACB</span>
                </Link>

                {/* desktop navigation */}
                <nav className="hidden xl:flex items-center gap-0.5 ml-4 flex-1 min-w-0" aria-label="Navegación principal">
                  {NAV.map((item) => {
                    const Icon = item.icon
                    const isActive = activeGroupId === item.id || (item.single && activeTab === item.id)

                    if (item.single) {
                      return (
                        <Link
                          key={item.id}
                          to={TAB_PATHS[item.id] || '/'}
                          className={`flex items-center gap-1.5 px-2.5 py-1.5 rounded-md border transition-colors text-xs font-medium whitespace-nowrap
                            ${isActive ? 'border-transparent text-accent-700 font-semibold' : 'border-transparent text-acb-600 hover:text-accent-700'}`}
                        >
                          <Icon className="w-3.5 h-3.5" />
                          <span>{item.short}</span>
                        </Link>
                      )
                    }

                    // grouped dropdown
                    return (
                      <div
                        key={item.id}
                        className="relative"
                        onMouseEnter={() => setOpenGroup(item.id)}
                        onMouseLeave={() => setOpenGroup(null)}
                        onBlur={(event) => {
                          if (!event.currentTarget.contains(event.relatedTarget)) setOpenGroup(null)
                        }}
                        onKeyDown={(event) => {
                          if (event.key === 'Escape') {
                            setOpenGroup(null)
                            event.currentTarget.querySelector('button')?.focus()
                          }
                        }}
                      >
                        <button
                          type="button"
                          onClick={() => setOpenGroup(item.id)}
                          aria-expanded={openGroup === item.id}
                          aria-haspopup="menu"
                          aria-controls={`desktop-menu-${item.id}`}
                          className={`flex items-center gap-1.5 px-2.5 py-1.5 rounded-md border transition-colors text-xs font-medium whitespace-nowrap
                            ${isActive ? 'border-transparent text-accent-700 font-semibold' : 'border-transparent text-acb-600 hover:text-accent-700'}`}
                        >
                          <Icon className="w-3.5 h-3.5" />
                          <span>{item.short}</span>
                          <ChevronDown className={`w-3 h-3 opacity-40 transition-transform ${openGroup === item.id ? 'rotate-180' : ''}`} />
                        </button>

                        {openGroup === item.id && (
                          <div className="absolute top-full left-0 pt-1 z-50 min-w-max">
                            <div id={`desktop-menu-${item.id}`} role="menu" className="bg-white border border-acb-200 rounded-lg shadow-lg py-1">
                            {item.tabs.map(tab => {
                              const TabIcon = tab.icon
                              return (
                                <Link
                                  key={tab.id}
                                  to={TAB_PATHS[tab.id] || '/'}
                                  onClick={() => setOpenGroup(null)}
                                  role="menuitem"
                                  className={`flex items-center gap-2.5 w-full px-4 py-2 text-xs transition-colors
                                    ${activeTab === tab.id
                                      ? 'text-accent-700 font-semibold'
                                      : 'text-acb-600 hover:text-accent-700'}`}
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

                {/* hamburger button */}
                <div className="xl:hidden" ref={menuRef}>
                  <button
                    type="button"
                    onClick={() => setMenuOpen(!menuOpen)}
                    aria-label={menuOpen ? 'Cerrar menú de navegación' : 'Abrir menú de navegación'}
                    aria-expanded={menuOpen}
                    aria-controls="mobile-navigation"
                    className="p-2 rounded-md text-acb-600 hover:text-acb-900 hover:bg-acb-100 transition-colors"
                  >
                    {menuOpen ? <X className="w-5 h-5 sm:w-6 sm:h-6" /> : <Menu className="w-5 h-5 sm:w-6 sm:h-6" />}
                  </button>
                </div>
              </div>
            </div>
          </div>

          {/* mobile dropdown */}
          {menuOpen && (
            <div
              id="mobile-navigation"
              ref={dropdownRef}
              className="xl:hidden absolute left-0 right-0 top-full max-h-[calc(100vh-3rem)] sm:max-h-[calc(100vh-4rem)] overflow-y-auto bg-white border-b border-acb-200 shadow-lg z-50"
            >
              <nav className="max-w-7xl mx-auto px-3 sm:px-4 py-2 flex flex-col gap-0.5" aria-label="Navegación móvil">
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
                            className={`flex items-center gap-3 w-full px-3 py-2 pl-7 rounded-md transition-colors text-sm
                              ${activeTab === tab.id ? 'text-accent-700 font-semibold' : 'text-acb-600 hover:text-accent-700'}`}
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

        {/* main content */}
        <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6">
          {pathNeedsNormalization ? (
            <Navigate to={`${normalizedPathname}${location.search}`} replace />
          ) : missingResources.length > 0 && !loadError ? (
            <div className="flex min-h-[40vh] items-center justify-center text-acb-500" role="status" aria-live="polite">
              Cargando datos...
            </div>
          ) : loadError ? (
            <div className="mx-auto my-16 max-w-md rounded-lg border border-negative-200 bg-white p-6 text-center shadow-sm" role="alert">
              <h1 className="text-xl font-semibold text-acb-900">No se pueden mostrar los datos</h1>
              <p className="mt-2 text-sm text-acb-600">{loadError}</p>
              <button
                type="button"
                onClick={() => setRetryToken(token => token + 1)}
                className="mt-5 rounded-lg bg-acb-900 px-4 py-2 text-sm font-medium text-white hover:bg-acb-800"
              >
                Reintentar
              </button>
            </div>
          ) : canonicalResolution?.status === 'redirect' ? (
            <Navigate to={canonicalResolution.to} replace />
          ) : canonicalResolution?.status === 'not-found' ? (
            <RouteProblem title="Enlace no encontrado" message="La entidad indicada en este enlace no existe o ya no está disponible." />
          ) : canonicalResolution?.status === 'ambiguous' ? (
            <RouteProblem title="El enlace necesita más información" message="Hay más de un jugador con ese nombre. Abre la herramienta y selecciona el jugador correcto." />
          ) : (
            <Suspense fallback={<div className="py-16 text-center text-acb-500" role="status">Cargando herramienta...</div>}>
              <Routes>
                <Route path="/" element={<Home />} />
                <Route path={buildTeamStatsPath()} element={<TeamStats teams={data.teamsByStage} teamLogos={data.teamLogos} />} />
                {[buildTeamProfilePath(), `${buildTeamProfilePath()}/:teamId`].map(path => (
                  <Route key={path} path={path} element={
                    <TeamFingerprint
                      teams={data.teams}
                      players={namedPlayers}
                      playerBio={data.playerBio}
                      teamLogos={data.teamLogos}
                      loadShotsForSeason={loadShotsForSeason}
                      shotsCache={shotsCache}
                      loadingShots={loadingShots}
                    />
                  } />
                ))}
                {[buildTeamComparisonPath(), `${buildTeamComparisonPath()}/:teamAId/:teamBId`].map(path => (
                  <Route key={path} path={path} element={
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
                ))}
                <Route path={buildFourFactorsPath()} element={<FourFactors teams={data.teams} />} />
                <Route path={buildTeamQuartersPath()} element={
                  <TeamPace
                    teams={data.teams}
                    loadTeamPaceForSeason={loadTeamPaceForSeason}
                    teamPaceCache={teamPaceCache}
                    loadingTeamPace={loadingTeamPace}
                  />
                } />
                <Route path={buildTeamClutchPath()} element={
                  <TeamClutchPage
                    teams={data.teams}
                    loadClutchForSeason={loadClutchForSeason}
                    clutchCache={clutchCache}
                    loadingClutch={loadingClutch}
                  />
                } />
                {[buildGamesPath(), `${buildGamesPath()}/:game`].map(path => (
                  <Route key={path} path={path} element={
                    <GameFlow
                      teams={data.teams}
                      playerRecords={namedPlayers}
                      loadGameFlowForSeason={loadGameFlowForSeason}
                      gameFlowCache={gameFlowCache}
                      loadingGameFlow={loadingGameFlow}
                    />
                  } />
                ))}
                <Route path={buildPlayerStatsPath()} element={<PlayerStats players={namedPlayersByStage} playerBio={data.playerBio} />} />
                {[buildPlayerProfilePath(), `${buildPlayerProfilePath()}/:player`].map(path => (
                  <Route key={path} path={path} element={
                    <PlayerProfile
                      players={namedPlayersByStage}
                      allPlayers={namedPlayers}
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
                ))}
                {[buildPlayerSimilarityPath(), `${buildPlayerSimilarityPath()}/:player`].map(path => (
                  <Route key={path} path={path} element={<PlayerSimilarity players={namedPlayers} similarity={data.similarity} />} />
                ))}
                {[buildPlayerComparisonPath(), `${buildPlayerComparisonPath()}/:playerA/:playerB`].map(path => (
                  <Route key={path} path={path} element={
                    <PlayerComparison
                      players={namedPlayers}
                      playerPhotos={data.playerPhotos}
                      playerBio={data.playerBio}
                      loadLineupsForSeason={loadLineupsForSeason}
                      lineupsCache={lineupsCache}
                      loadingLineups={loadingLineups}
                    />
                  } />
                ))}
                <Route path={buildPlayerClutchPath()} element={
                  <ClutchStats
                    teams={data.teams}
                    players={namedPlayers}
                    playerBio={data.playerBio}
                    loadClutchForSeason={loadClutchForSeason}
                    clutchCache={clutchCache}
                    loadingClutch={loadingClutch}
                  />
                } />
                {[buildLineupAnalysisPath(), `${buildLineupAnalysisPath()}/:teamId`].map(path => (
                  <Route key={path} path={path} element={
                    <LineupAnalysis
                      teams={data.teams}
                      loadLineupsForSeason={loadLineupsForSeason}
                      lineupsCache={lineupsCache}
                      loadingLineups={loadingLineups}
                      playerPhotos={data.playerPhotos}
                      playerRecords={namedPlayers}
                    />
                  } />
                ))}
                <Route path={buildLineupRankingsPath()} element={
                  <LineupRankings
                    teams={data.teams}
                    loadLineupsForSeason={loadLineupsForSeason}
                    lineupsCache={lineupsCache}
                    loadingLineups={loadingLineups}
                    playerRecords={namedPlayers}
                  />
                } />
                {[buildShotChartsPath(), `${buildShotChartsPath()}/equipo/:teamId`, `${buildShotChartsPath()}/jugador/:player`].map(path => (
                  <Route key={path} path={path} element={
                    <ShotCharts
                      loadShotsForSeason={loadShotsForSeason}
                      shotsCache={shotsCache}
                      loadingShots={loadingShots}
                      teams={data.teams}
                      players={namedPlayers}
                      playerPhotos={data.playerPhotos}
                    />
                  } />
                ))}
                <Route path={buildZoneLeadersPath()} element={
                  <ZoneLeaders
                    loadShotsForSeason={loadShotsForSeason}
                    shotsCache={shotsCache}
                    loadingShots={loadingShots}
                    teams={data.teams}
                    players={namedPlayers}
                    playerPhotos={data.playerPhotos}
                  />
                } />
                <Route path={buildAboutPath()} element={<About />} />
                {ROUTE_MANIFEST.flatMap(route => route.legacyPatterns.map(path => (
                  <Route key={`legacy:${path}`} path={path} element={<LegacyRedirect context={routingContext} />} />
                )))}
                <Route path="/tiro" element={<LegacyRedirect context={routingContext} />} />
                <Route path="*" element={
                  <RouteProblem title="Página no encontrada" message="La dirección no corresponde a ninguna herramienta de openACB." />
                } />
              </Routes>
            </Suspense>
          )}
        </main>

        {/* footer */}
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
