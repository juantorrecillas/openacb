// @vitest-environment jsdom
import '@testing-library/jest-dom/vitest'
import { cleanup, render, screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { afterEach, describe, expect, test, vi } from 'vitest'
import {
  MemoryRouter,
  Navigate,
  Route,
  Routes,
  useLocation,
  useNavigate,
} from 'react-router-dom'
import { buildRouteUrl, resolveLegacyLocation } from './index.js'
import GameFlow from '../pages/GameFlow.jsx'
import ZoneLeaders from '../pages/ZoneLeaders.jsx'
import ClutchStats from '../pages/ClutchStats.jsx'
import PlayerProfile from '../pages/PlayerProfile.jsx'
import PlayerSimilarity from '../pages/PlayerSimilarity.jsx'

const teamIdentities = {
  teams: [
    {
      teamId: 'baskonia',
      aliases: [{ name: 'KIROLBET Baskonia', seasons: [2020] }],
    },
  ],
}

afterEach(cleanup)

function LocationProbe({ label }) {
  const location = useLocation()
  const navigate = useNavigate()
  return (
    <div>
      <output aria-label="ubicacion">{`${location.pathname}${location.search}`}</output>
      <span>{label}</span>
      <button type="button" onClick={() => navigate(-1)}>Atrás</button>
      <button type="button" onClick={() => navigate(1)}>Adelante</button>
    </div>
  )
}

function LegacyResolver({ context }) {
  const location = useLocation()
  const resolution = resolveLegacyLocation(location, context)
  if (resolution?.status === 'redirect') return <Navigate to={resolution.to} replace />
  return <output aria-label="resolucion">{resolution?.status || 'sin-resolver'}</output>
}

function HistoryHarness() {
  const navigate = useNavigate()
  return (
    <div>
      <LocationProbe label="historial" />
      <button
        type="button"
        onClick={() => navigate(buildRouteUrl('playerStats', {}, {
          temporada: 2025,
          fase: 'playoffs',
          vista: 'advanced',
        }))}
      >
        Cambiar filtros
      </button>
    </div>
  )
}

describe('navegación canónica', () => {
  test('un enlace antiguo se sustituye y no queda duplicado en el historial', async () => {
    const user = userEvent.setup()
    render(
      <MemoryRouter
        initialEntries={['/inicio-prueba', '/perfil-equipo/2020/kirolbet-baskonia']}
        initialIndex={1}
      >
        <Routes>
          <Route
            path="/perfil-equipo/:season/:team"
            element={<LegacyResolver context={{ teamIdentities }} />}
          />
          <Route path="/equipos/perfil/:teamId" element={<LocationProbe label="perfil" />} />
          <Route path="/inicio-prueba" element={<LocationProbe label="inicio" />} />
        </Routes>
      </MemoryRouter>
    )

    expect(await screen.findByLabelText('ubicacion')).toHaveTextContent(
      '/equipos/perfil/baskonia?temporada=2020'
    )
    await user.click(screen.getByRole('button', { name: 'Atrás' }))
    expect(await screen.findByLabelText('ubicacion')).toHaveTextContent('/inicio-prueba')
  })

  test('los cambios del usuario crean historial y atrás/adelante restaura la URL', async () => {
    const user = userEvent.setup()
    render(
      <MemoryRouter initialEntries={['/jugadores/estadisticas?temporada=2026']}>
        <Routes>
          <Route path="*" element={<HistoryHarness />} />
        </Routes>
      </MemoryRouter>
    )

    await user.click(screen.getByRole('button', { name: 'Cambiar filtros' }))
    expect(screen.getByLabelText('ubicacion')).toHaveTextContent(
      '/jugadores/estadisticas?temporada=2025&fase=playoffs&vista=avanzado'
    )

    await user.click(screen.getByRole('button', { name: 'Atrás' }))
    await waitFor(() => expect(screen.getByLabelText('ubicacion')).toHaveTextContent(
      '/jugadores/estadisticas?temporada=2026'
    ))

    await user.click(screen.getByRole('button', { name: 'Adelante' }))
    await waitFor(() => expect(screen.getByLabelText('ubicacion')).toHaveTextContent(
      '/jugadores/estadisticas?temporada=2025&fase=playoffs&vista=avanzado'
    ))
  })

  test('un equipo inexistente en un enlace explícito no se sustituye por otro', () => {
    render(
      <MemoryRouter initialEntries={['/perfil-equipo/2020/equipo-inexistente']}>
        <Routes>
          <Route
            path="*"
            element={<LegacyResolver context={{ teamIdentities }} />}
          />
        </Routes>
      </MemoryRouter>
    )

    expect(screen.getByLabelText('resolucion')).toHaveTextContent('not-found')
  })

  test('una ronda compartida se conserva mientras cargan sus partidos', async () => {
    render(
      <MemoryRouter initialEntries={['/partidos?temporada=2026&ronda=regular%3A1']}>
        <Routes>
          <Route path="/partidos" element={
            <>
              <GameFlow
                teams={[{ season: 2026, team: 'Real Madrid', teamId: 'real-madrid' }]}
                playerRecords={[]}
                loadGameFlowForSeason={vi.fn()}
                gameFlowCache={{}}
                loadingGameFlow={{ 2026: true }}
              />
              <LocationProbe label="partidos" />
            </>
          } />
        </Routes>
      </MemoryRouter>
    )

    await waitFor(() => expect(screen.getByLabelText('ubicacion')).toHaveTextContent(
      '/partidos?temporada=2026&ronda=regular%3A1'
    ))
  })

  test('un filtro de equipo se conserva mientras cargan sus tiros', async () => {
    render(
      <MemoryRouter initialEntries={['/tiro/lideres?temporada=2026&equipo=real-madrid']}>
        <Routes>
          <Route path="/tiro/lideres" element={
            <>
              <ZoneLeaders
                teams={[{ season: 2026, team: 'Real Madrid', teamId: 'real-madrid' }]}
                players={[]}
                playerPhotos={{}}
                loadShotsForSeason={vi.fn()}
                shotsCache={{}}
                loadingShots={{ 2026: true }}
              />
              <LocationProbe label="tiros" />
            </>
          } />
        </Routes>
      </MemoryRouter>
    )

    await waitFor(() => expect(screen.getByLabelText('ubicacion')).toHaveTextContent(
      '/tiro/lideres?temporada=2026&equipo=real-madrid'
    ))
  })
})

const seasonPlayers = [
  { licenseId: '1', season: 2026, teamId: 'real-madrid', team: 'Real Madrid', playerFull: 'Jugador Actual', games: 20, mpg: 20 },
  { licenseId: '1', season: 2027, teamId: 'real-madrid', team: 'Real Madrid', playerFull: 'Jugador Actual', games: 1, mpg: 20 },
  { licenseId: '2', season: 2026, teamId: 'baskonia', team: 'Baskonia', playerFull: 'Jugador Histórico', games: 20, mpg: 20 },
]

describe('latest-season defaults', () => {
  test.each([
    ['profile', '/jugadores/perfil', PlayerProfile, 'profile-season-filter'],
    ['similarity', '/jugadores/similitud', PlayerSimilarity, 'similarity-season-filter'],
  ])('%s defaults its search to the latest season and honors historical selections', async (_name, path, Page, seasonId) => {
    const user = userEvent.setup()
    render(
      <MemoryRouter initialEntries={[path]}>
        <Routes>
          <Route path={`${path}/:player?`} element={
            <>
              <Page players={seasonPlayers} similarity={[]} loadOnOffForSeason={vi.fn()} onOffCache={{}} loadingOnOff={{}} />
              <LocationProbe label="selector" />
            </>
          } />
        </Routes>
      </MemoryRouter>
    )

    const season = document.getElementById(seasonId)
    expect(season).toHaveValue('2027')
    await user.click(screen.getByRole('combobox', { name: 'Jugador' }))
    expect(screen.queryByRole('button', { name: /Jugador Histórico/ })).not.toBeInTheDocument()
    await user.selectOptions(season, '2026')
    await user.click(screen.getByRole('combobox', { name: 'Jugador' }))
    await user.click(screen.getByRole('button', { name: /Jugador Actual/ }))
    await waitFor(() => expect(screen.getByLabelText('ubicacion')).toHaveTextContent('temporada=2026'))
    expect(season).toHaveValue('2026')

    await user.selectOptions(season, '')
    expect(season).toHaveValue('')
    await user.click(screen.getByRole('combobox', { name: 'Jugador' }))
    expect(screen.getByRole('button', { name: /Jugador Histórico/ })).toBeVisible()
  })

  test.each([
    ['', '2027', '1', true],
    ['?temporada=2027&min-partidos=3', '2027', '3', false],
    ['?temporada=2026', '2026', '3', false],
    ['?temporada=2026&min-partidos=1', '2026', '1', true],
  ])('clutch defaults remain useful early in the season and retain explicit filters: %s', async (search, season, minimum, showsPlayer) => {
    const row = {
      licenseId: '1', teamId: 'real-madrid', team: 'Real Madrid', nick: 'Jugador Actual',
      games: 1, fg2M: 1, fg2A: 1, fg3M: 0, fg3A: 0, ftM: 0, ftA: 0,
    }
    const loadSeason = vi.fn()
    render(
      <MemoryRouter initialEntries={[`/jugadores/clutch${search}`]}>
        <ClutchStats
          teams={[{ season: 2026 }, { season: 2027 }]}
          players={seasonPlayers}
          loadClutchForSeason={loadSeason}
          clutchCache={{ 2026: { players: [row] }, 2027: { players: [row] } }}
          loadingClutch={{}}
        />
      </MemoryRouter>
    )

    await waitFor(() => expect(loadSeason).toHaveBeenCalledWith(Number(season)))
    expect(screen.getByLabelText('Temporada')).toHaveValue(season)
    expect(screen.getByLabelText('Mín. PJ clutch')).toHaveValue(minimum)
    // The name is rendered twice: desktop cell and mobile name line (CSS picks one)
    expect(screen.queryAllByText('Jugador Actual').length > 0).toBe(showsPlayer)
  })

  test('a latest-season similarity link retains the season while awaiting qualified games', async () => {
    render(
      <MemoryRouter initialEntries={['/jugadores/similitud/jugador-actual--1?temporada=2027&equipo=real-madrid']}>
        <Routes>
          <Route path="/jugadores/similitud/:player" element={
            <>
              <PlayerSimilarity players={seasonPlayers} similarity={[]} />
              <LocationProbe label="similitud" />
            </>
          } />
        </Routes>
      </MemoryRouter>
    )

    expect(await screen.findByText(/No se encontraron datos de similitud para esta temporada/)).toBeVisible()
    expect(screen.getByLabelText('ubicacion')).toHaveTextContent('temporada=2027')
    expect(document.getElementById('similarity-season-filter')).toHaveValue('2027')
  })
})
