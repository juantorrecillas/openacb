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
