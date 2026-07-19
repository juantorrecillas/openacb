// @vitest-environment jsdom
import '@testing-library/jest-dom/vitest'
import { cleanup, render, screen, within } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { afterEach, describe, expect, test } from 'vitest'
import { MemoryRouter } from 'react-router-dom'
import TeamRosterTable from './TeamRosterTable'
import {
  playerProfileAdvancedStatColumns,
  playerProfileBasicStatColumns,
} from '../utils/playerProfileTableColumns'

afterEach(cleanup)

const players = [
  {
    licenseId: 2,
    playerFull: 'Jugador Interior',
    season: 2026,
    teamId: 'equipo-prueba',
    team: 'Equipo Prueba',
    position: 'Pívot',
    qualified: false,
    games: 4,
    mpg: 5.5,
    ppg: 1.5,
    astToRatio: 0.5,
  },
  {
    licenseId: 1,
    playerFull: 'Jugador Principal',
    season: 2026,
    teamId: 'equipo-prueba',
    team: 'Equipo Prueba',
    position: 'Base',
    qualified: false,
    games: 30,
    mpg: 24.5,
    ppg: 12.3,
    astToRatio: 2.5,
  },
]

describe('tabla de plantilla', () => {
  test('ordena primero por posición y después por nombre', () => {
    const unorderedPlayers = [
      { ...players[0], licenseId: 10, playerFull: 'Pívot Alfa', position: 'Pívot' },
      { ...players[0], licenseId: 11, playerFull: 'Base Zeta', position: 'Base' },
      { ...players[0], licenseId: 12, playerFull: 'Alero Alfa', position: 'Alero' },
      { ...players[0], licenseId: 13, playerFull: 'Base Alfa', position: 'Base' },
      { ...players[0], licenseId: 14, playerFull: 'Escolta Alfa', position: 'Escolta' },
      { ...players[0], licenseId: 15, playerFull: 'Sin Posición', position: '' },
    ]

    render(
      <MemoryRouter>
        <TeamRosterTable players={unorderedPlayers} />
      </MemoryRouter>
    )

    expect(screen.getAllByRole('link').map(link => link.textContent)).toEqual([
      'Base Alfa',
      'Base Zeta',
      'Escolta Alfa',
      'Alero Alfa',
      'Pívot Alfa',
      'Sin Posición',
    ])
  })

  test('mantiene jugador, posición y arquetipo antes de las estadísticas del perfil', () => {
    render(
      <MemoryRouter>
        <TeamRosterTable players={players} />
      </MemoryRouter>
    )

    const headers = screen.getAllByRole('columnheader')
    expect(headers.slice(0, 3).map(header => header.textContent.trim())).toEqual([
      'Jugador',
      'Posición',
      'Arquetipo',
    ])
    expect(headers.slice(3).map(header => header.textContent.trim())).toEqual(
      playerProfileBasicStatColumns.map(column => column.label)
    )
    expect(headers[0]).toHaveClass('team-roster-sticky-player')
    expect(headers[1]).toHaveClass('team-roster-sticky-position')
    expect(headers[2]).toHaveClass('team-roster-sticky-archetype')

    const firstPlayerRow = screen.getByRole('link', { name: 'Jugador Principal' }).closest('tr')
    expect(within(firstPlayerRow).getByText('Base')).toBeInTheDocument()
    expect(within(firstPlayerRow).getByText('Datos insuficientes')).toBeInTheDocument()
    expect(within(firstPlayerRow).getByText('24.5')).toBeInTheDocument()
  })

  test('cambia a las mismas columnas avanzadas que el perfil de jugador', async () => {
    const user = userEvent.setup()
    render(
      <MemoryRouter>
        <TeamRosterTable players={players} />
      </MemoryRouter>
    )

    await user.click(screen.getByRole('button', { name: 'Avanzado' }))

    const headers = screen.getAllByRole('columnheader')
    expect(headers.slice(3).map(header => header.textContent.trim())).toEqual(
      playerProfileAdvancedStatColumns.map(column => column.label)
    )
    const firstPlayerRow = screen.getByRole('link', { name: 'Jugador Principal' }).closest('tr')
    expect(within(firstPlayerRow).getByText('2.50')).toBeInTheDocument()
    expect(screen.getByRole('button', { name: 'Avanzado' })).toHaveAttribute('aria-pressed', 'true')
  })
})
