// @vitest-environment jsdom
import '@testing-library/jest-dom/vitest'
import { cleanup, render, screen, within } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { afterEach, expect, test } from 'vitest'
import { MemoryRouter } from 'react-router-dom'
import HomeToolPreview from './HomeToolPreview'

afterEach(cleanup)

test('the carousel cycles through the three tools and preserves each example destination', async () => {
  const user = userEvent.setup()
  render(<MemoryRouter><HomeToolPreview /></MemoryRouter>)
  expect(screen.getByRole('link', { name: 'Abrir perfil de equipo' })).toHaveAttribute('href', '/equipos/perfil/unicaja?temporada=2026')
  await user.click(screen.getByRole('button', { name: 'Jugador' }))
  expect(screen.getByRole('heading', { name: /Marcelinho Huertas/ })).toBeVisible()
  expect(screen.getByText('Motor Ofensivo')).toBeVisible()
  expect(screen.getByRole('link', { name: 'Abrir perfil de jugador' })).toHaveAttribute('href', '/jugadores/perfil/marcelinho-huertas?temporada=2024')
  await user.click(screen.getByRole('button', { name: 'Alineaciones' }))
  expect(screen.getByRole('link', { name: 'Abrir ranking de alineaciones' })).toHaveAttribute('href', '/alineaciones/rankings?temporada=2026&equipo=murcia&categoria=trios')
  await user.click(screen.getByRole('button', { name: 'Equipo' }))
  expect(screen.getByRole('heading', { name: /Unicaja/ })).toBeVisible()
})

test('a keyboard user can select an example without leaving the carousel controls', async () => {
  const user = userEvent.setup()
  render(<MemoryRouter><HomeToolPreview /></MemoryRouter>)
  const choices = within(screen.getByRole('group', { name: 'Herramienta del ejemplo' }))
  await user.tab()
  await user.tab()
  expect(choices.getByRole('button', { name: 'Jugador' })).toHaveFocus()
  await user.keyboard('{Enter}')
  expect(choices.getByRole('button', { name: 'Jugador' })).toHaveAttribute('aria-pressed', 'true')
  expect(choices.getByRole('button', { name: 'Jugador' })).toHaveFocus()
})
