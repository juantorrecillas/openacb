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
  expect(screen.getByRole('link', { name: 'Ver perfil de equipo' })).toHaveAttribute('href', '/equipos/perfil/real-madrid?temporada=2025')
  await user.click(screen.getByRole('button', { name: 'Ejemplo siguiente' }))
  expect(screen.getByRole('heading', { name: 'Perfil de jugador' })).toBeVisible()
  expect(screen.getByRole('link', { name: 'Ver perfil de jugador' })).toHaveAttribute('href', '/jugadores/perfil/facundo-campazzo-avedano?temporada=2025&fase=regular')
  await user.click(screen.getByRole('button', { name: 'Ejemplo siguiente' }))
  expect(screen.getByRole('link', { name: 'Ver mapa por zonas' })).toHaveAttribute('href', '/tiro/cartas/equipo/real-madrid?temporada=2025&vista=zonas&zonas=frecuencia')
  expect(screen.getByText('Diferencia respecto a la liga, en puntos porcentuales')).toBeVisible()
  await user.click(screen.getByRole('button', { name: 'Ejemplo siguiente' }))
  expect(screen.getByRole('heading', { name: 'Perfil de equipo' })).toBeVisible()
  await user.click(screen.getByRole('button', { name: 'Ejemplo anterior' }))
  expect(screen.getByRole('heading', { name: 'Frecuencia de tiro por zonas' })).toBeVisible()
})

test('a keyboard user can select an example without leaving the carousel controls', async () => {
  const user = userEvent.setup()
  render(<MemoryRouter><HomeToolPreview /></MemoryRouter>)
  const choices = within(screen.getByRole('group', { name: 'Herramienta del ejemplo' }))
  await user.tab()
  await user.tab()
  expect(choices.getByRole('button', { name: 'Jugadores' })).toHaveFocus()
  await user.keyboard('{Enter}')
  expect(choices.getByRole('button', { name: 'Jugadores' })).toHaveAttribute('aria-pressed', 'true')
  expect(choices.getByRole('button', { name: 'Jugadores' })).toHaveFocus()
})
