// @vitest-environment jsdom
import '@testing-library/jest-dom/vitest'
import { cleanup, render, screen, within } from '@testing-library/react'
import userEvent from '@testing-library/user-event'
import { afterEach, expect, test } from 'vitest'
import { MemoryRouter, useLocation } from 'react-router-dom'
import SiteHeader from './SiteHeader'

const items = [
  { id: 'equipos', label: 'Equipos', tabs: [{ id: 'teams', label: 'Estadísticas de Equipo' }, { id: 'fingerprint', label: 'Perfil de Equipo' }] },
  { id: 'about', label: 'Proyecto', single: true },
]
const paths = { teams: '/equipos/estadisticas', fingerprint: '/equipos/perfil', about: '/info' }
function Harness() {
  const location = useLocation()
  return <><SiteHeader items={items} paths={paths} activeTab="home" /><output aria-label="ubicación">{location.pathname}</output><button>Fuera del menú</button></>
}
afterEach(cleanup)

test('desktop disclosure toggles, closes on Escape and outside clicks, and navigates', async () => {
  const user = userEvent.setup()
  render(<MemoryRouter><Harness /></MemoryRouter>)
  const navigation = within(screen.getByRole('navigation', { name: 'Navegación principal' }))
  const trigger = navigation.getByRole('button', { name: 'Equipos' })
  await user.click(trigger)
  expect(trigger).toHaveAttribute('aria-expanded', 'true')
  await user.tab()
  await user.keyboard('{Escape}')
  expect(trigger).toHaveFocus()
  expect(trigger).toHaveAttribute('aria-expanded', 'false')
  await user.click(trigger)
  await user.click(screen.getByRole('button', { name: 'Fuera del menú' }))
  expect(trigger).toHaveAttribute('aria-expanded', 'false')
  await user.click(trigger)
  await user.click(navigation.getByRole('link', { name: 'Perfil de Equipo' }))
  expect(screen.getByRole('status', { name: 'ubicación' })).toHaveTextContent('/equipos/perfil')
  expect(trigger).toHaveAttribute('aria-expanded', 'false')
})

test('mobile categories disclose their links and Escape returns focus to the menu button', async () => {
  const user = userEvent.setup()
  render(<MemoryRouter><Harness /></MemoryRouter>)
  await user.click(screen.getByRole('button', { name: 'Abrir menú de navegación' }))
  const mobile = within(screen.getByRole('navigation', { name: 'Navegación móvil' }))
  await user.click(mobile.getByRole('button', { name: 'Equipos' }))
  expect(mobile.getByRole('link', { name: 'Estadísticas de Equipo' })).toBeVisible()
  await user.keyboard('{Escape}')
  expect(screen.queryByRole('navigation', { name: 'Navegación móvil' })).not.toBeInTheDocument()
  expect(screen.getByRole('button', { name: 'Abrir menú de navegación' })).toHaveFocus()
})
