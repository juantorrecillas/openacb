// @vitest-environment jsdom
import { afterEach, expect, test } from 'vitest'
import { ROUTE_MANIFEST } from '../routing/manifest.js'
import { getPageMetadata, readDocumentMetadata, SITE_IMAGE, SITE_SCHEMA, updatePageMetadata } from './metadata.js'

afterEach(() => { document.head.innerHTML = '' })

test('every tool has its own descriptive title and description', () => {
  const titles = new Set()
  const descriptions = new Set()
  for (const route of ROUTE_MANIFEST) {
    const metadata = getPageMetadata(route.patterns[0])
    expect(metadata.title).toContain('ACB')
    expect(metadata.description.length).toBeGreaterThan(60)
    expect(metadata.robots).not.toContain('noindex')
    titles.add(metadata.title)
    descriptions.add(metadata.description)
  }
  expect(titles.size).toBe(ROUTE_MANIFEST.length)
  expect(descriptions.size).toBe(ROUTE_MANIFEST.length)
})

test('navigation keeps one description and canonical, including when returning home', () => {
  document.head.innerHTML = '<meta name="description" content="old"><link rel="canonical" href="https://openacb.com/">'
  for (const path of ['/', '/jugadores/estadisticas', '/tiro/cartas', '/']) {
    const metadata = getPageMetadata(path)
    updatePageMetadata(metadata)
    expect(document.title).toBe(metadata.title)
    expect(document.head.querySelectorAll('meta[name="description"]')).toHaveLength(1)
    expect(document.head.querySelector('meta[name="description"]').content).toBe(metadata.description)
    expect(document.head.querySelectorAll('link[rel="canonical"]')).toHaveLength(1)
    expect(document.head.querySelector('link[rel="canonical"]').href).toBe(metadata.canonical)
    expect(document.head.querySelector('meta[property="og:url"]').content).toBe(metadata.canonical)
    expect(document.head.querySelector('meta[name="twitter:description"]').content).toBe(metadata.description)
  }
  expect(JSON.parse(document.getElementById('site-schema').textContent)).toEqual(SITE_SCHEMA)
})

test('link previews use the large card with its size', () => {
  updatePageMetadata(getPageMetadata('/tiro/cartas'))
  const meta = (attribute, key) => document.head.querySelector(`meta[${attribute}="${key}"]`)?.content
  expect(meta('name', 'twitter:card')).toBe('summary_large_image')
  expect(meta('property', 'og:image')).toBe(SITE_IMAGE)
  expect(meta('property', 'og:image:width')).toBe('1200')
  expect(meta('property', 'og:image:height')).toBe('630')

  // A prerendered page's own image survives a round trip through the document.
  updatePageMetadata({ ...getPageMetadata('/jugadores/perfil/facu-campazzo'), image: 'https://openacb.com/og/jugadores/facu-campazzo.png?v=1' })
  expect(readDocumentMetadata().image).toBe('https://openacb.com/og/jugadores/facu-campazzo.png?v=1')
})

test('entity URLs keep their path while tool filters share the primary tool URL', () => {
  expect(getPageMetadata('/equipos/perfil/real-madrid?temporada=2026').canonical)
    .toBe('https://openacb.com/equipos/perfil/real-madrid')
  expect(getPageMetadata('/jugadores/perfil/facundo-campazzo').canonical)
    .toBe('https://openacb.com/jugadores/perfil/facundo-campazzo')
  expect(getPageMetadata('/jugadores/estadisticas/?temporada=2026&vista=avanzado').canonical)
    .toBe('https://openacb.com/jugadores/estadisticas')
  expect(getPageMetadata('/jugadores').canonical).toBeNull()
})

test('missing pages remove canonical and home schema, and noindex clears after navigation', () => {
  updatePageMetadata(getPageMetadata('/'))
  for (const metadata of [
    getPageMetadata('/does-not-exist'),
    getPageMetadata('/jugadores/perfil/desconocido', { notFound: true }),
  ]) {
    updatePageMetadata(metadata)
    expect(document.head.querySelector('meta[name="robots"]').content).toBe('noindex, follow')
    expect(document.head.querySelector('link[rel="canonical"]')).toBeNull()
    expect(document.head.querySelector('meta[property="og:url"]')).toBeNull()
    expect(document.getElementById('site-schema')).toBeNull()
  }
  updatePageMetadata(getPageMetadata('/equipos/estadisticas'))
  expect(document.head.querySelector('meta[name="robots"]').content).not.toContain('noindex')
})
