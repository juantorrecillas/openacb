import { canonicalizePathname, matchCanonicalRoute, matchLegacyRoute } from '../routing/manifest.js'

export const SITE_URL = 'https://openacb.com'
export const SITE_IMAGE = `${SITE_URL}/openacb.png`
export const SITE_SCHEMA = {
  '@context': 'https://schema.org',
  '@type': 'WebSite',
  name: 'openACB',
  alternateName: 'OpenACB',
  url: `${SITE_URL}/`,
  inLanguage: 'es-ES',
}

export function getPageMetadata(pathname, { notFound = false } = {}) {
  const path = canonicalizePathname(pathname).toLocaleLowerCase('es')
  const canonicalMatch = matchCanonicalRoute(path)
  const route = canonicalMatch?.route || matchLegacyRoute(path)?.route
  const isMissing = notFound || !route

  return {
    title: isMissing ? 'Página no encontrada | openACB'
      : route.id === 'home' ? route.title : `${route.title} | openACB`,
    description: isMissing ? 'La dirección no corresponde a una página disponible de OpenACB.'
      : route.description,
    // UI filters share the main tool URL; entity paths remain distinct.
    // The SPA fallback must never claim that an entity page is the homepage.
    canonical: !isMissing && canonicalMatch ? `${SITE_URL}${path}` : null,
    robots: isMissing ? 'noindex, follow' : 'index, follow, max-image-preview:large',
    isHome: !isMissing && route.id === 'home',
  }
}

export function updatePageMetadata(metadata) {
  document.title = metadata.title

  const values = [
    ['name', 'description', metadata.description],
    ['name', 'robots', metadata.robots],
    ['property', 'og:type', 'website'],
    ['property', 'og:site_name', 'openACB'],
    ['property', 'og:locale', 'es_ES'],
    ['property', 'og:title', metadata.title],
    ['property', 'og:description', metadata.description],
    ['property', 'og:url', metadata.canonical],
    ['property', 'og:image', SITE_IMAGE],
    ['property', 'og:image:alt', 'Logo de openACB'],
    ['name', 'twitter:card', 'summary'],
    ['name', 'twitter:title', metadata.title],
    ['name', 'twitter:description', metadata.description],
    ['name', 'twitter:image', SITE_IMAGE],
  ]

  values.forEach(([attribute, key, content]) => {
    let element = document.head.querySelector(`meta[${attribute}="${key}"]`)
    if (content == null) {
      element?.remove()
      return
    }
    if (!element) {
      element = document.createElement('meta')
      element.setAttribute(attribute, key)
      document.head.appendChild(element)
    }
    element.content = content
  })

  let canonical = document.head.querySelector('link[rel="canonical"]')
  if (metadata.canonical) {
    if (!canonical) {
      canonical = document.createElement('link')
      canonical.rel = 'canonical'
      document.head.appendChild(canonical)
    }
    canonical.href = metadata.canonical
  } else {
    canonical?.remove()
  }

  let schema = document.getElementById('site-schema')
  if (metadata.isHome) {
    if (!schema) {
      schema = document.createElement('script')
      schema.id = 'site-schema'
      schema.type = 'application/ld+json'
      document.head.appendChild(schema)
    }
    schema.textContent = JSON.stringify(SITE_SCHEMA)
  } else {
    schema?.remove()
  }
}
