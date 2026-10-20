import { mkdirSync, readFileSync, writeFileSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'
import { ROUTE_MANIFEST } from '../src/routing/manifest.js'
import { getPageMetadata, SITE_IMAGE, SITE_SCHEMA, SITE_URL } from '../src/seo/metadata.js'

const output = fileURLToPath(new URL('../dist/', import.meta.url))
const template = readFileSync(resolve(output, 'index.html'), 'utf8')
const seoBlock = /<!-- seo:start -->[\s\S]*?<!-- seo:end -->/
if (!seoBlock.test(template)) throw new Error('Missing SEO markers in built index.html')

const escapeHtml = value => String(value).replace(/[&<>"']/g, character => ({
  '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;',
})[character])

function renderHead(metadata) {
  const tags = [
    `<title>${escapeHtml(metadata.title)}</title>`,
    `<meta name="description" content="${escapeHtml(metadata.description)}" />`,
    `<meta name="robots" content="${metadata.robots}" />`,
    '<meta property="og:type" content="website" />',
    '<meta property="og:site_name" content="openACB" />',
    '<meta property="og:locale" content="es_ES" />',
    `<meta property="og:title" content="${escapeHtml(metadata.title)}" />`,
    `<meta property="og:description" content="${escapeHtml(metadata.description)}" />`,
    `<meta property="og:image" content="${SITE_IMAGE}" />`,
    '<meta property="og:image:alt" content="Logo de openACB" />',
    '<meta name="twitter:card" content="summary" />',
    `<meta name="twitter:title" content="${escapeHtml(metadata.title)}" />`,
    `<meta name="twitter:description" content="${escapeHtml(metadata.description)}" />`,
    `<meta name="twitter:image" content="${SITE_IMAGE}" />`,
  ]
  if (metadata.canonical) {
    tags.push(`<link rel="canonical" href="${escapeHtml(metadata.canonical)}" />`)
    tags.push(`<meta property="og:url" content="${escapeHtml(metadata.canonical)}" />`)
  }
  if (metadata.isHome) {
    tags.push(`<script id="site-schema" type="application/ld+json">${JSON.stringify(SITE_SCHEMA)}</script>`)
  }
  return `<!-- seo:start -->\n    ${tags.join('\n    ')}\n    <!-- seo:end -->`
}

// Deep entity links and old URLs still load React. Leave the canonical unset in
// this shell so it cannot misidentify those pages as the homepage before rendering.
const homeMetadata = getPageMetadata('/')
writeFileSync(resolve(output, 'app.html'), template.replace(seoBlock, renderHead({
  ...homeMetadata, canonical: null, isHome: false,
})))

const paths = ROUTE_MANIFEST.flatMap(route => route.patterns.filter(path => !path.includes(':')))
for (const path of paths) {
  const file = resolve(output, path === '/' ? 'index.html' : `${path.slice(1)}.html`)
  mkdirSync(dirname(file), { recursive: true })
  writeFileSync(file, template.replace(seoBlock, renderHead(getPageMetadata(path))))
}

writeFileSync(resolve(output, 'sitemap.xml'), `<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
${paths.map(path => `  <url><loc>${SITE_URL}${path}</loc></url>`).join('\n')}
</urlset>
`)
writeFileSync(resolve(output, 'robots.txt'), `User-agent: *
Allow: /

Sitemap: ${SITE_URL}/sitemap.xml
`)

console.log(`SEO: generated metadata for ${paths.length} pages, robots.txt and sitemap.xml`)
