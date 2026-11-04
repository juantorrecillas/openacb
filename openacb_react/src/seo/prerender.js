import { readDocumentMetadata } from './metadata.js'

// Entity pages (players, teams) are prerendered by scripts/build-seo.mjs with a
// readable summary inside #root. Read it once, before React replaces #root, so
// the app can keep showing it while the tool's data loads and keep its metadata.
let page = null

export function capturePrerenderedPage() {
  if (typeof document === 'undefined') return null
  const node = document.querySelector('#root [data-prerendered-path]')
  if (!node) return null
  page = {
    path: node.getAttribute('data-prerendered-path'),
    html: node.querySelector('.prerender-content')?.innerHTML || '',
    metadata: readDocumentMetadata(),
  }
  return page
}

export function getPrerenderedPage() {
  return page
}
