// Chart for the team map image export: every team at its two stats, drawn at a
// fixed size so the picture looks the same from a phone or a desktop. Returns
// a panel for utils/chartImage.js.
//
// Logos live on static.acb.com, which sends no CORS headers, so they are read
// through the site's own /acb-static proxy (vercel.json, vite.config.js) and
// embedded: a picture of an SVG cannot load outside images.
const WIDTH = 1088
const HEIGHT = 720
const LEFT = 92
const RIGHT = WIDTH - 14
const TOP = 14
const BOTTOM = HEIGHT - 70

const INTER = 'Inter, sans-serif'
const MONO = "'JetBrains Mono', Consolas, monospace"
const LABEL_SIZE = 15
const LABEL_HEIGHT = 18

const escapeXml = value => String(value).replace(/[&<>"']/g, character => ({
  '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&apos;',
})[character])

const round = value => Math.round(value * 10) / 10

// ── Logos ─────────────────────────────────────────────────────

const LOGO_PIXELS = 128
const logoCache = new Map()

function proxiedUrl(url) {
  const clean = String(url || '').trim()
  return clean.replace(/^https?:\/\/static\.acb\.com\//, '/acb-static/')
}

const LOGO_TIMEOUT_MS = 6000

const loadImage = src => new Promise((resolve, reject) => {
  const image = new Image()
  image.onload = () => resolve(image)
  image.onerror = () => reject(new Error('Logo ilegible'))
  image.src = src
})

async function fetchLogo(url) {
  const response = await fetch(proxiedUrl(url))
  if (!response.ok) throw new Error(`Logo no disponible: ${url}`)
  const objectUrl = URL.createObjectURL(await response.blob())
  try {
    const image = await loadImage(objectUrl)
    // Shrunk to a small PNG so the image stays light whatever the source size.
    const scale = Math.min(LOGO_PIXELS / image.naturalWidth, LOGO_PIXELS / image.naturalHeight, 1)
    const width = Math.max(1, Math.round(image.naturalWidth * scale))
    const height = Math.max(1, Math.round(image.naturalHeight * scale))
    const canvas = document.createElement('canvas')
    canvas.width = width
    canvas.height = height
    canvas.getContext('2d').drawImage(image, 0, 0, width, height)
    return canvas.toDataURL('image/png')
  } finally {
    URL.revokeObjectURL(objectUrl)
  }
}

// Resolves to { [url]: dataUrl }; a logo that fails is left out (the team is
// drawn as a dot with its name instead).
export async function loadTeamLogos(urls) {
  const unique = [...new Set(urls.filter(Boolean))]
  const entries = await Promise.all(unique.map(async url => {
    if (!logoCache.has(url)) {
      // A slow logo must not hold up the image: it becomes a dot after a while.
      const timeout = new Promise((_, reject) => setTimeout(() => reject(new Error('Logo lento')), LOGO_TIMEOUT_MS))
      logoCache.set(url, Promise.race([fetchLogo(url), timeout]).catch(() => {
        logoCache.delete(url)
        return null
      }))
    }
    return [url, await logoCache.get(url)]
  }))
  return Object.fromEntries(entries.filter(([, dataUrl]) => dataUrl))
}

// ── Labels ────────────────────────────────────────────────────

let measureContext = null
function labelWidth(value) {
  measureContext ||= document.createElement('canvas').getContext('2d')
  measureContext.font = `600 ${LABEL_SIZE}px ${INTER}`
  return measureContext.measureText(String(value)).width
}

const overlap = (a, b) => Math.max(0, Math.min(a.x + a.w, b.x + b.w) - Math.max(a.x, b.x))
  * Math.max(0, Math.min(a.y + a.h, b.y + b.h) - Math.max(a.y, b.y))

function outside(box) {
  const dx = Math.max(0, LEFT - box.x) + Math.max(0, box.x + box.w - RIGHT)
  const dy = Math.max(0, TOP - box.y) + Math.max(0, box.y + box.h - BOTTOM)
  return (dx + dy) * LABEL_HEIGHT * 4
}

// Tries the spots around each mark and keeps the first that touches nothing
// (or the least crowded one).
function placeLabels(points) {
  const marks = points.map(point => ({ x: point.px - point.half, y: point.py - point.half, w: point.half * 2, h: point.half * 2 }))
  const placed = []
  const gap = 5
  points.forEach((point, index) => {
    if (!point.showLabel) return
    const half = point.half
    const w = labelWidth(point.label)
    const h = LABEL_HEIGHT
    const candidates = [
      { x: point.px + half + gap, y: point.py - h / 2, anchor: 'start' },
      { x: point.px - half - gap - w, y: point.py - h / 2, anchor: 'end' },
      { x: point.px - w / 2, y: point.py - half - gap - h, anchor: 'middle' },
      { x: point.px - w / 2, y: point.py + half + gap, anchor: 'middle' },
      { x: point.px + half * 0.7, y: point.py - half - h, anchor: 'start' },
      { x: point.px + half * 0.7, y: point.py + half, anchor: 'start' },
      { x: point.px - half * 0.7 - w, y: point.py - half - h, anchor: 'end' },
      { x: point.px - half * 0.7 - w, y: point.py + half, anchor: 'end' },
    ]
    let best = null
    for (const candidate of candidates) {
      const box = { x: candidate.x, y: candidate.y, w, h }
      let score = outside(box)
      marks.forEach((mark, markIndex) => { if (markIndex !== index) score += overlap(box, mark) })
      placed.forEach(other => { score += overlap(box, other.box) * 2 })
      if (!best || score < best.score) best = { score, box, anchor: candidate.anchor }
      if (score === 0) break
    }
    placed.push({ point, ...best })
  })
  return placed
}

function labelMarkup({ point, box, anchor }) {
  const x = anchor === 'start' ? box.x : anchor === 'end' ? box.x + box.w : box.x + box.w / 2
  const y = box.y + box.h - 4
  return `<text x="${round(x)}" y="${round(y)}" font-family="${INTER}" font-size="${LABEL_SIZE}" font-weight="600" fill="#243b53" stroke="#ffffff" stroke-width="4" stroke-linejoin="round" paint-order="stroke"${anchor === 'start' ? '' : ` text-anchor="${anchor}"`}>${escapeXml(point.label)}</text>`
}

// ── Panel ─────────────────────────────────────────────────────

/**
 * rows: [{ x, y, label, logo, color }], logos: { [url]: dataUrl }
 * xAxis / yAxis: { title, domain: [min, max], ticks: [{ value, label }] }
 * average: { x, y }, showLabels: names next to every team (as on screen)
 */
export function buildTeamScatterPanel({ rows, logos = {}, xAxis, yAxis, average, showLabels }) {
  const valid = rows.filter(row => Number.isFinite(row.x) && Number.isFinite(row.y))
  const [x0, x1] = xAxis.domain
  const [y0, y1] = yAxis.domain
  const scaleX = value => LEFT + ((value - x0) / ((x1 - x0) || 1)) * (RIGHT - LEFT)
  const scaleY = value => BOTTOM - ((value - y0) / ((y1 - y0) || 1)) * (BOTTOM - TOP)
  const size = valid.length > 40 ? 32 : valid.length > 24 ? 44 : 60
  const dotRadius = Math.round(size * 0.22)

  const parts = []
  xAxis.ticks.forEach(tick => {
    const x = round(scaleX(tick.value))
    parts.push(`<line x1="${x}" y1="${TOP}" x2="${x}" y2="${BOTTOM}" stroke="#e2e8f0" stroke-dasharray="4 4"/>`)
    parts.push(`<text x="${x}" y="${BOTTOM + 28}" font-family="${MONO}" font-size="16" fill="#627d98" text-anchor="middle">${escapeXml(tick.label)}</text>`)
  })
  yAxis.ticks.forEach(tick => {
    const y = round(scaleY(tick.value))
    parts.push(`<line x1="${LEFT}" y1="${y}" x2="${RIGHT}" y2="${y}" stroke="#e2e8f0" stroke-dasharray="4 4"/>`)
    parts.push(`<text x="${LEFT - 12}" y="${y + 5}" font-family="${MONO}" font-size="16" fill="#627d98" text-anchor="end">${escapeXml(tick.label)}</text>`)
  })
  parts.push(`<line x1="${LEFT}" y1="${BOTTOM}" x2="${RIGHT}" y2="${BOTTOM}" stroke="#9fb3c8"/>`)
  parts.push(`<line x1="${LEFT}" y1="${TOP}" x2="${LEFT}" y2="${BOTTOM}" stroke="#9fb3c8"/>`)
  parts.push(`<text x="${round((LEFT + RIGHT) / 2)}" y="${HEIGHT - 8}" font-family="${INTER}" font-size="18" font-weight="600" fill="#486581" text-anchor="middle">${escapeXml(xAxis.title)}</text>`)
  const yTitleY = round((TOP + BOTTOM) / 2)
  parts.push(`<text x="20" y="${yTitleY}" transform="rotate(-90 20 ${yTitleY})" font-family="${INTER}" font-size="18" font-weight="600" fill="#486581" text-anchor="middle">${escapeXml(yAxis.title)}</text>`)

  if (Number.isFinite(average?.x)) {
    const x = round(scaleX(average.x))
    parts.push(`<line x1="${x}" y1="${TOP}" x2="${x}" y2="${BOTTOM}" stroke="#94a3b8" stroke-width="1.5" stroke-dasharray="7 6"/>`)
  }
  if (Number.isFinite(average?.y)) {
    const y = round(scaleY(average.y))
    parts.push(`<line x1="${LEFT}" y1="${y}" x2="${RIGHT}" y2="${y}" stroke="#94a3b8" stroke-width="1.5" stroke-dasharray="7 6"/>`)
  }

  const points = valid.map(row => {
    const logo = logos[row.logo]
    return {
      ...row,
      px: scaleX(row.x),
      py: scaleY(row.y),
      logoData: logo,
      half: logo ? size / 2 : dotRadius + 2,
      // A team without its logo always gets its name.
      showLabel: showLabels || !logo,
    }
  })

  points.forEach(point => {
    if (point.logoData) {
      parts.push(`<image href="${point.logoData}" x="${round(point.px - point.half)}" y="${round(point.py - point.half)}" width="${size}" height="${size}" preserveAspectRatio="xMidYMid meet"/>`)
    } else {
      parts.push(`<circle cx="${round(point.px)}" cy="${round(point.py)}" r="${dotRadius}" fill="${point.color || '#486581'}" stroke="#ffffff" stroke-width="2"/>`)
    }
  })
  placeLabels(points).forEach(label => parts.push(labelMarkup(label)))

  return {
    viewBox: [0, 0, WIDTH, HEIGHT],
    markup: parts.join(''),
    maxHeight: HEIGHT,
  }
}
