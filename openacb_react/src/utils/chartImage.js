// Shareable PNG of a chart: the chart's own SVG layers plus a title, key
// numbers, a legend and the openacb.com footer. The card is always laid out at
// 1200 px wide, so an export from a phone looks the same as one from a desktop.
// On phones that can share files the image goes to the share sheet
// (WhatsApp, X...); elsewhere it downloads.
import interLatin500 from '@fontsource/inter/files/inter-latin-500-normal.woff2?url'
import interLatinExt500 from '@fontsource/inter/files/inter-latin-ext-500-normal.woff2?url'
import interLatin600 from '@fontsource/inter/files/inter-latin-600-normal.woff2?url'
import interLatinExt600 from '@fontsource/inter/files/inter-latin-ext-600-normal.woff2?url'
import monoLatin400 from '@fontsource/jetbrains-mono/files/jetbrains-mono-latin-400-normal.woff2?url'
import monoLatin500 from '@fontsource/jetbrains-mono/files/jetbrains-mono-latin-500-normal.woff2?url'
import monoLatin600 from '@fontsource/jetbrains-mono/files/jetbrains-mono-latin-600-normal.woff2?url'
import { trackEvent } from './analytics.js'
import { canShareImages } from './shareSupport.js'

const WIDTH = 1200
const PAD = 56
const CONTENT = WIDTH - PAD * 2
const FOOTER_HEIGHT = 76
const SCALE = 2

const INK = '#102a43'
const COPY = '#486581'
const MUTED = '#627d98'
const RULE = '#d9e2ec'
const ORANGE = '#fe5917'

const LATIN = 'U+0000-00FF,U+0131,U+0152-0153,U+02BB-02BC,U+02C6,U+02DA,U+02DC,U+0304,U+0308,U+0329,U+2000-206F,U+20AC,U+2122,U+2191,U+2193,U+2212,U+2215,U+FEFF,U+FFFD'
const LATIN_EXT = 'U+0100-02BA,U+02BD-02C5,U+02C7-02CC,U+02CE-02D7,U+02DD-02FF,U+0304,U+0308,U+0329,U+1D00-1DBF,U+1E00-1E9F,U+1EF2-1EFF,U+2020,U+20A0-20AB,U+20AD-20C0,U+2113,U+2C60-2C7F,U+A720-A7FF'

// A picture of an SVG cannot use the page's fonts, so they are embedded.
const FONT_FACES = [
  { family: 'Manrope', weight: 700, url: '/fonts/manrope/manrope-700.ttf', format: 'truetype' },
  { family: 'Inter', weight: 500, url: interLatin500, format: 'woff2', range: LATIN },
  { family: 'Inter', weight: 500, url: interLatinExt500, format: 'woff2', range: LATIN_EXT },
  { family: 'Inter', weight: 600, url: interLatin600, format: 'woff2', range: LATIN },
  { family: 'Inter', weight: 600, url: interLatinExt600, format: 'woff2', range: LATIN_EXT },
  { family: 'JetBrains Mono', weight: 400, url: monoLatin400, format: 'woff2' },
  { family: 'JetBrains Mono', weight: 500, url: monoLatin500, format: 'woff2' },
  { family: 'JetBrains Mono', weight: 600, url: monoLatin600, format: 'woff2' },
]

const FAMILIES = {
  Manrope: "Manrope, Inter, sans-serif",
  Inter: 'Inter, sans-serif',
  Mono: "'JetBrains Mono', Consolas, monospace",
}

// ── Fonts ─────────────────────────────────────────────────────

let fontCss = null

const blobToDataUrl = blob => new Promise((resolve, reject) => {
  const reader = new FileReader()
  reader.onload = () => resolve(reader.result)
  reader.onerror = () => reject(reader.error)
  reader.readAsDataURL(blob)
})

function loadFontCss() {
  fontCss ||= Promise.all(FONT_FACES.map(async face => {
    const response = await fetch(face.url)
    if (!response.ok) throw new Error(`No se pudo cargar la fuente ${face.url}`)
    const dataUrl = await blobToDataUrl(await response.blob())
    const range = face.range ? `unicode-range:${face.range};` : ''
    return `@font-face{font-family:'${face.family}';font-style:normal;font-weight:${face.weight};src:url(${dataUrl}) format('${face.format}');${range}}`
  })).then(rules => rules.join('')).catch(error => {
    fontCss = null
    throw error
  })
  return fontCss
}

// Starts loading the fonts early (on hover or focus), so the click is quick
// enough for the share sheet, which needs a recent tap.
export function prepareChartImage() {
  loadFontCss().catch(() => {})
}

// ── Text ──────────────────────────────────────────────────────

const escapeXml = value => String(value).replace(/[&<>"']/g, character => ({
  '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&apos;',
})[character])

let measureContext = null
function measure(value, size, weight, family) {
  measureContext ||= document.createElement('canvas').getContext('2d')
  measureContext.font = `${weight} ${size}px ${FAMILIES[family]}`
  return measureContext.measureText(String(value)).width
}

function text(x, y, value, { size, weight = 500, family = 'Inter', color = INK, anchor = 'start', spacing = 0 }) {
  const extra = `${anchor === 'start' ? '' : ` text-anchor="${anchor}"`}${spacing ? ` letter-spacing="${spacing}"` : ''}`
  return `<text x="${round(x)}" y="${round(y)}" font-family="${FAMILIES[family]}" font-size="${size}" font-weight="${weight}" fill="${color}"${extra}>${escapeXml(value)}</text>`
}

const round = value => Math.round(value * 10) / 10

function ellipsize(value, maxWidth, size, weight, family = 'Inter') {
  if (measure(value, size, weight, family) <= maxWidth) return value
  let cut = value
  while (cut.length > 1 && measure(`${cut}…`, size, weight, family) > maxWidth) cut = cut.slice(0, -1)
  return `${cut.trimEnd()}…`
}

function fitTitle(title) {
  for (const size of [54, 48, 42]) {
    if (measure(title, size, 700, 'Manrope') <= CONTENT) return { size, lines: [title] }
  }
  const words = title.split(' ')
  const lines = ['']
  words.forEach(word => {
    const candidate = lines.at(-1) ? `${lines.at(-1)} ${word}` : word
    if (measure(candidate, 42, 700, 'Manrope') <= CONTENT || !lines.at(-1)) lines[lines.length - 1] = candidate
    else lines.push(word)
  })
  return { size: 42, lines: lines.slice(0, 2).map(line => ellipsize(line, CONTENT, 42, 700, 'Manrope')) }
}

// ── Chart layers ──────────────────────────────────────────────

const SHAPE_PROPERTIES = [
  'fill', 'fill-opacity', 'stroke', 'stroke-width', 'stroke-opacity', 'stroke-dasharray',
  'stroke-linecap', 'stroke-linejoin', 'opacity', 'filter', 'visibility', 'display', 'paint-order',
]
const TEXT_PROPERTIES = ['font-family', 'font-size', 'font-weight', 'font-style', 'letter-spacing', 'text-anchor', 'dominant-baseline']
// Values that match the SVG defaults are left out to keep the file small.
const DEFAULTS = {
  'fill-opacity': '1', 'stroke-opacity': '1', opacity: '1', stroke: 'none', 'stroke-width': '1px',
  'stroke-dasharray': 'none', 'stroke-linecap': 'butt', 'stroke-linejoin': 'miter', filter: 'none',
  visibility: 'visible', display: 'inline', 'paint-order': 'normal', 'font-style': 'normal',
  'letter-spacing': 'normal', 'text-anchor': 'start', 'dominant-baseline': 'auto',
}
const TEXT_TAGS = new Set(['text', 'tspan', 'textPath'])

// Copies the resolved style of every element (CSS classes, custom properties,
// Tailwind utilities) onto the clone, which is drawn without the page's CSS.
function inlineStyles(source, target) {
  const computed = getComputedStyle(source)
  const properties = TEXT_TAGS.has(source.tagName) ? [...SHAPE_PROPERTIES, ...TEXT_PROPERTIES] : SHAPE_PROPERTIES
  const declarations = []
  properties.forEach(property => {
    const value = computed.getPropertyValue(property)
    if (value && DEFAULTS[property] !== value) declarations.push(`${property}:${value}`)
  })
  target.removeAttribute('class')
  if (declarations.length) target.setAttribute('style', declarations.join(';'))
  else target.removeAttribute('style')
  for (let index = 0; index < source.children.length; index += 1) {
    inlineStyles(source.children[index], target.children[index])
  }
}

function viewBoxOf(svg) {
  const box = svg.viewBox?.baseVal
  if (box && box.width && box.height) return { x: box.x, y: box.y, width: box.width, height: box.height }
  const rect = svg.getBoundingClientRect()
  return { x: 0, y: 0, width: rect.width || 100, height: rect.height || 100 }
}

// Radars draw their labels outside the viewBox on purpose; courts clip shots
// taken from beyond half court. The export keeps whichever the page does.
const overflowsViewBox = svg => getComputedStyle(svg).overflow === 'visible'

function serializeLayer(layer, frame) {
  const clone = layer.cloneNode(true)
  inlineStyles(layer, clone)
  clone.querySelectorAll('title, desc').forEach(node => node.remove())
  ;['class', 'style', 'role', 'aria-label', 'aria-hidden', 'tabindex', 'focusable'].forEach(name => clone.removeAttribute(name))
  const box = viewBoxOf(layer)
  clone.setAttribute('viewBox', `${box.x} ${box.y} ${box.width} ${box.height}`)
  clone.setAttribute('x', frame.x)
  clone.setAttribute('y', frame.y)
  clone.setAttribute('width', frame.width)
  clone.setAttribute('height', frame.height)
  clone.setAttribute('overflow', overflowsViewBox(layer) ? 'visible' : 'hidden')
  return new XMLSerializer().serializeToString(clone)
}

// A panel is either live chart SVGs stacked in the first one's coordinates
// ({ layers }) or ready-made markup ({ markup, viewBox }).
function preparePanel(panel) {
  if (panel.markup) return { viewBox: panel.viewBox, markup: panel.markup }
  const layers = (panel.layers || []).filter(Boolean)
  if (!layers.length) throw new Error('El gráfico no está disponible')
  const frame = viewBoxOf(layers[0])
  let minX = frame.x
  let minY = frame.y
  let maxX = frame.x + frame.width
  let maxY = frame.y + frame.height
  // Labels may sit outside the viewBox (overflow: visible on screen).
  layers.filter(overflowsViewBox).forEach(layer => {
    try {
      const box = layer.getBBox()
      if (box.width && box.height) {
        minX = Math.min(minX, box.x)
        minY = Math.min(minY, box.y)
        maxX = Math.max(maxX, box.x + box.width)
        maxY = Math.max(maxY, box.y + box.height)
      }
    } catch {
      // getBBox fails on hidden SVGs; keep the viewBox
    }
  })
  const pad = 6
  return {
    viewBox: [minX - pad, minY - pad, maxX - minX + pad * 2, maxY - minY + pad * 2],
    markup: layers.map(layer => serializeLayer(layer, frame)).join(''),
  }
}

function panelsMarkup(panels, top) {
  const gap = 40
  const width = (CONTENT - gap * (panels.length - 1)) / panels.length
  const prepared = panels.map(preparePanel)
  const titleHeight = panels.some(panel => panel.title) ? 38 : 0
  const chartHeight = Math.max(...prepared.map((panel, index) => Math.min(
    width * (panel.viewBox[3] / panel.viewBox[2]),
    panels[index].maxHeight || 1150,
  )))
  const markup = prepared.map((panel, index) => {
    const x = PAD + index * (width + gap)
    const title = panels[index].title
      ? text(x, top + 24, panels[index].title, { size: 24, weight: 700, family: 'Manrope' })
      : ''
    return `${title}<svg x="${round(x)}" y="${round(top + titleHeight)}" width="${round(width)}" height="${round(chartHeight)}" viewBox="${panel.viewBox.map(round).join(' ')}" preserveAspectRatio="xMidYMid meet" overflow="visible">${panel.markup}</svg>`
  }).join('')
  return { markup, height: titleHeight + chartHeight }
}

// ── Header, legend and footer ─────────────────────────────────

function statsMarkup(stats, top) {
  let x = PAD
  const parts = []
  stats.forEach((stat, index) => {
    // Labels keep their case (eFG%, TS%) as on the site.
    const label = String(stat.label)
    const width = Math.max(measure(stat.value, 32, 600, 'Mono'), measure(label, 14, 600, 'Inter') + label.length * 1.2)
    parts.push(text(x, top + 30, stat.value, { size: 32, weight: 600, family: 'Mono' }))
    parts.push(text(x, top + 56, label, { size: 14, weight: 600, color: MUTED, spacing: 1.2 }))
    if (index < stats.length - 1) {
      parts.push(`<line x1="${round(x + width + 26)}" y1="${top + 4}" x2="${round(x + width + 26)}" y2="${top + 58}" stroke="${RULE}"/>`)
    }
    x += width + 52
  })
  return { markup: parts.join(''), height: 60 }
}

function legendMark(item, x, y) {
  const color = item.color || INK
  if (item.type === 'dot') return `<circle cx="${x + 8}" cy="${y - 6}" r="7" fill="${color}" fill-opacity="${item.opacity ?? 0.75}"/>`
  if (item.type === 'cross') {
    return `<path d="M${x + 2} ${y - 12}L${x + 14} ${y}M${x + 14} ${y - 12}L${x + 2} ${y}" stroke="${color}" stroke-width="2.5" stroke-opacity="${item.opacity ?? 0.75}"/>`
  }
  if (item.type === 'line') {
    return `<line x1="${x}" y1="${y - 6}" x2="${x + 22}" y2="${y - 6}" stroke="${color}" stroke-width="3"${item.dashed ? ' stroke-dasharray="5 4"' : ''}/>`
  }
  return `<rect x="${x}" y="${y - 14}" width="16" height="16" fill="${color}" fill-opacity="${item.opacity ?? 1}" stroke="rgba(16,42,67,.18)"/>`
}

function legendMarkup(items, top) {
  const parts = []
  let x = PAD
  let y = top + 20
  items.forEach(item => {
    if (item.type === 'scale') {
      const fromWidth = measure(item.from, 18, 500, 'Inter')
      const toWidth = measure(item.to, 18, 500, 'Inter')
      const total = fromWidth + 12 + 150 + 12 + toWidth + 32
      if (x + total > PAD + CONTENT && x > PAD) { x = PAD; y += 32 }
      parts.push(text(x, y, item.from, { size: 18, color: COPY }))
      const barX = x + fromWidth + 12
      const step = 150 / item.colors.length
      item.colors.forEach((color, index) => parts.push(`<rect x="${round(barX + index * step)}" y="${y - 14}" width="${round(step + 0.5)}" height="16" fill="${color}"/>`))
      parts.push(`<rect x="${round(barX)}" y="${y - 14}" width="150" height="16" fill="none" stroke="${RULE}"/>`)
      parts.push(text(barX + 162, y, item.to, { size: 18, color: COPY }))
      x += total
      return
    }
    const markWidth = item.type === 'line' ? 22 : 16
    const total = markWidth + 9 + measure(item.label, 18, 500, 'Inter') + 30
    if (x + total > PAD + CONTENT && x > PAD) { x = PAD; y += 32 }
    parts.push(legendMark(item, x, y))
    parts.push(text(x + markWidth + 9, y, item.label, { size: 18, color: COPY }))
    x += total
  })
  return { markup: parts.join(''), height: y - top + 12 }
}

function footerMarkup(top) {
  return `<rect x="0" y="${top}" width="${WIDTH}" height="${FOOTER_HEIGHT}" fill="${INK}"/>`
    + `<text x="${PAD}" y="${top + 48}" font-family="${FAMILIES.Manrope}" font-size="30" font-weight="700" fill="#ffffff" letter-spacing="-1">open<tspan fill="${ORANGE}">ACB</tspan></text>`
    + text(WIDTH - PAD, top + 46, 'openacb.com', { size: 22, weight: 500, color: '#bcccdc', anchor: 'end' })
}

async function composeChartSvg(spec) {
  const css = await loadFontCss().catch(() => '')
  if (document.fonts?.load) {
    await Promise.all([
      document.fonts.load(`700 54px ${FAMILIES.Manrope}`, spec.title),
      document.fonts.load(`500 22px ${FAMILIES.Inter}`, spec.subtitle || 'a'),
      document.fonts.load(`600 32px ${FAMILIES.Mono}`, '0123456789'),
    ]).catch(() => {})
  }

  const parts = []
  let y = PAD
  if (spec.kicker) {
    parts.push(text(PAD, y + 14, spec.kicker.toUpperCase(), { size: 16, weight: 600, color: MUTED, spacing: 2.2 }))
    y += 14 + 20
  }
  const title = fitTitle(spec.title)
  title.lines.forEach((line, index) => {
    y += index === 0 ? title.size * 0.92 : title.size * 1.1
    parts.push(text(PAD, y, line, { size: title.size, weight: 700, family: 'Manrope', spacing: -1.2 }))
  })
  if (spec.subtitle) {
    y += 38
    parts.push(text(PAD, y, ellipsize(spec.subtitle, CONTENT, 22, 500), { size: 22, color: COPY }))
  }
  if (spec.stats?.length) {
    y += 30
    const stats = statsMarkup(spec.stats, y)
    parts.push(stats.markup)
    y += stats.height
  }
  y += 26
  parts.push(`<line x1="${PAD}" y1="${y}" x2="${WIDTH - PAD}" y2="${y}" stroke="${RULE}"/>`)
  y += 28

  const panels = panelsMarkup(spec.panels, y)
  parts.push(panels.markup)
  y += panels.height + 18

  if (spec.legend?.length) {
    const legend = legendMarkup(spec.legend, y)
    parts.push(legend.markup)
    y += legend.height
  }
  const note = [spec.note, 'Datos: play-by-play de acb.com'].filter(Boolean).join(' · ')
  y += 30
  parts.push(text(PAD, y, ellipsize(note, CONTENT, 16, 500), { size: 16, color: MUTED }))
  y += 30

  parts.push(footerMarkup(y))
  const height = Math.ceil(y + FOOTER_HEIGHT)
  return {
    width: WIDTH,
    height,
    markup: `<svg xmlns="http://www.w3.org/2000/svg" width="${WIDTH}" height="${height}" viewBox="0 0 ${WIDTH} ${height}">`
      + `<style>${css}</style><rect width="${WIDTH}" height="${height}" fill="#ffffff"/>${parts.join('')}</svg>`,
  }
}

// ── Output ────────────────────────────────────────────────────

async function rasterize({ markup, width, height }) {
  const image = new Image()
  image.src = `data:image/svg+xml;charset=utf-8,${encodeURIComponent(markup)}`
  await image.decode()
  // Gives embedded fonts a moment in browsers that report the image ready early.
  await new Promise(resolve => setTimeout(resolve, 30))
  const canvas = document.createElement('canvas')
  canvas.width = width * SCALE
  canvas.height = height * SCALE
  const context = canvas.getContext('2d')
  context.scale(SCALE, SCALE)
  context.drawImage(image, 0, 0, width, height)
  return new Promise((resolve, reject) => {
    canvas.toBlob(blob => (blob ? resolve(blob) : reject(new Error('No se pudo crear la imagen'))), 'image/png')
  })
}

function downloadBlob(blob, filename) {
  const url = URL.createObjectURL(blob)
  const link = document.createElement('a')
  link.href = url
  link.download = filename
  document.body.appendChild(link)
  link.click()
  link.remove()
  setTimeout(() => URL.revokeObjectURL(url), 10000)
}

export function imageFilename(...parts) {
  const slug = parts.filter(Boolean).join(' ')
    .normalize('NFD').replace(/[̀-ͯ]/g, '').toLowerCase()
    .replace(/[^a-z0-9]+/g, '-').replace(/^-+|-+$/g, '')
  return `openacb-${slug || 'grafico'}.png`
}

/**
 * spec: {
 *   chart: analytics name ('carta-tiro', 'radar-equipo'...),
 *   kicker, title, subtitle, stats: [{ label, value }],
 *   panels: [{ title?, layers: [SVGElement] } | { title?, markup, viewBox }],
 *   legend: [{ type: 'dot'|'cross'|'swatch'|'line', color, label } | { type: 'scale', colors, from, to }],
 *   note, filename,
 * }
 */
export async function createChartImage(spec) {
  const blob = await rasterize(await composeChartSvg(spec))
  return {
    blob,
    filename: spec.filename || imageFilename(spec.title, spec.chart),
    chart: spec.chart,
    title: spec.title,
    url: window.location.href,
  }
}

// Shares on phones and downloads elsewhere. Resolves to 'shared', 'downloaded',
// 'cancelled', or 'blocked' when the share sheet wants a fresh tap (the image
// took too long after the first one): call it again from the next click.
export async function deliverChartImage(image) {
  if (canShareImages()) {
    const file = new File([image.blob], image.filename, { type: 'image/png' })
    try {
      await navigator.share({ files: [file], title: image.title, text: `${image.title} · ${image.url}` })
      trackEvent('imagen', { grafico: image.chart, accion: 'compartir' })
      return 'shared'
    } catch (error) {
      if (error?.name === 'AbortError') return 'cancelled'
      if (error?.name === 'NotAllowedError') return 'blocked'
      // Any other failure: download instead.
    }
  }
  downloadBlob(image.blob, image.filename)
  trackEvent('imagen', { grafico: image.chart, accion: 'descarga' })
  return 'downloaded'
}
