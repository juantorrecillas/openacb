import { readFileSync } from 'node:fs'
import { createRequire } from 'node:module'
import satori from 'satori'
import { renderAsync } from '@resvg/resvg-js'

// Link-preview cards (1200x630 PNG) for the prerendered pages.
// satori lays the card out and turns text into paths; resvg rasterises it.
export const OG_WIDTH = 1200
export const OG_HEIGHT = 630

const require = createRequire(import.meta.url)
const fontFile = (pkg, file) => readFileSync(require.resolve(`${pkg}/files/${file}`))

const INK = '#102a43'
const MUTED = '#627d98'
const COPY = '#486581'
const RULE = '#d9e2ec'
const PAPER = '#fffaf5'
const ORANGE = '#fe5917'

function loadFonts() {
  const inter = (weight, subset) => ({
    name: 'Inter', weight, style: 'normal', data: fontFile('@fontsource/inter', `inter-${subset}-${weight}-normal.woff`),
  })
  return [
    { name: 'Manrope', weight: 700, style: 'normal', data: readFileSync(new URL('../public/fonts/manrope/manrope-700.ttf', import.meta.url)) },
    inter(500, 'latin'), inter(500, 'latin-ext'), inter(600, 'latin'), inter(600, 'latin-ext'),
    { name: 'JetBrains Mono', weight: 600, style: 'normal', data: fontFile('@fontsource/jetbrains-mono', 'jetbrains-mono-latin-600-normal.woff') },
  ]
}

// Minimal element helper: satori takes React-like { type, props } objects.
const box = (style, ...children) => ({
  type: 'div',
  props: { style: { display: 'flex', ...style }, children: children.flat().filter(child => child != null && child !== false) },
})

function titleSize(title) {
  if (title.length <= 18) return 84
  if (title.length <= 24) return 72
  if (title.length <= 32) return 60
  return 52
}

function footer() {
  return box({ height: 92, background: INK, alignItems: 'center', justifyContent: 'space-between', padding: '0 72px' },
    box({ fontFamily: 'Manrope', fontWeight: 700, fontSize: 38, color: '#ffffff', letterSpacing: -1 }, 'open', box({ color: ORANGE }, 'ACB')),
    box({ fontFamily: 'Inter', fontWeight: 500, fontSize: 26, color: '#bcccdc' }, 'openacb.com'))
}

function statBlock({ label, value }, index, count) {
  const last = index === count - 1
  return box({
    flexDirection: 'column',
    paddingRight: last ? 0 : 36,
    marginRight: last ? 0 : 36,
    borderRight: last ? 'none' : `1px solid ${RULE}`,
  },
  box({ fontFamily: 'JetBrains Mono', fontWeight: 600, fontSize: count > 4 ? 50 : 56, color: INK, lineHeight: 1 }, value),
  box({ fontFamily: 'Inter', fontWeight: 600, fontSize: 20, color: MUTED, marginTop: 12, letterSpacing: 2 }, label))
}

function cardElement({ kicker, title, meta, stats = [] }) {
  return box({ width: OG_WIDTH, height: OG_HEIGHT, flexDirection: 'column', background: PAPER },
    box({ flexDirection: 'column', flexGrow: 1, padding: '60px 72px 0' },
      box({ fontFamily: 'Inter', fontWeight: 600, fontSize: 22, color: MUTED, letterSpacing: 3, textTransform: 'uppercase' }, kicker),
      box({ fontFamily: 'Manrope', fontWeight: 700, fontSize: titleSize(title), color: INK, marginTop: 18, lineHeight: 1.04, letterSpacing: -2, maxWidth: OG_WIDTH - 144 }, title),
      meta ? box({ fontFamily: 'Inter', fontWeight: 500, fontSize: 30, color: COPY, marginTop: 18 }, meta) : null,
      stats.length ? box({ marginTop: 'auto', marginBottom: 52 }, stats.map((stat, index) => statBlock(stat, index, stats.length))) : null),
    footer())
}

// The generic card for pages without their own (tools, home, older players).
function siteCardElement() {
  const line = text => box({ fontFamily: 'Manrope', fontWeight: 700, fontSize: 92, color: INK, lineHeight: 1.02, letterSpacing: -3 }, text)
  return box({ width: OG_WIDTH, height: OG_HEIGHT, flexDirection: 'column', background: PAPER },
    box({ flexDirection: 'column', flexGrow: 1, padding: '60px 72px 52px' },
      box({ fontFamily: 'Inter', fontWeight: 600, fontSize: 22, color: MUTED, letterSpacing: 3, textTransform: 'uppercase' }, 'Gratis y en código abierto'),
      box({ flexDirection: 'column', marginTop: 22 }, line('Estadística avanzada'), line('de la Liga Endesa')),
      box({ marginTop: 'auto', paddingTop: 22, borderTop: `1px solid ${RULE}`, fontFamily: 'Inter', fontWeight: 500, fontSize: 29, color: COPY },
        'Cartas de tiro · On/Off · Quintetos · Comparador de jugadores')),
    footer())
}

// System fonts are not needed (satori already turned text into paths), and
// loading them costs ~0.4 s per image.
const RESVG_OPTIONS = { fitTo: { mode: 'width', value: OG_WIDTH }, font: { loadSystemFonts: false } }

export async function createOgRenderer() {
  const fonts = loadFonts()
  // renderAsync rasterises off the main thread, so several cards render at once.
  const render = async element => {
    const svg = await satori(element, { width: OG_WIDTH, height: OG_HEIGHT, fonts })
    return (await renderAsync(svg, RESVG_OPTIONS)).asPng()
  }
  return {
    renderCard: card => render(cardElement(card)),
    renderSiteCard: () => render(siteCardElement()),
  }
}
