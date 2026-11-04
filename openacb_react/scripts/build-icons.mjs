import { readFileSync, writeFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { Resvg } from '@resvg/resvg-js'

// Builds the PNG favicons from public/favicon.svg, and the square logo
// (public/openacb.png) from public/logo.svg. Run it by hand after changing
// either: node scripts/build-icons.mjs
// The header uses public/logo.svg directly, so it needs no PNG.
// The ball has no navy in it, so one set of PNGs works in light and dark tabs.

const favicon = readFileSync(new URL('../public/favicon.svg', import.meta.url), 'utf8')
const logo = readFileSync(new URL('../public/logo.svg', import.meta.url), 'utf8')
const font = { fontFiles: [fileURLToPath(new URL('../public/fonts/manrope/manrope-700.ttf', import.meta.url))], loadSystemFonts: false, defaultFontFamily: 'Manrope' }
const render = (svg, size) => new Resvg(svg, { fitTo: { mode: 'width', value: size }, font }).render().asPng()

// iOS fills transparent pixels with black, so the home-screen icon gets a
// navy tile, with the ball shrunk to leave a margin around it.
const homeScreen = favicon.replace(/(<svg[^>]*>)([\s\S]*)<\/svg>/,
  '$1<rect width="64" height="64" fill="#102a43"/><g transform="translate(9 9) scale(.72)">$2</g></svg>')

// Mark over the wordmark, on the share cards' paper colour. build-seo.mjs
// uses it as the link-preview image if the preview cards fail to render.
const square = `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1024 1024" width="1024" height="1024">
  <rect width="1024" height="1024" fill="#fffaf5"/>
  ${logo.replace(/<svg[^>]*>/, '<svg x="232" y="220" width="560" height="407" viewBox="0 0 88 64">')}
  <text x="512" y="801" text-anchor="middle" font-family="Manrope" font-weight="700" font-size="164" letter-spacing="-5.74" fill="#102a43">open<tspan fill="#fe5917">ACB</tspan></text>
</svg>`

const outputs = [
  ['openacb.png', square, 1024],
  ['apple-touch-icon.png', homeScreen, 180],
  ['favicon-96.png', favicon, 96],
  ['favicon-32.png', favicon, 32],
  ['favicon-16.png', favicon, 16],
]
for (const [name, svg, size] of outputs) {
  const png = render(svg, size)
  writeFileSync(new URL(`../public/${name}`, import.meta.url), png)
  console.log(`${name}: ${size} px, ${(png.length / 1024).toFixed(1)} KB`)
}
