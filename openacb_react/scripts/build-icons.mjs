import { readFileSync, writeFileSync } from 'node:fs'
import { Resvg } from '@resvg/resvg-js'

// Builds the header logo and the favicons from public/openacb_nobckg.png
// (1024x1024, 1.4 MB), which is too heavy to serve. Run it by hand after
// changing the logo: node scripts/build-icons.mjs
//
// Each image is halved step by step (1024 -> 512 -> 256 ...) before the final
// size: a single big jump leaves jagged edges and broken text.
// The favicons use only the ball and braces, without the "openACB" text that
// cannot be read at 16-32 px, so the mark fills the square.

const SOURCE_SIZE = 1024
// Ball and braces in the source image (the text starts at y = 626).
const MARK = { x: 228, y: 209, width: 580, height: 361 }

const source = readFileSync(new URL('../public/openacb_nobckg.png', import.meta.url))
const dataUrl = png => `data:image/png;base64,${png.toString('base64')}`
const render = (svg, width) => new Resvg(svg, { fitTo: { mode: 'width', value: width }, font: { loadSystemFonts: false } }).render().asPng()
const svgOf = (size, body) => `<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="${size}" height="${size}" viewBox="0 0 ${size} ${size}">${body}</svg>`

function shrink(png, from, to) {
  let current = from
  let image = png
  while (current / 2 >= to) {
    image = render(svgOf(current, `<image width="${current}" height="${current}" xlink:href="${dataUrl(image)}"/>`), current / 2)
    current /= 2
  }
  return current === to ? image : render(svgOf(current, `<image width="${current}" height="${current}" xlink:href="${dataUrl(image)}"/>`), to)
}

// The mark centred on a transparent square, with a small margin, at 1024 px.
function markSquare() {
  const side = Math.round(MARK.width * 1.08)
  const scale = SOURCE_SIZE / side
  const offsetX = side / 2 - (MARK.x + MARK.width / 2)
  const offsetY = side / 2 - (MARK.y + MARK.height / 2)
  const clip = `<clipPath id="mark"><rect x="${MARK.x - 4}" y="${MARK.y - 4}" width="${MARK.width + 8}" height="${MARK.height + 8}"/></clipPath>`
  const body = `<defs>${clip}</defs><g transform="scale(${scale}) translate(${offsetX} ${offsetY})">`
    + `<image width="${SOURCE_SIZE}" height="${SOURCE_SIZE}" clip-path="url(#mark)" xlink:href="${dataUrl(source)}"/></g>`
  return render(svgOf(SOURCE_SIZE, body), SOURCE_SIZE)
}

const mark = markSquare()
const outputs = [
  // Header: the full logo, shown at 56 CSS px (up to ~3x screens).
  ['logo-160.png', source, 160],
  // iOS home screen: the full logo, as before.
  ['apple-touch-icon.png', source, 180],
  ['favicon-96.png', mark, 96],
  ['favicon-32.png', mark, 32],
  ['favicon-16.png', mark, 16],
]
for (const [name, image, size] of outputs) {
  const png = shrink(image, SOURCE_SIZE, size)
  writeFileSync(new URL(`../public/${name}`, import.meta.url), png)
  console.log(`${name}: ${size} px, ${(png.length / 1024).toFixed(1)} KB`)
}
