// Chart for the On/Off image export: one row per player with the team's net
// rating difference with him on and off the court (the table's Impacto column)
// as a bar either side of zero. Returns a panel for utils/chartImage.js.
const WIDTH = 1088
const HEADER = 40
const ROW = 46
const NAME_WIDTH = 300
const MINUTES_RIGHT = 390
const VALUE_WIDTH = 92
const BAR_LEFT = 420
const BAR_RIGHT = WIDTH - VALUE_WIDTH - 16

const INTER = 'Inter, sans-serif'
const MONO = "'JetBrains Mono', Consolas, monospace"

const escapeXml = value => String(value).replace(/[&<>"']/g, character => ({
  '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&apos;',
})[character])

// Same thresholds as the table: within ±2 points reads as neutral.
function tone(value) {
  if (value > 2) return '#2aa867'
  if (value < -2) return '#dd415d'
  return '#9fb3c8'
}

const signed = value => `${value > 0 ? '+' : ''}${value.toFixed(1)}`

export function buildOnOffPanel(players, { maxRows = 16 } = {}) {
  const rows = players
    .filter(player => Number.isFinite(player.netDiff))
    .sort((a, b) => b.netDiff - a.netDiff)
    .slice(0, maxRows)
  const middle = (BAR_LEFT + BAR_RIGHT) / 2
  const reach = Math.max(10, ...rows.map(player => Math.abs(player.netDiff)))
  const scale = (BAR_RIGHT - middle - 6) / reach
  const height = HEADER + rows.length * ROW + 8

  const label = (x, value, anchor = 'start') => `<text x="${x}" y="24" font-family="${INTER}" font-size="13" font-weight="600" fill="#627d98" letter-spacing="1.3"${anchor === 'start' ? '' : ` text-anchor="${anchor}"`}>${value}</text>`
  const parts = [
    label(0, 'JUGADOR'),
    label(MINUTES_RIGHT, 'MIN', 'end'),
    label(BAR_LEFT, 'PEOR CON ÉL'),
    label(BAR_RIGHT, 'MEJOR CON ÉL', 'end'),
    label(WIDTH, 'NETO', 'end'),
    `<line x1="0" y1="${HEADER - 6}" x2="${WIDTH}" y2="${HEADER - 6}" stroke="#bcccdc"/>`,
  ]

  rows.forEach((player, index) => {
    const top = HEADER + index * ROW
    const center = top + ROW / 2
    const color = tone(player.netDiff)
    const length = Math.abs(player.netDiff) * scale
    const x = player.netDiff >= 0 ? middle : middle - length
    const name = String(player.name || '').length > 28 ? `${String(player.name).slice(0, 27)}…` : player.name
    if (index % 2 === 1) parts.push(`<rect x="0" y="${top}" width="${WIDTH}" height="${ROW}" fill="#f8fafc"/>`)
    parts.push(`<text x="0" y="${center + 7}" font-family="${INTER}" font-size="20" font-weight="500" fill="#102a43">${escapeXml(name)}</text>`)
    parts.push(`<text x="${MINUTES_RIGHT}" y="${center + 6}" font-family="${MONO}" font-size="17" fill="#627d98" text-anchor="end">${Math.round(player.onMin ?? 0)}</text>`)
    parts.push(`<rect x="${x.toFixed(1)}" y="${center - 11}" width="${Math.max(length, 1.5).toFixed(1)}" height="22" fill="${color}"/>`)
    parts.push(`<text x="${WIDTH}" y="${center + 7}" font-family="${MONO}" font-size="20" font-weight="600" fill="${color === '#9fb3c8' ? '#486581' : color}" text-anchor="end">${signed(player.netDiff)}</text>`)
  })
  parts.push(`<line x1="${middle}" y1="${HEADER - 6}" x2="${middle}" y2="${height - 4}" stroke="#829ab1" stroke-width="1.5"/>`)

  return {
    viewBox: [0, 0, WIDTH, height],
    markup: parts.join(''),
    maxHeight: height,
  }
}
