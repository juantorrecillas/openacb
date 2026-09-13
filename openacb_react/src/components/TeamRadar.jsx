import styles from './TeamRadar.module.css'

function wrapLabel(label) {
  return label.split(' ').reduce((lines, word) => {
    const last = lines.length - 1
    if (last >= 0 && `${lines[last]} ${word}`.length <= 12) lines[last] += ` ${word}`
    else lines.push(word)
    return lines
  }, [])
}

export default function TeamRadar({ axes, name, color = '#d04313', dark = false, showValues = true }) {
  const center = [260, 202]
  const radius = 124
  const point = (index, value) => {
    const angle = index * 2 * Math.PI / axes.length - Math.PI / 2
    return [center[0] + Math.cos(angle) * radius * value / 100, center[1] + Math.sin(angle) * radius * value / 100]
  }
  const polygon = values => values.map((value, index) => point(index, value).join(',')).join(' ')
  const complete = axes.every(axis => Number.isFinite(axis.value))

  return (
    <svg viewBox="0 0 520 404" className={`${styles.radar} ${dark ? styles.dark : ''}`} style={{ '--radar-color': color }} role="img" aria-label={`Radar de ${name}`}>
      <desc>{axes.map(axis => `${axis.label}: ${Number.isFinite(axis.value) ? Math.round(axis.value) : 'sin dato'}`).join('; ')}. Percentiles de 0 a 100. Línea discontinua: percentil 50.</desc>
      {[25, 50, 75, 100].map(level => (
        <polygon key={level} points={polygon(axes.map(() => level))} className={level === 50 ? styles.median : styles.grid} />
      ))}
      {axes.map((axis, index) => {
        const [x, y] = point(index, 100)
        return <line key={axis.key} x1={center[0]} y1={center[1]} x2={x} y2={y} className={styles.grid} />
      })}
      {complete && <polygon points={polygon(axes.map(axis => axis.value))} className={styles.area} />}
      {axes.map((axis, index) => {
        const [labelX, y] = point(index, 123)
        const x = Math.max(120, Math.min(400, labelX))
        const anchor = x < center[0] - 15 ? 'end' : x > center[0] + 15 ? 'start' : 'middle'
        const lines = wrapLabel(axis.label)
        const top = y - (lines.length - 1) * 10 - (showValues ? 8 : 0)
        const known = Number.isFinite(axis.value)
        const [px, py] = point(index, known ? axis.value : 0)
        return (
          <g key={axis.key}>
            {known && <circle cx={px} cy={py} r="4" className={styles.point}><title>{axis.label}: {axis.value.toFixed(1)}</title></circle>}
            <text x={x} y={top} textAnchor={anchor} className={styles.label}>
              {lines.map((line, lineIndex) => <tspan key={line} x={x} dy={lineIndex ? 20 : 0}>{line}</tspan>)}
              {showValues && <tspan x={x} dy="21" className={styles.value}>{known ? Math.round(axis.value) : '—'}</tspan>}
            </text>
          </g>
        )
      })}
      <text x={center[0] + 7} y={center[1] - radius / 2 + 4} className={styles.scale}>50</text>
    </svg>
  )
}
