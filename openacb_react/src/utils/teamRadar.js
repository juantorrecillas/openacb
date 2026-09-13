export const offensiveAxes = [
  { key: 'ortg',      label: 'Rating Ofensivo',      inverted: false, format: v => v.toFixed(1) },
  { key: 'pace',      label: 'Ritmo',                inverted: false, format: v => v.toFixed(1) },
  { key: 'threeRate', label: 'Dependencia 3P',       inverted: false, format: v => `${(v * 100).toFixed(1)}%` },
  { key: 'threePct',  label: 'Eficiencia 3P',        inverted: false, format: v => `${(v * 100).toFixed(1)}%` },
  { key: 'ts',        label: 'Eficiencia de Tiro',   inverted: false, format: v => `${(v * 100).toFixed(1)}%` },
  { key: 'astRate',   label: 'Ratio de Asistencias', inverted: false, format: v => `${(v * 100).toFixed(1)}%` },
  { key: 'orbPct',    label: 'Rebote Ofensivo',      inverted: false, format: v => `${(v * 100).toFixed(1)}%` },
  { key: 'tovRate',   label: 'Cuidado de Balón',     inverted: true,  format: v => `${(v * 100).toFixed(1)}%` },
]

export const defensiveAxes = [
  { key: 'drtg',         label: 'Rating Defensivo',      inverted: true,  format: v => v.toFixed(1) },
  { key: 'opp_threePct', label: 'Riv. Eficiencia 3P',    inverted: true,  format: v => `${(v * 100).toFixed(1)}%` },
  { key: 'opp_ts',       label: 'Riv. Eficiencia Tiro',  inverted: true,  format: v => `${(v * 100).toFixed(1)}%` },
  { key: 'opp_tovRate',  label: 'Pérdidas Forzadas',     inverted: false, format: v => `${(v * 100).toFixed(1)}%` },
  { key: 'stlRate',      label: 'Ratio de Robos',        inverted: false, format: v => `${(v * 100).toFixed(1)}%` },
  { key: 'blkRate',      label: 'Ratio de Tapones',      inverted: false, format: v => `${(v * 100).toFixed(1)}%` },
  { key: 'drbPct',       label: 'Rebote Defensivo',      inverted: false, format: v => `${(v * 100).toFixed(1)}%` },
]

// Same midpoint-of-ties definition as the player export. Every finite team-season
// observation has equal weight; lower-is-better metrics reverse the rank.
export function midrankPercentile(value, reference, inverted = false) {
  if (!Number.isFinite(value)) return null
  const sample = reference.filter(Number.isFinite)
  if (!sample.length) return null
  const below = sample.filter(item => item < value).length
  const tied = sample.filter(item => item === value).length
  const percentile = 100 * (below + tied / 2) / sample.length
  return inverted ? 100 - percentile : percentile
}

export function getTeamRadarAxes(team, reference, axes = offensiveAxes) {
  return axes.map(axis => ({
    key: axis.key,
    label: axis.label,
    value: midrankPercentile(team[axis.key], reference.map(row => row[axis.key]), axis.inverted),
  }))
}
