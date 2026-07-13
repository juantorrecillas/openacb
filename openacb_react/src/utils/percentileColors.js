export function getPercentileBadgeClass(percentile) {
  if (percentile == null || !Number.isFinite(Number(percentile))) {
    return 'bg-acb-100 text-acb-600'
  }

  const value = Number(percentile)
  if (value >= 75) return 'bg-accent-200 text-accent-900'
  if (value >= 50) return 'bg-accent-100 text-accent-800'
  if (value >= 25) return 'bg-info-100 text-info-800'
  return 'bg-info-200 text-info-900'
}

export function getPercentileBarClass(percentile) {
  if (percentile == null || !Number.isFinite(Number(percentile))) return 'bg-acb-200'

  const value = Number(percentile)
  if (value >= 75) return 'bg-accent-300'
  if (value >= 50) return 'bg-accent-200'
  if (value >= 25) return 'bg-info-200'
  return 'bg-info-300'
}
