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

// Rank 1 of `total` is the 100th percentile and the last rank the 0th
export function rankToPercentile(rank, total) {
  if (!rank || !Number.isFinite(Number(rank)) || !(total > 1)) return null
  return ((total - rank) / (total - 1)) * 100
}

// Every rank badge (#1, #2...) uses the same colours as percentile badges
export function getRankBadgeClass(rank, total) {
  return getPercentileBadgeClass(rankToPercentile(rank, total))
}

export function getPercentileBarClass(percentile) {
  if (percentile == null || !Number.isFinite(Number(percentile))) return 'bg-acb-200'

  const value = Number(percentile)
  if (value >= 75) return 'bg-accent-300'
  if (value >= 50) return 'bg-accent-200'
  if (value >= 25) return 'bg-info-200'
  return 'bg-info-300'
}
