export const playerProfileBasicStatColumns = [
  { key: 'games', label: 'PJ', integer: true },
  { key: 'mpg', label: 'MPP' },
  { key: 'ppg', label: 'PPP' },
  { key: 'rpg', label: 'RPP' },
  { key: 'orebpg', label: 'RO' },
  { key: 'drebpg', label: 'RD' },
  { key: 'apg', label: 'APP' },
  { key: 'spg', label: 'RBP' },
  { key: 'bpg', label: 'TPP' },
  { key: 'topg', label: 'PER' },
  { key: 'fpg', label: 'FPP' },
  { key: 'fgPct', label: 'TC%', suffix: '%' },
  { key: 'fg3Pct', label: '3P%', suffix: '%' },
  { key: 'ftPct', label: 'TL%', suffix: '%' },
]

export const playerProfileAdvancedStatColumns = [
  { key: 'games', label: 'PJ', integer: true },
  { key: 'ppg', label: 'PPP' },
  { key: 'ortg', label: 'ORtg' },
  { key: 'usg', label: 'USG%', suffix: '%' },
  { key: 'efg', label: 'eFG%', suffix: '%' },
  { key: 'ts', label: 'TS%', suffix: '%' },
  { key: 'threeRate', label: '3PAr', suffix: '%' },
  { key: 'orbPct', label: 'RO%', suffix: '%' },
  { key: 'drbPct', label: 'RD%', suffix: '%' },
  { key: 'trbPct', label: 'REB%', suffix: '%' },
  { key: 'astPct', label: 'AST%', suffix: '%' },
  { key: 'astToRatio', label: 'AST:TOV', fmtFn: v => v.toFixed(2) },
  { key: 'assistedFgm', label: '% asistidos', fmtFn: v => `${(v * 100).toFixed(1)}%` },
  { key: 'assistedFgm2', label: 'Ast 2P%', fmtFn: v => `${(v * 100).toFixed(1)}%` },
  { key: 'assistedFgm3', label: 'Ast 3P%', fmtFn: v => `${(v * 100).toFixed(1)}%` },
  { key: 'stlPct', label: 'ROB%', suffix: '%' },
  { key: 'blkPct', label: 'TAP%', suffix: '%' },
  { key: 'tovPct', label: 'PER%', suffix: '%' },
]

const formatSigned = v => `${v > 0 ? '+' : ''}${v.toFixed(1)}`

export const playerProfileImpactStatColumns = [
  { key: 'games', label: 'PJ', integer: true },
  { key: 'mpg', label: 'MPP' },
  { key: 'per', label: 'PER' },
  { key: 'gmsc', label: 'GmSc' },
  { key: 'ows', label: 'OWS' },
  { key: 'dws', label: 'DWS' },
  { key: 'ws', label: 'WS' },
  { key: 'ws40', label: 'WS/40', fmtFn: v => v.toFixed(3) },
  { key: 'oImpact', label: 'Imp. of.', fmtFn: formatSigned },
  { key: 'dImpact', label: 'Imp. def.', fmtFn: formatSigned },
  { key: 'impact', label: 'Impacto', fmtFn: formatSigned },
]

export function formatPlayerProfileTableValue(record, column) {
  const value = record[column.key]
  if (value == null) return '-'
  if (column.fmtFn) return column.fmtFn(value)
  if (column.left || column.integer) return value
  return `${Number(value).toFixed(1)}${column.suffix || ''}`
}
