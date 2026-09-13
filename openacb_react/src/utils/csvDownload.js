// build a csv string from rows + column descriptors.
// columns: array of { key, label? }. label falls back to key.
export function tableToCsv(rows, columns) {
  const escape = (v) => {
    if (v == null) return ''
    const s = String(v)
    return /[",\n;]/.test(s) ? `"${s.replace(/"/g, '""')}"` : s
  }
  const header = columns.map(c => escape(c.label ?? c.key)).join(',')
  const body = rows.map(r => columns.map(c => escape(r[c.key])).join(',')).join('\n')
  return `${header}\n${body}`
}

// trigger a browser download of the given csv string.
// prepends a utf-8 bom so excel opens the file with the right encoding.
export function downloadCsv(filename, csv) {
  const blob = new Blob(['﻿', csv], { type: 'text/csv;charset=utf-8;' })
  const url = URL.createObjectURL(blob)
  const a = document.createElement('a')
  a.href = url
  a.download = filename
  document.body.appendChild(a)
  a.click()
  document.body.removeChild(a)
  URL.revokeObjectURL(url)
}

export function downloadTableAsCsv(filename, rows, columns) {
  downloadCsv(filename, tableToCsv(rows, columns))
}
