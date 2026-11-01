// Builds one React key per row and guarantees they are unique.
// If the data has duplicated rows (same base key), React would misplace them
// when the table is re-sorted. Repeats get a "#2", "#3"... suffix instead,
// and a warning is logged in development so the data issue is still visible.
export function uniqueRowKeys(rows, getBaseKey) {
  const seen = new Map()
  const duplicates = []

  const keys = rows.map(row => {
    const base = String(getBaseKey(row))
    const count = (seen.get(base) || 0) + 1
    seen.set(base, count)
    if (count === 1) return base
    duplicates.push(base)
    return `${base}#${count}`
  })

  if (duplicates.length > 0 && import.meta.env?.DEV) {
    console.warn('Filas duplicadas en la tabla (revisar los datos):', [...new Set(duplicates)])
  }

  return keys
}
