import { ArrowDown, ArrowUp } from 'lucide-react'

// A sortable column's header text with its arrow. The arrow sits in the
// cell's right padding (see index.css), so sorting never changes the column
// width. `direction` is 'asc', 'desc', or empty when the column isn't sorted.
export default function SortLabel({ children, direction }) {
  const Icon = direction === 'asc' ? ArrowUp : ArrowDown
  return (
    <span className="data-table-sort-label">
      {children}
      {direction && <Icon className="data-table-sort-icon" aria-hidden="true" />}
    </span>
  )
}
