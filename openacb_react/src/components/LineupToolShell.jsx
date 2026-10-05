import { ListOrdered, UsersRound } from 'lucide-react'
import { Link } from 'react-router-dom'
import { buildLineupAnalysisPath, buildLineupRankingsPath } from '../routing'
import styles from './LineupToolShell.module.css'

const TOOLS = [
  {
    id: 'analysis',
    label: 'Análisis On/Off',
    path: buildLineupAnalysisPath(),
    icon: UsersRound,
  },
  {
    id: 'rankings',
    label: 'Rankings',
    path: buildLineupRankingsPath(),
    icon: ListOrdered,
  },
]

export default function LineupToolShell({ activeTool, title, children }) {
  return (
    <div className={`app-page ${styles.page}`}>
      <header className={styles.masthead}>
        <h1 className={styles.title}>{title}</h1>
      </header>

      <nav className={styles.toolNav} aria-label="Herramientas de alineaciones">
        {TOOLS.map(tool => {
          const Icon = tool.icon
          const isActive = activeTool === tool.id
          return (
            <Link
              key={tool.id}
              to={tool.path}
              className={`${styles.toolLink} ${isActive ? styles.toolLinkActive : ''}`}
              aria-current={isActive ? 'page' : undefined}
            >
              <Icon size={18} strokeWidth={1.8} aria-hidden="true" />
              <span>
                <strong>{tool.label}</strong>
              </span>
            </Link>
          )
        })}
      </nav>

      <div className={styles.workspace}>{children}</div>
    </div>
  )
}

export { styles as lineupToolStyles }
