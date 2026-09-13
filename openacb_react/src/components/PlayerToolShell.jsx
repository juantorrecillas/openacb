import { BarChart3, Flame, GitCompareArrows, ScanSearch, UserRound } from 'lucide-react'
import { Link } from 'react-router-dom'
import {
  buildPlayerClutchPath,
  buildPlayerComparisonPath,
  buildPlayerProfilePath,
  buildPlayerSimilarityPath,
  buildPlayerStatsPath,
} from '../routing'
import styles from './PlayerToolShell.module.css'

const TOOLS = [
  {
    id: 'stats',
    label: 'Estadísticas',
    path: buildPlayerStatsPath(),
    icon: BarChart3,
  },
  {
    id: 'profile',
    label: 'Perfil',
    path: buildPlayerProfilePath(),
    icon: UserRound,
  },
  {
    id: 'similarity',
    label: 'Similitud',
    path: buildPlayerSimilarityPath(),
    icon: ScanSearch,
  },
  {
    id: 'comparison',
    label: 'Comparar',
    path: buildPlayerComparisonPath(),
    icon: GitCompareArrows,
  },
  {
    id: 'clutch',
    label: 'Clutch',
    path: buildPlayerClutchPath(),
    icon: Flame,
  },
]

export default function PlayerToolShell({ activeTool, title, scope, actions, children }) {
  return (
    <div className={`app-page ${styles.page}`}>
      <header className={styles.masthead}>
        <h1 className={styles.title}>{title}</h1>
        {(scope || actions) && (
          <div className={styles.introMeta}>
            {scope && <span>{scope}</span>}
            {actions && <div className={styles.headerActions}>{actions}</div>}
          </div>
        )}
      </header>

      <nav className={styles.toolNav} aria-label="Herramientas de jugador">
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
              <Icon size={17} strokeWidth={1.8} aria-hidden="true" />
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

export { styles as playerToolStyles }
