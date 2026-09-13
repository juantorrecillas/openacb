import { Activity, BarChart3, Fingerprint, Percent } from 'lucide-react'
import { Link } from 'react-router-dom'
import {
  buildFourFactorsPath,
  buildGamesPath,
  buildTeamProfilePath,
  buildTeamStatsPath,
} from '../routing'
import styles from './TeamToolShell.module.css'

const TOOLS = [
  {
    id: 'stats',
    label: 'Estadísticas',
    path: buildTeamStatsPath(),
    icon: BarChart3,
  },
  {
    id: 'profile',
    label: 'Perfil de equipo',
    path: buildTeamProfilePath(),
    icon: Fingerprint,
  },
  {
    id: 'games',
    label: 'Análisis de partido',
    path: buildGamesPath(),
    icon: Activity,
  },
  {
    id: 'factors',
    label: 'Four Factors',
    path: buildFourFactorsPath(),
    icon: Percent,
  },
]

export default function TeamToolShell({ activeTool, title, scope, actions, children }) {
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

      <nav className={styles.toolNav} aria-label="Herramientas de equipo">
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

      <div className={styles.workspace} data-team-tool={activeTool}>{children}</div>
    </div>
  )
}

export { styles as teamToolStyles }
