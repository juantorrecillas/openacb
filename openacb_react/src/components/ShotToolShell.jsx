import { Map, Trophy } from 'lucide-react'
import { Link } from 'react-router-dom'
import { buildShotChartsPath, buildZoneLeadersPath } from '../routing'
import baseStyles from './PlayerToolShell.module.css'
import styles from './ShotToolShell.module.css'

const TOOLS = [
  {
    id: 'charts',
    label: 'Cartas de tiro',
    path: buildShotChartsPath(),
    icon: Map,
  },
  {
    id: 'leaders',
    label: 'Líderes por zona',
    path: buildZoneLeadersPath(),
    icon: Trophy,
  },
]

export default function ShotToolShell({ activeTool, title, scope, actions, children }) {
  return (
    <div className={`app-page ${baseStyles.page}`}>
      <header className={baseStyles.masthead}>
        <h1 className={baseStyles.title}>{title}</h1>
        {(scope || actions) && (
          <div className={baseStyles.introMeta}>
            {scope && <span>{scope}</span>}
            {actions && <div className={baseStyles.headerActions}>{actions}</div>}
          </div>
        )}
      </header>

      <nav className={`${baseStyles.toolNav} ${styles.toolNav}`} aria-label="Herramientas de tiro">
        {TOOLS.map(tool => {
          const Icon = tool.icon
          const isActive = activeTool === tool.id
          return (
            <Link
              key={tool.id}
              to={tool.path}
              className={`${baseStyles.toolLink} ${isActive ? baseStyles.toolLinkActive : ''}`}
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

      <div className={baseStyles.workspace}>{children}</div>
    </div>
  )
}

export const shotToolStyles = { ...baseStyles, ...styles }
