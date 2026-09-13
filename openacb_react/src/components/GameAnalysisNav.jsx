import { NavLink } from 'react-router-dom'
import {
  buildGamesPath,
  buildTeamClutchPath,
  buildTeamQuartersPath,
  serializeRouteQuery,
  withQuery,
} from '../routing'
import styles from './TeamToolShell.module.css'

const ITEMS = [
  { path: buildGamesPath(), routeId: 'games', label: 'Flujo de partido' },
  { path: buildTeamQuartersPath(), routeId: 'teamQuarters', label: 'Rendimiento por cuarto' },
  { path: buildTeamClutchPath(), routeId: 'teamClutch', label: 'Clutch' },
]

export default function GameAnalysisNav({ season }) {
  return (
    <nav className={styles.analysisNav} aria-label="Vistas del análisis de partido">
      {ITEMS.map(item => (
        <NavLink
          key={item.path}
          to={withQuery(item.path, serializeRouteQuery(item.routeId, { temporada: season }, { strict: false }))}
          className={({ isActive }) => `${styles.analysisNavLink} ${isActive ? styles.analysisNavLinkActive : ''}`}
        >
          {item.label}
        </NavLink>
      ))}
    </nav>
  )
}
