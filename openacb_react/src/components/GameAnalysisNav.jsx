import { NavLink } from 'react-router-dom'
import {
  buildGamesPath,
  buildTeamClutchPath,
  buildTeamQuartersPath,
  serializeRouteQuery,
  withQuery,
} from '../routing'

const ITEMS = [
  { path: buildGamesPath(), routeId: 'games', label: 'Flujo de partido' },
  { path: buildTeamQuartersPath(), routeId: 'teamQuarters', label: 'Rendimiento por cuarto' },
  { path: buildTeamClutchPath(), routeId: 'teamClutch', label: 'Clutch' },
]

export default function GameAnalysisNav({ season }) {
  return (
    <nav className="flex flex-wrap gap-x-5 border-b border-acb-300" aria-label="Análisis de partido">
      {ITEMS.map(item => (
        <NavLink
          key={item.path}
          to={withQuery(item.path, serializeRouteQuery(item.routeId, { temporada: season }, { strict: false }))}
          end={item.path === buildGamesPath()}
          className={({ isActive }) => `border-b-2 px-0 py-2 text-sm font-semibold transition-colors ${
            isActive
              ? 'border-accent-500 text-acb-900'
              : 'border-transparent text-acb-500 hover:border-acb-300 hover:text-acb-900'
          }`}
        >
          {item.label}
        </NavLink>
      ))}
    </nav>
  )
}
