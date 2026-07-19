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
    <nav className="flex gap-2 flex-wrap" aria-label="Análisis de partido">
      {ITEMS.map(item => (
        <NavLink
          key={item.path}
          to={withQuery(item.path, serializeRouteQuery(item.routeId, { temporada: season }, { strict: false }))}
          end={item.path === buildGamesPath()}
          className={({ isActive }) => `px-4 py-1.5 rounded-full text-sm font-medium ${
            isActive
              ? 'bg-acb-900 text-white'
              : 'border border-acb-200 text-acb-500 hover:bg-acb-50'
          }`}
        >
          {item.label}
        </NavLink>
      ))}
    </nav>
  )
}
