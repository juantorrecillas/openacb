import { useMemo, useState } from 'react'
import { Link } from 'react-router-dom'
import { buildPlayerProfilePath } from '../routing/paths'
import { serializeRouteQuery, withQuery } from '../routing/query'
import { classifyArchetype } from '../utils/playerArchetypes'
import { getPlayerDisplayName } from '../utils/playerNames'
import {
  formatPlayerProfileTableValue,
  playerProfileAdvancedStatColumns,
  playerProfileBasicStatColumns,
} from '../utils/playerProfileTableColumns'

const positionOrder = new Map([
  'Base',
  'Escolta',
  'Alero',
  'Ala-pívot',
  'Pívot',
].map((position, index) => [position, index]))

function playerProfileUrl(player) {
  return withQuery(
    buildPlayerProfilePath(player, getPlayerDisplayName(player)),
    serializeRouteQuery('playerProfile', {
      temporada: player.season,
      equipo: player.teamId,
      fase: 'regular',
    }, { strict: false })
  )
}

function cleanString(value) {
  return typeof value === 'string' && value.trim() ? value.trim() : null
}

export default function TeamRosterTable({ players = [], playerBio = {} }) {
  const [tab, setTab] = useState('basic')
  const columns = tab === 'basic'
    ? playerProfileBasicStatColumns
    : playerProfileAdvancedStatColumns

  const roster = useMemo(() => {
    return players
      .map(player => {
        const bio = playerBio[String(player.licenseId)] || {}
        return {
          ...player,
          rosterName: getPlayerDisplayName(player),
          rosterPosition: cleanString(player.position) || cleanString(bio.position) || '-',
          rosterArchetype: classifyArchetype(player, bio),
          rosterProfileUrl: playerProfileUrl(player),
        }
      })
      .sort((a, b) => (
        (positionOrder.get(a.rosterPosition) ?? positionOrder.size)
        - (positionOrder.get(b.rosterPosition) ?? positionOrder.size)
        || a.rosterName.localeCompare(b.rosterName, 'es')
      ))
  }, [playerBio, players])

  return (
    <section className="overflow-hidden rounded-lg border border-acb-200 bg-white" aria-labelledby="team-roster-title">
      <div className="flex flex-wrap items-center gap-3 border-b border-acb-200 px-4 py-3">
        <div>
          <h3 id="team-roster-title" className="font-semibold text-acb-900">Plantilla</h3>
          <p className="text-xs text-acb-500">
            {roster.length} jugadores · estadísticas de la temporada completa
          </p>
        </div>
        <div className="ml-auto flex items-center gap-1 rounded-md bg-acb-100 p-0.5" aria-label="Tipo de estadísticas de la plantilla">
          <button
            type="button"
            onClick={() => setTab('basic')}
            aria-pressed={tab === 'basic'}
            className={`rounded px-3 py-1 text-xs font-medium transition-colors ${
              tab === 'basic' ? 'bg-white text-acb-900 shadow-sm' : 'text-acb-600 hover:text-acb-900'
            }`}
          >
            Básico
          </button>
          <button
            type="button"
            onClick={() => setTab('advanced')}
            aria-pressed={tab === 'advanced'}
            className={`rounded px-3 py-1 text-xs font-medium transition-colors ${
              tab === 'advanced' ? 'bg-white text-acb-900 shadow-sm' : 'text-acb-600 hover:text-acb-900'
            }`}
          >
            Avanzado
          </button>
        </div>
      </div>

      {roster.length === 0 ? (
        <p className="px-4 py-8 text-center text-sm text-acb-400">
          No hay jugadores disponibles para este equipo y temporada.
        </p>
      ) : (
        <div className="overflow-x-auto" tabIndex={0} aria-label="Estadísticas de la plantilla">
          <table className="data-table min-w-full">
            <thead>
              <tr className="border-b border-acb-200 bg-acb-50">
                <th className="data-table-head data-table-identity data-table-sticky-head team-roster-sticky-player team-roster-col-player bg-acb-50">
                  Jugador
                </th>
                <th className="data-table-head text-left data-table-sticky-head team-roster-sticky-position team-roster-col-position bg-acb-50">
                  Posición
                </th>
                <th className="data-table-head text-left data-table-sticky-head team-roster-sticky-archetype team-roster-col-archetype bg-acb-50">
                  Rol
                </th>
                {columns.map(column => (
                  <th
                    key={column.key}
                    className={`data-table-head data-table-number ${column.integer ? 'data-col-games' : 'data-col-number'}`}
                  >
                    {column.label}
                  </th>
                ))}
              </tr>
            </thead>
            <tbody className="divide-y divide-acb-100">
              {roster.map(player => (
                <tr key={`${player.licenseId}-${player.teamId}`} className="data-table-row">
                  <td className="data-table-cell data-table-identity team-roster-sticky-player team-roster-col-player">
                    <Link
                      to={player.rosterProfileUrl}
                      className="block truncate hover:text-accent-700 hover:underline"
                      title={player.rosterName}
                    >
                      {player.rosterName}
                    </Link>
                  </td>
                  <td className="data-table-cell text-left team-roster-sticky-position team-roster-col-position">
                    <span className="block truncate" title={player.rosterPosition}>{player.rosterPosition}</span>
                  </td>
                  <td className="data-table-cell text-left team-roster-sticky-archetype team-roster-col-archetype">
                    <span
                      className={`inline-flex max-w-full truncate rounded border px-2 py-0.5 text-[11px] font-medium ${player.rosterArchetype.color}`}
                      title={`${player.rosterArchetype.name}: ${player.rosterArchetype.desc}`}
                    >
                      {player.rosterArchetype.name}
                    </span>
                  </td>
                  {columns.map(column => (
                    <td
                      key={column.key}
                      className={`data-table-cell data-table-number ${column.integer ? 'data-col-games' : 'data-col-number'}`}
                    >
                      {formatPlayerProfileTableValue(player, column)}
                    </td>
                  ))}
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      )}
    </section>
  )
}
