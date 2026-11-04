import { useMemo, useState } from 'react'
import { Link, useNavigate } from 'react-router-dom'
import MobileNameRow from './MobileNameRow'
import { buildPlayerProfilePath } from '../routing/paths'
import { serializeRouteQuery, withQuery } from '../routing/query'
import { classifyArchetype } from '../utils/playerArchetypes'
import { getPlayerDisplayName } from '../utils/playerNames'
import { uniqueRowKeys } from '../utils/uniqueRowKeys'
import {
  formatPlayerProfileTableValue,
  playerProfileAdvancedStatColumns,
  playerProfileBasicStatColumns,
  playerProfileImpactStatColumns,
} from '../utils/playerProfileTableColumns'

const ROSTER_TABS = [
  { key: 'basic', label: 'Básico', columns: playerProfileBasicStatColumns },
  { key: 'advanced', label: 'Avanzado', columns: playerProfileAdvancedStatColumns },
  { key: 'impact', label: 'Impacto', columns: playerProfileImpactStatColumns },
]

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

function archetypeTextClass(colorClasses) {
  return typeof colorClasses === 'string'
    ? colorClasses.split(/\s+/).find(className => className.startsWith('text-')) || ''
    : ''
}

export default function TeamRosterTable({ players = [], playerBio = {}, className = '' }) {
  const navigate = useNavigate()
  const [tab, setTab] = useState('basic')
  const columns = (ROSTER_TABS.find(option => option.key === tab) || ROSTER_TABS[0]).columns

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
  const rosterRowKeys = uniqueRowKeys(roster, p => `${p.licenseId}-${p.teamId}`)

  return (
    <section className={`overflow-hidden rounded-lg border border-acb-200 bg-white ${className}`} aria-labelledby="team-roster-title">
      <div className="flex flex-wrap items-center gap-3 border-b border-acb-200 px-4 py-3">
        <div>
          <h3 id="team-roster-title" className="font-semibold text-acb-900">Plantilla</h3>
        </div>
        <div className="ml-auto flex items-center gap-1 rounded-md bg-acb-100 p-0.5" aria-label="Tipo de estadísticas de la plantilla">
          {ROSTER_TABS.map(option => (
            <button
              key={option.key}
              type="button"
              onClick={() => setTab(option.key)}
              aria-pressed={tab === option.key}
              className={`rounded px-3 py-1 text-xs font-medium transition-colors ${
                tab === option.key ? 'bg-white text-acb-900 shadow-sm' : 'text-acb-600 hover:text-acb-900'
              }`}
            >
              {option.label}
            </button>
          ))}
        </div>
      </div>

      {roster.length === 0 ? (
        <p className="px-4 py-8 text-center text-sm text-acb-400">
          No hay jugadores disponibles para este equipo y temporada.
        </p>
      ) : (
        <div className="overflow-x-auto mobile-name-frame" tabIndex={0} aria-label="Estadísticas de la plantilla">
          <table className="data-table">
            <thead>
              <tr>
                <th className="data-table-head text-left data-table-sticky-head team-roster-sticky-player team-roster-col-player mobile-name-hide">
                  Jugador
                </th>
                <th className="data-table-head text-left data-table-sticky-head team-roster-sticky-position team-roster-col-position mobile-name-hide">
                  Posición
                </th>
                <th className="data-table-head text-left data-table-sticky-head team-roster-sticky-archetype team-roster-col-archetype mobile-name-hide">
                  Rol
                </th>
                {columns.map(column => (
                  <th
                    key={column.key}
                    className="data-table-head data-table-number"
                  >
                    {column.label}
                  </th>
                ))}
              </tr>
            </thead>
            {roster.map((player, i) => (
              <tbody key={rosterRowKeys[i]} className="mobile-name-group">
                <MobileNameRow
                  name={player.rosterName}
                  meta={(
                    <>
                      {player.rosterPosition} · <span className={archetypeTextClass(player.rosterArchetype.color)}>{player.rosterArchetype.name}</span>
                    </>
                  )}
                  onClick={() => navigate(player.rosterProfileUrl)}
                />
                <tr className="data-table-row">
                  <td className="data-table-cell data-table-identity team-roster-sticky-player team-roster-col-player mobile-name-hide">
                    <Link
                      to={player.rosterProfileUrl}
                      className="data-table-link block truncate"
                      title={player.rosterName}
                    >
                      {player.rosterName}
                    </Link>
                  </td>
                  <td className="data-table-cell text-left text-acb-600 team-roster-sticky-position team-roster-col-position mobile-name-hide">
                    <span className="block truncate" title={player.rosterPosition}>{player.rosterPosition}</span>
                  </td>
                  <td className="data-table-cell text-left team-roster-sticky-archetype team-roster-col-archetype mobile-name-hide">
                    <span
                      className={`team-roster-role ${archetypeTextClass(player.rosterArchetype.color)}`}
                      title={`${player.rosterArchetype.name}: ${player.rosterArchetype.desc}`}
                    >
                      {player.rosterArchetype.name}
                    </span>
                  </td>
                  {columns.map(column => (
                    <td
                      key={column.key}
                      className="data-table-cell data-table-number"
                    >
                      {formatPlayerProfileTableValue(player, column)}
                    </td>
                  ))}
                </tr>
              </tbody>
            ))}
          </table>
        </div>
      )}
    </section>
  )
}
