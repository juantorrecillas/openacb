import { useMemo, useEffect } from 'react'
import { useLocation, useNavigate, useParams, useSearchParams } from 'react-router-dom'
import { getPlayerPhoto } from '../utils/playerPhotos'
import { getPlayerDisplayName } from '../utils/playerNames'
import Court, { ShotMarker } from '../components/Court'
import ZoneHeatmap from '../components/ZoneHeatmap'
import DensityHeatmap from '../components/DensityHeatmap'
import PlayerCombobox from '../components/PlayerCombobox'
import ShotToolShell, { shotToolStyles } from '../components/ShotToolShell'
import { Circle, X } from 'lucide-react'
import {
  buildPlayerShotChartPath,
  buildShotChartsPath,
  buildTeamShotChartPath,
  isTeamId,
  parseRouteQuery,
  parsePlayerSegment,
  serializeRouteQuery,
  withQuery,
} from '../routing'


// Note: Zone calculation functions removed since we now use pre-calculated
// zone and zoned fields from the CSV data

const getPlayerRecord = (players, playerId, season) => players.find(
  player => String(player.licenseId) === String(playerId) && Number(player.season) === Number(season)
)

const normalizeSearch = (value) => String(value || '')
  .normalize('NFD')
  .replace(/[\u0300-\u036f]/g, '')
  .toLocaleLowerCase('es')

const isMadeShot = (shot) => shot.made === true || shot.made === 'true' || shot.made === 1 || shot.made === '1'

export default function ShotCharts({ loadShotsForSeason, shotsCache, loadingShots, teams, players, playerPhotos = {} }) {
  const location = useLocation()
  const navigate = useNavigate()
  const { teamId: urlTeamId, player: urlPlayer } = useParams()
  const [searchParams] = useSearchParams()

  // get available seasons from teams data since shots load per season
  const availableSeasons = useMemo(() => {
    const seasons = [...new Set(teams.map(t => t.season))].filter(s => s >= 2021).sort((a, b) => b - a)
    return seasons
  }, [teams])

  const search = searchParams.toString()
  const query = useMemo(() => parseRouteQuery('shotCharts', search), [search])
  const requestedSeason = Number(query.values.temporada)
  const selectedSeason = availableSeasons.includes(requestedSeason)
    ? requestedSeason
    : (availableSeasons[0] || 2025)
  const parsedPlayer = parsePlayerSegment(urlPlayer)
  const filterType = urlTeamId ? 'team' : parsedPlayer ? 'player' : (query.values.tipo || 'team')
  const selectedPlayer = parsedPlayer?.id || ''
  const shotFilter = query.values.resultado || 'all'
  const displayMode = query.values.vista || 'shots'
  const heatmapMode = query.values.mapa || 'frequency'
  const zoneMode = query.values.zonas || 'efficiency'

  const seasonTeams = useMemo(
    () => teams.filter(team => Number(team.season) === Number(selectedSeason)),
    [selectedSeason, teams]
  )
  const teamById = useMemo(
    () => new Map(seasonTeams.map(team => [team.teamId, team.team])),
    [seasonTeams]
  )
  const teamIdByName = useMemo(
    () => new Map(seasonTeams.map(team => [team.team, team.teamId])),
    [seasonTeams]
  )
  const selectedTeamId = urlTeamId || (filterType === 'player' ? query.values.equipo : null)
  const selectedTeam = selectedTeamId ? (teamById.get(selectedTeamId) || '') : ''
  const playerRecord = selectedPlayer ? getPlayerRecord(players, selectedPlayer, selectedSeason) : null
  const efficiencyMap = displayMode === 'zones' && zoneMode === 'efficiency'
  const effectiveShotFilter = efficiencyMap ? 'all' : shotFilter

  const canonicalValues = useMemo(() => ({
    ...query.values,
    temporada: selectedSeason,
    resultado: effectiveShotFilter,
    vista: displayMode,
    mapa: heatmapMode,
    zonas: zoneMode,
    equipo: filterType === 'player' ? selectedTeamId : undefined,
    tipo: urlTeamId || urlPlayer ? undefined : filterType,
  }), [displayMode, effectiveShotFilter, filterType, heatmapMode, query.values, selectedSeason, selectedTeamId, urlPlayer, urlTeamId, zoneMode])

  useEffect(() => {
    let canonicalPath = buildShotChartsPath()
    if (urlTeamId) {
      if (!isTeamId(urlTeamId)) return
      canonicalPath = buildTeamShotChartPath(urlTeamId)
    } else if (urlPlayer) {
      if (!parsedPlayer || !playerRecord) return
      canonicalPath = buildPlayerShotChartPath(playerRecord)
    }

    const canonicalSearch = serializeRouteQuery('shotCharts', canonicalValues, { strict: false })
    const target = withQuery(canonicalPath, canonicalSearch)
    const current = withQuery(location.pathname, search)
    if (target !== current) navigate(target, { replace: true })
  }, [canonicalValues, location.pathname, navigate, parsedPlayer, playerRecord, search, urlPlayer, urlTeamId])

  const updateRoute = (updates, pathname = location.pathname) => {
    const nextValues = { ...canonicalValues, ...updates }
    if (pathname !== buildShotChartsPath()) nextValues.tipo = undefined

    navigate(withQuery(
      pathname,
      serializeRouteQuery('shotCharts', nextValues, { strict: false })
    ))
  }

  // load shots when season changes
  useEffect(() => {
    if (selectedSeason) {
      loadShotsForSeason(selectedSeason)
    }
  }, [selectedSeason, loadShotsForSeason])

  // get shots for current season from cache
  const seasonFilteredShots = useMemo(() => {
    return shotsCache[selectedSeason] || []
  }, [shotsCache, selectedSeason])

  // check if shots are currently loading
  const isLoadingSeasonShots = loadingShots[selectedSeason] || false

  const teamList = useMemo(() =>
    [...new Set(seasonFilteredShots.map(s => s.team))].sort((a, b) => a.localeCompare(b, 'es')),
    [seasonFilteredShots]
  )

  const playerList = useMemo(() => {
    // Build list of unique players using playerId as the unique key
    const shots = (filterType === 'team' || filterType === 'player') && selectedTeam
      ? seasonFilteredShots.filter(s => s.team === selectedTeam)
      : seasonFilteredShots

    // Create a map of playerId -> player name to ensure uniqueness by ID
    const playerMap = new Map()
    shots.forEach(s => {
      const id = String(s.playerId)
      if (s.playerId && !playerMap.has(id)) {
        playerMap.set(id, s.player)
      }
    })

    // Helper function to extract surname for sorting
  // Handles: "J. Rubio" -> "Rubio", "M. A. Gasol" -> "A. Gasol", "Luwawu-Cabarrot" -> "Luwawu-Cabarrot"
  const getSortKey = (name) => {
    if (!name || typeof name !== 'string') return ''
    
    // Normalize: trim and replace multiple spaces
    const normalized = name.trim().replace(/\s+/g, ' ')
    
    // Split by dot to handle initials
    const dotParts = normalized.split('.')
    if (dotParts.length >= 2) {
      // Take everything after the last dot
      const afterLastDot = dotParts.slice(1).join('.').trim()
      if (afterLastDot) {
        return afterLastDot
      }
    }
    
    // No dots - use full name
    return normalized
  }

  // Return array of {id, name, displayName} objects sorted by surname
    return Array.from(playerMap.entries())
      .map(([id, name]) => {
        const record = getPlayerRecord(players, id, selectedSeason)
        const displayName = record ? getPlayerDisplayName(record, name) : name
        return { id, name, displayName }
      })
      .sort((a, b) => {
        const aSortKey = getSortKey(a.displayName)
        const bSortKey = getSortKey(b.displayName)
        return aSortKey.localeCompare(bSortKey)
      })
  }, [seasonFilteredShots, filterType, selectedTeam, players, selectedSeason])

  const playerOptions = useMemo(() => playerList.map(player => ({
    value: player.id,
    label: player.displayName,
    searchText: `${normalizeSearch(player.name)} ${normalizeSearch(player.displayName)}`,
    meta: selectedTeam || 'Todos los equipos',
  })), [playerList, selectedTeam])

  // conserva todos los intentos de la selección para los denominadores
  const analysisShots = useMemo(() => {
    if (!seasonFilteredShots || !Array.isArray(seasonFilteredShots)) return []

    if (filterType === 'team' && !selectedTeam) return []
    if (filterType === 'player' && !selectedPlayer) return []

    return seasonFilteredShots.filter(shot => {
      if (filterType === 'team' && selectedTeam && shot.team !== selectedTeam) {
        return false
      }
      if (filterType === 'player' && selectedPlayer && String(shot.playerId) !== selectedPlayer) {
        return false
      }
      if (filterType === 'player' && selectedTeam && shot.team !== selectedTeam) return false

      return true
    })
  }, [seasonFilteredShots, filterType, selectedTeam, selectedPlayer])

  // el resultado del tiro solo filtra capas visuales compatibles
  const filteredShots = useMemo(() => {
    if (effectiveShotFilter === 'all') return analysisShots
    return analysisShots.filter(shot => effectiveShotFilter === 'made' ? isMadeShot(shot) : !isMadeShot(shot))
  }, [analysisShots, effectiveShotFilter])

  const heatmapReferenceShots = useMemo(() => {
    if (!seasonFilteredShots || !Array.isArray(seasonFilteredShots)) return []
    if (effectiveShotFilter === 'all') return seasonFilteredShots

    return seasonFilteredShots.filter(shot => effectiveShotFilter === 'made' ? isMadeShot(shot) : !isMadeShot(shot))
  }, [effectiveShotFilter, seasonFilteredShots])
  
  const stats = useMemo(() => {
    const total = analysisShots.length
    const made = analysisShots.filter(isMadeShot).length
    const threes = analysisShots.filter(s => s.shotType?.includes('3'))
    const threeMade = threes.filter(isMadeShot).length
    const twos = analysisShots.filter(s => !s.shotType?.includes('3'))
    const twoMade = twos.filter(isMadeShot).length
    const points = analysisShots.reduce((sum, s) => sum + (Number(s.points) || 0), 0)
    
    return {
      total,
      made,
      fgPct: total > 0 ? ((made / total) * 100).toFixed(1) : '0.0',
      threePct: threes.length > 0 ? ((threeMade / threes.length) * 100).toFixed(1) : '0.0',
      twoPct: twos.length > 0 ? ((twoMade / twos.length) * 100).toFixed(1) : '0.0',
      pps: total > 0 ? (points / total).toFixed(2) : '0.00',
      efg: total > 0 ? (((twoMade + 1.5 * threeMade) / total) * 100).toFixed(1) : '0.0'
    }
  }, [analysisShots])
  
  // Zone breakdown
  const zoneStats = useMemo(() => {
    const byZone = {}
    analysisShots.forEach(shot => {
      // Use the pre-calculated zone field from CSV data
      const dataZone = shot.zoned || shot.zone
      if (!dataZone) return
      if (!byZone[dataZone]) {
        byZone[dataZone] = { attempts: 0, makes: 0, points: 0 }
      }
      byZone[dataZone].attempts++
      if (isMadeShot(shot)) {
        byZone[dataZone].makes++
        byZone[dataZone].points += Number(shot.points) || 0
      }
    })
    
    return Object.entries(byZone).map(([zone, data]) => ({
      zone,
      ...data,
      pct: ((data.makes / data.attempts) * 100).toFixed(1),
      pps: (data.points / data.attempts).toFixed(2)
    })).sort((a, b) => b.attempts - a.attempts)
  }, [analysisShots])

  const invalidTeam = Boolean(urlTeamId && (!isTeamId(urlTeamId) || !selectedTeam))
  const invalidPlayer = Boolean(urlPlayer && (!parsedPlayer || !playerRecord))
  const invalidPlayerTeam = Boolean(filterType === 'player' && selectedTeamId && !selectedTeam)
  const hasInvalidEntity = invalidTeam || invalidPlayer || invalidPlayerTeam
  const hasSelection = !hasInvalidEntity && (filterType === 'team' ? Boolean(selectedTeam) : Boolean(selectedPlayer))

  const subjectName = filterType === 'player' && selectedPlayer
    ? playerList.find(player => player.id === selectedPlayer)?.displayName || selectedPlayer
    : selectedTeam || 'Selección de tiro'
  const viewDescription = displayMode === 'shots'
    ? 'Localización de cada intento'
    : displayMode === 'heatmap'
      ? (heatmapMode === 'frequency' ? 'Frecuencia respecto a la liga' : 'Densidad de intentos')
      : (zoneMode === 'frequency' ? 'Frecuencia por zona' : 'Eficiencia por zona')

  const setDisplayMode = (mode) => {
    updateRoute({
      vista: mode,
      resultado: mode === 'zones' && zoneMode === 'efficiency' ? 'all' : effectiveShotFilter,
    })
  }

  const summaryMetrics = [
    { label: 'TC%', value: `${stats.fgPct}%` },
    { label: 'eFG%', value: `${stats.efg}%` },
    { label: 'PPT', value: stats.pps },
    { label: 'Anotados / intentos', value: `${stats.made}/${stats.total}` },
    { label: '2P%', value: `${stats.twoPct}%` },
    { label: '3P%', value: `${stats.threePct}%` },
  ]

  return (
    <ShotToolShell
      activeTool="charts"
      title="Cartas de tiro"
      scope={isLoadingSeasonShots ? 'Cargando datos…' : `Temporada ${selectedSeason - 1}-${String(selectedSeason).slice(-2)}`}
    >
      <section className={shotToolStyles.controlBand} aria-label="Filtros de la carta de tiro">
        <div className={shotToolStyles.controlsGrid}>
          <div className={shotToolStyles.controlField}>
            <label className={shotToolStyles.controlLabel} htmlFor="shot-season">Temporada</label>
            <select
              id="shot-season"
              value={selectedSeason}
              onChange={(event) => updateRoute({ temporada: Number(event.target.value) })}
              className="form-control"
            >
              {availableSeasons.map(season => (
                <option key={season} value={season}>{season - 1}-{String(season).slice(-2)}</option>
              ))}
            </select>
          </div>

          <div className={shotToolStyles.controlField}>
            <span className={shotToolStyles.controlLabel}>Selección</span>
            <div className={`segmented-control ${shotToolStyles.segmentFill}`} aria-label="Vista por equipo o jugador">
              <button
                type="button"
                className="segmented-option"
                aria-pressed={filterType === 'team'}
                onClick={() => updateRoute({ tipo: 'team', equipo: undefined }, buildShotChartsPath())}
              >
                Equipo
              </button>
              <button
                type="button"
                className="segmented-option"
                aria-pressed={filterType === 'player'}
                onClick={() => updateRoute({ tipo: 'player', equipo: undefined }, buildShotChartsPath())}
              >
                Jugador
              </button>
            </div>
          </div>

          <div className={shotToolStyles.controlField}>
            <label className={shotToolStyles.controlLabel} htmlFor="shot-team">
              Equipo{filterType === 'player' ? ' · opcional' : ''}
            </label>
            <select
              id="shot-team"
              value={selectedTeamId || ''}
              onChange={(event) => {
                const nextTeamId = event.target.value || undefined
                const pathname = filterType === 'team'
                  ? (nextTeamId ? buildTeamShotChartPath(nextTeamId) : buildShotChartsPath())
                  : (playerRecord ? buildPlayerShotChartPath(playerRecord) : buildShotChartsPath())
                updateRoute({ equipo: filterType === 'player' ? nextTeamId : undefined }, pathname)
              }}
              className="form-control"
            >
              <option value="" disabled={filterType === 'team'}>
                {filterType === 'player' ? 'Todos los equipos' : 'Selecciona equipo…'}
              </option>
              {teamList.map(team => {
                const teamId = teamIdByName.get(team)
                return teamId ? <option key={teamId} value={teamId}>{team}</option> : null
              })}
            </select>
          </div>

          {filterType === 'player' && (
            <div className={`${shotToolStyles.controlField} ${shotToolStyles.controlFieldWide}`}>
              <PlayerCombobox
                id="shot-chart-player"
                label={`Jugador (${playerOptions.length})`}
                options={playerOptions}
                value={selectedPlayer}
                onChange={(option) => {
                  const record = option ? getPlayerRecord(players, option.value, selectedSeason) : null
                  updateRoute(
                    { equipo: selectedTeamId || undefined },
                    record ? buildPlayerShotChartPath(record) : buildShotChartsPath()
                  )
                }}
                placeholder="Buscar jugador…"
              />
            </div>
          )}

          <div className={`${shotToolStyles.controlField} ${shotToolStyles.controlFieldWide}`}>
            <span className={shotToolStyles.controlLabel}>Representación</span>
            <div className={`segmented-control ${shotToolStyles.segmentFill}`} aria-label="Modo de visualización">
              {[
                ['shots', 'Tiros'],
                ['heatmap', 'Mapa de calor'],
                ['zones', 'Zonas'],
              ].map(([mode, label]) => (
                <button
                  key={mode}
                  type="button"
                  className="segmented-option"
                  aria-pressed={displayMode === mode}
                  onClick={() => setDisplayMode(mode)}
                >
                  {label}
                </button>
              ))}
            </div>
          </div>

          {displayMode === 'heatmap' && (
            <div className={shotToolStyles.controlField}>
              <span className={shotToolStyles.controlLabel}>Lectura del mapa</span>
              <div className={`segmented-control ${shotToolStyles.segmentFill}`} aria-label="Tipo de mapa de calor">
                <button type="button" className="segmented-option" aria-pressed={heatmapMode === 'frequency'} onClick={() => updateRoute({ mapa: 'frequency' })}>Frecuencia</button>
                <button type="button" className="segmented-option" aria-pressed={heatmapMode === 'density'} onClick={() => updateRoute({ mapa: 'density' })}>Densidad</button>
              </div>
            </div>
          )}

          {displayMode === 'zones' && (
            <div className={shotToolStyles.controlField}>
              <span className={shotToolStyles.controlLabel}>Métrica de zona</span>
              <div className={`segmented-control ${shotToolStyles.segmentFill}`} aria-label="Métrica del mapa por zonas">
                <button type="button" className="segmented-option" aria-pressed={zoneMode === 'efficiency'} onClick={() => updateRoute({ zonas: 'efficiency', resultado: 'all' })}>Eficiencia</button>
                <button type="button" className="segmented-option" aria-pressed={zoneMode === 'frequency'} onClick={() => updateRoute({ zonas: 'frequency', resultado: effectiveShotFilter })}>Frecuencia</button>
              </div>
            </div>
          )}

          <div className={shotToolStyles.controlField}>
            <span className={shotToolStyles.controlLabel}>Resultado</span>
            <div className={`segmented-control ${shotToolStyles.segmentFill}`} aria-label="Resultado del tiro">
              {[
                ['all', 'Todos'],
                ['made', 'Anotados'],
                ['missed', 'Fallados'],
              ].map(([result, label]) => (
                <button
                  key={result}
                  type="button"
                  className="segmented-option"
                  aria-pressed={effectiveShotFilter === result}
                  disabled={efficiencyMap && result !== 'all'}
                  onClick={() => updateRoute({ resultado: result })}
                >
                  {label}
                </button>
              ))}
            </div>
            {efficiencyMap && <p className={shotToolStyles.controlNote}>La eficiencia usa todos los intentos.</p>}
          </div>
        </div>
      </section>

      {hasInvalidEntity ? (
        <section className={shotToolStyles.emptyState} role="alert">
          <div>
            <p className="font-semibold text-acb-900">La selección del enlace no está disponible.</p>
            <p className="mt-1 text-sm">Elige otro equipo o jugador para esta temporada.</p>
          </div>
        </section>
      ) : !hasSelection ? (
        <section className={shotToolStyles.emptyState}>
          <div>
            <p className="font-semibold text-acb-900">Elige {filterType === 'team' ? 'un equipo' : 'un jugador'}.</p>
            <p className="mt-1 text-sm">La carta y su resumen aparecerán aquí con la misma selección.</p>
          </div>
        </section>
      ) : (
        <section className={shotToolStyles.analysisSheet} aria-label={`Carta de tiro de ${subjectName}`}>
          <div className={shotToolStyles.courtPanel}>
            <header className={shotToolStyles.panelHeader}>
              <div className={shotToolStyles.panelIdentity}>
                {filterType === 'player' && selectedPlayer && getPlayerPhoto(playerPhotos, selectedPlayer, selectedSeason) && (
                  <img
                    src={getPlayerPhoto(playerPhotos, selectedPlayer, selectedSeason)}
                    alt=""
                    className={shotToolStyles.panelPortrait}
                  />
                )}
                <div className="min-w-0">
                  <h2 className={shotToolStyles.panelTitle}>{subjectName}</h2>
                  <p className={shotToolStyles.panelContext}>{viewDescription}</p>
                </div>
              </div>
              <div className={shotToolStyles.markerLegend}>
                {displayMode === 'shots' && (
                  <>
                    <span><Circle className="h-3 w-3 fill-positive text-positive" aria-hidden="true" /> Anotado</span>
                    <span><X className="h-3 w-3 text-negative" aria-hidden="true" /> Fallado</span>
                  </>
                )}
                {displayMode !== 'shots' && <span>{filteredShots.length} tiros representados</span>}
              </div>
            </header>

            <div className={shotToolStyles.courtStage}>
              {displayMode === 'shots' && (
                <div className={shotToolStyles.courtScroller} tabIndex="0" aria-label="Carta de tiro; desplázate horizontalmente para ver toda la cancha">
                  <div className={shotToolStyles.courtFrame}>
                    <Court width={750} height={705}>
                      {filteredShots.map((shot, index) => (
                        <ShotMarker
                          key={shot.id || index}
                          x={shot.x}
                          y={shot.y}
                          made={shot.made}
                          size={5}
                          width={750}
                        />
                      ))}
                    </Court>
                  </div>
                </div>
              )}

              {displayMode === 'heatmap' && (
                <DensityHeatmap
                  shots={filteredShots}
                  referenceShots={heatmapReferenceShots}
                  mode={heatmapMode}
                  width={750}
                  height={705}
                />
              )}

              {displayMode === 'zones' && (
                <ZoneHeatmap
                  shots={zoneMode === 'efficiency' ? analysisShots : filteredShots}
                  leagueShots={zoneMode === 'efficiency' ? seasonFilteredShots : heatmapReferenceShots}
                  metric={zoneMode}
                  width={750}
                  height={705}
                />
              )}
            </div>
          </div>

          <aside className={shotToolStyles.sidePanel} aria-label="Resumen de la selección">
            <header className={shotToolStyles.sideHeader}>
              <h2 className={shotToolStyles.sideTitle}>Resumen</h2>
            </header>
            <div className={shotToolStyles.summaryGrid}>
              {summaryMetrics.map(metric => (
                <div key={metric.label} className={shotToolStyles.summaryMetric}>
                  <div className={shotToolStyles.summaryValue}>{metric.value}</div>
                  <div className={shotToolStyles.summaryLabel}>{metric.label}</div>
                </div>
              ))}
            </div>
            <h3 className={shotToolStyles.subsectionHeader}>Detalle por zona</h3>
            <div className={shotToolStyles.zoneList}>
              {zoneStats.length > 0 ? zoneStats.map(zone => (
                <div key={zone.zone} className={shotToolStyles.zoneRow}>
                  <div className="min-w-0">
                    <div className={shotToolStyles.zoneName}>{zone.zone}</div>
                    <div className={shotToolStyles.zoneSample}>{zone.makes}/{zone.attempts}</div>
                  </div>
                  <div className={shotToolStyles.zoneReading}>
                    <div>{zone.pct}%</div>
                    <div className={shotToolStyles.zoneSecondary}>{zone.pps} PPT</div>
                  </div>
                </div>
              )) : (
                <p className={`${shotToolStyles.zoneRow} ${shotToolStyles.emptyMessage}`}>No hay detalle de zonas para esta selección.</p>
              )}
            </div>
          </aside>
        </section>
      )}
    </ShotToolShell>
  )
}
