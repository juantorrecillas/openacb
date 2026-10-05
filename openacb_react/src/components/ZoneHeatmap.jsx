import { useMemo } from 'react';
import Court from './Court';

import { getZonePolygons, CUSTOM_LABEL_POSITIONS, polygonToPath, getPolygonCentroid, relativeTurboColor } from '../utils/shotZones';

export default function ZoneHeatmap({ shots, leagueShots = [], metric = 'efficiency', width = 750, height = 705, minEfficiencyAttempts = 10, higherIsBetter = true }) {
  // Get zone polygon definitions
  const zonePolygons = useMemo(() => getZonePolygons(), []);

  // Calculate zone statistics for filtered shots (team/player)
  const zoneStats = useMemo(() => {
    const stats = {};
    const totalAttempts = shots.reduce((sum, shot) => (shot.zoned || shot.zone) ? sum + 1 : sum, 0);

    shots.forEach(shot => {
      const zone = shot.zoned || shot.zone;
      if (!zone) return;

      if (!stats[zone]) {
        stats[zone] = { attempts: 0, makes: 0, points: 0 };
      }

      stats[zone].attempts++;
      if (shot.made) {
        stats[zone].makes++;
      }
      stats[zone].points += shot.points || 0;
    });

    // Calculate percentages for each zone
    const result = {};
    Object.keys(zonePolygons).forEach(zone => {
      const s = stats[zone] || { attempts: 0, makes: 0, points: 0 };
      result[zone] = {
        attempts: s.attempts,
        makes: s.makes,
        fgPct: s.attempts > 0 ? (s.makes / s.attempts) * 100 : 0,
        freqPct: totalAttempts > 0 ? (s.attempts / totalAttempts) * 100 : 0,
        pps: s.attempts > 0 ? s.points / s.attempts : 0
      };
    });

    return result;
  }, [shots, zonePolygons]);

  // Calculate LEAGUE average FG% PER ZONE for comparison
  const leagueZoneAverages = useMemo(() => {
    const shotsToUse = leagueShots.length > 0 ? leagueShots : shots;
    const stats = {};
    let totalAttempts = 0;

    shotsToUse.forEach(shot => {
      const zone = shot.zoned || shot.zone;
      if (!zone) return;
      totalAttempts++;

      if (!stats[zone]) {
        stats[zone] = { attempts: 0, makes: 0 };
      }

      stats[zone].attempts++;
      if (shot.made) {
        stats[zone].makes++;
      }
    });

    // Calculate league FG% and shot frequency per zone
    const result = {};
    Object.keys(stats).forEach(zone => {
      const s = stats[zone];
      result[zone] = {
        fgPct: s.attempts > 0 ? (s.makes / s.attempts) * 100 : 0,
        freqPct: totalAttempts > 0 ? (s.attempts / totalAttempts) * 100 : 0
      };
    });

    return result;
  }, [leagueShots, shots]);

  const scale = width / 15; // Court is 15m wide
  const offsetX = 7.5; // Center of court

  // Transform court coordinates to SVG coordinates
  const courtToSVG = (x, y) => {
    const svgX = (x + offsetX) * scale;
    const svgY = (-y) * scale;
    return { x: svgX, y: svgY };
  };

  const getEfficiencyColor = (performanceDiff) => {
    if (performanceDiff > 0) return 'var(--color-positive)';
    if (performanceDiff < 0) return 'var(--color-negative)';
    return '#ffffff';
  };

  const getEfficiencyOpacity = (performanceDiff) => {
    const difference = Math.abs(performanceDiff);
    if (difference > 10) return 0.72;
    if (difference > 5) return 0.5;
    if (difference > 0) return 0.26;
    return 1;
  };

  // Fixed font size for all labels
  const fontSize = 10;
  const isFrequencyMetric = metric === 'frequency';

  if (shots.length === 0) {
    return (
      <div className="flex items-center justify-center" style={{ width: '100%', maxWidth: width, aspectRatio: `${width} / ${height}` }}>
        <div className="text-center text-acb-500">
          <p className="text-sm">No hay tiros disponibles con los filtros seleccionados</p>
          <p className="text-xs mt-1">Prueba a ajustar los filtros</p>
        </div>
      </div>
    );
  }

  const maxFrequencyDiff = Math.max(
    ...Object.keys(zoneStats).map(zone => {
      const leagueAvgForZone = leagueZoneAverages[zone] || { freqPct: 0 };
      return Math.abs(zoneStats[zone].freqPct - leagueAvgForZone.freqPct);
    }),
    1
  );

  const hasValidZones = shots.some(shot => shot.zoned || shot.zone);
  if (!hasValidZones) {
    return (
      <div className="flex items-center justify-center" style={{ width: '100%', maxWidth: width, aspectRatio: `${width} / ${height}` }}>
        <div className="text-center text-acb-500">
          <p className="text-sm">No hay datos de zona disponibles</p>
          <p className="text-xs mt-1">Puede faltar la zona en los datos de tiro</p>
        </div>
      </div>
    );
  }

  return (
    <div className="w-full" style={{ maxWidth: width }}>
      <div className="overflow-x-auto pb-1">
        <div className="relative" style={{ minWidth: Math.min(560, width), aspectRatio: `${width} / ${height}` }}>
          <Court width={width} height={height} />

          <svg
            viewBox={`0 0 ${width} ${height}`}
            className="absolute inset-0 w-full h-full"
            role="img"
            aria-label={isFrequencyMetric ? 'Frecuencia de tiro por zona' : 'Eficiencia de tiro por zona'}
            style={{ pointerEvents: 'none' }}
          >
            {/* Draw zone polygons */}
            {Object.entries(zonePolygons).map(([zoneName, points]) => {
          const stats = zoneStats[zoneName] || { attempts: 0, makes: 0, fgPct: 0, freqPct: 0, pps: 0 };
          if (!isFrequencyMetric && stats.attempts === 0) return null;

          const leagueAvgForZone = leagueZoneAverages[zoneName] || { fgPct: 0, freqPct: 0 };
          const pathD = polygonToPath(points, scale, offsetX);
          const fgPctDiff = stats.fgPct - leagueAvgForZone.fgPct;
          const freqPctDiff = stats.freqPct - leagueAvgForZone.freqPct;
          const metricValue = isFrequencyMetric ? stats.freqPct : stats.fgPct;
          const displayedDiff = isFrequencyMetric ? freqPctDiff : fgPctDiff;
          const performanceDiff = higherIsBetter ? fgPctDiff : -fgPctDiff;
          const hasEfficiencySample = stats.attempts >= minEfficiencyAttempts;
          const color = isFrequencyMetric
            ? relativeTurboColor(freqPctDiff / maxFrequencyDiff, 0.66)
            : hasEfficiencySample
              ? getEfficiencyColor(performanceDiff)
              : '#94a3b8';
          const colorOpacity = isFrequencyMetric
            ? 1
            : hasEfficiencySample
              ? getEfficiencyOpacity(performanceDiff)
              : 0.25;

          // Use custom position if available, otherwise use polygon centroid
          const customPos = CUSTOM_LABEL_POSITIONS[zoneName];
          const labelPos = customPos || getPolygonCentroid(points);
          const { x: labelX, y: labelY } = courtToSVG(labelPos.x, labelPos.y);

          return (
            <g key={zoneName}>
              {/* Zone polygon */}
              <path
                d={pathD}
                fill={color}
                fillOpacity={colorOpacity}
                stroke="#333"
                strokeWidth="1"
                strokeOpacity="0.5"
              />

              {/* Zone label */}
              <text
                x={labelX}
                y={labelY - 4}
                textAnchor="middle"
                fontSize={fontSize}
                fontWeight="bold"
                fill="#111"
                fontFamily="JetBrains Mono, Consolas, monospace"
              >
                {metricValue.toFixed(1)}%
              </text>

              {/* Detail line */}
              <text
                x={labelX}
                y={labelY + 10}
                textAnchor="middle"
                fontSize={fontSize}
                fill="#333"
                fontFamily="JetBrains Mono, Consolas, monospace"
              >
                {isFrequencyMetric ? `${stats.attempts} tiros` : `${stats.makes}/${stats.attempts}`}
              </text>

              {/* Difference from league zone average */}
              {(isFrequencyMetric || hasEfficiencySample) && Math.abs(displayedDiff) > (isFrequencyMetric ? 1 : 2) && (
                <text
                  x={labelX}
                  y={labelY + 22}
                  textAnchor="middle"
                  fontSize={fontSize * 0.85}
                  className="fill-acb-600"
                  fontFamily="JetBrains Mono, Consolas, monospace"
                  fontWeight="bold"
                >
                  {displayedDiff > 0 ? '+' : ''}{displayedDiff.toFixed(1)} pp
                </text>
              )}
            </g>
          );
            })}
          </svg>
        </div>
      </div>

      {/* Legend */}
      <div className="border-t border-acb-200 bg-acb-50 px-4 py-3 text-xs">
        <div className="flex flex-col sm:flex-row sm:items-center justify-between gap-2">
          <div className="flex items-center gap-2 flex-wrap">
            <span className="font-medium text-acb-700">
              {isFrequencyMetric ? 'Distribución por zonas de tiro' : 'Eficiencia por zonas de tiro'}
            </span>
            <span className="text-acb-500">vs. media de la liga</span>
          </div>
          <div className="flex items-center gap-3 text-xs flex-wrap">
            <div className="flex items-center gap-1">
              <div
                className="h-3 w-3 border border-white/60"
                style={{ backgroundColor: isFrequencyMetric ? relativeTurboColor(1, 0.9) : 'var(--color-positive)' }}
              />
              <span>{isFrequencyMetric ? 'Más frecuente' : 'Mejor que la media'}</span>
            </div>
            <div className="flex items-center gap-1">
              <div
                className="h-3 w-3 border border-white/60"
                style={{ backgroundColor: isFrequencyMetric ? relativeTurboColor(-1, 0.9) : 'var(--color-negative)' }}
              />
              <span>{isFrequencyMetric ? 'Menos frecuente' : 'Peor que la media'}</span>
            </div>
            {!isFrequencyMetric && (
              <div className="flex items-center gap-1">
                <div className="h-3 w-3 border border-acb-300 bg-slate-400/25" />
                <span>Menos de {minEfficiencyAttempts} intentos</span>
              </div>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}
