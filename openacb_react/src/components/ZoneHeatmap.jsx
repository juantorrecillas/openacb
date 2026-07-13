import { useMemo } from 'react';
import Court from './Court';

// Court constants (matching Court.jsx)
const BASKET_Y = -12.425;      // 1.575m from baseline
const FREE_THROW_Y = -8.2;     // 5.8m from baseline
const THREE_PT_R = 6.75;       // meters from basket
const CORNER_3_X = 6.6;        // meters from center
const RESTRICTED_R = 1.25;     // meters
const BASELINE_Y = -14;
const SIDELINE_X = 7.5;
const PAINT_HALF_WIDTH = 2.4;  // From R script: x > -2.4 and x < 2.4
const CORNER_BOUNDARY_Y = -11; // From R script: y <= -11 for corners

// Angle where center zones meet elbow zones (29.7 degrees from vertical)
const ANGLE_BOUNDARY = 29.7 * (Math.PI / 180);

// Calculate points on the 3pt arc at angle boundary
const THREE_PT_AT_ANGLE_X = THREE_PT_R * Math.sin(ANGLE_BOUNDARY);
const THREE_PT_AT_ANGLE_Y = BASKET_Y + THREE_PT_R * Math.cos(ANGLE_BOUNDARY);

// Helper function to generate arc path points
function generateArcPoints(centerX, centerY, radius, startAngle, endAngle, numPoints = 20) {
  const points = [];
  for (let i = 0; i <= numPoints; i++) {
    const angle = startAngle + (endAngle - startAngle) * (i / numPoints);
    points.push({
      x: centerX + radius * Math.sin(angle),
      y: centerY + radius * Math.cos(angle)
    });
  }
  return points;
}

// Define zone polygons based on the R script logic
function getZonePolygons() {
  const zones = {};

  // 1. Zona (Restringida) - Semicircle r=1.25 centered at basket
  // Full semicircle from left to right, curving toward midcourt, with straight edge at bottom
  const restrictedArc = generateArcPoints(0, BASKET_Y, RESTRICTED_R, -Math.PI/2, Math.PI/2, 20);
  zones['Zona (Restringida)'] = [
    { x: -RESTRICTED_R, y: BASKET_Y },  // Start at left edge
    ...restrictedArc.slice(1, -1),       // Arc points (excluding duplicates)
    { x: RESTRICTED_R, y: BASKET_Y }     // End at right edge (closes with straight line)
  ];

  // 2. Zona no restringida - Paint area from FT line to restricted semicircle
  // Rectangle with bottom curved cutout for restricted area
  const paintLeft = -PAINT_HALF_WIDTH;
  const paintRight = PAINT_HALF_WIDTH;
  const restrictedArcReverse = generateArcPoints(0, BASKET_Y, RESTRICTED_R, Math.PI/2, -Math.PI/2, 20);
  zones['Zona no restringida'] = [
    { x: paintLeft, y: FREE_THROW_Y },   // Top left of paint
    { x: paintRight, y: FREE_THROW_Y },  // Top right of paint
    { x: paintRight, y: BASKET_Y },      // Down right side to basket level
    { x: RESTRICTED_R, y: BASKET_Y },    // To restricted area edge
    ...restrictedArcReverse,              // Arc around restricted area (toward midcourt)
    { x: -RESTRICTED_R, y: BASKET_Y },   // End of arc
    { x: paintLeft, y: BASKET_Y }        // Back up left side
  ];

  // 3. Triple Esquina Derecha - Right corner 3 (x <= -6.6, y <= -11)
  zones['Triple Esquina Derecha'] = [
    { x: -SIDELINE_X, y: BASELINE_Y },
    { x: -CORNER_3_X, y: BASELINE_Y },
    { x: -CORNER_3_X, y: CORNER_BOUNDARY_Y },
    { x: -SIDELINE_X, y: CORNER_BOUNDARY_Y }
  ];

  // 4. Triple Esquina Izquierda - Left corner 3 (x >= 6.6, y <= -11)
  zones['Triple Esquina Izquierda'] = [
    { x: CORNER_3_X, y: BASELINE_Y },
    { x: SIDELINE_X, y: BASELINE_Y },
    { x: SIDELINE_X, y: CORNER_BOUNDARY_Y },
    { x: CORNER_3_X, y: CORNER_BOUNDARY_Y }
  ];

  // 5. Media Distancia Esquina Derecha - Right corner mid (x: -6.6 to -2.4, y <= -11)
  zones['Media Distancia Esquina Derecha'] = [
    { x: -CORNER_3_X, y: BASELINE_Y },
    { x: -PAINT_HALF_WIDTH, y: BASELINE_Y },
    { x: -PAINT_HALF_WIDTH, y: CORNER_BOUNDARY_Y },
    { x: -CORNER_3_X, y: CORNER_BOUNDARY_Y }
  ];

  // 6. Media Distancia Esquina Izquierda - Left corner mid (x: 2.4 to 6.6, y <= -11)
  zones['Media Distancia Esquina Izquierda'] = [
    { x: PAINT_HALF_WIDTH, y: BASELINE_Y },
    { x: CORNER_3_X, y: BASELINE_Y },
    { x: CORNER_3_X, y: CORNER_BOUNDARY_Y },
    { x: PAINT_HALF_WIDTH, y: CORNER_BOUNDARY_Y }
  ];

  // 7. Triple Codo Derecha - Right elbow 3 (angle > 29.7, y > -11, distance >= 6.75, x < 0)
  // From 3pt arc at angle boundary to corner transition, then to sideline up to midcourt
  const tripleElbowRightArc = generateArcPoints(0, BASKET_Y, THREE_PT_R, -ANGLE_BOUNDARY, -Math.asin(CORNER_3_X / THREE_PT_R), 15);
  zones['Triple Codo Derecha'] = [
    { x: -THREE_PT_AT_ANGLE_X, y: THREE_PT_AT_ANGLE_Y },
    ...tripleElbowRightArc.slice(1),
    { x: -CORNER_3_X, y: CORNER_BOUNDARY_Y },
    { x: -SIDELINE_X, y: CORNER_BOUNDARY_Y },
    { x: -SIDELINE_X, y: 0 },  // Extend to midcourt
    { x: -THREE_PT_AT_ANGLE_X, y: 0 },  // Along midcourt to angle boundary
    { x: -THREE_PT_AT_ANGLE_X, y: THREE_PT_AT_ANGLE_Y }
  ];

  // 8. Triple Codo Izquierda - Left elbow 3 (angle > 29.7, y > -11, distance >= 6.75, x > 0)
  const tripleElbowLeftArc = generateArcPoints(0, BASKET_Y, THREE_PT_R, Math.asin(CORNER_3_X / THREE_PT_R), ANGLE_BOUNDARY, 15);
  zones['Triple Codo Izquierda'] = [
    { x: CORNER_3_X, y: CORNER_BOUNDARY_Y },
    ...tripleElbowLeftArc,
    { x: THREE_PT_AT_ANGLE_X, y: THREE_PT_AT_ANGLE_Y },
    { x: THREE_PT_AT_ANGLE_X, y: 0 },  // Up to midcourt at angle boundary
    { x: SIDELINE_X, y: 0 },  // Along midcourt to sideline
    { x: SIDELINE_X, y: CORNER_BOUNDARY_Y }
  ];

  // 9. Media Distancia Codo Derecha - Right elbow mid (angle > 29.7, y > -11, distance < 6.75, x < 0)
  // Between paint edge and 3pt line on the right wing
  const midElbowRightOuterArc = generateArcPoints(0, BASKET_Y, THREE_PT_R, -ANGLE_BOUNDARY, -Math.asin(CORNER_3_X / THREE_PT_R), 15);
  zones['Media Distancia Codo Derecha'] = [
    // Start from paint corner at y=-11
    { x: -PAINT_HALF_WIDTH, y: CORNER_BOUNDARY_Y },
    // Go up to paint corner at FT line (where angle ≈ 29.7°)
    { x: -PAINT_HALF_WIDTH, y: FREE_THROW_Y },
    // To the 3pt arc at angle boundary
    { x: -THREE_PT_AT_ANGLE_X, y: THREE_PT_AT_ANGLE_Y },
    // Arc along 3pt line to corner transition
    ...midElbowRightOuterArc.slice(1),
    // Down to corner boundary
    { x: -CORNER_3_X, y: CORNER_BOUNDARY_Y }
  ];

  // 10. Media Distancia Codo Izquierda - Left elbow mid (angle > 29.7, y > -11, distance < 6.75, x > 0)
  const midElbowLeftOuterArc = generateArcPoints(0, BASKET_Y, THREE_PT_R, Math.asin(CORNER_3_X / THREE_PT_R), ANGLE_BOUNDARY, 15);
  zones['Media Distancia Codo Izquierda'] = [
    { x: CORNER_3_X, y: CORNER_BOUNDARY_Y },
    ...midElbowLeftOuterArc,
    { x: THREE_PT_AT_ANGLE_X, y: THREE_PT_AT_ANGLE_Y },
    // To paint corner at FT line (where angle ≈ 29.7°)
    { x: PAINT_HALF_WIDTH, y: FREE_THROW_Y },
    { x: PAINT_HALF_WIDTH, y: CORNER_BOUNDARY_Y }
  ];

  // 11. Media Distancia Centro - Center mid (angle < 29.7, y > -8.2, distance < 6.75)
  // From FT line to 3pt arc, within angle boundaries (between elbow midranges)
  const midCenterOuterArc = generateArcPoints(0, BASKET_Y, THREE_PT_R, -ANGLE_BOUNDARY, ANGLE_BOUNDARY, 15);
  zones['Media Distancia Centro'] = [
    // Start at right paint corner at FT line
    { x: -PAINT_HALF_WIDTH, y: FREE_THROW_Y },
    // Across the FT line to left paint corner
    { x: PAINT_HALF_WIDTH, y: FREE_THROW_Y },
    // Up to 3pt arc at left angle boundary
    { x: THREE_PT_AT_ANGLE_X, y: THREE_PT_AT_ANGLE_Y },
    // Arc along 3pt line to right angle boundary
    ...midCenterOuterArc.reverse().slice(1)
  ];

  // 12. Triple Centro - Center 3 (angle < 29.7, y > -8.2, distance >= 6.75)
  // From 3pt arc to midcourt, within angle boundaries
  const tripleCenterArc = generateArcPoints(0, BASKET_Y, THREE_PT_R, -ANGLE_BOUNDARY, ANGLE_BOUNDARY, 15);
  zones['Triple Centro'] = [
    { x: -THREE_PT_AT_ANGLE_X, y: THREE_PT_AT_ANGLE_Y },
    ...tripleCenterArc,
    { x: THREE_PT_AT_ANGLE_X, y: THREE_PT_AT_ANGLE_Y },
    { x: THREE_PT_AT_ANGLE_X, y: 0 },  // To midcourt
    { x: -THREE_PT_AT_ANGLE_X, y: 0 }  // Along midcourt
  ];

  return zones;
}

// Custom label positions for zones where the centroid isn't ideal
// Coordinates are in court coordinate system
const CUSTOM_LABEL_POSITIONS = {
  'Zona no restringida': { x: 0, y: -10.2 },  // Between FT circle and restricted area
  'Triple Codo Derecha': { x: -5.8, y: -4 },  // Diagonally outward, behind 3pt line
  'Triple Codo Izquierda': { x: 5.8, y: -4 },  // Diagonally outward, behind 3pt line
  'Triple Centro': { x: 0, y: -4 },  // Same y-height as elbow threes for consistency
};

// Convert polygon points to SVG path
function polygonToPath(points, scale, offsetX) {
  if (!points || points.length === 0) return '';
  const svgPoints = points.map(p => ({
    x: (p.x + offsetX) * scale,
    y: (-p.y) * scale
  }));
  const d = svgPoints.map((p, i) => `${i === 0 ? 'M' : 'L'} ${p.x} ${p.y}`).join(' ');
  return d + ' Z';
}

// Calculate centroid of polygon for label placement
function getPolygonCentroid(points) {
  if (!points || points.length === 0) return { x: 0, y: 0 };
  const sum = points.reduce((acc, p) => ({ x: acc.x + p.x, y: acc.y + p.y }), { x: 0, y: 0 });
  return { x: sum.x / points.length, y: sum.y / points.length };
}

const TURBO_COLORS = [
  [48, 18, 59],
  [62, 84, 163],
  [33, 145, 140],
  [94, 201, 98],
  [253, 231, 37],
  [234, 151, 36],
  [217, 72, 33],
  [122, 4, 3]
];

function interpolateColors(colors, normalizedValue) {
  const idx = Math.max(0, Math.min(1, normalizedValue)) * (colors.length - 1);
  const lower = Math.floor(idx);
  const upper = Math.ceil(idx);
  const t = idx - lower;

  if (upper >= colors.length) return colors[colors.length - 1];

  const c1 = colors[lower];
  const c2 = colors[upper];
  return [
    Math.round(c1[0] + (c2[0] - c1[0]) * t),
    Math.round(c1[1] + (c2[1] - c1[1]) * t),
    Math.round(c1[2] + (c2[2] - c1[2]) * t)
  ];
}

function relativeTurboColor(normalizedValue, opacity = 0.7) {
  const clamped = Math.max(-1, Math.min(1, normalizedValue));
  const neutral = [255, 255, 255];
  const paletteColor = clamped < 0
    ? interpolateColors(TURBO_COLORS, Math.max(0.05, 0.32 * Math.abs(clamped)))
    : interpolateColors(TURBO_COLORS, 0.45 + (0.55 * clamped));
  const t = Math.abs(clamped);

  const r = Math.round(neutral[0] + (paletteColor[0] - neutral[0]) * t);
  const g = Math.round(neutral[1] + (paletteColor[1] - neutral[1]) * t);
  const b = Math.round(neutral[2] + (paletteColor[2] - neutral[2]) * t);

  return `rgba(${r}, ${g}, ${b}, ${opacity})`;
}

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
      <div className="mt-2 rounded border border-acb-200 bg-white p-2 text-xs">
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
                className="w-3 h-3 rounded"
                style={{ backgroundColor: isFrequencyMetric ? relativeTurboColor(1, 0.9) : 'var(--color-positive)' }}
              />
              <span>{isFrequencyMetric ? 'Más frecuente' : 'Mejor que la media'}</span>
            </div>
            <div className="flex items-center gap-1">
              <div
                className="w-3 h-3 rounded"
                style={{ backgroundColor: isFrequencyMetric ? relativeTurboColor(-1, 0.9) : 'var(--color-negative)' }}
              />
              <span>{isFrequencyMetric ? 'Menos frecuente' : 'Peor que la media'}</span>
            </div>
            {!isFrequencyMetric && (
              <div className="flex items-center gap-1">
                <div className="w-3 h-3 rounded bg-slate-400/25" />
                <span>Menos de {minEfficiencyAttempts} intentos</span>
              </div>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}
