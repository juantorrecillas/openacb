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

// Calculate where corner 3 line meets the arc
const THREE_PT_ARC_Y = BASKET_Y + Math.sqrt(THREE_PT_R ** 2 - CORNER_3_X ** 2);

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
function polygonToPath(points, scale, offsetX, offsetY) {
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

export default function ZoneHeatmap({ shots, leagueShots = [], width = 750, height = 705 }) {
  // Get zone polygon definitions
  const zonePolygons = useMemo(() => getZonePolygons(), []);

  // Calculate zone statistics for filtered shots (team/player)
  const zoneStats = useMemo(() => {
    const stats = {};

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
    Object.keys(stats).forEach(zone => {
      const s = stats[zone];
      result[zone] = {
        attempts: s.attempts,
        makes: s.makes,
        fgPct: s.attempts > 0 ? (s.makes / s.attempts) * 100 : 0,
        pps: s.attempts > 0 ? s.points / s.attempts : 0
      };
    });

    return result;
  }, [shots]);

  // Calculate LEAGUE average FG% PER ZONE for comparison
  const leagueZoneAverages = useMemo(() => {
    const shotsToUse = leagueShots.length > 0 ? leagueShots : shots;
    const stats = {};

    shotsToUse.forEach(shot => {
      const zone = shot.zoned || shot.zone;
      if (!zone) return;

      if (!stats[zone]) {
        stats[zone] = { attempts: 0, makes: 0 };
      }

      stats[zone].attempts++;
      if (shot.made) {
        stats[zone].makes++;
      }
    });

    // Calculate league FG% per zone
    const result = {};
    Object.keys(stats).forEach(zone => {
      const s = stats[zone];
      result[zone] = s.attempts > 0 ? (s.makes / s.attempts) * 100 : 0;
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

  // Get color based on FG% difference from league average for that zone
  const getColor = (fgPct, leagueAvgForZone, opacity = 0.7) => {
    const diff = fgPct - leagueAvgForZone;

    // Color scale: red (bad) -> white (average) -> green (good)
    if (diff > 10) return `rgba(34, 197, 94, ${opacity})`; // Strong green
    if (diff > 5) return `rgba(134, 239, 172, ${opacity})`; // Light green
    if (diff > 0) return `rgba(220, 252, 231, ${opacity})`; // Very light green
    if (diff > -5) return `rgba(254, 226, 226, ${opacity})`; // Very light red
    if (diff > -10) return `rgba(252, 165, 165, ${opacity})`; // Light red
    return `rgba(220, 38, 38, ${opacity})`; // Strong red
  };

  // Fixed font size for all labels
  const fontSize = 11;

  if (shots.length === 0) {
    return (
      <div className="flex items-center justify-center" style={{ width, height }}>
        <div className="text-center text-acb-500">
          <p className="text-sm">No shots available for the selected filters</p>
          <p className="text-xs mt-1">Try adjusting your filter settings</p>
        </div>
      </div>
    );
  }

  const hasValidZones = shots.some(shot => shot.zoned || shot.zone);
  if (!hasValidZones) {
    return (
      <div className="flex items-center justify-center" style={{ width, height }}>
        <div className="text-center text-acb-500">
          <p className="text-sm">No zone data available in shots</p>
          <p className="text-xs mt-1">Zone information might be missing</p>
        </div>
      </div>
    );
  }

  return (
    <div className="relative" style={{ width, height }}>
      <Court width={width} height={height} />

      <svg
        width={width}
        height={height}
        viewBox={`0 0 ${width} ${height}`}
        className="absolute top-0 left-0"
        style={{ pointerEvents: 'none' }}
      >
        {/* Draw zone polygons */}
        {Object.entries(zonePolygons).map(([zoneName, points]) => {
          const stats = zoneStats[zoneName];
          if (!stats || stats.attempts === 0) return null;

          const leagueAvgForZone = leagueZoneAverages[zoneName] || 0;
          const pathD = polygonToPath(points, scale, offsetX);
          const color = getColor(stats.fgPct, leagueAvgForZone, 0.6);

          // Use custom position if available, otherwise use polygon centroid
          const customPos = CUSTOM_LABEL_POSITIONS[zoneName];
          const labelPos = customPos || getPolygonCentroid(points);
          const { x: labelX, y: labelY } = courtToSVG(labelPos.x, labelPos.y);

          const fgPctDiff = stats.fgPct - leagueAvgForZone;

          return (
            <g key={zoneName}>
              {/* Zone polygon */}
              <path
                d={pathD}
                fill={color}
                stroke="#333"
                strokeWidth="1"
                strokeOpacity="0.5"
              />

              {/* Zone label - FG% */}
              <text
                x={labelX}
                y={labelY - 4}
                textAnchor="middle"
                fontSize={fontSize}
                fontWeight="bold"
                fill="#111"
                fontFamily="Consolas, monospace"
              >
                {stats.fgPct.toFixed(1)}%
              </text>

              {/* Makes/Attempts */}
              <text
                x={labelX}
                y={labelY + 10}
                textAnchor="middle"
                fontSize={fontSize}
                fill="#333"
                fontFamily="Consolas, monospace"
              >
                {stats.makes}/{stats.attempts}
              </text>

              {/* Difference from league zone average */}
              {Math.abs(fgPctDiff) > 2 && (
                <text
                  x={labelX}
                  y={labelY + 22}
                  textAnchor="middle"
                  fontSize={fontSize * 0.85}
                  fill={fgPctDiff > 0 ? '#166534' : '#991b1b'}
                  fontFamily="Consolas, monospace"
                  fontWeight="bold"
                >
                  {fgPctDiff > 0 ? '+' : ''}{fgPctDiff.toFixed(1)}%
                </text>
              )}
            </g>
          );
        })}
      </svg>

      {/* Legend */}
      <div className="absolute bottom-2 left-2 right-2 bg-white/95 p-2 rounded border border-acb-300 text-xs">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-2">
            <span className="font-medium text-acb-700">Zone Statistics</span>
            <span className="text-acb-500">
              (vs League Zone Avg)
            </span>
          </div>
          <div className="flex items-center gap-3 text-xs">
            <div className="flex items-center gap-1">
              <div className="w-3 h-3 rounded" style={{ backgroundColor: 'rgba(34, 197, 94, 0.7)' }}></div>
              <span>Above avg</span>
            </div>
            <div className="flex items-center gap-1">
              <div className="w-3 h-3 rounded" style={{ backgroundColor: 'rgba(220, 38, 38, 0.7)' }}></div>
              <span>Below avg</span>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
