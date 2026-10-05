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


export { getZonePolygons, CUSTOM_LABEL_POSITIONS, polygonToPath, getPolygonCentroid, relativeTurboColor };
