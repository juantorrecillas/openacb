/**
 * FIBA Half-Court SVG Component
 *
 * Coordinate system (court coords):
 * - x: -7.5 to 7.5 (15m court width)
 * - y: -14 (baseline) to 0 (midcourt)
 * - Basket at (0, -12.425) which is 1.575m from baseline
 *
 * SVG coordinate system:
 * - x: 0 (left) to width (right)
 * - y: 0 (top = midcourt) to height (bottom = baseline)
 *
 * FIBA dimensions:
 * - Paint: 4.9m wide × 5.8m deep (from baseline to free throw line)
 * - Free throw line: 5.8m from baseline (y = -8.2)
 * - Free throw circle: 1.8m radius
 * - Three-point arc: 6.75m radius from basket center
 * - Corner 3s: straight lines at x = ±6.6 (0.9m from sideline)
 * - Restricted area: 1.25m radius semicircle
 */

// FIBA court constants
const BASKET_Y = -12.425      // 1.575m from baseline
const FREE_THROW_Y = -8.2     // 5.8m from baseline
const PAINT_WIDTH = 4.9       // meters
const PAINT_DEPTH = 5.8       // meters (baseline to FT line)
const FT_CIRCLE_R = 1.8       // meters
const THREE_PT_R = 6.75       // meters from basket
const CORNER_3_X = 6.6        // meters from center (0.9m from sideline)
const RESTRICTED_R = 1.25     // meters

// Calculate where corner 3 line meets the arc
// At x = 6.6, find y where distance from basket = 6.75
// 6.75² = 6.6² + (y - BASKET_Y)²
// y = BASKET_Y + sqrt(6.75² - 6.6²)
const THREE_PT_ARC_Y = BASKET_Y + Math.sqrt(THREE_PT_R ** 2 - CORNER_3_X ** 2) // ≈ -11.01

export default function Court({ children, width = 500, height = 470 }) {
  const scale = width / 15
  // Transform court coords to SVG coords
  const tx = (x) => (x + 7.5) * scale
  const ty = (y) => (-y) * scale

  return (
    <svg
      viewBox={`0 0 ${width} ${height}`}
      className="w-full h-auto"
      style={{ maxWidth: width }}
    >
      {/* Background - light court surface (slightly lighter than paint area) */}
      <rect x={0} y={0} width={width} height={height} fill="#fafbfc" rx={4} />

      {/* Paint (key area) - from baseline to free throw line */}
      <rect
        x={tx(-PAINT_WIDTH / 2)}
        y={ty(FREE_THROW_Y)}
        width={PAINT_WIDTH * scale}
        height={PAINT_DEPTH * scale}
        fill="#f1f5f9" stroke="#94a3b8" strokeWidth={1.5}
      />

      {/* Free throw circle - at the free throw line */}
      <circle
        cx={tx(0)}
        cy={ty(FREE_THROW_Y)}
        r={FT_CIRCLE_R * scale}
        fill="none" stroke="#94a3b8" strokeWidth={1.5}
      />

      {/* Restricted area semicircle - centered on basket, curves towards midcourt */}
      {/* sweep-flag=1 because Y is flipped in SVG coords */}
      <path
        d={`M ${tx(-RESTRICTED_R)} ${ty(BASKET_Y)} A ${RESTRICTED_R * scale} ${RESTRICTED_R * scale} 0 0 1 ${tx(RESTRICTED_R)} ${ty(BASKET_Y)}`}
        fill="none" stroke="#94a3b8" strokeWidth={1.5}
      />

      {/* Three-point line: corner lines + arc */}
      {/* sweep-flag=1 to curve toward midcourt (away from basket) in flipped Y coords */}
      <path
        d={`M ${tx(-CORNER_3_X)} ${ty(-14)} L ${tx(-CORNER_3_X)} ${ty(THREE_PT_ARC_Y)} A ${THREE_PT_R * scale} ${THREE_PT_R * scale} 0 0 1 ${tx(CORNER_3_X)} ${ty(THREE_PT_ARC_Y)} L ${tx(CORNER_3_X)} ${ty(-14)}`}
        fill="none" stroke="#94a3b8" strokeWidth={1.5}
      />

      {/* Basket rim */}
      <circle
        cx={tx(0)}
        cy={ty(BASKET_Y)}
        r={0.225 * scale}
        fill="none" stroke="#f97316" strokeWidth={2}
      />
      {/* Backboard - positioned behind the basket (closer to baseline) */}
      <line
        x1={tx(-0.9)} y1={ty(BASKET_Y - 0.15)}
        x2={tx(0.9)} y2={ty(BASKET_Y - 0.15)}
        stroke="#64748b" strokeWidth={3}
      />

      {/* Court boundary lines */}
      {/* Baseline */}
      <line x1={tx(-7.5)} y1={ty(-14)} x2={tx(7.5)} y2={ty(-14)} stroke="#94a3b8" strokeWidth={1.5} />
      {/* Left sideline */}
      <line x1={tx(-7.5)} y1={ty(-14)} x2={tx(-7.5)} y2={ty(0)} stroke="#94a3b8" strokeWidth={1.5} />
      {/* Right sideline */}
      <line x1={tx(7.5)} y1={ty(-14)} x2={tx(7.5)} y2={ty(0)} stroke="#94a3b8" strokeWidth={1.5} />
      {/* Midcourt line */}
      <line x1={tx(-7.5)} y1={ty(0)} x2={tx(7.5)} y2={ty(0)} stroke="#94a3b8" strokeWidth={1.5} />

      {/* Center circle (half) - curves into the half court (toward baseline) */}
      {/* sweep-flag=1 (clockwise) makes arc curve downward in SVG (toward baseline) */}
      <path
        d={`M ${tx(-1.8)} ${ty(0)} A ${1.8 * scale} ${1.8 * scale} 0 0 1 ${tx(1.8)} ${ty(0)}`}
        fill="none" stroke="#94a3b8" strokeWidth={1.5}
      />


      {/* Shots layer */}
      {children}
    </svg>
  )
}

export function ShotMarker({ x, y, made, size = 6, width = 500 }) {
  const scale = width / 15
  const cx = (x + 7.5) * scale
  const cy = (-y) * scale
  
  return made ? (
    <circle cx={cx} cy={cy} r={size} fill="#059669" fillOpacity={0.7} />
  ) : (
    <g transform={`translate(${cx}, ${cy})`}>
      <line x1={-size} y1={-size} x2={size} y2={size} stroke="#dc2626" strokeWidth={2} strokeOpacity={0.6} />
      <line x1={size} y1={-size} x2={-size} y2={size} stroke="#dc2626" strokeWidth={2} strokeOpacity={0.6} />
    </g>
  )
}
