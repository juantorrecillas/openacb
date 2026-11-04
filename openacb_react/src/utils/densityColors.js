// Color scales of the shot density heat map (components/DensityHeatmap.jsx).
// The legend steps are also used by the image export.
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
  const idx = normalizedValue * (colors.length - 1);
  const lower = Math.floor(idx);
  const upper = Math.ceil(idx);
  const t = idx - lower;

  if (upper >= colors.length) {
    const c = colors[colors.length - 1];
    return c;
  }

  const c1 = colors[lower];
  const c2 = colors[upper];
  const r = Math.round(c1[0] + (c2[0] - c1[0]) * t);
  const g = Math.round(c1[1] + (c2[1] - c1[1]) * t);
  const b = Math.round(c1[2] + (c2[2] - c1[2]) * t);

  return [r, g, b];
}

const toRgb = (color) => `rgb(${color[0]}, ${color[1]}, ${color[2]})`;

export const getDensityColor = (normalizedValue) => {
  return toRgb(interpolateColors(TURBO_COLORS, normalizedValue));
};

export const getRelativeDensityColor = (normalizedValue) => {
  const clamped = Math.max(-1, Math.min(1, normalizedValue));
  const neutral = [255, 255, 255];
  const acbBlue = [72, 101, 129];
  const paletteColor = clamped < 0
    ? acbBlue
    : interpolateColors(TURBO_COLORS, 0.45 + (0.55 * clamped));
  const t = Math.abs(clamped);

  const r = Math.round(neutral[0] + (paletteColor[0] - neutral[0]) * t);
  const g = Math.round(neutral[1] + (paletteColor[1] - neutral[1]) * t);
  const b = Math.round(neutral[2] + (paletteColor[2] - neutral[2]) * t);

  return `rgb(${r}, ${g}, ${b})`;
};

// The legend's color steps, also used by the image export.
export function getDensityLegendColors(mode) {
  return mode === 'relative' || mode === 'frequency'
    ? Array(9).fill(0).map((_, i) => getRelativeDensityColor((i / 4) - 1))
    : Array(8).fill(0).map((_, i) => getDensityColor(i / 7));
}
