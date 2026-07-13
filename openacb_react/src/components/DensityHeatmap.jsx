import { useMemo } from 'react';
import Court from './Court';

const GRID_SIZE = 100;
const MIN_RELATIVE_INTENSITY = 0.08;

function createEmptyGrid(gridSize) {
  return Array(gridSize).fill(null).map(() => Array(gridSize).fill(0));
}

function quantile(values, q) {
  if (values.length === 0) return 0;
  const sorted = [...values].sort((a, b) => a - b);
  const index = Math.min(sorted.length - 1, Math.floor((sorted.length - 1) * q));
  return sorted[index];
}

function createShotGrid(shots, width, height, gridSize) {
  const grid = createEmptyGrid(gridSize);
  const scale = width / 15;
  const cellWidth = width / gridSize;
  const cellHeight = height / gridSize;
  const bandwidth = Math.min(width, height) / 11;
  const radiusX = Math.ceil((bandwidth * 3) / cellWidth);
  const radiusY = Math.ceil((bandwidth * 3) / cellHeight);

  shots.forEach(shot => {
    const shotX = Number(shot.x);
    const shotY = Number(shot.y);
    if (!Number.isFinite(shotX) || !Number.isFinite(shotY)) return;

    const svgX = (shotX + 7.5) * scale;
    const svgY = (-shotY) * scale;
    const centerI = Math.floor(svgX / cellWidth);
    const centerJ = Math.floor(svgY / cellHeight);

    const startI = Math.max(0, centerI - radiusX);
    const endI = Math.min(gridSize - 1, centerI + radiusX);
    const startJ = Math.max(0, centerJ - radiusY);
    const endJ = Math.min(gridSize - 1, centerJ + radiusY);

    for (let i = startI; i <= endI; i++) {
      const cellX = (i + 0.5) * cellWidth;
      const dx = (svgX - cellX) / bandwidth;

      for (let j = startJ; j <= endJ; j++) {
        const cellY = (j + 0.5) * cellHeight;
        const dy = (svgY - cellY) / bandwidth;
        const distSq = dx * dx + dy * dy;
        if (distSq <= 9) {
          grid[i][j] += Math.exp(-0.5 * distSq);
        }
      }
    }
  });

  return grid;
}

export default function DensityHeatmap({
  shots,
  referenceShots = [],
  mode = 'relative',
  width = 750,
  height = 705
}) {
  const densityData = useMemo(() => {
    if (shots.length === 0) {
      return { grid: [], max: 0, gridSize: GRID_SIZE, cellWidth: 0, cellHeight: 0 };
    }

    const baselineShots = referenceShots.length > 0 ? referenceShots : shots;

    const gridSize = GRID_SIZE;
    const cellWidth = width / gridSize;
    const cellHeight = height / gridSize;
    const selectedGrid = createShotGrid(shots, width, height, gridSize);

    if (mode !== 'relative' && mode !== 'frequency') {
      const rawValues = selectedGrid.flat().filter(value => value > 0);
      const highDensity = quantile(rawValues, 0.98);
      const maxDensity = Math.max(highDensity, Math.max(...rawValues, 0) * 0.35);
      return { grid: selectedGrid, max: maxDensity, gridSize, cellWidth, cellHeight };
    }

    const referenceGrid = createShotGrid(baselineShots, width, height, gridSize);
    const selectedTotal = Math.max(shots.length, 1);
    const referenceTotal = Math.max(baselineShots.length, 1);
    const diffGrid = createEmptyGrid(gridSize);
    const absValues = [];

    for (let i = 0; i < gridSize; i++) {
      for (let j = 0; j < gridSize; j++) {
        const selectedShare = selectedGrid[i][j] / selectedTotal;
        const referenceShare = referenceGrid[i][j] / referenceTotal;
        const diff = selectedShare - referenceShare;
        diffGrid[i][j] = diff;
        if (Math.abs(diff) > 0) absValues.push(Math.abs(diff));
      }
    }

    const highDiff = quantile(absValues, 0.97);
    const maxDiff = Math.max(highDiff, Math.max(...absValues, 0) * 0.35);
    return { grid: diffGrid, max: maxDiff, gridSize, cellWidth, cellHeight };
  }, [shots, referenceShots, mode, width, height]);

  const turboColors = [
    [48, 18, 59],
    [62, 84, 163],
    [33, 145, 140],
    [94, 201, 98],
    [253, 231, 37],
    [234, 151, 36],
    [217, 72, 33],
    [122, 4, 3]
  ];

  const interpolateColors = (colors, normalizedValue) => {
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
  };

  const toRgb = (color) => `rgb(${color[0]}, ${color[1]}, ${color[2]})`;

  const getColor = (normalizedValue) => {
    return toRgb(interpolateColors(turboColors, normalizedValue));
  };

  const getRelativeColor = (normalizedValue) => {
    const clamped = Math.max(-1, Math.min(1, normalizedValue));
    const neutral = [255, 255, 255];
    const acbBlue = [72, 101, 129];
    const paletteColor = clamped < 0
      ? acbBlue
      : interpolateColors(turboColors, 0.45 + (0.55 * clamped));
    const t = Math.abs(clamped);

    const r = Math.round(neutral[0] + (paletteColor[0] - neutral[0]) * t);
    const g = Math.round(neutral[1] + (paletteColor[1] - neutral[1]) * t);
    const b = Math.round(neutral[2] + (paletteColor[2] - neutral[2]) * t);

    return `rgb(${r}, ${g}, ${b})`;
  };

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

  const { grid, max, cellWidth, cellHeight } = densityData;
  const isFrequency = mode === 'relative' || mode === 'frequency';
  const isComparison = isFrequency;

  return (
    <div className="w-full" style={{ maxWidth: width }}>
      <div className="overflow-x-auto pb-1">
        <div className="relative min-w-[560px]" style={{ aspectRatio: `${width} / ${height}` }}>
          <Court width={width} height={height} />

          <svg
            viewBox={`0 0 ${width} ${height}`}
            className="absolute inset-0 w-full h-full"
            role="img"
            aria-label={isComparison ? 'Frecuencia de tiro respecto a la liga' : 'Densidad de tiro'}
            style={{ pointerEvents: 'none' }}
          >
            {/* Add slight blur for smooth appearance */}
            <defs>
              <filter id="smooth-blur">
                <feGaussianBlur in="SourceGraphic" stdDeviation="0.8" />
              </filter>
            </defs>

            {/* Draw density heatmap */}
            <g filter="url(#smooth-blur)">
              {grid.map((row, i) =>
                row.map((value, j) => {
                  if (value === 0 || max === 0) return null;

                  const normalizedValue = isComparison
                    ? Math.max(-1, Math.min(1, value / max))
                    : Math.min(1, value / max);
                  const intensity = isComparison ? Math.abs(normalizedValue) : normalizedValue;
                  if (isComparison && intensity < MIN_RELATIVE_INTENSITY) return null;
                  if (!isComparison && normalizedValue < 0.01) return null;

                  const x = i * cellWidth;
                  const y = j * cellHeight;
                  const color = isComparison ? getRelativeColor(normalizedValue) : getColor(normalizedValue);

                  return (
                    <rect
                      key={`${i}-${j}`}
                      x={x}
                      y={y}
                      width={cellWidth + 0.5}
                      height={cellHeight + 0.5}
                      fill={color}
                      opacity={isComparison ? 0.2 + (intensity * 0.75) : 0.85}
                      style={{ strokeWidth: 0 }}
                    />
                  );
                })
              )}
            </g>
          </svg>
        </div>
      </div>

      {/* Legend */}
      <div className="mt-2 rounded border border-acb-200 bg-white p-2">
        <div className="flex flex-col sm:flex-row sm:items-center justify-between gap-2">
          <div className="flex items-center gap-2 flex-wrap">
            <span className="text-xs font-medium text-acb-700">
              {isComparison ? 'Frecuencia' : 'Densidad de tiro'}
            </span>
            {isComparison && (
              <span className="text-xs text-acb-500">(vs. media de la liga)</span>
            )}
            <span className="text-xs text-acb-500">
              ({shots.length} tiros)
            </span>
          </div>
          <div className="flex items-center gap-2 flex-wrap">
            {isComparison ? (
              <>
                <span className="text-xs text-acb-600">Menos freq.</span>
                <div className="flex h-3 w-36 rounded overflow-hidden border border-acb-200">
                  {Array(9).fill(0).map((_, i) => {
                    const value = (i / 4) - 1;
                    return (
                      <div
                        key={i}
                        style={{
                          backgroundColor: getRelativeColor(value),
                          width: `${100 / 9}%`,
                          height: '100%'
                        }}
                      />
                    );
                  })}
                </div>
                <span className="text-xs text-acb-600">Más freq.</span>
              </>
            ) : (
              <>
                <span className="text-xs text-acb-600">Baja</span>
                <div className="flex h-3 w-32 rounded overflow-hidden">
                  {Array(8).fill(0).map((_, i) => (
                    <div
                      key={i}
                      style={{
                        backgroundColor: getColor(i / 7),
                        width: '12.5%',
                        height: '100%'
                      }}
                    />
                  ))}
                </div>
                <span className="text-xs text-acb-600">Alta</span>
              </>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}
