import { useMemo } from 'react';
import Court from './Court';

export default function DensityHeatmap({ shots, width = 750, height = 705 }) {
  // Create a 2D density grid similar to ggplot2's geom_density_2d_filled
  const densityData = useMemo(() => {
    if (shots.length === 0) return { grid: [], max: 0, levels: [] };

    // Transform court coordinates to SVG coordinates
    const scale = width / 15;
    const shotsWithSVG = shots.map(shot => ({
      x: (shot.x + 7.5) * scale,
      y: (-shot.y) * scale
    }));

    // Create a very fine-grained grid for smooth, continuous appearance
    const gridSize = 150; // Very high resolution grid (150x150 = 22,500 cells)
    const cellWidth = width / gridSize;
    const cellHeight = height / gridSize;

    // Initialize grid
    const grid = Array(gridSize).fill(null).map(() => Array(gridSize).fill(0));

    // Kernel density estimation with Gaussian kernel
    const bandwidth = Math.min(width, height) / 10; // Adjusted smoothing parameter for smooth transitions

    // For each grid cell, calculate density
    for (let i = 0; i < gridSize; i++) {
      for (let j = 0; j < gridSize; j++) {
        const cellX = (i + 0.5) * cellWidth;
        const cellY = (j + 0.5) * cellHeight;

        // Calculate density at this point using Gaussian kernel
        let density = 0;
        shotsWithSVG.forEach(shot => {
          const dx = (shot.x - cellX) / bandwidth;
          const dy = (shot.y - cellY) / bandwidth;
          const distSq = dx * dx + dy * dy;
          // Gaussian kernel
          density += Math.exp(-0.5 * distSq);
        });

        grid[i][j] = density;
      }
    }

    // Find max density for normalization
    const maxDensity = Math.max(...grid.flat());

    // Create density levels for contour-like visualization (similar to filled contours)
    const numLevels = 15; // bins=15 in the Shiny app
    const levels = Array(numLevels).fill(0).map((_, i) => ({
      level: i,
      threshold: (i / numLevels) * maxDensity
    }));

    return { grid, max: maxDensity, levels, gridSize, cellWidth, cellHeight };
  }, [shots, width, height]);

  // Viridis "turbo" color scale approximation
  const getColor = (normalizedValue) => {
    // Turbo colormap approximation (blue -> cyan -> green -> yellow -> orange -> red)
    const colors = [
      [48, 18, 59],     // dark blue
      [62, 84, 163],    // blue
      [33, 145, 140],   // cyan
      [94, 201, 98],    // green
      [253, 231, 37],   // yellow
      [234, 151, 36],   // orange
      [217, 72, 33],    // red-orange
      [122, 4, 3]       // dark red
    ];

    const idx = normalizedValue * (colors.length - 1);
    const lower = Math.floor(idx);
    const upper = Math.ceil(idx);
    const t = idx - lower;

    if (upper >= colors.length) {
      const c = colors[colors.length - 1];
      return `rgb(${c[0]}, ${c[1]}, ${c[2]})`;
    }

    const c1 = colors[lower];
    const c2 = colors[upper];
    const r = Math.round(c1[0] + (c2[0] - c1[0]) * t);
    const g = Math.round(c1[1] + (c2[1] - c1[1]) * t);
    const b = Math.round(c1[2] + (c2[2] - c1[2]) * t);

    return `rgb(${r}, ${g}, ${b})`;
  };

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

  const { grid, max, gridSize, cellWidth, cellHeight } = densityData;

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
        {/* Add slight blur for smooth appearance */}
        <defs>
          <filter id="smooth-blur">
            <feGaussianBlur in="SourceGraphic" stdDeviation="0.8" />
          </filter>
        </defs>

        {/* Draw density heatmap */}
        <g filter="url(#smooth-blur)">
        {grid.map((row, i) =>
          row.map((density, j) => {
            if (density === 0) return null;

            const normalizedDensity = density / max;
            // Only show cells with significant density
            if (normalizedDensity < 0.01) return null;

            const x = i * cellWidth;
            const y = j * cellHeight;
            const color = getColor(normalizedDensity);

            return (
              <rect
                key={`${i}-${j}`}
                x={x}
                y={y}
                width={cellWidth + 0.5}
                height={cellHeight + 0.5}
                fill={color}
                opacity={0.85}
                style={{ strokeWidth: 0 }}
              />
            );
          })
        )}
        </g>
      </svg>

      {/* Legend */}
      <div className="absolute bottom-2 left-2 right-2 bg-white/95 p-2 rounded border border-acb-300">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-2">
            <span className="text-xs font-medium text-acb-700">Shot Density</span>
            <span className="text-xs text-acb-500">
              ({shots.length} shots)
            </span>
          </div>
          <div className="flex items-center gap-2">
            <span className="text-xs text-acb-600">Low</span>
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
            <span className="text-xs text-acb-600">High</span>
          </div>
        </div>
      </div>
    </div>
  );
}
