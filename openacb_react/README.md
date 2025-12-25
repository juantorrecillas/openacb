# OpenACB React App

A modern basketball analytics dashboard for Liga Endesa (ACB) data.

## Features

- **Shot Charts**: Interactive visualization of shooting patterns with zone breakdowns
- **Team Statistics**: Scatter plots and sortable tables for team metrics (ORtg, DRtg, eFG%, etc.)
- **Player Statistics**: Searchable, sortable player stats with percentile coloring
- **Lineup Analysis**: On/Off court impact analysis (requires full PBP data)

## Quick Start (Production Build)

The `dist/` folder contains a pre-built production version. To view it:

```bash
# Using Python
cd dist
python -m http.server 8080

# Using Node
npx serve dist

# Using PHP
cd dist
php -S localhost:8080
```

Then open http://localhost:8080

## Development

```bash
# Install dependencies
npm install

# Start development server
npm run dev

# Build for production
npm run build
```

## Project Structure

```
openacb_react/
├── dist/               # Production build (ready to deploy)
├── src/
│   ├── App.jsx         # Main app with navigation
│   ├── index.css       # Tailwind CSS styles
│   ├── components/
│   │   └── Court.jsx   # Basketball court SVG component
│   └── pages/
│       ├── ShotCharts.jsx     # Shot visualization
│       ├── TeamStats.jsx      # Team statistics
│       ├── PlayerStats.jsx    # Player statistics
│       └── LineupAnalysis.jsx # On/Off analysis
├── data/
│   ├── shots.json      # Processed shot chart data
│   ├── teams.json      # Team statistics
│   └── players.json    # Player statistics
├── processData.mjs     # Script to convert PBP CSVs to JSON
└── package.json
```

## Data Processing

To update data with new games:

1. Add raw PBP CSV files to `../data/raw/2022-2023/`
2. Run the processing script:
   ```bash
   node processData.mjs
   ```
3. Rebuild the app: `npm run build`

## Deployment

### Vercel (Recommended)
```bash
npm install -g vercel
vercel
```

### Netlify
Drag the `dist/` folder to Netlify's deploy page.

### GitHub Pages
Use the `gh-pages` branch or configure Actions.

## Tech Stack

- **React 18** - UI framework
- **Vite** - Build tool
- **Tailwind CSS** - Styling
- **Recharts** - Scatter plots
- **Lucide React** - Icons

## Design Notes

- Inspired by Cleaning the Glass for the on/off analysis presentation
- Muted, professional color palette
- Light mode with subtle accent colors
- Monospace fonts for numeric data

## Next Steps

1. **Add full lineup analysis**: Requires `PbP_adjustedData.Rds` with `{player}_pista` columns
2. **Add DuckDB backend**: For more efficient data querying
3. **Add more visualizations**: Heatmaps, comparison charts
4. **Add season selector**: Support multiple seasons

## Credits

- Data source: jv.acb.com
- Created by Juan Torrecillas
