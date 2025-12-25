# ACB Basketball Analytics Pipeline v2.0

This project contains the R code to scrap, clean, analyse and present the play-by-play data from Liga Endesa.

The project presents 4 main data outputs: Shot charts, lineup analysis, advanced team statistics and advanced player statistics.

## Quick Start

```r
# Set working directory to the pipeline folder
setwd("./acb_pipeline")

# Load the pipeline
source("run_pipeline.R")

# Process a single season (downloads data + full processing)
run_season_pipeline(2026)

# Or process all seasons at once - necessary to have the whole dataset
run_full_pipeline()

# Weekly update (skips scraping, just reprocesses)
quick_update(2026)
```

## Project Structure

```
OpenACB2.0/
└──openacb_api/
  ├── config/
  │   └── seasons.R
  ├── etl/
  │   ├── 01_scrape.R        # API data scraping
  │   ├── 02_clean.R         # PBP data cleaning
  │   ├── 03_variables.R     # Player on-court tracking
  │   ├── 04_team_stats.R    # Advanced team statistics
  │   └── 05_shot_charts.R   # Shot location processing
  ├── data/
  │   ├── raw/               # Raw play by play data
  │   │   ├── 2024-2025/
  │   │   ├── 2023-2024/
  │   │   └── ...
  │   └── processed/         # Cleaned & aggregated data
  │       ├── FinalData_2025.csv
  │       ├── PbP_adjustedData2025.Rds
  │       ├── TeamAdvancedStats2025.csv
  │       ├── ShotChartData2025.csv
  │       └── ...
  ├── run_pipeline.R         # Main orchestration script
  └── README.md
└── openacb_react/            
    ├── dist/                 
    │   └── data/             
    ├── export_to_react.R     
    └── launch_app.bat       

```

## Output

| File                            | Description                       | Use Case                  |
| ------------------------------- | --------------------------------- | ------------------------- |
| `FinalData_{year}.csv`        | Cleaned PBP with boxscore columns | Basic analysis            |
| `PbP_adjustedData{year}.Rds`  | PBP with player tracking columns  | Lineup analysis           |
| `TeamAdvancedStats{year}.csv` | Team-level advanced stats         | Team comparisons          |
| `ShotChartData{year}.csv`     | Shot locations with zones         | Shot chart visualizations |

## Individual Module Usage

Each ETL module can be run independently:

```r

# Load configuration first
source("config/seasons.R")

# Step 1: Scrape data from ACB API
source("etl/01_scrape.R")
scrape_season(2025)

# Step 2: Clean PBP data
source("etl/02_clean.R")
clean_pbp(2025)

# Step 3: Create player on-court variables (slow!)
source("etl/03_variables.R")
create_pbp_variables(2025)

# Step 4: Calculate team statistics
source("etl/04_team_stats.R")
calculate_team_stats(2025)

# Step 5: Process shot charts
source("etl/05_shot_charts.R")
process_shot_charts(2025)

# Step 6: Export lineup analysis
source("etl/06_lineup_analysis.R")
calculate_lineup_analysis(2025)
```

1. Run `run_season_pipeline(2026)`

## Statistics Calculated

### Team Statistics

- **Offensive/Defensive Rating**: Points per 100 possessions
- **True Shooting %**: Shooting efficiency accounting for 3s and FTs
- **Effective FG%**: FG% weighted for 3-pointers
- **Turnover Rate**: Turnovers per possession
- **Rebound Rates**: % of available rebounds captured
- **Assist Rate**: % of made FGs that are assisted
- **Block/Steal Rates**: Defensive metrics per opponent possession
- **Free Throw Rate**: FT attempts per FG attempt

### Shot Zones

| Zone                | Description                    |
| ------------------- | ------------------------------ |
| Zona (Restringida)  | Within 1.25m of basket         |
| Zona no restringida | Paint, outside restricted area |
| Triple Esquina      | Corner 3s                      |
| Triple Codo         | Wing/elbow 3s                  |
| Triple Centro       | Top of the key 3s              |
| Media Distancia *   | Mid-range areas                |

## 🔄 Weekly Update Workflow

Every Sunday after games:

```r
# 1. Scrape new games
scrape_season(2025)

# 2. Quick update (reprocess all)
quick_update(2025)
```

## 🔜 Next Steps (Phase 2-4)

1. **Phase 2**: Migrate to DuckDB for efficient querying
2. **Phase 3**: Build React frontend with interactive visualizations
3. **Phase 4**: Deploy to production (Vercel/Netlify + API)

---

*Created by Juan Torrecillas*
