# OpenACB

OpenACB es una plataforma que recoge herramientas de análisis de estadísticas avanzadas para la Liga Endesa. Este proyecto compila datos de Play-by-Play de acb.com para presentar una serie de estadísticas y herramientas analíticas que pueden resultar útiles para aficionados o cuerpo técnico. En mi opinión, la herramienta tiene una serie de ventajas que la hacen interesante para el mencionado público. En primer lugar, es una herramienta rápida y ligera, con una gran cantidad de estadísticas disponible. Segundo, proporciona herramientas de analítica avanzada, como el análisis On/Off o las cartas de tiro, que pueden ser de gran interés tanto para analistas como para aficionados. En tercer lugar, es una app de código abierto. La mayoría de herramientas de analítica avanzada están escondidas bajo servicios de suscripción o muros de pago. Aquí tienes varios de esos servicios disponibles gratuitamente, sin anuncios y en código abierto. Sin ser una plataforma profesional, presenta una serie de herramientas de analítica avanzada que pueden ayudar a conocer mejor el perfil de equipos y jugadores a un nivel más técnico y avanzado.

## Estructura del Proyecto

```
OpenACB2.0/
├── openacb_api/
│   ├── config/
│   │   └── seasons.R
│   ├── etl/
│   │   ├── 01_scrape.R
│   │   ├── 02_clean.R
│   │   ├── 03_variables.R
│   │   ├── 04_team_stats.R
│   │   ├── 05_shot_charts.R
│   │   ├── 06_lineup_analysis.R
│   │   ├── 07_player_stats.R
│   │   ├── 08_game_flow.R
│   │   ├── 09_team_pace.R
│   │   ├── 10_team_logos.R
│   │   ├── 11_player_photos.R
│   │   └── 12_player_positions.R
│   ├── data/
│   │   ├── raw/
│   │   └── processed/
│   ├── run_pipeline.R
│   ├── export_to_react.R
│   └── export_lineup_data.R
│
└── openacb_react/
    ├── src/
    │   ├── App.jsx
    │   ├── pages/
    │   │   ├── Home.jsx
    │   │   ├── TeamStats.jsx
    │   │   ├── TeamFingerprint.jsx
    │   │   ├── GameFlow.jsx
    │   │   ├── TeamPace.jsx
    │   │   ├── FourFactors.jsx
    │   │   ├── PlayerStats.jsx
    │   │   ├── PlayerProfile.jsx
    │   │   ├── PlayerSimilarity.jsx
    │   │   ├── LineupAnalysis.jsx
    │   │   ├── LineupRankings.jsx
    │   │   ├── ShotCharts.jsx
    │   │   ├── ZoneLeaders.jsx
    │   │   └── About.jsx
    │   └── components/
    │       ├── Court.jsx
    │       ├── DensityHeatmap.jsx
    │       └── ZoneHeatmap.jsx
    ├── public/data/
    └── dist/
```

## Pipeline de Procesamiento (R)

El backend procesa los datos en 12 pasos:

### 01_scrape.R - Descarga de datos

Descarga play-by-play desde `api2.acb.com`. Itera sobre todas las jornadas y partidos finalizados y genera un CSV por partido en `data/raw/{temporada}/`.

### 02_clean.R - Limpieza y eventos boxscore

Combina todos los partidos de una temporada. Crea columnas de eventos (T2A/T2F, T3A/T3F, reb_def, reb_of, asis, perdida, etc.), estandariza nombres de equipos y calcula contexto de tiro (transición, segunda oportunidad).

### 03_variables.R - Tracking de jugadores

Identifica qué 5 jugadores están en cancha en cada jugada y crea columnas binarias `{jugador}_{license}_pista`.

### 04_team_stats.R - Estadísticas de equipos

Calcula ratings (ORtg, DRtg), cuatro factores (eFG%, TOV%, ORB%/DRB%, FT Rate) y estadísticas por partido para cada equipo y sus rivales.

### 05_shot_charts.R - Procesamiento de tiros

Transforma coordenadas de API a coordenadas FIBA (-7.3 a 7.3m), calcula distancia y ángulo al aro, y clasifica cada tiro en zonas (zona restringida, triple esquina, triple codo, media distancia, etc.).

### 06_lineup_analysis.R - Análisis de quintetos

Analiza todas las combinaciones de jugadores: individuales, parejas, tríos y quintetos completos. Para cada combinación calcula minutos, ORtg, DRtg, Net Rating y cuatro factores, con comparación On/Off court.

### 07_player_stats.R - Estadísticas de jugadores

Minutos jugados con tracking del paso 3, estadísticas por partido y totales, percentiles vs liga y métricas de eficiencia individuales.

### 08_game_flow.R - Evolución del marcador

Calcula la evolución del marcador a lo largo de cada partido.

### 09_team_pace.R - Splits por cuarto

Estadísticas desagregadas por cuarto para cada equipo, incluyendo ritmo de juego y splits ofensivos/defensivos.

### 10_team_logos.R - Logos de equipos

Genera un JSON con las URLs de logos de cada equipo.

### 11_player_photos.R - Fotos de jugadores

Genera un JSON con las URLs de fotos de perfil de cada jugador.

### 12_player_positions.R - Posiciones

Scrapea el HTML de acb.com para extraer información básica sobre cada jugador: altura, posición y edad.

## Frontend

Aplicación web interactiva con 13 páginas organizadas en 4 secciones:

**Equipos**

- Estadísticas de Equipo: scatter plot con ORtg vs DRtg y métricas avanzadas
- Perfil de Equipo: Estilo de juego de cada equipo
- Análisis de Partido: Evolución del marcador dentro del partido
- Four Factors: Análisis de los cuatro factores por equipo

**Jugadores**

- Estadísticas de Jugador: tablas ordenables con métricas avanzadas y percentiles
- Perfil de Jugador: Estilo de juego y estadísticas históricas del jugador
- Similitud: cálculo de similitud estadística de jugadores

**Alineaciones**

- Análisis On/Off: impacto de combinaciones de jugadores en cancha
- Mejores Alineaciones: ranking de quintetos por Net Rating

**Tiro**

- Cartas de Tiro: Todos los tiros de un jugador o equipo presentados de diversas formas
- Líderes por Zona: mejores anotadores por zona de la cancha

## Archivos de Datos Generados

| Archivo                         | Descripción                                         |
| ------------------------------- | ---------------------------------------------------- |
| `FinalData_{año}.csv`        | Play-by-play limpio con eventos boxscore             |
| `PbP_adjustedData{año}.Rds`  | PBP comprimido con tracking de jugadores             |
| `TeamAdvancedStats{año}.csv` | Ratings, cuatro factores y métricas avanzadas       |
| `ShotChartData{año}.csv`     | Coordenadas, zonas, distancias y resultados de tiros |
| `lineups-{año}.json`         | Análisis completo de quintetos                      |
| `shots-{año}.json`           | Datos de tiro para el frontend                       |
| `gameflow-{año}.json`        | Evolución de marcador por partido                   |
| `teampace-{año}.json`        | Splits por cuarto por equipo                         |
| `teams.json`                  | Estadísticas de equipo agregadas                    |
| `players.json`                | Estadísticas de jugador agregadas                   |
| `similarity.json`             | Matriz de similitud entre jugadores                  |
| `team-logos.json`             | URLs de logos de equipos                             |
| `player-photos.json`          | URLs de fotos de jugadores                           |
| `player-bio.json`             | Datos biográficos de jugadores                      |

*creado por juan torrecillas 🍋*
