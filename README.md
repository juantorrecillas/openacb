# OpenACB

OpenACB es una plataforma que recoge herramientas de análisis de estadísticas avanzadas para la Liga Endesa. Este proyecto compila datos de Play-by-Play de acb.com para presentar una serie de estadísticas y herramientas analíticas que pueden resultar útiles para aficionados o cuerpo técnico. En mi opinión, la herramienta tiene una serie de ventajas que la hacen interesante para el mencionado público. En primer lugar, es una herramienta rápida y ligera, con una gran cantidad de estadísticas disponible. Segundo, proporciona herramientas de analítica avanzada, como el análisis On/Off o las cartas de tiro, que pueden ser de gran interés tanto para analistas como para aficionados. En tercer lugar, es una app de código abierto. La mayoría de herramientas de analítica avanzada están escondidas bajo servicios de suscripción o muros de pago. Aquí tienes varios de esos servicios disponibles gratuitamente, sin anuncios y en código abierto. Sin ser una plataforma profesional, presenta una serie de herramientas de analítica avanzada que pueden ayudar a conocer mejor el perfil de equipos y jugadores a un nivel más técnico y avanzado.

## Estructura del Proyecto

```
OpenACB2.0/
├── openacb_api/
│   ├── config/
│   │   └── seasons.R
│   ├── etl/                          # Módulos ETL — ejecutar en orden por temporada
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
│   │   ├── 12_player_positions.R
│   │   ├── 13_clutch_stats.R
│   │   └── 14_player_impact.R
│   ├── runners/                      # Scripts de ejecución
│   │   ├── run_pipeline.R            # Pipeline completo para una o todas las temporadas
│   │   ├── update.R                  # Actualización semanal rápida (temporada actual)
│   │   ├── export_to_react.R         # Exportar todos los datos a JSON para React
│   │   ├── player_similarity.R       # Preparar perfiles para el nuevo análisis de similitud
│   │   ├── export_lineup_data.R      # Exportar datos de quintetos a JSON para React
│   │   ├── regen_team_stats.R        # Regenerar estadísticas de equipo
│   │   ├── regen_photos.R            # Regenerar fotos de jugadores
│   │   ├── run_clutch.R              # Regenerar estadísticas clutch
│   │   └── run_team_pace.R           # Regenerar splits por cuarto
│   └── data/
│       ├── raw/
│       └── processed/
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
    │   │   ├── ClutchStats.jsx
    │   │   └── About.jsx
    │   └── components/
    │       ├── Court.jsx
    │       ├── DensityHeatmap.jsx
    │       └── ZoneHeatmap.jsx
    ├── public/data/
    └── dist/
```

## Pipeline de Procesamiento (R)

### Actualización semanal

Desde la raíz del repositorio, ejecutar en R:

```r
source("openacb_api/runners/update.R")
```

También se puede ejecutar `Rscript openacb_api/runners/update.R` desde una terminal
con R en el `PATH`. El script resuelve su directorio de trabajo, selecciona la
temporada marcada como incompleta en `config/seasons.R` (actualmente 2026–27,
`season_id = 2027`) y utiliza `ACB_BEARER_TOKEN` del entorno o de
`openacb_api/.Renviron`.

Descarga solo partidos finalizados que aún no estén guardados, valida la cobertura,
actualiza las fichas de los jugadores antes de calcular sus estadísticas y regenera
los JSON de React, incluidos los quintetos y los datos por fase. Los errores de
descarga o procesamiento detienen la actualización. Después hay que compilar y
publicar el frontend para que los nuevos datos aparezcan en la web desplegada.

Las herramientas seleccionan por defecto la temporada más reciente disponible.
La similitud de jugadores requiere al menos 10 partidos y 10 minutos por partido;
por eso sus resultados tardan en aparecer al comienzo de una temporada.

El backend procesa los datos en 14 pasos:

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

`usg_simple` mide el porcentaje de acciones de finalización del equipo que corresponden
al jugador mientras está en pista:
`100 × (FGA + FT_trips + TOV) / (FGA_equipo_en_pista + FT_trips_equipo_en_pista + TOV_equipo_en_pista)`.
Usa las visitas a la línea registradas en el play-by-play, en lugar de aproximarlas
con `0,44 × FTA`. No reparte crédito por asistencias; el `usg` anterior sí lo hace
y sigue disponible. El denominador se limita a los partidos del jugador con ese equipo.

Incluye también métricas de impacto adaptadas de la NBA:

- **PER** (Hollinger): ajustado al ritmo de cada equipo y reescalado para que la media de la liga, ponderada por minutos, sea 15.
- **Game Score** (Hollinger): media por partido.
- **Win Shares** (método de Basketball Reference): ofensivas (`ows`), defensivas (`dws`), totales (`ws`) y por 40 minutos (`ws40`). Se adaptan a la ACB en tres puntos:
  - Los puntos por victoria salen de un exponente pitagórico estimado con datos ACB (10,3 en 2017-2026, frente a ~14 en la NBA).
  - La defensa usa los datos del rival mientras el jugador está en pista.
  - Las posesiones y el DRtg individual (`drtg`) se reajustan para que los jugadores de cada equipo sumen lo mismo que el equipo. Así, las Win Shares de un equipo se aproximan a sus victorias.

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

### 13_clutch_stats.R - Estadísticas clutch

Calcula estadísticas de equipos y jugadores en situaciones clutch: últimos 5 minutos del 4º cuarto o prórroga con diferencia de puntos ≤ 5. Incluye balance de victorias/derrotas en esos partidos, puntos anotados y encajados de media, y estadísticas de tiro en esos momentos.

### 14_player_impact.R - Impacto de jugador (tipo LEBRON/EPM)

Paso entre temporadas, posterior al 07. Estima el impacto de cada jugador en puntos por 100 posesiones respecto a un jugador medio (`impact` = `o_impact` + `d_impact`):

- Divide cada partido en tramos con los mismos 10 jugadores en pista. Los tiros libres se asignan al quinteto que estaba en pista cuando se cometió la falta.
- Ajuste por suerte: los puntos de triple y de tiro libre se sustituyen por el porcentaje esperado del tirador, regresado a la media de la liga.
- Prior de boxscore centrado por posición: los pesos se estiman dentro del propio modelo.
- Regresión ridge que agrupa todas las temporadas. La penalización se elige por validación cruzada por partidos.

También guarda el RAPM sin prior (`rapm`) y el componente de boxscore (`box`) para comparar. En temporadas en curso el índice se apoya casi por completo en el prior.

### Preparar el análisis de similitud

Desde la raíz del repositorio, ejecutar en R:

```r
source("openacb_api/runners/player_similarity.R")
```

Requiere `dplyr` (>= 1.1), `readr`, `here`, `cluster`, `ggplot2`, `mclust` y los archivos procesados
`PlayerStats{temporada}.Rds` y `PlayerImpact{temporada}.Rds`. Si las estadísticas
todavía no incluyen `usg_simple`, regenerarlas desde `openacb_api`:

```r
source("etl/07_player_stats.R")
source("config/seasons.R")
for (sid in SEASONS$season_id[SEASONS$season_id >= 2021]) calculate_player_stats(sid)
```

El bloque `cfg` del script permite elegir temporadas y mínimos de participación.
Por defecto toma temporadas desde 2020–21, con cobertura de zonas de tiro,
y exige 10 partidos y 10 minutos por partido, contando todas las fases.
Produce `output/player-similarity/player_profiles.csv`, con una fila por
jugador, equipo y temporada. Tanto los datos procesados como esta salida están
excluidos de Git; el script queda versionado.

`similarity_df` contiene los perfiles y `feature_cols` identifica 21 variables
candidatas: cinco frecuencias de zonas de tiro, porcentajes de canastas de dos
y tres asistidas, FTA/FGA, TS%, 3P%, FT%, usage simple, AST%, TOV%, ORB%, DRB%,
STL%, BLK%, altura e impacto ofensivo y defensivo ajustados. Las tasas se expresan
en puntos porcentuales; la altura, en metros; el impacto, en puntos por 100 posesiones.
El impacto es de jugador–temporada y se comparte entre sus equipos si hubo un traspaso.

También conserva identificadores, contexto y recuentos de tiro y posesiones para
valorar la fiabilidad de las métricas. La tabla observada mantiene `NA` cuando no
existe denominador. Para modelizar, suaviza 3P%, FT% y porcentajes de canastas
asistidas mediante un modelo beta-binomial: media agrupada de los jugadores
seleccionados en cada temporada y fuerza
del prior estimada con los datos. Sin intentos, usa ese prior, que representa falta
de información. Aplica `log1p` a BLK% por su asimetría y estandariza las 21 variables.

Compara K-means sobre las variables con K-means sobre los componentes que retienen
el 80% y el 90% de la varianza. Conserva las cinco frecuencias de zonas para mantener
una distancia simétrica; al sumar 100, aportan cuatro dimensiones independientes.
Los componentes no se reescalan a varianza uno. `cfg` fija la semilla, el número de
clusters de referencia (11) y los valores explorados (6, 8, 9, 10 y 11).
El gráfico clásico de codo (`elbow.png`, con datos en `elbow.csv`) calcula la suma
de cuadrados intracluster para 1–15 clusters en los tres espacios. En esta muestra,
el cambio de pendiente más claro está en 3; los 8 clusters anteriores siguen siendo
la especificación exploratoria usada en la revisión manual.

Añade una mezcla gaussiana de 11 componentes. Usa los 20 componentes de rango no
nulo del PCA, conservando toda la información y eliminando solo la dependencia
exacta de las zonas. Compara cinco estructuras de covarianza elipsoidal
(`EEE`, `VEE`, `EEV`, `VEV`, `VVV`), con el prior conjugado por defecto de `mclust`,
y selecciona la estructura por BIC. La búsqueda adicional de 6–12 componentes se
guarda en `gmm_bic.csv` y `gmm_bic.png`; no cambia la elección de 11 para el ejercicio.
`gmm_posteriors.csv` conserva las probabilidades de los componentes y `1 - max(p)` como
incertidumbre de asignación. Estas probabilidades no son proporciones de acciones
tácticas ni confianza calibrada en un rol verdadero. `gmm_model.Rds` guarda el ajuste.
`gmm_transitions.csv` sigue a los jugadores desde el ajuste de 9 componentes hasta
el actual, sin asumir que las etiquetas de los grupos sean equivalentes.
`gmm_component_comparison.csv` resume tamaños, BIC e incertidumbre de ambos ajustes.

En esta máquina, `mclust` 6.1.2 está instalado en `output/r-library`, que el script
añade a la búsqueda de paquetes. También puede instalarse normalmente en la
biblioteca de R. No se instalan paquetes al ejecutar el análisis.

Guarda distribuciones, correlaciones, priors, varianza y cargas del PCA, métricas,
perfiles y representantes de clusters y vecinos de 12 casos de revisión en la misma
carpeta de salida. Para comparar particiones usa también silhouette sobre el espacio
original común. Los vecinos se buscan dentro de la misma temporada, excluyendo al
propio jugador, con y sin restricción de cluster. GMM modifica la pertenencia a
grupos; las distancias euclídeas sin restricción siguen siendo las del modelo completo.
Para GMM, las tablas de vecinos también muestran el producto escalar de las
probabilidades de los dos jugadores como probabilidad de compartir componente
condicional al ajuste. El informe actual está en `output/player-similarity/gmm11_review.md`;
el ajuste anterior y sus tablas están en `gmm9/`. La revisión histórica de 8 clusters
está en `review.md` y sus tablas en `kmeans8/`.
El cálculo publicado sigue en `export_to_react.R`.

## Frontend

Aplicación web interactiva con 13 páginas organizadas en 5 secciones:

**Equipos**

- Estadísticas de Equipo: scatter plot con ORtg vs DRtg y métricas avanzadas
- Perfil de Equipo: estilo de juego y huella táctica de cada equipo
- Análisis de Partido: evolución del marcador dentro del partido y tabla clutch por equipos
- Four Factors: análisis de los cuatro factores por equipo
- Splits por Cuarto: estadísticas desagregadas por cuarto

**Jugadores**

- Estadísticas de Jugador: tablas ordenables con métricas avanzadas y percentiles
- Perfil de Jugador: estilo de juego y estadísticas históricas del jugador
- Similitud de Jugadores: cálculo de similitud estadística entre jugadores
- Estadísticas Clutch: rendimiento de jugadores en situaciones de alta presión

**Alineaciones**

- Análisis On/Off: impacto de combinaciones de jugadores en cancha
- Mejores Alineaciones: ranking de quintetos por Net Rating

**Tiro**

- Cartas de Tiro: todos los tiros de un jugador o equipo presentados de diversas formas
- Líderes por Zona: mejores anotadores por zona de la cancha

## SEO y apariencia en buscadores

El frontend define los títulos y descripciones de las herramientas en
`openacb_react/src/routing/manifest.js`. Al ejecutar `npm run build` desde
`openacb_react`, el paso `postbuild` (`scripts/build-seo.mjs`) genera:

- Un HTML con metadatos propios para cada una de las 18 páginas principales.
- Un HTML por jugador (`/jugadores/perfil/{slug}`, unos 1.400) y, para los equipos
  de la temporada en curso, su perfil, su análisis On/Off y su carta de tiro. Cada
  uno lleva título, descripción con sus números clave, canónica, migas de pan en
  JSON-LD y un resumen legible (texto y tablas) que la app mantiene en pantalla
  mientras cargan los datos. Los textos salen de `src/seo/entities.js`.
- Imágenes de vista previa de 1200×630 (`/og/...`, con `summary_large_image`):
  una por equipo actual, una por jugador de las tres últimas temporadas
  (`OG_IMAGE_SEASONS`) y una genérica para el resto. Se dibujan con `satori` y
  `@resvg/resvg-js` (`scripts/og-image.mjs`); si el renderizador falla, el build
  sigue y las páginas usan el logo.
- `robots.txt` y `sitemap.xml` con todas esas URLs.

Los slugs de jugador salen de la misma función que usa la app
(`src/utils/profileSlugs.js`), y el script comprueba cada ruta con el resolvedor de
URLs de la app: una ruta que la app redirigiría no se genera. Los equipos que ya no
están en la liga no tienen página propia, porque sin `?temporada=` la herramienta
abre la temporada actual. El resto de enlaces y las URLs antiguas siguen entrando
por el shell de React (`app.html`), que no lleva una canónica de portada.

Las URLs canónicas omiten los filtros de la interfaz (incluida la temporada) para
concentrar la indexación en cada herramienta. En una página pre-generada, React
conserva los metadatos del servidor; al navegar a otras páginas los actualiza y
marca las inexistentes como `noindex`.

Después de desplegar, comprobar que `/robots.txt` devuelve texto y `/sitemap.xml`
devuelve XML. En [Google Search Console](https://search.google.com/search-console),
verificar la propiedad `openacb.com`, enviar `https://openacb.com/sitemap.xml` e
inspeccionar la portada y las páginas principales con la prueba de URL publicada.
Comprobar el HTML renderizado y solicitar su indexación. Google decide el texto
del resultado y necesita volver a rastrear las páginas para reflejar cambios;
las descripciones no garantizan un snippet concreto ni una posición determinada.
Para revisar cómo se ve un enlace al compartirlo, usar el
[Sharing Debugger de Facebook](https://developers.facebook.com/tools/debug/)
(también refresca la caché de WhatsApp) o pegar el enlace en un mensaje privado.

## Imágenes para compartir

La carta de tiro, el radar de equipo, el radar del perfil de jugador, la
comparación de jugadores y el resumen On/Off del equipo tienen un botón de imagen
(`src/components/ChartImageButton.jsx`). Genera un PNG de 1200 px de ancho (a
doble resolución) con el título, las cifras clave, la leyenda y un pie con
`openacb.com`, igual desde el móvil que desde el escritorio
(`src/utils/chartImage.js`). En móviles que permiten compartir archivos abre el
menú de compartir del sistema; en el resto, lo descarga. El On/Off se exporta como
un gráfico de barras con la columna Impacto de la tabla
(`src/utils/onOffImage.js`). Las fotos y escudos de acb.com no se incluyen: su
servidor no permite usarlos en un canvas.

## Analítica

Vercel Web Analytics (`<Analytics />`) cuenta visitas, páginas, referentes,
países y dispositivos. Sus eventos personalizados solo existen en el plan Pro, así
que los eventos se envían a [Umami](https://umami.is) si se configura:

1. Crear una cuenta en Umami Cloud (el plan gratuito permite 100.000 eventos al
   mes) y añadir el sitio `openacb.com`.
2. En Vercel, añadir la variable de entorno `VITE_UMAMI_WEBSITE_ID` con el ID del
   sitio y volver a desplegar.

Sin esa variable no se carga nada. Con ella, `vite.config.js` añade el script de
Umami, que solo informa desde `openacb.com` (ni local ni previsualizaciones;
se puede cambiar con `VITE_UMAMI_DOMAINS`). La app registra una visita por ruta
(los cambios de filtros no cuentan como visitas nuevas) y estos eventos
(`src/utils/analytics.js`):

| Evento        | Datos                                    | Cuándo                              |
| ------------- | ---------------------------------------- | ----------------------------------- |
| `herramienta` | `id` de la ruta (`playerProfile`...)     | Al abrir otra herramienta           |
| `imagen`      | `grafico` y `accion` (`descarga`, `compartir`) | Al descargar o compartir una imagen |

El origen del tráfico sale del referente y de los parámetros `utm_*` de la
primera visita. Los enlaces que se abren desde WhatsApp suelen llegar sin
referente y cuentan como tráfico directo, salvo que lleven `utm_source`.

## Archivos de Datos Generados

| Archivo                         | Descripción                                         |
| ------------------------------- | ---------------------------------------------------- |
| `FinalData_{año}.csv`        | Play-by-play limpio con eventos boxscore             |
| `PbP_adjustedData{año}.Rds`  | PBP comprimido con tracking de jugadores             |
| `TeamAdvancedStats{año}.csv` | Ratings, cuatro factores y métricas avanzadas       |
| `ShotChartData{año}.csv`     | Coordenadas, zonas, distancias y resultados de tiros |
| `PlayerStats{año}.csv`       | Estadísticas de jugador, incluidos PER, Game Score y Win Shares |
| `PlayerImpact{año}.csv`      | Impacto de jugador (RAPM + prior de boxscore)        |
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
