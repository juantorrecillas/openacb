import { Link } from 'react-router-dom'
import { ArrowRight, ArrowUpRight } from 'lucide-react'
import styles from './Home.module.css'
import HomeToolPreview from '../components/HomeToolPreview'
import {
  buildAboutPath, buildFourFactorsPath, buildGamesPath, buildLineupAnalysisPath,
  buildLineupRankingsPath, buildPlayerClutchPath, buildPlayerComparisonPath,
  buildPlayerProfilePath, buildPlayerSimilarityPath, buildPlayerStatsPath,
  buildShotChartsPath, buildTeamComparisonPath, buildTeamProfilePath,
  buildTeamStatsPath, buildZoneLeadersPath,
} from '../routing'

const categories = [
  {
    id: 'equipos', title: 'Equipos',
    tools: [
      { path: buildTeamStatsPath(), title: 'Estadísticas de Equipo', description: 'Rendimiento, ritmo y eficiencia.' },
      { path: buildTeamProfilePath(), title: 'Perfil de Equipo', description: 'Fortalezas, debilidades y estilo.' },
      { path: buildGamesPath(), title: 'Análisis de Partido', description: 'Evolución del marcador y jugadas.' },
      { path: buildFourFactorsPath(), title: 'Four Factors', description: 'Tiro, pérdidas, rebote y tiros libres.' },
    ],
  },
  {
    id: 'jugadores', title: 'Jugadores',
    tools: [
      { path: buildPlayerStatsPath(), title: 'Estadísticas de Jugador', description: 'Producción, eficiencia y métricas avanzadas.' },
      { path: buildPlayerProfilePath(), title: 'Perfil de Jugador', description: 'Perfil completo, estilo y evolución.' },
      { path: buildPlayerClutchPath(), title: 'Estadísticas clutch', description: 'Rendimiento en finales ajustados.' },
    ],
  },
  {
    id: 'herramientas', title: 'Comparativas',
    tools: [
      { path: buildPlayerSimilarityPath(), title: 'Similitud', description: 'Jugadores con perfiles similares.' },
      { path: buildPlayerComparisonPath(), title: 'Comparar Jugadores', description: 'Comparación directa entre jugadores.' },
      { path: buildTeamComparisonPath(), title: 'Cara a Cara', description: 'Compara dos equipos por métricas.' },
    ],
  },
  {
    id: 'alineaciones', title: 'Alineaciones',
    tools: [
      { path: buildLineupAnalysisPath(), title: 'Análisis On/Off', description: 'Impacto de jugadores y quintetos.' },
      { path: buildLineupRankingsPath(), title: 'Mejores Alineaciones', description: 'Quintetos con mayor rendimiento.' },
    ],
  },
  {
    id: 'tiro', title: 'Tiro',
    tools: [
      { path: buildShotChartsPath(), title: 'Cartas de tiro', description: 'Mapas de tiro por jugador o equipo.' },
      { path: buildZoneLeadersPath(), title: 'Líderes por zona', description: 'Anotadores y eficiencia por zona.' },
    ],
  },
]

function CategoryBlock({ category }) {
  return (
    <section id={category.id} aria-labelledby={`category-${category.id}`} className={styles.category}>
      <div className={styles.categoryIntro}>
        <h3 id={`category-${category.id}`}>{category.title}</h3>
      </div>
      <ul className={styles.toolList}>
        {category.tools.map((tool) => (
          <li key={tool.path}>
            <Link to={tool.path} className={styles.toolLink}>
              <span>
                <span className={styles.toolTitle}>{tool.title}</span>
                <span className={styles.toolDescription}>{tool.description}</span>
              </span>
              <ArrowUpRight aria-hidden="true" size={18} />
            </Link>
          </li>
        ))}
      </ul>
    </section>
  )
}

export default function Home() {
  return (
    <div className={styles.page}>
      <section className={styles.hero} aria-labelledby="home-title">
        <div className={styles.heroMain}>
          <div className={styles.intro}>
            <h1 id="home-title">La ACB,<br />al detalle.</h1>
            <p className={styles.introCopy}>Estadísticas avanzadas y herramientas para entender la Liga Endesa.</p>
            <div className={styles.heroActions}>
              <a href="#herramientas-openacb" className={styles.primaryAction}>Explorar herramientas <ArrowRight aria-hidden="true" size={20} /></a>
              <Link to={buildAboutPath()} className={styles.textAction}>Conocer el proyecto <ArrowRight aria-hidden="true" size={18} /></Link>
            </div>
          </div>
          <HomeToolPreview />
        </div>
        <nav className={styles.familyRunway} aria-label="Familias de herramientas">
          {categories.map((category) => (
            <a key={category.id} href={`#${category.id}`} className={styles.familyLink}>
              <span className={styles.familyTitle}>{category.title}</span>
              <ArrowRight aria-hidden="true" size={20} />
            </a>
          ))}
        </nav>
      </section>

      <section id="herramientas-openacb" className={styles.directory} aria-labelledby="tools-title">
        <div className={styles.directoryHeading}>
          <div>
            <h2 id="tools-title">Estadísticas<br />y herramientas.</h2>
          </div>
        </div>
        <div className={styles.categories}>
          {categories.map((category) => <CategoryBlock key={category.id} category={category} />)}
        </div>
      </section>

      <section className={styles.project} aria-labelledby="project-title">
        <div className={styles.projectInner}>
          <div>
            <h2 id="project-title">Sobre openACB</h2>
          </div>
          <div className={styles.projectBody}>
            <p>openACB es un proyecto gratuito y de código abierto.</p>
            <div className={styles.projectLinks}>
              <Link to={buildAboutPath()}>Sobre openACB <ArrowRight aria-hidden="true" size={17} /></Link>
              <a href="https://github.com/juantorrecillas/openacb" target="_blank" rel="noopener noreferrer">Código en GitHub <ArrowUpRight aria-hidden="true" size={17} /></a>
            </div>
          </div>
        </div>
      </section>
    </div>
  )
}
