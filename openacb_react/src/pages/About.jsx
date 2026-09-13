import { useEffect, useRef, useState } from 'react'
import { ArrowRight, ArrowUpRight, ChevronDown, Code, Github, Globe, Mail } from 'lucide-react'
import styles from './About.module.css'

const contactInfo = {
  email: 'juan.torrecillas.jodar@gmail.com',
  website: 'https://juantorrecillas.es',
  github: 'https://github.com/juantorrecillas/openacb',
  x: 'https://x.com/juan_torrec',
}

const toolSummaries = [
  {
    title: 'Equipos',
    body: 'Las herramientas de equipo nos muestran estadísticas avanzadas para todos los equipos ACB en la temporada escogida. Es útil para obtener información rápida de un vistazo, identificar fortalezas o debilidades de algunos equipos, o para saber qué equipos de la liga destacan en las categorías en las que estemos interesados. Además de las estadísticas generales, también hay perfiles de equipo para entender mejor su estilo, una herramienta de análisis de partido para revisar la evolución del marcador y de las jugadas, y una sección de Four Factors para resumir tiro, pérdidas, rebote y tiros libres.',
  },
  {
    title: 'Jugadores',
    body: 'Estas herramientas muestran una compilación de estadísticas individuales, tanto básicas como avanzadas. Existen disponibles tablas para toda la liga, perfiles de jugador con información más detallada sobre producción, eficiencia y evolución, y estadísticas clutch para analizar el rendimiento en finales ajustados. La idea es que se pueda pasar de una visión general de la liga a una lectura más concreta de qué hace bien un jugador y en qué contexto aporta más.',
  },
  {
    title: 'Comparativas',
    body: 'Este bloque recoge herramientas pensadas para comparar y buscar relaciones entre jugadores y equipos. La similitud permite encontrar jugadores con un perfil estadístico parecido; la comparación directa sirve para poner dos jugadores frente a frente en una misma vista; y el cara a cara de equipos permite comparar dos equipos por métricas para entender mejor cómo encajan sus fortalezas y debilidades.',
  },
  {
    title: 'Alineaciones',
    body: 'Las estadísticas de alineación nos permiten analizar el desempeño de un equipo cuando ciertos jugadores o conjuntos de jugadores están (o no) en la cancha. Se puede utilizar para identificar combinaciones de jugadores con las que un equipo tiende a tener un mejor desempeño. Además del análisis On/Off, también hay un ranking de mejores alineaciones para encontrar quintetos que hayan producido especialmente bien en una temporada determinada.',
  },
  {
    title: 'Tiro',
    body: 'Las cartas de tiro muestran la distribución de tiros de campo para un jugador o equipo. Es una herramienta que nos permite, por ejemplo, conocer desde dónde un equipo tiene tendencia a tirar, desde dónde son más eficientes los lanzamientos de un jugador o si la selección de tiro puede ser mejorable. También se ofrecen líderes por zona para revisar los máximos anotadores y los anotadores más eficientes desde cada zona del campo en una temporada dada.',
  },
]

const definitions = [
  { term: 'Posesiones', body: 'FGA + FT_trip - ORB + TOV. Número de posesiones de un equipo.' },
  { term: 'ORtg', body: 'Rating Ofensivo = (Puntos / Posesiones) x 100. Puntos anotados por cada 100 posesiones.' },
  { term: 'ORtg individual', body: '(Puntos producidos/ Posesiones utilizadas) x 100. La métrica individual tiene en cuenta, no sólo los puntos anotados, sino los producidos a partir de asistencias y rebotes ofensivos. Las asistencias se dividen entre 0.3 puntos para el asistente y 0.7 para el anotador por cada punto anotado.' },
  { term: 'DRtg', body: 'Defensive Rating = (Puntos Rivales / Posesiones Rivales) x 100. Puntos permitidos por cada 100 posesiones.' },
  { term: 'NetRtg', body: 'Net Rating = ORtg - DRtg. Diferencial de eficiencia entre ataque y defensa.' },
  { term: 'TS%', body: 'True Shooting % = Puntos / (2 x (FGA + FT_trip)). Eficiencia de tiro incluyendo tiros libres y el valor extra de los triples.' },
  { term: 'eFG%', body: 'Effective Field Goal % = (FGM + 0.5 x 3PM) / FGA. Porcentaje de tiro ajustado por el valor de los triples.' },
  { term: '3PT Rating %', body: '3ptAtt% = 3PI / TCI. Qué porcentaje representan los triples sobre el total de tiros.' },
  { term: 'PER%', body: '% Pérdidas = Pérdidas / Posesiones. Porcentaje de posesiones que terminan en pérdida.' },
  { term: 'RO%', body: '% Rebote Ofensivo = ORB / (ORB + Opp_DRB). Porcentaje de rebotes ofensivos capturados.' },
  { term: 'RD%', body: '% Rebote Defensivo = DRB / (DRB + Opp_ORB). Porcentaje de rebotes defensivos capturados.' },
  { term: '% Tiros libres', body: 'FTM / FGA. Tiros libres anotados por cada tiro de campo intentado.' },
  { term: 'AST%', body: 'Tasa de Asistencias = Asistencias / FGM. Porcentaje de canastas asistidas.' },
  { term: 'Ritmo', body: 'Posesiones / Partidos. Ritmo de juego (posesiones por partido).' },
  { term: 'Usage', body: '% de Uso: Estimación del número de posesiones utilizadas por un jugador con respecto al total de posesiones disponibles cuando estaba en pista. Además de los tiros, tiros libres y pérdidas, también se consideran las asistencias como parte porcentual de una posesión finalizada.' },
  { term: 'On/Off', body: 'Compara las estadísticas del equipo cuando ciertos jugadores están en cancha y cuando no están. Se agregan las posesiones en cada escenario y se calculan ORtg/DRtg para describir la diferencia observada; no es una estimación causal del impacto individual.' },
]

function ExternalLink({ href, children, className, ariaLabel }) {
  return (
    <a href={href} target="_blank" rel="noopener noreferrer" className={className} aria-label={ariaLabel}>
      {children}
    </a>
  )
}

const chapters = [
  { id: 'proyecto', label: '¿Qué es openACB?' },
  { id: 'datos', label: 'Cómo se construyen los datos' },
  { id: 'herramientas', label: 'Herramientas de análisis' },
  { id: 'codigo', label: 'Código abierto' },
  { id: 'definiciones', label: 'Métricas e indicadores' },
  { id: 'sobremi', label: 'Contacto' },
]

export default function About() {
  const documentRef = useRef(null)
  const [activeChapter, setActiveChapter] = useState('proyecto')

  useEffect(() => {
    const sections = Array.from(documentRef.current.querySelectorAll(':scope > section'))
    let frame = 0
    const updateChapter = () => {
      frame = 0
      const current = sections.filter(section => section.getBoundingClientRect().top <= 120).at(-1)
      const atBottom = window.scrollY + window.innerHeight >= document.documentElement.scrollHeight - 2
      setActiveChapter(atBottom ? chapters.at(-1).id : current?.id || chapters[0].id)
    }
    const scheduleUpdate = () => {
      if (!frame) frame = window.requestAnimationFrame(updateChapter)
    }
    window.addEventListener('scroll', scheduleUpdate, { passive: true })
    window.addEventListener('resize', scheduleUpdate)
    const observer = new ResizeObserver(scheduleUpdate)
    observer.observe(documentRef.current)
    updateChapter()
    return () => {
      window.removeEventListener('scroll', scheduleUpdate)
      window.removeEventListener('resize', scheduleUpdate)
      observer.disconnect()
      window.cancelAnimationFrame(frame)
    }
  }, [])

  return (
    <div className={styles.page}>
      <header className={styles.dossierHero}>
        <div className={styles.heroCopy}>
          <h1>Información sobre el proyecto</h1>
          <p className={styles.heroStatement}>Datos, herramientas y método abiertos para entender mejor la Liga Endesa.</p>
          <p className={styles.heroSummary}>openACB reúne y publica datos y herramientas de análisis en código abierto para que cualquier persona pueda explorar y analizar el rendimiento en la Liga Endesa.</p>
          <ExternalLink href={contactInfo.github} className={styles.heroLink}>
            Ver el proyecto en GitHub
            <ArrowRight aria-hidden="true" size={17} />
          </ExternalLink>
        </div>

        <aside className={styles.ledger} aria-labelledby="ledger-title">
          <h2 id="ledger-title">Registro abierto</h2>
          <ol>
            <li>
              <span className={styles.ledgerNumber}>01</span>
              <a href="#datos"><strong>Datos</strong><span>Play-by-Play y estadísticas de acb.com</span></a>
            </li>
            <li>
              <span className={styles.ledgerNumber}>02</span>
              <a href="#herramientas"><strong>Herramientas</strong><span>Presentación de estadísticas y herramientas de análisis</span></a>
            </li>
            <li>
              <span className={styles.ledgerNumber}>03</span>
              <a href="#codigo"><strong>Código</strong><span>Repositorio en GitHub</span></a>
            </li>
            <li>
              <span className={styles.ledgerNumber}>04</span>
              <a href="#definiciones"><strong>Métricas</strong><span>Definiciones de estadísticos avanzados</span></a>
            </li>
          </ol>
        </aside>
      </header>

      <div className={styles.readingLayout}>
        <aside className={styles.chapterIndex}>
          <nav aria-label="Índice del dossier">
            <h2>Índice</h2>
            <ol>
              {chapters.map((chapter, index) => (
                <li key={chapter.id}>
                  <a
                    href={`#${chapter.id}`}
                    aria-current={activeChapter === chapter.id ? 'location' : undefined}
                    onClick={() => setActiveChapter(chapter.id)}
                  >
                    <span>{String(index + 1).padStart(2, '0')}</span>{chapter.label}
                  </a>
                </li>
              ))}
            </ol>
          </nav>
        </aside>

        <main ref={documentRef} className={styles.document}>
          <section id="proyecto" className={styles.projectSection} aria-labelledby="project-title">
            <h2 id="project-title"><span>01 ·</span>¿Qué es openACB?</h2>
            <div className={styles.prose}>
              <p>openACB es una plataforma que recoge herramientas de análisis de estadísticas avanzadas para la Liga Endesa. Este proyecto compila datos de Play-by-Play de acb.com para presentar una serie de estadísticas y herramientas analíticas que pueden resultar útiles para aficionados o cuerpos técnicos</p>
              <p>En mi opinión, la herramienta tiene una serie de ventajas que la hacen interesante para este público. En primer lugar, reúne en un mismo sitio una gran cantidad de estadísticas. Segundo, proporciona herramientas de analítica avanzada, como el análisis On/Off o las cartas de tiro, que pueden ser de gran interés tanto para analistas como para aficionados. En tercer lugar, es una app de código abierto. La mayoría de herramientas de analítica avanzada están escondidas bajo servicios de suscripción o muros de pago. Aquí tienes varios de esos servicios disponibles gratuitamente, sin anuncios y en código abierto.</p>
            </div>
          </section>

          <section id="datos" className={styles.dataSection} aria-labelledby="data-title">
            <h2 id="data-title"><span>02 ·</span>Cómo se construyen los datos</h2>
            <div className={styles.prose}>
              <p>En el repositorio de GitHub puedes encontrar tanto los datos como el código utilizado para descargar, tratar y presentar los mismos.</p>
            </div>
            <ExternalLink href={contactInfo.github} className={styles.textLink}>
              Consultar datos y código
              <ArrowUpRight aria-hidden="true" size={16} />
            </ExternalLink>
          </section>

          <section id="herramientas" className={styles.contentSection} aria-labelledby="tools-title">
            <h2 id="tools-title"><span>03 ·</span>Herramientas de análisis</h2>
            <div className={styles.toolList}>
              {toolSummaries.map((tool, index) => (
                <article key={tool.title}>
                  <span className={styles.toolNumber}>{String(index + 1).padStart(2, '0')}</span>
                  <h3>{tool.title}</h3>
                  <p>{tool.body}</p>
                </article>
              ))}
            </div>
          </section>

          <section id="codigo" className={styles.codeSection} aria-labelledby="code-title">
            <h2 id="code-title"><span>04 ·</span>Código abierto</h2>
            <div className={styles.prose}>
              <p>Sin ser una plataforma profesional, presenta una serie de herramientas de analítica avanzada que pueden ayudar a conocer mejor el perfil de equipos y jugadores a un nivel más técnico y avanzado. Y todo, repito, de forma gratuita.</p>
            </div>
            <p className={styles.projectConclusion}>Es, en definitiva, una herramienta cuyo único objetivo es ser utilizada.</p>
            <p className={styles.feedback}>Si te ha parecido útil, por favor, házmelo saber escribiéndome a mi correo: <a href={`mailto:${contactInfo.email}`}>{contactInfo.email}</a>.</p>
          </section>

          <section id="definiciones" className={styles.contentSection} aria-labelledby="definitions-title">
            <div className={styles.sectionIntro}>
              <h2 id="definitions-title"><span>05 ·</span>Métricas e indicadores</h2>
            </div>
            <div className={styles.definitionList}>
              {definitions.map((definition) => (
                <details key={definition.term}>
                  <summary>
                    <span>{definition.term}</span>
                    <ChevronDown aria-hidden="true" size={18} />
                  </summary>
                  <p>{definition.body}</p>
                </details>
              ))}
            </div>
          </section>

          <section id="sobremi" className={styles.authorSection} aria-labelledby="author-title">
            <h2 id="author-title">Sobre mí</h2>
            <p><strong>Juan Torrecillas.</strong> Soy Analista de Política Económica en el Joint Research Centre de la Comisión Europea. Como académico y aficionado al baloncesto, me alegra ver que el uso de la analítica y la estadística avanzada se hace más común. Con esta herramienta, tanto para aficionados como para amantes del dato, quiero hacer la perspectiva analítica accesible para todo el mundo. Que cada vez que alguien entre al sitio consiga llevarse algo nuevo aprendido en su mochila.</p>
            <div className={styles.socialLinks}>
              <ExternalLink href={contactInfo.x} ariaLabel="Perfil de Juan Torrecillas en X">
                <svg className={styles.xIcon} viewBox="0 0 24 24" aria-hidden="true">
                  <path d="M18.244 2.25h3.308l-7.227 8.26 8.502 11.24H16.17l-5.214-6.817L4.99 22.75H1.68l7.73-8.835L1.254 2.25H8.08l4.713 6.231zm-1.161 17.52h1.833L7.084 4.126H5.117z" />
                </svg>
                X
              </ExternalLink>
              <ExternalLink href={contactInfo.website}><Globe aria-hidden="true" size={17} />Web personal</ExternalLink>
              <ExternalLink href={contactInfo.github}><Github aria-hidden="true" size={17} />GitHub</ExternalLink>
              <a href={`mailto:${contactInfo.email}`}><Mail aria-hidden="true" size={17} />Email</a>
            </div>
          </section>

          <section id="contacto" className={styles.contactSection} aria-labelledby="contact-title">
            <h2 id="contact-title">Contacto</h2>
            <div className={styles.contactLinks}>
              <a href={`mailto:${contactInfo.email}`}>
                <Mail aria-hidden="true" size={19} />
                <span>{contactInfo.email}</span>
              </a>
              <ExternalLink href={contactInfo.website}>
                <Globe aria-hidden="true" size={19} />
                <span>{contactInfo.website}</span>
              </ExternalLink>
              <ExternalLink href={contactInfo.github}>
                <Code aria-hidden="true" size={19} />
                <span>Ver código en GitHub</span>
              </ExternalLink>
            </div>
            <p>Si tienes algún comentario, sugerencia o encuentras un error en la página, escríbeme un correo o abre un issue en GitHub.</p>
          </section>
        </main>
      </div>
    </div>
  )
}
