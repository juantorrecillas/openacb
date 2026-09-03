import { ArrowRight, ArrowUpRight } from 'lucide-react'

const definitions = [
  ['Posesiones', 'FGA + FT_trip - ORB + TOV. Número de posesiones de un equipo.'],
  ['ORtg', 'Rating Ofensivo = (Puntos / Posesiones) x 100. Puntos anotados por cada 100 posesiones.'],
  ['ORtg individual', 'Puntos producidos / posesiones utilizadas x 100. La métrica individual tiene en cuenta no sólo los puntos anotados, sino los producidos a partir de asistencias y rebotes ofensivos. Las asistencias se dividen entre 0.3 puntos para el asistente y 0.7 para el anotador por cada punto anotado.'],
  ['DRtg', 'Defensive Rating = (Puntos Rivales / Posesiones Rivales) x 100. Puntos permitidos por cada 100 posesiones.'],
  ['NetRtg', 'Net Rating = ORtg - DRtg. Diferencial de eficiencia entre ataque y defensa.'],
  ['TS%', 'True Shooting % = Puntos / (2 x (FGA + FT_trip)). Eficiencia de tiro incluyendo tiros libres y el valor extra de los triples.'],
  ['eFG%', 'Effective Field Goal % = (FGM + 0.5 x 3PM) / FGA. Porcentaje de tiro ajustado por el valor de los triples.'],
  ['3PT Rating %', '3ptAtt% = 3PI / TCI. Qué porcentaje representan los triples sobre el total de tiros.'],
  ['PER%', '% Pérdidas = Pérdidas / Posesiones. Porcentaje de posesiones que terminan en pérdida.'],
  ['RO%', '% Rebote Ofensivo = ORB / (ORB + Opp_DRB). Porcentaje de rebotes ofensivos capturados.'],
  ['RD%', '% Rebote Defensivo = DRB / (DRB + Opp_ORB). Porcentaje de rebotes defensivos capturados.'],
  ['% Tiros libres', 'FTM / FGA. Tiros libres anotados por cada tiro de campo intentado.'],
  ['AST%', 'Tasa de Asistencias = Asistencias / FGM. Porcentaje de canastas asistidas.'],
  ['Ritmo', 'Posesiones / Partidos. Ritmo de juego (posesiones por partido).'],
  ['Usage', '% de Uso: estimación del número de posesiones utilizadas por un jugador con respecto al total de posesiones disponibles cuando estaba en pista. Además de tiros, tiros libres y pérdidas, también se consideran las asistencias como parte porcentual de una posesión finalizada.'],
  ['On/Off', 'Compara las estadísticas del equipo cuando ciertos jugadores están en cancha y cuando no están. Se agregan las posesiones en cada escenario y se calculan ORtg/DRtg para describir la diferencia observada; no es una estimación causal del impacto individual.'],
]

const toolSummaries = [
  ['Equipos', 'Las herramientas de equipo muestran estadísticas avanzadas para todos los equipos ACB en la temporada escogida. Sirven para obtener información rápida de un vistazo, identificar fortalezas o debilidades y saber qué equipos destacan en las categorías de interés. También incluyen perfiles de equipo, análisis de partido y Four Factors.'],
  ['Jugadores', 'Estas herramientas reúnen estadísticas individuales básicas y avanzadas. Incluyen tablas para toda la liga, perfiles con detalle sobre producción, eficiencia y evolución, y estadísticas clutch para analizar el rendimiento en finales ajustados.'],
  ['Herramientas', 'Este bloque permite comparar y buscar relaciones entre jugadores y equipos. La similitud encuentra perfiles estadísticos parecidos; la comparación directa enfrenta dos jugadores; y el cara a cara compara dos equipos por métricas.'],
  ['Alineaciones', 'Las estadísticas de alineación analizan el desempeño de un equipo cuando ciertos jugadores o conjuntos de jugadores están o no en cancha. Incluyen análisis On/Off y rankings para identificar combinaciones especialmente productivas.'],
  ['Tiro', 'Las cartas de tiro muestran la distribución de lanzamientos de un jugador o equipo. Permiten estudiar tendencia, eficiencia y selección de tiro. Los líderes por zona recogen a los máximos anotadores y a los más eficientes desde cada zona del campo.'],
]

function About() {
  const contactInfo = {
    email: 'juan.torrecillas.jodar@gmail.com',
    website: 'https://juantorrecillas.es',
    GitHub: 'https://github.com/juantorrecillas/openacb',
  }

  const textLink = 'font-semibold text-acb-800 underline decoration-accent-500 decoration-2 underline-offset-4 transition-colors hover:text-accent-700'

  return (
    <div className="app-page mx-auto max-w-6xl">
      <header className="grid gap-8 border-b border-acb-300 pb-10 pt-2 lg:grid-cols-[minmax(0,1.35fr)_minmax(18rem,0.65fr)] lg:items-end">
        <div>
          <h1 className="font-display text-5xl font-semibold leading-none tracking-[-0.025em] text-acb-900 sm:text-6xl">Sobre openACB</h1>
          <p className="mt-5 max-w-3xl text-xl leading-relaxed text-acb-600">
            Una plataforma abierta de análisis y estadística avanzada para la Liga Endesa.
          </p>
        </div>
        <nav className="border-t-2 border-acb-800 py-4" aria-label="Enlaces de Juan Torrecillas">
          <a href="https://x.com/juan_torrec" target="_blank" rel="noopener noreferrer" className="flex justify-between border-b border-acb-200 py-2 text-sm font-semibold text-acb-700 hover:text-accent-700">
            X <ArrowUpRight className="h-4 w-4" aria-hidden="true" />
          </a>
          <a href={contactInfo.website} target="_blank" rel="noopener noreferrer" className="flex justify-between border-b border-acb-200 py-2 text-sm font-semibold text-acb-700 hover:text-accent-700">
            Web personal <ArrowUpRight className="h-4 w-4" aria-hidden="true" />
          </a>
          <a href={contactInfo.GitHub} target="_blank" rel="noopener noreferrer" className="flex justify-between border-b border-acb-200 py-2 text-sm font-semibold text-acb-700 hover:text-accent-700">
            GitHub <ArrowUpRight className="h-4 w-4" aria-hidden="true" />
          </a>
          <a href={`mailto:${contactInfo.email}`} className="flex justify-between py-2 text-sm font-semibold text-acb-700 hover:text-accent-700">
            Email <ArrowRight className="h-4 w-4" aria-hidden="true" />
          </a>
        </nav>
      </header>

      {/* proyecto y autor */}
      <section className="grid gap-10 border-b border-acb-300 py-12 lg:grid-cols-[minmax(0,1.3fr)_minmax(18rem,0.7fr)]">
        <div>
          <h2 className="font-display text-3xl font-semibold text-acb-900">¿Qué es openACB?</h2>
          <p className="mt-4 max-w-[72ch] text-base leading-7 text-acb-700">
            openACB recoge herramientas de análisis de estadísticas avanzadas para la Liga Endesa. El proyecto compila datos de Play-by-Play de acb.com para presentar estadísticas y herramientas analíticas útiles para aficionados o cuerpos técnicos. Reúne una gran cantidad de estadísticas y ofrece análisis On/Off, cartas de tiro y otros recursos avanzados que suelen estar detrás de servicios de suscripción o muros de pago. Aquí están disponibles gratuitamente, sin anuncios y en código abierto. En el repositorio de GitHub se pueden consultar tanto los datos como el código utilizado para descargar, tratar y presentarlos. <strong>Es, en definitiva, una herramienta cuyo único objetivo es ser utilizada.</strong>
          </p>
        </div>
        <aside className="border-t-4 border-accent-500 bg-white px-5 py-6 sm:px-6">
          <h2 className="font-display text-2xl font-semibold text-acb-900">Juan Torrecillas</h2>
          <p className="mt-3 text-sm leading-6 text-acb-600">
            Analista de Política Económica en el Joint Research Centre de la Comisión Europea, académico y aficionado al baloncesto. openACB busca hacer la perspectiva analítica accesible y que cada visita deje algo nuevo aprendido.
          </p>
        </aside>
      </section>

      {/* contacto */}
      <section className="grid gap-6 border-b border-acb-300 py-10 md:grid-cols-[12rem_1fr]">
        <h2 className="font-display text-3xl font-semibold text-acb-900">Contacto</h2>
        <div>
          <div className="grid gap-x-8 gap-y-3 sm:grid-cols-2">
            <a href={`mailto:${contactInfo.email}`} className={`${textLink} break-all`}>{contactInfo.email}</a>
            <a href={contactInfo.website} target="_blank" rel="noopener noreferrer" className={`${textLink} break-all`}>{contactInfo.website}</a>
            <a href={contactInfo.GitHub} target="_blank" rel="noopener noreferrer" className={textLink}>Ver código en GitHub</a>
          </div>
          <p className="mt-6 max-w-[70ch] text-base text-acb-600">
            Si tienes algún comentario, sugerencia o encuentras un error en la página, escríbeme un correo o abre un issue en GitHub.
          </p>
        </div>
      </section>

      {/* glosario y herramientas */}
      <div className="grid gap-12 py-12 xl:grid-cols-[minmax(0,1.15fr)_minmax(0,0.85fr)]">
        <section>
          <h2 className="font-display text-3xl font-semibold text-acb-900">Algunas definiciones</h2>
          <dl className="mt-5 border-t-2 border-acb-800">
            {definitions.map(([term, description]) => (
              <div key={term} className="grid gap-1 border-b border-acb-200 py-3 sm:grid-cols-[9rem_1fr] sm:gap-5">
                <dt className="font-mono text-xs font-medium text-acb-900">{term}</dt>
                <dd className="text-sm leading-6 text-acb-600">{description}</dd>
              </div>
            ))}
          </dl>
        </section>

        <section>
          <h2 className="font-display text-3xl font-semibold text-acb-900">Resumen de las herramientas</h2>
          <dl className="mt-5 border-t-2 border-acb-800">
            {toolSummaries.map(([term, description]) => (
              <div key={term} className="border-b border-acb-200 py-4">
                <dt className="font-display text-xl font-semibold text-acb-900">{term}</dt>
                <dd className="mt-1 text-sm leading-6 text-acb-600">{description}</dd>
              </div>
            ))}
          </dl>
        </section>
      </div>
    </div>
  )
}

export default About
