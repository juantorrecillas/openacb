import { Mail, Globe, Code, Info, Heart, Twitter, Github } from 'lucide-react'

function About() {
  const contactInfo = {
    name: "Juan Torrecillas",
    email: "juan.torrecillas.jodar@gmail.com",
    website: "https://juantorrecillas.es",
    GitHub: "https://github.com/juantorrecillas/openacb"  
  }

  return (
    <div className="max-w-4xl mx-auto">

{/* Sobre mí */}
      <div className="bg-white rounded-lg shadow-sm border border-acb-200 p-8 mb-6">
        <h2 className="text-2xl font-semibold text-acb-900 mb-4">Sobre mí</h2>
        <div className="text-base text-acb-700 space-y-2">
          <p>
            <strong> juan torrecillas.</strong> Soy Analista de Política Económica en el Joint Research Centre de la Comisión Europea. Como académico y aficionado al baloncesto, me alegra ver que el uso de la analítica y la estadística avanzada se hace más común. Con esta herramienta, tanto para aficionados como para amantes del dato, quiero hacer la perspectiva analítica accesible para todo el mundo. Que cada vez que alguien entre al sitio consiga llevarse algo nuevo aprendido en su mochila.
          </p>
        </div>
        <div className="flex items-center gap-4 mt-4 pt-4 border-t border-acb-200">
          <a href="https://x.com/juan_torrec" target="_blank" rel="noopener noreferrer" className="flex items-center gap-2 text-acb-600 hover:text-accent-500 transition-colors">
            {/* Replaced <Twitter /> with the official X SVG logo */}
            <svg className="w-5 h-5" fill="currentColor" viewBox="0 0 24 24" aria-hidden="true">
              <path d="M18.244 2.25h3.308l-7.227 8.26 8.502 11.24H16.17l-5.214-6.817L4.99 22.75H1.68l7.73-8.835L1.254 2.25H8.08l4.713 6.231zm-1.161 17.52h1.833L7.084 4.126H5.117z" />
            </svg>
            <span className="text-sm"></span>
          </a>
          <a href={contactInfo.website} target="_blank" rel="noopener noreferrer" className="flex items-center gap-2 text-acb-600 hover:text-lemon transition-colors">
            <Globe className="w-5 h-5" />
            <span className="text-sm">Web personal</span>
          </a>
          <a href={contactInfo.GitHub} target="_blank" rel="noopener noreferrer" className="flex items-center gap-2 text-acb-600 hover:text-accent-500 transition-colors">
            <Github className="w-5 h-5" />
            <span className="text-sm">GitHub</span>
          </a>
          <a href={`mailto:${contactInfo.email}`} className="flex items-center gap-2 text-acb-600 hover:text-accent-500 transition-colors">
            <Mail className="w-5 h-5" />
            <span className="text-sm">Email</span>
          </a>
        </div>
      </div>
      <div className="bg-white rounded-lg shadow-sm border border-acb-200 p-8 mb-6">
        <div className="flex items-center gap-4 mb-4">
          <div className="p-3 bg-acb-100 rounded-lg">
            <Info className="w-8 h-8 text-acb-700" />
          </div>
          <div>
            <h1 className="text-3xl font-bold text-acb-900">¿Qué es openACB?</h1>
          </div>
        </div>

          <div className="prose prose-slate max-w-none">
            <p className="text-acb-700 leading-relaxed">
          openACB es una plataforma que recoge herramientas de análisis de estadísticas avanzadas para la Liga Endesa.
          Este proyecto compila datos de Play-by-Play de acb.com para presentar una serie de estadísticas y herramientas analíticas que pueden resultar útiles para aficionados o cuerpo técnico. En mi opinión, la herramienta tiene una serie de ventajas que la hacen interesante para el mencionado público. En primer lugar, es una herramienta rápida y ligera, con una gran cantidad de estadísticas disponible. Segundo, proporciona herramientas de analítica avanzada, como el análisis On/Off o las cartas de tiro, que pueden ser de gran interés tanto para analistas como para aficionados. En tercer lugar, es una app de código abierto. La mayoría de herramientas de analítica avanzada están escondidas bajo servicios de suscripción o muros de pago.  Aquí tienes varios de esos servicios disponibles gratuitamente, sin anuncios y en código abierto. Sin ser una plataforma profesional, presenta una serie de herramientas de analítica avanzada que pueden ayudar a conocer mejor el perfil de equipos y jugadores a un nivel más técnico y avanzado. Y todo, repito, de forma gratuita. En el repositorio de GitHub puedes encontrar tanto los datos como el código utilizado para descargar, tratar y presentar los mismos. <strong>Es, en definitiva, una herramienta cuyo único objetivo es ser utilizada.</strong> Si te ha parecido útil, por favor, házmelo saber escribiéndome a mi correo: <a href={`mailto:${contactInfo.email}`} className="text-acb-600 hover:text-accent-500 underline">{contactInfo.email}</a>.
            </p>
            <p className="text-acb-700 leading-relaxed"> 
            </p>
          </div>
        </div>

      

      {/* Contacto */}
      <div className="bg-white rounded-lg shadow-sm border border-acb-200 p-8 mb-6">
        <h2 className="text-2xl font-semibold text-acb-900 mb-4">Contacto</h2>

        <div className="space-y-4">
          <div className="flex items-center gap-3 text-acb-700">
            <Mail className="w-5 h-5 text-acb-400" />
            <a
              href={`mailto:${contactInfo.email}`}
              className="hover:text-accent-500 hover:underline text-base"
            >
              {contactInfo.email}
            </a>
          </div>

          <div className="flex items-center gap-3 text-acb-700">
            <Globe className="w-5 h-5 text-acb-400" />
            <a
              href={contactInfo.website}
              target="_blank"
              rel="noopener noreferrer"
              className="hover:text-lemon hover:underline text-base"
            >
              {contactInfo.website}
            </a>
          </div>

          <div className="flex items-center gap-3 text-acb-700">
            <Code className="w-5 h-5 text-acb-400" />
            <a
              href={contactInfo.GitHub}
              target="_blank"
              rel="noopener noreferrer"
              className="hover:text-accent-500 hover:underline text-base"
            >
              Ver código en GitHub
            </a>
          </div>
        </div>

        <div className="mt-6 pt-6 border-t border-acb-200">
          <p className="text-base text-acb-600">
            Si tienes algún comentario, sugerencia, o encontraste un bug en la página, escríbeme un correo o abre un issue en GitHub!
          </p>
        </div>
      </div>

      {/* Glosario y Herramientas */}
      <div className="grid md:grid-cols-2 gap-6 mb-6">

        {/* Glosario */}
        <div className="bg-white rounded-lg shadow-sm border border-acb-200 p-6">
          <h2 className="text-xl font-semibold text-acb-900 mb-4 flex items-center gap-2">
            Algunas definiciones
          </h2>
          <div className="space-y-3 text-sm text-acb-700">
            <div>
              <span className="font-medium">Posesiones:</span> FGA + FT_trip - ORB + TOV. Número de posesiones de un equipo.
            </div>
            <div>
              <span className="font-medium">ORtg:</span> Rating Ofensivo = (Puntos / Posesiones) x 100. Puntos anotados por cada 100 posesiones.
            </div>
                        <div>
              <span className="font-medium">ORtg individual:</span> (Puntos producidos/ Posesiones utilizadas) x 100. La métrica individual tiene en cuenta, no sólo los puntos anotados, sino los producidos a partir de asistencias y rebotes ofensivos. Las asistencias se dividen entre 0.3 puntos para el asistente y 0.7 para el anotador por cada punto anotado.
            </div>
            <div>
              <span className="font-medium">DRtg:</span> Defensive Rating = (Puntos Rivales / Posesiones Rivales) x 100. Puntos permitidos por cada 100 posesiones.
            </div>
            <div>
              <span className="font-medium">NetRtg:</span> Net Rating = ORtg - DRtg. Diferencial de eficiencia entre ataque y defensa.
            </div>
            <div>
              <span className="font-medium">TS%:</span> True Shooting % = Puntos / (2 x (FGA + FT_trip)). Eficiencia de tiro incluyendo tiros libres y el valor extra de los triples.
            </div>
            <div>
              <span className="font-medium">eFG%:</span> Effective Field Goal % = (FGM + 0.5 x 3PM) / FGA. Porcentaje de tiro ajustado por el valor de los triples.
            </div>
            <div>
              <span className="font-medium">3PT Rating %:</span> 3ptAtt% = 3PI / TCI. Qué porcentaje representan los triples sobre el total de tiros.</div>
            <div>
              <span className="font-medium">PER%:</span> % Pérdidas = Pérdidas / Posesiones. Porcentaje de posesiones que terminan en pérdida.
            </div>
            <div>
              <span className="font-medium">RO%:</span> % Rebote Ofensivo = ORB / (ORB + Opp_DRB). Porcentaje de rebotes ofensivos capturados.
            </div>
            <div>
              <span className="font-medium">RD%:</span> % Rebote Defensivo = DRB / (DRB + Opp_ORB). Porcentaje de rebotes defensivos capturados.
            </div>
            <div>
              <span className="font-medium">% Tiros Libres:</span> FTA / FGA. Tiros libres intentados por cada tiro de campo.
            </div>
            <div>
              <span className="font-medium">AST%:</span> Tasa de Asistencias = Asistencias / FGM. Porcentaje de canastas asistidas.
            </div>
            <div>
              <span className="font-medium">Ritmo:</span> Posesiones / Partidos. Ritmo de juego (posesiones por partido).
            </div>
            <div>
              <span className="font-medium">Usage:</span> % de Uso: Estimación del número de posesiones utilizadas por un jugador con respecto al total de posesiones disponibles cuando estaba en pista. Además de los tiros, tiros libres y pérdidas, también se consideran las asistencias como parte porcentual de una posesión finalizada.
            </div>
            <div>
              <span className="font-medium">On/Off:</span> Compara las estadísticas del equipo cuando ciertos jugadores están en cancha vs. cuando no están. Se agregan todas las posesiones en cada escenario y se calculan ORtg/DRtg para medir el impacto del jugador/lineup.
            </div>
          </div>
        </div>

        {/* Sobre las Herramientas */}
        <div className="bg-white rounded-lg shadow-sm border border-acb-200 p-6">
          <h2 className="text-xl font-semibold text-acb-900 mb-4">Resumen de las herramientas</h2>
          <div className="space-y-3 text-sm text-acb-700">
            <div>
              <span className="font-medium">Equipos:</span> Las herramientas de equipo nos muestran estadísticas avanzadas para todos los equipos ACB en la temporada escogida. Es útil para obtener información rápida de un vistazo, identificar fortalezas o debilidades de algunos equipos, o para saber qué equipos de la liga destacan en las categorías en las que estemos interesados. Además de las estadísticas generales, también hay perfiles de equipo para entender mejor su estilo, una herramienta de análisis de partido para revisar la evolución del marcador y de las jugadas, y una sección de Four Factors para resumir tiro, pérdidas, rebote y tiros libres.
            </div>
            <div>
              <span className="font-medium">Jugadores:</span> Estas herramientas muestran una compilación de estadísticas individuales, tanto básicas como avanzadas. Existen disponibles tablas para toda la liga, perfiles de jugador con información más detallada sobre producción, eficiencia y evolución, y estadísticas clutch para analizar el rendimiento en finales ajustados. La idea es que se pueda pasar de una visión general de la liga a una lectura más concreta de qué hace bien un jugador y en qué contexto aporta más.
            </div>
            <div>
              <span className="font-medium">Herramientas:</span> Este bloque recoge herramientas pensadas para comparar y buscar relaciones entre jugadores y equipos. La similitud permite encontrar jugadores con un perfil estadístico parecido; la comparación directa sirve para poner dos jugadores frente a frente en una misma vista; y el cara a cara de equipos permite comparar dos equipos por métricas para entender mejor cómo encajan sus fortalezas y debilidades.
            </div>
            <div>
              <span className="font-medium">Alineaciones:</span> Las estadísticas de alineación nos permiten analizar el desempeño de un equipo cuando ciertos jugadores o conjuntos de jugadores están (o no) en la cancha. Se puede utilizar para identificar combinaciones de jugadores con las que un equipo tiende a tener un mejor desempeño. Además del análisis On/Off, también hay un ranking de mejores alineaciones para encontrar quintetos que hayan producido especialmente bien en una temporada determinada.
            </div>
            <div>
              <span className="font-medium">Tiro:</span> Las cartas de tiro muestran la distribución de tiros de campo para un jugador o equipo. Es una herramienta que nos permite, por ejemplo, conocer desde dónde un equipo tiene tendencia a tirar, desde dónde son más eficientes los lanzamientos de un jugador o si la selección de tiro puede ser mejorable. También se ofrecen líderes por zona para revisar los máximos anotadores y los anotadores más eficientes desde cada zona del campo en una temporada dada.
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}

export default About
