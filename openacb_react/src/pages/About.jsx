import { Mail, Globe, Code, Info, Heart } from 'lucide-react'

function About() {
  // ============================================================================
  // EDITA AQUÍ TU INFORMACIÓN PERSONAL
  // ============================================================================
  const contactInfo = {
    name: "Juan Torrecillas",
    email: "juan.torrecillas.jodar@gmail.com",
    website: "https://juantorrecillas.es",
    GitHub: "https://github.com/juantorrecillas/openacb"  
  }

  return (
    <div className="max-w-4xl mx-auto">
        <div className="bg-white rounded-lg shadow-sm border border-slate-200 p-8 mb-6">
          <div className="flex items-center gap-4 mb-4">
            <div className="p-3 bg-slate-100 rounded-lg">
          <Info className="w-8 h-8 text-slate-700" />
            </div>
            <div>
          <h1 className="text-3xl font-bold text-slate-900">¿Qué es OpenACB?</h1>
            </div>
          </div>

          <div className="prose prose-slate max-w-none">
            <p className="text-slate-700 leading-relaxed">
          OpenACB es una plataforma de análisis de estadísticas avanzadas para la Liga Endesa - coloquialmente conocida por todos, especialmente los que ya tenemos una edad, como ACB.
          Este proyecto compila datos de Play-by-Play de acb.com para proporcionar una serie de estadísticas y herramientas analíticas que pueden resultar útiles para aficionados, entrenadores o cuerpo técnico. En mi opinión, la herramienta tiene una serie de ventajas que la hacen interesante para el mencionado público. En primer lugar, es una herramienta rápida y ligera, con una gran cantidad de estadísticas disponible. Segundo, proporciona herramientas de analítica avanzada, como el análisis On/Off o las cartas de tiro, que pueden ser de gran interés tanto para cuerpo técnico como para aficionados. En tercer lugar, es una app de código abierto. La mayoría de herramientas de analítica avanzada están escondidas bajo servicios de suscripción o muros de pago. Aquí tienes varios de esos servicios disponibles gratis, sin anuncios y en código abierto. En el repositorio de GitHub puedes encontrar tanto los datos como el código utilizado para descargar, tratar y presentar los mismos. <strong>Es, en definitiva, una herramienta cuyo único objetivo es ser utilizada.</strong>
            </p>
          </div>
        </div>

      {/* Sobre mí */}
      <div className="bg-white rounded-lg shadow-sm border border-slate-200 p-8 mb-6">
        <h2 className="text-2xl font-semibold text-slate-900 mb-4">Sobre mí</h2>
        <div className="text-base text-slate-700 space-y-2">
          <p>
            <strong> Juan Torrecillas.</strong> Soy Analista de Política Económica en el Joint Research Centre de la Comisión Europea. Como académico y aficionado al baloncesto, me alegra ver que el uso de la analítica y la estadística avanzada se hace más común. Este deporte se está haciendo más familiar con el mundo del dato; eso es positivo. Podéis seguirme en <a href="https://twitter.com/juan_torrec" target="_blank" rel="noopener noreferrer" className="text-slate-600 hover:text-slate-900 underline">Twitter (X para los modernos)</a>. También podéis ver más sobre mi trabajo académico en mi <a href={contactInfo.website} target="_blank" rel="noopener noreferrer" className="text-slate-600 hover:text-slate-900 underline">web personal</a>.
          </p>
        </div>
      </div>

      {/* Contacto */}
      <div className="bg-white rounded-lg shadow-sm border border-slate-200 p-8 mb-6">
        <h2 className="text-2xl font-semibold text-slate-900 mb-4">Contacto</h2>

        <div className="space-y-4">
          <div className="flex items-center gap-3 text-slate-700">
            <Mail className="w-5 h-5 text-slate-400" />
            <a
              href={`mailto:${contactInfo.email}`}
              className="hover:text-slate-900 hover:underline text-base"
            >
              {contactInfo.email}
            </a>
          </div>

          <div className="flex items-center gap-3 text-slate-700">
            <Globe className="w-5 h-5 text-slate-400" />
            <a
              href={contactInfo.website}
              target="_blank"
              rel="noopener noreferrer"
              className="hover:text-slate-900 hover:underline text-base"
            >
              {contactInfo.website}
            </a>
          </div>

          <div className="flex items-center gap-3 text-slate-700">
            <Code className="w-5 h-5 text-slate-400" />
            <a
              href={contactInfo.GitHub}
              target="_blank"
              rel="noopener noreferrer"
              className="hover:text-slate-900 hover:underline text-base"
            >
              Ver código en GitHub
            </a>
          </div>
        </div>

        <div className="mt-6 pt-6 border-t border-slate-200">
          <p className="text-base text-slate-600">
            Si tienes algún comentario, sugerencia, o encontraste un bug en la página, escríbeme un correo o abre un issue en GitHub!
          </p>
        </div>
      </div>

      {/* Glosario y Herramientas */}
      <div className="grid md:grid-cols-2 gap-6 mb-6">

        {/* Glosario */}
        <div className="bg-white rounded-lg shadow-sm border border-slate-200 p-6">
          <h2 className="text-xl font-semibold text-slate-900 mb-4 flex items-center gap-2">
            Glosario
          </h2>
          <div className="space-y-3 text-sm text-slate-700">
            <div>
              <span className="font-medium">Posesiones:</span> FGA + FT_trip - ORB + TOV. Número de posesiones de un equipo.
            </div>
            <div>
              <span className="font-medium">ORtg:</span> Offensive Rating = (Puntos / Posesiones) × 100. Puntos anotados por cada 100 posesiones.
            </div>
                        <div>
              <span className="font-medium">ORtg individual:</span> (Puntos producidos/ Posesiones utilizadas) × 100. La métrica individual tiene en cuenta, no sólo los puntos anotados, sino los producidos a partir de asistencias y rebotes ofensivos. Las asistencias se dividen entre 0.3 puntos para el asistente y 0.7 para el anotador por cada punto anotado.
            </div>
            <div>
              <span className="font-medium">DRtg:</span> Defensive Rating = (Puntos Rivales / Posesiones Rivales) × 100. Puntos permitidos por cada 100 posesiones.
            </div>
            <div>
              <span className="font-medium">NetRtg:</span> Net Rating = ORtg - DRtg. Diferencial de eficiencia entre ataque y defensa.
            </div>
            <div>
              <span className="font-medium">TS%:</span> True Shooting % = Puntos / (2 × (FGA + FT_trip)). Eficiencia de tiro incluyendo tiros libres y el valor extra de los triples.
            </div>
            <div>
              <span className="font-medium">eFG%:</span> Effective Field Goal % = (FGM + 0.5 × 3PM) / FGA. Porcentaje de tiro ajustado por el valor de los triples.
            </div>
            <div>
              <span className="font-medium">3Att%:</span> 3ptAtt% = 3PI / TCI. Qué porcentaje representan los triples sobre el total de tiros.</div>
            <div>
              <span className="font-medium">TOV%:</span> Turnover Rate = Pérdidas / Posesiones. Porcentaje de posesiones que terminan en pérdida.
            </div>
            <div>
              <span className="font-medium">ORB%:</span> Offensive Rebound % = ORB / (ORB + Opp_DRB). Porcentaje de rebotes ofensivos capturados.
            </div>
            <div>
              <span className="font-medium">DRB%:</span> Defensive Rebound % = DRB / (DRB + Opp_ORB). Porcentaje de rebotes defensivos capturados.
            </div>
            <div>
              <span className="font-medium">FT Rate:</span> Free Throw Rate = FTA / FGA. Tiros libres intentados por cada tiro de campo.
            </div>
            <div>
              <span className="font-medium">AST%:</span> Assist Rate = Asistencias / FGM. Porcentaje de canastas asistidas.
            </div>
            <div>
              <span className="font-medium">Pace:</span> Posesiones / Partidos. Ritmo de juego (posesiones por partido).
            </div>
            <div>
              <span className="font-medium">Four Factors:</span> Las cuatro variables clave del éxito: Shooting (eFG%), Turnovers (TOV%), Rebounding (ORB%/DRB%), Free Throws (FT Rate).
            </div>
            <div>
              <span className="font-medium">On/Off:</span> Compara las estadísticas del equipo cuando ciertos jugadores están en cancha vs. cuando no están. Se agregan todas las posesiones en cada escenario y se calculan ORtg/DRtg para medir el impacto del jugador/lineup.
            </div>
          </div>
        </div>

        {/* Sobre las Herramientas */}
        <div className="bg-white rounded-lg shadow-sm border border-slate-200 p-6">
          <h2 className="text-xl font-semibold text-slate-900 mb-4">Sobre las herramientas</h2>
          <div className="space-y-3 text-sm text-slate-700">
            <div>
              <span className="font-medium">Cartas de tiro:</span> Las cartas de tiro muestran la distribución de tiros de campo para un jugador o equipo. Es una herramienta que nos permite,
                      por ejemplo, conocer desde dónde un equipo tiene tendencia a tirar (por ejemplo, hay equipos y jugadores que tienen tendencia a  cargar muchos tiros en un lado del campo), desde dónde son más eficientes los lanzamientos de un jugador o si la selección de tiro puede ser mejorable.
            </div>
            <div>
              <span className="font-medium">Estadísticas de Equipo:</span> Este panel nos muestra estadísticas avanzadas para todos los equipos ACB en la temporada escogida. Es útil para obtener información rápida de un 
              vistazo, para identificar fortalezas o debilidades de algunos equipos, o para saber qué equipos de la liga destacan en las categorías en las que estemos interesados. Además, todas las estadísticas también están disponibles agregadas a nivel de oponente.
            </div>
            <div>
              <span className="font-medium">Estadísticas de Jugador:</span> El panel muestra una serie de estadísticas básicas y avanzadas a nivel jugador. Es una herramienta interesante para conocer el perfil de un jugador en concreto, o para consultar los líderes de la liga o de un equipo en una determinada estadística. También permite conocer la eficiencia de tiro de jugador en cada zona del campo y el efecto que la presencia del jugador tiene en la eficacia de tiros del rival.
            </div>
            <div>
              <span className="font-medium">Análisis de Alineación:</span> Las estadísticas de alineación nos permite analizar el desempeño de un equipo cuando ciertos jugadores o conjuntos de jugadores están (o no) en 
                        la cancha. Se puede utilizar para identificar combinaciones de jugadores con las que un equipo tiende a tener un mejor desempeño.
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}

export default About
