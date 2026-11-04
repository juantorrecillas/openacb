import { useEffect, useRef, useState } from 'react'
import { ImageDown, Loader2, Share2 } from 'lucide-react'
import { canShareImages } from '../utils/shareSupport.js'
import styles from './ChartImageButton.module.css'

// The exporter and its fonts (~280 KB) load only when the visitor points at,
// focuses or taps the button, never with the page.
const loadExporter = () => import('../utils/chartImage.js')

// A prepared image waits this long for the "tap to share" click.
const PENDING_MS = 30000

/**
 * Exports a chart as a branded PNG. getImage() returns the spec described in
 * utils/chartImage.js, built at click time from the chart currently on screen.
 */
export default function ChartImageButton({ getImage, className = '' }) {
  const [state, setState] = useState('idle') // idle | working | ready | error
  const [canShare] = useState(canShareImages)
  const pending = useRef(null)
  const timer = useRef(null)

  useEffect(() => () => clearTimeout(timer.current), [])

  const prepare = () => {
    loadExporter().then(module => module.prepareChartImage()).catch(() => {})
  }

  const settle = (next, resetAfter) => {
    clearTimeout(timer.current)
    setState(next)
    if (resetAfter) {
      timer.current = setTimeout(() => {
        pending.current = null
        setState('idle')
      }, resetAfter)
    }
  }

  const handleClick = async () => {
    if (state === 'working') return
    try {
      const { createChartImage, deliverChartImage } = await loadExporter()
      let image = pending.current
      pending.current = null
      settle('working')
      if (!image) image = await createChartImage(getImage())
      const result = await deliverChartImage(image)
      // The share sheet wants a fresh tap when the image took too long: keep it ready.
      if (result === 'blocked') {
        pending.current = image
        settle('ready', PENDING_MS)
        return
      }
      settle('idle')
    } catch (error) {
      console.error('Error creando la imagen:', error)
      settle('error', 4000)
    }
  }

  const idleLabel = canShare ? 'Compartir imagen' : 'Descargar imagen'
  const label = {
    working: 'Preparando…',
    ready: 'Toca para compartir',
    error: 'No se pudo crear',
  }[state] || idleLabel
  const Icon = state === 'working' ? Loader2 : canShare ? Share2 : ImageDown

  return (
    <button
      type="button"
      className={`${styles.button} ${state === 'ready' ? styles.ready : ''} ${className}`}
      onClick={handleClick}
      onPointerEnter={prepare}
      onPointerDown={prepare}
      onFocus={prepare}
      aria-busy={state === 'working'}
      title={canShare ? 'Compartir el gráfico como imagen' : 'Descargar el gráfico como imagen PNG'}
    >
      <Icon className={state === 'working' ? styles.spin : undefined} size={14} aria-hidden="true" />
      <span aria-live="polite">{label}</span>
    </button>
  )
}
