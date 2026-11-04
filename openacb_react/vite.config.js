import { defineConfig, loadEnv } from 'vite'
import process from 'node:process'
import react from '@vitejs/plugin-react'

const UUID = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i

// Adds the Umami tracker when VITE_UMAMI_WEBSITE_ID is set (for example in the
// Vercel project settings). It only reports from the production domains, so
// local builds and preview deployments are not counted. See src/utils/analytics.js.
function umamiTracker(env) {
  return {
    name: 'openacb-umami',
    transformIndexHtml() {
      const websiteId = env.VITE_UMAMI_WEBSITE_ID?.trim()
      if (!websiteId) return []
      if (!UUID.test(websiteId)) throw new Error('VITE_UMAMI_WEBSITE_ID must be the website UUID from Umami')
      return [{
        tag: 'script',
        injectTo: 'head',
        attrs: {
          async: true,
          src: env.VITE_UMAMI_SCRIPT_URL || 'https://cloud.umami.is/script.js',
          'data-website-id': websiteId,
          'data-domains': env.VITE_UMAMI_DOMAINS || 'openacb.com,www.openacb.com',
          'data-auto-pageview': 'false',
          'data-openacb-analytics': '',
        },
      }]
    },
  }
}

// configuración de vite
export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, process.cwd(), 'VITE_')
  return {
    plugins: [react(), umamiTracker(env)],
    define: {
      __DEFINES__: JSON.stringify({})
    },
    server: {
      headers: {
        'Cache-Control': 'no-store, no-cache, must-revalidate, proxy-revalidate',
        'Pragma': 'no-cache',
        'Expires': '0',
        'Surrogate-Control': 'no-store'
      }
    }
  }
})
