import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// configuración de vite
export default defineConfig({
  plugins: [react()],
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
})
