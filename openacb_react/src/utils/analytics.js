// Page views and events for Umami (https://umami.is). The tracker script is
// only added to the page when VITE_UMAMI_WEBSITE_ID is set at build time (see
// vite.config.js); without it every call here does nothing. Vercel Web
// Analytics keeps counting page views on its own (<Analytics /> in App.jsx).
//
// The script loads with data-auto-pageview="false": the app reports page views
// itself, once per path, so the query string rewrites that tools make on load
// are not counted as extra visits.

// Captured when the app starts, before any tool rewrites the URL, so the first
// page view keeps its utm_* parameters and the external referrer.
const boot = typeof window === 'undefined'
  ? null
  : { url: window.location.href, referrer: document.referrer }

let firstPageview = true
const queue = []
let waiting = false

function trackerScript() {
  return typeof document === 'undefined' ? null : document.querySelector('script[data-website-id][data-openacb-analytics]')
}

function withUmami(callback) {
  if (typeof window === 'undefined') return
  if (window.umami) {
    callback(window.umami)
    return
  }
  const script = trackerScript()
  if (!script) return // analytics not configured for this build
  queue.push(callback)
  if (waiting) return
  waiting = true
  script.addEventListener('load', () => {
    waiting = false
    if (!window.umami) return
    queue.splice(0).forEach(run => run(window.umami))
  }, { once: true })
}

let previousUrl = null

// The URL and referrer are fixed when the page view happens: the tracker may
// load later, after more navigation.
export function trackPageview() {
  if (typeof window === 'undefined') return
  const { origin, pathname, href } = window.location
  // The first view keeps the original query string (utm_*) on the current path,
  // which differs from the boot URL when an old link redirected.
  const url = firstPageview && boot ? `${origin}${pathname}${new URL(boot.url).search}` : href
  const referrer = firstPageview && boot
    ? boot.referrer
    : (previousUrl?.startsWith(origin) ? previousUrl.slice(origin.length) : previousUrl || '')
  firstPageview = false
  previousUrl = url
  withUmami(umami => {
    try {
      umami.track(props => ({ ...props, url, referrer }))
    } catch {
      // analytics must never break the page
    }
  })
}

export function trackEvent(name, data) {
  if (typeof window === 'undefined') return
  const url = window.location.href
  withUmami(umami => {
    try {
      umami.track(props => ({ ...props, url, name, data }))
    } catch {
      // analytics must never break the page
    }
  })
}
