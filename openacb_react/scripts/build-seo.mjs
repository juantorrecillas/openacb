import { createHash } from 'node:crypto'
import { existsSync, mkdirSync, readFileSync, writeFileSync } from 'node:fs'
import { dirname, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'
import { ROUTE_MANIFEST } from '../src/routing/manifest.js'
import { resolveCanonicalEntityLocation } from '../src/routing/legacy.js'
import { buildPlayerProfilePath, buildTeamProfilePath } from '../src/routing/paths.js'
import { getMetaTags, getPageMetadata, SITE_IMAGE_ALT, SITE_IMAGE_SIZE, SITE_SCHEMA, SITE_URL } from '../src/seo/metadata.js'
import { buildLineupPage, buildPlayerPage, buildTeamPage, buildTeamShotPage, seasonLabel, SHOT_DATA_FIRST_SEASON } from '../src/seo/entities.js'
import { addPlayerNames, buildProfileSlugs } from '../src/utils/profileSlugs.js'
import { createOgRenderer } from './og-image.mjs'

// Runs after `vite build`. Writes, from dist/index.html:
// - one HTML file per tool with its own title and description
// - one HTML file per player, and per current team (profile, On/Off, shot chart),
//   with entity metadata and a readable summary for crawlers and link previews
// - 1200x630 link-preview images, sitemap.xml and robots.txt
// Players whose last season is within OG_IMAGE_SEASONS get their own preview
// image; older players use the generic card.
const OG_IMAGE_SEASONS = 3

const output = fileURLToPath(new URL('../dist/', import.meta.url))
const template = readFileSync(resolve(output, 'index.html'), 'utf8')
const seoBlock = /<!-- seo:start -->[\s\S]*?<!-- seo:end -->/
const rootBlock = '<div id="root"></div>'
if (!seoBlock.test(template)) throw new Error('Missing SEO markers in built index.html')
if (!template.includes(rootBlock)) throw new Error('Missing empty #root in built index.html')

const escapeHtml = value => String(value).replace(/[&<>"']/g, character => ({
  '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;',
})[character])
// JSON inside <script> must not close the tag early.
const scriptJson = value => JSON.stringify(value).replace(/</g, '\\u003c')
const readData = file => JSON.parse(readFileSync(resolve(output, 'data', file), 'utf8'))
const writePage = (path, html) => {
  const file = resolve(output, path === '/' ? 'index.html' : `${path.slice(1)}.html`)
  mkdirSync(dirname(file), { recursive: true })
  writeFileSync(file, html)
}
const shortHash = value => createHash('sha1').update(JSON.stringify(value)).digest('hex').slice(0, 10)

// ── Preview images ────────────────────────────────────────────
let renderer = null
let siteImage = { image: `${SITE_URL}/openacb.png`, imageAlt: 'Logo de openACB', card: 'summary', imageWidth: null, imageHeight: null }
try {
  renderer = await createOgRenderer()
  const png = await renderer.renderSiteCard()
  mkdirSync(resolve(output, 'og'), { recursive: true })
  writeFileSync(resolve(output, 'og', 'openacb.png'), png)
  siteImage = {
    image: `${SITE_URL}/og/openacb.png?v=${shortHash(png.toString('base64'))}`,
    imageAlt: SITE_IMAGE_ALT,
    imageWidth: SITE_IMAGE_SIZE.width,
    imageHeight: SITE_IMAGE_SIZE.height,
    card: 'summary_large_image',
  }
} catch (error) {
  // A missing native renderer must not block a data deploy: fall back to the logo.
  renderer = null
  console.warn(`SEO: preview images skipped (${error.message})`)
}

// Renders every card (a few at a time) and returns the image metadata per
// `${folder}/${name}`. A card that fails to render is left out, so its page
// falls back to the generic image instead of pointing at a missing file.
async function renderCardImages(jobs, concurrency = 6) {
  const images = new Map()
  if (!renderer) return images
  let next = 0
  let failures = 0
  await Promise.all(Array.from({ length: Math.min(concurrency, jobs.length) }, async () => {
    while (next < jobs.length) {
      const { folder, name, card } = jobs[next++]
      try {
        const file = resolve(output, 'og', folder, `${name}.png`)
        mkdirSync(dirname(file), { recursive: true })
        writeFileSync(file, await renderer.renderCard(card))
        images.set(`${folder}/${name}`, {
          image: `${SITE_URL}/og/${folder}/${name}.png?v=${shortHash(card)}`,
          imageAlt: `${card.title}: ${card.meta}`,
          imageWidth: SITE_IMAGE_SIZE.width,
          imageHeight: SITE_IMAGE_SIZE.height,
          card: 'summary_large_image',
        })
      } catch (error) {
        failures += 1
        if (failures <= 3) console.warn(`SEO: preview image ${folder}/${name} failed (${error.message})`)
      }
    }
  }))
  return images
}

// ── Head and body rendering ───────────────────────────────────
function renderHead(metadata, extraScripts = []) {
  const tags = [`<title>${escapeHtml(metadata.title)}</title>`]
  getMetaTags(metadata).forEach(([attribute, key, content]) => {
    if (content != null) tags.push(`<meta ${attribute}="${key}" content="${escapeHtml(content)}" />`)
  })
  if (metadata.canonical) tags.push(`<link rel="canonical" href="${escapeHtml(metadata.canonical)}" />`)
  if (metadata.isHome) {
    tags.push(`<script id="site-schema" type="application/ld+json">${scriptJson(SITE_SCHEMA)}</script>`)
  }
  extraScripts.forEach(script => tags.push(`<script type="application/ld+json">${scriptJson(script)}</script>`))
  return `<!-- seo:start -->\n    ${tags.join('\n    ')}\n    <!-- seo:end -->`
}

const link = (href, label) => `<a href="${escapeHtml(href)}">${escapeHtml(label)}</a>`

function renderTable({ caption, columns, numeric = [], rows }) {
  const head = columns.map((column, index) => `<th scope="col"${numeric[index] ? ' class="num"' : ''}>${escapeHtml(column)}</th>`).join('')
  const body = rows.map(row => `<tr>${row.map((cell, index) => `<td${numeric[index] ? ' class="num"' : ''}>${escapeHtml(cell)}</td>`).join('')}</tr>`).join('')
  return `<section><h2>${escapeHtml(caption)}</h2><div class="prerender-table"><table><thead><tr>${head}</tr></thead><tbody>${body}</tbody></table></div></section>`
}

// Rows with a linked entity name in the first column.
function renderNamedTable({ caption, columns, rows }) {
  const head = columns.map((column, index) => `<th scope="col"${index > 0 ? ' class="num"' : ''}>${escapeHtml(column)}</th>`).join('')
  const body = rows.map(row => {
    const name = row.path ? link(row.path, row.name) : escapeHtml(row.name)
    return `<tr><td>${name}</td>${row.cells.map(cell => `<td class="num">${escapeHtml(cell)}</td>`).join('')}</tr>`
  }).join('')
  return `<section><h2>${escapeHtml(caption)}</h2><div class="prerender-table"><table><thead><tr>${head}</tr></thead><tbody>${body}</tbody></table></div></section>`
}

function renderBody(page) {
  const crumbs = page.breadcrumbs
    .map((crumb, index) => (index === page.breadcrumbs.length - 1 ? escapeHtml(crumb.name) : link(crumb.path, crumb.name)))
    .join(' › ')
  const parts = [
    `<nav class="prerender-crumbs" aria-label="Ruta">${crumbs}</nav>`,
    `<h1>${escapeHtml(page.heading)}</h1>`,
    page.facts?.length ? `<p class="prerender-facts">${page.facts.map(escapeHtml).join(' · ')}</p>` : '',
    page.fullName ? `<p class="prerender-facts">Nombre completo: ${escapeHtml(page.fullName)}</p>` : '',
    page.summary?.length ? `<div class="prerender-summary">${page.summary.map(text => `<p>${escapeHtml(text)}</p>`).join('')}</div>` : '',
    page.roster ? renderNamedTable(page.roster) : '',
    page.table ? renderTable(page.table) : '',
    page.similar?.length
      ? `<section><h2>Jugadores con un perfil parecido</h2><ul class="prerender-list">${page.similar.map(item => `<li>${link(item.path, item.name)} · ${escapeHtml(item.context)}</li>`).join('')}</ul></section>`
      : '',
    page.links?.length
      ? `<section><h2>Más análisis</h2><ul class="prerender-list">${page.links.map(item => `<li>${link(item.href, item.label)}</li>`).join('')}</ul></section>`
      : '',
  ]
  return `<div id="root"><div class="prerender" data-prerendered-path="${escapeHtml(page.path)}">`
    + '<div class="prerender-bar"><div class="prerender-bar-inner"><a class="prerender-brand" href="/">open<span>ACB</span></a></div></div>'
    + `<main class="prerender-main"><div class="prerender-content">${parts.join('')}</div></main>`
    + '</div></div>'
}

function breadcrumbSchema(page) {
  return {
    '@context': 'https://schema.org',
    '@type': 'BreadcrumbList',
    itemListElement: page.breadcrumbs.map((crumb, index) => ({
      '@type': 'ListItem',
      position: index + 1,
      name: crumb.name,
      item: `${SITE_URL}${crumb.path}`,
    })),
  }
}

function writeEntityPage(page, image) {
  const metadata = {
    title: page.title,
    description: page.description,
    canonical: `${SITE_URL}${page.path}`,
    robots: 'index, follow, max-image-preview:large',
    isHome: false,
    ...(image || siteImage),
  }
  writePage(page.path, template
    .replace(seoBlock, renderHead(metadata, [breadcrumbSchema(page)]))
    .replace(rootBlock, renderBody(page)))
}

// ── Tool pages and the SPA shell ──────────────────────────────
// Deep entity links without a prerendered page and old URLs still load React. Leave the
// canonical unset in this shell so it cannot misidentify those pages as the homepage.
writeFileSync(resolve(output, 'app.html'), template.replace(seoBlock, renderHead({
  ...getPageMetadata('/'), ...siteImage, canonical: null, isHome: false,
})))

const toolPaths = ROUTE_MANIFEST.flatMap(route => route.patterns.filter(path => !path.includes(':')))
for (const path of toolPaths) {
  writePage(path, template.replace(seoBlock, renderHead({ ...getPageMetadata(path), ...siteImage })))
}

// ── Entity pages ──────────────────────────────────────────────
// Reads the exports inside a function so the large arrays can be freed before
// the preview cards render (rendering is ~3x slower with them still in memory).
function collectEntityPages() {
  const playerIndex = readData('player-index.json')
  const playerNames = readData('player-names.json')
  const playersByStage = readData('players-by-stage.json')
  const playerBio = readData('player-bio.json')
  const teams = readData('teams.json')
  const similarity = readData('similarity.json')

  const currentSeason = Math.max(...teams.map(team => team.season))
  const currentTeams = teams.filter(team => team.season === currentSeason)
  const teamPathById = Object.fromEntries(currentTeams.map(team => [team.teamId, buildTeamProfilePath(team.teamId)]))

  // Same slugs and names as the app (App.jsx), checked against its URL resolver below.
  const profileSlugs = buildProfileSlugs(playerIndex, playerNames)
  const namedIndex = addPlayerNames(playerIndex.map(row => ({ ...row })), playerNames, profileSlugs)
  const routingContext = { players: namedIndex }
  const fullNameById = new Map(namedIndex.map(row => [String(row.licenseId), row.playerFull || row.player]))
  const nameOf = id => playerNames[id] || fullNameById.get(id) || 'Jugador'
  const profilePathOf = id => buildPlayerProfilePath({ profileSlug: profileSlugs[id] })

  const regularById = new Map()
  playersByStage.forEach(row => {
    if ((row.competitionStage || 'regular') !== 'regular') return
    const id = String(row.licenseId)
    if (!regularById.has(id)) regularById.set(id, [])
    regularById.get(id).push(row)
  })
  const similarityByKey = new Map(similarity.map(row => [`${row.licenseId}|${row.season}|${row.teamId}`, row]))
  const teamNameBySeason = new Map(teams.map(team => [`${team.season}|${team.teamId}`, team.team]))

  // Entity pages are collected first, their preview cards rendered together,
  // and only then written, each with its own image or the generic one.
  const entities = [] // { page, imageKey }
  const imageJobs = []
  // A page the app would redirect or reject is skipped, so it cannot be indexed under a wrong URL.
  const skippedPaths = []
  const isCanonical = path => {
    const resolution = resolveCanonicalEntityLocation({ pathname: path, search: '' }, routingContext)
    if (resolution) skippedPaths.push(`${path} (${resolution.status}${resolution.to ? ` → ${resolution.to}` : ''})`)
    return !resolution
  }
  const addImageJob = (folder, name, card) => {
    imageJobs.push({ folder, name, card })
    return `${folder}/${name}`
  }

  for (const id of Object.keys(profileSlugs)) {
    const rows = regularById.get(id)
    if (!rows?.length) continue
    const name = nameOf(id)
    const latest = [...rows].sort((a, b) => b.season - a.season || (b.games ?? 0) - (a.games ?? 0))[0]
    const similarEntry = similarityByKey.get(`${id}|${latest.season}|${latest.teamId}`)
    const similar = (similarEntry?.similar || [])
      .filter(item => profileSlugs[String(item.licenseId)])
      .slice(0, 3)
      .map(item => ({
        name: nameOf(String(item.licenseId)),
        path: profilePathOf(String(item.licenseId)),
        context: `${seasonLabel(item.season)}, ${teamNameBySeason.get(`${item.season}|${item.teamId}`) || item.teamId}`,
      }))
    const page = buildPlayerPage({
      licenseId: id,
      name,
      fullName: latest.playerFull,
      profileSlug: profileSlugs[id],
      rows,
      bio: playerBio[id] || {},
      currentSeason,
      teamPathById,
      similar,
      hasSimilarity: Boolean(similarEntry),
    })
    if (!isCanonical(page.path)) continue
    const imageKey = latest.season > currentSeason - OG_IMAGE_SEASONS
      ? addImageJob('jugadores', profileSlugs[id], page.card)
      : null
    entities.push({ page, imageKey })
  }

  const lineupFile = resolve(output, 'data', `lineups-${currentSeason}.json`)
  const lineups = existsSync(lineupFile) ? readData(`lineups-${currentSeason}.json`).data : {}
  const shotsFile = resolve(output, 'data', `shots-${currentSeason}.json`)
  const seasonShots = currentSeason >= SHOT_DATA_FIRST_SEASON && existsSync(shotsFile) ? readData(`shots-${currentSeason}.json`) : []
  const regularSeasonRows = playersByStage.filter(row => row.season === currentSeason && (row.competitionStage || 'regular') === 'regular')

  for (const team of currentTeams) {
    const roster = regularSeasonRows
      .filter(row => row.teamId === team.teamId && profileSlugs[String(row.licenseId)])
      .sort((a, b) => (b.ppg ?? 0) - (a.ppg ?? 0))
      .map(row => ({
        name: nameOf(String(row.licenseId)),
        path: profilePathOf(String(row.licenseId)),
        position: row.position || playerBio[String(row.licenseId)]?.position,
        games: row.games,
        ppg: row.ppg,
        rpg: row.rpg,
        apg: row.apg,
      }))
    const profile = buildTeamPage({
      teamId: team.teamId,
      rows: teams.filter(row => row.teamId === team.teamId),
      seasonRows: currentTeams,
      roster,
      currentSeason,
    })
    // The team card also previews the team's On/Off and shot chart pages.
    const imageKey = addImageJob('equipos', team.teamId, profile.card)
    entities.push({ page: profile, imageKey })

    const lineupData = lineups[team.team]
    if (lineupData?.players) {
      const nameById = Object.fromEntries(Object.values(lineupData.players).map(player => {
        const id = String(player.id)
        return [id, profileSlugs[id] ? { name: nameOf(id), path: profilePathOf(id) } : { name: player.name }]
      }))
      entities.push({
        page: buildLineupPage({
          teamId: team.teamId,
          team: team.team,
          season: currentSeason,
          players: Object.values(lineupData.players),
          lineups: Object.values(lineupData.lineups || {}),
          nameById,
        }),
        imageKey,
      })
    }

    const teamShots = seasonShots.filter(shot => shot.team === team.team)
    if (teamShots.length) {
      entities.push({
        page: buildTeamShotPage({ teamId: team.teamId, team: team.team, season: currentSeason, shots: teamShots, leagueShots: seasonShots }),
        imageKey,
      })
    }
  }
  return { entities, imageJobs, skippedPaths }
}

const { entities, imageJobs, skippedPaths } = collectEntityPages()
const images = await renderCardImages(imageJobs)
for (const { page, imageKey } of entities) writeEntityPage(page, imageKey ? images.get(imageKey) : null)
const entityPaths = entities.map(({ page }) => page.path)

// ── Sitemap and robots ────────────────────────────────────────
const sitemapPaths = [...toolPaths, ...entityPaths]
writeFileSync(resolve(output, 'sitemap.xml'), `<?xml version="1.0" encoding="UTF-8"?>
<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
${sitemapPaths.map(path => `  <url><loc>${escapeHtml(`${SITE_URL}${path}`)}</loc></url>`).join('\n')}
</urlset>
`)
writeFileSync(resolve(output, 'robots.txt'), `User-agent: *
Allow: /

Sitemap: ${SITE_URL}/sitemap.xml
`)

if (skippedPaths.length) console.warn(`SEO: skipped ${skippedPaths.length} non-canonical pages:\n  ${skippedPaths.join('\n  ')}`)
console.log(`SEO: ${toolPaths.length} tool pages, ${entityPaths.length} entity pages, ${images.size} of ${imageJobs.length} preview images${renderer ? '' : ' (renderer unavailable)'}, sitemap with ${sitemapPaths.length} URLs`)
