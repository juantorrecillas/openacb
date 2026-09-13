---
name: "OpenACB"
description: "The current visual system of a basketball statistics reference."
colors:
  acb-50: "#f0f4f8"
  acb-100: "#d9e2ec"
  acb-200: "#bcccdc"
  acb-300: "#9fb3c8"
  acb-400: "#829ab1"
  acb-500: "#627d98"
  acb-600: "#486581"
  acb-700: "#334e68"
  acb-800: "#243b53"
  acb-900: "#102a43"
  accent-50: "#fef5ef"
  accent-100: "#fde6d5"
  accent-200: "#fcc7a8"
  accent-300: "#f9b478"
  accent-400: "#f8974d"
  accent-500: "#fe5917"
  accent-700: "#d04313"
  accent-800: "#cb600e"
  accent-900: "#a43900"
  info-100: "#dbeafe"
  info-200: "#bfdbfe"
  info-300: "#93c5fd"
  info-800: "#1e40af"
  info-900: "#1e3a8a"
  positive-100: "#dcfce7"
  positive-500: "#2aa867"
  positive-700: "#15803d"
  negative-100: "#ffe4e6"
  negative-200: "#fecdd3"
  negative-500: "#dd415d"
  negative-700: "#9f1239"
  gold-700: "#744f09"
  sage-700: "#1d4f4b"
  plum-700: "#5220a0"
  sand-700: "#60492d"
  white: "#ffffff"
  analysis-slate: "#e7edf3"
  body-bg: "#f8fafc"
  body-text: "#1e293b"
  lemon: "#ece338"
typography:
  lineup-display:
    fontFamily: "Manrope, Inter, sans-serif"
    fontSize: "40px"
    fontWeight: 700
    lineHeight: "1.08"
    letterSpacing: "-0.03em"
  team-display:
    fontFamily: "Manrope, Inter, sans-serif"
    fontSize: "40px"
    fontWeight: 700
    lineHeight: "1.08"
    letterSpacing: "-0.03em"
  display:
    fontFamily: "Manrope, Inter, sans-serif"
    fontSize: "36px"
    fontWeight: 700
    lineHeight: "40px"
  headline:
    fontFamily: "Manrope, Inter, sans-serif"
    fontSize: "24px"
    fontWeight: 600
    lineHeight: "32px"
  title:
    fontFamily: "Manrope, Inter, sans-serif"
    fontSize: "20px"
    fontWeight: 600
    lineHeight: "28px"
  body:
    fontFamily: "Inter, system-ui, sans-serif"
    fontSize: "16px"
    fontWeight: 400
    lineHeight: "24px"
  body-small:
    fontFamily: "Inter, system-ui, sans-serif"
    fontSize: "14px"
    fontWeight: 400
    lineHeight: "20px"
  label:
    fontFamily: "Inter, system-ui, sans-serif"
    fontSize: "14px"
    fontWeight: 500
    lineHeight: "20px"
  caption:
    fontFamily: "Inter, system-ui, sans-serif"
    fontSize: "12px"
    fontWeight: 400
    lineHeight: "16px"
  table-head:
    fontFamily: "Inter, system-ui, sans-serif"
    fontSize: "10.5px"
    fontWeight: 600
    lineHeight: "15px"
  table-number:
    fontFamily: "JetBrains Mono, Consolas, monospace"
    fontSize: "12px"
    fontWeight: 400
    lineHeight: "18px"
rounded:
  default: "4px"
  md: "6px"
  lg: "8px"
  xl: "12px"
  full: "9999px"
spacing:
  "1": "4px"
  "2": "8px"
  "3": "12px"
  "4": "16px"
  "5": "20px"
  "6": "24px"
  "8": "32px"
  "10": "40px"
  "12": "48px"
  "1.5": "6px"
  "2.5": "10px"
  "3.5": "14px"
components:
  lineup-tool-link:
    backgroundColor: "{colors.white}"
    textColor: "{colors.acb-600}"
    padding: "10px 18px"
    height: "64px"
    rounded: "0"
  lineup-tool-link-active:
    backgroundColor: "{colors.accent-50}"
    textColor: "{colors.acb-900}"
  lineup-control-band:
    backgroundColor: "{colors.analysis-slate}"
    textColor: "{colors.acb-900}"
    padding: "16px 20px"
    rounded: "0"
  lineup-evidence-header:
    backgroundColor: "{colors.acb-900}"
    textColor: "{colors.white}"
    padding: "18px 20px"
    rounded: "0"
  lineup-option-selected:
    backgroundColor: "{colors.acb-900}"
    textColor: "{colors.white}"
  button-primary:
    backgroundColor: "{colors.acb-900}"
    textColor: "{colors.white}"
    rounded: "{rounded.lg}"
    padding: "12px 24px"
    typography: "{typography.body}"
  button-primary-hover:
    backgroundColor: "{colors.acb-800}"
  button-secondary:
    backgroundColor: "{colors.white}"
    textColor: "{colors.acb-700}"
    rounded: "{rounded.default}"
    padding: "4px 10px"
    typography: "{typography.caption}"
  form-control:
    backgroundColor: "{colors.white}"
    textColor: "{colors.acb-900}"
    rounded: "{rounded.md}"
    padding: "0 12px"
    height: "40px"
    typography: "{typography.body-small}"
  nav-item:
    textColor: "{colors.acb-600}"
    rounded: "{rounded.md}"
    padding: "6px 10px"
    typography: "{typography.caption}"
  game-nav-active:
    backgroundColor: "{colors.white}"
    textColor: "{colors.acb-900}"
    rounded: "0"
    padding: "10px 16px"
    height: "44px"
    typography: "{typography.caption}"
  content-card:
    backgroundColor: "{colors.white}"
    rounded: "{rounded.lg}"
    padding: "20px"
  landing-primary-action:
    backgroundColor: "{colors.accent-500}"
    textColor: "{colors.white}"
    rounded: "0"
    padding: "17px 22px"
    height: "66px"
    typography: "{typography.body}"
  landing-family-runway:
    backgroundColor: "{colors.white}"
    textColor: "{colors.acb-900}"
    rounded: "0"
  landing-analysis-preview:
    backgroundColor: "{colors.acb-900}"
    textColor: "{colors.white}"
    rounded: "0"
  landing-directory-sheet:
    backgroundColor: "{colors.white}"
    textColor: "{colors.acb-900}"
    rounded: "0"
  segmented-option:
    backgroundColor: "{colors.white}"
    textColor: "{colors.acb-600}"
    padding: "6px 12px"
    typography: "{typography.label}"
  segmented-option-selected:
    backgroundColor: "{colors.acb-800}"
    textColor: "{colors.white}"
  percentile-high:
    backgroundColor: "{colors.accent-200}"
    textColor: "{colors.accent-900}"
    rounded: "{rounded.default}"
  data-table:
    backgroundColor: "{colors.white}"
    textColor: "{colors.acb-700}"
    typography: "{typography.table-number}"
  team-tool-link:
    backgroundColor: "{colors.white}"
    textColor: "{colors.acb-600}"
    padding: "10px 14px"
    height: "64px"
    typography: "{typography.label}"
  team-tool-link-active:
    backgroundColor: "{colors.accent-50}"
    textColor: "{colors.acb-900}"
  team-control-band:
    backgroundColor: "{colors.analysis-slate}"
    textColor: "{colors.acb-900}"
    rounded: "0"
    padding: "16px 20px"
  team-data-sheet:
    backgroundColor: "{colors.white}"
    textColor: "{colors.acb-900}"
    rounded: "0"
  team-view-selected:
    backgroundColor: "{colors.acb-900}"
    textColor: "{colors.white}"
  player-tool-link:
    backgroundColor: "{colors.white}"
    textColor: "{colors.acb-600}"
    padding: "10px 14px"
    height: "64px"
    typography: "{typography.label}"
  player-tool-link-active:
    backgroundColor: "{colors.accent-50}"
    textColor: "{colors.acb-900}"
  player-control-band:
    backgroundColor: "{colors.analysis-slate}"
    textColor: "{colors.acb-900}"
    rounded: "0"
    padding: "16px 20px"
  player-header-action:
    backgroundColor: "{colors.white}"
    textColor: "{colors.acb-600}"
    rounded: "{rounded.default}"
    padding: "7px 10px"
    height: "36px"
  player-identity-lead:
    backgroundColor: "{colors.accent-50}"
    textColor: "{colors.acb-900}"
    rounded: "{rounded.lg}"
    padding: "26px 28px"
  control-band:
    backgroundColor: "{colors.analysis-slate}"
    textColor: "{colors.acb-900}"
    rounded: "{rounded.lg}"
    padding: "14px 16px"
  editorial-dossier:
    backgroundColor: "{colors.white}"
    textColor: "{colors.acb-900}"
    rounded: "0"
    padding: "0"
    typography: "{typography.body-small}"
---

# Design System: OpenACB

## Overview

**Creative North Star: "A basketball statistics reference"**

OpenACB is practical, compact, and approachable, with analytical clarity as its priority. Its visual identity combines deep navy, slate blue-gray, white data sheets, and basketball orange. The landing page makes that identity explicit as a **Sala de análisis**: a full-width working surface where the visitor sees real analysis before choosing a tool. The reusable analytical composition remains a working basketball analysis desk: a clear identity header, visible tool choices, a compact control band, a primary visual, and dense evidence without decorative dashboard chrome.

This is a descriptive record of the current implementation, including the landing page, the public information dossier, Team Tools, and the five shipped Player Tools. The September 2026 Team Tools refresh received a source finish-review verdict of SHIP; desktop and phone examples were inspected locally, while owner visual acceptance remains pending. This record does not certify every current pattern as an ideal. Preserve the owner-confirmed page structure, palette, and analytical functionality when refining the interface.

**Key Characteristics:**
- A full-width landing first viewport split between a pale slate declaration and a deep-navy live analysis canvas.
- A five-column numbered tool-family runway, followed by one continuous ruled white directory sheet and a flat navy open-source close.
- Manrope for display and section headings, Inter for interface text, and JetBrains Mono for aligned statistical values.
- Compact, familiar controls and information-dense tables.
- Flat, border-defined analytical surfaces with slate control bands.
- Orange reserved for active states and accents alongside separate, meaningful statistical color scales.
- Visible family-level tool rails: four Team Tools as 4 / 2x2 and five Player Tools as 5 / 3+2 / 2+2+1.
- Player identity presented as a warm court-paper scouting dossier joined to dense, ruled evidence.
- Lineup selection presented as a rotation strip above a ruled roster pool, followed by continuous evidence sheets and a permanently visible two-tool rail.
- Public and reference material presented as an editorial dossier: a declaration, a ruled register, a chapter index, and one continuous white reading sheet.

Sources: [product context](PRODUCT.md), [theme](openacb_react/tailwind.config.js), [global styles](openacb_react/src/index.css), [app shell](openacb_react/src/App.jsx), [landing page](openacb_react/src/pages/Home.jsx), the [public information dossier](openacb_react/src/pages/About.jsx), the [Team Tools shell](openacb_react/src/components/TeamToolShell.jsx), the [Player Tools shell](openacb_react/src/components/PlayerToolShell.jsx), the five player-tool pages under [src/pages](openacb_react/src/pages), and shared components under [src/components](openacb_react/src/components). Values in the frontmatter document reused tokens and roles. Existing component-level overrides still apply. The companion file is [.impeccable/design.json](.impeccable/design.json).

## Colors

Deep navy and slate blue-gray provide structure; basketball orange identifies selected links and interactive emphasis. White data sheets sit on a light blue-gray application canvas, while the slightly stronger analysis slate separates controls from results.

### Primary

**Deep navy** (acb-900) is the main heading color, the landing-page analysis canvas and open-source close, and the generic primary action fill outside the landing hero. acb-800 supplies selected segmented controls and generic primary action hover. acb-700 appears on other section headers.

**Basketball orange** (accent-500) is the strong interaction accent and the landing page's filled primary action, numbered runway marks, chart accents, and directional cues. Darker accent-700 is used for active and hovered navigation text and for the landing primary-action hover. Pale accent-50 and accent-100 appear behind search selections and hovered tool links. Accent ramps also serve statistical encodings, where the legend takes precedence.

### Secondary

**Statistical green and rose** (positive and negative families) communicate signed results and shot outcomes. Made-shot marks use positive-500 at 0.75 opacity; missed-shot marks use negative-500 at 0.5 opacity in the global shot classes. Individual charts can define their own contextual encodings.

**Statistical blue** (info family) is paired with orange in the percentile helpers. Badge mappings in [percentileColors.js](openacb_react/src/utils/percentileColors.js) are: 75+ uses accent-200/accent-900; 50–74 uses accent-100/accent-800; 25–49 uses info-100/info-800; below 25 uses info-200/info-900. Missing or non-finite values use acb-100/acb-600. The corresponding bars use accent-300, accent-200, info-200, and info-300; missing values use acb-200. The colors encode percentile ranges, not universally good or bad performance.

**Player role and category colors** use the established gold-700, sage-700, plum-700, and sand-700 alongside accent, info, positive, and negative text. In the shipped player dossier, these are typographic cues for role and archetype names on neutral or warm paper; they do not create a grid of colored badges or cards. The category mapping is descriptive, while positive/negative and percentile mappings retain their separate analytical meanings.

**Lemon** is an existing footer accent; it is not the general action color. The public information dossier keeps links within the established ink-and-orange palette, using orange for underline, arrow, hover, and focus emphasis rather than introducing another accent family.

### Neutral

White is the header, card, form, and table-row surface. Analysis slate is the reusable control-band and chart-control fill. acb-50 is the visible app canvas and common hover surface; acb-100 separates groups and backs icon tiles. acb-200 provides default borders. acb-400, acb-500, and acb-600 express progressively stronger secondary text. Body fallback colors are recorded separately because the application wrapper overrides the body background.

**The Statistical Meaning Rule.** Preserve the semantic meaning of each analytical color scale; a visual edit must not change the interpretation of the data.

**The Orange Reserve Rule.** Use basketball orange for the current tool, selected state, focus treatment, or a deliberate accent; do not spread it across passive analytical surfaces.

The CSS declaration --color-accent is a separate legacy value (#f0845e); no current reference was found in the scanned source. Do not substitute it for accent-500. Gold, sage, plum, and sand are included only at the 700 steps now reused for player-role typography; their lighter scale steps are not permission to introduce tinted analytical surfaces. This record preserves these distinctions rather than normalizing the code.

## Typography

**Display font:** Manrope, with Inter and sans-serif fallbacks. **Body and interface font:** Inter, with system-ui and sans-serif fallbacks. **Numeric font:** JetBrains Mono, with Consolas and monospace fallbacks. The Team, Player, and Lineup Tool shells serve Manrope locally from the bundled font file; the other families use the existing font loading path.

The typography is a restrained three-family hierarchy: Manrope gives page titles, section headings, and identity names a confident display voice; Inter keeps copy, labels, controls, and entity names neutral; JetBrains Mono aligns records, metadata, scores, and table values. The landing declaration expands Manrope to 66–82px on large screens and 58–68px on phones, while runway numbers and compact analytical readouts use JetBrains Mono. Weight, size, alignment, and grouping carry the hierarchy without introducing a decorative sports face or a fourth font.

| Role | Use |
| --- | --- |
| Display | Landing-page and analytical masthead titles; the largest shared hierarchy entry. |
| Headline | Page and major analytical headings. |
| Title | Panel, section, method, and identity headings; some local headings use 17–19px. |
| Body | General page prose and default text. |
| Body-small | Subtitles, explanatory UI, and most standard controls. |
| Label | Form labels and segmented-control text. |
| Caption | Compact navigation, supporting metadata, and tool descriptions. |
| Table-head | Compact column headings; some identity headers use medium weight. |
| Table-number | Numeric table cells with tabular figures and right alignment. |

Table-specific CSS overrides generic responsive text utilities: body cells remain 12px with 18px line height; desktop headers are 10.5px with 15px line height, rising to 11px below 640px. Mobile form fields use 16px text even when individual controls request a smaller size. Entity names retain Inter. Uppercase tracking appears on category and table headings; it is not universal body styling. Player positions and archetype names use compact, weighted Inter text with category color; color supplements the label instead of replacing it.

Long-form public reading surfaces may extend the same three voices rather than introducing a fourth. The information dossier uses a large Manrope declaration, smaller Manrope chapter headings, compact Inter prose, and JetBrains Mono only for ordered register and chapter numbers. This editorial ramp is local to reading surfaces; it does not replace the standard PageHeader scale.

**The Numbers Rule.** Keep statistical values right-aligned with tabular numerals; retain sans-serif labels and entity names.

**The Three-Voice Rule.** Use Manrope for display hierarchy, Inter for interface language, and JetBrains Mono only where numeric alignment or compact analytical metadata benefits from it.

## Layout

The application uses a centered container capped at 1280px. Main horizontal padding is 16px by default, 24px from 640px, and 32px from 1024px, with 24px vertical padding. Analytical sections commonly use 24px vertical separation. Filter panels wrap their children, align them at the bottom, and use 16px padding and gaps.

The landing page is full-width rather than capped to the application container. Its large-screen first viewport fills the space below the header and uses two stacked bands: the upper 68% pairs a 33.5% pale-slate declaration with a 66.5% deep-navy live analysis preview, and the lower 32% is a five-column numbered family runway on white. The declaration states the promise, offers an orange primary action and underlined project link, and carries no centered logo, decorative eyebrow, card shell, gradient, or shadow. The preview supplies the proof through three real, user-selectable examples—Team, Player, and Alineaciones—with analytical labels, radar, compact shot-efficiency, or ruled lineup evidence, and direct links to the corresponding tool.

Below the first viewport, the tool directory is one square-edged white sheet capped at 1160px. Each of its five numbered family sections is a ruled row with a left introduction column and a two-column list of tool links; the sections are not independent cards. The page closes with a full-width, flat deep-navy open-source statement and links to the project and GitHub. At widths below 900px, the declaration and live preview stack, and the family runway becomes 2+2+1 with the fifth family spanning the last row. The directory progressively collapses to single-column tool lists and then to stacked family introductions on phones; its sheet reaches the viewport edges below 640px. Interactive links and preview tabs retain a 44px minimum touch target at narrow widths.

The header content is 48px high below 640px, 64px from 640px, and 80px from 1280px, plus its bottom border. Desktop navigation appears at 1280px; below that, a scrollable mobile menu contains grouped links. PageHeader stacks its title and actions on small screens and switches to a horizontal arrangement at 640px.

Analytical Team Tools use a reusable analysis-desk shell. A split masthead balances the question and supporting context, followed by a persistent white four-tool rail, a compact control band, then the main chart, identity surface, or table and its supporting evidence. The Manrope masthead is 40px, reducing to 32px below 768px. The rail is four equal columns on desktop and a visible 2x2 grid below 768px; do not hide tools behind a carousel or disclosure on phones. The workspace uses a 24px vertical rhythm, reducing to 22px below 480px.

Player Tools directly extend that shell. Their five links are equal columns above 1024px, form a visible 3+2 rail below 1024px, and form 2+2+1 below 768px, with the fifth link spanning the final phone row. The result area then supports three recurring evidence shapes: a dense directory table, a joined identity dossier and ruled facts, or paired comparison sheets. All keep the same 24px workspace rhythm.

Control bands are flexible slate strips with 14–18px gaps, bottom-aligned labels and controls, and white fields. Generic bands retain 14px by 16px padding. Player scope bands use 16px by 20px padding and horizontal boundary rules at every width. Team bands use the same desktop spacing, with 14px inline padding below 768px. They wrap as space narrows. At phone widths, field groups stretch and supporting analytical grids collapse progressively. Team game fixtures use an auto-fit ruled grid with minimum widths of 300px, 240px below 1024px, and 155px below 768px, bounded by the available width.

Tables scroll horizontally where needed, with designated identity columns remaining sticky. Standard widths include 40px for rank, 72px for season and numeric columns, 192px for player, 160px for team, and 80px for position. Below 640px, roster player columns shrink to 136px and secondary roster/career sticky columns become static. Preserve these local exceptions instead of applying a blanket sticky-column rule.

Public and reference pages may use the shipped editorial-dossier composition. A wide first viewport balances a left declaration against a flat ruled register, followed by a narrow chapter index and a single continuous reading sheet. The index is sticky only while there is room for a true side column; below 900px the declaration, register, index, and sheet stack in reading order, and below 640px the index becomes a two-column ruled grid while the sheet reaches the viewport edges. Preserve this hierarchy during reflow instead of compressing the desktop columns.

Lineup Tools retain two equal navigation columns at every width. Their split masthead stacks below 768px, with the title reducing from 40px to 32px. The roster pool uses five columns, four below 1024px, three below 768px, and two below 480px, inside a vertically scrollable region capped at 230px. Below 768px the four-value score strip becomes 2x2 and comparison and definition strips stack. Below 480px scope fields take full rows, the four ranking categories become 2x2, and ordering segments fill the available width. Wide tables retain named, keyboard-focusable horizontal scroll frames; responsive layout does not remove statistical columns.

**The Visible Tool Rail Rule.** Keep every family tool visible: Team Tools use four columns on desktop and 2x2 on phones; Player Tools use five columns on desktop, 3+2 at tablet width, and 2+2+1 on phones; Lineup Tools retain two columns throughout.

**The Landing Analysis Room Rule.** Keep the live Team/Player/Lineups evidence in the first viewport, with the complete five-family runway immediately below it and the full ruled directory later in the page.

**The Continuous Reading Sheet Rule.** Keep related long-form chapters inside one ruled white document; do not fragment each chapter into an independent floating card.

## Elevation & Depth

The approved analytical description is **flat, border-defined surfaces with tonal separation**. Team and Player Tool panels, data sections, control bands, identity dossiers, comparison sheets, charts, method panels, and the public information dossier use one-pixel borders, white, slate, or warm-paper fills and no shadow. Standard fields may retain a small shadow; dropdowns and tooltips use a larger shadow. Localized sticky identity or context panels elsewhere may retain medium depth, but the shipped player and public dossiers are flat.

The exact Tailwind shadow values are stored in the sidecar: small for fields and subtle surfaces, medium for sticky identity/context panels, and large for floating menus and tooltips. Sticky table identities use a one-pixel blue-gray separator shadow rather than a floating-card shadow.

Color transitions generally use Tailwind's 150ms default with cubic-bezier(0.4, 0, 0.2, 1). Team Tool links and controls use 150–160ms ease-out state transitions. The landing preview uses one 300ms settle-in transition when its selected example changes; hover feedback stays within color and underline changes. Other analytical animations remain local to their components. In reduced-motion mode, the landing page removes all animation and transition, while the analytical shell disables transitions and automatic smooth scrolling; navigation, controls, tables, and charts must remain fully usable without motion.

**The Flat Data Sheet Rule.** Use borders and tonal bands—not stacks of shadows—to organize analytical surfaces.

Lineup Tools extend that flat treatment with square slate working bands, white evidence sheets, and navy analysis headers, without routine surface shadows. Fields and selection controls retain compact rounding. Their color transitions use 150–160ms ease-out; reduced-motion mode removes transitions and automatic smooth scrolling.

## Shapes

Compact, familiar controls use gently rounded corners. Ordinary badges use the default small radius, form fields and filter groups use medium corners, and generic analytical content cards use large corners. Team Tools use square data sheets, dossier surfaces, and control bands, while retaining 4px corners on interactive controls. The landing page is deliberately square-edged: its split first viewport, runway, live preview, directory sheet, family rows, primary action, and open-source close use rectangles and rules rather than rounded cards. Full rounding remains local to existing circular portraits, selected badges, and bars.

One-pixel blue-gray outlines separate controls and surfaces. Player Tool panels, identity dossiers, profile-action strips, and comparison pickers retain their restrained 8px corners; the shared Player scope band is square with horizontal boundary rules. The player dossier portrait is deliberately rectangular: 6px top corners, 2px bottom corners, and a thin warm-gray border. Game-analysis navigation uses flat underlined links. On the landing page, full-width color fields and continuous rules define the hierarchy; court geometry, marks, team logos, and player imagery are meaningful basketball content, not interchangeable decoration.

Editorial dossier sheets are square-edged. Their form comes from page-scale rectangles, one-pixel rules, inset chapter spacing, and a restrained three-pixel orange annotation—not from card rounding or decorative silhouettes.

## Components

### Buttons

Primary actions generally use deep navy with white text and deepen/lighten within the navy family on hover. The landing-page hero is the deliberate exception: its square orange action is 66px tall on large screens, 58px on phones, and darkens to accent-700 on hover. The accompanying project action and dark-close links are underlined text links with orange directional marks. Retry actions use a more compact 8px by 16px padding. Secondary export actions are white outlined buttons with muted navy text, small labels, and restrained corner rounding. Avoid inventing a single universal button size where the implementation already has contextual sizes.

### Inputs / Fields

Standard fields are white with a blue-gray border, medium corners, 40px height, and small shadow. Compact variants use smaller padding; TeamStats explicitly uses 32px-high controls. Focus changes the border to accent-400 and adds a two-pixel accent-200 ring. Placeholder text uses acb-400. Other controls use local or browser-native focus styling; a universal branded focus rule is not established.

Within analytical Team and Player Tools, fields remove the shadow, use compact 4px corners, and sit on a slate control band. Player Tools use acb-600 for placeholders and muted utility text so labels and empty prompts remain legible. Desktop density may remain compact, but below 768px every form control, segmented option, view tab, analysis-navigation link, and secondary button has a minimum 44px interactive height.

PlayerCombobox adds a search icon, labeled input, keyboard-selectable options, and a white floating list with large shadow and rounded corners. Active and selected options use pale orange. It supports arrow keys, Enter, and Escape and exposes a no-results message. Preserve those behaviors when reproducing its appearance.

### Segmented Controls and Chips

Segmented controls share a bordered, rounded container. Inactive options are white with blue-gray labels; generic selected options use acb-800 and white, while Team view tabs use acb-900 and white. Selection is conveyed with aria-pressed. Game-analysis navigation uses a ruled strip of links with navy text and an orange underline on the current view. Below 480px its three destinations share one grid row. Percentile badges follow the analytical mapping in Colors.

### Cards / Containers

Content containers are white, usually with large corners, a blue-gray border, and 16px or 20px padding. Filter panels have medium corners and 16px padding. Those analytical card conventions do not transfer to the landing page. Its family runway is one white ruled navigation band, and its directory is one continuous white sheet whose numbered family rows and tool links are separated by one-pixel rules. Directory tool links use plain text plus an external-direction arrow; hover and keyboard focus use pale orange and stronger orange text without adding icon tiles, floating shells, or card chrome.

### Landing Analysis Room

The landing signature is the dark live analysis preview joined directly to the slate declaration. It exposes three tabs—Team, Player, and Alineaciones—and updates the evidence in place. Team and Player examples combine compact metric registers with a radar; Team retains a shot-efficiency view and legend, Player states position and archetype, while Alineaciones makes a ruled UCAM Murcia register of the three best and three worst trios primary. The preview is product evidence, not a generic dashboard card: it is flat, full-height within the hero, and uses navy, white, blue-gray rules, orange data marks, and the same Manrope/Inter/JetBrains Mono hierarchy as the application. At tablet and phone widths the preview moves below the declaration; its internal evidence simplifies or stacks without hiding the three tool-family tabs.

The five-family runway remains visible directly beneath the preview. It uses numbered mono marks, Manrope family names, short Inter descriptions, orange arrows, and vertical or horizontal rules. It is five columns on desktop and 2+2+1 below 900px. The concluding project section is a flat navy band with a plain-language open-source statement and two underlined links; it has no statistic tiles, gradient, shadow, or rounded container.

Player analytical sections retain flat white panels with an 8px radius and a one-pixel slate border. Team sections use square white sheets with horizontal boundary rules; shared Team headers use Manrope titles, compact supporting copy, 20px 22px 16px padding, and a soft divider. Chart-specific controls live directly above the plot; the plot remains the primary visual. In Team Statistics, scope controls are separated from the axes toolbar and joined to the chart sheet.

The player identity dossier joins two materials inside one outlined 8px container. Its warm court-paper lead holds a rectangular portrait, Manrope name, typographic position, and exact team-season context; a three-pixel orange baseline closes that lead. The adjacent white sheet presents career facts as a two-column ruled definition list, collapsing to one column on narrow phones. Similarity signals, comparison summaries, and metric evidence reuse the same dense one-pixel rule language instead of becoming separate elevated cards.

### Public Dossier and Reference Pages

The public dossier is the reusable reading pattern for project, method, provenance, and reference material beneath the shared OpenACB header and on the established slate ground. Its first viewport pairs a large declaration with a flat white register whose numbered entries sit on a vertical rule. The reading body pairs a ruled chapter index with one square-edged white document; sections are separated by full-width rules, not individual card shells. Compact orange marks identify the first or active entry, links use underlines and orange arrows, and reference definitions use native `details`/`summary` rows with a rotating chevron. The material is paper and rules: no gradient, routine shadow, pill container, or card grid belongs in this pattern.

On tablets the hero and register stack, then the index becomes a visible grid above the document. On phones the index uses two columns, the document runs edge to edge, definition rows become one column, and author/contact subgrids collapse without changing source order. All links and disclosures keep a 44px interaction floor, and reduced-motion mode removes nonessential transitions.

### Navigation

A white top bar pairs the existing logo and wordmark with six navigation entries: Equipos, Jugadores, Comparativas, Alineaciones, Tiro, and Proyecto. Grouped entries use 12px labels with dropdown chevrons; Proyecto is a direct link, while dropdown destinations use small arrow-up-right icons. Active links use darker orange and semibold text. Dropdowns use white fill, blue-gray borders, rounded corners, and large shadows. The desktop groups open on button activation and close on pointer exit, loss of focus, or Escape. On mobile, a labeled toggle reveals grouped links in a scrollable menu; route changes close it.

### Analytical Tool Shells and Control Bands

The Team Tool shell frames the team analytical family. Its split masthead states the question and scope, and its four equal tool links combine a 17px outline icon, a compact label, and a short descriptor on white. The active tool uses a warm accent-50 fill, navy text, an orange icon, and a two-pixel orange underline. On desktop, vertical separators divide the four columns; below 768px, the same links form a visible 2x2 rail with both vertical and horizontal separators.

The Player Tool shell is a direct continuation of the same frame with five destinations: Estadísticas, Perfil, Similitud, Comparar, and Clutch. Its white rail uses 17px icons, labels, descriptors, and dividers; the active link has a warm accent-50 fill, navy text, an orange icon, and a two-pixel orange underline. Links use 10px by 14px padding. Its responsive grid remains five columns, then 3+2, then 2+2+1. Optional masthead actions are white outlined links with 4px corners, muted navy text, and a 36px minimum height, increasing to 44px below 768px. Hover strengthens the border and text on acb-50; keyboard focus uses the shell's orange outline. Scope and actions stack below 1024px.

The control band groups scope-setting inputs before the result. Generic bands retain an analysis-slate fill, a one-pixel border, an 8px radius, white fields, and no shadow. Player scope bands use square edges and horizontal boundary rules; successive rows in Player Statistics have an inset divider and 12px top padding. The Team variant is square with horizontal boundary rules; its chart toolbar uses the lighter acb-50 fill. Mobile wrapping must preserve the label-control relationship and the 44px interaction floor.

The Team Profile entry state lists actual available teams as native links that retain season and chart context. Its ruled directory is three columns, two below 1024px, and one below 480px. The selected team's warm-paper identity and white trend evidence form one square dossier; radar, metric, shot, and roster sheets retain that square edge. Game fixtures align each team with its score on the same row, and highlighted runs use ruled rows. Quarter tables open joined detail sheets through a team-name button with an expanded state; timeout summaries use a ruled three-column statistical strip.

### Lineup Tools

The [Lineup Tool shell](openacb_react/src/components/LineupToolShell.jsx) exposes Análisis On/Off and Rankings through a white ruled rail. Each link has an 18px outline icon, a label, and a short descriptor; the current link uses warm paper, navy text, and a two-pixel orange underline. Scope copy uses acb-600, as do labels and muted utilities, so the full-season context remains legible on the slate ground. The flat working band retains white, shadow-free fields with 4px corners. Below 768px fields, search, category and ordering buttons, selection-removal buttons, Con/Sin actions, clear, return, secondary, and retry controls have a 44px interaction floor; removal buttons are 44px square.

[Lineup Analysis](openacb_react/src/pages/LineupAnalysis.jsx) places the selected rotation in a warm-paper strip above the ruled roster pool. Selected players remain visible while other players expose Con/Sin relationships. Navy identity headers join directly to white columnar On/Off evidence or metric strips; roster summaries and explanations use white ruled sheets. Performance markers pair restrained 8px dots with explicit text labels. Preserve the existing green, rose, orange, and slate statistical meanings, sample-size cues, inverse-metric interpretation, and numeric formatting.

[Lineup Rankings](openacb_react/src/pages/LineupRankings.jsx) joins season and team scope, four categories, best/worst ordering, applicable metric selection, and the minimum-minute note inside one slate band. Selected category and ordering controls use navy and white. Results use a white header and continuous table sheet. The results subtitle follows the actual ordering: impact On/Off or net efficiency on court, including team-filtered and five-player cases. Keep that statement tied to the data ordering rather than merely to a retained toggle preference.

The lineup refresh's initial finish disposition was FIX. The final scoped review resolved ranking-subtitle truth, phone target size, and scope-text contrast and returned SHIP. Reviewed desktop, mobile, and 1272px screenshots are recorded in the sidecar and [lineup brief](.impeccable/lineup-tools-brief.md). User visual acceptance remains pending; no tests were added or run.

### Page Headers and Data Tables

PageHeader combines a headline, optional subtitle, optional scope, and optional actions. It does not introduce a card or divider of its own. Analytical Tool shells use a split masthead variant with the title opposite explanatory copy and optional scope/actions; it stacks before the rail below 768px.

Data tables use compact grouped headers, pale header fills, white rows, hover shading, aligned numbers, and sticky identity columns. Player directories, career tables, similarity results, clutch evidence, and comparison tables keep dense rows and one-pixel rules; role/category color is applied to the relevant words, while numeric color continues to follow its stated analytical mapping. Keep sorting, explanations, export controls, and units attached to their existing data context. The sidecar table is a visual specimen with illustrative values, not an analytical result.

On narrow screens, keep the table at its useful analytical width inside a keyboard-focusable horizontal overflow frame. Collapse surrounding grids and toolbars before compressing table labels or numeric columns past readability.

### Charts and Imagery

Existing Recharts plots, FIBA court views, shot maps, team logos, and player photographs provide product-specific content. Chart colors can be assigned at the component level and may differ from the interface palette. Retain each chart's legend and relevant team or metric mapping; a palette cleanup must not erase those distinctions. Lucide supplies the main outline icon vocabulary, generally at 14–20px, while the logo and official social marks remain separate assets.

Charts sit in flat white panels with a separate slate control strip when controls are needed. Their primary plotting area should receive the available space; supporting legends, notes, and table evidence remain attached to the same analytical context. Responsive changes may reduce chart height and collapse surrounding grids, but must not silently change domains, encodings, or calculations.

## Do's and Don'ts

### Do:
- **Do** retain the existing OpenACB logo, Spanish interface, page structure, and color palettes.
- **Do** preserve the landing page as a full-width Sala de análisis: slate declaration, navy live Team/Player/Lineups preview, five-part numbered runway, continuous ruled directory, and dark flat open-source close.
- **Do** reuse the shared header, form, and table patterns when extending an existing analytical page.
- **Do** preserve chart legends, percentile thresholds, inverse-metric interpretations, and numeric formatting.
- **Do** support phone lookups and desktop exploration through the existing responsive layout and table scrolling.
- **Do** use the analysis-desk shell, the correct visible family rail, slate control bands, and flat data sheets for related analytical tools.
- **Do** use the joined warm-paper/white-sheet dossier for reusable player identity and career context.
- **Do** use the editorial dossier for substantial public or reference material that benefits from a declaration, visible contents, and continuous ruled reading.
- **Do** keep phone controls at least 44px tall and collapse surrounding grids before compromising data readability.
- **Do** disable nonessential transitions and smooth scrolling when reduced motion is requested.
- **Do** treat this document as an incumbent baseline; obtain a new direction before replacing its visual identity.

### Don't:
- **Don't** replace the landing analysis room with a centered logo hero, generic card grid, gradient banner, floating statistics tiles, or shadowed containers.
- **Don't** introduce decorative marketing eyebrow labels on the landing page; compact uppercase text is reserved for analytical metadata inside the live preview.
- **Don't** interchange navigation orange, positive/negative colors, and percentile colors; their roles differ.
- **Don't** infer approval for new typography, colors, or decorative motifs from this documentation request.
- **Don't** add generic AI-styled decoration at the expense of basketball information or access to tools.
- **Don't** hide either analytical tool rail on phones or compress it into a single horizontal row; retain Team 2x2 and Player 2+2+1.
- **Don't** turn player positions or archetypes into passive colored pills; the shipped category treatment is typographic.
- **Don't** use orange as a passive panel fill or add shadows to routine analytical sheets.
- **Don't** decorate an editorial dossier with gradients, routine shadows, pill containers, or a card grid; use rules, spacing, and a restrained orange annotation to expose structure.
- **Don't** describe native or inconsistent focus and disabled states as a complete, verified accessibility system.

## September 2026 polish

Tool pages now lead with the tool title and compact navigation. Repeated introductory prose and static scope footnotes have been removed; functional filters, loading states, chart meanings, and analytical data remain in place. Selected tool links use orange text on the warm background, retain that background on hover, and have no accent underline.

The landing examples omit redundant radar captions, decorative orange rules, duplicate player-role explanations, and lineup commentary. The directory uses unnumbered category headings and concise tool descriptions. The shared header uses the same dimensions on every route at each breakpoint; its ACB wordmark uses #fe5917. Desktop navigation underlines cover the label alone, excluding the chevron.

The information-page index follows the section at the reading position and exposes its selection through aria-current="location". On narrow layouts the index stays in document flow so it does not cover the content.

## Team radar percentiles

Team radars and the landing team example share `utils/teamRadar.js` and `components/TeamRadar.jsx`. Radars use empirical midpoint-of-ties percentiles, reverse lower-is-better metrics, exclude missing values, and no longer transform or cap z-scores. The default reference uses teams from the selected season. Histórico optionally pools all available team-season observations (currently 180, 2016–17 to 2025–26), persisted through `referencia=historico` in shared links. This is a raw historical distribution, not an adjustment for era or schedule. The existing narrative and metric-table z-scores retain their current-season reference, identified in the table heading.

Radar values sit with wrapped outer labels, with a dashed 50th-percentile ring. Mobile labels enlarge and wrap within the chart. The landing team view uses the same radar labels and data without a duplicate metric register. Refresh its snapshot with `npm run refresh:home-team`; the standard test suite checks parity against the linked team and source export. Player profiles are unchanged by this pass.
