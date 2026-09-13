# OpenACB

<!-- impeccable:product-schema 1 -->

## Platform

web

## Users

OpenACB serves two audiences, both confirmed by the owner:

- Casual basketball fans interested in their team and league statistics.
- Analytically minded basketball users who value advanced statistics and tools, together with clarity and easy access to information.

Neither audience is a secondary afterthought. Analytical depth must remain available without making straightforward questions difficult to answer.

## Product Purpose

Help visitors independently answer basketball questions using ACB statistics and analytical tools. A successful visit starts with a question and gives the visitor enough access to relevant evidence to reach their own answer.

Representative questions supplied by the owner:

- How good is this player?
- From where does this team shoot?
- How does this lineup perform, or how do these players play together?
- Who leads in this statistic?

OpenACB is an exploration and reference tool, not a teaching platform. Casual visitors may learn advanced indicators through navigation and use. Teaching sequences, lessons, or onboarding curricula are not its purpose.

## Operating Context

The owner expects analytical users to work mainly on desktop and casual users mainly on phones. These are product expectations, not measured usage findings.

Quick lookups are expected to be more frequent. The platform also supports longer exploration, especially by advanced users. Both types of visit should work without forcing visitors into a prescribed analytical sequence.

Desktop should accommodate sustained comparison and detailed inspection. Mobile should make common questions and their answers easy to reach and read, while retaining access to the analytical tools.

## Positioning

The existing README and About page describe a free, open-source collection of advanced Liga Endesa / ACB analytical tools using play-by-play data from acb.com. These are existing project claims, not findings from a competitive study. The product makes statistics and tools available for visitors to investigate questions themselves.

## Capabilities and Constraints

The existing application includes team and player statistics and profiles, game analysis, four factors, quarter splits, clutch statistics, player similarity and comparisons, team comparisons, lineup and On/Off analysis, lineup rankings, shot charts, and zone leaders.

The current main navigation groups are Equipos, Jugadores, Comparativas, Alineaciones, and Tiro, with project information accessible through Proyecto.

Owner-confirmed boundaries for design work:

- Preserve the current page structure and color palettes.
- Preserve the existing analytical functionality and the information needed to use it.
- Layouts within pages may be rearranged, and substantial visual changes are permitted within those boundaries.
- Avoid the generic appearance of an LLM-designed site.

Treat routes, filters, metrics, data meanings, and chart encodings as functional requirements. A visual redesign must not silently alter statistical calculations or analytical interpretations.

## Brand Commitments

Keep the existing OpenACB identity and the owner's explicit palette and structure constraints. The current interface is in Spanish and uses the existing OpenACB logo. The owner wants originality that serves the site's basketball and analytical purpose.

The established OpenACB visual world and the Manrope/Inter/JetBrains type hierarchy documented in DESIGN.md and the surface briefs are the current baseline. The discarded redesign does not establish a design precedent.

## Evidence on Hand

- Owner interview in this task: audiences, representative questions, expected devices, and usage patterns.
- `README.md` and `openacb_react/src/pages/About.jsx`: existing purpose, positioning, and explanations of the tools.
- `openacb_react/src/routing/`: page structure and URL behavior.
- `openacb_react/src/pages/` and `openacb_react/src/components/`: implemented workflows and controls.
- `openacb_react/public/data/`: exported analytical data used by the application.
- `openacb_react/tailwind.config.js` and `openacb_react/src/index.css`: incumbent palettes and style definitions.
- `openacb_react/public/openacb_nobckg.png`: existing logo.

No usage analytics or user-testing results were supplied for this interview. Do not present expected audience behavior as measured evidence.

## Product Principles

1. Make the path from a basketball question to relevant evidence easy to find.
2. Keep quick lookups efficient while supporting deeper, self-directed exploration.
3. Preserve analytical depth and statistical meaning as clarity improves.
4. Serve casual phone users and analytical desktop users intentionally.
5. Let familiarity with advanced indicators develop through use; do not turn the product into a course.
