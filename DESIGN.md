---
name: openACB
description: A data-first Liga Endesa analysis desk built from decisive type, ruled structure, and restrained color.
colors:
  league-navy: "#102a43"
  deep-blue-ink: "#243b53"
  body-blue: "#334e68"
  muted-blue: "#526f8a"
  strong-rule: "#9fb3c8"
  soft-rule: "#bcccdc"
  hairline: "#d9e2ec"
  cool-ground: "#f0f4f8"
  paper: "#ffffff"
  court-orange: "#fe5917"
  orange-active: "#d04313"
  positive: "#2aa867"
  negative: "#dd415d"
  comparison-blue: "#3b82f6"
  archetype-gold: "#b48a10"
  archetype-sage: "#358479"
  archetype-plum: "#8349d8"
  archetype-sand: "#957851"
  lemon-note: "#ece338"
typography:
  display:
    fontFamily: "Barlow Semi Condensed, Arial Narrow, sans-serif"
    fontSize: "clamp(3rem, 5vw, 4.5rem)"
    fontWeight: 600
    lineHeight: 0.94
    letterSpacing: "-0.025em"
  headline:
    fontFamily: "Barlow Semi Condensed, Arial Narrow, sans-serif"
    fontSize: "clamp(2.25rem, 4vw, 3rem)"
    fontWeight: 600
    lineHeight: 1
    letterSpacing: "-0.02em"
  title:
    fontFamily: "Barlow Semi Condensed, Arial Narrow, sans-serif"
    fontSize: "1.5rem"
    fontWeight: 600
    lineHeight: 1.333
  body:
    fontFamily: "Source Sans 3, Segoe UI, sans-serif"
    fontSize: "1rem"
    fontWeight: 400
    lineHeight: 1.5
  label:
    fontFamily: "Source Sans 3, Segoe UI, sans-serif"
    fontSize: "0.75rem"
    fontWeight: 600
    lineHeight: 1.333
    letterSpacing: "0.08em"
  control:
    fontFamily: "Source Sans 3, Segoe UI, sans-serif"
    fontSize: "0.875rem"
    fontWeight: 600
    lineHeight: 1.429
  form:
    fontFamily: "Source Sans 3, Segoe UI, sans-serif"
    fontSize: "0.875rem"
    fontWeight: 400
    lineHeight: 1.429
  table:
    fontFamily: "Source Sans 3, Segoe UI, sans-serif"
    fontSize: "0.75rem"
    fontWeight: 400
    lineHeight: 1.5
  numeric:
    fontFamily: "JetBrains Mono, Consolas, monospace"
    fontSize: "0.75rem"
    fontWeight: 500
    lineHeight: 1.5
rounded:
  none: "0"
  sharp: "2px"
  micro: "4px"
  full: "9999px"
spacing:
  "1": "4px"
  "2": "8px"
  "3": "12px"
  "4": "16px"
  "6": "24px"
  "8": "32px"
  "10": "40px"
  "12": "48px"
components:
  button-primary:
    backgroundColor: "{colors.league-navy}"
    textColor: "{colors.paper}"
    typography: "{typography.control}"
    rounded: "{rounded.none}"
    padding: "8px 16px"
  button-primary-hover:
    backgroundColor: "{colors.deep-blue-ink}"
  field:
    backgroundColor: "{colors.paper}"
    textColor: "{colors.league-navy}"
    typography: "{typography.form}"
    rounded: "{rounded.sharp}"
    padding: "0 12px"
    height: "40px"
  segmented-control:
    backgroundColor: "{colors.paper}"
    textColor: "{colors.muted-blue}"
    typography: "{typography.control}"
    rounded: "{rounded.none}"
    padding: "6px 12px"
  primary-nav:
    backgroundColor: "{colors.paper}"
    textColor: "{colors.muted-blue}"
    typography: "{typography.control}"
    rounded: "{rounded.none}"
    padding: "0 12px"
  analysis-tabs:
    backgroundColor: "transparent"
    textColor: "{colors.muted-blue}"
    typography: "{typography.control}"
    rounded: "{rounded.none}"
    padding: "8px 0"
  page-header:
    textColor: "{colors.league-navy}"
    typography: "{typography.headline}"
    rounded: "{rounded.none}"
    padding: "0 0 20px"
  stat-plaque:
    backgroundColor: "{colors.league-navy}"
    textColor: "{colors.paper}"
    rounded: "{rounded.none}"
    padding: "24px"
  data-table:
    backgroundColor: "{colors.paper}"
    textColor: "{colors.body-blue}"
    typography: "{typography.table}"
    rounded: "{rounded.sharp}"
    padding: "8px"
  chip-archetype:
    backgroundColor: "#fdfbf0"
    textColor: "#744f09"
    typography: "{typography.label}"
    rounded: "{rounded.micro}"
    padding: "2px 8px"
---

# Design System: openACB

## Overview

**Creative North Star: "The Courtside Statistics Desk"**

OpenACB should feel like a working Liga Endesa statistics desk: decisive condensed headlines, ruled information architecture, and dense analytical material arranged for fast scanning. The cool blue-grey ground behaves like editorial stock, while navy anchors identity and orange marks active states and important edges.

Surfaces stay largely flat and rectangular so the basketball evidence remains primary. Restrained overlays are reserved for menus and search results; team marks, player imagery, charts, and data supply the visual personality. The system explicitly avoids generic AI-dashboard gradients, glass effects, icon-tile grids, and stacks of soft floating cards.

**Key Characteristics:**

- Condensed, decisive hierarchy
- Ruled, data-first information architecture
- Cool blue-grey ground with navy and orange identity
- Sharp geometry with restrained depth
- Dense but readable analytical controls and tables

## Colors

The palette is a cool basketball editorial field: blue-grey neutrals carry most of the interface, navy establishes authority, and orange appears sparingly as a signal.

### Primary

- **League Navy:** The darkest brand anchor for headlines, strong panels, buttons, the footer, and high-emphasis text.
- **Deep Blue Ink / Body Blue:** The main reading hierarchy for body copy, labels, navigation, and table content.

### Secondary

- **Court Orange:** The identity accent for the top rule, selected tabs, focus, underlines, and directional details.
- **Orange Active:** The darker orange used for active text and hover emphasis where the main accent needs stronger contrast.

### Tertiary

- **Positive / Negative:** Semantic green and red reserved for analytical meaning, outcomes, and error feedback rather than decoration.
- **Comparison Blue:** The second-series color for comparisons and opposing-team data.
- **Archetype Gold, Sage, Plum, and Sand:** Muted categorical families for player-role chips; use their pale backgrounds, quiet borders, and dark text shades as a unit.
- **Lemon Note:** A rare warm note in the footer hover treatment, inherited from the product's existing voice.

### Neutral

- **Cool Ground / Paper:** The page field and working surfaces. Paper is used for controls, navigation, tables, and selected states.
- **Strong Rule / Soft Rule / Hairline:** The three levels of structural separation, from section boundaries to table rows.
- **Muted Blue:** Secondary copy and inactive navigation; it should remain legible without competing with primary information.

### Named Rules

**The Orange Is a Signal Rule.** Use orange for identity, selection, focus, and directional emphasis; never wash large analytical surfaces in it.

## Typography

**Display Font:** Barlow Semi Condensed (with Arial Narrow and sans-serif fallbacks)

**Body Font:** Source Sans 3 (with Segoe UI and sans-serif fallbacks)

**Label/Mono Font:** JetBrains Mono (with Consolas and monospace fallbacks)

**Character:** Condensed display type gives the product a decisive sports-editorial voice. The humanist body face stays calm at dashboard density, while monospaced numerals make statistics, scopes, and season ranges easy to compare.

### Hierarchy

- **Display** (600, 48px mobile to 72px desktop, 0.94 line-height): Landing-page statements and the largest editorial numbers only.
- **Headline** (600, 36px mobile to 48px desktop, 1 line-height): Tool and page titles with tight tracking.
- **Title** (600, 24px, 32px line-height): Section and category headings.
- **Body** (400, 16px, 24px line-height): Explanations and working copy; long-form passages stay near 70–72 characters per line.
- **Label** (600, 12px, 0.08em letter-spacing, uppercase): Field labels and compact table headers.
- **Control** (600, 14px, 20px line-height): Navigation, tabs, buttons, and compact interactive text; form values may use regular weight.
- **Form** (400, 14px, 20px line-height): Input and select values, increasing to 16px on small screens.
- **Table** (400, 12px, 18px line-height): Dense row content; identity cells may rise to medium weight.
- **Numeric** (500, 12px, 18px line-height): Tabular values, scope strings, dates, and compact statistical notation.

### Named Rules

**The Condensed Hierarchy Rule.** Use the display family for headings and emphatic figures, not for paragraphs or dense table values.

## Layout

The application shell uses a centered 1440px maximum width with responsive horizontal gutters of 16px, 24px, and 32px. Editorial landing and information pages narrow to 1152px. Major sections are separated by horizontal rules and generous vertical intervals, while controls and table cells use a compact 4px-based rhythm.

Layouts stack naturally on small screens, move to paired columns from 1024px where content allows, and expose the full desktop navigation from 1280px. At widths below 640px, fields and segmented controls maintain at least a 44px touch height, dense tables retain 12px data text, and horizontal overflow plus selective sticky identity columns preserves scanability.

**The Evidence First Rule.** Give charts, tables, filters, and definitions the width they need before adding decorative composition.

## Elevation & Depth

This is a flat-by-default system. Depth comes primarily from tonal contrast, one-pixel rules, strong top edges, and dark inset-like panels. Shadows are restricted to the sticky site header and temporary overlays such as dropdowns, the mobile menu, and combobox results.

### Shadow Vocabulary

- **Header Lift** (`0 8px 22px rgba(16, 42, 67, 0.06)`): A barely visible separation beneath the sticky header.
- **Overlay Lift** (`0 14px 30px rgba(16, 42, 67, 0.14)`): Desktop dropdowns and temporary menus.
- **Mobile Overlay Lift** (`0 16px 30px rgba(16, 42, 67, 0.14)`): Full-width mobile navigation.
- **Raised Overlay** (`0 14px 30px rgba(16, 42, 67, 0.16)`): The strongest approved shadow for transient layered content.

### Named Rules

**The Flat-by-Default Rule.** Resting content surfaces have no shadow; elevation is a temporary state, not a card style.

## Shapes

The form language is sharp and editorial. Most application corners resolve to a restrained 2px radius, compact badges may use 4px, and full circles are reserved for avatars, chart marks, or small categorical dots. Structural borders are usually 1px; 2px rules mark selected tabs or category starts, while 3px and 4px orange rules carry brand emphasis.

**The Square Desk Rule.** Prefer crisp rectangles and ruled edges; rounded silhouettes are for data marks and compact tags, not general containers.

## Components

### Buttons

- **Shape:** Square, compact, and direct; the primary action has no visible corner radius.
- **Primary:** League Navy with white text, 8px vertical and 16px horizontal padding, and semibold body typography.
- **Hover / Focus:** Hover shifts to Deep Blue Ink. Keyboard focus uses a 2px Court Orange outline with a 3px offset; active states remain flat without simulated press depth.
- **Secondary / Ghost:** Text actions use underlines, bottom rules, or transparent backgrounds instead of raised neutral pills.

### Chips

- **Style:** Compact 4px-radius labels with a pale categorical background, matching quiet border, and dark category text.
- **State:** Chips communicate classification or value bands. Do not use them as a substitute for primary navigation or ordinary buttons.

### Cards / Containers

- **Corner Style:** Sharp 2px corners where containment is needed; many editorial sections use rules without a card boundary.
- **Background:** Paper for working containers and League Navy for the signature statistics plaque.
- **Shadow Strategy:** Flat at rest; follow the restricted overlay vocabulary in Elevation & Depth.
- **Border:** One-pixel blue-grey rules, often paired with a 2–4px top edge for hierarchy.
- **Internal Padding:** Usually 16–24px, increasing to 28px on roomier breakpoints.

### Inputs / Fields

- **Style:** White 40px controls with a 1px Strong Rule border, 2px corners, 12px horizontal padding, and League Navy text.
- **Focus:** Court Orange border plus a single-pixel orange ring; the global focus outline remains visible.
- **Error / Disabled:** Error containers use the negative pale surface, border, and dark text ramp. Disabled controls must remain visibly distinct without weakening label readability.

### Navigation

- **Primary navigation:** Semibold 14px labels on Paper, with a 2px bottom rule. Active items use Court Orange and League Navy; hover uses a blue-grey rule.
- **Analysis tabs:** A border-bottom row with open spacing rather than a filled pill group. The active tab receives the same 2px orange rule.
- **Mobile treatment:** The desktop row becomes a ruled, full-width grouped menu with 44px minimum targets and restrained overlay depth.

### Page Headers

Large condensed titles sit above a Soft Rule with optional body subtitle, monospaced scope, and right-aligned actions. The header is an information boundary, not a hero card.

### Data Tables

Tables use 12px body values and approximately 10.5–11px uppercase headers, 8–10px cell padding, monospaced tabular numerals, white rows, pale blue-grey headers, and hairline row rules. Hover uses Cool Ground; sticky identity columns keep their inherited row background and gain only a one-pixel edge shadow.

### Tool Rows and Statistics Plaques

Tool links are full-width ruled rows with condensed titles, short descriptions, and a restrained directional arrow. The signature statistics plaque is a sharp League Navy block with a 4px orange top rule, large condensed numerals, monospaced range text, and internal dividers.

**The Ruled Container Rule.** Prefer borders and aligned baselines over nested cards when grouping related analytical content.

## Do's and Don'ts

### Do:

- **Do** put basketball evidence before decorative interface chrome.
- **Do** use the 4px spacing rhythm, sharp corners, and ruled sections to keep dense content orderly.
- **Do** reserve monospaced type for numbers, dates, scopes, and compact statistical notation.
- **Do** preserve full player names wherever identity matters.
- **Do** keep focus visible, touch targets at least 44px on mobile, and dense tables horizontally operable.

### Don't:

- **Don't** introduce gradients, glass effects, icon-tile dashboards, or soft floating card stacks.
- **Don't** use orange as a broad surface color or semantic substitute for positive and negative values.
- **Don't** add shadows to resting analytical containers.
- **Don't** round every panel, control, or navigation item into a pill.
- **Don't** sacrifice labels, units, or methodological context for visual minimalism.
