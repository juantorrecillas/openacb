# Product

<!-- impeccable:product-schema 1 -->

## Platform

web

## Users

- Basketball fans who want to understand Liga Endesa teams and players beyond box-score totals. [Confirmed in `README.md` and the About page.]
- Analysts and coaching staff who need fast access to advanced team, player, lineup, shot, game-flow, and clutch views. [Confirmed in `README.md`; the exact mix and frequency of professional use remain unverified.]

## Product Purpose

OpenACB turns Liga Endesa play-by-play data into free, interactive analytical tools. Success means a user can move from a basketball question to a useful comparison, profile, table, or visualization quickly and leave with a clearer technical understanding of the league.

## Positioning

OpenACB combines a reproducible open-source data pipeline with a broad set of freely available Liga Endesa analysis tools that are commonly placed behind subscriptions or paywalls.

## Operating Context

- Users explore ten seasons of data through team, player, comparison, lineup, shooting, game-flow, quarter-split, and clutch tools.
- The interface is data-dense and is expected to support scanning, filtering, comparing, sorting, and exporting.
- Spanish is the product's current interface language.

## Capabilities and Constraints

- Preserve the existing page and route structure, navigation categories, analytical behavior, and factual copy unless a correction is required.
- Player names must remain fully identifying; do not reduce players to surnames.
- The React frontend consumes static JSON produced by the R ETL pipeline.
- The product must remain responsive and usable for dense tables and charts on smaller screens.

## Brand Commitments

- Preserve the `openACB` name and existing logo assets.
- Preserve the established navy, blue-grey, orange, and semantic data-color palette. [Explicitly confirmed by the user for this redesign.]
- The product is free, advertisement-free, and open source; its tone should remain useful, technically literate, and unpretentious.
- Avoid visual conventions that make the product feel like a generic AI-generated dashboard. [Explicitly confirmed by the user.]

## Evidence on Hand

- Product description and operating claims in `README.md` and `openacb_react/src/pages/About.jsx`.
- Real Liga Endesa data exports in `openacb_react/public/data/`.
- Existing team logos, player photos, and openACB logo assets in the frontend public directory.
- No testimonials, usage metrics, accessibility certification, or professional-adoption evidence should be fabricated.

## Product Principles

- Put basketball evidence before decorative interface chrome.
- Keep dense analytical work fast to scan and straightforward to operate.
- Make advanced statistics accessible without concealing their technical meaning.
- Preserve traceability from open data and code to the displayed analysis.
- Let team, player, and game information carry the visual personality.

## Accessibility & Inclusion

Maintain semantic controls, visible keyboard focus, readable contrast, useful alternative text, and touch-safe control sizing. No product-specific accessibility standard has been confirmed.
