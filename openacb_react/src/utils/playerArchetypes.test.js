import { readFileSync } from 'node:fs'
import { describe, expect, test } from 'vitest'
import { classifyArchetype, getScopedArchetypePlayer } from './playerArchetypes'

function makePlayer(overrides = {}) {
  return {
    qualified: true,
    position: 'Alero',
    mpg: 22,
    ppgPct: 40,
    tsPct: 50,
    usgPct: 40,
    astPctPct: 30,
    astPctPosPct: 50,
    astToRatioPosPct: 50,
    trbPctPct: 40,
    trbPctPosPct: 40,
    stlPctPct: 40,
    blkPctPct: 40,
    blkPctPosPct: 40,
    orbPctPct: 40,
    orbPctPosPct: 40,
    threeRatePct: 50,
    fg3PctPct: 50,
    fga3: 10,
    assistedFgm: 0.70,
    assistedFgm3: 0.80,
    ...overrides,
  }
}

function archetype(overrides) {
  return classifyArchetype(makePlayer(overrides), null).name
}

describe('shooting archetypes', () => {
  test('requires top-quartile rate and at least 20 attempts', () => {
    const shooter = {
      threeRatePct: 75,
      fg3PctPct: 70,
      fga3: 20,
    }
    expect(archetype(shooter)).toBe('Francotirador')
    expect(archetype({ ...shooter, threeRatePct: 74.9 })).not.toBe('Francotirador')
    expect(archetype({ ...shooter, fga3: 19 })).not.toBe('Francotirador')
  })

  test('separates elite, spot-up, inefficient, and off-dribble shooting', () => {
    const volume = { threeRatePct: 80, fga3: 50 }
    expect(archetype({ ...volume, fg3PctPct: 70 })).toBe('Francotirador')
    expect(archetype({ ...volume, fg3PctPct: 69, assistedFgm3: 0.80 })).toBe('Especialista spot-up')
    expect(archetype({ ...volume, fg3PctPct: 34.9 })).toBe('Tirador Ineficiente')
    expect(archetype({ ...volume, fg3PctPct: 70, assistedFgm3: 0.59 })).toBe('Tirador tras Bote')
  })

  test('separates efficient 3&d from lower-efficiency defensive shooting', () => {
    const defender = {
      position: 'Alero',
      threeRatePct: 80,
      fga3: 50,
      stlPctPct: 80,
      assistedFgm3: 0.80,
    }
    expect(archetype({ ...defender, fg3PctPct: 60 })).toBe('3&D')
    expect(archetype({ ...defender, fg3PctPct: 60, assistedFgm3: 0.59 })).toBe('Creador 3&D')
    expect(archetype({ ...defender, fg3PctPct: 59.9 })).toBe('Defensor spot-up')
  })

  test('allows power forwards but keeps centers in the stretch-big taxonomy', () => {
    const player = {
      threeRatePct: 80,
      fg3PctPct: 70,
      fga3: 50,
      stlPctPct: 80,
      trbPctPct: 60,
      trbPctPosPct: 60,
      blkPctPct: 50,
      blkPctPosPct: 50,
    }
    expect(archetype({ ...player, position: 'Ala-pívot' })).toBe('3&D')
    expect(archetype({ ...player, position: 'Pívot' })).toBe('Interior con Tiro')
  })

  test('requires viable accuracy for stretch-big labels', () => {
    const center = {
      position: 'Pívot',
      threeRatePct: 80,
      fga3: 50,
      trbPctPct: 60,
      trbPctPosPct: 60,
      blkPctPct: 50,
      blkPctPosPct: 50,
    }
    expect(archetype({ ...center, fg3PctPct: 60 })).toBe('Interior con Tiro')
    expect(archetype({ ...center, fg3PctPct: 59.9 })).not.toBe('Interior con Tiro')
  })

  test('uses shooting rather than rebounding to identify stretch centers', () => {
    expect(archetype({
      position: 'Pívot',
      threeRatePct: 75,
      fg3PctPct: 60,
      fga3: 20,
      trbPctPosPct: 20,
      blkPctPosPct: 80,
    })).toBe('Interior con Tiro')
  })
})

describe('playmaking archetypes', () => {
  test('uses exact-position percentiles for passing bigs', () => {
    const big = {
      position: 'Pívot',
      astPctPct: 50,
      trbPctPct: 50,
      trbPctPosPct: 50,
      usgPct: 50,
    }
    expect(archetype({ ...big, astPctPosPct: 90 })).toBe('Interior Creador')
    expect(archetype({ ...big, astPctPct: 99, astPctPosPct: 89.9 })).not.toBe('Interior Creador')
    expect(archetype({ ...big, astPctPct: 99, astPctPosPct: null })).not.toBe('Interior Creador')
  })

  test('reserves interior creator for centers', () => {
    expect(archetype({
      position: 'Ala-pívot',
      astPctPct: 75,
      astPctPosPct: 95,
      astToRatioPosPct: 70,
      apg: 2,
      trbPctPct: 60,
      usgPct: 50,
    })).toBe('Point Forward')
  })

  test('uses ast:tov as a point-guard role modifier', () => {
    const guard = {
      position: 'Base',
      astPctPct: 80,
      usgPct: 50,
      ppgPct: 40,
    }
    expect(archetype({ ...guard, astToRatioPosPct: 60 })).toBe('Organizador Puro')
    expect(archetype({ ...guard, astToRatioPosPct: 34.9 })).toBe('Creador de Juego')
  })

  test('keeps lower-control scoring creators in the scorer taxonomy', () => {
    expect(archetype({
      position: 'Base',
      astPctPct: 90,
      astToRatioPosPct: 20,
      usgPct: 85,
      ppgPct: 80,
      trbPctPct: 60,
      mpg: 25,
    })).toBe('Creador de Tiros-Organizador')
  })

  test('uses position-appropriate labels for combo guards and forwards', () => {
    expect(archetype({
      position: 'Escolta',
      astPctPct: 80,
      astToRatioPosPct: 60,
      usgPct: 50,
      ppgPct: 40,
    })).toBe('Combo Guard Organizador')

    expect(archetype({
      position: 'Alero',
      astPctPct: 80,
      astPctPosPct: 85,
      astToRatioPosPct: 90,
      apg: 1.5,
    })).toBe('Point Forward')

    expect(archetype({
      position: 'Alero',
      astPctPct: 80,
      astPctPosPct: 50,
      astToRatioPosPct: 90,
    })).not.toBe('Organizador Puro')
  })

  test('requires genuine ball-handling production from power forwards', () => {
    const pointForward = {
      position: 'Ala-pívot',
      astPctPct: 70,
      astPctPosPct: 90,
      astToRatioPosPct: 60,
      apg: 1.5,
      trbPctPct: 60,
      usgPct: 50,
    }

    expect(archetype(pointForward)).toBe('Point Forward')
    expect(archetype({ ...pointForward, astPctPosPct: 89.9 })).not.toContain('Point Forward')
    expect(archetype({ ...pointForward, astPctPct: 69.9 })).not.toContain('Point Forward')
    expect(archetype({ ...pointForward, astToRatioPosPct: 59.9 })).not.toContain('Point Forward')
    expect(archetype({ ...pointForward, apg: 1.49 })).not.toContain('Point Forward')
  })

  test('accepts controlled secondary creation from wings', () => {
    const secondaryWing = {
      position: 'Alero',
      astPctPct: 60,
      astPctPosPct: 85,
      astToRatio: 1.40,
      astToRatioPosPct: 60,
      apg: 1.5,
    }

    expect(archetype(secondaryWing)).toBe('Point Forward')
    expect(archetype({ ...secondaryWing, astPctPct: 59.9 })).not.toContain('Point Forward')
    expect(archetype({ ...secondaryWing, astToRatio: 1.39 })).not.toContain('Point Forward')
  })
})

describe('defensive priority', () => {
  test('selects total and versatile defenders before the generic specialist', () => {
    expect(archetype({ stlPctPct: 86, blkPctPct: 86 })).toBe('Defensor Total')
    expect(archetype({ stlPctPct: 80, blkPctPct: 75 })).toBe('Defensor Polivalente')
    expect(archetype({ stlPctPct: 80, blkPctPct: 50 })).toBe('Especialista Defensivo')
  })

  test('does not infer perimeter specialization from a center steal percentile', () => {
    expect(archetype({
      position: 'Pívot',
      stlPctPct: 99,
      blkPctPosPct: 50,
      trbPctPosPct: 30,
    })).not.toBe('Especialista Defensivo')
  })
})

describe('center position benchmarks', () => {
  test('does not infer an interior role without positive position evidence', () => {
    expect(archetype({
      position: null,
      heightM: null,
      trbPctPct: 85,
      blkPctPct: 95,
      usgPct: 40,
    })).toBe('Jugador de Rotación')
  })

  test('uses center percentiles for total rebounding and rim protection', () => {
    const center = {
      position: 'Pívot',
      trbPctPct: 99,
      blkPctPct: 99,
      trbPctPosPct: 40,
      blkPctPosPct: 40,
    }
    expect(archetype(center)).toBe('Interior de Rotación')
    expect(archetype({ ...center, position: 'Ala-pívot' })).toBe('Protector del Aro')
    expect(archetype({
      ...center,
      trbPctPct: 10,
      blkPctPct: 10,
      trbPctPosPct: 80,
      blkPctPosPct: 80,
    })).toBe('Protector del Aro')
  })

  test('uses Ancla for elite rebounding and rim protection before Protector del Aro', () => {
    const center = {
      position: 'Pívot',
      trbPctPosPct: 85,
      blkPctPosPct: 85,
    }
    expect(archetype(center)).toBe('Ancla')
    expect(archetype({ ...center, trbPctPosPct: 84.9 })).toBe('Protector del Aro')
    expect(archetype({ ...center, blkPctPosPct: 84.9 })).toBe('Protector del Aro')
  })

  test('uses center percentiles for offensive rebounding', () => {
    const center = {
      position: 'Pívot',
      trbPctPosPct: 80,
      blkPctPosPct: 50,
      orbPctPct: 99,
      orbPctPosPct: 69.9,
    }
    expect(archetype(center)).not.toBe('Aspiradora')
    expect(archetype({ ...center, orbPctPct: 10, orbPctPosPct: 70 })).toBe('Aspiradora')
  })
})

describe('rebounding wing archetypes', () => {
  test('extends the rebounding-wing role to shooting guards', () => {
    const guard = {
      position: 'Escolta',
      trbPctPct: 60,
      blkPctPct: 50,
    }
    expect(archetype(guard)).toBe('Alero Reboteador')
    expect(archetype({ ...guard, blkPctPct: 49.9 })).not.toBe('Alero Reboteador')
    expect(archetype({ ...guard, trbPctPct: 75, blkPctPct: 40 })).toBe('Alero Reboteador')
  })
})

describe('rotation fallbacks', () => {
  test('recognizes productive sub-18-minute bench scorers just below top-quintile usage', () => {
    const benchScorer = {
      position: 'Escolta',
      mpg: 17.9,
      ppgPct: 80,
      usgPct: 75,
    }

    expect(archetype(benchScorer)).toBe('Sexto Hombre')
    expect(archetype({ ...benchScorer, ppgPct: 79.9 })).toBe('Jugador de Rol')
    expect(archetype({ ...benchScorer, usgPct: 74.9 })).toBe('Jugador de Rol')
  })

  test('classifies meaningful minutes by responsibility rather than efficiency', () => {
    expect(archetype({
      position: 'Escolta',
      mpg: 20,
      ppgPct: 80,
      usgPct: 80,
      tsPct: 34.9,
    })).toBe('Anotador de Volumen')
    expect(archetype({ position: 'Pívot', mpg: 18 })).toBe('Interior de Rotación')
    expect(archetype({
      position: 'Escolta',
      mpg: 18,
      ppg: 8,
      usg: 18,
    })).toBe('Anotador de Rotación')
    expect(archetype({ position: 'Alero', mpg: 18, ppg: 7.9, usg: 17.9 })).toBe('Jugador de Rotación')
    expect(archetype({ position: 'Alero', mpg: 17.9, ppg: 12, usg: 25 })).toBe('Jugador de Rol')
  })

  test('uses efficiency only in the description', () => {
    const makeVolumeScorer = tsPct => classifyArchetype(makePlayer({
      position: 'Escolta',
      mpg: 20,
      ppgPct: 80,
      usgPct: 80,
      tsPct,
    }), null)

    expect(makeVolumeScorer(34.9).desc).toContain('limitada')
    expect(makeVolumeScorer(35).desc).toContain('media')
    expect(makeVolumeScorer(64.9).desc).toContain('media')
    expect(makeVolumeScorer(65).desc).toContain('buena')
    expect(new Set([34.9, 35, 64.9, 65].map(value => makeVolumeScorer(value).name))).toEqual(
      new Set(['Anotador de Volumen'])
    )
  })

  test('keeps role descriptions inside their tested dimensions', () => {
    const compulsiveScorer = classifyArchetype(makePlayer({
      position: 'Escolta',
      mpg: 20,
      ppgPct: 80,
      usgPct: 70,
      astPctPct: 40,
      threeRatePct: 30,
      tsPct: 95,
    }), null)
    const completeGuard = classifyArchetype(makePlayer({
      position: 'Base',
      ppgPct: 66,
      usgPct: 80,
      astPctPct: 85,
      trbPctPct: 40,
      blkPctPct: 20,
      tsPct: 10,
    }), null)
    const modernCenter = classifyArchetype(makePlayer({
      position: 'Pívot',
      ppgPct: 80,
      usgPct: 70,
      astPctPct: 70,
      trbPctPosPct: 80,
      blkPctPosPct: 10,
      threeRatePct: 25,
      mpg: 20,
    }), null)

    expect(compulsiveScorer.name).toBe('Anotador Compulsivo')
    expect(compulsiveScorer.desc).toContain('buena eficiencia')
    expect(completeGuard.name).toBe('Base Completo')
    expect(completeGuard.desc).not.toContain('eficien')
    expect(modernCenter.name).toBe('Pívot Moderno Estrella')
    expect(modernCenter.desc).not.toContain('protege')
  })

  test('keeps specialist archetypes ahead of rotation fallbacks', () => {
    expect(archetype({
      position: 'Alero',
      mpg: 18,
      ppg: 9,
      usg: 18,
      threeRatePct: 75,
      fg3PctPct: 70,
      fga3: 20,
    })).toBe('Francotirador')
  })
})

describe('exported player regressions', () => {
  const playersUrl = new URL('../../public/data/players.json', import.meta.url)
  const playersByStageUrl = new URL('../../public/data/players-by-stage.json', import.meta.url)
  const players = JSON.parse(readFileSync(playersUrl, 'utf8'))
  const playersByStage = JSON.parse(readFileSync(playersByStageUrl, 'utf8'))
  const qualified = players.filter(player => player.qualified && player.competitionStage === 'all')

  test('classifies Aaron Doornekamp in 2022-23 from corrected midrank percentiles', () => {
    const doornekamp = qualified.find(player => (
      player.season === 2023 && /Doornekamp/i.test(player.playerFull || '')
    ))
    expect(doornekamp).toBeDefined()
    expect(doornekamp.stlPctPct).toBeLessThan(80)
    expect(classifyArchetype(doornekamp, null).name).toBe('Francotirador')
  })

  test('keeps profile archetypes in the selected competition stage', () => {
    const regularDoornekamp = playersByStage.find(player => (
      player.season === 2023
      && player.competitionStage === 'regular'
      && /Doornekamp/i.test(player.playerFull || '')
    ))
    const scopedDoornekamp = getScopedArchetypePlayer(regularDoornekamp)
    expect(regularDoornekamp).toBeDefined()
    expect(scopedDoornekamp.competitionStage).toBe('regular')
    expect(classifyArchetype(scopedDoornekamp, null).name).toBe('Francotirador')
  })

  test('classifies Wilhelm Falk as a rebounding wing', () => {
    const falk = qualified.find(player => (
      player.season === 2026 && /Falk/i.test(player.playerFull || '')
    ))
    expect(falk).toBeDefined()
    expect(falk.position).toBe('Escolta')
    expect(classifyArchetype(falk, null).name).toBe('Alero Reboteador')
  })

  test('classifies Dustin Sleva as an all-around power forward', () => {
    const sleva = qualified.find(player => (
      player.season === 2024 && /Dustin.*Sleva/i.test(player.playerFull || '')
    ))
    expect(sleva).toBeDefined()
    expect(classifyArchetype(sleva, null).name).toBe('Ala-Pívot Versátil')
  })

  test('classifies Howard Sant-Roos in 2025-26 as a defensive point forward', () => {
    const santRoos = qualified.find(player => (
      player.season === 2026 && /Sant-roos/i.test(player.playerFull || '')
    ))
    expect(santRoos).toBeDefined()
    expect(classifyArchetype(santRoos, null).name).toBe('Point-Forward Defensivo')
  })

  test('classifies Matt Costello in 2025-26 as a stretch center', () => {
    const costello = qualified.find(player => (
      player.season === 2026 && /Costello/i.test(player.playerFull || '')
    ))
    expect(costello).toBeDefined()
    expect(costello.position).toBe('Pívot')
    expect(classifyArchetype(costello, null).name).toBe('Interior con Tiro')
  })

  test('classifies Cate and Best by their rotation responsibility', () => {
    const cate = qualified.find(player => (
      player.season === 2026 && /Cate/i.test(player.playerFull || '')
    ))
    const best = qualified.find(player => (
      player.season === 2026 && /Aaron Matthew Best/i.test(player.playerFull || '')
    ))

    expect(cate).toBeDefined()
    expect(best).toBeDefined()
    expect(classifyArchetype(cate, null).name).toBe('Interior de Rotación')
    expect(classifyArchetype(best, null).name).toBe('Anotador de Rotación')
  })

  test('classifies Jaycee Carroll in 2019-20 as a sixth man', () => {
    const carroll = qualified.find(player => (
      player.season === 2020 && /Carroll/i.test(player.playerFull || '')
    ))

    expect(carroll).toBeDefined()
    expect(carroll.mpg).toBeLessThan(18)
    expect(carroll.ppgPct).toBeGreaterThanOrEqual(80)
    expect(carroll.usgPct).toBeGreaterThanOrEqual(75)
    expect(classifyArchetype(carroll, null).name).toBe('Sexto Hombre')
  })

  test('classifies Nikola Mirotic in 2019-20 as a scoring star', () => {
    const mirotic = qualified.find(player => (
      player.season === 2020 && /Nikola Mirotic/i.test(player.playerFull || '')
    ))

    expect(mirotic).toBeDefined()
    expect(mirotic.ppgPct).toBeGreaterThanOrEqual(90)
    expect(mirotic.tsPct).toBeGreaterThanOrEqual(70)
    expect(mirotic.usgPct).toBeGreaterThanOrEqual(90)
    expect(classifyArchetype(mirotic, null).name).toBe('Estrella Anotadora')
  })

  test('no qualified player with meaningful minutes remains a generic role player', () => {
    qualified.forEach(player => {
      if (player.mpg >= 18) {
        expect(classifyArchetype(player, null).name).not.toBe('Jugador de Rol')
      }
    })
  })

  test('preserves missing shooting rates when there are no attempts', () => {
    const zeroAttemptShooters = qualified.filter(player => player.fga3 === 0)
    expect(zeroAttemptShooters.length).toBeGreaterThan(0)
    zeroAttemptShooters.forEach(player => {
      expect(player.fg3Pct).toBeNull()
      expect(player.fg3PctPct).toBeNull()
      expect(player.fg3PctPosPct).toBeNull()
    })
  })

  test('does not route unknown-position players into interior roles', () => {
    const interiorRoles = new Set([
      'Ancla',
      'Aspiradora',
      'Bestia en la Zona',
      'Coche Escoba',
      'Creador de Tiros Interior',
      'Interior Anotador',
      'Interior de Rol Completo',
      'Intimidador Interior',
      'Protector del Aro',
    ])
    const unknownPosition = qualified.filter(player => !player.position?.trim())
    expect(unknownPosition.length).toBeGreaterThan(0)
    unknownPosition.forEach(player => {
      expect(interiorRoles.has(classifyArchetype(player, null).name)).toBe(false)
    })
  })

  test('embeds available bio fields in the player export', () => {
    expect(players.filter(player => player.heightM != null).length).toBeGreaterThan(3000)
    expect(players.filter(player => player.birthDate != null).length).toBeGreaterThan(3000)
  })

  test('all generated shooting and passing labels satisfy their invariants', () => {
    qualified.forEach(player => {
      const name = classifyArchetype(player, null).name
      const isBig = ['Ala-pívot', 'Pívot'].includes(player.position)
      const shootingAccuracyPct = isBig
        ? (player.fg3PctPosPct ?? player.fg3PctPct)
        : player.fg3PctPct
      if (name === 'Francotirador') {
        expect(player.threeRatePct).toBeGreaterThanOrEqual(75)
        expect(player.fga3).toBeGreaterThanOrEqual(20)
        expect(shootingAccuracyPct).toBeGreaterThanOrEqual(70)
      }
      if (name === '3&D' || name === 'Creador 3&D') {
        expect(player.threeRatePct).toBeGreaterThanOrEqual(75)
        expect(player.fga3).toBeGreaterThanOrEqual(20)
        expect(shootingAccuracyPct).toBeGreaterThanOrEqual(60)
        expect(player.stlPctPct >= 80 || player.blkPctPct >= 75).toBe(true)
        expect(['Base', 'Escolta', 'Alero', 'Ala-pívot']).toContain(player.position)
      }
      if (name === 'Defensor spot-up') {
        expect(shootingAccuracyPct).toBeLessThan(60)
        expect(player.assistedFgm3).toBeGreaterThanOrEqual(0.75)
      }
      if (name === 'Interior con Tiro') {
        expect(['Ala-pívot', 'Pívot']).toContain(player.position)
        expect(player.fg3PctPosPct ?? player.fg3PctPct).toBeGreaterThanOrEqual(60)
      }
      if (name === 'Interior Creador') {
        expect(player.position).toBe('Pívot')
        expect(player.astPctPosPct).toBeGreaterThanOrEqual(90)
      }
      if (name === 'Point Forward' || name === 'Point-Forward Defensivo') {
        expect(['Alero', 'Ala-pívot']).toContain(player.position)
        expect(player.astPctPosPct).toBeGreaterThanOrEqual(player.position === 'Ala-pívot' ? 90 : 85)
        if (player.astPctPct < 70) {
          expect(player.position).toBe('Alero')
          expect(player.astPctPct).toBeGreaterThanOrEqual(60)
          expect(player.astToRatio).toBeGreaterThanOrEqual(1.40)
        }
        expect(player.astToRatioPosPct).toBeGreaterThanOrEqual(60)
        expect(player.apg).toBeGreaterThanOrEqual(1.5)
      }
      if (name === 'Organizador Puro') {
        expect(player.position).toBe('Base')
        expect(player.astToRatioPosPct).toBeGreaterThanOrEqual(60)
      }
      if (name === 'Especialista Defensivo') {
        expect(['Base', 'Escolta', 'Alero', 'Ala-pívot']).toContain(player.position)
      }
    })
  })

  test('all qualified centers include position rebounding and rim-protection percentiles', () => {
    const centers = qualified.filter(player => player.position === 'Pívot')
    expect(centers.length).toBeGreaterThan(0)
    centers.forEach(player => {
      expect(player.trbPctPosPct).not.toBeNull()
      expect(player.orbPctPosPct).not.toBeNull()
      expect(player.blkPctPosPct).not.toBeNull()
    })
  })
})
