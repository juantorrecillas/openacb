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

  test.each([
    ['Pívot', 25],
    ['Ala-pívot', 50],
  ])('recognizes %s shooting below perimeter-specialist volume', (position, rate) => {
    const big = {
      position, mpg: 16, threeRatePct: rate, fga3: 20,
      fg3PctPosPct: 60, trbPctPct: 50,
    }
    expect(archetype(big)).toBe('Interior con Tiro')
    for (const change of [
      { threeRatePct: rate - 0.1 }, { threeRatePct: null },
      { fga3: 19 }, { fg3PctPosPct: 59.9 },
    ]) {
      expect(archetype({ ...big, ...change })).not.toBe('Interior con Tiro')
    }
  })
})

describe('playmaking archetypes', () => {
  test('uses exact-position percentiles for passing bigs', () => {
    const big = {
      position: 'Pívot',
      astPctPct: 50,
      trbPctPct: 50,
      trbPctPosPct: 1,
      astToRatioPosPct: 60,
      usgPct: 50,
    }
    expect(archetype({ ...big, astPctPosPct: 85 })).toBe('Interior Creador')
    expect(archetype({ ...big, astPctPosPct: 84.9 })).not.toBe('Interior Creador')
    expect(archetype({ ...big, astPctPosPct: 90, astToRatioPosPct: 59.9 })).not.toBe('Interior Creador')
    expect(archetype({ ...big, astPctPosPct: 90, astToRatioPosPct: null })).not.toBe('Interior Creador')
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

describe('big-man specialist coverage', () => {
  test('recognizes efficient interior scoring without high usage or steals', () => {
    const center = {
      position: 'Pívot', ppgPct: 75, tsPct: 75,
      usgPct: 50, stlPctPct: 1, threeRatePct: 10,
    }
    expect(archetype(center)).toBe('Finalizador Interior')
    expect(archetype({ ...center, ppgPct: 74.9 })).not.toBe('Finalizador Interior')
    expect(archetype({ ...center, tsPct: 74.9 })).not.toBe('Finalizador Interior')
    expect(archetype({ ...center, position: 'Ala-pívot' })).not.toBe('Finalizador Interior')
    expect(archetype({ ...center, ppgPct: 80, usgPct: 70, tsPct: 40 })).toBe('Finalizador Interior')
  })

  test('keeps modern and rebounding scorers ahead of the broader finishing role', () => {
    const center = {
      position: 'Pívot', ppgPct: 80, usgPct: 70, tsPct: 80,
      threeRatePct: 10, trbPctPosPct: 80, blkPctPosPct: 20,
    }
    expect(archetype({ ...center, astPctPct: 70 })).toBe('Pívot Moderno Estrella')
    expect(archetype({ ...center, astPctPct: 70, mpg: 19 })).toBe('Pívot Moderno')
    expect(archetype(center)).toBe('Coche Escoba')
    expect(archetype({ ...center, assistedFgm: 0.3 })).toBe('Creador de Tiros Interior')
  })

  test('recognizes a center blocking specialist without requiring strong rebounding', () => {
    const center = { position: 'Pívot', mpg: 14, trbPctPosPct: 20, blkPctPosPct: 80 }
    expect(archetype(center)).toBe('Intimidador Interior')
    expect(archetype({ ...center, blkPctPosPct: 79.9 })).toBe('Jugador de Rol')
    expect(archetype({ ...center, blkPctPosPct: null })).toBe('Jugador de Rol')
    expect(archetype({ ...center, position: 'Ala-pívot', blkPctPct: 80 })).not.toBe('Intimidador Interior')
  })

  test.each([
    ['Pívot', 'trbPctPosPct', 'orbPctPosPct', 80],
    ['Ala-pívot', 'trbPctPct', 'orbPctPct', 75],
  ])('recognizes %s offensive rebounders with appropriate total rebounding', (position, reb, orb, threshold) => {
    const big = { position, mpg: 14, [reb]: threshold, [orb]: 70 }
    expect(archetype(big)).toBe('Aspiradora')
    expect(archetype({ ...big, [reb]: threshold - 0.1 })).not.toBe('Aspiradora')
    expect(archetype({ ...big, [orb]: 69.9 })).not.toBe('Aspiradora')
    expect(archetype({ ...big, [reb]: null })).not.toBe('Aspiradora')
  })

  test.each(['Pívot', 'Ala-pívot'])('retains the fallback for a %s without a clear specialty', position => {
    expect(archetype({ position, mpg: 16 })).toBe('Jugador de Rol')
    expect(archetype({ position, mpg: 18 })).toBe('Interior de Rotación')
    expect(archetype({ position, qualified: false, blkPctPosPct: 99 })).toBe('Datos insuficientes')
  })
})

describe('center scoring and rebounding styles', () => {
  const scorer = {
    position: 'Pívot', ppgPct: 80, usgPct: 65, tsPct: 60, threeRatePct: 10,
    assistedFgm2: 0.70, fgm2: 60,
    freqRim: 50, freqShortMid: 35, freqLongMid: 10, freqAllMid: 45, freqAllThree: 5,
    fgaRim: 50, fgaShortMid: 35, fgaLongMid: 10, fgaAllMid: 45, fgaAllThree: 5,
    fgpctLongMid: 45, fgpctAllThree: 35,
  }

  test('splits paint scorers using assisted twos even when assisted totals disagree', () => {
    expect(archetype({ ...scorer, assistedFgm: 0.90, assistedFgm2: 0.599 })).toBe('Anotador en el Poste')
    expect(archetype({ ...scorer, assistedFgm: 0.20, assistedFgm2: 0.60 })).toBe('Finalizador Interior')
  })

  test('requires paint concentration for post scoring and rim attempts for assisted finishing', () => {
    expect(archetype({ ...scorer, freqRim: 30, freqShortMid: 34.9, assistedFgm2: 0.50 })).not.toBe('Anotador en el Poste')
    expect(archetype({ ...scorer, freqRim: 39.9, freqShortMid: 45.1 })).not.toBe('Finalizador Interior')
    expect(archetype({ ...scorer, freqRim: 40, freqShortMid: 45 })).toBe('Finalizador Interior')
  })

  test.each([null, undefined, NaN, -0.1, 1.1])('does not infer creation from an invalid assisted-two rate: %s', assistedFgm2 => {
    const name = archetype({ ...scorer, assistedFgm2 })
    expect(['Anotador en el Poste', 'Finalizador Interior']).not.toContain(name)
  })

  test('requires meaningful scoring and a made-shot sample for the paint split', () => {
    expect(archetype({ ...scorer, ppgPct: 69.9 })).not.toBe('Finalizador Interior')
    expect(archetype({ ...scorer, fgm2: 19 })).not.toBe('Finalizador Interior')
    expect(archetype({ ...scorer, fgm2: 20 })).toBe('Finalizador Interior')
    expect(archetype({ ...scorer, usgPct: 59.9 })).not.toBe('Finalizador Interior')
    expect(archetype({ ...scorer, usgPct: 30, tsPct: 75 })).toBe('Finalizador Interior')
  })

  const midrangeScorer = {
    ...scorer, freqRim: 40, freqShortMid: 35, freqLongMid: 25, freqAllMid: 60, freqAllThree: 0,
    fgaLongMid: 25, fgaAllMid: 60, fgaAllThree: 0, fgpctLongMid: 45, fgpctAllThree: null,
  }

  test('recognizes efficient midrange scoring without any three-point attempts', () => {
    expect(archetype(midrangeScorer)).toBe('Pívot Anotador Versátil')
    expect(archetype({ ...midrangeScorer, freqLongMid: 24.9 })).not.toBe('Pívot Anotador Versátil')
    expect(archetype({ ...midrangeScorer, fgaLongMid: 19 })).not.toBe('Pívot Anotador Versátil')
    expect(archetype({ ...midrangeScorer, fgpctLongMid: 44.9 })).not.toBe('Pívot Anotador Versátil')
    expect(archetype({ ...midrangeScorer, fgpctLongMid: null })).not.toBe('Pívot Anotador Versátil')
  })

  test('keeps shots in the non-restricted paint separate from outside scoring', () => {
    expect(archetype({
      ...midrangeScorer, freqRim: 40, freqShortMid: 60, freqLongMid: null,
      fgaLongMid: null, fgpctLongMid: null,
    })).toBe('Finalizador Interior')
  })

  test('uses the three-point value when measuring outside scoring efficiency', () => {
    expect(archetype({
      ...midrangeScorer, freqLongMid: null, fgaLongMid: null, fgpctLongMid: null,
      freqAllThree: 25, fgaAllThree: 25, fgpctAllThree: 35,
    })).toBe('Pívot Anotador Versátil')
  })

  test('keeps a pure perimeter shooter separate from a scorer with inside and outside range', () => {
    expect(archetype({
      ...midrangeScorer, freqRim: 10, freqShortMid: 10, freqAllMid: 20,
      freqLongMid: 10, freqAllThree: 70, fgaAllThree: 70, fgpctAllThree: 40,
      threeRatePct: 90, fga3: 70, fg3PctPosPct: 85,
    })).toBe('Interior con Tiro')
  })

  test('keeps versatile scoring ahead of the post split and generic scoring labels', () => {
    expect(archetype({
      ...midrangeScorer, assistedFgm2: 0.40, usgPct: 80, astPctPct: 50, threeRatePct: 40,
    })).toBe('Pívot Anotador Versátil')
    expect(archetype({ ...scorer, usgPct: 70, trbPctPosPct: 80, astPctPct: 70 })).toBe('Pívot Moderno Estrella')
  })

  test('does not infer the new scoring roles from missing zones or a different position', () => {
    const newScoringRoles = ['Anotador en el Poste', 'Pívot Anotador Versátil']
    expect(newScoringRoles).not.toContain(archetype({ ...midrangeScorer, freqAllMid: null }))
    expect(newScoringRoles).not.toContain(archetype({ ...scorer, assistedFgm2: 0.40, fgaAllMid: null }))
    expect(newScoringRoles).not.toContain(archetype({ ...midrangeScorer, position: 'Ala-pívot' }))
    expect(archetype({ ...scorer, usgPct: 70, freqAllMid: null })).toBe('Finalizador Interior')
  })

  test('distinguishes ordinary rebounding from elite total or offensive rebounding', () => {
    const center = { position: 'Pívot', mpg: 16, trbPctPosPct: 60, orbPctPosPct: 50 }
    expect(archetype(center)).toBe('Pívot Reboteador')
    expect(archetype({ ...center, trbPctPosPct: 59.9 })).not.toBe('Pívot Reboteador')
    expect(archetype({ ...center, trbPctPosPct: null })).not.toBe('Pívot Reboteador')
    expect(archetype({ ...center, usgPct: 60 })).not.toBe('Pívot Reboteador')
    expect(archetype({ ...center, usgPct: null })).not.toBe('Pívot Reboteador')
    expect(archetype({ ...center, orbPctPosPct: 80 })).toBe('Pívot Reboteador')
    expect(archetype({ ...center, trbPctPosPct: 80, orbPctPosPct: 70 })).toBe('Aspiradora')
    expect(archetype({ ...center, trbPctPosPct: 50, orbPctPosPct: 90 })).toBe('Aspiradora')
  })

  test('keeps defensive and passing specialties ahead of ordinary rebounding', () => {
    const center = { position: 'Pívot', trbPctPosPct: 85, orbPctPosPct: 50 }
    expect(archetype({ ...center, blkPctPosPct: 85 })).toBe('Ancla')
    expect(archetype({ ...center, blkPctPosPct: 72 })).toBe('Pívot de Rol')
    expect(archetype({ ...center, astPctPosPct: 95 })).toBe('Interior Creador')
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
  const qualifiedAcrossStages = [...qualified, ...playersByStage.filter(player => player.qualified)]

  test.each([
    [2026, /Shermadini/i, 'Finalizador Interior'],
    [2026, /Pustovyi/i, 'Finalizador Interior'],
    [2026, /Diakite/i, 'Pívot Anotador Versátil'],
    [2026, /Itan Majkl Hap/i, 'Anotador en el Poste'],
    [2025, /Tomic/i, 'Anotador en el Poste'],
    [2024, /Hernangómez/i, 'Anotador en el Poste'],
    [2026, /Cacok/i, 'Finalizador Interior'],
    [2026, /Geben/i, 'Pívot Anotador Versátil'],
    [2024, /Vesely/i, 'Pívot Anotador Versátil'],
    [2026, /Birgander/i, 'Interior Creador'],
    [2026, /Bagayoko/i, 'Pívot Reboteador'],
    [2026, /Youssoupha Birima Fall/i, 'Aspiradora'],
    [2026, /Kravish/i, 'Interior Creador'],
    [2026, /Krutwig/i, 'Interior Creador'],
    [2026, /Golden/i, 'Interior Creador'],
    [2026, /Neal Omar Sako/i, 'Aspiradora'],
    [2026, /Nzosa/i, 'Intimidador Interior'],
    [2026, /Labeyrie/i, 'Interior con Tiro'],
    [2024, /Llovet/i, 'Aspiradora'],
    [2026, /Tavares/i, 'Ancla'],
    [2026, /Burjanadze/i, 'Jugador de Rol'],
  ])('recognizes the %i %s profile as %s', (season, name, expected) => {
    const player = qualified.find(record => record.season === season && name.test(record.playerFull || ''))
    expect(player).toBeDefined()
    expect(classifyArchetype(player, null).name).toBe(expected)
  })

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

  test('all generated specialist labels satisfy their invariants across competition stages', () => {
    qualifiedAcrossStages.forEach(player => {
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
        expect(player.threeRatePct).toBeGreaterThanOrEqual(player.position === 'Pívot' ? 25 : 50)
        expect(player.fga3).toBeGreaterThanOrEqual(20)
        expect(player.fg3PctPosPct ?? player.fg3PctPct).toBeGreaterThanOrEqual(60)
      }
      if (name === 'Aspiradora') {
        expect(isBig).toBe(true)
        const isCenter = player.position === 'Pívot'
        if (isCenter) {
          expect((player.trbPctPosPct >= 80 && player.orbPctPosPct >= 70)
            || (player.trbPctPosPct >= 50 && player.orbPctPosPct >= 90)).toBe(true)
        } else {
          expect(player.trbPctPct).toBeGreaterThanOrEqual(75)
          expect(player.orbPctPct).toBeGreaterThanOrEqual(70)
        }
      }
      if (name === 'Pívot Reboteador') {
        expect(player.position).toBe('Pívot')
        expect(player.trbPctPosPct).toBeGreaterThanOrEqual(60)
        expect(player.usgPct).not.toBeNull()
        expect(player.usgPct).toBeLessThan(60)
      }
      if (name === 'Anotador en el Poste' || name === 'Pívot Anotador Versátil') {
        expect(player.position).toBe('Pívot')
        expect(player.freqAllMid).not.toBeNull()
        expect(player.freqAllThree).not.toBeNull()
        expect(player.ppgPct).toBeGreaterThanOrEqual(70)
        expect(player.usgPct >= 60 || player.tsPct >= 75).toBe(true)
      }
      if (name === 'Anotador en el Poste'
        || (name === 'Finalizador Interior' && player.position === 'Pívot' && player.freqAllMid != null)) {
        expect((player.freqRim ?? 0) + (player.freqShortMid ?? 0)).toBeGreaterThanOrEqual(65)
        expect(player.fgm2).toBeGreaterThanOrEqual(20)
        expect(player.assistedFgm2).not.toBeNull()
        if (name === 'Anotador en el Poste') {
          expect(player.assistedFgm2).toBeLessThan(0.60)
        } else {
          expect(player.assistedFgm2).toBeGreaterThanOrEqual(0.60)
          expect(player.freqRim).toBeGreaterThanOrEqual(40)
        }
      }
      if (name === 'Pívot Anotador Versátil') {
        const midAttempts = player.fgaLongMid ?? 0
        const threeAttempts = player.fgaAllThree
        expect((player.freqRim ?? 0) + (player.freqShortMid ?? 0)).toBeGreaterThanOrEqual(25)
        expect((player.freqLongMid ?? 0) + player.freqAllThree).toBeGreaterThanOrEqual(25)
        expect(midAttempts + threeAttempts).toBeGreaterThanOrEqual(20)
        const outsidePoints = 2 * midAttempts * (player.fgpctLongMid ?? 0) / 100
          + 3 * threeAttempts * (player.fgpctAllThree ?? 0) / 100
        expect(outsidePoints / (midAttempts + threeAttempts)).toBeGreaterThanOrEqual(0.90)
      }
      if (name === 'Intimidador Interior') {
        expect(isBig).toBe(true)
        const isCenter = player.position === 'Pívot'
        expect(isCenter ? player.blkPctPosPct : player.blkPctPct).toBeGreaterThanOrEqual(isCenter ? 80 : 90)
      }
      if (name === 'Interior Creador') {
        expect(player.position).toBe('Pívot')
        expect(player.astPctPosPct).toBeGreaterThanOrEqual(85)
        expect(player.astToRatioPosPct).toBeGreaterThanOrEqual(60)
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
