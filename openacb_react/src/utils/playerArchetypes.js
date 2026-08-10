export function getScopedArchetypePlayer(player) {
  return player || null
}

export function classifyArchetype(player, bio) {
  if (!player || player.qualified === false) {
    return {
      name: 'Datos insuficientes',
      desc: 'No cumple el mínimo de partidos o minutos para calcular el arquetipo',
      color: 'text-acb-400 bg-acb-50 border-acb-200',
    }
  }

  const ppg = player.ppgPct ?? 50
  const ts = player.tsPct ?? 50
  const usg = player.usgPct ?? 50
  const ast = player.astPctPct ?? 50
  const astPos = player.astPctPosPct ?? null
  const astToRatio = player.astToRatio ?? null
  const astToPos = player.astToRatioPosPct ?? null
  const trbLeague = player.trbPctPct ?? 50
  const stl = player.stlPctPct ?? 50
  const blkLeague = player.blkPctPct ?? 50
  const orbLeague = player.orbPctPct ?? 50
  const trbPos = player.trbPctPosPct ?? null
  const blkPos = player.blkPctPosPct ?? null
  const orbPos = player.orbPctPosPct ?? null
  const thr = player.threeRatePct ?? 50
  const fg3League = player.fg3PctPct ?? 50
  const fg3Pos = player.fg3PctPosPct ?? null
  const fga3 = player.fga3 ?? 0
  const apg = player.apg ?? 0
  const mpg = player.mpg

  // use the record first and keep the bio lookup as a compatibility fallback
  const cleanPosition = value => (
    typeof value === 'string' && value.trim() ? value.trim() : null
  )
  const position = cleanPosition(player.position) || cleanPosition(bio && bio.position)
  const height = (() => {
    const value = player.heightM ?? (bio && bio.heightM)
    return value != null && isFinite(value) ? value : null
  })()

  const isGuardPos = position === 'Base' || position === 'Escolta'
  const isPointGuard = position === 'Base'
  const isSecondGuard = position === 'Escolta'
  const isWingPos = position === 'Alero'
  const isBigPos = position === 'Ala-pívot' || position === 'Pívot'
  const isCenterPos = position === 'Pívot'
  const isPFPos = position === 'Ala-pívot'
  const isPerimeterRole = isGuardPos || isWingPos || isPFPos
  const fg3 = isBigPos ? (fg3Pos ?? fg3League) : fg3League

  // centers are evaluated against centers for rebounding and interior defense
  const trb = isCenterPos ? (trbPos ?? 50) : trbLeague
  const blk = isCenterPos ? (blkPos ?? 50) : blkLeague
  const orb = isCenterPos ? (orbPos ?? 50) : orbLeague

  const isTall = height != null && height >= 2.00
  const isVeryTall = height != null && height >= 2.08

  const astdFgm = player.assistedFgm != null && isFinite(player.assistedFgm)
    ? player.assistedFgm
    : null
  const astdFgm3 = player.assistedFgm3 != null && isFinite(player.assistedFgm3)
    ? player.assistedFgm3
    : null

  const isSelfCreator = astdFgm != null && astdFgm < 0.40
  const isOffDribble3 = astdFgm3 != null && astdFgm3 < 0.60
  const isSpotUp3 = astdFgm3 != null && astdFgm3 >= 0.75

  const isHighVolume = usg >= 80
  const isScorer = ppg >= 80 && usg >= 70
  const isEfficient = ts >= 75
  const isPlaymaker = ast >= 75
  const isRebounder = trb >= 80
  const isRimProtector = blk >= 75
  const isPerimDefender = stl >= 80
  const isStrongDefender = isPerimDefender || isRimProtector
  const isAllAround = ppg >= 50 && ast >= 50 && blk >= 50 && stl >= 50 && trb >= 40
  const isVersatilePowerForward = (isPFPos || (isWingPos && isTall))
    && ppg >= 65
    && trb >= 65
    && thr > 25

  // shooting roles require both a high rate and a stable attempt sample
  const isHighThreeVolume = thr >= 75 && fga3 >= 20
  const isEliteThreeAccuracy = fg3 >= 70
  const isViableThreeAccuracy = fg3 >= 60
  const isVeryPoorThreeAccuracy = fg3 < 35
  const scoringEfficiency = ts >= 65
    ? 'buena eficiencia anotadora'
    : ts < 35
      ? 'eficiencia anotadora limitada'
      : 'eficiencia anotadora media'

  // ball security modifies point-guard roles without disqualifying star labels
  const isControlledPointGuard = isPointGuard && astToPos != null && astToPos >= 60
  const isControlledComboGuard = isSecondGuard && astToPos != null && astToPos >= 60
  const isRiskyPointGuard = isPointGuard && astToPos != null && astToPos < 35
  const isControlledSecondaryWingCreator = isWingPos
    && ast >= 60
    && astToRatio != null
    && astToRatio >= 1.40

  // point forwards must create at a meaningful absolute level, not only for their position
  const isPointForwardCreator = (isWingPos || isPFPos)
    && astPos != null
    && astPos >= (isPFPos ? 90 : 85)
    && (ast >= 70 || isControlledSecondaryWingCreator)
    && astToPos != null
    && astToPos >= 60
    && apg >= 1.5

  // passing centers are exceptional relative to their exact listed position
  if (isCenterPos && astPos != null && astPos >= 90 && !isScorer && usg < 80 && trb >= 50) {
    return {
      name: 'Interior Creador',
      desc: 'Interior con visión de juego excepcional para su posición que facilita el ataque',
      color: 'text-info-700 bg-info-50 border-info-200',
    }
  }

  if (usg >= 80 && ts < 10) {
    return {
      name: 'Mandarinas',
      desc: 'Mandarinero de élite. Tira tanto como falla.',
      color: 'text-negative-700 bg-negative-50 border-negative-200',
    }
  }

  if (isScorer && isPlaymaker && isEfficient && isPerimDefender && thr > 20 && mpg >= 20 && !isRebounder && isPointGuard) {
    return {
      name: 'Base Estrella',
      desc: 'Anota, crea, defiende y lo hace todo con eficiencia de élite',
      color: 'text-accent-700 bg-accent-50 border-accent-200',
    }
  }

  if (isPlaymaker && isPerimDefender && usg >= 75 && ppg >= 85 && thr > 20 && mpg >= 20 && !isRebounder && !isCenterPos && isSecondGuard) {
    return {
      name: 'Combo Guard Todoterreno Élite',
      desc: 'Escolta con gran defensa perimetral y capaz de anotar con gran volumen',
      color: 'text-accent-700 bg-accent-50 border-accent-200',
    }
  }

  if (isPlaymaker && isPerimDefender && usg >= 75 && ppg >= 85 && thr > 20 && mpg >= 20 && !isRebounder && isPointGuard) {
    return {
      name: 'Base Todoterreno Élite',
      desc: 'Anotador y creador estrella con defensa perimetral de alto nivel',
      color: 'text-accent-700 bg-accent-50 border-accent-200',
    }
  }

  if (ppg >= 90 && usg >= 90 && thr >= 35 && ast < 70 && ts >= 70 && mpg >= 20) {
    return {
      name: 'Estrella Anotadora',
      desc: 'Anotador élite de alto volumen y eficiencia',
      color: 'text-accent-700 bg-accent-50 border-accent-200',
    }
  }

  if (isScorer && isPlaymaker && isEfficient && trb < 50 && mpg >= 20 && isPointGuard) {
    return {
      name: 'Base Dominador',
      desc: 'Anota, crea para otros y lo hace con eficiencia',
      color: 'text-info-700 bg-info-50 border-info-200',
    }
  }

  if (isPlaymaker && isPerimDefender && usg >= 75 && ppg >= 75 && blk < 50 && !isRebounder && isSelfCreator) {
    return {
      name: 'Creador de Tiros Polivalente',
      desc: 'Creador de juego y anotador con defensa perimetral de alto nivel',
      color: 'text-sage-700 bg-sage-50 border-sage-200',
    }
  }

  if (ast >= 95 && isHighVolume && ppg >= 70 && trb < 80 && isControlledPointGuard) {
    return {
      name: 'General en la Pista',
      desc: 'Creador de alto volumen que encuentra a sus compañeros y protege el balón',
      color: 'text-gold-700 bg-gold-50 border-gold-200',
    }
  }

  if (ast >= 80 && usg >= 75 && ppg >= 80 && trb < 80 && mpg >= 20 && isSecondGuard) {
    return {
      name: 'Combo Guard Anotador',
      desc: 'Escolta con buena capacidad de organizar el juego y alto volumen anotador',
      color: 'text-gold-700 bg-gold-50 border-gold-200',
    }
  }

  if (ast >= 80 && isHighVolume && ppg >= 75 && trb < 80 && mpg >= 20 && isGuardPos) {
    return {
      name: 'Creador de Tiros-Organizador',
      desc: 'Creador de alto octanaje que también habilita a sus compañeros',
      color: 'text-gold-700 bg-gold-50 border-gold-200',
    }
  }

  if (isScorer && thr >= 40 && ast >= 50 && ast < 70 && stl < 75 && isEfficient && isSelfCreator) {
    return {
      name: 'Anotador Autosuficiente',
      desc: 'Genera y convierte su propio tiro con alto volumen y eficiencia',
      color: 'text-gold-700 bg-gold-50 border-gold-200',
    }
  }

  if (isScorer && thr >= 40 && ast >= 50 && ast < 70 && stl < 75 && isEfficient) {
    return {
      name: 'Anotador Eficiente',
      desc: 'Anotador de alto volumen y alta eficiencia',
      color: 'text-gold-700 bg-gold-50 border-gold-200',
    }
  }

  if (isScorer && thr >= 30 && ast >= 40 && stl < 75 && mpg >= 20) {
    return {
      name: 'Anotador Compulsivo',
      desc: `Anotador de gran volumen con ${scoringEfficiency}`,
      color: 'text-gold-700 bg-gold-50 border-gold-200',
    }
  }

  if (ppg > 85 && thr >= 30 && ast < 50 && stl < 75 && ts >= 70 && isSelfCreator) {
    return {
      name: 'Anotador Puro',
      desc: 'Anotador eficiente capaz de generar sus propios tiros',
      color: 'text-gold-700 bg-gold-50 border-gold-200',
    }
  }

  if (ppg > 85 && thr >= 30 && ast < 50 && stl < 75 && ts >= 70) {
    return {
      name: 'Finalizador Eficiente',
      desc: 'Anotador eficiente que convierte oportunidades sin responsabilidades de creación',
      color: 'text-gold-700 bg-gold-50 border-gold-200',
    }
  }

  if (isScorer && thr >= 40 && ast <= 40 && stl < 75 && mpg >= 20 && isWingPos) {
    return {
      name: 'Alero Anotador',
      desc: 'Alero con gran volumen anotador y sin responsabilidades de creación',
      color: 'text-gold-700 bg-gold-50 border-gold-200',
    }
  }

  if (isPlaymaker && isPerimDefender && !isScorer && ppg < 80 && isGuardPos) {
    return {
      name: 'Creador de Juego Defensivo',
      desc: 'Organiza el ataque y lidera la defensa perimetral',
      color: 'text-sage-700 bg-sage-50 border-sage-200',
    }
  }

  if (isPointForwardCreator && isPerimDefender && !isScorer && ppg < 80) {
    return {
      name: 'Point-Forward Defensivo',
      desc: 'Organiza el ataque y lidera la defensa perimetral',
      color: 'text-sage-700 bg-sage-50 border-sage-200',
    }
  }

  // route genuine forward ball handlers before the pure-organizer fallback
  if (isPointForwardCreator) {
    return {
      name: 'Point Forward',
      desc: 'Alero o ala-pívot que maneja el balón y crea ventajas para sus compañeros',
      color: 'text-info-700 bg-info-50 border-info-200',
    }
  }

  if (isPointGuard && ast >= 75 && usg < 80 && !isScorer) {
    if (isControlledPointGuard) {
      return {
        name: 'Organizador Puro',
        desc: 'Prioriza la asistencia, organiza el ataque y protege el balón',
        color: 'text-info-700 bg-info-50 border-info-200',
      }
    }
    return {
      name: 'Creador de Juego',
      desc: isRiskyPointGuard
        ? 'Genera juego para sus compañeros asumiendo un riesgo elevado de pérdida'
        : 'Genera juego para sus compañeros con margen para mejorar el control del balón',
      color: 'text-info-700 bg-info-50 border-info-200',
    }
  }

  if (isSecondGuard && ast >= 75 && usg < 80 && !isScorer) {
    if (isControlledComboGuard) {
      return {
        name: 'Combo Guard Organizador',
        desc: 'Escolta que asume la creación y cuida el balón',
        color: 'text-info-700 bg-info-50 border-info-200',
      }
    }
    return {
      name: 'Creador de Juego',
      desc: 'Genera juego desde el puesto de escolta con control de balón irregular',
      color: 'text-info-700 bg-info-50 border-info-200',
    }
  }

  if (isScorer && (stl >= 75 || blk >= 65) && ast >= 50 && ast < 70 && trb < 75 && isSecondGuard) {
    return {
      name: 'Escolta Two-Way',
      desc: 'Anotador con impacto defensivo élite',
      color: 'text-sage-700 bg-sage-50 border-sage-200',
    }
  }

  if (isScorer && isEfficient && ast < 65 && ast >= 50 && trb < 75 && isGuardPos) {
    return {
      name: 'Combo Guard Anotador',
      desc: 'Anotador eficiente puro, genera poco para los demás',
      color: 'text-gold-700 bg-gold-50 border-gold-200',
    }
  }

  if (ast >= 85 && usg > 70 && ppg > 65 && trb < 50 && blk < 30 && isPointGuard) {
    return {
      name: 'Base Completo',
      desc: 'Organiza y anota con buen volumen',
      color: 'text-info-700 bg-info-50 border-info-200',
    }
  }

  if (ppg >= 70 && trb >= 70 && stl >= 70 && thr > 20 && ppg > 90 && isWingPos) {
    return {
      name: 'Alero Dominante',
      desc: 'Estrella anotadora que también contribuye en defensa y rebote',
      color: 'text-accent-700 bg-accent-50 border-accent-200',
    }
  }

  if (ppg >= 70 && trb >= 70 && stl >= 70 && thr > 20 && ppg < 90 && trb < 90 && stl < 90 && isWingPos) {
    return {
      name: 'Alero Completo',
      desc: 'Contribuye de forma equilibrada en ataque, rebote y defensa',
      color: 'text-sage-700 bg-sage-50 border-sage-200',
    }
  }

  if (trb >= 70 && ppg >= 60 && ppg < 75 && blk < 70 && isPerimDefender && stl < 90 && thr > 20 && isWingPos) {
    return {
      name: 'Alero Defensivo Completo',
      desc: 'Rebotea y anota sin ser el foco de atención',
      color: 'text-sage-700 bg-sage-50 border-sage-200',
    }
  }

  if (isScorer && thr < 20 && !isRimProtector && stl > 30) {
    return {
      name: 'Finalizador Interior',
      desc: 'Anotador agresivo atacando el aro',
      color: 'text-gold-700 bg-gold-50 border-gold-200',
    }
  }

  // shooting and 3&d roles precede generic defensive labels
  if (!isScorer && isPerimeterRole && isHighThreeVolume && isStrongDefender) {
    if (isViableThreeAccuracy && isOffDribble3) {
      return {
        name: 'Creador 3&D',
        desc: 'Genera su tiro exterior con eficiencia y defiende a alto nivel',
        color: 'text-sage-700 bg-sage-50 border-sage-200',
      }
    }
    if (isViableThreeAccuracy) {
      return {
        name: '3&D',
        desc: 'Aporta tiro exterior eficiente y defensa de alto nivel',
        color: 'text-sage-700 bg-sage-50 border-sage-200',
      }
    }
    if (isSpotUp3) {
      return {
        name: 'Defensor spot-up',
        desc: 'Especialista defensivo que finaliza posesiones exteriores tras pase',
        color: 'text-positive-700 bg-positive-50 border-positive-200',
      }
    }
  }

  if (!isScorer && !isCenterPos && ast < 65 && isHighThreeVolume) {
    if (isVeryPoorThreeAccuracy) {
      return {
        name: 'Tirador Ineficiente',
        desc: 'Concentra muchos tiros en el perímetro con un porcentaje muy bajo',
        color: 'text-negative-700 bg-negative-50 border-negative-200',
      }
    }
    if (isEliteThreeAccuracy && isOffDribble3) {
      return {
        name: 'Tirador tras Bote',
        desc: 'Genera y convierte su propio tiro exterior con eficiencia',
        color: 'text-sand-700 bg-sand-50 border-sand-200',
      }
    }
    if (isEliteThreeAccuracy) {
      return {
        name: 'Francotirador',
        desc: 'Especialista exterior de alto volumen y eficiencia',
        color: 'text-sand-700 bg-sand-50 border-sand-200',
      }
    }
    if (isSpotUp3) {
      return {
        name: 'Especialista spot-up',
        desc: 'Finaliza tras pase con alto volumen exterior y eficiencia intermedia',
        color: 'text-sand-700 bg-sand-50 border-sand-200',
      }
    }
  }

  // qualified shooting centers stay in the stretch-big taxonomy
  const isStretchCenter = isCenterPos && isHighThreeVolume && isViableThreeAccuracy && !isScorer
  const isStretchPowerForward = isPFPos
    && isHighThreeVolume
    && isViableThreeAccuracy
    && trb >= 50
    && blk < 60
    && !isScorer
  if (isStretchCenter || isStretchPowerForward) {
    return {
      name: 'Interior con Tiro',
      desc: 'Interior que abre el campo con una amenaza exterior eficiente',
      color: 'text-plum-700 bg-plum-50 border-plum-200',
    }
  }

  if (blk >= 85 && stl > 85) {
    return {
      name: 'Defensor Total',
      desc: 'Defensor de élite tanto en la zona como en el perímetro',
      color: 'text-positive-700 bg-positive-50 border-positive-200',
    }
  }

  if (isRimProtector && isPerimDefender) {
    return {
      name: 'Defensor Polivalente',
      desc: 'Impacto defensivo interior y perimetral',
      color: 'text-positive-700 bg-positive-50 border-positive-200',
    }
  }

  if (isVersatilePowerForward) {
    return {
      name: 'Ala-Pívot Versátil',
      desc: 'Ala-pívot con capacidad de anotar, rebotear y abrir el campo',
      color: 'text-plum-700 bg-plum-50 border-plum-200',
    }
  }

  if (isPerimeterRole && isPerimDefender && !isScorer && !isRimProtector) {
    return {
      name: 'Especialista Defensivo',
      desc: 'Especialista en robos y presión en el perímetro',
      color: 'text-positive-700 bg-positive-50 border-positive-200',
    }
  }

  if (ppg >= 90 && ast >= 90 && trb >= 90) {
    return {
      name: 'Amenaza de Triple-Doble',
      desc: 'Amenaza en anotación, asistencias y rebotes',
      color: 'text-accent-700 bg-accent-50 border-accent-200',
    }
  }

  if (ppg >= 85 && usg >= 85 && (blk >= 80 || stl >= 80) && thr >= 25 && mpg >= 20) {
    return {
      name: 'Estrella Two-Way',
      desc: 'Dominante en ataque y defensa',
      color: 'text-accent-700 bg-accent-50 border-accent-200',
    }
  }

  if (usg >= 65 && ast >= 70 && stl >= 70 && ppg >= 70 && usg >= 70 && thr > 20 && !isRebounder && isPointGuard) {
    return {
      name: 'Base Polivalente',
      desc: 'Base versátil con anotación, creación y defensa',
      color: 'text-info-700 bg-info-50 border-info-200',
    }
  }

  if (isScorer && trb >= 78 && blk > 70 && ast < 75 && mpg >= 20 && isCenterPos) {
    return {
      name: 'Pívot Estrella',
      desc: 'Domina la zona, intimida y lleva el peso ofensivo del equipo',
      color: 'text-accent-700 bg-accent-50 border-accent-200',
    }
  }

  if (isScorer && trb >= 80 && ast >= 70 && mpg >= 20 && isCenterPos) {
    return {
      name: 'Pívot Moderno Estrella',
      desc: 'Anota en la pintura, rebotea y habilita a sus compañeros con volumen de estrella',
      color: 'text-accent-700 bg-accent-50 border-accent-200',
    }
  }

  if (isScorer && trb >= 80 && ast >= 70 && isCenterPos) {
    return {
      name: 'Pívot Moderno',
      desc: 'Anota en la pintura, rebotea y habilita a sus compañeros',
      color: 'text-plum-700 bg-plum-50 border-plum-200',
    }
  }

  if (isScorer && isRebounder && blk < 70 && thr < 40 && isBigPos && isSelfCreator) {
    return {
      name: 'Creador de Tiros Interior',
      desc: 'Crea sus propios tiros en la zona con eficiencia y volumen',
      color: 'text-gold-700 bg-gold-50 border-gold-200',
    }
  }

  if (isScorer && isRebounder && blk < 70 && thr < 40 && isBigPos) {
    return {
      name: 'Coche Escoba',
      desc: 'Finalizador interior con poca capacidad de generar sus propios tiros',
      color: 'text-gold-700 bg-gold-50 border-gold-200',
    }
  }

  if (isCenterPos && trb >= 85 && blk >= 85 && !isScorer) {
    return {
      name: 'Ancla',
      desc: 'Interior de élite en rebote y protección del aro que sostiene la defensa',
      color: 'text-plum-700 bg-plum-50 border-plum-200',
    }
  }

  if (isRebounder && blk >= 80 && ts >= 85 && ppg > 75 && isBigPos) {
    return {
      name: 'Bestia en la Zona',
      desc: 'Domina la zona con rebotes y protección de aro, anotando con eficiencia',
      color: 'text-gold-700 bg-gold-50 border-gold-200',
    }
  }

  if (blk >= 70 && trb >= 70 && ppg >= 85 && isBigPos) {
    return {
      name: 'Interior Anotador',
      desc: 'Rebotea, protege el aro y anota en alto volumen',
      color: 'text-gold-700 bg-gold-50 border-gold-200',
    }
  }

  if ((isRebounder || (isCenterPos && trb >= 72)) && blk >= 80 && usg < 60 && isBigPos) {
    return {
      name: 'Protector del Aro',
      desc: 'Protector interior eficaz sin responsabilidades ofensivas',
      color: 'text-plum-700 bg-plum-50 border-plum-200',
    }
  }

  if (blk >= 90 && !isScorer && isBigPos) {
    return {
      name: 'Intimidador Interior',
      desc: 'Presencia defensiva cerca del aro con tapones',
      color: 'text-plum-700 bg-plum-50 border-plum-200',
    }
  }

  if (trb >= 75 && blk >= 60 && isHighThreeVolume && isViableThreeAccuracy && (isCenterPos || (position == null && isVeryTall))) {
    return {
      name: 'Pívot Abierto',
      desc: 'Grande que abre el campo con tiro exterior eficiente',
      color: 'text-plum-700 bg-plum-50 border-plum-200',
    }
  }

  if (trb >= 80 && ppg >= 75 && usg > 65 && ts > 80 && isPFPos && isSelfCreator) {
    return {
      name: 'Ala-Pívot Anotador',
      desc: 'Rebotea y crea su propia anotación con gran eficiencia',
      color: 'text-plum-700 bg-plum-50 border-plum-200',
    }
  }

  if (trb >= 80 && ppg >= 75 && usg > 65 && ts > 80 && isPFPos) {
    return {
      name: 'Ala Pívot Finalizador',
      desc: 'Rebotea y finaliza tras pase con gran eficiencia',
      color: 'text-plum-700 bg-plum-50 border-plum-200',
    }
  }

  if (blk >= 70 && trb >= 70 && ppg >= 70 && isBigPos) {
    return {
      name: 'Interior de Rol Completo',
      desc: 'Rebotea, protege el aro y anota sin ser el foco de atención',
      color: 'text-plum-700 bg-plum-50 border-plum-200',
    }
  }

  if (trb >= 70 && ppg >= 70 && usg < 60 && isPFPos) {
    return {
      name: 'Ala Pívot de Rol',
      desc: 'Rebotea y anota sin ser el foco de atención',
      color: 'text-plum-700 bg-plum-50 border-plum-200',
    }
  }

  if ((isRebounder || (isCenterPos && trb >= 72)) && orb >= 70 && !isScorer && isBigPos) {
    return {
      name: 'Aspiradora',
      desc: 'Dominador del rebote ofensivo y defensivo',
      color: 'text-sand-700 bg-sand-50 border-sand-200',
    }
  }

  if (blk >= 70 && trb >= 70 && !isScorer && isCenterPos) {
    return {
      name: 'Pívot de Rol',
      desc: 'Cumple su función de protector del aro y reboteador',
      color: 'text-plum-700 bg-plum-50 border-plum-200',
    }
  }

  if ((isWingPos || isSecondGuard) && trb >= 60 && (blk >= 50 || trb >= 75)) {
    return {
      name: 'Alero Reboteador',
      desc: 'Exterior con impacto en el rebote y la defensa interior',
      color: 'text-sage-700 bg-sage-50 border-sage-200',
    }
  }

  if (usg >= 85 && mpg < 20 && ts >= 75) {
    return {
      name: 'Microondas',
      desc: 'Anota eficientemente y en alto volumen en minutos limitados',
      color: 'text-sand-700 bg-sand-50 border-sand-200',
    }
  }

  if (mpg < 20 && (usg >= 80 || (mpg < 18 && ppg >= 80 && usg >= 75))) {
    return {
      name: 'Sexto Hombre',
      desc: 'Foco principal de la segunda unidad con alto volumen y minutos limitados',
      color: 'text-sand-700 bg-sand-50 border-sand-200',
    }
  }

  if (isAllAround) {
    return {
      name: 'Todoterreno',
      desc: 'Contribuye en todas las facetas del juego',
      color: 'text-info-700 bg-info-50 border-info-200',
    }
  }

  // rotation fallbacks describe responsibility; efficiency only qualifies the description
  if (mpg >= 18 && ppg >= 80 && usg >= 80) {
    return {
      name: 'Anotador de Volumen',
      desc: `Asume una carga ofensiva alta con ${scoringEfficiency}`,
      color: 'text-gold-700 bg-gold-50 border-gold-200',
    }
  }

  if (mpg >= 18 && isBigPos) {
    return {
      name: 'Interior de Rotación',
      desc: `Interior habitual de la rotación con ${scoringEfficiency}`,
      color: 'text-plum-700 bg-plum-50 border-plum-200',
    }
  }

  if (mpg >= 18 && (isGuardPos || isWingPos) && player.ppg >= 8 && player.usg >= 18) {
    return {
      name: 'Anotador de Rotación',
      desc: `Aporta volumen anotador secundario con ${scoringEfficiency}`,
      color: 'text-gold-700 bg-gold-50 border-gold-200',
    }
  }

  if (mpg >= 18) {
    return {
      name: 'Jugador de Rotación',
      desc: `Pieza habitual de la rotación con ${scoringEfficiency}`,
      color: 'text-acb-700 bg-acb-50 border-acb-200',
    }
  }

  if ((stl >= 50 || blk >= 50) && trb >= 40 && mpg < 20) {
    return {
      name: 'Pegamento',
      desc: 'Jugador de equipo que contribuye con esfuerzo defensivo en minutos limitados',
      color: 'text-acb-700 bg-acb-50 border-acb-200',
    }
  }

  return {
    name: 'Jugador de Rol',
    desc: 'Cumple una función limitada en el esquema del equipo',
    color: 'text-acb-700 bg-acb-50 border-acb-200',
  }
}
