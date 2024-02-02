	ORG $D000
	RORG $F000

	;BANK 1 - CONTAINS THE MAJORITY OF THE BATTLE SYSTEM LOGIC

LReset:
	nop $1FF9 ;Go to bank 3, the startup bank

LBattleProcessLowBytes:
	.byte (LProcessFighting & $FF)
	.byte (LProcessFighting & $FF)
	.byte (LProcessRunning & $FF)
	.byte (LProcessGuarding & $FF)
	.byte (LProcessParrying & $FF)
	.byte (LProcessSpecial & $FF)
LBattleProcessHighBytes:
	.byte (LProcessFighting >> 8 & $FF)
	.byte (LProcessFighting >> 8 & $FF)
	.byte (LProcessRunning >> 8 & $FF)
	.byte (LProcessGuarding >> 8 & $FF)
	.byte (LProcessParrying >> 8 & $FF)
	.byte (LProcessSpecial >> 8 & $FF)

LDoBattle: SUBROUTINE ;Perform the correct battle logic and update the messages accordingly. This one's a doozy.
	lda currentSound
	beq .LNoSound
	cmp #$15 ;Menu confirm sound
	bne .LReturn ;Do not advance battle logic if a non-UI sound is playing!
.LNoSound:
	ldx currentBattler
	cpx #4
	bcs .LNeedEnemyAction
	lda battleActions,x
	jmp .LGotCurrentAction
.LReturn
	rts

.LGoToAdvanceBattlerStatus:
	jmp LAdvanceBattlerStatus
.LGoToProcessAction:
	jmp .LProcessAction
.LGoToCampfirePhase1:
	jmp .LCampfirePhase1
.LGoToCampfirePhase2:
	jmp .LCampfirePhase2

.LNeedEnemyAction
	lda enemyAction
.LGotCurrentAction:
	sta temp1 ;temp1 will contain the current battler's action
	lda inBattle
	cmp #$D0
	beq .LGoToCampfirePhase1
	cmp #$D1
	beq .LGoToCampfirePhase2
	cmp #$81
	beq .LGoToAdvanceBattlerStatus
	cmp #$83
	beq .LGoToAdvanceBattlerStatus
	cmp #$84
	beq .LGoToAdvanceBattlerStatus
	cmp #$85
	beq .LGoToAdvanceBattlerStatus
	cmp #$F0
	bmi .LGoToProcessAction

LProcessCharacterAdvancement:
	cmp #$F0
	beq .LGoToCheckPartyXP
	cmp #$F1
	beq .LPartyDown
	cmp #$F2
	beq .LGoToPartyLeveledUp
	cmp #$F3
	beq .LGoToCheckTypeOfConclusion
	cmp #$F4
	beq .LGoToCheckForNewSpells
	cmp #$FC
	beq .LGameOver
	cmp #$FD
	beq .LGoToNextFloor
	cmp #$FE
	beq .LGameCompleted
	cmp #$FF
	beq .LExitBattleViaVictory
	rts

.LGoToCheckPartyXP:
	jmp .LCheckPartyXP
.LGoToPartyLeveledUp:
	jmp .LPartyLeveledUp
.LGoToCheckTypeOfConclusion:
	jmp .LCheckTypeOfConclusion
.LGoToCheckForNewSpells:
	jmp .LCheckForNewSpells

.LGoToNextFloor:
	lda flags
	ora #NEED_NEW_MAZE
	sta flags

	lda mazeAndPartyLevel
	and #$0F
	sta temp1
	lda mazeAndPartyLevel
	and #$F0
	sta temp2
	clc
	adc #$10
	ora temp1
	sta mazeAndPartyLevel

	lda temp2
	jsr L4Lsr
	adc #$2C ;offset to get to INTO THE CASTLE

	sta currentMessage
	lda #$FF
	sta inBattle
	rts

.LPartyDown:
	lda #$FC
	sta inBattle
	lda #$11
.LStoreEndMessage:
	sta currentMessage ;PARTY DOWN
	rts
.LGameOver:
	lda #$1F ;GAME OVER
	bne .LStoreEndMessage
.LGameCompleted:
	ldx #6
	jsr LLoadEffect
	lda #$21 ;Trophy enemy id
	sta enemyID+1
	sta enemyHP+1
	lda #$20 ;GAME CLEAR
	bne .LStoreEndMessage
.LExitBattleViaVictory:
	lda #0
	sta inBattle
	sta currentEffect
	sta currentBattler ;Needed for maze mode menuing

	ldx #7
.LClearBattlerStatus:
	sta battlerStatus,x
	dex
	bpl .LClearBattlerStatus
	rts

.LCheckPartyXP:
	lda #$13 ;PARTY WINS
	sta currentMessage
	lda mazeAndPartyLevel
	and #$0F
	sta temp4 ;The current party level
	cmp #$9
	bcs .LGoToCheckTypeOfConclusion ;Party is already at max level
	lda #0
	sta temp1 ;temp1 will contain the experience earned during this battle
	ldx #0
.LGetTotalXPLoop:
	lda enemyID,x
	bmi .LCheckNextEnemy
	tay
	lda LEnemyExperience,y
	clc
	adc temp1
	sta temp1
.LCheckNextEnemy:
	inx
	cpx #4
	bcc .LGetTotalXPLoop
.LGotTotalXP:
	;temp1 now contains the total amount of experience gained from this battle
	lda experienceToNextLevel
	sec
	sbc temp1 
	bcc .LLeveledUp
	sta experienceToNextLevel
	jmp .LGoToCheckTypeOfConclusion
.LLeveledUp:
	sta experienceToNextLevel
	lda mazeAndPartyLevel
	and #$F0
	sta temp3 ;Maze level
	inc temp4
	ldx temp4 ;New party level
	lda LXPToNextLevel,x
	clc
	adc experienceToNextLevel
	sta experienceToNextLevel
	lda temp4
	ora temp3
	sta mazeAndPartyLevel

	lda #$F2
	sta inBattle
	rts

.LPartyLeveledUp:
	sed
	ldx #3
.LApplyLevelUpDeltas:
	lda char1,x
	and #$0f
	tay
	ora #(EXCITED << 4)
	sta char1,x
	lda hp1,x
	clc
	adc LHPDeltas,y
	sta hp1,x
	lda mp1,x
	clc
	adc LMPDeltas,y
	sta mp1,x

	dex
	bpl .LApplyLevelUpDeltas
	cld

	lda #$0A ;PARTY LEVELS UP
	sta currentMessage
	ldx #0
	stx aoeTargetsRemaining
	;Determine how many battlers learned a spell
	lda mazeAndPartyLevel
	and #$0F
	tay
.LCheckForNewSpellLoop
	stx aoeTargetID
	lda char1,x
	and #$0F
	tax
	lda LSpellListLookup,x
	sta tempPointer1
	lda #(LWizardSpellList >> 8 & $FF)
	sta tempPointer1+1
	lda (tempPointer1),y
	bmi .LSkipIncrement
	beq .LSkipIncrement
	inc aoeTargetsRemaining
.LSkipIncrement:
	ldx aoeTargetID
	inx
	cpx #4
	bcc .LCheckForNewSpellLoop
	lda #0
	sta aoeTargetID
	ldx aoeTargetsRemaining
	bne .LSomeoneLearnedSpell
	beq .LCheckTypeOfConclusion ;Nobody learned anything!
.LSomeoneLearnedSpell
	lda #$F4
	sta inBattle
	rts

.LCheckForNewSpells:
	lda mazeAndPartyLevel
	and #$0F
	tay
.LCheckSpellLoop:
	ldx aoeTargetID
	lda char1,x
	and #$0F
	tax
	lda LSpellListLookup,x
	sta tempPointer1
	lda #(LWizardSpellList >> 8 & $FF)
	sta tempPointer1+1
	lda (tempPointer1),y
	bpl .LLearnedNewSpell
	inc aoeTargetID
	bpl .LCheckSpellLoop
.LLearnedNewSpell:
	ldx aoeTargetID
	stx currentBattler
	inc aoeTargetID
	sta cursorIndexAndMessageY
	lda #$0B ;X LEARNS Y
	sta currentMessage
	dec aoeTargetsRemaining
	beq .LCheckTypeOfConclusion
	bmi .LCheckTypeOfConclusion
	rts

.LCheckTypeOfConclusion:
	lda playerX
	jsr L4Asl
	ora playerY
	cmp exitLocation
	beq .LWasBoss
	lda #$FF
	sta inBattle
	rts
.LWasBoss:
	lda mazeAndPartyLevel
	jsr L4Lsr
	and #$0F
	cmp #MAX_MAZE_LEVEL
	bcs .LMarkGameComplete
	lda #$FD
	sta inBattle
	rts

.LMarkGameComplete:
	lda #$FE
	sta inBattle
	rts

.LCampfirePhase1:
	ldx #3
.LFullRestoreLoop:
	stx charIndex
	jsr LGetBattlerMaxHPDecimal
	ldx charIndex
	sta battlerHP,x
	jsr LGetBattlerMaxMP
	brk ;LBinaryToDecimal
	ldx charIndex
	sta mp1,x
	dex
	bpl .LFullRestoreLoop

	lda flags
	ora #CAMPFIRE_USED
	sta flags
	lda #$D1
	bne .LCampfireStoreAndExit
.LCampfirePhase2:
	lda #$00
.LCampfireStoreAndExit:
	sta inBattle
	rts

.LGoToHandleAoEEffect:
	jmp .LHandleAoEEffect

.LProcessAction:
	lda temp1
	bmi LProcessCasting
	and #$07
	tay
	lda LBattleProcessLowBytes,y
	sta tempPointer1
	lda LBattleProcessHighBytes,y
	sta tempPointer1+1
	jmp (tempPointer1)
LAdvanceBattlerStatus:
	jsr LCheckBattlerStatus
	bne .LProcessAction
	rts

.LGoToHandleSingleTgtEffect:
	jmp .LHandleSingleTgtEffect
.LGoToSingleTgtPhase2:
	jmp .LSingleTgtPhase2
.LGoToDrainKilled:
	jmp .LDrainKilled
.LGoToWishMPRestoration:
	jmp .LWishMPRestoration

LProcessCasting:
	lda inBattle
	cmp #$A0
	beq .LGoToHandleSingleTgtEffect
	cmp #$A1
	beq .LSingleTgtSpellKill
	cmp #$A2
	beq .LGoToSingleTgtPhase2
	cmp #$A3
	beq .LGoToDrainKilled
	cmp #$A4
	beq .LGoToWishMPRestoration
	cmp #$B0
	beq .LGoToHandleAoEEffect
	cmp #$B1
	beq .LAoESpellKill
.LCastingInitialization:
	lda temp1
	and #$1F ;Spell ID
	tax
	sta cursorIndexAndMessageY
	cmp #$0A ;SMITE
	beq .LSmiteMessage
	cmp #$0B ;VOLLEY
	beq .LVolleyMessage
	lda #$05 ;X CASTS Y
	bne .LStoreCastsMessage

.LSmiteMessage:
	jsr LGetTargetFromActionOffensive
	stx startingCursorIndexAndTargetID
	lda #$2B ;X SMITES Y
	bne .LStoreCastsMessage
.LVolleyMessage:
	lda #$21 ;X SHOT A Y
.LStoreCastsMessage:
	sta currentMessage

	ldx cursorIndexAndMessageY ;spellID
	jsr LLoadSoundInS
	stx mazeAndEffectColor
	ldx #7 ;Pre-spell delay
	jsr LLoadEffect

	ldx cursorIndexAndMessageY ;spell ID
	ldy currentBattler
	cpy #4
	bcs .LDontRemoveMana
	lda mp1,y
	sec
	sed
	sbc LSpellManaLookup,x
	cld
	sta mp1,y
.LDontRemoveMana:
	lda LSpellTargetingLookup,x
	bmi .LIsAoE
.LSingleTarget:
	lda #$A0
	sta inBattle
	rts
.LIsAoE:
	cmp #$82
	beq .LIsOffensive
	cmp #$86
	beq .LIsOffensive
	jsr LSetTotalAoETgtsDefensive
	jmp .LTotalTgtsSet
.LIsOffensive:
	jsr LSetTotalAoETgtsOffensive
.LTotalTgtsSet:
	lda #$B0
	sta inBattle
	rts

.LSingleTgtSpellKill:
	lda #$81
	sta inBattle
.LSharedKillLogic:
	lda #$09 ;X DOWN
	sta currentMessage
	ldx startingCursorIndexAndTargetID
	;Target ID should already be set from previous message
	jsr LDeathCleanup
	rts

.LAoESpellKill:
	lda #$B0
	sta inBattle
	lda #$09 ;X DOWN
	sta currentMessage
	ldx startingCursorIndexAndTargetID
	;Target ID should already be set from previous message
	jsr LDeathCleanup
	jmp .LTryNextTgt

.LDrainKilled:
	lda #$A2
	sta inBattle
	bne .LSharedKillLogic

.LWishMPRestoration:
	lda #$25 ;PARTY MP UP
	sta currentMessage
	ldx #3
	stx aoeTargetID
.LRestoreAllAlliesLoop:
	lda aoeValueAndCampfireControl
	jsr LApplyRestoration
.LNotThisBattler:
	dec aoeTargetID
	ldx aoeTargetID
	cpx currentBattler
	beq .LNotThisBattler
	inx
	dex
	bpl .LRestoreAllAlliesLoop
	jmp .LNormalTgtedExit

.LSingleTgtPhase2:
	;Currently only used by the DRAIN and TRANCE spells
	lda temp1
	and #$1F
	cmp #$04 ;ID for DRAIN
	beq .LDrainPhase2
	cmp #$11 ;ID for TRANCE
	beq .LTrancePhase2
	cmp #$12 ;ID for WISH
	beq .LWishPhase2
	rts ;This code intentionally left blank
.LDrainPhase2:
	lda aoeValueAndCampfireControl ;The amount that was dealt
	;lsr Don't halve the amount to heal after HP scaling changes
	ldx currentBattler
	stx startingCursorIndexAndTargetID
	jmp .LManageHealingMessage
.LWishPhase2:
	lda #$24 ;PARTY HP UP
	sta currentMessage
	ldx #3
.LHealAllAlliesLoop:
	lda aoeValueAndCampfireControl
	jsr LApplyHealing
	ldx temp5
.LNoHealingThisBattler:
	dex
	cpx currentBattler
	beq .LNoHealingThisBattler
	inx
	dex
	bpl .LHealAllAlliesLoop
	lda #$A4
	sta inBattle
	rts
.LTrancePhase2:
	ldx currentBattler
	jsr LGetBattlerMaxMP
	lsr
	lsr
	ldx currentBattler
	jsr LApplyRestoration ;Restore 1/4 max mp
	lda #$2A ;X MP UP
	sta currentMessage ;targetID should already be set from previous message
.LGoToNormalTgtedExit:
	jmp .LNormalTgtedExit

.LHandleSingleTgtEffect:
	lda temp1
	and #$1F ;Spell ID
	tax
	stx temp6
	lda LSpellTargetingLookup,x
	beq .LSelfTargeting
	cmp #$05
	beq .LHighestHPTargetingEnemy
	cmp #$03
	beq .LBasicTargetingAlly
	cmp #$01
	beq .LBasicTargetingEnemy
	rts ;Unknown targeting mode!
.LBasicTargetingEnemy:
	jsr LGetTargetFromActionOffensive
	jmp .LTargetAcquired

.LSelfTargeting
	ldx currentBattler
	jmp .LTargetAcquired

.LBasicTargetingAlly:
	jsr LGetTargetFromActionDefensive
	jmp .LTargetAcquired

.LHighestHPTargetingEnemy:
	ldx currentBattler
	cpx #4
	bcc .LFindMaxHPEnemy
	ldy #0
	beq .LFindMaxHP
.LFindMaxHPEnemy:
	ldy #4
.LFindMaxHP:
	ldx #4
	lda #0
	sta temp2 ;The current max HP
.LFindMaxHPLoop:
	lda battlerHP,y
	cmp temp2
	bcc .LCheckNextHP
	sta temp2
	sty temp3
.LCheckNextHP:
	iny
	dex
	bne .LFindMaxHPLoop
	lda temp3 ;Index of the opposing side's battler with the most HP
	jsr L5Asl 
	ora temp1 ;Hacky technique in order to reuse this function
	sta temp1
	jsr LGetTargetFromActionOffensive

.LTargetAcquired:
	stx startingCursorIndexAndTargetID

	ldx temp6 ;spell ID
	;Need to check if this spell should miss or not
	jsr LCheckSpellHit
	bpl .LSpellConnects
.LNoEffect:
	lda #$15 ;NO EFFECT
	sta currentMessage
	bne .LGoToNormalTgtedExit

.LSpellConnects:
	;Need to check if this spell will be shielded or not
	jsr LCheckSpellShield
	beq .LNoShield
	bmi .LShieldDestroyed
.LShieldWeakened:
	lda #$10 ;X HAS A SHIELD
	sta currentMessage
	bne .LGoToNormalTgtedExit
.LShieldDestroyed:
	lda #$1E ;X SHIELD FADES
	sta currentMessage
	bne .LGoToNormalTgtedExit

.LGoToTgtDamageKilled:
	jmp .LTgtDamageKilled
.LGoToTgtDamageSurvived:
	jmp .LTgtDamageSurvived
.LGoToTgtDamage:
	jmp .LTgtDamage

.LNoShield:
	ldx temp6
	lda LHighSpellLogicLocations,x
	sta tempPointer1+1
	lda LLowSpellLogicLocations,x
	sta tempPointer1
	jmp (tempPointer1)

.LFire:
	ldy #FULL_MAGIC
	jsr LDetermineSpellPower
	ldy #FIRE_RESIST_MASK
	bne .LGoToTgtDamage

.LSleep:
	jsr LTrySleep
	jmp .LNormalTgtedExit

.LDrain:
	;this is a 2-stage maneuver
	ldy #HALF_MAGIC
	jsr LDetermineSpellPower
	sta aoeValueAndCampfireControl ;Save so that the damaged amount can be used for healing the user
	ldy #$FF ;True damage
	ldx startingCursorIndexAndTargetID
	jsr LApplyDamage
	bmi .LTgtDrainKilled
	lda #$A2
	bne .LSetMessageDrain
.LTgtDrainKilled:
	lda #$A3
.LSetMessageDrain:
	sta inBattle
	lda #07 ;X LOSES Y HP
	sta currentMessage
	lda temp2
	brk ;LBinaryToDecimal
	sta cursorIndexAndMessageY
	rts

.LThundr:
	ldy #THREE_HALVES_MAGIC
	jsr LDetermineSpellPower
	ldy #ELECTRIC_RESIST_MASK
	bne .LGoToTgtDamage

.LWish:
	ldy #FULL_MAGIC
	jsr LDetermineSpellPower
	sta aoeValueAndCampfireControl
	lda #$28 ;PARTY STATUS CLEAR
	sta currentMessage
	lda #(~(ASLEEP_MASK | BLIGHTED_MASK))
	sta temp2
	ldx #3
.LClearNegativeStatusLoop:
	lda temp2
	and battlerStatus,x
	sta battlerStatus,x
	dex
	bpl .LClearNegativeStatusLoop
	lda #$A2
	sta inBattle
	rts

.LShield:
	ldx startingCursorIndexAndTargetID
	lda #SHIELDED_MASK
	jsr LApplyStatus
	lda #TIMER_MASK
	jsr LApplyStatus
	lda #$10 ;X HAS A SHIELD
.LNormalTgtedExitSaveMessage:
	sta currentMessage
.LNormalTgtedExit:
	lda #$81
	sta inBattle
	rts

.LHeal:
	ldy #DOUBLE_MAGIC
	jsr LDetermineSpellPower
	ldx startingCursorIndexAndTargetID
.LManageHealingMessage:
	jsr LApplyHealing
	bmi .LHealDenied
	beq .LHealMaxed
	sta cursorIndexAndMessageY
	lda #$06 ;X HEALS Y HP
	bne .LSaveHealMessage
.LHealMaxed:
	lda #$26 ;X HEALS FULLY
	bne .LSaveHealMessage
.LHealDenied:
	lda #$0F ;X WAS CURED
.LSaveHealMessage:
	sta currentMessage
	jmp .LNormalTgtedExit

.LSmite:
	ldy #ATTACK_AND_HALF_MAGIC
	jsr LDetermineSpellPower
	sta temp2
	jsr LApplySharpDamageModifier
	lda temp2
	ldy #HOLY_RESIST_MASK
	bne .LTgtDamage

.LBlight:
	ldx startingCursorIndexAndTargetID
	lda #BLIGHTED_MASK
	jsr LApplyStatus
	lda #$0E ;X WASTES AWAY
	sta currentMessage
	bne .LNormalTgtedExit
	
.LWither:
	ldy #THREE_HALVES_MAGIC
	jsr LDetermineSpellPower
	ldy #POISON_RESIST_MASK
	bne .LTgtDamage

.LTrance:
	ldx currentBattler
	lda #$10 ;Sleep for 1 turn
	ora battlerStatus,x
	sta battlerStatus,x
	stx startingCursorIndexAndTargetID
	lda #$1B ;X FELL ASLEEP
	sta currentMessage
	lda #$A2
	sta inBattle
	rts

.LTgtDamage:
	ldx startingCursorIndexAndTargetID
	jsr LApplyDamage
	bmi .LTgtDamageKilled
.LTgtDamageSurvived:
	lda #$81
	bne .LSetDamageMessage
.LTgtDamageKilled:
	lda #$A1
.LSetDamageMessage:
	sta inBattle
	lda #07 ;X LOSES Y HP
	sta currentMessage
	lda temp2
	brk ;LBinaryToDecimal
	sta cursorIndexAndMessageY
	rts

.LGoToTryNextTgt:
	jmp .LTryNextTgt

.LHandleAoEEffect:
	lda temp1
	and #$1F ;Spell ID
	tax
	stx temp6
	lda LSpellTargetingLookup,x
	cmp #$82
	beq .LAllEnemies
	cmp #$84
	beq .LAllAllies
	cmp #$86
	beq .LAllEnemies
	rts ;Unknown targeting mode!
.LAllEnemies:
	jsr LFindAoETgtOffensive
	jmp .LAoETgtAcquired
.LAllAllies:
	jsr LFindAoETgtDefensive
.LAoETgtAcquired:
	ldx aoeTargetID
	stx startingCursorIndexAndTargetID

	ldx temp6 ;spell ID
	;Need to check if this spell should miss or not
	jsr LCheckSpellHit
	bpl .LAoESpellConnects
	lda #$15 ;NO EFFECT
	sta currentMessage
	bne .LGoToTryNextTgt

.LAoESpellConnects:
	;Need to check if this spell will be shielded or not
	jsr LCheckSpellShield
	beq .LAoENoShield
	bmi .LAoEShieldDestroyed
.LAoEShieldWeakened:
	lda #$10 ;X HAS A SHIELD
	sta currentMessage
	bne .LGoToTryNextTgt
.LAoEShieldDestroyed:
	lda #$1E ;X SHIELD FADES
	sta currentMessage
	bne .LGoToTryNextTgt

.LAoENoShield:
	ldx temp6
	lda LHighSpellLogicLocations,x
	sta tempPointer1+1
	lda LLowSpellLogicLocations,x
	sta tempPointer1
	jmp (tempPointer1)

.LBlizrd:
	ldy #HALF_MAGIC
	jsr LDetermineSpellPower
	ldy #ICE_RESIST_MASK
	jmp .LAoEDamage

.LMeteor:
	lda temp1
	and #$60
	jsr L5Lsr
	sta temp2
	lda startingCursorIndexAndTargetID
	and #$03
	cmp temp2
	beq .LHitWithMeteor
.LHitWithFire:
	ldy #HALF_MAGIC
	jsr LDetermineSpellPower
	ldy #FIRE_RESIST_MASK
	jmp .LAoEDamage
.LHitWithMeteor:
	ldy #FULL_MAGIC
	jsr LDetermineSpellPower
	ldy #PHYSICAL_RESIST_MASK
	jmp .LAoEDamage

.LChaos:
	jsr LRandom
	and #$60
	beq .LChaosStatus ;1/4 chance to do a status
	ldy #HALF_MAGIC
	jsr LDetermineSpellPower
	sta temp2
	lda rand8
	and #$7F
	tay
	lda LChaosElements,y
	tay ;Y contains the correct damage type
	lda temp2 ;Damage to deal
	jmp .LAoEDamage
.LChaosStatus:
	jsr LRandom
	bpl .LChaosSleep
.LChaosBlight:
	lda #$0E ;X WASTES AWAY
	sta currentMessage
	lda #BLIGHTED_MASK
	ldx startingCursorIndexAndTargetID
	jsr LApplyStatus
	jmp .LTryNextTgt
.LChaosSleep:
	jsr LTrySleep
	jmp .LTryNextTgt

.LTriage:
	ldy #FULL_MAGIC
	jsr LDetermineSpellPower
	ldx startingCursorIndexAndTargetID
.LManageAoEHealingMessage:
	jsr LApplyHealing
	bmi .LAoEHealDenied
	beq .LAoEHealMaxed
	sta cursorIndexAndMessageY
	lda #$06 ;X HEALS Y HP
	bne .LSaveAoEHealMessage
.LAoEHealMaxed:
	lda #$26 ;X HEALS FULLY
	bne .LSaveAoEHealMessage
.LAoEHealDenied:
	lda #$0F ;X WAS CURED
.LSaveAoEHealMessage:
	sta currentMessage
	bne .LTryNextTgt

.LBanish:
	ldx startingCursorIndexAndTargetID
	jsr LGetBattlerResistances
	and #LEGENDARY_RESIST_MASK
	bne .LNoBanishing
	jsr LRandom
	bpl .LNoBanishing ;50% chance per enemy
	lda #$17 ;X EXILED
	sta currentMessage
	ldx startingCursorIndexAndTargetID
	jsr LDeathCleanup
	jmp .LTryNextTgt
.LNoBanishing:
	lda #$15 ;NO EFFECT
	sta currentMessage
	bne .LTryNextTgt

.LSharp:
	ldx startingCursorIndexAndTargetID
	lda #SHARPENED_MASK
	jsr LApplyStatus
	lda #$1A ;X ATTACK UP
	sta currentMessage
	bne .LTryNextTgt

.LVolley:
	ldx currentBattler
	jsr LGetBattlerAttack
	lsr
	ldy #PHYSICAL_RESIST_MASK

.LAoEDamage:
	ldx startingCursorIndexAndTargetID
	jsr LApplyDamage
	beq .LAoEDamageSurvived
.LAoEDamageKilled:
	lda #$B1
	sta inBattle
.LAoEDamageSurvived:
	lda #07 ;X LOSES Y HP
	sta currentMessage
	lda temp2
	brk ;LBinaryToDecimal
	sta cursorIndexAndMessageY
	lda inBattle
	cmp #$B1
	bne .LTryNextTgt ;Try the next tgt if this didn't kill
	rts

.LTryNextTgt:
	dec aoeTargetsRemaining
	beq .LSpellComplete
	rts
.LSpellComplete:
	lda #$81
	sta inBattle
	rts

LHighSpellLogicLocations:
	.byte $FF ;Back
	.byte (.LFire >> 8 & $FF)
	.byte (.LSleep >> 8 & $FF)
	.byte (.LBlizrd >> 8 & $FF)
	.byte (.LDrain >> 8 & $FF)
	.byte (.LThundr >> 8 & $FF)
	.byte (.LShield >> 8 & $FF)
	.byte (.LMeteor >> 8 & $FF)
	.byte (.LChaos >> 8 & $FF)
	.byte (.LHeal >> 8 & $FF)
	.byte (.LSmite >> 8 & $FF)
	.byte (.LVolley >> 8 & $FF)
	.byte (.LSharp >> 8 & $FF)
	.byte (.LBlight >> 8 & $FF)
	.byte (.LTriage >> 8 & $FF)
	.byte (.LWither >> 8 & $FF)
	.byte (.LBanish >> 8 & $FF)
	.byte (.LTrance >> 8 & $FF)
	.byte (.LWish >> 8 & $FF)
	
LLowSpellLogicLocations:
	.byte $FF ;Back
	.byte (.LFire & $FF)
	.byte (.LSleep & $FF)
	.byte (.LBlizrd & $FF)
	.byte (.LDrain & $FF)
	.byte (.LThundr & $FF)
	.byte (.LShield & $FF)
	.byte (.LMeteor & $FF)
	.byte (.LChaos & $FF)
	.byte (.LHeal & $FF)
	.byte (.LSmite & $FF)
	.byte (.LVolley & $FF)
	.byte (.LSharp & $FF)
	.byte (.LBlight & $FF)
	.byte (.LTriage & $FF)
	.byte (.LWither & $FF)
	.byte (.LBanish & $FF)
	.byte (.LTrance & $FF)
	.byte (.LWish & $FF)

.LGoToCalculateFightDamage:
	jmp .LCalculateFightDamage
.LGoToDamageWasAKill:
	jmp .LDamageWasAKill

LProcessFighting:
	lda inBattle
	cmp #$90
	beq .LGoToCalculateFightDamage
	cmp #$91
	beq .LGoToDamageWasAKill
	cmp #$92
	beq .LTargetWasParrying
	cmp #$93
	beq .LRetortDescription
	cmp #$94
	beq .LRetortDamage
.LSetFightWindup:
	ldx #$19 ;Swing
	jsr LLoadSoundInS
	jsr LGetTargetFromActionOffensive ;Returns the absolute target ID from the currentBattler's action in X 
	lda battlerHP,x
	beq .LAttackMissed 
	lda rand8
	and #$0F
	bne .LAttackHit
.LAttackMissed:
	lda #$81
	sta inBattle
	lda #$08 ;X MISSES
	sta currentMessage
	rts
.LAttackHit:
	stx startingCursorIndexAndTargetID
	lda #$00 ;X {ATTACK} Y
	sta currentMessage

	;Check if this battler is parrying
	lda #PARRYING_MASK
	ldx startingCursorIndexAndTargetID
	and battlerStatus,x
	beq .LNotParrying
	lda #$92
	sta inBattle
	rts
.LNotParrying:
	lda #$90
	sta inBattle
	rts
.LTargetWasParrying:
	ldx #$1A ;Tink
	jsr LLoadSoundInS
	lda #$23 ;X BLOCKS
	sta currentMessage
	ldx currentBattler
	jsr LGetBattlerResistances
	and #RANGED_MASK
	beq .LIsMelee
	lda #$81 ;Can't riposte against ranged attackers
	sta inBattle
	rts
.LIsMelee:
	lda #$93
	sta inBattle
	rts
.LRetortDescription:
	ldx #$19 ;Swing
	jsr LLoadSoundInS
	lda #$22 ;X STABS Y (parry version)
	sta currentMessage
	lda #$94
	sta inBattle
	rts
.LRetortDamage:
	ldx #$18 ;Hit
	jsr LLoadSoundInS
	
	ldx startingCursorIndexAndTargetID ;The person who was parrying
	jsr LGetBattlerAttack
	sta temp2 ;The raw damage number

	ldx startingCursorIndexAndTargetID
	jsr LApplySharpDamageModifier

	ldx currentBattler
	stx startingCursorIndexAndTargetID ;The person who initiated the attack

	jmp .LApplyFightDamage
.LDamageWasAKill:
	lda #$81
	sta inBattle
	lda #$09 ;X DOWN
	sta currentMessage
	ldx startingCursorIndexAndTargetID
	jsr LDeathCleanup
	;Target ID should already be set from previous message
	rts
.LCalculateFightDamage:
	jsr LGetBattlerAttack
	sta temp2 ;Contains the raw damage number

	ldx currentBattler
	jsr LApplySharpDamageModifier

	jsr LGetTargetFromActionOffensive ;Returns the target in X
	stx startingCursorIndexAndTargetID
.LApplyFightDamage:
	ldy #PHYSICAL_RESIST_MASK
	jsr LApplyDamageNoStoring ;Use temp2 as the damage value
	beq .LBattlerSurvived
.LBattlerDied:
	lda #$91
	bne .LSaveNewBattleState
.LBattlerSurvived:
	lda #$81
.LSaveNewBattleState:
	sta inBattle
	lda #$07 ;X LOSES Y HP
	sta currentMessage
	lda temp2
	brk ;LBinaryToDecimal
	sta cursorIndexAndMessageY
	rts

LProcessRunning:
	;Only party members are allowed to run away
	lda inBattle
	cmp #$E0
	beq .LFailedToRun
	cmp #$E1
	beq .LRanAway
	cmp #$E2
	beq .LExitBattleViaRun
.LInitiateRun:
	lda #$14 ;X TRIES TO RUN
	sta currentMessage

	lda playerX
	jsr L4Asl
	ora playerY
	cmp exitLocation
	beq .LCannotRunAway

	lda #0
	sta temp1
	jsr LSetTotalAoETgtsOffensive
.LFindMaxEnemySpeed
	jsr LFindAoETgtOffensive ;X contains aoeTargetID after this call
	lda battleActions,x ;4 ahead of enemyID
	tax
	lda LEnemySpeed,x
	cmp temp1
	bcc .LNextIteration
	sta temp1
.LNextIteration
	dec aoeTargetsRemaining
	bne .LFindMaxEnemySpeed

	;temp1 should now contain the maximum speed of any alive enemy
	ldx currentBattler
	jsr LGetBattlerSpeed
	sec
	sbc temp1
	bmi .LCannotRunAway ;Can only run away if this battler's speed is equal or greater to the speed of the fastest living enemy
.LRunAway:
	lda #$E1
	sta inBattle
	rts
.LCannotRunAway:
	lda #$E0
	sta inBattle
	rts
.LFailedToRun:
	lda #$81
	sta inBattle
	lda #$16 ;X CANNOT ESCAPE
	sta currentMessage
	rts
.LRanAway:
	lda #$E2
	sta inBattle
	lda #$12 ;PARTY FLEES
	sta currentMessage
	rts
.LExitBattleViaRun:
	lda #0
	sta inBattle
	sta currentEffect
	sta currentBattler ;Needed for maze mode menuing

	ldx #7
.LClearStatusRun:
	sta battlerStatus,x
	dex
	bpl .LClearStatusRun
	rts

LProcessGuarding:
	lda #$81
	sta inBattle
	lda #$19 ;X GUARDS Y
	sta currentMessage

	jsr LGetTargetFromActionDefensive
	stx startingCursorIndexAndTargetID
	lda #GUARDED_MASK
	jsr LApplyStatus
	rts

LProcessParrying:
	lda #$81
	sta inBattle
	lda #$1D ;X GUARDS
	sta currentMessage
	lda #PARRYING_MASK
	ldx currentBattler
	jsr LApplyStatus
	rts

.LGoToArmorSpecial:
	jmp .LArmorSpecial
.LGoToHorrorSpecial:
	jmp .LHorrorSpecial

LProcessSpecial:
	ldx currentBattler
	lda battleActions,x ;battleActions is 4 bytes before enemyID
	cmp #$1E
	beq .LLichSpecial
	cmp #$1A
	beq .LJesterSpecial
	cmp #$6
	beq .LGiftSpecial
	cmp #$24
	beq .LOozeSpecial 
	cmp #$1D
	beq .LSlimeSpecial
	cmp #$1B
	beq .LGoToArmorSpecial
	cmp #$24
	beq .LGoToHorrorSpecial
	rts

.LLichSpecial:
	jsr LFindNextEmptySpot
	cpx #$FF
	beq .LCantSummon
	lda #$34 ;X RAISES Y
	sta temp2
	lda rand8
	bpl .LSummonZombie
.LSummonSkeleton:
	lda #$A
	ldy LSkltonHP ;Can actually save some bytes by replacing these with immediates
	bne .LSummon
.LSummonZombie:
	lda #$9
	ldy LZombieHP
	bne .LSummon

.LJesterSpecial:
	jsr LFindNextEmptySpot
	cpx #$FF
	beq .LCantSummon
	lda #$35 ;X LEAVES Y
	sta temp2
	lda #$6 ;GIFT ID
	ldy #30 ;TODO GIFT hp
.LSummon:
	sta enemyID,x
	tya
	sta enemyHP,x
	jsr L4INX
	stx startingCursorIndexAndTargetID
	lda temp2 ;The correct message
	jmp .LNormalTgtedExitSaveMessage

.LCantSummon:
	lda #$37 ;X CANNOT SUMMON
	jmp .LNormalTgtedExitSaveMessage

.LGiftSpecial:
	;Need to set a firey effect here
	lda #0
	sta battlerHP,x
	lda #$36 ;X BLOWS UP
	sta currentMessage
	lda #$83 ;Cast BLIZRD (can't tell the difference between this and fire damage)
	sta enemyAction
	sta temp1 ;Inject the new enemyAction
	jmp .LIsOffensive ;Jumps into the relevant part of AoE spell setup code

.LOozeSpecial:
	lda enemyHP
	cmp #(OOZE_HP / 2)
	bcs .LAtOrAboveHalf
	;Ooze needs to split
	lda #$33 ;OOZE SPLITS APART
	sta currentMessage
	lda #$1D ;Slime ID
	sta enemyID
	sta enemyID+2
	lda #SLIME_HP
	sta enemyHP
	sta enemyHP+2
	lda #$0A
	ora hasAction
	sta hasAction
	rts
.LSlimeSpecial:
	lda battlerHP,x
	cmp #(SLIME_HP / 2)
	bcs .LAtOrAboveHalf
	;Slime needs to split
	lda #$32 ;SLIME SPLITS APART
	sta currentMessage
	txa
	tay
	iny
	lda #$0C ;Goop ID
	sta battleActions,x
	sta battleActions,y
	lda #GOOP_HP
	sta battlerHP,x
	sta battlerHP,y
	lda LSlimeActionMasks,x ;Index savings...
	ora hasAction
	sta hasAction
	rts
.LAtOrAboveHalf:
	lda enemyAction
	and #$60
	sta temp1
	sta enemyAction
	jmp .LSetFightWindup



.LArmorSpecial:
	lda enemyHP
	ora enemyHP+3
	bne .LDontResummon
.LResummon:
	;The sword and shield have both died...
	lda #30
	sta enemyHP
	sta enemyHP+3
	lda #$FF ;message
	sta currentMessage
	rts
.LDontResummon:
	;Need to figure our what the ARMOR should do here...
	rts

.LHorrorSpecial:
	rts

LFindNextEmptySpot: SUBROUTINE ;Starting from position 2, returns the first empty spot in X. #$FF if no spot exists
	ldx #2
.LFindNextEmptySpotLoop:
	lda enemyHP,x
	beq .LGotIt
	inx
	cpx #4
	bcc .LFindNextEmptySpotLoop
.LNoTarget:
	ldx #$FF
.LGotIt:
	rts

LDetermineSpellPower: SUBROUTINE ;Interprets Y as the damage formula to follow, and returns the appropriate value based on the battler's Attack & Magic
	ldx currentBattler
	dey
	bmi .LFullMagic
	beq .LHalfMagic
	dey
	beq .LThreeHalvesMagic
	dey
	beq .LAttackAndHalfMagic
	dey
	beq .LDoubleMagic
	lda #0 ;Invalid power formula!
	rts
.LFullMagic:
	jsr LGetBattlerMagic
	rts
.LHalfMagic:
	jsr LGetBattlerMagic
	lsr
	rts
.LDoubleMagic:
	jsr LGetBattlerMagic
	asl
	rts
.LThreeHalvesMagic:
	jsr LGetBattlerMagic
	sta temp6
	bne .LHalveAndAdd
.LAttackAndHalfMagic:
	jsr LGetBattlerAttack
	sta temp6
	jsr LGetBattlerMagic
.LHalveAndAdd
	lsr
	clc
	adc temp6
	rts

LGetTargetFromAction: SUBROUTINE
LGetTargetFromActionOffensive: ;Returns the absolute battlerID of the offensive target from the currentBattler's action in X
	ldy #$FF
	bne .LGetRelativeTargetID
LGetTargetFromActionDefensive: ;Returns the absolute battlerID of the defensive target from the currentBattler's action in X
	ldy #$0
.LGetRelativeTargetID:
	lda temp1 ;Should only be called from LDoBattle, so that this contains the currentBattler's action
	and #$60
	jsr L5Lsr
	tax ;X now contains the relative targetID stored with the current action
	iny
	beq .LIsOffensive
.LIsDefensive:
	ldy currentBattler
	cpy #4
	bcs .LTargetEnemies
	rts
.LIsOffensive:
	ldy currentBattler
	cpy #4
	bcc .LTargetEnemies
	rts
.LTargetEnemies:
	inx
	inx
	inx
	inx
	rts

LDetermineNextBattler: SUBROUTINE ;Performs the logic required to determine the next battler to take their action
	;Need to check if either side has lost
	lda hp1
	ora hp2
	ora hp3
	ora hp4
	beq .LPartyDead
	lda enemyHP
	ora enemyHP+1
	ora enemyHP+2
	ora enemyHP+3
	beq .LEnemiesDefeated
	lda hasAction
	bne .LContinue
	;If here, that means that all actions have been taken, so need to take new actions
	beq .LUpdateHasAction
.LPartyDead:
	lda #$F1
	sta inBattle
	rts
.LEnemiesDefeated:
	lda #$F0
	sta inBattle
	rts
LEnterBattleSetup:
	lda #0
	sta hasAction
.LUpdateHasAction:
	ldx #7
.LSetHasActionLoop:
	lda battlerStatus,x
	and #$BB ;Unset the guard and parry flags from all battlers
	sta battlerStatus,x
	lda battlerHP,x
	beq .NoActionThisTurn
	lda LHasActionMasks,x
	ora hasAction
	sta hasAction
.NoActionThisTurn:	
	dex
	bpl .LSetHasActionLoop

	lda #$80 ;At least one battler on each side is still alive, so continue the battle
	sta inBattle
	sta currentMenu
	ldx #1
	jsr LLoadEffect
	dex
	stx cursorIndexAndMessageY
	stx battleActions
	stx battleActions+1
	stx battleActions+2
	stx battleActions+3
	lda #2
	sta menuSize
	jsr LFindFirstLivingAlly
	stx currentBattler

	jsr LUpdateAvatars
	rts

.LContinue:
	lda #0
	sta temp5 ;Will be used to hold the currentBattler value of the battler with the current max speed
	sta temp6 ;Will be used to hold the current max speed
	ldx #7
.LFindMaxSpeed:
	lda hasAction
	and LHasActionMasks,x 
	beq .LNextIteration ;Go check the next person if this battler has already acted this turn

	;Check to see if this is a guard action... If so, give it priority
	cpx #4
	bcs .LCheckSpeed ;Don't check enemy actions for priority status
	lda battleActions,x
	and #$9F ;Ignore the target of this action
	cmp #%00000011 ;GUARD
	beq .LFoundFastestBattler

.LCheckSpeed:
	jsr LGetBattlerSpeed
	cmp temp6
	bcc .LNextIteration
	;This battler is faster than the currently assumed fastest
	sta temp6
	stx temp5
.LNextIteration:
	dex
	bpl .LFindMaxSpeed
	ldx temp5
.LFoundFastestBattler:
	stx currentBattler ;The next action to be taken is the one now in currentBattler
	lda LHasActionMasks,x
	eor hasAction
	sta hasAction ;Mark that this battler has already taken their action
	cpx #4
	rts

LCheckSpellHit: SUBROUTINE ;Determines if the spell corresponding to ID X should miss (because the target battler is unconscious or dead). Returns FF if the spell misses
	ldy startingCursorIndexAndTargetID
	lda battlerHP,y
	bne .LSpellHits ;Any spell on a conscious target hits
	cpy #4
	bcs .LSpellMisses ;Enemies cease to exist after reaching 0 HP
	cpx #$09 ;HEAL
	beq .LSpellHits
	cpx #$0E ;TRIAGE
	beq .LSpellHits
	cpx #$12 ;WISH
	beq .LSpellHits
.LSpellMisses:
	lda #$FF
	rts
.LSpellHits:
	lda #0
	rts

LCheckSpellShield: SUBROUTINE ;Determines if the current spell should be negated by a shield. Returns 0 if no shield, 1 if shield blocks the spell, FF if the shield is destroyed
	ldx startingCursorIndexAndTargetID
	lda battlerStatus,x
	and #SHIELDED_MASK
	bne .LHasShield
.LAlliedSpellsNotBlocked:
	lda #0
	rts
.LHasShield:
	;Make sure that allied spells are not blocked
	txa
	and #$04
	sta temp5
	lda currentBattler
	and #$04
	eor temp5
	beq .LAlliedSpellsNotBlocked

	ldx #$1A ;Tink
	jsr LLoadSoundInS

	lda battlerStatus,x
	and #TIMER_MASK
	beq .LShieldBreaks
	lda battlerStatus,x
	and #(~TIMER_MASK) ;The shield only has 1 more hit left
	sta battlerStatus,x
	lda #1
	rts
.LShieldBreaks:
	lda battlerStatus,x
	and #(~SHIELDED_MASK) ;Destroy the shield
	sta battlerStatus,x
	lda #$FF
	rts

LTrySleep: SUBROUTINE ;Performs logic necessary to try to put the targeted battler to sleep.
	ldx startingCursorIndexAndTargetID
	jsr LGetBattlerResistances
	and #LEGENDARY_RESIST_MASK
	bne .LTryingToPutBossToSleep
	lda #$01 ;50% for normal enemies
	bne .LCheckSleepRandom
.LTryingToPutBossToSleep:
	lda #$07 ;12.5% for bosses
.LCheckSleepRandom:
	sta temp2
	jsr LRandom
	and temp2
	bne .LSleepFailed
	lda #ASLEEP_MASK
	ldx startingCursorIndexAndTargetID
	jsr LApplyStatus
	lda #$1B ;X FELL ASLEEP
	bne .LStoreAndReturn
.LSleepFailed:
	lda #$15 ;NO EFFECT
.LStoreAndReturn:
	sta currentMessage
	rts

LFindFirstLivingAlly: SUBROUTINE ;Returns the id of first party member with positive HP in X.
	ldx #0
.LLoop:
	lda hp1,x
	bne .LEnd
	inx
	bne .LLoop
.LEnd:
	rts

LCheckBattlerStatus: SUBROUTINE ;Similar to LDoBattle, but just processes control flow logic to do with SLEEP and BLIGHT
	lda inBattle
	cmp #$81
	beq .LCheckIfBlighted
	cmp #$83
	beq .LSetBlightDamage
	cmp #$84
	beq .LDiedToBlight
	cmp #$85
	beq .LCheckIfSleeping
	rts
.LCheckIfSleeping:
	lda battlerStatus,x
	and #ASLEEP_MASK
	bne .LIsAsleep
	lda #$FF ;Continue in LDoBattle
	rts
.LIsAsleep:
	;This person is currently asleep
	lsr
	lsr
	lsr
	sec
	sbc #1
	sta temp2
	beq .LWakeUpBattler
	;Still asleep
	lda #$81
	sta inBattle
	lda #$1C ;X IS ASLEEP
	bne .LStoreNewSleepValue
.LWakeUpBattler:
	lda #$82
	sta inBattle
	lda #$18 ;X WAKES UP
.LStoreNewSleepValue:
	sta currentMessage
	lda temp2
	asl
	asl
	asl
	sta temp2
	lda battlerStatus,x
	and #$E7
	ora temp2
	sta battlerStatus,x
	stx startingCursorIndexAndTargetID
	lda #0 ;Return in LDoBattle
	rts
.LCheckIfBlighted:
	lda battlerStatus,x
	and #BLIGHTED_MASK
	beq .LCheckIfSleeping
	;This battler has blight
	stx startingCursorIndexAndTargetID
	lda #$0E ;X WASTES AWAY
	sta currentMessage
	ldx #$1D ;Blight
	jsr LLoadSoundInS
	lda #$83
	bne .LSaveInBattle
.LDiedToBlight:
	lda #$09
	sta currentMessage
	ldx currentBattler
	stx startingCursorIndexAndTargetID
	jsr LDeathCleanup
	lda #$81
	bne .LSaveInBattle
.LSetBlightDamage:
	jsr LGetBattlerMaxHP
	lsr
	lsr
	lsr
	sta temp2 ;Damage to deal in binary
	beq .LAtLeast1Damage
	bne .LNoDamageClampNeeded
.LAtLeast1Damage:
	inc temp2
.LNoDamageClampNeeded:
	ldx currentBattler
	stx startingCursorIndexAndTargetID
	ldy #POISON_RESIST_MASK
	jsr LApplyDamageNoStoring ;Use temp2 as the damage to deal
	sta temp3

	lda temp2 ;Actual damage dealt by LApplyDamage
	brk ;LBinaryToDecimal
	sta cursorIndexAndMessageY
	lda #$07 ;X LOSES Y HP
	sta currentMessage

	lda temp3
	bne .LDied
	lda #$85
	bne .LSaveInBattle
.LDied:
	lda #$84
.LSaveInBattle:
	sta inBattle
	lda #0 ;Return in LDoBattle
	rts

LApplySharpDamageModifier: SUBROUTINE ;Checks if the battler in X is sharpened, and doubles their damage for this attack if so.
	lda battlerStatus,x
	and #SHARPENED_MASK
	beq .LNoSharp
	asl temp2
	lda battlerStatus,x
	and #(~SHARPENED_MASK)
	sta battlerStatus,x
.LNoSharp:
	rts

LApplyRandomModifier: SUBROUTINE ;Adds a random number between 0-1 if the party is level 1, 2, or 3, otherwise adds a random number between 0-7
	lda mazeAndPartyLevel
	and #$0f
	cmp #6
	bcc .LLevelLessThan6
	lda #$03
	bne .LCalculate
.LLevelLessThan6:
	lda #$01
.LCalculate:
	sta tempPointer1
	lda rand8 ;Preferably would've been jsr LRandom, but this exceeds recursion depth when ally takes blight damage...
	and tempPointer1
	clc
	adc temp2
	sta temp2
	rts

LApplyDamage: SUBROUTINE ;Applies binary damage A of damage type Y to target X. Returns 0 in A if target survived, FF if target died.
	sta temp2
LApplyDamageNoStoring: ;Applies binary damage stored in temp2 of damage type Y to target X. Returns 0 in A if target survived, FF if target died.
	stx temp3
	sty temp4

	lda viewedPartyInfo
	sta tempPointer6

	ldx #$18 ;Hit
	jsr LLoadSoundInS

	jsr LRandom ;Moved to here because of recursion depth exception in LApplyRandomModifier
	jsr LApplyRandomModifier
	ldx temp3
	ldy temp4
	cpy #$FF
	beq .LCheckIfGuarded
	jsr LGetBattlerResistances
	sta temp5 ;The resistances for this battler
	and temp4
	beq .LCheckIfGuarded
	lsr temp2
.LCheckIfGuarded:
	ldx temp3
	lda battlerStatus,x
	and #GUARDED_MASK
	beq .LNotGuarded
	lsr temp2
	lsr temp2
.LNotGuarded:
	;Determine if this battler benefits from Legendary resistance
	lda temp5
	bpl .LNotBoss
	lsr temp2
.LNotBoss:
	lda temp2 ;A now contains the final binary damage that should be dealt to target X
	bne .LNonZeroDamage
	lda #1
	sta temp2
.LNonZeroDamage:
	ldx temp3
	cpx #4
	bcs .LDealDamageToEnemy
.LDealDamageToAlly:
	brk ;LBinaryToDecimal
	sta temp4

	ldy #PAIN
	ldx temp3
	jsr LOverrideAvatar

	lda battlerHP,x
	sed
	sec
	sbc temp4
	cld
	beq .LDied
	bcc .LDied
	bcs .LSurvived
.LDealDamageToEnemy:
	ldx #5 ;Enemy damage flash effect
	jsr LLoadEffect ;This goes into a different bank, and so exceeds recursion depth when caused by blight damage

	lda temp4 ;The damage mask
	ldy #$FF
.LDamageShiftingLoop:
	iny
	lsr
	bcc .LDamageShiftingLoop

	;Y = 0 is non elemental, 1 is poison, 2 is electric, 3 is divine, 4 is ice, 5 is fire, 6 is physical
	lda LDamageColors,y
	sta mazeAndEffectColor

	ldx temp3
	lda battlerHP,x
	sec
	sbc temp2
	beq .LDied
	bcc .LDied
.LSurvived:
	ldy temp5
	bpl .LNormalEnemyLived
	asl temp2
.LNormalEnemyLived:
	sta battlerHP,x

	lda tempPointer6
	sta viewedPartyInfo

	lda #0
	rts
.LDied:
	ldy temp5
	bpl .LNormalEnemyDied
	asl temp2
.LNormalEnemyDied
	lda tempPointer6
	sta viewedPartyInfo

	lda #$FF
	rts

LDeathCleanup: SUBROUTINE ;Performs death housekeeping for target X
	lda LHasActionMasks,x
	eor #$FF
	and hasAction
	sta hasAction ;Make sure this battler loses their action on death
	lda #0
	sta battlerHP,x
	sta battlerStatus,x
	ldx #$1C ;Dead
	jsr LLoadSoundInS
	rts

LApplyStatus: SUBROUTINE ;Applies additional status A to target X
	sta temp4
	cmp #$18
	beq .LPuttingTargetToSleep
	eor #$FF
	bne .LAddNewStatus
.LPuttingTargetToSleep:
	lda $E7
.LAddNewStatus:
	and battlerStatus,x
	ora temp4
	sta battlerStatus,x
	rts

LApplyHealing: SUBROUTINE ;Applies binary healing A to target X. Returns $FF if healing was denied by blight, 0 if this battler's health was maxed out, else the decimal amount that was healed
	stx temp5 ;target index
	sta temp2 ;binary amount to regain

	ldx #$1B ;Heal
	jsr LLoadSoundInS
	ldx temp5

	lda #BLIGHTED_MASK
	and battlerStatus,x
	beq .LNoBlight
	;Need to clear the blight
	lda #(BLIGHTED_MASK ^ $FF)
	and battlerStatus,x
	sta battlerStatus,x ;Clear the blight status
	lda #$FF ;Healing was denied by blight
	rts
.LNoBlight:
	cpx #4
	bcs .LHealEnemy
	;healing an ally
	lda temp2 ;binary health to heal
	brk ;LBinaryToDecimal
	sta temp2 ;decimal health to heal
	ldx temp5
	lda battlerHP,x
	sed
	clc
	adc temp2
	cld
	bcs .LAdditionOverflow
	sta temp3 ;current health + heal amount
	jsr LGetBattlerMaxHPDecimal
	ldx temp5 ;targetID
	cmp temp3
	bcc .LMaxedOutHP
	lda temp3
	sta battlerHP,x
	lda temp2
	rts
.LAdditionOverflow:
	jsr LGetBattlerMaxHPDecimal
	ldx temp5
.LMaxedOutHP:
	sta battlerHP,x
	lda #0
	rts
.LHealEnemy:
	lda battlerHP,x
	clc
	adc temp2 ;amount to heal
	sta temp3 ;current health + heal amount (in binary)
	jsr LGetBattlerMaxHP
	ldx temp5 ;target id
	cmp temp3 ; maxHP - predicted health after healing
	bcc .LMaxedOutHPEnemy
	lda temp3
	sta battlerHP,x
	lda temp2
	brk ;LBinaryToDecimal
	rts
.LMaxedOutHPEnemy:
	sta battlerHP,x ;predicted healing is greater, so just store the max hp
	lda #0
	rts

LApplyRestoration: SUBROUTINE ;Applies binary mana restoration A to target X. Returns FF if this battler does not have mp.
	cpx #4
	bcc .LIsAlly
.LNoMana:
	lda #$FF ;Enemies don't track or recover mana
	rts
.LIsAlly:
	stx temp5 ;target index
	brk ;LBinaryToDecimal
	sta temp3 ;decimal amount to regain
	ldx temp5
	lda mp1,x
	clc
	sed
	adc temp3
	cld
	sta temp2 ;Total mana after regaining, but before clamping (decimal)
	jsr LGetBattlerMaxMP ;max mana for this battler (binary)
	brk ;LBinaryToDecimal ;A contains max mana for this battler (decimal)
	beq .LNoMana
	ldx temp5
	cmp temp2
	bcc .LOverRestored
	lda temp2 ;Didn't hit max mana, so just store the amount after addition
.LOverRestored:
	sta mp1,x
	rts

LFindAoETgtOffensive: SUBROUTINE ;Updates the aoeTargetID to the next relevant battler for offensive casts (make sure to check that aoeTargetsRemaining > 0 before use!)
	ldx aoeTargetID
.LSearchForTarget:
	inx
	lda battlerHP,x
	beq .LSearchForTarget
.LFoundTarget:
	stx aoeTargetID
	rts

LFindAoETgtDefensive: SUBROUTINE ;Updates the aoeTargetID to the next relevant battler for defensive casts (make sure to check that aoeTargetsRemaining > 0 before use!)
	ldx currentBattler
	cpx #4
	bcs .LIsEnemy
	;If this is an ally, just increment it, since friendly AoE always affects allies
	inc aoeTargetID
	rts
.LIsEnemy:
	ldx aoeTargetID
.LSearchForTarget:
	inx
	lda battlerHP,x
	beq .LSearchForTarget
.LFoundTarget:
	stx aoeTargetID
	rts

LSetTotalAoETgtsOffensive: SUBROUTINE ;Sets the aoeTargetsRemaining byte to the number of battlers to affect with the AoE spell (on the opposite side of the currentBattler)
									  ;Also initializes the aoeTargetID to the appropriate value
	ldy #0
	ldx currentBattler
	cpx #4
	bcs .LTargetingAllies
.LTargetingEnemies:
	ldx #3
	stx aoeTargetID
	inx
	lda #8
	bne .LSaveLoopLimit
.LTargetingAllies:
	ldx #$FF
	stx aoeTargetID
	inx
	lda #4
.LSaveLoopLimit:
	sta temp3
.LTargetingLoop:
	lda battlerHP,x
	beq .LBattlerDead
	iny
.LBattlerDead:
	inx
	cpx temp3
	bcc .LTargetingLoop
	sty aoeTargetsRemaining
	rts

LSetTotalAoETgtsDefensive: SUBROUTINE ;Sets the aoeTargetsRemaining byte to the number of battlers to affect with the AoE spell (on the same side as the currentBattler)
									  ;Also initializes the aoeTargetID to the appropriate value
	ldx currentBattler
	cpx #4
	bcs .LIsEnemy
.LIsAlly:
	lda #4
	sta aoeTargetsRemaining
	lda #$FF
	sta aoeTargetID
	rts
.LIsEnemy:
	ldy #0
	ldx #3
	stx aoeTargetID
	inx
.LCheckEnemiesLoop:
	lda battlerHP,x
	beq .LNoEnemy
	iny
.LNoEnemy:
	inx
	cpx #8
	bcc .LCheckEnemiesLoop
	sty aoeTargetsRemaining
	rts

LUpdateAvatars: SUBROUTINE ;Updates each party member's avatar based on their status and health
	ldx #3
.LUpdateAvatarLoop:
	;Check for status effect
	stx charIndex
	lda battlerStatus,x
	and #ASLEEP_MASK
	bne .LAsleep
	lda battlerStatus,x
	and #BLIGHTED_MASK
	bne .LBlighted

	;Check for HP
	lda hp1,x
	beq .LDead
	sta tempPointer2 ;current HP in decimal

	jsr LGetBattlerMaxHPDecimal ;Doesn't change X
	;A now contains the max hp of this battler in decimal
	sed
	sec
	sbc tempPointer2 ;Repeatedly subtract the current HP
	sbc tempPointer2
	bcc .LAboveHalf ;jump if A would be < 0
	sbc tempPointer2
	bcc .LAboveThird

	lda #$10 ;Mood 1 -- Sad
	bne .LChangeMood
.LAboveHalf:
	lda #$30 ;Mood 3 -- Happy
	bne .LChangeMood
.LAboveThird:
	lda #$20 ;Mood 2 -- Neutral
	bne .LChangeMood
.LAsleep:
	lda #$60 ;Mood 6
	sta tempPointer1
	bne .LChangeMoodLater
.LBlighted:
	lda #$70 ;Mood 7
	sta tempPointer1
	bne .LChangeMoodLater
.LDead:
	lda #$00 ;Mood 0
.LChangeMood:
	sta tempPointer1
	ldx charIndex
.LChangeMoodLater:
	cld
	lda char1,x
	and #$0F
	ora tempPointer1
	sta char1,x
	
	dex
	bpl .LUpdateAvatarLoop
	rts

LOverrideAvatar: SUBROUTINE ;Sets party member X's mood to Y. 17 bytes
	lda char1,x
	and #$0F ;Get just the class
	sta temp6
	tya
	jsr L4Asl
	ora temp6
	sta char1,x
	rts

LLoadPlayerVars: SUBROUTINE ;Loads each party members max HP and MP
	ldx #3
.LLoadPlayerVarsLoop:
	stx charIndex
	jsr LGetBattlerMaxHPDecimal
	ldx charIndex
	sta hp1,x
	jsr LGetBattlerMaxMP
	brk ;LBinaryToDecimal
	ldx charIndex
	sta mp1,x
	dex
	bpl .LLoadPlayerVarsLoop
	rts

LLoadEnemyHP: SUBROUTINE ;Loads the correct starting HP values for all enemies based on enemyID. DO NOT CALL IF ENEMYIDs ARE NOT SET! 
	ldx #3
.LLoadEnemyHPLoop:
	lda enemyID,x
	cmp #$FF
	beq .LBlankSpace
	tay
	lda LEnemyHP,y
	sta enemyHP,x
	bne .LNextIteration
.LBlankSpace:
	lda #0
	sta enemyHP,x
.LNextIteration:
	dex
	bpl .LLoadEnemyHPLoop
	rts

LHPDeltas:
	.byte 4
	.byte 2
	.byte 3
	.byte 2
	.byte 3
	.byte 3

LMPDeltas:
	.byte 0
	.byte 0
LSlimeActionMasks: ;Must be exactly 4 bytes before .byte $0C
	.byte 4
	.byte 4
	.byte 2
	.byte 2

	.byte $0C
	.byte $06
	.byte $03

LSpellTargetingLookup:
	.byte $0 ;BACK
	.byte $1 ;FIRE
	.byte $1 ;SLEEP
	.byte $82 ;BLIZRD
	.byte $1 ;DRAIN
	.byte $5 ;THUNDR
	.byte $3 ;SHIELD
	.byte $86 ;METEOR
	.byte $82 ;CHAOS
	.byte $3 ;HEAL
	.byte $1 ;SMITE
	.byte $82 ;VOLLEY
	.byte $84 ;SHARP
	.byte $1 ;BLIGHT
	.byte $84 ;TRIAGE
	.byte $1 ;WITHER
	.byte $82 ;BANISH
	.byte $0 ;TRANCE
	.byte $0 ;WISH

LSpellManaLookup:
	.byte 0 ;BACK
	.byte 4 ;FIRE
	.byte 6 ;SLEEP
	.byte 6 ;BLIZRD
	.byte 8 ;DRAIN
	.byte 6 ;THUNDR
	.byte 6 ;SHIELD
	.byte 8 ;METEOR
	.byte 6 ;CHAOS
	.byte 5 ;HEAL
	.byte 4 ;SMITE
	.byte 5 ;VOLLEY
	.byte 8 ;SHARP
	.byte 8 ;BLIGHT
	.byte 5 ;TRIAGE
	.byte 4 ;WITHER
	.byte 5 ;BANISH
	.byte 0 ;TRANCE
	.byte 15 ;WISH

	ORG $DD00 ;Used to hold enemy stats and related data) No new tables can really be added here
	RORG $FD00

LXPToNextLevel:
	.byte #0 ;Shouldn't be used, xp for level 0 -> 1
	.byte #15 ; 1 -> 2
	.byte #30 ; 2 -> 3
	.byte #30
	.byte #60
	.byte #60
	.byte #120
	.byte #120
	.byte #180 ; 8 -> 9

LSpellListLookup:
	.byte (LEmptySpellList & $FF)
	.byte (LEmptySpellList & $FF)
	.byte (LClericSpellList & $FF)
	.byte (LWizardSpellList & $FF)
	.byte (LRangerSpellList & $FF)
	.byte (LPaladinSpellList & $FF)

LEnemyExperience:
	.byte 1 ;Wolf
	.byte 1 ;Druid
	.byte 1 ;Shroom
	.byte 2 ;Squire
	.byte 2 ;Archer
	.byte 2 ;Priest
	.byte 0 ;Gift
	.byte 0 ;Sword
	.byte 0 ;Shield
	.byte 4 ;Zombie
	.byte 4 ;Sklton
	.byte 4 ;Mage
	.byte 4 ;Goop
	.byte 8 ;Warlok
	.byte 8 ;Imp
	.byte 8 ;Wisp
	.byte 0 ;RedOrb
	.byte 0 ;BluOrb
	.byte 0 ;GrnOrb
	.byte 0 ;GldOrb
	.byte 2 ;Bear
	.byte 2 ;Unicrn
	.byte 8 ;Volcio
	.byte 7 ;Glacia
	.byte 4 ;Grgoyl
	.byte 4 ;Mimic
	.byte 30 ;Jester
	.byte 30 ;Armor
	.byte 8 ;Spider
	.byte 8 ;Slime
	.byte 60 ;Lich
	.byte 16 ;Shfflr
	.byte 16 ;Shmblr
	.byte 0 ;Trophy
	.byte 15 ;Thickt
	.byte 0 ;Horror
	.byte 60 ;Ooze
	.byte 0 ;Campfire

LEnemyAttack:
	.byte 1 ;Wolf
	.byte 0 ;Druid
	.byte 0 ;Shroom
	.byte 1 ;Squire
	.byte 2 ;Archer
	.byte 0 ;Priest
	.byte 0 ;Gift
	.byte 3 ;Sword
	.byte 1 ;Shield
	.byte 2 ;Zombie
	.byte 2 ;Sklton
	.byte 0 ;Mage
	.byte 2 ;Goop
	.byte 0 ;Warlok
	.byte 2 ;Imp
	.byte 0 ;Wisp
	.byte 0 ;RedOrb
	.byte 0 ;BluOrb
	.byte 0 ;GrnOrb
	.byte 5 ;GldOrb
	.byte 3 ;Bear
	.byte 2 ;Unicrn
	.byte 4 ;Volcio
	.byte 4 ;Glacia
	.byte 4 ;Grgoyl
	.byte 4 ;Mimic
	.byte 8 ;Jester
	.byte 1 ;Armor
	.byte 5 ;Spider
	.byte 5 ;Slime
	.byte 0 ;Lich
	.byte 7 ;Shfflr
	.byte 5 ;Shmblr
	.byte 0 ;Trophy
	.byte 9 ;Thickt
	.byte 30 ;Horror
	.byte 12 ;Ooze
	.byte 0 ;Campfire

LEnemySpeed:
	.byte 32 ;Wolf -- Outspeeds mid at level 1
	.byte 38 ;Druid -- Outspeeds mid at level 2
	.byte 1 ;Shroom -- Always slowest
	.byte 35 ;Squire -- Outspeeds slow at level 4
	.byte 50 ;Archer -- Outspeeds mid at level 4
	.byte 1 ;Priest -- Always slowest
	.byte 1 ;Gift -- Always slowest
	.byte 65 ;Sword -- Outspeeds fast at level 4
	.byte 45 ;Shield -- Outspeeds mid at level 3
	.byte 38 ;Zombie -- Outspeeds slow at level 5
	.byte 76 ;Sklton -- Outspeeds fast at level 5
	.byte 65 ;Mage -- Outspeeds mid at level 6
	.byte 1 ;Goop -- Always slowest
	.byte 60 ;Warlok -- Outspeeds slow
	.byte 90 ;Imp -- Outspeeds fast at level 7
	.byte 1 ;Wisp -- Always slowest
	.byte 70 ;RedOrb -- Outspeeds mid at level 7
	.byte 1 ;BluOrb -- Always slowest
	.byte 50 ;GrnOrb -- Outspeeds slow at level 8
	.byte 90 ;GldOrb -- Outspeeds fast at level 7
	.byte 25 ;Bear -- Outspeeds slow at level 2
	.byte 1 ;Unicrn -- Always slowest
	.byte 38 ;Volcio -- Outspeeds mid at level 2
	.byte 38 ;Glacia -- Outspeeds mid at level 2
	.byte 30 ;Grgoyl -- Outspeeds slow at level 3
	.byte 45 ;Mimic -- Outspeeds mid at level 3
	.byte 60 ;Jester -- Outspeeds fast at level 3
	.byte 35 ;Armor -- Outspeeds slow at level 4
	.byte 55 ;Spider -- Outspeeds mid at level 5
	.byte 1 ;Slime -- Always slowest
	.byte 55 ;Lich -- Outspeeds mid at level 5
	.byte 70 ;Shfflr -- Outspeeds mid at level 7
	.byte 70 ;Shmblr -- Outspeeds mid at level 7
	.byte 1 ;Trophy -- N/A
	.byte 32 ;Thickt -- Outspeeds mid at level 1
	.byte 75 ;Horror -- Outspeeds mid at level 8
	.byte 1 ;Ooze -- Always slowest
	.byte 1 ;Campfire -- N/A

LEnemyMagic:
	.byte 0 ;Wolf
	.byte 1 ;Druid
	.byte 0 ;Shroom
	.byte 0 ;Squire
	.byte 0 ;Archer
	.byte 2 ;Priest
	.byte 15 ;Gift
	.byte 0 ;Sword
	.byte 0 ;Shield
	.byte 0 ;Zombie
	.byte 0 ;Sklton
	.byte 3 ;Mage
	.byte 0 ;Goop
	.byte 4 ;Warlok
	.byte 3 ;Imp
	.byte 4 ;Wisp
	.byte 15 ;RedOrb
	.byte 15 ;BluOrb
	.byte 15 ;GrnOrb
	.byte 15 ;GldOrb
	.byte 0 ;Bear
	.byte 2 ;Unicrn
	.byte 6 ;Volcio
	.byte 6 ;Glacia
	.byte 0 ;Grgoyl
	.byte 0 ;Mimic
	.byte 0 ;Jester
	.byte 0 ;Armor
	.byte 0 ;Spider
	.byte 0 ;Slime
	.byte 10 ;Lich
	.byte 0 ;Shfflr
	.byte 0 ;Shmblr
	.byte 0 ;Trophy
	.byte 0 ;Thickt
	.byte 0 ;Horror
	.byte 0 ;Ooze
	.byte 0 ;Campfire

LEnemyHP:
	.byte 7 ;Wolf
	.byte 5 ;Druid
	.byte 8 ;Shroom
	.byte 18 ;Squire
	.byte 12 ;Archer
	.byte 10 ;Priest
LGiftHP:
	.byte 20 ;Gift
	.byte 20 ;Sword
	.byte 20 ;Shield
LZombieHP:
	.byte 30 ;Zombie
LSkltonHP:
	.byte 24 ;Sklton
	.byte 20 ;Mage
LGoopHP:
	.byte 20 ;Goop
	.byte 30 ;Warlok
	.byte 35 ;Imp
	.byte 30 ;Wisp
	.byte 70 ;RedOrb
	.byte 70 ;BluOrb
	.byte 70 ;GrnOrb
	.byte 70 ;GldOrb
	.byte 16 ;Bear
	.byte 14 ;Unicrn
	.byte 20 ;Volcio
	.byte 20 ;Glacia
	.byte 34 ;Grgoyl
	.byte 36 ;Mimic
	.byte 50 ;Jester
	.byte 50 ;Armor
	.byte 50 ;Spider
LSlimeHP:
	.byte 60 ;Slime
	.byte 85 ;Lich
	.byte 70 ;Shfflr
	.byte 85 ;Shmblr
	.byte 1 ;Trophy
	.byte 35 ;Thickt
	.byte 200 ;Horror
	.byte 150 ;Ooze
	.byte 1 ;Campfire
 
;Format is LPFIHEPR
;L : Legendary (bosses), P : Physical, F : Fire, I : Ice, H : Holy, E : Electric, P : Poison, R : isRanged (prevents riposte)
LEnemyResistances:
	.byte #%00000000 ;Wolf
	.byte #%00000000 ;Druid
	.byte #%00000000 ;Shroom
	.byte #%00000000 ;Squire
	.byte #%00000000 ;Archer
	.byte #%00000000 ;Priest
	.byte #%00000000 ;Gift
	.byte #%00000000 ;Sword
	.byte #%00000000 ;Shield
	.byte #%00000000 ;Zombie
	.byte #%00000000 ;Sklton
	.byte #%00000000 ;Mage
	.byte #%00000000 ;Goop
	.byte #%00000000 ;Warlok
	.byte #%00000000 ;Imp
	.byte #%00000000 ;Wisp
	.byte #%00000000 ;RedOrb
	.byte #%00000000 ;BluOrb
	.byte #%00000000 ;GrnOrb
	.byte #%00000000 ;GldOrb
	.byte #%00000000 ;Bear
	.byte #%00000000 ;Unicrn
	.byte #%00000000 ;Volcio
	.byte #%00000000 ;Glacia
	.byte #%00000000 ;Grgoyl
	.byte #%00000000 ;Mimic
	.byte #%00000000 ;Jester
	.byte #%00000000 ;Armor
	.byte #%00000000 ;Spider
	.byte #%00000000 ;Slime
	.byte #%00000000 ;Lich
	.byte #%00000000 ;Shfflr
	.byte #%00000000 ;Shmblr
	.byte #%00000000 ;Trophy
	.byte #%00000000 ;Thickt
	.byte #%00000000 ;Horror
	.byte #%00000000 ;Ooze
	.byte #%00000000 ;Campfire

	ORG $DE00 ;Used to hold miscellaneous data/lookup tables
	RORG $FE00

LAllZeroes:
	.byte 0
	.byte 0
	.byte 0
	.byte 0
	.byte 0
	.byte 0
	.byte 0
	.byte 0
	.byte 0
LLowStatGrowth:
	.byte 1
	.byte 2
	.byte 3
	.byte 4
	.byte 5
	.byte 6
	.byte 7
	.byte 8
	.byte 9
LMidStatGrowth:
	.byte 2
	.byte 4
	.byte 6
	.byte 8
	.byte 10
	.byte 12
	.byte 14
	.byte 16
	.byte 18
LHighStatGrowth:
	.byte 3
	.byte 6
	.byte 9
	.byte 12
	.byte 15
	.byte 18
	.byte 21
	.byte 24
	.byte 27
LLowHPGrowth:
	.byte 20
	.byte 24
	.byte 28
	.byte 32
	.byte 36
	.byte 40
	.byte 44
	.byte 48
	.byte 52
LMidHPGrowth:
	.byte 30
	.byte 36
	.byte 42
	.byte 48
	.byte 54
	.byte 60
	.byte 66
	.byte 72
	.byte 78
LHighHPGrowth:
	.byte 40
	.byte 48
	.byte 56
	.byte 64
	.byte 72
	.byte 80
	.byte 88
	.byte 96
	.byte 99
LLowHPGrowthDecimal:
	.byte $20
	.byte $24
	.byte $28
	.byte $32
	.byte $36
	.byte $40
	.byte $44
	.byte $48
	.byte $52
LMidHPGrowthDecimal:
	.byte $30
	.byte $36
	.byte $42
	.byte $48
	.byte $54
	.byte $60
	.byte $66
	.byte $72
	.byte $78
LHighHPGrowthDecimal:
	.byte $40
	.byte $48
	.byte $56
	.byte $64
	.byte $72
	.byte $80
	.byte $88
	.byte $96
	.byte $99

LClassAttackLookup:
	.byte (LMidStatGrowth & $FF) ;Knight
	.byte (LHighStatGrowth & $FF) ;Rogue
	.byte (LMidStatGrowth & $FF) ;Cleric
	.byte (LLowStatGrowth & $FF) ;Wizard
	.byte (LMidStatGrowth & $FF) ;Ranger
	.byte (LMidStatGrowth & $FF) ;Paladin
LClassMagicLookup:
	.byte (LAllZeroes & $FF)
	.byte (LAllZeroes & $FF)
	.byte (LHighStatGrowth & $FF)
	.byte (LHighStatGrowth & $FF)
	.byte (LLowStatGrowth & $FF)
	.byte (LLowStatGrowth & $FF)
LClassSpeedLookup:
	.byte (LLowHPGrowth & $FF)
	.byte (LHighHPGrowth & $FF)
	.byte (LLowHPGrowth & $FF)
	.byte (LMidHPGrowth & $FF)
	.byte (LHighHPGrowth & $FF)
	.byte (LMidHPGrowth & $FF)
LClassHPLookup:
	.byte (LHighHPGrowth & $FF)
	.byte (LLowHPGrowth & $FF)
	.byte (LMidHPGrowth & $FF)
	.byte (LLowHPGrowth & $FF)
	.byte (LMidHPGrowth & $FF)
	.byte (LMidHPGrowth & $FF)
LClassMPLookup:
	.byte (LAllZeroes & $FF)
	.byte (LAllZeroes & $FF)
	.byte (LHighHPGrowth & $FF)
	.byte (LHighHPGrowth & $FF)
	.byte (LLowHPGrowth & $FF)
	.byte (LLowHPGrowth & $FF)
LClassHPDecimalLookup:
	.byte (LHighHPGrowthDecimal & $FF)
	.byte (LLowHPGrowthDecimal & $FF)
	.byte (LMidHPGrowthDecimal & $FF)
	.byte (LLowHPGrowthDecimal & $FF)
	.byte (LMidHPGrowthDecimal & $FF)
	.byte (LMidHPGrowthDecimal & $FF)

LWizardSpellList:
	.byte #$0 ;BACK
	.byte #$1 ;FIRE
	.byte #$3 ;BLIZRD
	.byte #$4 ;DRAIN
	.byte #$2 ;SLEEP
	.byte #$5 ;THUNDR
	.byte #$6 ;SHIELD
	.byte #$8 ;CHAOS
	.byte #$7 ;METEOR
LClericSpellList:
	.byte #$0 ;BACK
	.byte #$9 ;HEAL
	.byte #$F ;WITHER
	.byte #$C ;SHARP
	.byte #$E ;TRIAGE
	.byte #$D ;BLIGHT
	.byte #$11 ;TRANCE
	.byte #$10 ;BANISH
	.byte #$12 ;WISH
LPaladinSpellList:
	.byte #$0 ;BACK
	.byte #$FF 
	.byte #$9 ;HEAL
	.byte #$FF
	.byte #$A ;SMITE
	.byte #$FF
	.byte #$C ;SHARP
	.byte #$FF 
	.byte #$6 ;SHIELD
LRangerSpellList:
	.byte #$0 ;BACK
	.byte #$FF
	.byte #$B ;VOLLEY
	.byte #$FF
	.byte #$9 ;HEAL
	.byte #$FF
	.byte #$2 ;SLEEP
	.byte #$FF
	.byte #$D ;BLIGHT
LEmptySpellList:
	.byte #0
	.byte #$FF
	.byte #$FF
	.byte #$FF
	.byte #$FF
	.byte #$FF
	.byte #$FF
	.byte #$FF
	.byte #$FF

LLowAllyStatPointers:
	.byte (LClassAttackLookup & $FF)
	.byte (LClassMagicLookup & $FF)
	.byte (LClassSpeedLookup & $FF)
	.byte (LClassHPLookup & $FF)
	.byte (LClassMPLookup & $FF)
	.byte (LClassHPDecimalLookup & $FF)
LHighAllyStatPointers:
	.byte (LClassAttackLookup >> 8 & $FF)
	.byte (LClassMagicLookup >> 8 & $FF)
	.byte (LClassSpeedLookup >> 8 & $FF)
	.byte (LClassHPLookup >> 8 & $FF)
	.byte (LClassMPLookup >> 8 & $FF)
	.byte (LClassHPDecimalLookup >> 8 & $FF)
LLowEnemyStatPointers:
	.byte (LEnemyAttack & $FF)
	.byte (LEnemyMagic & $FF)
	.byte (LEnemySpeed & $FF)
	.byte (LEnemyHP & $FF)
	.byte 0 ;This should never be referenced
	.byte 0 ;This should never be referenced
LHighEnemyStatPointers:
	.byte (LEnemyAttack >> 8 & $FF)
	.byte (LEnemyMagic >> 8 & $FF)
	.byte (LEnemySpeed >> 8 & $FF)
	.byte (LEnemyHP >> 8 & $FF)
	.byte 0 ;This should never be referenced
	.byte 0 ;This should never be referenced

LHasActionMasks:
	.byte #$80
	.byte #$40
	.byte #$20
	.byte #$10
	.byte #$08
	.byte #$04
	.byte #$02
	.byte #$01

LChaosElements:
	.byte $FF ;Non-elemental
	.byte PHYSICAL_RESIST_MASK
	.byte FIRE_RESIST_MASK
	.byte ICE_RESIST_MASK
	.byte ELECTRIC_RESIST_MASK
	.byte HOLY_RESIST_MASK
	.byte POISON_RESIST_MASK
	.byte $FF ;Non-elemental

LDamageColors:
	.byte $00
	.byte $CA
	.byte $1E
	.byte $0E
	.byte $9C
	.byte $FC
	.byte $08

LBinaryToDecimal: SUBROUTINE ;Will interpret A as the number in binary to convert to decimal. Returns the result in A.
	tsx
	inx
	inx
	dec $00,x ;Trim the extra address increment that happens as part of brk

	;Consider adding overflow protection here like in SGetMazeRoomData

	ldx #0
.LRemove10s:
	sec
	sbc #10
	bmi .LDoneRemoving10s
	inx
	jmp .LRemove10s
.LDoneRemoving10s:
	clc
	adc #10
	sta temp4
	txa
	asl
	asl
	asl
	asl
	ora temp4
	rti

LRandom: SUBROUTINE ;Ticks the random number generator when called 10 bytes
	lda rand8
	lsr
	bcc .LNoEOR
	eor #$B4
.LNoEOR:
	sta rand8
	rts

LGetBattlerStat: SUBROUTINE ;Returns the appropriate stat of battlerID X in A
LGetBattlerAttack:
	ldy #0
	beq LSetStatPointers
LGetBattlerMagic:
	ldy #1
	bne LSetStatPointers
LGetBattlerSpeed:
	ldy #2
	bne LSetStatPointers
LGetBattlerMaxHP:
	ldy #3
	bne LSetStatPointers
LGetBattlerMaxHPDecimal:
	ldy #5
	bne LSetStatPointers
LGetBattlerMaxMP:
	ldy #4
LSetStatPointers:
	lda LLowAllyStatPointers,y
	sta tempPointer3
	lda LHighAllyStatPointers,y
	sta tempPointer3+1
	lda LLowEnemyStatPointers,y
	sta temp4
	lda LHighEnemyStatPointers,y
	sta tempPointer4
.LGetStat:
	cpx #4
	bcs .LCheckEnemyStat
.LCheckAllyStat:
	lda char1,x
	and #$0F
	tay
	lda (tempPointer3),y
	sta tempPointer3
	lda #(LLowStatGrowth >> 8 & $FF)
	sta tempPointer3+1
	lda mazeAndPartyLevel
	and #$0F
	tay
	dey
	lda (tempPointer3),y
	rts
.LCheckEnemyStat:
	dex
	dex
	dex
	dex
	lda enemyID,x
	tay
	lda (temp4),y
L4INX:
	inx
	inx
	inx
	inx
	rts

	ORG $DF80
	RORG $FF80

LLoadSoundInS:
	sta $1FF9 ;Go to bank 3
LFrontlineModifiers:
	.byte $0 ;Knight
	.byte $0 ;Rogue
	.byte $0 ;Cleric
	.byte $1 ;Wizard
	.byte $1 ;Ranger
	.byte $0 ;Paladin
	rts

	ORG $DF90
	RORG $FF90

LLoadEffect:
	sta $1FF8 ;Go to bank 2
LIsClassRanged:
	.byte $0 ;Knight
	.byte $0 ;Rogue
	.byte $0 ;Cleric
	.byte $1 ;Wizard
	.byte $1 ;Ranger
	.byte $0 ;Paladin
	rts

LLowLabelBytes:
	.byte (LDoBattle & $FF)
	.byte (LDetermineNextBattler & $FF)
	.byte (LUpdateAvatars & $FF)
	.byte (LLoadEnemyHP & $FF)
	.byte (LEnterBattleSetup & $FF)
	.byte (LLoadPlayerVars & $FF)
	.byte (LLoadEffect & $FF)

LHighLabelBytes:
	.byte (LDoBattle >> 8 & $FF)
	.byte (LDetermineNextBattler >> 8 & $FF)
	.byte (LUpdateAvatars >> 8 & $FF)
	.byte (LLoadEnemyHP >> 8 & $FF)
	.byte (LEnterBattleSetup >> 8 & $FF)
	.byte (LLoadPlayerVars >> 8 & $FF)
	.byte (LLoadEffect >> 8 & $FF)

	ORG $DFB0
	RORG $FFB0

LRunFunctionForSBank:
	nop ;1
	nop ;sta $1FF7
	nop ;3
	lda LHighLabelBytes,y ;6
	sta tempPointer6 ;8
	lda LLowLabelBytes,y ;11
	sta temp6 ;13
	lda #(LReturnLocation >> 8 & $FF) ;15
	pha ;16
	lda #(LReturnLocation & $FF) ;18
	pha ;19
LReturnLocation: 
	jmp (temp6) ;22
	sta returnValue ;24
	sta $1FF9 ;Return to S bank ;27

LGetBattlerResistances: SUBROUTINE ;Will interpret X as the targetID to return the resistances of (in A). Format is LPFIDEPR
									;L: Legendary resist (Banish/Sleep), P: Physical, F: Fire, I:Ice, D: Divine, E: Electric, P: Poison, R: isRanged
	cpx #4
	bcs .LHasResistanceByte 
	lda char1,x
	and #$0f
	tax
	lda LIsClassRanged,x
	rts
.LHasResistanceByte:
	dex
	dex
	dex
	dex
	lda enemyID,x
	tay
	lda LEnemyResistances,y
	inx
	inx
	inx
	inx
	rts

L6Lsr:
	lsr
L5Lsr
	lsr
L4Lsr:
	lsr
	lsr
	lsr
	lsr
	rts

L5Asl:
	asl
L4Asl:
	asl
	asl
	asl
	asl
	rts

	ORG $DFFA
	RORG $FFFA

	;NMI, IRQ, and RESET information
	.word LReset
	.word LReset
	.word LBinaryToDecimal


