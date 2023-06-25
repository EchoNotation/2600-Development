	ORG $D000
	RORG $F000

	;BANK 1 - CONTAINS THE MAJORITY OF THE MAZE/BATTLE SYSTEM LOGIC

LReset:
	nop $1FF7 ;Make sure to stay in bank 1
	ldx #0
	txa
	tay
LClear:
	dex
	txs
	pha	
	bne LClear
	cld

	lda #21
	sta CTRLPF ;Sets the playfield to reflect, and makes the ball 4 clocks wide

	lda #0
	sta SWACNT
	sta playerX
	sta playerY
	sta playerFacing

	lda INTIM ;Seed the random number generator
	bne LSkipSeeding
	lda #$6B ;Extremely random random number generator here
LSkipSeeding:
	sta rand8

	lda #$09 ;Maze level 0, party level 9
	sta mazeAndPartyLevel

	ldy #3; Subroutine ID for SClearMazeData
	jsr LRunFunctionInSBank

	ldy #0; Subroutine ID for SGenerateMazeData
	jsr LRunFunctionInSBank

	;Temp testing code that will be removed much, much later
	lda #$33
	sta char1
	lda #F
	sta name1
	lda #R
	sta name2
	lda #E
	sta name3
	lda #D
	sta name4
	lda #EMPTY
	sta name5
	lda #$58
	sta hp1
	lda #$23
	sta mp1

	lda #$35
	sta char2
	lda #D
	sta name1+1
	lda #A
	sta name2+1
	lda #V
	sta name3+1
	lda #E
	sta name4+1
	lda #EMPTY
	sta name5+1
	lda #$17
	sta hp2
	lda #$17
	sta mp2

	lda #$32
	sta char3
	lda #T
	sta name1+2
	lda #I
	sta name2+2
	lda #M
	sta name3+2
	lda #EMPTY
	sta name4+2
	sta name5+2
	lda #$45
	sta hp3
	lda #$02
	sta mp3

	lda #$34
	sta char4
	lda #J
	sta name1+3
	lda #O
	sta name2+3
	lda #H
	sta name3+3
	lda #N
	sta name4+3
	lda #EMPTY
	sta name5+3
	lda #$1
	sta hp4
	lda #$04
	sta mp4

	lda #$09
	sta battlerStatus

	lda #$F8
	sta currentInput
	sta previousInput

	jsr LUpdateAvatars

	lda #$44
	sta exitLocation

	lda #$80
	sta inBattle
	sta currentMenu
	lda #$F8
	sta hasAction
	lda #$FF
	sta enemyID+1
	sta enemyID+3
	lda #$03
	sta menuSize
	lda #1
	sta enemyHP
	; sta enemyHP+1
	sta enemyHP+2
	; sta enemyHP+3
	sta highlightedLine
	sta currentEffect
	sta enemyAction

LStartOfFrame:
	lda #$82
	sta VBLANK ;Enable blanking
	sta VSYNC ;Enable syncing signal
	sta WSYNC ;Requisite 3 scanlines of VSYNC
	sta WSYNC
	sta WSYNC
	lda #0
	sta VSYNC ;Stop broadcasting VSYNC signal


	jsr LRandom ;Tick the random number generator

	lda #1
	bit SWCHB
	beq LGoToReset ;Reset the game if the console reset switch is pressed

	lda #VBLANK_TIMER_DURATION
	sta TIM64T ;Set timer to complete at the end of VBLANK.

	lda inBattle
	bne LBattleLogicVBlank ;Skip this logic if we are not in maze mode...

LMazeLogicVBlank:
	ldy #2 ;Subroutine ID for SUpdatePlayerMovement
	jsr LRunFunctionInSBank

	ldy #1 ;Subroutine ID for SUpdateMazeRenderingPointers
	jsr LRunFunctionInSBank

	ldy #4 ;Subroutine ID for SUpdateCompassPointerBoss
	jsr LRunFunctionInSBank
	jmp LGoToUpdateEffects

LBattleLogicVBlank:
	lda currentMenu
	bmi LUpdateMenuRenderingVBlank
	lda currentInput
	eor previousInput
	and #$08
	beq LWaitForVblankTimer ;Must be different from previousInput
	lda #$08
	bit currentInput
	bne LWaitForVblankTimer ;Button must be pressed in
	lda inBattle
	cmp #$81
	bne LDontNeedANewBattler
	jsr LDetermineNextBattler
	lda inBattle
	cmp #$80
	beq LUpdateMenuRenderingVBlank ;Don't advance if we just entered the menu
LDontNeedANewBattler:
	jsr LUpdateAvatars
	jsr LDoBattle
	jmp LWaitForVblankTimer

LGoToReset:
	jmp LReset

LUpdateMenuRenderingVBlank:
	ldy #6 ;Subroutine ID for SUpdateMenuRendering
	jsr LRunFunctionInSBank

LAfterEffectUpdate:
LWaitForVblankTimer:
	lda INTIM
	bne LWaitForVblankTimer ;Is VBLANK over yet?
	sta WSYNC

	jmp LGoToMainPicture

LOverscan:
	lda #OVERSCAN_TIMER_DURATION
	sta TIM64T

	;Update the currentInput variable
	lda currentInput
	sta previousInput
	lda SWCHA
	and #$F0
	sta temp1
	lda INPT4
	and #$80
	jsr L4Lsr
	ora temp1
	sta currentInput

	lda inBattle
	beq LMazeLogicOverscan ;Skip the following logic if we are in maze mode...

LBattleLogicOverscan:
	lda currentMenu
	bpl LAfterMenuLogic
	lda currentInput
	eor previousInput
	and #$F8
	beq LNoMenuAdvancement
	jsr LUpdateMenuCursorPos
	
	ldy #5 ;Subroutine ID for SUpdateMenuAdvancement
	jsr LRunFunctionInSBank
LNoMenuAdvancement:
LAfterMenuLogic:
	jmp LWaitForOverscanTimer

LMazeLogicOverscan:
	;Need to check for the maze exit and campfire location
	;Need to determine if a random encounter occurs	

LWaitForOverscanTimer:
	lda INTIM
	bne LWaitForOverscanTimer

	sta WSYNC
	jmp LStartOfFrame

LBattleProcessLowBytes:
	.byte (LProcessFighting & $FF)
	.byte (LProcessMoving & $FF)
	.byte (LProcessRunning & $FF)
	.byte (LProcessGuarding & $FF)
	.byte (LProcessParrying & $FF)
LBattleProcessHighBytes:
	.byte (LProcessFighting >> 8 & $FF)
	.byte (LProcessMoving >> 8 & $FF)
	.byte (LProcessRunning >> 8 & $FF)
	.byte (LProcessGuarding >> 8 & $FF)
	.byte (LProcessParrying >> 8 & $FF)

LDoBattle: SUBROUTINE ;Perform the correct battle logic and update the messages accordingly. This one's a doozy.	
	ldx currentBattler
	cpx #4
	bcs .LNeedEnemyAction
	lda battleActions,x
	jmp .LGotCurrentAction

.LGoToAdvanceBattlerStatus:
	jmp LAdvanceBattlerStatus
.LGoToProcessAction:
	jmp .LProcessAction

.LNeedEnemyAction
	lda enemyAction
.LGotCurrentAction:
	sta temp1 ;temp1 will contain the current battler's action
	lda inBattle
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
	beq .LCheckPartyXP
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

.LGoToPartyLeveledUp:
	jmp .LPartyLeveledUp
.LGoToCheckTypeOfConclusion:
	jmp .LCheckTypeOfConclusion
.LGoToCheckForNewSpells:
	jmp .LCheckForNewSpells

.LGoToNextFloor:
	;Exit battle, generate new maze, position player within maze. Probably shouldn't happen from within this function, so just set a flag?
	rts
.LPartyDown:
	lda #$FC
	sta inBattle
	lda #$11
	sta currentMessage ;PARTY DOWN
	rts
.LGameOver:
.LGameCompleted:
	rts
.LExitBattleViaVictory:
	lda #0
	sta inBattle
	sta currentEffect
	sta currentBattler ;Needed for maze mode menuing
	rts

.LCheckPartyXP:
	lda #$13 ;PARTY WINS
	sta currentMessage
	lda mazeAndPartyLevel
	and #$0F
	sta temp4 ;The current party level
	cmp #$9
	bcs .LDidntLevelUp ;Party is already at max level
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
.LDidntLevelUp:
	lda #$F3
	sta inBattle
	rts
.LLeveledUp: ;THIS WHOLE SECTION NEEDS TO BE TESTED
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
	lda #$0A ;PARTY LEVELS UP
	sta currentMessage
	lda #$F4
	sta inBattle
	lda #0
	sta aoeTargetID
	rts

.LCheckForNewSpells:
	lda mazeAndPartyLevel
	and #$0F
	tay
	ldx aoeTargetID
.LCheckSpellLoop:
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
	ldx aoeTargetID
	cpx #4
	bcs .LCheckTypeOfConclusion
	bcc .LCheckSpellLoop
.LLearnedNewSpell:
	ldx aoeTargetID
	stx currentBattler
	inc aoeTargetID
	sta cursorIndexAndMessageY
	lda #$0B ;X LEARNS Y
	sta currentMessage
	ldx aoeTargetID
	cpx #4
	bcs .LCheckTypeOfConclusion
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
	bcs .LGameWon
	lda #$21 ;THE MAZE AWAITS
	sta currentMessage
	lda #$FD
	sta inBattle
	rts
.LGameWon:
	lda #$20 ;GAME CLEAR!
	sta currentMessage
	lda #$FE
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
LProcessCasting:
	lda inBattle
	cmp #$A0
	beq .LHandleSingleTgtEffect
	cmp #$A1
	beq .LSingleTgtSpellKill
	cmp #$B0
	beq .LGoToHandleAoEEffect
	cmp #$B1
	beq .LAoESpellKill
.LCastingInitialization:
	lda temp1
	and #$1F ;Spell ID
	tax
	sta cursorIndexAndMessageY
	lda #$05 ;X CASTS Y
	sta currentMessage

	ldy currentBattler
	cpy #4
	bcs .LDontRemoveMana
	lda mp1,y
	sec
	sbc LSpellManaLookup,x
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
	rts

.LGoToNormalTgtedExit:
	jmp .LNormalTgtedExit

.LHandleSingleTgtEffect:
	lda temp1
	and #$1F ;Spell ID
	tax
	lda LSpellTargetingLookup,x
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

.LBasicTargetingAlly:
	jsr LGetTargetFromActionDefensive
	jmp .LTargetAcquired

.LHighestHPTargetingEnemy:
	ldx currentBattler
	cpx #4
	bcs .LFindMaxHPEnemy
	ldy #0
	bne .LFindMaxHP
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
	lda temp1
	and #$1F ;Spell ID
	tax
	stx temp6

	;Need to check if this spell should miss or not
	jsr LCheckSpellHit
	bpl .LSpellConnects
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
	bne .LTgtDamage
.LSleep:
	ldx startingCursorIndexAndTargetID
	lda #ASLEEP_MASK
	jsr LApplyStatus
	lda #$1B ;X FELL ASLEEP
	sta currentMessage
	bne .LNormalTgtedExit
.LDrain:
	;this is a 2-stage maneuver
	rts
.LThundr:
	ldy #THREE_HALVES_MAGIC
	jsr LDetermineSpellPower
	ldy #ELECTRIC_RESIST_MASK
	bne .LTgtDamage
.LShield:
	ldx startingCursorIndexAndTargetID
	lda #SHIELDED_MASK
	jsr LApplyStatus
	lda #TIMER_MASK
	jsr LApplyStatus
	lda #$10 ;X HAS A SHIELD
	sta currentMessage
	bne .LNormalTgtedExit
.LHeal:
	rts
.LSmite:
	ldy #ATTACK_AND_HALF_MAGIC
	jsr LDetermineSpellPower
	ldy #HOLY_RESIST_MASK
	bne .LTgtDamage
.LPoison:
	ldy #ATTACK_AND_HALF_MAGIC
	jsr LDetermineSpellPower
	ldy #POISON_RESIST_MASK
	bne .LTgtDamage
.LSharp:
	ldx startingCursorIndexAndTargetID
	lda #SHARPENED_MASK
	jsr LApplyStatus
	lda #$1A ;X ATTACK UP
	sta currentMessage
	bne .LNormalTgtedExit
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
	;TODO this spell is a pain in the ass that needs more special messages, not as bad as wish though
	rts
.LShift:
	lda partyBattlePos
	eor #$0F
	sta partyBattlePos
	;lda need a message for this
	;sta currentMessage 
.LNormalTgtedExit:
	lda #$81
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
	jsr LBinaryToDecimal
	sta cursorIndexAndMessageY
	rts

.LHandleAoEEffect:

.LBlizrd:
	rts
.LMeteor:
	rts
.LChaos:
	rts
.LTriage:
	rts
.LBanish:
	rts
.LWish:
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
	.byte (.LPoison >> 8 & $FF)
	.byte (.LSharp >> 8 & $FF)
	.byte (.LBlight >> 8 & $FF)
	.byte (.LTriage >> 8 & $FF)
	.byte (.LWither >> 8 & $FF)
	.byte (.LBanish >> 8 & $FF)
	.byte (.LTrance >> 8 & $FF)
	.byte (.LWither >> 8 & $FF)
	.byte (.LShift >> 8 & $FF)
	
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
	.byte (.LPoison & $FF)
	.byte (.LSharp & $FF)
	.byte (.LBlight & $FF)
	.byte (.LTriage & $FF)
	.byte (.LWither & $FF)
	.byte (.LBanish & $FF)
	.byte (.LTrance & $FF)
	.byte (.LWither & $FF)
	.byte (.LShift & $FF)

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
	ldx currentBattler
	cpx #4
	bcs .LGetEnemyFightMessage
	lda char1,x
	and #$0F
	tay
	lda LClassFightMessages,y
	jmp .LStoreFightMessage	
.LGetEnemyFightMessage:
	dex
	dex
	dex
	dex
	lda enemyID,x
	tax
	lda LEnemyFightMessages,x
.LStoreFightMessage:
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
	lda #$22 ;X STABS Y (parry version)
	sta currentMessage
	lda #$94
	sta inBattle
	rts
.LRetortDamage:
	ldx startingCursorIndexAndTargetID ;The person who was parrying
	jsr LGetBattlerAttack
	sta temp2 ;The raw damage number

	ldx startingCursorIndexAndTargetID
	jsr LApplyPositionalDamageModifier

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
	jsr LApplyPositionalDamageModifier

	jsr LGetTargetFromActionOffensive ;Returns the target in X
	stx startingCursorIndexAndTargetID
.LApplyFightDamage:
	ldy #PHYSICAL_RESIST_MASK
	lda temp2
	jsr LApplyDamage
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
	jsr LBinaryToDecimal
	sta cursorIndexAndMessageY
	rts

LProcessMoving:
	lda #$81
	sta inBattle
	ldx currentBattler
	lda partyBattlePos
	eor LPartyPositionMasks,x
	sta partyBattlePos
	lda #$0C
	sta temp2
	lda partyBattlePos
	and LPartyPositionMasks,x
	beq .LBackline
	bne .LSetMoveMessage
.LBackline:
	inc temp2
.LSetMoveMessage:
	lda temp2
	sta currentMessage
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
	ldx #4
.LFindMaxEnemySpeed:
	jsr LGetBattlerSpeed
	cmp temp1
	bcc .LNextIteration
	sta temp1
.LNextIteration:
	inx
	cpx #8
	bcc .LFindMaxEnemySpeed
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
	lda #$1D ;X IS ON GUARD
	sta currentMessage
	lda #PARRYING_MASK
	ldx currentBattler
	jsr LApplyStatus
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
	lda #0 ;Invalid power formula!
	rts
.LFullMagic:
	jsr LGetBattlerMagic
	rts
.LHalfMagic:
	jsr LGetBattlerMagic
	lsr
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
	lda inBattle
	cmp #$81
	bne .LNotSeekingNewBattler
	lda #$08
	bit currentInput
	beq .LButtonPressed
.LNotSeekingNewBattler:
	rts
.LButtonPressed:
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
	stx currentEffect
	dex
	stx cursorIndexAndMessageY
	stx battleActions
	stx battleActions+1
	stx battleActions+2
	stx battleActions+3
	lda #3
	sta menuSize
	jsr LFindFirstLivingAlly
	stx currentBattler
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
	bcs .LDetermineEnemyAI
	rts
.LDetermineEnemyAI
	jsr LSetEnemyAction
	rts

LCheckSpellHit: SUBROUTINE ;Determines if the spell corresponding to ID X should miss (because the target battler is unconscious or dead). Returns FF if the spell misses
	ldy startingCursorIndexAndTargetID
	lda battlerHP,y
	bne .LSpellHits ;Any spell on a conscious target hits
	cpy #4
	bcs .LSpellMisses ;Enemies cease to exist after reaching 0 hp
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
	lda #0
	rts
.LHasShield:
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

LFindFirstLivingAlly: SUBROUTINE ;Returns the id of first party member with positive HP in X.
	ldx #0
.LLoop:
	lda hp1,x
	bne .LEnd
	inx
	bne .LLoop
.LEnd:
	rts

LSetEnemyAction: SUBROUTINE ;Choose what action this enemy will perform and set enemyAction accordingly.
	;Will have to be way more complicated in the future, but this works for the moment. RIP Whomever is in front of the party
	;This is a likely candidate for relocation into the S bank
	lda #$00
	sta enemyAction
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
	ldy #0 ;Return in LDoBattle
	stx startingCursorIndexAndTargetID
	rts
.LCheckIfBlighted:
	lda battlerStatus,x
	and #BLIGHTED_MASK
	beq .LCheckIfSleeping
	;This battler has blight
	lda #$83
	sta inBattle
	lda #$0E ;X WASTES AWAY
	sta currentMessage
	lda #0 ;Return in LDoBattle
	stx startingCursorIndexAndTargetID
	rts
.LDiedToBlight:
	lda #$09
	sta currentMessage
	lda #$81
	sta inBattle
	ldx currentBattler
	stx startingCursorIndexAndTargetID
	rts
.LSetBlightDamage:
	jsr LGetBattlerMaxHP
	lsr
	lsr
	lsr
	beq .LAtLeast1Damage
	sta temp2 ;Damage to deal in binary
	bne .LNoDamageClampNeeded
.LAtLeast1Damage:
	inc temp2
.LNoDamageClampNeeded:
	lda temp2 ;Damage to deal in binary
	ldx currentBattler
	stx startingCursorIndexAndTargetID
	ldy #POISON_RESIST_MASK
	jsr LApplyDamage
	sta temp3

	lda temp2 ;Actual damage dealt by LApplyDamage
	jsr LBinaryToDecimal
	sta cursorIndexAndMessageY
	lda #$07 ;X LOSES Y HP
	sta currentMessage

	lda temp3
	bne .LDied
	lda #$85
	sta inBattle
	lda #0
	rts
.LDied:
	lda #$84
	sta inBattle
	lda #0
	rts

LApplyPositionalDamageModifier: SUBROUTINE ;Treats X as the absolute ID of the attacker, and modifies the binary damage in temp2 accordingly
	cpx #4
	bcs .LFullDamage ;Enemies do not have frontline/backline, so disregard the following checks

	lda char1,x
	and #$0F
	tay ;Save the class of this battler for later

	lda LPartyPositionMasks,x
	and partyBattlePos
	bne .LBattlerInFrontline
.LBattlerInBackline:
	lda LBacklineModifiers,y ;0 means normal damage, anything else means half
	bne .LHalfDamage
	beq .LFullDamage
.LBattlerInFrontline:
	lda LFrontlineModifiers,y
	beq .LFullDamage
.LHalfDamage:
	lsr temp2
.LFullDamage
	rts

LApplyDamage: SUBROUTINE ;Applies binary damage A of damage type Y to target X. Returns 0 in A if target survived, FF if target died.
	sta temp2
	stx temp3
	sty temp4
	jsr LGetBattlerResistances
	and temp4
	beq .LDamageNotResisted
	lsr temp2
	jmp .LCheckDamageModifiers
.LDamageNotResisted:
	lda temp2
.LCheckDamageModifiers:
	ldx currentBattler
	lda battlerStatus,x
	and #SHARPENED_MASK
	beq .LSharpInactive
.LSharpActive:
	asl temp2
.LSharpInactive:
.LCheckIfGuarded:
	ldx temp3
	lda battlerStatus,x
	and #GUARDED_MASK
	beq .LNotGuarded
	lsr temp2
	lsr temp2
.LNotGuarded:

	lda temp2 ;A now contains the final binary damage that should be dealt to target X
	ldx temp3
	cpx #4
	bcs .LDealDamageToEnemy
.LDealDamageToAlly:
	jsr LBinaryToDecimal
	sta temp4
	ldx temp3
	lda battlerHP,x
	sed
	sec
	sbc temp4
	cld
	beq .LDied
	bcc .LDied
	bcs .LSurvived
.LDealDamageToEnemy:
	lda battlerHP,x
	clc
	sbc temp2
	beq .LDied
	bcc .LDied
.LSurvived:
	sta battlerHP,x
	lda #0
	rts
.LDied:
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

LApplyHealing: SUBROUTINE ;Applies binary healing A to target X. Returns $FF if healing was denied by blight, otherwise the amount that was actually healed
	stx temp4 ;target index
	sta temp2 ;binary amount to regain
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
	jsr LBinaryToDecimal
	sta temp2 ;decimal health to heal
	ldx temp4
	lda battlerHP,x
	sed
	clc
	adc temp2
	cld
	sta temp3 ;current health + heal amount
	jsr LGetBattlerMaxHP
	ldx temp4 ;targetID
	cmp temp3
	bcs .LMaxedOutHP
	lda temp3 ;Didn't max out, so load the current health + heal amount
	sta battlerHP,x
	lda temp2 ;the decimal amount that was healed
	rts
.LMaxedOutHP:
	sta battlerHP,x

	rts

.LHealEnemy:

	rts

LApplyRestoration: SUBROUTINE ;Applies binary mana restoration A to target X. Returns the amount of mana that was actually recovered (0 for enemies)
	cpx #4
	bcc .LIsAlly
	lda #0 ;Enemies don't track or recover mana
	rts
.LIsAlly:
	stx temp4 ;target index
	jsr LBinaryToDecimal
	sta temp3 ;decimal amount to regain
	ldx temp4
	lda mp1,x
	sta temp5 ;current mana
	clc
	sed
	adc temp3
	cld
	sta temp2 ;Total mana after regaining, but before clamping
	jsr LGetBattlerMaxMP
	ldx temp4
	cmp temp2
	bcc .LOverRestored
	lda temp2 ;Didn't hit max mana, so just store the amount after addition
	sta mp1,x
	lda temp3
	;A now contains the amount of mana that was restored
	rts
.LOverRestored:
	sed
	sbc mp1,x ;Max MP - Current MP
	cld
	;A now contains the amount of mana that was restored
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
	cpx #8
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
	beq .LEnemyDead
	iny
.LEnemyDead:
	inx
	cpx #8
	bcc .LCheckEnemiesLoop
	sty aoeTargetsRemaining
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
	lda #(LStat1PerLevel >> 8 & $FF)
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
	inx
	inx
	inx
	inx
	rts

LBinaryToDecimal: SUBROUTINE ;Will interpret A as the number in binary to convert to decimal. Returns the result in A.
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
	jsr L4Asl
	ora temp4
	rts

LDecimalToBinary: SUBROUTINE ;Will interpret A as the number in decimal to convert to binary. Returns the result in A.
	ldx #0
	sed
.LRemove16s:
	sec
	sbc #$16
	bmi .LDoneRemoving16s
	inx
	bne .LRemove16s
.LDoneRemoving16s:
	adc #$16
	cld
	cmp #$10
	bcs .LOver10
	sta tempPointer2
	bcc .LCombine ;Should always be taken
.LOver10:
	and #$0F
	clc
	adc #10
	sta tempPointer2
.LCombine:
	txa
	jsr L4Asl
	ora tempPointer2
	rts

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
	tax
	lda LEnemyResistances,x
	rts

LRandom: SUBROUTINE ;Ticks the random number generator when called
	lda rand8
	lsr
	bcc .LNoEOR
	eor #$B4
.LNoEOR:
	sta rand8
	rts

LUpdateMenuCursorPos: SUBROUTINE ;Updates the cursor according to joystick presses
	ldy cursorIndexAndMessageY
	lda #DOWN_MASK
	bit currentInput
	beq .LDownPressed
	lda #UP_MASK
	bit currentInput
	beq .LUpPressed
	rts
.LDownPressed:
	cpy menuSize
	bcc .LNotAtLastPosition
	rts
.LNotAtLastPosition
	iny
	sty cursorIndexAndMessageY
	rts
.LUpPressed:
	ldy cursorIndexAndMessageY
	bne .LNotAtFirstPosition
	rts
.LNotAtFirstPosition
	dey
	sty cursorIndexAndMessageY
	rts

LUpdateAvatars: SUBROUTINE ;Updates each party member's avatar based on their status and health
							;This is a candidate for relocation to S bank
	lda inBattle
	bpl .LContinue
	lda #$08
	bit currentInput
	beq .LContinue
	rts
.LContinue:
	ldy #3
	sty charIndex
.LUpdateAvatarLoop:
	;Check for status effect
	ldy charIndex

	lda battlerStatus,y
	sta tempPointer1
	and #ASLEEP_MASK
	bne .LAsleep
	lda tempPointer1
	and #BLIGHTED_MASK
	bne .LBlighted

	;Check for HP

	lda hp1,y
	beq .LDead
	jsr LDecimalToBinary
	sta tempPointer1 ;Now contains the current HP for this party member in binary

	lda char1,y
	and #$0F
	tax
	lda LClassHPLookup,x
	sta tempPointer2
	lda #(LStat1PerLevel >> 8 & $FF)
	sta tempPointer2+1

	lda mazeAndPartyLevel
	and #$0F
	tay
	dey
	lda (tempPointer2),y ;A now contains the max HP for this party member
	lsr
	cmp tempPointer1
	bcc .LAboveHalf
	lsr
	cmp tempPointer1
	bcc .LAboveQuarter
	lda #$10 ;Mood 1
	bne .LChangeMood
.LAboveHalf:
	lda #$30 ;Mood 3
	bne .LChangeMood
.LAboveQuarter:
	lda #$20 ;Mood 2
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

	ldy charIndex

.LChangeMoodLater:
	lda char1,y
	and #$0F
	ora tempPointer1
	sta char1,y
	
	dec charIndex
	bpl .LUpdateAvatarLoop
	inc charIndex
	rts

LOverrideAvatar: SUBROUTINE ;Sets party member Y's mood to X.
	lda char1,y
	and #$0F ;Get just the class
	sta temp6
	txa
	jsr L4Asl
	ora temp6
	sta char1,y
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

	ORG $DD00 ;Used to hold enemy stats and related data) No new tables can really be added here
	RORG $FD00

LXPToNextLevel: ;TODO balance this
	.byte #0 ;Shouldn't be used, xp for level 0 -> 1
	.byte #8 ; 1 -> 2
	.byte #16 ; 2 -> 3
	.byte #26
	.byte #36
	.byte #51
	.byte #66
	.byte #86
	.byte #106 ; 8 -> 9

LEnemyExperience:
	.byte #1 ;Zombie
	.byte #5 ;Giant
	.byte #20 ;Dragon

LEnemyAttack:
	.byte 2 ;Zombie
	.byte 10 ;Giant
	.byte 35 ;Dragon
LEnemySpeed:
	.byte 1 ;Zombie
	.byte 3 ;Giant
	.byte 40 ;Dragon
LEnemyMagic:
	.byte 0 ;Zombie
	.byte 0 ;Giant
	.byte 20 ;Dragon
LEnemyHP:
	.byte #1 ;Zombie
	.byte #10 ;Giant
	.byte #150 ;Dragon
 
;Format is LPFIHEPR
;L : Legendary (bosses), P : Physical, F : Fire, I : Ice, H : Holy, E : Electric, P : Poison, R : isRanged (prevents riposte)
LEnemyResistances:
	.byte #%01011010 ;Zombie
	.byte #%00000000 ;Giant
	.byte #%10000010 ;Dragon

	ORG $DE00 ;Used to hold miscellaneous data/lookup tables
	RORG $FE00

LClassFightMessages:
	.byte $4 ;Knight
	.byte $0 ;Rogue
	.byte $2 ;Cleric
	.byte $1 ;Wizard
	.byte $1 ;Ranger
	.byte $0 ;Paladin

LEnemyFightMessages:
	.byte $3 ;Zombie
	.byte $2 ;Giant
	.byte $4 ;Dragon

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
LStat1PerLevel:
	.byte 4
	.byte 5
	.byte 6
	.byte 7
	.byte 8
	.byte 9
	.byte 10
	.byte 11
	.byte 12
LStat2PerLevel:
	.byte 5
	.byte 7
	.byte 9
	.byte 11
	.byte 13
	.byte 15
	.byte 17
	.byte 19
	.byte 21
LStat3PerLevel:
	.byte 6
	.byte 9
	.byte 12
	.byte 15
	.byte 18
	.byte 21
	.byte 24
	.byte 27
	.byte 30
LStat4PerLevel:
	.byte 7
	.byte 11
	.byte 15
	.byte 19
	.byte 23
	.byte 27
	.byte 31
	.byte 35
	.byte 39
LStat5PerLevel:
	.byte 8
	.byte 13
	.byte 18
	.byte 23
	.byte 28
	.byte 33
	.byte 38
	.byte 43
	.byte 48
LHP2PerLevel:
	.byte 11
	.byte 13
	.byte 15
	.byte 17
	.byte 19
	.byte 21
	.byte 23
	.byte 25
	.byte 27
LHP4PerLevel:
	.byte 12
	.byte 16
	.byte 20
	.byte 24
	.byte 28
	.byte 32
	.byte 36
	.byte 40
	.byte 44
LHP6PerLevel:
	.byte 13
	.byte 19
	.byte 25
	.byte 31
	.byte 37
	.byte 43
	.byte 49
	.byte 55
	.byte 61
LHP8PerLevel:
	.byte 14
	.byte 22
	.byte 30
	.byte 38
	.byte 46
	.byte 54
	.byte 62
	.byte 70
	.byte 78
LHP10PerLevel:
	.byte 15
	.byte 25
	.byte 35
	.byte 45
	.byte 55
	.byte 65
	.byte 75
	.byte 85
	.byte 95
LMP2PerLevel:
	.byte 7
	.byte 9
	.byte 11
	.byte 13
	.byte 15
	.byte 17
	.byte 19
	.byte 21
	.byte 23
LMP3PerLevel:
	.byte 8
	.byte 11
	.byte 14
	.byte 17
	.byte 20
	.byte 23
	.byte 26
	.byte 29
	.byte 32
LMP5PerLevel:
	.byte 10
	.byte 15
	.byte 20
	.byte 25
	.byte 30
	.byte 35
	.byte 40
	.byte 45
	.byte 50

LClassAttackLookup:
	.byte (LStat3PerLevel & $FF) ;Knight
	.byte (LStat5PerLevel & $FF) ;Rogue
	.byte (LStat2PerLevel & $FF) ;Cleric
	.byte (LStat1PerLevel & $FF) ;Wizard
	.byte (LStat4PerLevel & $FF) ;Ranger
	.byte (LStat3PerLevel & $FF) ;Paladin
LClassMagicLookup:
	.byte (LAllZeroes & $FF)
	.byte (LAllZeroes & $FF)
	.byte (LStat3PerLevel & $FF)
	.byte (LStat5PerLevel & $FF)
	.byte (LStat2PerLevel & $FF)
	.byte (LStat3PerLevel & $FF)
LClassSpeedLookup:
	.byte (LStat2PerLevel & $FF)
	.byte (LStat5PerLevel & $FF)
	.byte (LStat1PerLevel & $FF)
	.byte (LStat3PerLevel & $FF)
	.byte (LStat4PerLevel & $FF)
	.byte (LStat3PerLevel & $FF)
LClassHPLookup:
	.byte (LHP10PerLevel & $FF)
	.byte (LHP4PerLevel & $FF)
	.byte (LHP8PerLevel & $FF)
	.byte (LHP2PerLevel & $FF)
	.byte (LHP4PerLevel & $FF)
	.byte (LHP6PerLevel & $FF)
LClassMPLookup:
	.byte (LAllZeroes & $FF)
	.byte (LAllZeroes & $FF)
	.byte (LMP3PerLevel & $FF)
	.byte (LMP5PerLevel & $FF)
	.byte (LMP2PerLevel & $FF)
	.byte (LMP3PerLevel & $FF)

LPartyPositionMasks:
	.byte $01
	.byte $02
	.byte $04
	.byte $08

LSpellListLookup:
	.byte (LEmptySpellList & $FF)
	.byte (LEmptySpellList & $FF)
	.byte (LClericSpellList & $FF)
	.byte (LWizardSpellList & $FF)
	.byte (LRangerSpellList & $FF)
	.byte (LPaladinSpellList & $FF)

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
	.byte #$12 ;DONATE
	.byte #$10 ;BANISH
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
	.byte #$B ;POISON
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
	.byte $1 ;POISON
	.byte $3 ;SHARP
	.byte $1 ;BLIGHT
	.byte $84 ;TRIAGE
	.byte $1 ;WITHER
	.byte $82 ;BANISH
	.byte $0 ;TRANCE
	.byte $84 ;WISH
	.byte $82 ;SHIFT

LSpellManaLookup:
	.byte 0 ;BACK
	.byte 1 ;FIRE
	.byte 1 ;SLEEP
	.byte 1 ;BLIZRD
	.byte 1 ;DRAIN
	.byte 1 ;THUNDR
	.byte 1 ;SHIELD
	.byte 1 ;METEOR
	.byte 1 ;CHAOS
	.byte 1 ;HEAL
	.byte 1 ;SMITE
	.byte 1 ;POISON
	.byte 1 ;SHARP
	.byte 1 ;BLIGHT
	.byte 1 ;TRIAGE
	.byte 1 ;WITHER
	.byte 1 ;BANISH
	.byte 1 ;TRANCE
	.byte 1 ;WISH
	.byte 0 ;SHIFT

LLowAllyStatPointers:
	.byte (LClassAttackLookup & $FF)
	.byte (LClassMagicLookup & $FF)
	.byte (LClassSpeedLookup & $FF)
	.byte (LClassHPLookup & $FF)
	.byte (LClassMPLookup & $FF)
LHighAllyStatPointers:
	.byte (LClassAttackLookup >> 8 & $FF)
	.byte (LClassMagicLookup >> 8 & $FF)
	.byte (LClassSpeedLookup >> 8 & $FF)
	.byte (LClassHPLookup >> 8 & $FF)
	.byte (LClassMPLookup >> 8 & $FF)
LLowEnemyStatPointers:
	.byte (LEnemyAttack & $FF)
	.byte (LEnemyMagic & $FF)
	.byte (LEnemySpeed & $FF)
	.byte (LEnemyHP & $FF)
	.byte 0 ;This should never be referenced
LHighEnemyStatPointers:
	.byte (LEnemyAttack >> 8 & $FF)
	.byte (LEnemyMagic >> 8 & $FF)
	.byte (LEnemySpeed >> 8 & $FF)
	.byte (LEnemyHP >> 8 & $FF)
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

LFrontlineModifiers:
	.byte $0 ;Knight
	.byte $0 ;Rogue
	.byte $0 ;Cleric
	.byte $1 ;Wizard
	.byte $1 ;Ranger
	.byte $0 ;Paladin
LBacklineModifiers:
	.byte $1 ;Knight
	.byte $1 ;Rogue
	.byte $1 ;Cleric
	.byte $0 ;Wizard
	.byte $0 ;Ranger
	.byte $1 ;Paladin

LIsClassRanged:
	.byte $0 ;Knight
	.byte $0 ;Rogue
	.byte $0 ;Cleric
	.byte $1 ;Wizard
	.byte $1 ;Ranger
	.byte $0 ;Paladin

	ORG $DFB0
	RORG $FFB0

LRunFunctionInSBank:
	sta $1FF9 ;Go to bank 3
	nop ;4
	nop
	nop
	nop
	nop
	nop
	nop ;10
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	nop ;20
	nop
	nop
	nop
	nop
	nop
	nop
	nop
	rts ;28

	ORG $DFD0
	RORG $FFD0

LGoToUpdateEffects:
	sta $1FF8 ;Go to bank 2
	nop ;
	nop ; jsr EUpdateEffects
	nop ;
	nop ;
	nop ; sta $1FF7
	nop ;
	jmp LAfterEffectUpdate

	ORG $DFE0
	RORG $FFE0

LGoToMainPicture:
	sta $1FF6 ;Go to bank 1, it is time to render the picture
	nop
	nop
	nop
LCatchFromMainPicture:
	nop
	nop
	nop
	jmp LOverscan

	ORG $DFFA
	RORG $FFFA

	;NMI, IRQ, and RESET information
	.word LReset
	.word LReset
	.word LReset


