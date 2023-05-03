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

	lda #$02 ;Maze level 0, party level 1
	sta mazeAndPartyLevel

	ldy #3; Subroutine ID for SClearMazeData
	jsr LRunFunctionInSBank

	ldy #0; Subroutine ID for SGenerateMazeData
	jsr LRunFunctionInSBank

	;Temp testing code that will be removed much, much later
	lda #$30
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

	lda #$31
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
	sta enemyID+2
	sta enemyID+3
	lda #$03
	sta menuSize
	lda #1
	sta enemyHP
	; sta enemyHP+1
	; sta enemyHP+2
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
	ldy #2; Subroutine ID for SUpdatePlayerMovement
	jsr LRunFunctionInSBank

	ldy #1; Subroutine ID for SUpdateMazeRenderingPointers
	jsr LRunFunctionInSBank

	jsr LUpdateCompassPointerBoss
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
	jsr LUpdateMenuRendering

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
	jsr LUpdateMenuAdvancement
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
	beq .LCheckIfSpellHit
	cmp #$A1
	beq .LSpellKill
	cmp #$A2
	beq .LGiveDonationHealth
	cmp #$B0
	beq .LCheckIfAoESpellHit
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
	tax
	lda LSpellAoELookup,x
	bne .LIsAoE
.LSingleTarget:
	lda #$A0
	sta inBattle
	rts
.LIsAoE:
	lda #$B0
	sta inBattle
	lda #4
	sta aoeTargetsRemaining
	lda LSpellTargetingLookup,x
	
	rts
.LGiveDonationHealth:
	lda #$81
	sta inBattle
	rts
.LSpellKill:
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
.LCheckIfSpellHit:
	rts
.LCheckIfAoESpellHit:
	rts

LProcessFighting:
	lda inBattle
	cmp #$90
	beq .LCalculateFightDamage
	cmp #$91
	beq .LDamageWasAKill
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
	lda #$90
	sta inBattle
	rts
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

	jsr LGetTargetFromActionOffensive ;Returns the target in X
	stx startingCursorIndexAndTargetID

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
	lda #0
	sta currentMessage
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

LHasActionMasks:
	.byte #$80
	.byte #$40
	.byte #$20
	.byte #$10
	.byte #$08
	.byte #$04
	.byte #$02
	.byte #$01

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
	lda temp2
	jsr LBinaryToDecimal
	sta cursorIndexAndMessageY
	lda #$07 ;X LOSES Y HP
	sta currentMessage

	lda temp2 ;Damage to deal in binary
	ldx currentBattler
	stx startingCursorIndexAndTargetID
	ldy #POISON_RESIST_MASK
	jsr LApplyDamage
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

LApplyDamage: SUBROUTINE ;Applies binary damage A of damage type Y to target X. Returns 0 in A if target survived, FF if target died.
	sta temp2
	stx temp3
	sty temp4
	jsr LGetBattlerResistances
	and temp4
	beq .LDamageNotResisted
	lda temp2
	lsr
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
.LCheckBattlerPos:
	ldx currentBattler
	cpx #4
	bcs .LDoDamage ;Enemies do not have frontline/backline, so disregard the following checks

	lda char1,x
	and #$0F
	tay ;Save the class of this battler for later

	lda LPartyPositionMasks,x
	and partyBattlePos
	bne .LBattlerInFrontline
.LBattlerInBackline:
	lda LBacklineModifiers,y ;0 means normal damage, anything else means half
	bne .LHalfDamage
	beq .LDoDamage
.LBattlerInFrontline:
	lda LFrontlineModifiers,y
	beq .LDoDamage
.LHalfDamage:
	lsr temp2

.LDoDamage:
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
	bne .LPuttingTargetToSleep
	eor #$FF
	bne .LAddNewStatus
.LPuttingTargetToSleep:
	lda $E7
.LAddNewStatus:
	and battlerStatus,x
	ora temp4
	sta battlerStatus,x
	rts

LLowAllyStatPointers:
	.byte (LClassAttackLookup & $FF)
	.byte (LClassMagicLookup & $FF)
	.byte (LClassSpeedLookup & $FF)
	.byte (LClassHPLookup & $FF)
LHighAllyStatPointers:
	.byte (LClassAttackLookup >> 8 & $FF)
	.byte (LClassMagicLookup >> 8 & $FF)
	.byte (LClassSpeedLookup >> 8 & $FF)
	.byte (LClassHPLookup >> 8 & $FF)
LLowEnemyStatPointers:
	.byte (LEnemyAttack & $FF)
	.byte (LEnemyMagic & $FF)
	.byte (LEnemySpeed & $FF)
	.byte (LEnemyHP & $FF)
LHighEnemyStatPointers:
	.byte (LEnemyAttack >> 8 & $FF)
	.byte (LEnemyMagic >> 8 & $FF)
	.byte (LEnemySpeed >> 8 & $FF)
	.byte (LEnemyHP >> 8 & $FF)

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

;Interprets X as the cursorPosition
LCursorIndexToBattlerIndex: SUBROUTINE ;Converts the position of a menu cursor into the correct location in the array of the target (based on tempPointer1)
	ldy #0
	inx
.LIndexConversionLoop
	lda (tempPointer1),y
	cmp #0
	beq .LNoHit
	dex
	beq .LDone
.LNoHit:
	iny
	bpl .LIndexConversionLoop ;Saves byte over jmp
.LDone:
	rts ;Y is the correct offset into the enemyID array



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

LGetBattlerResistances: SUBROUTINE ;Will interpret X as the targetID to return the resistances of (in A). Format is LPFIDEP0
									;L: Legendary resist (Banish/Sleep), P: Physical, F: Fire, I:Ice, D: Divine, E: Electric, P: Poison
	cpx #4
	bcs .LHasResistanceByte
	lda #0
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

LUpdateMenuAdvancement: SUBROUTINE ;Checks if the button is pressed, and advances with the selected options if so.
	lda #$08
	bit currentInput
	beq .LContinue ;Return if the button is not pressed
.LReturn:
	rts
.LContinue:
	lda currentMenu
	beq .LReturn
	ldx currentBattler
	cmp #$80
	beq .LBattleOptionsMenu
	cmp #$81
	beq .LGoToSelectEnemyMenu
	cmp #$82
	beq .LSelectAllyMenu
	cmp #$83
	beq .LSelectOtherAllyMenu
	cmp #$84
	beq .LGoToSelectSpellMenu
	cmp #$85
	beq .LNoSpellsKnownMenu
	rts

.LGoToSelectEnemyMenu:
	jmp .LSelectEnemyMenu
.LGoToSelectSpellMenu:
	jmp .LSelectSpellMenu

.LNoSpellsKnownMenu
	lda #$80
	sta currentMenu
	rts

.LSelectOtherAllyMenu:
	ldx cursorIndexAndMessageY
	inx
	ldy #0
.LIndexConversionLoop
	cpy currentBattler
	beq .LIsCurrent
	dex
	beq .LSaveAllyTargeting
.LIsCurrent:
	iny
	bpl .LIndexConversionLoop ;Saves byte over jmp


.LSelectAllyMenu:
	ldy cursorIndexAndMessageY
.LSaveAllyTargeting:
	tya
	jsr L5Asl
	ldx currentBattler
	ora battleActions,x
	sta battleActions,x
	jmp .LCheckNextBattler
.LBattleOptionsMenu:
	ldy highlightedLine
	lda menuLines,y
	cmp #$80
	beq .LSetFightAction
	cmp #$81
	beq .LEnterSpellMenu
	cmp #$82
	beq .LSetMoveAction
	cmp #$83
	beq .LSetRunAction
	cmp #$84
	beq .LSetGuardAction
.LSetParryAction:
	lda #$04
	sta battleActions,x
	jmp .LCheckNextBattler
.LSetGuardAction:
	lda #$03
	sta battleActions,x
	lda #$83
	sta currentMenu
	lda #0
	sta cursorIndexAndMessageY
	lda #2
	sta menuSize
	rts
.LSetRunAction:
	lda #$02
	sta battleActions,x
	jmp .LCheckNextBattler
.LSetMoveAction:
	lda #$01
	sta battleActions,x
	jmp .LCheckNextBattler
.LSetFightAction:
	lda #$00
	sta battleActions,x
	;Check how many enemies are alive, proceeding to $81 if more than 1 is alive
	jsr LCheckEnemies
	cpx #2
	bcs .LNeedToTargetFight
	;Only one enemy, so auto-target this fight
	ldx currentBattler ;Changed by LCheckEnemies
	tya
	jsr L5Asl
	ora battleActions,x
	sta battleActions,x
	jmp .LCheckNextBattler
.LNeedToTargetFight
	lda #$81
	sta currentMenu
	dex
	stx menuSize
	lda #0
	sta cursorIndexAndMessageY
	rts
.LEnterSpellMenu:
	;Check how many spells this party member has
	ldx currentBattler
	lda char1,x
	and #$0F ;Get just the class of this battler
	tay
	lda mazeAndPartyLevel
	and #$0F ;Get the level of the party
	tax
	cpy #4
	bcs .LIsHalfCaster
	cpx #8
	bcs .LClampLevel
	bcc .LDontClampLevel
.LClampLevel:
	dex
.LDontClampLevel:
	stx menuSize
	ldy #0
	sty cursorIndexAndMessageY
	lda #$84
	sta currentMenu
	rts
.LIsHalfCaster:
	txa
	lsr
	beq .LLevel1HalfCaster
	tax
	bne .LDontClampLevel
.LLevel1HalfCaster
	lda #$85 ;Show a special message if this party member knows no spells (only possible for level 1 Paladin and Ranger)
	sta currentMenu
	rts
.LSelectSpellMenu:
	lda highlightedLine
	and #$7F
	tay
	lda menuLines,y
	and #$1F ;Get just the spell ID
	bne .LCheckSpellLogic
	;Back button was selected
	lda #$80
	sta currentMenu
	lda #1 ;Put the cursor on CAST
	sta cursorIndexAndMessageY
	lda #3
	sta menuSize
	rts
.LCheckSpellLogic:
	ldy highlightedLine
	bpl .LConfirmSpell
	;Not enough mana to select this spell. Play an error sound effect
	rts
.LConfirmSpell:	
	;Need to determine what the targeting of this spell is in order to advance to none or correct targeting
	ldx currentBattler
	ora battleActions,x
	ora #$80 ;Set the spell flag of the action
	sta battleActions,x
	and #$1F ;Get just the spell ID again
	tax
	lda LSpellTargetingLookup,x
	beq .LNoSpellTargeting
	cmp #1
	beq .LTargetEnemy
	cmp #2
	beq .LAllEnemies
	cmp #3
	beq .LTargetAlly
	cmp #4
	beq .LOtherAlly
	cmp #5
	beq .LAllAllies
	rts
.LTargetEnemy:
	jsr LCheckEnemies
	cpx #2
	bcs .LNeedToTargetSpell
	;Only one enemy, so auto-target this spell
	ldx currentBattler ;Changed by LCheckEnemies
	tya
	jsr L5Asl
	ora battleActions,x
	sta battleActions,x
	jmp .LCheckNextBattler
.LNeedToTargetSpell:
	lda #$81
	sta currentMenu
	dex
	stx menuSize
	lda #0
	sta cursorIndexAndMessageY
	rts
.LTargetAlly:
	lda #$82
	sta currentMenu
	lda #$03
	sta menuSize
	lda #0
	sta cursorIndexAndMessageY
	rts
.LOtherAlly:
	lda #$83
	sta currentMenu
	lda #$02
	sta menuSize
	lda #0
	sta cursorIndexAndMessageY
	rts
.LAllEnemies:
.LAllAllies:
.LNoSpellTargeting:
	jmp .LCheckNextBattler
.LSelectEnemyMenu:
	lda #enemyHP
	sta tempPointer1
	lda #0
	sta tempPointer1+1
	ldx cursorIndexAndMessageY
	jsr LCursorIndexToBattlerIndex
	tya
	jsr L5Asl
	ldx currentBattler
	ora battleActions,x
	sta battleActions,x
.LCheckNextBattler:
	ldx currentBattler
	cpx #3
	bcs .LNoMoreActions
.LFindNextBattler:
	inx
	cpx #4
	bcs .LNoMoreActions ;Make sure to avoid checking enemyHP!
	lda hp1,x
	beq .LFindNextBattler
	stx currentBattler ;Found another party member, keep getting actions!
	lda #$80
	sta currentMenu
	lda #3
	sta menuSize
	lda #0
	sta cursorIndexAndMessageY
	rts
.LNoMoreActions:
	lda #$81 ;No more party member actions to collect.
	sta inBattle
	lda #0
	sta currentMenu
	rts

LUpdateMenuRendering: SUBROUTINE ;Updates the menuLines and highlightedLine according to the current menu state
	lda currentMenu
	cmp #$80
	beq .LSetupBattleOptions
	cmp #$81
	beq .LGoToEnemyTargeting
	cmp #$82
	beq .LSetupAllyTargeting
	cmp #$83
	beq .LSetupOtherAllyTargeting
	cmp #$84
	beq .LGoToSpellOptions
	cmp #$85
	beq .LGoToNoSpellsKnown
.LReturn
	rts
.LGoToEnemyTargeting:
	jmp .LSetupEnemyTargeting
.LGoToSpellOptions:
	jmp .LSetupSpellOptions
.LGoToNoSpellsKnown:
	jmp .LSetupNoSpellsKnown

.LSetupBattleOptions:
	ldx currentBattler
	lda char1,x
	and #$0F ;Get just the class of this party member
	tay
	lda LBattleTables,y
	sta tempPointer1
	lda #(LKnightBattleTable >> 8 & $FF)
	sta tempPointer1+1

	jsr LSetMenuActiveLine

	ldx #0
	ldy startingCursorIndexAndTargetID
.LOptionLinesLoop:
	lda (tempPointer1),y
	sta menuLines,x
	inx
	iny
	cpx #3
	bcs .LReturn
	bne .LOptionLinesLoop 

.LSetupOtherAllyTargeting:
	ldy cursorIndexAndMessageY
	sty highlightedLine
	ldx #0
	ldy #0
.LOtherAllyLinesLoop:
	cpx #3
	bcs .LReturn
	cpy currentBattler
	beq .LIsSelf
	sty menuLines,x
	inx
.LIsSelf:
	iny
	bne .LOtherAllyLinesLoop

.LSetupAllyTargeting:
	jsr LSetMenuActiveLine

	ldx #0
	ldy startingCursorIndexAndTargetID
.LAllyLoop:
	sty menuLines,x
	iny
	inx
	cpx #3
	bcc .LAllyLoop
	rts

.LSetupEnemyTargeting:
	lda #$FF
	sta menuLines+2 ;Make sure that the last line is cleared if there are only two enemies
	lda menuSize
	cmp #3
	bcs .LFourEnemies
	ldy cursorIndexAndMessageY
	sty highlightedLine
	bpl .LSetEnemyLines
.LFourEnemies:
	jsr LSetMenuActiveLine

.LSetEnemyLines:
	lda menuSize
	cmp #2
	bcs .LMoreThanTwo
	lda #2
	bne .LAfterLoadingCorrectSize
.LMoreThanTwo:
	lda #3
.LAfterLoadingCorrectSize:
	sta temp1
	ldx #0
	ldy startingCursorIndexAndTargetID
	iny
	iny
	iny
	iny
.LEnemyLineLoop:
	lda battlerHP,y
	beq .LNoEnemyHere
	sty menuLines,x
	inx
.LNoEnemyHere:
	iny
	cpx temp1
	bcc .LEnemyLineLoop

	lda #enemyHP
	sta tempPointer1
	lda #0
	sta tempPointer1+1
	ldx cursorIndexAndMessageY
	jsr LCursorIndexToBattlerIndex
	sty enemyAction
.LDone:
	rts

.LSetupNoSpellsKnown:
	ldx #$E1
	stx menuLines
	inx
	stx menuLines+1
	inx
	stx menuLines+2
	rts

.LSetupSpellOptions:
	lda #$FF
	sta menuLines+2 ;Set the third line to not show in case there are only two options
	ldx currentBattler
	lda char1,x
	and #$0F ;Get just the class of this party member
	sta temp2
	tax
	lda LSpellListLookup,x
	sta tempPointer1
	lda #(LWizardSpellList >> 8 & $FF)
	sta tempPointer1+1 ;tempPointer1 now points to the spell list for this class

	lda menuSize
	cmp #2
	bcs .LMoreThanTwoSpellOptions
	lda #2
	bne .LAfterLoadingCorrectSpellSize
.LMoreThanTwoSpellOptions:
	lda #3
.LAfterLoadingCorrectSpellSize:
	sta temp1

	jsr LSetMenuActiveLine

	ldx temp2
	lda LCasterType,x
	bmi .LIsHalfCaster
	ldy startingCursorIndexAndTargetID
	jmp .LSetSpells
.LIsHalfCaster:
	lda startingCursorIndexAndTargetID
	asl
	tay
.LSetSpells:
	ldx #0
.LSetSpellLoop:
	lda (tempPointer1),y
	cmp #$FF
	beq .LSkipThisSpell
	sta menuLines,x
	inx
	cpx temp1
	bcs .LCheckMana
.LSkipThisSpell:
	iny
	bne .LSetSpellLoop

.LCheckMana:
	ldy highlightedLine
	lda menuLines,y
	tay
	lda LSpellManaLookup,y
	sta temp1
	ldx currentBattler
	lda mp1,x
	cmp temp1
	bcs .LEnoughMana
	lda highlightedLine
	ora #$80
	sta highlightedLine
.LEnoughMana:
.LSpellIDsSet:
	ldx #2
.LEnforceSpellLoop:
	lda menuLines,x
	ora #$C0
	sta menuLines,x
	dex
	bpl .LEnforceSpellLoop
	rts

LSetMenuActiveLine: SUBROUTINE
	lda menuSize
	cmp #3
	bcs .LAtLeastThree
	lda cursorIndexAndMessageY
	sta highlightedLine
	lda #0
	sta startingCursorIndexAndTargetID
	rts
.LAtLeastThree:
	ldy cursorIndexAndMessageY
	beq .LTopOfMenu
	cpy menuSize
	bcc .LMiddleOfMenu
.LBottomOfMenu:
	dey
	dey
	sty startingCursorIndexAndTargetID
	ldy #2
	sty highlightedLine
	rts
.LMiddleOfMenu:
	dey
	sty startingCursorIndexAndTargetID
	ldy #1
	sty highlightedLine
	rts
.LTopOfMenu:
	sty startingCursorIndexAndTargetID
	sty highlightedLine
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

LCheckEnemies: SUBROUTINE ;Returns the number of enemies currently alive in X, and the last index of an alive enemy in Y
	ldx #0
	ldy #0
.LCheckEnemyLoop:
	lda enemyHP,y
	beq .LEnemyDead
	sty tempPointer6
	inx
.LEnemyDead
	iny
	cpy #4
	bcc .LCheckEnemyLoop
	ldy tempPointer6
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

LUpdateCompassPointerNormal: SUBROUTINE ;Updates tempPointer1 to point to the letter N, S, E, or W based on which direction the player is facing
										;There is a good chance this will be removed in the final version
	ldy playerFacing
	beq .LCompassEast
	dey
	beq .LCompassSouth
	dey
	beq .LCompassWest
.LCompassNorth:
	lda #(RLetterN & $FF)
	jmp .LStoreCompassPointer
.LCompassSouth:
	lda #(RLetterS & $FF)
	jmp .LStoreCompassPointer
.LCompassWest:
	lda #(RLetterW & $FF)
	jmp .LStoreCompassPointer
.LCompassEast:
	lda #(RLetterE & $FF)
.LStoreCompassPointer:
	sta tempPointer1
	lda #(RLetterN >> 8 & $FF)
	sta tempPointer1+1
	rts

LUpdateCompassPointerBoss: SUBROUTINE ;Updates tempPointer1 in order to render an arrow at the top of the screen pointing towards this floor's exit
	;This is a good candidate for relocation to bank S
	lda exitLocation
	and #$0F
	sta temp2 ;Y location of boss
	lda exitLocation
	and #$F0
	jsr L4Lsr
	sta temp1 ;X location of boss
	sec
	sbc playerX
	sta temp1 ;X offset
	lda temp2
	sec
	sbc playerY
	sta temp2 ;Y offset

	lda temp1
	beq .LNoDeltaX
	bpl .LDeltaXPositive
.LDeltaXNegative:
	lda #$04
	sta temp3
	bne .LEncodeDeltaY
.LNoDeltaX:
	sta temp3
	beq .LEncodeDeltaY
.LDeltaXPositive
	lda #$08
	sta temp3
.LEncodeDeltaY:
	lda temp2
	beq .LNoDeltaY
	bpl .LDeltaYPositive
.LDeltaYNegative:
	lda #$01
	ora temp3
	bne .LGetArrowID
.LNoDeltaY:
	ora temp3
	bpl .LGetArrowID
.LDeltaYPositive:
	lda #$02
	ora temp3
.LGetArrowID:
	tax
	lda LArrows,x ;Get the correct arrowID if facing east
	cmp #$FF
	bne .LNotOnExit
	lda #(RLetterX & $FF)
	sta tempPointer1
	lda #(RLetterX >> 8 & $FF)
	sta tempPointer1+1
	rts

.LNotOnExit:
	;Get proper arrow ID according to facing bias
	ldy playerFacing
	iny
.LRotateLoop:
	dey
	beq .LDoneRotating
	sec
	sbc #2
	jmp .LRotateLoop
.LDoneRotating:
	cmp #0
	bpl .LNoUnderflow
	clc
	adc #8

.LNoUnderflow:
	;Set compass pointer and reflection state
	tax
	lda LArrowGraphicsLookup,X
	sta tempPointer1
	lda #(RArrowUp >> 8 & $FF)
	sta tempPointer1+1

	lda LArrowReflectionLookup,X
	sta REFP0

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

	ORG $DC40 ;Used to hold enemy stats and related data)
	RORG $FC40

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
 
;Format is LPFIHEP0
;L : Legendary (bosses), P : Physical, F : Fire, I : Ice, H : Holy, E : Electric, P : Poison
LEnemyResistances:
	.byte #%01011001 ;Zombie
	.byte #%00000000 ;Giant
	.byte #%10000010 ;Dragon

	ORG $DD00 ;Used to hold battle-related data.
	RORG $FD00

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

LNormalBattleTable:
	.byte $80
	.byte $81
	.byte $82
	.byte $83
LKnightBattleTable:
	.byte $80
	.byte $84
	.byte $82
	.byte $83
LRogueBattleTable:
	.byte $80
	.byte $85
	.byte $82
	.byte $83

LBattleTables:
	.byte (LKnightBattleTable & $FF)
	.byte (LRogueBattleTable & $FF)
	.byte (LNormalBattleTable & $FF)
	.byte (LNormalBattleTable & $FF)
	.byte (LNormalBattleTable & $FF)
	.byte (LNormalBattleTable & $FF)

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

	;103 bytes here

	ORG $DE00 ;Used to hold miscellaneous data/lookup tables
	RORG $FE00

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
	.byte 0 ;BACK
	.byte 1 ;FIRE
	.byte 1 ;SLEEP
	.byte 2 ;BLIZRD
	.byte 1 ;DRAIN
	.byte 0 ;THUNDR
	.byte 3 ;SHIELD
	.byte 1 ;METEOR
	.byte 2 ;CHAOS
	.byte 3 ;HEAL
	.byte 1 ;SMITE
	.byte 1 ;POISON
	.byte 3 ;SHARP
	.byte 1 ;BLIGHT
	.byte 5 ;TRIAGE
	.byte 1 ;WITHER
	.byte 2 ;BANISH
	.byte 0 ;TRANCE
	.byte 4 ;DONATE

LSpellManaLookup:
	.byte 0
	.byte 1
	.byte 1
	.byte 1
	.byte 1
	.byte 1
	.byte 1
	.byte 1
	.byte 1
	.byte 1
	.byte 1
	.byte 1
	.byte 1
	.byte 1
	.byte 1
	.byte 1
	.byte 1
	.byte 1
	.byte 1

LSpellAoELookup:
	.byte 0 ;BACK
	.byte 0 ;FIRE
	.byte 0 ;SLEEP
	.byte 1 ;BLIZRD
	.byte 0 ;DRAIN
	.byte 0 ;THUNDR
	.byte 0 ;SHIELD
	.byte 1 ;METEOR
	.byte 1 ;CHAOS
	.byte 0 ;HEAL
	.byte 0 ;SMITE
	.byte 0 ;POISON
	.byte 0 ;SHARP
	.byte 0 ;BLIGHT
	.byte 1 ;TRIAGE
	.byte 0 ;WITHER
	.byte 1 ;BANISH
	.byte 0 ;TRANCE
	.byte 0 ;DONATE

LCasterType:
	.byte 0
	.byte 0
	.byte 1
	.byte 1
	.byte $FF
	.byte $FF

	;Arrow IDs start with 0 at straight east, then increasing moving clockwise
LArrows:
	.byte $FF
	.byte 6
	.byte 2
	.byte $FF ;Unused
	.byte 4
	.byte 5
	.byte 3
	.byte $FF ;Unused
	.byte 0
	.byte 7
	.byte 1 ;Values for LArrows,11-15 should never be accessed

LArrowGraphicsLookup:
	.byte (RArrowUp & $FF)
	.byte (RArrowDiagonalUp & $FF)
	.byte (RArrowRight & $FF)
	.byte (RArrowDiagonalDown & $FF)
	.byte (RArrowDown & $FF)
	.byte (RArrowDiagonalDown & $FF)
	.byte (RArrowRight & $FF)
	.byte (RArrowDiagonalUp & $FF)

LArrowReflectionLookup:
	.byte #0
	.byte #0
	.byte #0
	.byte #0
	.byte #0
	.byte #%00001000
	.byte #%00001000
	.byte #%00001000

	;~280 bytes in here

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


