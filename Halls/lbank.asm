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

	lda #$1 ;Force a seed for the rng
	;lda INTIM ;Seed the random number generator
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
	lda #$00
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
	lda #$10
	sta hp4
	lda #$00
	sta mp4

	jsr LUpdateAvatars

	; lda #$80
	; sta inBattle
	; lda #$FF
	; sta hasAction
	; lda #$80
	; sta currentMenu
	; lda #$03
	; sta menuSize
	; lda #1
	; sta enemyHP
	; sta enemyHP+1
	; sta enemyHP+2
	; sta enemyHP+3

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
	bne LWaitForVblankTimer ;Skip this logic if we are not in maze mode...

	ldy #2; Subroutine ID for SUpdatePlayerMovement
	jsr LRunFunctionInSBank

	ldy #1; Subroutine ID for SUpdateMazeRenderingPointers
	jsr LRunFunctionInSBank

	jmp LUpdateCompassPointer


LGoToReset:
	jmp LReset

LUpdateCompassPointer:
	ldy playerFacing
	beq LCompassEast
	dey
	beq LCompassSouth
	dey
	beq LCompassWest
LCompassNorth:
	lda #(RLetterN & $FF)
	jmp LStoreCompassPointer
LCompassSouth:
	lda #(RLetterS & $FF)
	jmp LStoreCompassPointer
LCompassWest:
	lda #(RLetterW & $FF)
	jmp LStoreCompassPointer
LCompassEast:
	lda #(RLetterE & $FF)

LStoreCompassPointer:
	sta tempPointer1

	lda #(RLetterN >> 8 & $FF)
	sta tempPointer1+1

	jmp LGoToUpdateEffects

LAfterEffectUpdate:
LWaitForVblankTimer:
	lda INTIM
	bne LWaitForVblankTimer ;Is VBLANK over yet?
	sta WSYNC

	jmp LGoToMainPicture

LOverscan:
	lda #OVERSCAN_TIMER_DURATION
	sta TIM64T

	lda inBattle
	beq LMazeLogic ;Skip the following logic if we are in maze mode...

LBattleLogic:
	lda SWCHA
	and #$F0
	sta temp1
	lda INPT4
	and #$80
	lsr
	lsr
	lsr
	lsr
	ora temp1
	sta temp1
	lda currentMenu
	bpl LNotInMenu
LInMenu:
	lda temp1
	eor previousInput
	and #$F0
	beq LNoMenuMovement
LMenuMovement:
	jsr LUpdateMenuCursorPos
LNoMenuMovement:
	lda temp1
	eor previousInput
	and #$08
	beq LNoMenuAdvancement
LMenuAdvancement:
	jsr LUpdateMenuAdvancement
	lda inBattle
	cmp #$81
	bne LSkipBattleLogic 
	jsr LDetermineNextBattler ;If battle options menu was just exited, force a new battler to be chosen
	jsr LUpdateAvatars
	jsr LDoBattle
LNoMenuAdvancement:
	jmp LSkipBattleLogic
LNotInMenu:
	lda temp1
	eor previousInput
	and #$08
	beq LSkipBattleLogic ;Button is not pressed, so don't advance battle logic
	lda inBattle
	cmp #$81
	beq LNeedANewBattler
	jsr LUpdateAvatars
	jsr LDoBattle
	jmp LSkipBattleLogic
LNeedANewBattler
	jsr LDetermineNextBattler
	;lda inBattle
	jsr LUpdateAvatars
	jsr LDoBattle
LSkipBattleLogic:
	jmp LDoneWithSeparateLogic

LMazeLogic:
	;Need to check for the maze exit and campfire location
	;Need to determine if a random encounter occurs	

LDoneWithSeparateLogic:

	;Update the previousInput variable, since both maze and battle logic use this.
	lda SWCHA
	and #$F0
	sta temp1
	lda INPT4
	and #$80
	lsr
	lsr
	lsr
	lsr
	ora temp1
	sta previousInput

LWaitForOverscanTimer:
	lda INTIM
	bne LWaitForOverscanTimer

	sta WSYNC
	jmp LStartOfFrame

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
	lda INPT4
	bpl .LButtonPressed
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
	jmp .LDoneDelaying ;Rename this label if this code works
	rts
.LPartyDead:
	lda #$91
	sta inBattle
	rts
.LEnemiesDefeated:
	lda #$90
	sta inBattle
	rts
.LDoneDelaying:
	ldx #0
	stx hasAction
.LDetermineIfBattlerIsAlive:
	cpx #4
	bcs .LCheckEnemies
	lda hp1,x
	beq .LBattlerIsUnconscious
	bne .L1
.LCheckEnemies:
	stx temp1
	dex
	dex
	dex
	dex
	lda enemyHP,x
	ldx temp1
	cmp #0
	beq .LBattlerIsUnconscious
.L1:
	lda #1
	sta temp1
	bne .LAfterChecking
.LBattlerIsUnconscious:
	lda #0
	sta temp1
.LAfterChecking:
	lda hasAction
	asl
	ora temp1
	sta hasAction
	inx
	cpx #8
	bcc .LDetermineIfBattlerIsAlive

	lda #$80 ;At least one battler on each side is still alive, so continue the battle
	sta inBattle
	sta currentMenu
	lda #0
	sta cursorIndexAndMessageY
	sta battleActions
	sta battleActions+1
	sta battleActions+2
	sta battleActions+3
	sta enemyAction
	lda #3
	sta menuSize
	jsr LFindFirstLivingAlly
	stx currentBattler
	stx highlightedIndex
	rts
.LContinue:
	lda #0
	sta temp5 ;Will be used to hold the currentBattler value of the battler with the current max speed
	sta temp6 ;Will be used to hold the current max speed
	ldx #7
	lda inBattle
	cmp #$81
	beq .LFindMaxSpeed
	rts ;inBattle is not 81, so do not update the current battler
.LFindMaxSpeed
	lda hasAction
	and LHasActionMasks,x ;Go check the next person if this battler has already acted this turn
	beq .LNextIteration 
	cpx #4
	bcs .LCheckEnemySpeed
.LCheckAllySpeed:
	lda char1,x
	and #$0F ;Get the class of this character
	tay
	lda LClassSpeedLookup,y ;Get the pointer to the table for this class's speed data
	sta tempPointer1
	lda #(LStat1PerLevel >> 8 & $FF)
	sta tempPointer1+1
	lda mazeAndPartyLevel
	and #$0F ;Get the level of the party
	tay
	dey
	lda (tempPointer1),y ;Get the speed for this character's class and level
	cmp temp6
	bcc .LNextIteration
	stx temp5
	sta temp6	
	jmp .LNextIteration
.LCheckEnemySpeed:
	dex
	dex
	dex
	dex
	lda enemyHP,x
	inx
	inx
	inx
	inx
	cmp #0
	beq .LNextIteration
	dex
	dex
	dex
	dex
	lda enemyID,x ;Horrifically inefficient
	inx
	inx
	inx
	inx
	and #$3F
	tay
	lda LEnemySpeed,y
	cmp temp6
	bcc .LNextIteration ;This enemy's speed is not high enough to be the new max
	stx temp5 ;The currentBattler value
	sta temp6 ;The new speed value to compare against
.LNextIteration:
	dex
	bpl .LFindMaxSpeed
	ldx temp5
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
	lda #$00
	sta enemyAction
	rts

LKillTarget: SUBROUTINE ;Performs the correct housekeeping after a target has suffered lethal damage.
	ldx startingCursorIndexAndTargetID
	lda LHasActionMasks,x
	eor #$FF
	and hasAction
	sta hasAction ;Make sure this battler loses their action on death
	cpx #4
	bcs .LEnemyDied
.LFriendlyDied:
	lda #0
	sta hp1,x
	rts
.LEnemyDied:
	dex
	dex
	dex
	dex
	lda #0
	sta enemyHP,x
	rts

LDoBattle: SUBROUTINE ;Perform the correct battle logic and update the messages accordingly. This one's a doozy.
	rts

LCheckBattlerDied: SUBROUTINE ;Applies the binary damage in A to the battler ID of startingCursorIndexAndTargetID, and checks for death
	sta tempPointer1
	ldy startingCursorIndexAndTargetID
	cpy #4
	bcs .LEnemy
.LAlly:
	jsr LBinaryToDecimal
	sta tempPointer1
	lda hp1,y
	sec
	sed
	sbc tempPointer1
	cld
	beq .LAllyDied
	bcc .LAllyDied
	sta hp1,y ;Ally did not die
	lda #$0
	rts
.LAllyDied:
	lda #0
	sta hp1,y
	lda #$FF
	rts
.LEnemy:
	dey
	dey
	dey
	dey
	lda enemyHP,y
	sec
	sbc tempPointer1
	beq .LEnemyDied
	bcc .LEnemyDied
	sta enemyHP,y ;Enemy did not die
	lda #$0
	rts
.LEnemyDied
	lda #0
	sta enemyHP,y
	lda #$FF
	rts

LFindAoETarget: SUBROUTINE ;Finds the next target for AoE spells, returning the correct id in A. Returns $FF if there are no more targets.
	ldx aoeTargetID
	cpx #4
	bcs .LLookingForEnemyID
.LLookingForAllyID:
	inx
	lda hp1,x
.LLookingForEnemyID:
	
	rts

LGetBattlerMagic: SUBROUTINE ;Returns the magic power of the currentBattler in A
	ldx currentBattler
	cpx #4
	bcs .LFindEnemyMagic
.LFindAllyMagic:
	lda mazeAndPartyLevel
	and #$0F
	tay ;Y now contains the level of the party
	lda char1,x
	and #$0F ;Get just the class of this battler
	tax
	lda LClassMagicLookup,x
	sta tempPointer1
	lda #(LStat1PerLevel >> 8 & $FF)
	sta tempPointer1+1
	lda (tempPointer1),y
	rts
.LFindEnemyMagic:
	lda enemyID,x
	and #$3F ;Get this enemy's id
	tax
	lda LEnemyMagic,x
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



LBinaryToDecimal: SUBROUTINE ;Will interpret A as the number in binary to convert to decimal.
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
	asl
	asl
	asl
	asl
	ora tempPointer2
	rts

LGetEnemyResistances: SUBROUTINE ;Will interpret Y as the enemy targetID to return the resistances of (in A). Format is LPFIGEP0
	dey
	dey
	dey
	dey
	lda enemyID,y
	tay
	lda LEnemyResistances,y
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
	lda INPT4
	bpl .LContinue ;Return if the button is not pressed
.LReturn:
	rts
.LContinue:
	lda currentMenu
	beq .LReturn
	ldx currentBattler
	cmp #$80
	beq .LBattleOptionsMenu
	cmp #$81
	beq .LSelectEnemyMenu
	cmp #$84
	beq .LSelectSpellMenu
.LSelectAllyMenuUnique:
.LSelectAllyMenu:
	lda #hp1
	sta tempPointer1
	lda #0
	sta tempPointer1+1
	ldx cursorIndexAndMessageY
	jsr LCursorIndexToBattlerIndex ;This doesn't correctly handle unique ally selection
	tya
	asl
	asl
	asl
	asl
	asl
	ldx currentBattler
	ora battleActions,x
	sta battleActions,x
	jmp .LCheckNextBattler
.LSelectSpellMenu:
	;Need to determine what the targeting of this spell is in order to advance to none or correct targeting

	jmp .LCheckNextBattler
.LBattleOptionsMenu:
	jmp .LCheckNextBattler
.LSelectEnemyMenu:
	;Determine how many enemies are alive
.LCheckNextBattler:
	rts

LUpdateMenuRendering: SUBROUTINE ;Updates the menuLines and highlightedLine according to the current menu state
	rts

LUpdateMenuCursorPos: SUBROUTINE ;Updates the cursor according to joystick presses
	ldy cursorIndexAndMessageY
	lda #DOWN_MASK
	bit SWCHA
	beq .LDownPressed
	lda #UP_MASK
	bit SWCHA
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



LUpdateAvatars: SUBROUTINE
	lda inBattle
	bpl .LContinue
	lda INPT4
	bpl .LContinue
	rts
.LContinue:
	ldy #3
.LUpdateAvatarLoop:
	;Check for status effect
	tya

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

	tya
	tax

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

	txa
	tay

.LChangeMoodLater:
	lda char1,y
	and #$0F
	ora tempPointer1
	sta char1,y
	dey
	bpl .LUpdateAvatarLoop
	rts

LOverrideAvatar: SUBROUTINE ;Sets party member Y's mood to X.
	lda char1,y
	and #$0F ;Get just the class
	sta temp6
	txa
	asl
	asl
	asl
	asl
	ora temp6
	sta char1,y
	rts

	ORG $DC40 ;Used to hold enemy stats and related data
	RORG $FC40

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

LMaxSpellsKnown:
	.byte #0
	.byte #0
	.byte #8 ;Cleric
	.byte #8 ;Wizard
	.byte #4 ;Ranger
	.byte #4 ;Paladin

LSpellListLookup:
	.byte #0
	.byte #0
	.byte (LClericSpellList & $FF)
	.byte (LWizardSpellList & $FF)
	.byte (LRangerSpellList & $FF)
	.byte (LPaladinSpellList & $FF)

LSpellTargetingLookup: ;Contains a 0 if the spell does not have specific targeting, a 1 if it targets enemies, and a 2 if it targets allies
	.byte $FF ;Back
	.byte 1 ;Fire
	.byte 0 ;Blizzard
	.byte 2 ;Invisible
	.byte 1 ;Hold
	.byte 1 ;Fira
	.byte 0 ;Vanish
	.byte 0 ;Holdra
	.byte 0 ;Flare
	.byte 2 ;Heal
	.byte 1 ;Smite
	.byte 2 ;Cure
	.byte 2 ;Shield
	.byte 0 ;Restore
	.byte 1 ;Blight
	.byte 0 ;Aegis
	.byte 0 ;Banish

LWizardSpellList:
	.byte #16 ;Back
	.byte #0 ;Fire
	.byte #1 ;Blizzard
	.byte #2 ;Invisible
	.byte #3 ;Hold
	.byte #4 ;Fira
	.byte #5 ;Vanish
	.byte #6 ;Holdra
	.byte #7 ;Flare
LClericSpellList:
	.byte #16 ;Back
	.byte #8 ;Heal
	.byte #9 ;Smite
	.byte #10 ;Cure
	.byte #11 ;Shield
	.byte #12 ;Restore
	.byte #13 ;Blight
	.byte #14 ;Aegis
	.byte #15 ;Banish
LRangerSpellList:
	.byte #16 ;Back
	.byte #8 ;Heal
	.byte #2 ;Invis
	.byte #10 ;Cure
	.byte #11 ;Shield
	.byte #5 ;Vanish
	.byte #13 ;Blight
LPaladinSpellList:
	.byte #16 ;Back
	.byte #0 ;Fire
	.byte #8 ;Heal
	.byte #9 ;Smite
	.byte #3 ;Hold
	.byte #11 ;Shield
	.byte #4 ;Fira

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
	nop ; JSR EUpdateEffects
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


