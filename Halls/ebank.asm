	;BANK 2 - CONTAINS LOGIC AND DATA USED FOR THE RENDERING OF ENEMIES IN BATTLE

	ORG $E000
	RORG $F000

EReset:
	nop $1FF9 ;Go to bank 3, the correct startup bank

ETestEffect:
	.byte $10
	.byte $20
	.byte $30
	.byte $40
	.byte $50
	.byte $60
	.byte $70
	.byte $80
EFireEffect:
	.byte $26
	.byte $26
	.byte $26
	.byte $28
	.byte $28
	.byte $2A
	.byte $2C
	.byte $2A
	.byte $2C
	.byte $2A
	.byte $2C
	.byte $2E
	.byte $2C
	.byte $2E
	.byte $0E
	.byte $0E
EBasicEffect:
	.byte $0E
	.byte $00
	.byte $0E
	.byte $00
	.byte $0E

EEffectLowLookup:
	.byte 0 ;No effect
	.byte 0 ;Party member highlighting
	.byte 0 ;Transition to battle
	.byte 0 ;Transition to fire
	.byte 0 ;Transition to maze
	.byte (ETestEffect & $FF)
	.byte (EFireEffect & $FF)
	.byte (EBasicEffect & $FF)

EEffectHighLookup:
	.byte 0 ;No effect
	.byte 0 ;Party member highlighting
	.byte 0 ;Transition to battle
	.byte 0 ;Transition to fire
	.byte 0 ;Transition to maze
	.byte (ETestEffect >> 8 & $FF)
	.byte (EFireEffect >> 8 & $FF)
	.byte (EBasicEffect >> 8 & $FF)

EEffectLength:
	.byte #0
	.byte #0
	.byte #0
	.byte #0
	.byte #0
	.byte #8
	.byte #16
	.byte #5
EEffectFrequency:
	.byte #0
	.byte #30
	.byte #10
	.byte #10
	.byte #10 ;Sadge
	.byte #30
	.byte #4
	.byte #30

ERenderEffects:
	sta WSYNC
	lda #BATTLE_BOX_COLOR
	sta COLUPF
	lda #$FF
	sta PF0
	sta PF1
	sta PF2

	jsr EUpdateEffects

	ldx temp2
	beq ELine0
	cmp #1
	beq ELine1
ELine2:
	sta WSYNC
ELine1:
	sta WSYNC
ELine0:
	sta WSYNC
	lda #0
	sta PF2
	lda #$80
	sta PF1
	
ERenderEnemies:
	lda #0
	sta temp3 ;Will be used to hold the index of the next enemy to render
	ldx #77
	stx temp1 ;Will be used to hold the total height of the battle box, so this number can be tuned.
	jmp EEnemyRenderingLoop

EGoToEAfterRenderingEnemies:
	jmp EAfterRenderingEnemies

EEnemyRenderingLoop:
	ldx temp3 ;Get the current index into the enemyID list
	cpx #4
	beq EGoToEAfterRenderingEnemies ;All enemies have been rendered, stop the rendering routine
	lda enemyHP,x
	cmp #0
	bne ERenderEnemy
	;If the current enemy is dead, delay the requisite amount of scanlines...
	inx
	stx temp3
	ldx #19
EDrawBlankEnemyLoop:
	sta WSYNC
	dex
	bne EDrawBlankEnemyLoop
	lda temp1
	sec
	sbc #19
	sta temp1
	jmp EEnemyRenderingLoop
ERenderEnemy:
	lda currentMenu
	cmp #$81
	sta WSYNC
	bne EUseNormalPFColor
	cpx enemyAction ;Needs to be set to be the absolute index of the enemy that is being targeted
	bne EUseNormalPFColor
	lda effectCountdown
	and #$10
	bne EUseNormalPFColor
	lda #TEXT_HIGHLIGHTED_COLOR
	bne EStorePlayfieldColor
EUseNormalPFColor:
	lda #BATTLE_BOX_COLOR
EStorePlayfieldColor:
	sta COLUPF
	lda enemyID,x
	and #$3F
	tax
	lda EEnemySizes,x
	cmp #1
	beq EPrepSmallEnemy
	cmp #2
	beq EPrepMediumEnemy
	jmp EPrepLargeEnemy
EPrepSmallEnemy:
	lda EEnemyGraphicsHighLookup,x
	sta tempPointer1+1
	lda EEnemyGraphicsLowLookup,x
	sta tempPointer1
	lda EEnemyColorsHighLookup,x
	sta tempPointer5
	lda EEnemyColorsLowLookup,x
	sta temp5
	
	sta WSYNC
	jsr EDrawSmallEnemy
	ldx temp3
	inx
	stx temp3
	lda temp1
	sec
	sbc #19
	sta temp1
	jmp EEnemyRenderingLoop
EPrepMediumEnemy:
	lda EEnemyGraphicsHighLookup,x
	sta tempPointer1+1
	sta tempPointer2+1
	lda EEnemyGraphicsLowLookup,x
	sta tempPointer2
	clc
	adc #16
	sta tempPointer1
	lda EEnemyColorsHighLookup,x
	sta tempPointer5
	sta tempPointer6
	lda EEnemyColorsLowLookup,x
	sta temp6
	adc #16
	sta temp5
	nop
	nop
	nop
	sta WSYNC
	sta WSYNC
	jsr EDrawMediumEnemy
	ldx temp3
	inx
	inx
	stx temp3
	lda temp1
	sec
	sbc #38
	sta temp1
	jmp EEnemyRenderingLoop
EPrepLargeEnemy:
	lda EEnemyGraphicsHighLookup,x
	sta tempPointer1+1
	sta tempPointer2+1
	sta tempPointer3+1
	sta tempPointer4
	lda EEnemyGraphicsLowLookup,x
	sta temp4
	clc
	sta WSYNC
	adc #32
	sta tempPointer3
	adc #32
	sta tempPointer2
	sta WSYNC
	adc #32
	sta tempPointer1
	lda EEnemyColorsHighLookup,x
	sta tempPointer5
	sta tempPointer6
	lda EEnemyColorsLowLookup,x
	sta temp6
	adc #32
	sta temp5
	jsr EDrawLargeEnemy
	ldx temp3
	inx
	inx
	inx
	inx
	stx temp3
	lda temp1
	sec
	sbc #70
	sta temp1
	jmp EEnemyRenderingLoop
EAfterRenderingEnemies:
	ldx temp1
	inx
EAfterRenderingEnemiesLoop:
	sta WSYNC
	dex
	bne EAfterRenderingEnemiesLoop
	stx COLUBK ;Clear any spellcasting effects that may be present
	jmp EGoToDrawingBattleText

EDrawSmallEnemy: SUBROUTINE ;This subroutine is used for drawing enemies that are 8x8 pixels i size. Graphical information is interpreted from tempPointer1, and color information is interpreted from tempPointer5
	jsr ESpinWheels
	jsr ESpinWheels
	jsr ESpinWheels
	nop
	sta RESP0
	ldy #8
	lda #0
	sta NUSIZ0
	sta NUSIZ1
.EDrawSmallEnemyLoop:
	dey
	sta WSYNC
	bmi .EDoneDrawingSmallEnemy
	lda (tempPointer1),y
	sta GRP0
	lda (temp5),y
	sta COLUP0
	sta WSYNC
	jmp .EDrawSmallEnemyLoop
.EDoneDrawingSmallEnemy
	iny
	lda #BATTLE_BOX_COLOR
	sta COLUPF
	sty GRP0
	sty COLUP0
	sty HMP0
	rts

EDrawMediumEnemy: SUBROUTINE ;This subroutine is used for drawing enemies that are 16x16 pixels in size. Graphical information is interpreted from tempPointer1 and tempPointer2, color information is interpreted from tempPointer5 and tempPointer6.
	lda #$10 ;Moves one pixel to the left
	sta HMP1
	jsr ESpinWheels
	jsr ESpinWheels
	nop
	nop
	nop
	nop
	nop
	sta RESP0
	sta RESP1
	jsr ESpinWheels
	nop
	nop
	nop
	nop
	;sta WSYNC
	sta HMOVE ;Need to make this happen on cycle 73 exactly...
	ldy #16 ;Height of the enemy
	lda #0 ;No duplication
	sta NUSIZ0
	sta NUSIZ1
.EDrawMediumEnemyLoop:
	dey
	sta WSYNC
	bmi .EDoneDrawingMediumEnemy 
	lda (tempPointer1),y
	sta GRP0
	lda (tempPointer2),y
	sta GRP1
	lda (temp5),y
	sta COLUP0
	lda (temp6),y
	sta COLUP1
	sta WSYNC
	jmp .EDrawMediumEnemyLoop
.EDoneDrawingMediumEnemy:
	iny
	lda #BATTLE_BOX_COLOR
	sta COLUPF
	sty GRP0
	sty GRP1
	sty COLUP0
	sty COLUP1
ESpinWheels:
	rts

EDrawLargeEnemy: SUBROUTINE; This subroutine is used for drawing enemies that are 32x32 in size. Graphical information is pulled from tempPointers1-4, color information for columns 0 and 2 is pulled from tempPointer5, and color information for columns 1 and 3 is pulled from tempPointer6.
	lda #$10 ;Moves one pixel to the left
	sta HMP1
	nop
	nop
	sta RESP0
	sta RESP1
	sta WSYNC
	jsr ESpinWheels ;Loses 12 cycles
	jsr ESpinWheels
	jsr ESpinWheels
	jsr ESpinWheels
	jsr ESpinWheels
	cpx temp1
	cpx temp1
	cpx temp1
	sta HMOVE
	ldy #32
	lda #1 ;Two copies close
	sta NUSIZ0
	sta NUSIZ1
.EDrawLargeEnemyLoop:
	dey
	sta WSYNC
	bmi .EDoneDrawingLargeEnemy
	lda (tempPointer1),y
	sta GRP0
	lda (tempPointer2),y
	sta GRP1
	lda (temp5),y
	sta COLUP0
	lda (temp6),y
	sta COLUP1
	lda (tempPointer3),y
	tax
	lda (temp4),y
	stx GRP0
	sta GRP1
	sta WSYNC
	nop
	lda (tempPointer1),y
	sta GRP0
	lda (tempPointer2),y
	sta GRP1
	lda (temp5),y
	sta COLUP0
	lda (temp6),y
	sta COLUP1
	lda (tempPointer3),y
	tax
	lda (temp4),y
	stx GRP0
	sta GRP1
	jmp .EDrawLargeEnemyLoop
.EDoneDrawingLargeEnemy
	iny
	lda #BATTLE_BOX_COLOR
	sta COLUPF
	sty GRP0
	sty GRP1
	sty COLUP0
	sty COLUP1
	rts

EUpdateEffects: SUBROUTINE
	lda #2
	sta temp2
	lda currentEffect
	bne .EContinue
	rts
.EContinue
	ldx effectCountdown
	dex
	stx effectCountdown
	bne .ESkipEffectCounterDecrement
	tax
	lda EEffectFrequency,x
	sta effectCountdown
	ldy effectCounter
	beq .EEndEffect
	dey
	sty effectCounter
.ESkipEffectCounterDecrement:
	ldx currentEffect
	cpx #$1 ;party member highlighting
	beq .EHighlightEffect
	cpx #$5 ;Branch if in one of the three transitions
	bcc .ETransitionEffect
.ENormalEffect:
	lda EEffectLowLookup,x
	sta tempPointer1
	lda EEffectHighLookup,x
	sta tempPointer1+1
	ldy effectCounter
	lda (tempPointer1),y ;Get the current background color for this effect and effectCounter
	sta COLUBK
	lda #1
	sta temp2 ;Don't use an extra WSYNC if called during enemy rendering
	rts
.EEndEffect:
	sty currentEffect
	sty effectCounter
	sty effectCountdown
	sty COLUBK
	lda #1
	sta temp2
	rts
.EHighlightEffect:
	lda #1
	sta effectCounter
	sta temp2
	rts
.ETransitionEffect:
	lda #1
	sta temp2
	rts

EGenerateEncounter: SUBROUTINE ;Sets the enemyIDs to be an appropriate battle for this maze level. Also handles bosses.
	lda mazeAndPartyLevel
	lsr
	lsr
	lsr
	lsr
	and #$0F
	tay ;Y now contains the current maze level
	lda playerX
	asl
	asl
	asl
	asl
	ora playerY
	cmp exitLocation
	bne .ESetupNormalEncounter
	;This is a boss battle!
	tya
	asl
	asl
	asl ;A now contains mazeLevel*8
	sta temp3
	jsr ERandom
	and #$04
	clc
	adc temp3
	adc #3
	tay ;Y now contains mazeLevel*8 + (0 or 4 randomly) + 3
	ldx #3
.ECopyBossBattleLoop:
	lda EBossEncounters,y
	sta enemyID,x
	dey
	dex
	bpl .ECopyBossBattleLoop
	rts
.ESetupNormalEncounter:
	lda #$FF
	sta enemyID
	sta enemyID+1
	sta enemyID+2
	sta enemyID+3
	lda #0
	sta temp2

	jsr ERandom
	and #$03
	tax
	lda EEncounterSizes,x
	sta temp1 ;Amount of space to use in this encounter (2-4)
	lda mazeAndPartyLevel
	and #$F0
	clc
	adc #(EGroundsEnemies & $FF)
	sta tempPointer1
	lda #(EGroundsEnemies >> 8 & $FF)
	sta tempPointer1+1
.EEncounterGenLoop:
	jsr ERandom
	and #$0F
	tay
	lda (tempPointer1),y
	tax ;X now contains the enemy ID to try
	lda temp1
	cmp EEnemySizes,x
	bcs .EEnemyFits
.EEnemyDoesNotFit
	iny
	lda (tempPointer1),y
	tax
.EEnemyFits:
	;X now contains the enemyID that will be added to the encounter next
	ldy temp2
	stx enemyID,y

	lda temp2
	clc
	adc EEnemySizes,x
	sta temp2

	lda temp1
	sec
	sbc EEnemySizes,x
	sta temp1
	cmp #1
	bcs .EEncounterGenLoop ;If there is at least one space left, continue
.EEncounterComplete:
	rts

ERandom: SUBROUTINE ;Ticks the random number generator when called
	lda rand8
	lsr
	bcc .ENoEOR
	eor #$B4
.ENoEOR:
	sta rand8
	rts

ELoadString: SUBROUTINE ;Copies the string of ID X into temp1-temp6
	lda EMessagesLowLookup,x
	sta tempPointer1
	lda EMessagesHighLookup,x
	sta tempPointer1+1
	ldy #0
	lda (tempPointer1),y
	sta temp1
	iny
	lda (tempPointer1),y
	sta temp2
	iny
	lda (tempPointer1),y
	sta temp3
	iny
	lda (tempPointer1),y
	sta temp4
	iny
	lda (tempPointer1),y
	sta temp5
	iny
	lda (tempPointer1),y
	sta temp6
	jmp EAfterLoadingString


	ORG $E400
	RORG $F400

	;Only 12 more strings can be added with this particular information in the same bank as the encounter junk
EMessagesLowLookup:
	.byte 0
	.byte 0
	.byte 0
	.byte 0
	.byte (EStabsText & $FF) ;Rogue/Paladin
	.byte (EShootsText & $FF) ;Wizard/Ranger
	.byte (EBashesText & $FF) ;Cleric
	.byte (EBitesText & $FF)
	.byte (ERushesText & $FF) ;Knight 
	.byte (ECastsText & $FF)
	.byte (EHealsText & $FF)
	.byte (ELosesText & $FF)
	.byte (EMissesText & $FF)
	.byte (ELevelsText & $FF)
	.byte (EUpText & $FF)
	.byte (ELearnsText & $FF)
	.byte (EMovesText & $FF)
	.byte (EBacksText & $FF)
	.byte (EDownText & $FF)
	.byte (EAwayText & $FF)
	.byte (EWastesText & $FF)
	.byte (EWasText & $FF)
	.byte (ECuredText & $FF)
	.byte (EWakesText & $FF)
	.byte (EHasAText & $FF)
	.byte (EShieldMessageText & $FF)
	.byte (EPartyText & $FF)
	.byte (EFleesText & $FF)
	.byte (EWinsText & $FF)
	.byte (ETriesText & $FF)
	.byte (EToRunText & $FF)
	.byte (ENoText & $FF)
	.byte (EEffectText & $FF)
	.byte (ECannotText & $FF)
	.byte (EEscapeText & $FF)
	.byte (EGuardsText & $FF)
	.byte (EAttackText & $FF)
	.byte (EFellText & $FF)
	.byte (EAsleepText & $FF)
	.byte (EIsText & $FF)
	.byte (EIsOnText & $FF)
	.byte (EFadesText & $FF)
	.byte (EExiledText & $FF)
	.byte (EGameText & $FF)
	.byte (EClearText & $FF)
	.byte (EOverText & $FF)
	.byte (ETheText & $FF)
	.byte (EMazeText & $FF)
	.byte (EAwaitsText & $FF)
	.byte (EGuardMessageText & $FF)
	.byte (EBlocksText & $FF)
	.byte (EHPUpText & $FF)
	.byte (EMPUpText & $FF)
	.byte (EFullyText & $FF)
	.byte (EMixedText & $FF)
	.byte (EStatusText & $FF)
	.byte (EEmptyText & $FF)
	.byte (ENoText & $FF)
	.byte (ESpellsText & $FF)
	.byte (EKnownText & $FF)
	.byte (ECampText & $FF)
	.byte (ELeaveText & $FF)
	.byte (EFormText & $FF)
	.byte (EYourText & $FF)
	.byte (ETeamText & $FF)
	.byte (EReadyText & $FF)

EMessagesHighLookup:
	.byte 0
	.byte 0
	.byte 0
	.byte 0
	.byte (EStabsText >> 8 & $FF) ;Rogue/Paladin
	.byte (EShootsText >> 8 & $FF) ;Wizard/Ranger
	.byte (EBashesText >> 8 & $FF) ;Cleric
	.byte (EBitesText >> 8 & $FF)
	.byte (ERushesText >> 8 & $FF) ;Knight 
	.byte (ECastsText >> 8 & $FF)
	.byte (EHealsText >> 8 & $FF)
	.byte (ELosesText >> 8 & $FF)
	.byte (EMissesText >> 8 & $FF)
	.byte (ELevelsText >> 8 & $FF)
	.byte (EUpText >> 8 & $FF)
	.byte (ELearnsText >> 8 & $FF)
	.byte (EMovesText >> 8 & $FF)
	.byte (EBacksText >> 8 & $FF)
	.byte (EDownText >> 8 & $FF)
	.byte (EAwayText >> 8 & $FF)
	.byte (EWastesText >> 8 & $FF)
	.byte (EWasText >> 8 & $FF)
	.byte (ECuredText >> 8 & $FF)
	.byte (EWakesText >> 8 & $FF)
	.byte (EHasAText >> 8 & $FF)
	.byte (EShieldMessageText >> 8 & $FF)
	.byte (EPartyText >> 8 & $FF)
	.byte (EFleesText >> 8 & $FF)
	.byte (EWinsText >> 8 & $FF)
	.byte (ETriesText >> 8 & $FF)
	.byte (EToRunText >> 8 & $FF)
	.byte (ENoText >> 8 & $FF)
	.byte (EEffectText >> 8 & $FF)
	.byte (ECannotText >> 8 & $FF)
	.byte (EEscapeText >> 8 & $FF)
	.byte (EGuardsText >> 8 & $FF)
	.byte (EAttackText >> 8 & $FF)
	.byte (EFellText >> 8 & $FF)
	.byte (EAsleepText >> 8 & $FF)
	.byte (EIsText >> 8 & $FF)
	.byte (EIsOnText >> 8 & $FF)
	.byte (EFadesText >> 8 & $FF)
	.byte (EExiledText >> 8 & $FF)
	.byte (EGameText >> 8 & $FF)
	.byte (EClearText >> 8 & $FF)
	.byte (EOverText >> 8 & $FF)
	.byte (ETheText >> 8 & $FF)
	.byte (EMazeText >> 8 & $FF)
	.byte (EAwaitsText >> 8 & $FF)
	.byte (EGuardMessageText >> 8 & $FF)
	.byte (EBlocksText >> 8 & $FF)
	.byte (EHPUpText >> 8 & $FF)
	.byte (EMPUpText >> 8 & $FF)
	.byte (EFullyText >> 8 & $FF)
	.byte (EMixedText >> 8 & $FF)
	.byte (EStatusText >> 8 & $FF)
	.byte (EEmptyText >> 8 & $FF)
	.byte (ENoText >> 8 & $FF)
	.byte (ESpellsText >> 8 & $FF)
	.byte (EKnownText >> 8 & $FF)
	.byte (ECampText >> 8 & $FF)
	.byte (ELeaveText >> 8 & $FF)
	.byte (EFormText >> 8 & $FF)
	.byte (EYourText >> 8 & $FF)
	.byte (ETeamText >> 8 & $FF)
	.byte (EReadyText >> 8 & $FF)

EEncounterSizes:
	.byte 2
	.byte 3
	.byte 4
	.byte 4

	;Encounter tables must ALWAYS end with a small enemy, and every instance of a medium or large enemy MUST be IMMEDIATELY followed by a small enemy
	;Encounter tables must be a multiple of 2 in size. 16 happens to be the most convenient size.
EGroundsEnemies:
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $01
	.byte $00
	.byte $01
	.byte $00
	.byte $01
	.byte $00
	.byte $01
	.byte $00

ECastleEnemies:
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $01
	.byte $00
	.byte $01
	.byte $00
	.byte $01
	.byte $00
	.byte $01
	.byte $00

ECatacombsEnemies:
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $01
	.byte $00
	.byte $01
	.byte $00
	.byte $01
	.byte $00
	.byte $01
	.byte $00

EAbyssEnemies:
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $01
	.byte $00
	.byte $01
	.byte $00
	.byte $01
	.byte $00
	.byte $01
	.byte $00

EBossEncounters:
	;GROUNDS BOSS 1
	.byte $00
	.byte $FF
	.byte $FF
	.byte $FF
	;GROUNDS BOSS 2
	.byte $00
	.byte $FF
	.byte $FF
	.byte $FF
	;CASTLE BOSS 1
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	;CASTLE BOSS 2
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	;CRYPT BOSS 1
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	;CRYPT BOSS 2
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	;ABYSS BOSS 1
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	;ABYSS BOSS 2
	.byte $00
	.byte $00
	.byte $00
	.byte $00

	ORG $E500
	RORG $F500

EWastesText:
	.byte #W
	.byte #A
	.byte #S
	.byte #T
	.byte #E ;Shared, saving 1 byte
EStabsText:
	.byte #S
	.byte #T
	.byte #A
	.byte #B
	.byte #S
	.byte #EMPTY
EShootsText:
	.byte #S
	.byte #H
	.byte #O
	.byte #O
	.byte #T
	.byte #S
EBashesText:
	.byte #B 
	.byte #A
	.byte #S
	.byte #H
	.byte #E
	.byte #S
EBitesText:
	.byte #B
	.byte #I
	.byte #T
	.byte #E
	.byte #S
	.byte #EMPTY
ERushesText:
	.byte #R
	.byte #U
	.byte #S
	.byte #H
	.byte #E
	.byte #S
ECastsText:
	.byte #C
	.byte #A
	.byte #S
	.byte #T
	.byte #S
	.byte #EMPTY
EHealsText:
	.byte #H
	.byte #E
	.byte #A
	.byte #L
	.byte #S
	.byte #EMPTY
ELosesText:
	.byte #L
	.byte #O
	.byte #S
	.byte #E
	.byte #S
	.byte #EMPTY
EMissesText:
	.byte #M
	.byte #I
	.byte #S
	.byte #S
	.byte #E
	.byte #S
ELevelsText:
	.byte #L
	.byte #E
	.byte #V
	.byte #E
	.byte #L
	.byte #S
EUpText:
	.byte #U
	.byte #P
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
EMovesText:
	.byte #M
	.byte #O
	.byte #V
	.byte #E
	.byte #S
	.byte #EMPTY
EBacksText:
	.byte #B
	.byte #A
	.byte #C
	.byte #K
	.byte #S
	.byte #EMPTY
ELearnsText:
	.byte #L
	.byte #E
	.byte #A
	.byte #R
	.byte #N
EShieldMessageText:
	.byte #S
	.byte #H
	.byte #I
	.byte #E
	.byte #L
	.byte #D
EHasAText:
	.byte #H
	.byte #A
	.byte #S
	.byte #EMPTY
	.byte #A
	.byte #EMPTY
EPartyText:
	.byte #P
	.byte #A
	.byte #R
	.byte #T
	.byte #Y
	.byte #EMPTY
EDownText:
	.byte #D
	.byte #O
	.byte #W
	.byte #N
	.byte #EMPTY
	.byte #EMPTY
EAwayText:
	.byte #A
	.byte #W
	.byte #A
	.byte #Y
	.byte #EMPTY
	.byte #EMPTY
EWakesText:
	.byte #W
	.byte #A
	.byte #K
	.byte #E
	.byte #S
	.byte #EMPTY
EWasText:
	.byte #W
	.byte #A
	.byte #S
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
ECuredText:
	.byte #C
	.byte #U
	.byte #R
	.byte #E
	.byte #D
	.byte #EMPTY

	ORG $E600
	RORG $F600

EFleesText:
	.byte #F
	.byte #L
	.byte #E
	.byte #E
	.byte #S
	.byte #EMPTY
EWinsText:
	.byte #W
	.byte #I
	.byte #N
	.byte #S
	.byte #EMPTY
	.byte #EMPTY
ETriesText:
	.byte #T
	.byte #R
	.byte #I
	.byte #E
	.byte #S
	.byte #EMPTY
EToRunText:
	.byte #T
	.byte #O
	.byte #EMPTY
	.byte #R
	.byte #U
	.byte #N
ENoText:
	.byte #N
	.byte #O
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
EEffectText:
	.byte #E
	.byte #F
	.byte #F
	.byte #E
	.byte #C
	.byte #T
ECannotText:
	.byte #C
	.byte #A
	.byte #N
	.byte #N
	.byte #O
	.byte #T
EEscapeText:
	.byte #E
	.byte #S
	.byte #C
	.byte #A
	.byte #P
EExiledText:
	.byte #E
	.byte #X
	.byte #I
	.byte #L
	.byte #E
	.byte #D
EFadesText:
	.byte #F
	.byte #A
	.byte #D
	.byte #E
	.byte #S
	.byte #EMPTY
EIsOnText:
	.byte #I
	.byte #S
	.byte #EMPTY
	.byte #O
	.byte #N
	.byte #EMPTY
EIsText:
	.byte #I
	.byte #S
EEmptyText:
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
EAttackText:
	.byte #A
	.byte #T
	.byte #T
	.byte #A
	.byte #C
	.byte #K
EGuardsText:
	.byte #G
	.byte #U
	.byte #A
	.byte #R
	.byte #D
	.byte #S
EAsleepText:
	.byte #A
	.byte #S
	.byte #L
	.byte #E
	.byte #E
	.byte #P
EFellText:
	.byte #F
	.byte #E
	.byte #L
	.byte #L
	.byte #EMPTY
	.byte #EMPTY
ESpellsText:
	.byte #S
	.byte #P
	.byte #E
	.byte #L
	.byte #L
	.byte #S
EKnownText:
	.byte #K
	.byte #N
	.byte #O
	.byte #W
	.byte #N
	.byte #EMPTY
EGameText:
	.byte #G
	.byte #A
	.byte #M
	.byte #E
	.byte #EMPTY
	.byte #EMPTY
EOverText:
	.byte #O
	.byte #V
	.byte #E
	.byte #R
	.byte #EMPTY
	.byte #EMPTY
EClearText:
	.byte #C
	.byte #L
	.byte #E
	.byte #A
	.byte #R
	.byte #EMPTY
ETheText:
	.byte #T
	.byte #H
	.byte #E
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
EMazeText:
	.byte #M
	.byte #A
	.byte #Z
	.byte #E
	.byte #EMPTY
	.byte #EMPTY
EAwaitsText:
	.byte #A
	.byte #W
	.byte #A
	.byte #I
	.byte #T
	.byte #S
EGuardMessageText:
	.byte #G
	.byte #U
	.byte #A
	.byte #R
	.byte #D
	.byte #EMPTY
EBlocksText:
	.byte #B
	.byte #L
	.byte #O
	.byte #C
	.byte #K
	.byte #S
EHPUpText:
	.byte #H
	.byte #P
	.byte #EMPTY
	.byte #U
	.byte #P
	.byte #EMPTY
EMPUpText:
	.byte #M
	.byte #P
	.byte #EMPTY
	.byte #U
	.byte #P
	.byte #EMPTY
EFullyText:
	.byte #F
	.byte #U
	.byte #L
	.byte #L
	.byte #Y
	.byte #EMPTY
EMixedText:
	.byte #M
	.byte #I
	.byte #X
	.byte #E
	.byte #D
	.byte #EMPTY
EStatusText:
	.byte #S
	.byte #T
	.byte #A
	.byte #T
	.byte #U
	.byte #S
ECampText:
	.byte #C
	.byte #A
	.byte #M
	.byte #P
	.byte #EMPTY
	.byte #EMPTY
ELeaveText:
	.byte #L
	.byte #E
	.byte #A
	.byte #V
	.byte #E
EFormText:
	.byte #EMPTY
	.byte #F
	.byte #O
	.byte #R
	.byte #M
EYourText:
	.byte #EMPTY
	.byte #Y
	.byte #O
	.byte #U
	.byte #R
ETeamText:
	.byte #EMPTY
	.byte #T
	.byte #E
	.byte #A
	.byte #M
	.byte #EMPTY
EReadyText:
	.byte #R
	.byte #E
	.byte #A
	.byte #D
	.byte #Y
	.byte EMPTY

	ORG $E700
	RORG $F700

	ORG $EB00
	RORG $FB00

CampfireGraphics:
	.byte %11111000
	.byte %11111110
	.byte %11110000
	.byte %11011000
	.byte %11000000
	.byte %00111000
	.byte %10000000
	.byte %10000000
	.byte %11000000
	.byte %01000000
	.byte %01000000
	.byte %01100000
	.byte %01010000
	.byte %10000000
	.byte %00100000
	.byte %10010000
	.byte %10010000
	.byte %10000000
	.byte %01000000
	.byte %00100000
	.byte %10001000
	.byte %01010000
	.byte %01000000
	.byte %10000000
	.byte %01000100
	.byte %10000100
	.byte %10000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00011000
	.byte %00111111
	.byte %01011111
	.byte %11101111
	.byte %11111111
	.byte %11110001
	.byte %11111100
	.byte %11111110
	.byte %10110110
	.byte %11011101
	.byte %01101110
	.byte %10111111
	.byte %10011011
	.byte %10101101
	.byte %10101101
	.byte %00101010
	.byte %01010011
	.byte %01000011
	.byte %01100101
	.byte %01100101
	.byte %11010101
	.byte %10010001
	.byte %10010001
	.byte %11001011
	.byte %01000010
	.byte %10000100
	.byte %10000101
	.byte %00000101
	.byte %10001100
	.byte %00011000
	.byte %00010000
	.byte %00100000
	.byte %11111000
	.byte %00001100
	.byte %00111110
	.byte %11111111
	.byte %00111011
	.byte %11100111
	.byte %11011111
	.byte %11110111
	.byte %11011101
	.byte %11101101
	.byte %01011011
	.byte %10110101
	.byte %11001100
	.byte %01011001
	.byte %11110010
	.byte %10110010
	.byte %10110101
	.byte %11010001
	.byte %10011001
	.byte %11001100
	.byte %10011000
	.byte %11011001
	.byte %10011001
	.byte %10010101
	.byte %00010001
	.byte %00011001
	.byte %00110000
	.byte %01100001
	.byte %01000000
	.byte %00100001
	.byte %00110000
	.byte %00000000
	.byte %01111000
	.byte %00111111
	.byte %00011110
	.byte %00110111
	.byte %01100011
	.byte %11000000
	.byte %00000000
	.byte %00000001
	.byte %00000011
	.byte %00000010
	.byte %00000011
	.byte %00000111
	.byte %00001010
	.byte %00001011
	.byte %00010010
	.byte %00010001
	.byte %00011001
	.byte %00010001
	.byte %00010011
	.byte %00100001
	.byte %00010011
	.byte %00010001
	.byte %00000001
	.byte %00000010
	.byte %00000010
	.byte %00000001
	.byte %01000010
	.byte %01000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000

CampfireColors:
	.byte $f2
	.byte $f0
	.byte $f2
	.byte $f2
	.byte $f0
	.byte $f0
	.byte $f2
	.byte $42
	.byte $42
	.byte $40
	.byte $36
	.byte $40
	.byte $42
	.byte $42
	.byte $40
	.byte $36
	.byte $40
	.byte $fe
	.byte $42
	.byte $40
	.byte $40
	.byte $36
	.byte $42
	.byte $fe
	.byte $36
	.byte $fe
	.byte $40
	.byte $40
	.byte $36
	.byte $40
	.byte $fe
	.byte $40
	.byte $f0
	.byte $f2
	.byte $f0
	.byte $f0
	.byte $f2
	.byte $f2
	.byte $f0
	.byte $40
	.byte $36
	.byte $42
	.byte $40
	.byte $42
	.byte $40
	.byte $36
	.byte $40
	.byte $40
	.byte $42
	.byte $38
	.byte $40
	.byte $36
	.byte $fe
	.byte $42
	.byte $40
	.byte $36
	.byte $42
	.byte $36
	.byte $fe
	.byte $40
	.byte $42
	.byte $42
	.byte $40
	.byte $40

	ORG $EC00
	RORG $FC00

SmallTestEnemyGraphics:
	.byte %01111110
	.byte %11011011
	.byte %10100101
	.byte %10000001
	.byte %10100101
	.byte %10000001
	.byte %11000011
	.byte %01111110
MediumTestEnemyGraphics:
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %01111110
	.byte %00000000
	.byte %00000000
	.byte %01111110
	.byte %01000000
	.byte %01000000
	.byte %01111000
	.byte %01000000
	.byte %01000000
	.byte %01111110
	.byte %01111000
	.byte %00000100
	.byte %00000100
	.byte %00111000
	.byte %01000000
	.byte %01000000
	.byte %00111100
	.byte %00000000
	.byte %00000000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %01111110

SmallTestEnemyColors:
	.byte $9e
	.byte $9c
	.byte $9a
	.byte $98
	.byte $96
	.byte $94
	.byte $82
	.byte $80
MediumTestEnemyColors:
	.byte $6c
	.byte $6a
	.byte $68
	.byte $66
	.byte $64
	.byte $62
	.byte $60
	.byte $0
	.byte $ce
	.byte $cc
	.byte $ca
	.byte $c8
	.byte $c6
	.byte $c4
	.byte $c2
	.byte $c0
	.byte $4e
	.byte $4c
	.byte $48
	.byte $46
	.byte $44
	.byte $42
	.byte $40
	.byte $0
	.byte $9e
	.byte $9c
	.byte $8a
	.byte $8a
	.byte $88
	.byte $86
	.byte $82
	.byte $80

	ORG $ED00
	RORG $FD00

LargeTestEnemyGraphics:
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %11111110
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11111100
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11111100
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %01111111
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00111111
	.byte %00100000
	.byte %00100000
	.byte %00100000
	.byte %00100000
	.byte %00100000
	.byte %00111111
	.byte %00100000
	.byte %00100000
	.byte %00100000
	.byte %00100000
	.byte %00111111
	.byte %11100000
	.byte %00110000
	.byte %00011000
	.byte %00001000
	.byte %00001000
	.byte %00001000
	.byte %00111000
	.byte %11110000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00011000
	.byte %11110000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %11111110
	.byte %00011111
	.byte %00110000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00001111
	.byte %00011000
	.byte %00110000
	.byte %00100000
	.byte %00100000
	.byte %00100000
	.byte %00110000
	.byte %00011000
	.byte %00000111
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %01111111

LargeTestEnemyColors:
	.byte $c8
	.byte $c8
	.byte $c8
	.byte $c8
	.byte $c8
	.byte $c8
	.byte $c8
	.byte $c8
	.byte $c8
	.byte $c8
	.byte $c8
	.byte $c8
	.byte $c8
	.byte $c8
	.byte $c8
	.byte $c8
	.byte $c8
	.byte $0
	.byte $0
	.byte $0
	.byte $9c
	.byte $9c
	.byte $9c
	.byte $9c
	.byte $9c
	.byte $9c
	.byte $9c
	.byte $9c
	.byte $9c
	.byte $9c
	.byte $9c
	.byte $9c
	.byte $c0
	.byte $c0
	.byte $c0
	.byte $c0
	.byte $c0
	.byte $c0
	.byte $c0
	.byte $c0
	.byte $c0
	.byte $c0
	.byte $c0
	.byte $c0
	.byte $c0
	.byte $c0
	.byte $c0
	.byte $c0
	.byte $0
	.byte $0
	.byte $0
	.byte $0
	.byte $90
	.byte $90
	.byte $90
	.byte $90
	.byte $90
	.byte $90
	.byte $90
	.byte $90
	.byte $90
	.byte $90
	.byte $90
	.byte $90

	ORG $EE00
	RORG $FE00

EEnemyGraphicsLowLookup: ;Stores the low bytes of the pointers to enemy graphics ordered by enemyID
	.byte (SmallTestEnemyGraphics & $FF)
	.byte (MediumTestEnemyGraphics & $FF)
	.byte (LargeTestEnemyGraphics & $FF)
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte (CampfireGraphics & $FF)

EEnemyGraphicsHighLookup: ;Stores the high bytes of the pointers to enemy graphics ordered by enemyID
	.byte (SmallTestEnemyGraphics >> 8 & $FF)
	.byte (MediumTestEnemyGraphics >> 8 & $FF)
	.byte (LargeTestEnemyGraphics >> 8 & $FF)
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte (CampfireGraphics >> 8 & $FF)

EEnemyColorsLowLookup: ;Stores the low bytes of the pointers to enemy color information ordered by enemyID
	.byte (SmallTestEnemyColors & $FF)
	.byte (MediumTestEnemyColors & $FF)
	.byte (LargeTestEnemyColors & $FF)
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00

	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00

	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00

	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00

	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte (CampfireColors & $FF)

EEnemyColorsHighLookup: ;Stores the high bytes of the pointers to enemy color information ordered by enemyID
	.byte (SmallTestEnemyColors >> 8 & $FF)
	.byte (MediumTestEnemyColors >> 8 & $FF)
	.byte (LargeTestEnemyColors >> 8 & $FF)
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte (CampfireColors >> 8 & $FF)

	ORG $EF00
	RORG $FF00

EEnemySizes: ;Stores the size of each enemy by enemyID. 0 if the enemy is 8x8, 1 if the enemy is 16x16, 2 if the enemy is 32x32
	.byte 1
	.byte 2
	.byte 3
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	.byte 3 ;Campfire

	ORG $EFA3
	RORG $FFA3

ENeedToGenerateEncounter:
	nop
	nop
	nop
	jsr EGenerateEncounter
	sta $1FF9 ;Go to bank 3
	nop
	nop
	nop

	ORG $EFB0
	RORG $FFB0

ENeedToSetBattleMessage:
	nop
	nop
	nop
	jmp ELoadString
EAfterLoadingString:
	sta $1FF6 ;Go to bank 0
	nop
	nop
	nop

	ORG $EFC0
	RORG $FFC0

EGoToDrawingBattleText:
	nop $1FF6 ;Go to bank 0
	nop
	nop
	nop
ECatchFromDrawingBox:
	nop
	nop
	nop
	jmp ERenderEffects

	ORG $EFD0
	RORG $FFD0

ECatchFromMazeLogic:
	nop
	nop
	nop
	jsr EUpdateEffects
	sta $1FF9 ;Go to bank 3
	nop
	nop
	nop

	ORG $EFFA
	RORG $FFFA

	;NMI, IRQ, and RESET information
	.word EReset
	.word EReset
	.word EReset


