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
	.byte #10
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
	beq EPrepSmallEnemy
	cmp #1
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
	lda #0
	sta enemyID
	lda #$FF
	sta enemyID+1
	sta enemyID+2
	sta enemyID+3
	rts

	;TODO figure this out
	jsr ERandom
	and #$03
	tax
	inx 
	stx temp1 ;Number of enemies to put in this encounter (1-4)
	lda EEnemyCounts,y
	sta temp2 ;Number of unique enemies that can randomly appear on this floor
.EGenerateEncounterOuterLoop:
	jsr ERandom
.EGenerateEncounterInnerLoop:
	sec
	sbc temp2


	rts

EEnemyCounts:
	.byte 1
	.byte 1
	.byte 1
	.byte 1

EGroundsEnemies:
	.byte $00
ECastleEnemies:
	.byte $00
ECatacombsEnemies:
	.byte $00
EAbyssEnemies:
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
	;CATACOMBS BOSS 1
	.byte $00
	.byte $00
	.byte $00
	.byte $00
	;CATACOMBS BOSS 2
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

ERandom: SUBROUTINE ;Ticks the random number generator when called
	lda rand8
	lsr
	bcc .ENoEOR
	eor #$B4
.ENoEOR:
	sta rand8
	rts

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
EEnemyGraphicsHighLookup: ;Stores the high bytes of the pointers to enemy graphics ordered by enemyID
	.byte (SmallTestEnemyGraphics >> 8 & $FF)
	.byte (MediumTestEnemyGraphics >> 8 & $FF)
	.byte (LargeTestEnemyGraphics >> 8 & $FF)
EEnemyColorsLowLookup: ;Stores the low bytes of the pointers to enemy color information ordered by enemyID
	.byte (SmallTestEnemyColors & $FF)
	.byte (MediumTestEnemyColors & $FF)
	.byte (LargeTestEnemyColors & $FF)
EEnemyColorsHighLookup: ;Stores the high bytes of the pointers to enemy color information ordered by enemyID
	.byte (SmallTestEnemyColors >> 8 & $FF)
	.byte (MediumTestEnemyColors >> 8 & $FF)
	.byte (LargeTestEnemyColors >> 8 & $FF)

	ORG $EF00
	RORG $FF00

EEnemySizes: ;Stores the size of each enemy by enemyID. 0 if the enemy is 8x8, 1 if the enemy is 16x16, 2 if the enemy is 32x32
	.byte 0
	.byte 1
	.byte 2

	ORG $EFA3
	RORG $EFA3

ENeedToGenerateEncounter:
	nop
	nop
	nop
	jsr EGenerateEncounter
	sta $1FF9 ;Go to bank 3
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


