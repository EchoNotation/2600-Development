	ORG $C000
	RORG $F000

	;BANK 0 - CONTAINS LOGIC AND DATA PERTAINING TO RENDERING THE MAZE VIEW AND THE MAJORITY OF THE BATTLE SCENE

;Sets all registers and RAM to 0. Also ensures that the processor is not in decimal mode.
RReset:
	nop $1FF9 ;Switch to bank 3, which contains the startup information

RMainPicture:
	ldx #$80
	stx VBLANK ;Disable blanking

	;Draw picture to the screen
	lda currentMenu
	cmp #$FF
	beq RGoToRenderSetupScreen
	lda inBattle
	beq RRenderMazeView
	jmp RBattleRendering

RGoToRenderSetupScreen
	jmp RRenderSetupScreen

RRenderMazeView:
RPrepareToDrawMaze:
	lda mazeAndEffectColor
	sta COLUPF
	lda #0
	sta REFP0
	sta HMCLR
	sta GRP0
	ldy #MAZE_HEIGHT
	lda aoeValueAndCampfireControl
	beq RConfigureNearFire
	bmi RGoToDrawMazeNoFire
RConfigureFarFire:
	sta WSYNC
	jsr RSpinWheels
	jsr RSpinWheels
	jsr RSpinWheels
	nop
	nop
	nop
	nop
	sta RESP0
	lda #$20
	sta HMP0
	sta WSYNC
	sta HMOVE
	lda #CAMPFIRE_COLOR
	sta COLUP0
	lda #(RDrawMazeFarFire >> 8 & $FF)
	sta tempPointer1+1
	lda #(RDrawMazeFarFire & $FF)
	sta tempPointer1
	lda #FAR_FIRE_MAZE_HEIGHT
	bne RStoreFireMazeHeight
RConfigureNearFire:
	sta WSYNC
	jsr RSpinWheels
	jsr RSpinWheels
	jsr RSpinWheels
	nop
	nop
	lda #$10
	sta RESP0
	sta RESP1
	sta HMP1
	lda #CAMPFIRE_COLOR
	sta COLUP0
	sta COLUP1
	sta WSYNC
	sta HMOVE
	lda #(RDrawMazeNearFire >> 8 & $FF)
	sta tempPointer1+1
	lda #(RDrawMazeNearFire & $FF)
	sta tempPointer1
	lda #NEAR_FIRE_MAZE_HEIGHT
	bne RStoreFireMazeHeight

RGoToDrawMazeNoFire:
	sta WSYNC
	sta WSYNC
	jmp RDrawMazeNoFire

	;40 cycles between sta WSYNC and stx PF2 in order to maintain proper maze functionality

RStoreFireMazeHeight:
	sta fireMazeHeightAndMessageLine
	lda #3
	sta charIndex
RDrawMazePreFire:
	sta WSYNC
	lda ROutermost,y; 4 cycles
	sta PF0 ;2
	lda (tempPointer2),y ;5
	sta PF1 ;2
	lda (tempPointer3),y ;5
	sta PF2 ;2
	lda (temp4),y ;5
	tax ;2
	lda (temp5),y ;5

	nop ;2
	nop ;2
	nop ;2
	nop ;2
	stx PF2 
	sta PF1

	dec charIndex
	bne RDrawMazePreFire
	lda #3
	sta charIndex
	dey
	cpy fireMazeHeightAndMessageLine
	bcs RDrawMazePreFire
	jmp (tempPointer1)
RDrawMazeFarFire:
	sta WSYNC
	lda ROutermost,y; 4 cycles
	sta PF0 ;2
	lda (tempPointer2),y ;5
	sta PF1 ;2
	lda (tempPointer3),y ;5
	sta PF2 ;2

	lda FAR_FIRE_GRAPHICS+1,y ;4
	sta GRP0 ;3

	lda (temp4),y ;5
	tax ;2
	lda (temp5),y ;5
	stx PF2 
	sta PF1

	sta WSYNC
	lda ROutermost,y; 4 cycles
	sta PF0 ;2
	lda (tempPointer2),y ;5
	sta PF1 ;2
	lda (tempPointer3),y ;5
	sta PF2 ;2

	lda FAR_FIRE_GRAPHICS+4,y ;4
	sta GRP0 ;3

	lda (temp4),y ;5
	tax ;2
	lda (temp5),y ;5
	stx PF2 
	sta PF1
	
	sta WSYNC
	lda ROutermost,y; 4 cycles
	sta PF0 ;2
	lda (tempPointer2),y ;5
	sta PF1 ;2
	lda (tempPointer3),y ;5
	sta PF2 ;2

	lda FAR_FIRE_GRAPHICS+7,y ;4
	sta GRP0 ;3

	lda (temp4),y ;5
	tax ;2
	lda (temp5),y ;5

	stx PF2 
	sta PF1
	dey
	cpy #FAR_FIRE_MAZE_HEIGHT - 3
	bcs RDrawMazeFarFire
	lda #0
	sta GRP0
	beq RDrawMazeNoFire

RDrawMazeNearFire:
	sta WSYNC
	lda ROutermost,y; 4 cycles
	sta PF0 ;2
	lda (tempPointer2),y ;5
	sta PF1 ;2
	lda (tempPointer3),y ;5
	sta PF2 ;2

	lda #NEAR_FIRE_GRAPHICS1+1,y
	sta GRP0

	lda (temp4),y ;5
	tax ;2
	lda (temp5),y ;5
	stx PF2 
	sta PF1

	lda #NEAR_FIRE_GRAPHICS2+1,y
	sta GRP1

	sta WSYNC
	lda ROutermost,y; 4 cycles
	sta PF0 ;2
	lda (tempPointer2),y ;5
	sta PF1 ;2
	lda (tempPointer3),y ;5
	sta PF2 ;2

	lda #NEAR_FIRE_GRAPHICS1+6,y
	sta GRP0

	lda (temp4),y ;5
	tax ;2
	lda (temp5),y ;5
	stx PF2 
	sta PF1
	
	lda #NEAR_FIRE_GRAPHICS2+6,y
	sta GRP1

	sta WSYNC
	lda ROutermost,y; 4 cycles
	sta PF0 ;2
	lda (tempPointer2),y ;5
	sta PF1 ;2
	lda (tempPointer3),y ;5
	sta PF2 ;2

	lda #NEAR_FIRE_GRAPHICS1+11,y
	sta GRP0

	lda (temp4),y ;5
	tax ;2
	lda (temp5),y ;5
	stx PF2 
	sta PF1

	lda #NEAR_FIRE_GRAPHICS2+11,y
	sta GRP1

	dey
	cpy #NEAR_FIRE_MAZE_HEIGHT - 5
	bcs RDrawMazeNearFire
	lda #0
	sta GRP0
	sta GRP1
	lda #3
	sta charIndex
	bne RDrawMazeNoFireNoExtraLine
RDrawMazeNoFire:
	lda #3
	sta charIndex
RDrawMazeNoFireLoop:	
	sta WSYNC
RDrawMazeNoFireNoExtraLine:
	lda ROutermost,y; 4 cycles
	sta PF0 ;2
	lda (tempPointer2),y ;5
	sta PF1 ;2
	lda (tempPointer3),y ;5
	sta PF2 ;2
	lda (temp4),y ;5
	tax ;2
	lda (temp5),y ;5

	nop ;2
	nop ;2
	nop ;2
	nop ;2
	stx PF2 
	sta PF1
	dec charIndex
	bne RDrawMazeNoFireLoop
	lda #3
	sta charIndex
	dey
	cpy #MAZE_HEIGHT+10
	bcc RDrawMazeNoFireLoop
RDoneDrawingMaze:
	sta WSYNC
	ldy #0
	sty PF0
	sty PF1
	sty PF2
	sty charIndex
RPlaceCompass:
	sta WSYNC
	lda mazeAndPartyLevel
	lsr
	lsr
	lsr
	lsr
	tay ;This doesn't do anything...
	lda mazeAndEffectColor
	sta COLUP0

	;Delay in order to put the compass in the middle of the screen
	;cpx temp1
	jsr RSpinWheels
	lda #$C0
	sta HMP0
	lda enemyAction
	sta REFP0
	sta RESP0
	sta WSYNC
	sta HMOVE
	ldy #CHARACTER_HEIGHT
RDrawCompass:
	sta WSYNC
	dey
	bmi RDrawPartyInfoMaze
	lda (temp6),y
	sta GRP0
	jmp RDrawCompass

RDrawPartyInfoMaze:
	cmp temp1
	cmp temp1
	cmp temp1

	ldx #$03 ;Triplicate
	stx NUSIZ0 ;Set both duplication registers to triplicate the sprites.
	stx NUSIZ1
	stx VDELP0
	stx VDELP1

	;Need some sort of delay here in order to more or less center this data.
	lda #0
	sta GRP0
	cmp temp1
	sta RESP0
	sta RESP1

	sta REFP0
	
	ldx #$E0 ;Moves one color clock to the left.
	sta GRP1
	stx HMP0
	lda #$F0
	sta HMP1

	ldy #3
.LWaitToDrawCharacterInfoLoop:
	sta WSYNC
	dey
	bne .LWaitToDrawCharacterInfoLoop
	sta WSYNC
	sta HMOVE

	jsr RDrawCharacterInfo
	inc charIndex
	sta WSYNC
	jsr RDrawCharacterInfo
	inc charIndex
	sta WSYNC
	jsr RDrawCharacterInfo
	inc charIndex
	sta WSYNC
	jsr RDrawCharacterInfo

	ldx #0
	stx NUSIZ0
	stx NUSIZ1
	stx VDELP0
	stx VDELP1
	stx charIndex

	ldx #2 ;This number is extremely arbitrary-- Because I don't really understand exactly how long the rendering of party info takes.

RFinishMazePicture:
	sta WSYNC
	dex
	bne RFinishMazePicture
	jmp RAfterRendering





RBattleRendering:
	ldx #4
RWaitToDrawBattleBox:
	sta WSYNC
	dex
	bne RWaitToDrawBattleBox

	jmp RGoToEnemyDrawingBank

RPlaceBattleText:
	lda #$FF
	sta PF1
	sta PF2

	sta WSYNC
	ldx #$03 ;Triplicate
	stx NUSIZ0 ;Set both duplication registers to triplicate the sprites.
	stx NUSIZ1
	stx VDELP0
	stx VDELP1
	lda #0
	sta GRP0
	sta GRP1
	sta GRP0
	sta GRP1

	ldx #$F0 ;Moves one color clock to the right.
	stx HMP1

	cmp temp1

	sta RESP0
	sta RESP1

	lda #$E0
	sta HMP0

	sta WSYNC
	sta WSYNC
	sta HMOVE

	lda #0
	sta PF1
	sta PF2
	sta PF0

	lda currentMenu
	sta WSYNC

	bpl RGoToRDrawBattleText
	jmp RDrawBattleMenu

RGoToRDrawBattleText:
	jmp RDrawBattleText
RGoToPostDrawingBattleText:
	jmp RPostDrawingBattleText

RDrawBattleMenu:
	ldx #$FF ;-1
	stx aoeTargetID
RDrawBattleMenuLoop:
	inc aoeTargetID ;Just for temporary storage of which line # this is
	ldx aoeTargetID
	cpx #3
	bcs RGoToPostDrawingBattleText
	lda menuLines,x
	cmp #$FF
	bne RNoBlankLines

	;Need to waste a lot of time here
	ldy #14
RDoNothingLoop:
	sta WSYNC
	dey
	bne RDoNothingLoop
	jmp RDrawBattleMenuLoop
RGoToSpecialOptions:
	jmp RShowSpecialOptions
RNoBlankLines:
	ldx aoeTargetID
	cmp #$0
	bpl RShowBattlerName
	and #$E0
	cmp #$80
	beq RShowBattleOptions
	cmp #$C0
	bne RGoToSpecialOptions
RShowSpellOptions:
	sta WSYNC
	lda menuLines,x
	and #$3F
	tay
	lda RSpellTextLookupTable,y
	sta tempPointer1
	lda #(RFireText >> 8 & $FF)
	sta tempPointer1+1
	
	lda highlightedLineAndSteps
	and #$7F
	sta temp4
	lda highlightedLineAndSteps
	bpl REnoughMana
	cpx temp4
	bne RDontHighlightSpell ;Branch if this line isn't hovered
	lda #TEXT_INVALID_COLOR
	bne RStoreSpellColor
REnoughMana:
	cpx temp4
	bne RDontHighlightSpell ;Branch if this line isn't hovered
	lda RSpellColors,y
	bne RStoreSpellColor ;No spells are black
RDontHighlightSpell:
	lda #TEXT_COLOR
RStoreSpellColor:
	jsr RDrawBattleMenuLine
	jmp RDrawBattleMenuLoop
RShowBattleOptions:
	sta WSYNC
	sta WSYNC
	lda menuLines,x
	and #$0F
	tay
	lda RBattleOptionsLookupTable,y
	sta tempPointer1
	lda #(RFightText >> 8 & $FF)
	sta tempPointer1+1
	lda #TEXT_COLOR
	cpx highlightedLineAndSteps
	bne RDontHighlightBattleOption
	lda #TEXT_HIGHLIGHTED_COLOR
RDontHighlightBattleOption:
	jsr RDrawBattleMenuLine
	jmp RDrawBattleMenuLoop
RShowBattlerName:
	sta WSYNC
	and #$04
	bne RShowEnemyName
RShowAllyName:
	sta WSYNC
	lda menuLines,x
	tay
	lda name1,y
	sta temp1
	lda name2,y
	sta temp2
	lda name3,y
	sta temp3
	lda name4,y
	sta temp4
	lda name5,y
	sta temp5
	lda #EMPTY
	sta temp6

	cpx highlightedLineAndSteps
	bne .RDontHighlightAllyName
	lda char1,y
	and #$0f
	tay
	lda RClassColors,y
	bne .RStoreAllyNameColor
.RDontHighlightAllyName:
	lda #TEXT_COLOR
.RStoreAllyNameColor:
	sta COLUP0
	sta COLUP1

	jsr RSetTextPointers
	jsr RDrawText
	jmp RDrawBattleMenuLoop
RShowEnemyName:
	lda menuLines,x
	sec
	sbc #4
	tay
	lda enemyID,y
	tay
	lda REnemyNameLookup,y
	sta tempPointer1
	lda #(RZombieText >> 8 & $FF)
	sta tempPointer1+1
	cpx highlightedLineAndSteps
	beq RUseEnemyColor
	lda #TEXT_COLOR
	bne RColorLoaded
RUseEnemyColor:
	lda REnemyColorLookup,y
RColorLoaded:
	jsr RDrawBattleMenuLine
	jmp RDrawBattleMenuLoop

RShowSpecialOptions:
	lda #TEXT_HIGHLIGHTED_COLOR
	cpx highlightedLineAndSteps
	beq RUseSpecialColor
	lda #TEXT_COLOR
RUseSpecialColor:
	sta COLUP0
	sta COLUP1
	lda menuLines,x
	and #$1F
	clc
	adc #56 ;This constant needs to be updated whenever there is message indexing tomfoolery.
	tax
	jsr RLoadString
	jsr RSetTextPointers
	jsr RDrawText
	jmp RDrawBattleMenuLoop

RDrawBattleText:
	lda #0
	sta fireMazeHeightAndMessageLine

RDrawBattleTextLoop:
	;sta WSYNC
	jsr RSetBattleMessage
RAfterSettingBattleMessage:
	jsr RSetTextPointers
	jsr RDrawText
	inc fireMazeHeightAndMessageLine
	lda fireMazeHeightAndMessageLine
	cmp #3
	bcc RDrawBattleTextLoop

RPostDrawingBattleText:
	ldx #2
RWaitToDrawPlayerText:
	sta WSYNC
	dex
	bne RWaitToDrawPlayerText

RDrawPartyInfoBattle:
	lda #0
	sta charIndex
	sta WSYNC
	jsr RDrawCharacterInfo
	inc charIndex
	sta WSYNC
	jsr RDrawCharacterInfo
	inc charIndex
	sta WSYNC
	jsr RDrawCharacterInfo
	inc charIndex
	sta WSYNC
	jsr RDrawCharacterInfo
	lda #0
	sta NUSIZ0
	sta NUSIZ1
	sta VDELP0
	sta VDELP1
	sta charIndex

	ldx #2
RFinishBattlePicture:
	sta WSYNC
	dex
	bne RFinishBattlePicture

RAfterRendering:
	lda #$82
	sta VBLANK ;Enable blanking
	jmp RGoToOverscan

RRenderSetupScreen:
	ldx #4
RWaitToDrawSetupScreenLoop:
	sta WSYNC
	dex
	bne RWaitToDrawSetupScreenLoop

RDrawLogo:
	lda #$F0
	sta HMP0
	lda #$0
	sta HMP1
	
	ldy #15
	jsr RSpinWheels
	nop
	nop
	nop
	nop
	cmp temp1
	sta RESP0
	sta RESP1
	
	lda #1
	sta NUSIZ0
	sta NUSIZ1
	sta WSYNC
	sta HMOVE
	lda #$66
	sta COLUPF
RDrawLogoLoop:
	sta WSYNC
	lda RLogo0,y
	sta GRP0
	lda RLogo1,y
	sta GRP1
	lda RLogoColors,y
	sta COLUP0
	sta COLUP1

	lda RLogoPF,y
	sta PF2

	lda RLogo2,y
	tax
	lda RLogo3,y
	cmp temp1
	stx GRP0
	sta GRP1
	sta WSYNC
	lda RLogo0,y
	sta GRP0
	lda RLogo1,y
	sta GRP1
	lda RLogoColors,y
	sta COLUP0
	sta COLUP1

	lda RLogoPF,y
	sta PF2

	lda RLogo2,y
	tax
	lda RLogo3,y
	cmp temp1
	stx GRP0
	sta GRP1
	dey
	bpl RDrawLogoLoop
	iny
	sty GRP0
	sty GRP1
	sta HMCLR

	sta WSYNC
	nop
	nop
	lda enemyID+1
	and #$F0
	sta HMBL
	lda enemyID+1
	and #$0F
	tay
.RWaitToPlaceBallLoop:
	dey
	bne .RWaitToPlaceBallLoop
	sta RESBL

	lda #SETUP_CURSOR_COLOR
	sta COLUPF

	sta WSYNC
	ldx #$03 ;Triplicate
	stx NUSIZ0 ;Set both duplication registers to triplicate the sprites.
	stx NUSIZ1
	stx VDELP0
	stx VDELP1

	ldx #$E0 ;Moves one color clock to the right.

	;Need some sort of delay here in order to more or less center this data.
	lda #0
	sta GRP0
	jsr RSpinWheels
	cmp temp1
	sta RESP0
	sta RESP1
	
	sta GRP1
	stx HMP0
	lda #$F0
	sta HMP1

	lda #TEXT_HIGHLIGHTED_COLOR
	sta COLUP0
	sta COLUP1

	sta WSYNC
	sta HMOVE

	ldx #$3E ;FORM A
	jsr RLoadString
	jsr RSetTextPointers
	jsr RDrawText
	ldx #$40 ;TEAM
	jsr RLoadString
	jsr RSetTextPointers
	jsr RDrawText
	

	ldx #0
RDrawSetupScreenLoop:
	stx charIndex

	sta WSYNC
	sta WSYNC

	jsr RDrawMinimalCharacterInfo
	ldx charIndex
	inx
	cpx #4
	bcc RDrawSetupScreenLoop

	lda #TEXT_HIGHLIGHTED_COLOR
	sta COLUP0
	sta COLUP1

	sta WSYNC
	sta WSYNC

	ldx #$41 ;PLAY
	jsr RLoadString
	jsr RSetTextPointers
	jsr RDrawText

	sta WSYNC

	lda enemyID
	cmp #4
	bne RDontUnderlineReady
	lda #$FF
	sta GRP0
	sta GRP1
	sta GRP0
	sta GRP1
RDontUnderlineReady:
	sta WSYNC
	sta WSYNC

	lda #0
	sta NUSIZ0
	sta NUSIZ1
	sta VDELP0
	sta VDELP1
	sta GRP0
	sta GRP1

	jmp RAfterRendering





RDrawBattleMenuLine: SUBROUTINE ;Draws one line of battle menu text, using A as the color of the text, and tempPointer1 as the location to pull text data from.
	sta COLUP0
	sta COLUP1

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

	jsr RSetTextPointers
	jsr RDrawText
RSpinWheels: SUBROUTINE
	rts

RMessageConstructors:
RXStabsY:
	.byte $0
	.byte $4
	.byte $1
RXShootsY:
	.byte $0
	.byte $5
	.byte $1
RXSlamsY:
	.byte $0
	.byte $6
	.byte $1
RXBitesY:
	.byte $0
	.byte $7
	.byte $1
RXSlicesY:
	.byte $0
	.byte $8
	.byte $1
RXCastsY:
	.byte $0
	.byte $9
	.byte $2
RXHealsYHP:
	.byte $1
	.byte $A
	.byte $3
RXLosesYHP:
	.byte $1
	.byte $B
	.byte $3
RXMisses:
	.byte $0
	.byte $C
	.byte $FF
RXDown:
	.byte $1
	.byte $12
	.byte $FF
RPartyLevelsUp:
	.byte $1A
	.byte $D
	.byte $E
RXLearnsY:
	.byte $0
	.byte $F
	.byte $2
RXMovesUp:
	.byte $0
	.byte $10
	.byte $E
RXBacksAway:
	.byte $0
	.byte $11
	.byte $13
RXWastesAway:
	.byte $1
	.byte $14
	.byte $13
RXWasCured:
	.byte $1
	.byte $15
	.byte $16
RXHasAShield:
	.byte $1
	.byte $18
	.byte $19
RPartyDown:
	.byte $1A
	.byte $12
	.byte $FF
RPartyFlees:
	.byte $1A
	.byte $1B
	.byte $FF
RPartyWins:
	.byte $1A
	.byte $1C
	.byte $FF
RXTriesToRun:
	.byte $0
	.byte $1D
	.byte $1E
RNoEffect:
	.byte $39
	.byte $20
	.byte $FF
RXCannotEscape:
	.byte $0
	.byte $21
	.byte $22
RXExiled:
	.byte $1
	.byte $2A
	.byte $FF
RXWakesUp:
	.byte $0
	.byte $17
	.byte $E
RXGuardsY:
	.byte $0
	.byte $23
	.byte $1
RXAttackUp:
	.byte $1
	.byte $24
	.byte $E
RXFellAsleep:
	.byte $1
	.byte $25
	.byte $26
RXIsAsleep:
	.byte $0
	.byte $27
	.byte $26
RXGuards:
	.byte $0
	.byte $23
	.byte $FF
RXShieldFades:
	.byte $1
	.byte $19
	.byte $29
RGameOver:
	.byte $2B
	.byte $2D
	.byte $FF
RGameClear:
	.byte $2B
	.byte $2C
	.byte $FF
RXShotAVolley:
	.byte $0
	.byte $43
	.byte $02
RXStabsYParry:
	.byte $1
	.byte $4
	.byte $0
RXBlocks:
	.byte $1
	.byte $32
	.byte $FF
RPartyHPUp:
	.byte $1A
	.byte $33
	.byte $FF
RPartyMPUp:
	.byte $1A
	.byte $34
	.byte $FF
RXHealsFully:
	.byte $1
	.byte $A
	.byte $35
RPartyHealsFully:
	.byte $1A
	.byte $A
	.byte $35
RPartyStatusClear:
	.byte $1A
	.byte $37
	.byte $2C
RPartyMixedUp:
	.byte $1A
	.byte $36
	.byte $0E
RXMPUp:
	.byte $1
	.byte $34
	.byte $FF
RXSmitesY:
	.byte $0
	.byte $42
	.byte $1
RIntoTheCastle:
	.byte $2F
	.byte $2E
	.byte $28
RIntoTheCrypt:
	.byte $2F
	.byte $2E
	.byte $31
RIntoTheAbyss:
	.byte $2F
	.byte $2E
	.byte $30
RXClawsY:
	.byte $0
	.byte $1F
	.byte $1
RXWhipsY:
	.byte $0
	.byte $3F
	.byte $1
RXMiresY:
	.byte $0
	.byte $44
	.byte $1
RSlimeSplitsApart:
	.byte $47
	.byte $45
	.byte $46
ROozeSplitsApart:
	.byte $48
	.byte $45
	.byte $46
RXRaisesY:
	.byte $0
	.byte $49
	.byte $1
RXLeavesY:
	.byte $0
	.byte $4A
	.byte $1
RXBlowsUp:
	.byte $0
	.byte $4B
	.byte $E
RXCannotSummon:
	.byte $0
	.byte $21
	.byte $4C

RSetBattleMessage: SUBROUTINE ;Uses the currentMessage to set the temp1-temp6 values correctly. Interprets A as the line of the message to set.
	ldy #TEXT_HIGHLIGHTED_COLOR
	sty COLUP0
	sty COLUP1
	clc
	adc currentMessage
	adc currentMessage
	adc currentMessage ;Calculate the correct offset
	tax
	lda RMessageConstructors,x ;Find what needs to be shown on this exact line
	bmi .RGoToAllEmpty
	cmp #4
	bcs .RGeneralMessageStructure
	sta WSYNC
	tax
	beq .RSourceBattlerName
	dex
	beq .RTargetBattlerName
	dex
	beq .RSpellName
	jmp .RHPCount

.RGoToAllEmpty:
	jmp .RAllEmpty

.RGeneralMessageStructure:
	tax
	jsr RLoadString
	rts
.RSourceBattlerName:
	ldx currentBattler
	cpx #4
	bcs .REnemySourceBattlerName
.RAllySourceBattlerName:
.RAllyIsTargeted:
	sta WSYNC
	lda char1,x
	and #$0F
	tay
	lda RClassColors,y
	sta COLUP0
	sta COLUP1
	lda name1,x
	sta temp1
	lda name2,x
	sta temp2
	lda name3,x
	sta temp3
	lda name4,x
	sta temp4
	lda name5,x
	sta temp5
	lda #EMPTY
	sta temp6
	rts
.REnemySourceBattlerName:
	dex
	dex
	dex
	dex
	lda enemyID,x
	and #$3F ;Get just the enemyID
	tax
	lda REnemyColorLookup,x
	sta COLUP0
	sta COLUP1
	lda REnemyNameLookup,x
	sta tempPointer1
	lda #(RZombieText >> 8 & $FF)
	sta tempPointer1+1
	bne .RSetTempVariables ;Should always be true, just saves a byte over jmp
.RTargetBattlerName:
	ldx startingCursorIndexAndTargetID
	cpx	#4
	bcc .RAllyIsTargeted
.REnemyIsTargeted:
	dex
	dex
	dex
	dex
	lda enemyID,x
	and #$3F ;Get just the enemyID
	tax
	lda REnemyColorLookup,x
	sta COLUP0
	sta COLUP1
	lda REnemyNameLookup,x
	sta tempPointer1
	lda #(RZombieText >> 8 & $FF)
	sta tempPointer1+1
	bne .RSetTempVariables ;Should always be true, just saves a byte over jmp
.RSpellName:
	ldx cursorIndexAndMessageY
	lda RSpellTextLookupTable,x
	sta tempPointer1
	lda RSpellColors,x
	sta COLUP0
	sta COLUP1
	lda #(RFireText >> 8 & $FF)
	sta tempPointer1+1
	bne .RSetTempVariables ;Should always be true, just saves a byte over jmp
.RHPCount:
	sta WSYNC
	lda cursorIndexAndMessageY
	jsr RCalculateDigitIndices
	sty temp1
	stx temp2
	lda #EMPTY
	sta temp3
	sta temp6
	lda #H
	sta temp4
	lda #P
	sta temp5
	rts
.RSetTempVariables:
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
	rts
.RAllEmpty:
	sta WSYNC
	sta WSYNC

	lda #EMPTY
	ldx #7
.RSetEmptyLoop:
	sta temp1,x
	dex
	bpl .RSetEmptyLoop
	rts

RDrawCharacterInfo: SUBROUTINE ;Draws one party members mood and name, hp and mp, or class name depending on viewedPartyInfo variable
	ldx charIndex ;Determine which character's data is about to be drawn
	lda char1,x
	and #$0F ;Get the class of this character
	tay

	lda #TEXT_HIGHLIGHTED_COLOR
	sta COLUP0
	sta COLUP1
	cpx currentBattler
	bne .RNoHighlighting
	lda currentEffect ;Should be highlighting, and this is the currently hovered character
	cmp #$1
	bne .RNoHighlighting
	lda effectCountdown
	and #$10
	beq .RPostClassColorSetting
.RNoHighlighting:
	lda RClassColors,y ;Get the color that corresponds with this class, and set both players to use that color
	sta COLUP0
	sta COLUP1
.RPostClassColorSetting:
	sta WSYNC

	lda viewedPartyInfo
	beq .RSetupMood
	cmp #1
	beq .RShowingHPAndMP
	jmp .RShowingClassAndLevel

.RSetupMood:
	;Need to set the mood picture and name

	lda name1,x
	sta temp2
	lda name2,x
	sta temp3
	lda name3,x
	sta temp4
	lda name4,x
	sta temp5
	lda name5,x
	sta temp6
	jsr RSetTextPointers

	ldx charIndex
	lda char1,x
	lsr
	lsr
	lsr
	lsr
	and #$0F
	tay
.RSelectedMood:
	lda RMoodLookupTable,y
	sta tempPointer1
	lda #(RAvatarHappy >> 8 & $FF)
	sta tempPointer1+1
	sta WSYNC

.RDrawTheText:	
	ldy #CHARACTER_HEIGHT-1
	cmp temp1 ;Just used to delay 3 cycles
.RDrawAvatarAndName:
	sty temp1 ;Stores how many loops are left
	lda (temp6),y
	sta temp2 ;Stores another digit that doesn't have a register to stay in
	sta WSYNC
	lda (tempPointer1),y
	sta GRP0
	lda (tempPointer2),y
	sta GRP1
	lda (tempPointer3),y
	sta GRP0
	lda (temp5),y
	tax
	lda (temp4),y
	ldy temp2
	nop
	sta GRP1
	stx GRP0
	sty GRP1
	sta GRP0
	
	ldy temp1
	dey
	bpl .RDrawAvatarAndName

	iny
	sty GRP0 ;Clear player graphics while HP, MP data is being prepared
	sty GRP1
	sty GRP0
	sty GRP1
	rts

.RShowingHPAndMP:
	ldx charIndex
	lda hp1,x
	ldy #H
	sty temp1 ;HP indicator

	jsr RCalculateDigitIndices
	stx temp3 ;The low digit of the HP value
	sty temp2 ;The high digit of the HP value

	ldx charIndex
	lda mp1,x
	ldy #M
	sty temp4 ;MP indicator

	jsr RCalculateDigitIndices
	stx temp6 ;The low digit of the MP value
	sty temp5 ;The high digit of the MP value

	jsr RSetTextPointers
	jmp .RDrawTheText

.RShowingClassAndLevel:
	ldx charIndex
	lda char1,x ;Format is -MMM-CCC
	and #$0F
	tax
	lda RClassNameLookupTable,x
	sta tempPointer1
	lda #(RKnightText >> 8 & $FF)
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

	jsr RSetTextPointers

	nop
	nop
	nop
	nop
	jmp .RDrawTheText

RDrawMinimalCharacterInfo: SUBROUTINE
	ldx charIndex
	lda char1,x
	and #$0F
	tay
	lda RClassColors,y
	sta COLUP0
	sta COLUP1

	lda name1,x
	sta temp2
	lda name2,x
	sta temp3
	lda name3,x
	sta temp4
	lda name4,x
	sta temp5
	lda name5,x
	sta temp6

	jsr RSetTextPointers

	lda #(RAvatarHappy & $FF)
	sta tempPointer1
	lda #(RAvatarHappy >> 8 & $FF)
	sta tempPointer1+1
	jsr RDrawText

	sta WSYNC

	ldx charIndex
	cpx enemyID
	bne .RDontEnableBall
	lda #2
	sta ENABL
.RDontEnableBall

	lda char1,x
	and #$0F
	tay
	lda RClassNameLookupTable,y
	sta tempPointer1
	lda #(RKnightText >> 8 & $FF)
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

	jsr RSetTextPointers

	lda #0
	sta ENABL

	jsr RDrawText

	rts


RDrawText: SUBROUTINE ;Will update graphics registers accordingly as long as the sprites are positioned correctly and the pointers set.
	sta WSYNC
	ldy #CHARACTER_HEIGHT-1
.RTextLoop
	sty temp1 ;Stores how many loops are left
	lda (temp6),y
	sta temp2 ;Stores another digit that doesn't have a register to stay in
	sta WSYNC
	lda (tempPointer1),y
	sta GRP0
	lda (tempPointer2),y
	sta GRP1
	lda (tempPointer3),y
	sta GRP0
	lda (temp5),y
	tax
	lda (temp4),y
	ldy temp2
	nop
	sta GRP1
	stx GRP0
	sty GRP1
	sta GRP0
	ldy temp1
	dey
	bpl .RTextLoop

	iny
	sty GRP0 ;Clear player graphics while HP, MP data is being prepared
	sty GRP1
	sty GRP0
	sty GRP1
	rts	

RSetTextPointers: SUBROUTINE ;Will treat the values in temp1-6 as character indices and stores the pointers to the graphics data in tempPointer6-tempPointer1 
	ldx temp1
	lda RCharacterLowLookupTable,x
	sta tempPointer1
	lda RCharacterHighLookupTable,x
	sta tempPointer1+1
	ldx temp2
	lda RCharacterLowLookupTable,x
	sta tempPointer2
	lda RCharacterHighLookupTable,x
	sta tempPointer2+1
	ldx temp3
	lda RCharacterLowLookupTable,x
	sta tempPointer3
	lda RCharacterHighLookupTable,x
	sta tempPointer3+1
	ldx temp4
	lda RCharacterLowLookupTable,x
	sta temp4
	lda RCharacterHighLookupTable,x
	sta tempPointer4
	ldx temp5
	lda RCharacterLowLookupTable,x
	sta temp5
	lda RCharacterHighLookupTable,x
	sta tempPointer5
	ldx temp6
	lda RCharacterLowLookupTable,x
	sta temp6
	lda RCharacterHighLookupTable,x
	sta tempPointer6
	rts

RIndexToEnemyPosition: SUBROUTINE ;Converts the position of a menu cursor into the correct location in the enemyID array of the target
	ldy #0
	inx
.RIndexConversionLoop
	lda enemyHP,y
	cmp #0
	beq .RNoHit
	dex
	beq .RDone
.RNoHit:
	iny
	jmp .RIndexConversionLoop
.RDone
	rts ;Y is the correct offset into the enemyID array

	ORG $C928 ;Used to hold enemy names, nothing else can go in this section
	RORG $F928

RWolfText:
	.byte #W
	.byte #O
	.byte #L
	.byte #F
	.byte #EMPTY
	.byte #EMPTY
RDruidText:
	.byte #D
	.byte #R
	.byte #U
	.byte #I
	.byte #D
	.byte #EMPTY
RShroomText:
	.byte #S
	.byte #H
	.byte #R
	.byte #O
	.byte #O
	.byte #M
RSquireText:
	.byte #S
	.byte #Q
	.byte #U
	.byte #I
	.byte #R
	.byte #E
RArcherText:
	.byte #A
	.byte #R
	.byte #C
	.byte #H
	.byte #E
	.byte #R
RPriestText:
	.byte #P
	.byte #R
	.byte #I
	.byte #E
	.byte #S
	.byte #T
RGiftText:
	.byte #G
	.byte #I
	.byte #F
	.byte #T
	.byte #EMPTY
	.byte #EMPTY
RSwordText:
	.byte #S
	.byte #W
	.byte #O
	.byte #R
	.byte #D
	.byte #EMPTY
RShieldEnemyText:
	.byte #S
	.byte #H
	.byte #I
	.byte #E
	.byte #L
	.byte #D
RZombieText:
	.byte #Z
	.byte #O
	.byte #M
	.byte #B
	.byte #I
	.byte #E
RSkltonText:
	.byte #S
	.byte #K
	.byte #L
	.byte #T
	.byte #O
	.byte #N
RMageText:
	.byte #M
	.byte #A
	.byte #G
	.byte #E
	.byte #EMPTY
	.byte #EMPTY
RGoopText:
	.byte #G
	.byte #O
	.byte #O
	.byte #P
	.byte #EMPTY
	.byte #EMPTY
RWarlokText:
	.byte #W
	.byte #A
	.byte #R
	.byte #L
	.byte #O
	.byte #K
RImpText:
	.byte #I
	.byte #M
	.byte #P
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
RWispText:
	.byte #W
	.byte #I
	.byte #S
	.byte #P
	.byte #EMPTY
	.byte #EMPTY
RRedOrbText:
	.byte #R
	.byte #E
	.byte #D
	.byte #O
	.byte #R
	.byte #B
RBluOrbText:
	.byte #B
	.byte #L
	.byte #U
	.byte #O
	.byte #R
	.byte #B
RGrnOrbText:
	.byte #G
	.byte #R
	.byte #N
	.byte #O
	.byte #R
	.byte #B
RGldOrbText:
	.byte #G
	.byte #L
	.byte #D
	.byte #O
	.byte #R
	.byte #B
RBearText:
	.byte #B
	.byte #E
	.byte #A
	.byte #R
	.byte #EMPTY
	.byte #EMPTY
RUnicrnText:
	.byte #U
	.byte #N
	.byte #I
	.byte #C
	.byte #R
	.byte #N
RVolcioText:
	.byte #V
	.byte #O
	.byte #L
	.byte #C
	.byte #I
	.byte #O
RGlaciaText:
	.byte #G
	.byte #L
	.byte #A
	.byte #C
	.byte #I
	.byte #A
RGrgoylText:
	.byte #G
	.byte #R
	.byte #G
	.byte #O
	.byte #Y
	.byte #L
RMimicText:
	.byte #M
	.byte #I
	.byte #M
	.byte #I
	.byte #C
	.byte #EMPTY
RJesterText:
	.byte #J
	.byte #E
	.byte #S
	.byte #T
	.byte #E
	.byte #R
RArmorText:
	.byte #A
	.byte #R
	.byte #M
	.byte #O
	.byte #R
	.byte #EMPTY
RSpiderText:
	.byte #S
	.byte #P
	.byte #I
	.byte #D
	.byte #E
	.byte #R
RSlimeText:
	.byte #S
	.byte #L
	.byte #I
	.byte #M
	.byte #E
	.byte #EMPTY
RLichText:
	.byte #L
	.byte #I
	.byte #C
	.byte #H
	.byte #EMPTY
	.byte #EMPTY
RShfflrText:
	.byte #S
	.byte #H
	.byte #F
	.byte #F
	.byte #L
	.byte #R
RShmblrText:
	.byte #S
	.byte #H
	.byte #M
	.byte #B
	.byte #L
	.byte #R
RThicktText:
	.byte #T
	.byte #H
	.byte #I
	.byte #C
	.byte #K
	.byte #T
ROozeText:
	.byte #O
	.byte #O
	.byte #Z
	.byte #E
	.byte #EMPTY
	.byte #EMPTY
RHorrorText:
	.byte #H
	.byte #O
	.byte #R
	.byte #R
	.byte #O
	.byte #R

	ORG $CA00 ;Used to hold text.
	RORG $FA00

RBackText:
	.byte #B
	.byte #A
	.byte #C
	.byte #K
	.byte #EMPTY
	.byte #EMPTY
RFireText:
	.byte #F
	.byte #I
	.byte #R
	.byte #E
	.byte #EMPTY
	.byte #EMPTY
RSleepText:
	.byte #S
	.byte #L
	.byte #E
	.byte #E
	.byte #P
	.byte #EMPTY
RBlizrdText:
	.byte #B
	.byte #L
	.byte #I
	.byte #Z
	.byte #R
	.byte #D
RDrainText:
	.byte #D
	.byte #R
	.byte #A
	.byte #I
	.byte #N
	.byte #EMPTY
RThundrText:
	.byte #T
	.byte #H
	.byte #U
	.byte #N
	.byte #D
	.byte #R
RShieldText:
	.byte #S
	.byte #H
	.byte #I
	.byte #E
	.byte #L
	.byte #D
RMeteorText:
	.byte #M
	.byte #E
	.byte #T
	.byte #E
	.byte #O
	.byte #R
RChaosText:
	.byte #C
	.byte #H
	.byte #A
	.byte #O
	.byte #S
	.byte #EMPTY
RHealText:
	.byte #H
	.byte #E
	.byte #A
	.byte #L
	.byte #EMPTY
	.byte #EMPTY
RSmiteText:
	.byte #S
	.byte #M
	.byte #I
	.byte #T
	.byte #E
	.byte #EMPTY
RVolleyText:
	.byte #V
	.byte #O
	.byte #L
	.byte #L
	.byte #E
	.byte #Y
RSharpText:
	.byte #S
	.byte #H
	.byte #A
	.byte #R
	.byte #P
	.byte #EMPTY
RBlightText:
	.byte #B
	.byte #L
	.byte #I
	.byte #G
	.byte #H
	.byte #T
RTriageText:
	.byte #T
	.byte #R
	.byte #I
	.byte #A
	.byte #G
	.byte #E
RWitherText:
	.byte #W
	.byte #I
	.byte #T
	.byte #H
	.byte #E
	.byte #R
RBanishText:
	.byte #B
	.byte #A
	.byte #N
	.byte #I
	.byte #S
	.byte #H
RTranceText:
	.byte #T
	.byte #R
	.byte #A
	.byte #N
	.byte #C
	.byte #E
RWishText:
	.byte #W
	.byte #I
	.byte #S
	.byte #H
	.byte #EMPTY
	.byte #EMPTY
RShiftText:
	.byte #S
	.byte #H
	.byte #I
	.byte #F
	.byte #T
	.byte #EMPTY

RClassColors:
	.byte $8A ;Knight
	.byte $46 ;Rogue
	.byte $1A ;Cleric
	.byte $66 ;Wizard
	.byte $C8 ;Ranger
	.byte $24 ;Paladin

RCasterType: ;0 is no casting, 1 is full caster, FF is half-caster
	.byte $0 ;Knight
	.byte $0 ;Rogue
	.byte $1 ;Cleric
	.byte $1 ;Wizard
	.byte $FF ;Ranger
	.byte $FF ;Paladin

RSpellColors:
	.byte TEXT_HIGHLIGHTED_COLOR ;BACK
	.byte $FA ;FIRE
	.byte $B6 ;SLEEP
	.byte $9E ;BLIZRD
	.byte $36 ;DRAIN
	.byte $1E ;THUNDR
	.byte $78 ;SHIELD
	.byte $FC ;METEOR
	.byte $5A ;CHAOS
	.byte $C8 ;HEAL
	.byte $1C ;SMITE
	.byte $0E ;VOLLEY
	.byte $A8 ;SHARP
	.byte $CE ;BLIGHT
	.byte $9A ;TRIAGE
	.byte $BC ;WITHER
	.byte $48 ;BANISH
	.byte $56 ;TRANCE
	.byte $2E ;WISH
	.byte $4C ;SHIFT

RClassNameLookupTable:
	.byte (RKnightText & $FF)
	.byte (RRogueText & $FF)
	.byte (RClericText & $FF)
	.byte (RWizardText & $FF)
	.byte (RRangerText & $FF)
	.byte (RPaladinText & $FF)

RBattleOptionsLookupTable:
	.byte (RFightText & $FF)
	.byte (RCastText & $FF)
	.byte (RMoveText & $FF)
	.byte (RRunText & $FF)
	.byte (RGuardText & $FF)
	.byte (RParryText & $FF)

;Make sure to add back room for the Trophy and Campfire ids if trying to remove this table!!
REnemyNameLookup: ;This table could be turned into a calculation RZombieText + (6 * enemyID)
	.byte (RWolfText & $FF)
	.byte (RDruidText & $FF)
	.byte (RShroomText & $FF)
	.byte (RSquireText & $FF)
	.byte (RArcherText & $FF)
	.byte (RPriestText & $FF)
	.byte (RGiftText & $FF)
	.byte (RSwordText & $FF)
	.byte (RShieldEnemyText & $FF)
	.byte (RZombieText & $FF)
	.byte (RSkltonText & $FF)
	.byte (RMageText & $FF)
	.byte (RGoopText & $FF)
	.byte (RWarlokText & $FF)
	.byte (RImpText & $FF)
	.byte (RWispText & $FF)
	.byte (RRedOrbText & $FF)
	.byte (RBluOrbText & $FF)
	.byte (RGrnOrbText & $FF)
	.byte (RGldOrbText & $FF)
	.byte (RBearText & $FF)
	.byte (RUnicrnText & $FF)
	.byte (RVolcioText & $FF)
	.byte (RGlaciaText & $FF)
	.byte (RGrgoylText & $FF)
	.byte (RMimicText & $FF)
	.byte (RJesterText & $FF)
	.byte (RArmorText & $FF)
	.byte (RSpiderText & $FF)
	.byte (RSlimeText & $FF)
	.byte (RLichText & $FF)
	.byte (RShfflrText & $FF)
	.byte (RShmblrText & $FF)
	.byte 0 ;Trophy
	.byte (RThicktText & $FF)
	.byte (RHorrorText & $FF)
	.byte (ROozeText & $FF)
	.byte 0 ;Campfire

REnemyColorLookup:
	.byte $0A ;Wolf
	.byte $B6 ;Druid
	.byte $46 ;Shroom
	.byte $44 ;Squire
	.byte $D6 ;Archer
	.byte $FC ;Priest
	.byte $96 ;Gift
	.byte $0C ;Sword
	.byte $0C ;Shield
	.byte $D6 ;Zombie
	.byte $0E ;Sklton
	.byte $9A ;Mage
	.byte $C4 ;Goop
	.byte $36 ;Warlok
	.byte $56 ;Imp
	.byte $8C ;Wisp
	.byte $36 ;RedOrb
	.byte $88 ;BluOrb
	.byte $B8 ;GrnOrb
	.byte $EC ;GldOrb

	.byte $16 ;Bear
	.byte $5C ;Unicrn
	.byte $38 ;Volcio
	.byte $88 ;Glacia
	.byte $08 ;Grgoyl
	.byte $16 ;Mimic
	.byte $6A ;Jester
	.byte $0C ;Armor
	.byte $04 ;Spider
	.byte $C4 ;Slime
	.byte $A6 ;Lich
	.byte $54 ;Shfflr
	.byte $56 ;Shmblr
	.byte $1C ;Trophy

	.byte $B6 ;Thickt
	.byte $46 ;Horror
	.byte $C4 ;Ooze
	.byte $2A ;Campfire

	;Only around 8 more bytes can fit here...

	ORG $CB00 ;Used to hold miscellaneous data/lookup tables and text
	RORG $FB00

RSpellTextLookupTable:
	.byte (RBackText & $FF)
	.byte (RFireText & $FF)
	.byte (RSleepText & $FF)
	.byte (RBlizrdText & $FF)
	.byte (RDrainText & $FF)
	.byte (RThundrText & $FF)
	.byte (RShieldText & $FF)
	.byte (RMeteorText & $FF)
	.byte (RChaosText & $FF)
	.byte (RHealText & $FF)
	.byte (RSmiteText & $FF)
	.byte (RVolleyText & $FF)
	.byte (RSharpText & $FF)
	.byte (RBlightText & $FF)
	.byte (RTriageText & $FF)
	.byte (RWitherText & $FF)
	.byte (RBanishText & $FF)
	.byte (RTranceText & $FF)
	.byte (RWishText & $FF)
	.byte (RShiftText & $FF)

RKnightText:
	.byte #K
	.byte #N
	.byte #I
	.byte #G
	.byte #H
	.byte #T
RRogueText:
	.byte #R
	.byte #O
	.byte #G
	.byte #U
	.byte #E
	.byte #EMPTY
RClericText:
	.byte #C
	.byte #L
	.byte #E
	.byte #R
	.byte #I
	.byte #C
RWizardText:
	.byte #W
	.byte #I
	.byte #Z
	.byte #A
	.byte #R
	.byte #D
RRangerText:
	.byte #R
	.byte #A
	.byte #N
	.byte #G
	.byte #E
	.byte #R
RPaladinText:
	.byte #P
	.byte #A
	.byte #L
	.byte #A
	.byte #D
	.byte #N

RFightText:
	.byte #F
	.byte #I
	.byte #G
	.byte #H
	.byte #T
	.byte #EMPTY
RCastText:
	.byte #C
	.byte #A
	.byte #S
	.byte #T
	.byte #EMPTY
	.byte #EMPTY
RMoveText:
	.byte #M
	.byte #O
	.byte #V
	.byte #E
	.byte #EMPTY
	.byte #EMPTY
RRunText:
	.byte #R
	.byte #U
	.byte #N
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
RGuardText:
	.byte #G
	.byte #U
	.byte #A
	.byte #R
	.byte #D
	.byte #EMPTY
RParryText:
	.byte #P
	.byte #A
	.byte #R
	.byte #R
	.byte #Y
	.byte #EMPTY

RCharacterLowLookupTable: ;Contains the low bytes of the pointers to all the character graphics.
	.byte (RNoCharacter & $FF)
	.byte (RLetterA & $FF)
	.byte (RLetterB & $FF)
	.byte (RLetterC & $FF)
	.byte (RLetterD & $FF)
	.byte (RLetterE & $FF)
	.byte (RLetterF & $FF)
	.byte (RLetterG & $FF)
	.byte (RLetterH & $FF)
	.byte (RLetterI & $FF)
	.byte (RLetterJ & $FF)
	.byte (RLetterK & $FF)
	.byte (RLetterL & $FF)
	.byte (RLetterM & $FF)
	.byte (RLetterN & $FF)
	.byte (RLetterO & $FF)
	.byte (RLetterP & $FF)
	.byte (RLetterQ & $FF)
	.byte (RLetterR & $FF)
	.byte (RLetterS & $FF)
	.byte (RLetterT & $FF)
	.byte (RLetterU & $FF)
	.byte (RLetterV & $FF)
	.byte (RLetterW & $FF)
	.byte (RLetterX & $FF)
	.byte (RLetterY & $FF)
	.byte (RLetterZ & $FF)

	.byte (RNumber0 & $FF)
	.byte (RNumber1 & $FF)
	.byte (RNumber2 & $FF)
	.byte (RNumber3 & $FF)
	.byte (RNumber4 & $FF)
	.byte (RNumber5 & $FF)
	.byte (RNumber6 & $FF)
	.byte (RNumber7 & $FF)
	.byte (RNumber8 & $FF)
	.byte (RNumber9 & $FF)

RCharacterHighLookupTable: ;Contains the high bytes of the pointers to all the character graphics.
	.byte (RNoCharacter >> 8 & $FF)
	.byte (RLetterA >> 8 & $FF)
	.byte (RLetterB >> 8 & $FF)
	.byte (RLetterC >> 8 & $FF)
	.byte (RLetterD >> 8 & $FF)
	.byte (RLetterE >> 8 & $FF)
	.byte (RLetterF >> 8 & $FF)
	.byte (RLetterG >> 8 & $FF)
	.byte (RLetterH >> 8 & $FF)
	.byte (RLetterI >> 8 & $FF)
	.byte (RLetterJ >> 8 & $FF)
	.byte (RLetterK >> 8 & $FF)
	.byte (RLetterL >> 8 & $FF)
	.byte (RLetterM >> 8 & $FF)
	.byte (RLetterN >> 8 & $FF)
	.byte (RLetterO >> 8 & $FF)
	.byte (RLetterP >> 8 & $FF)
	.byte (RLetterQ >> 8 & $FF)
	.byte (RLetterR >> 8 & $FF)
	.byte (RLetterS >> 8 & $FF)
	.byte (RLetterT >> 8 & $FF)
	.byte (RLetterU >> 8 & $FF)
	.byte (RLetterV >> 8 & $FF)
	.byte (RLetterW >> 8 & $FF)
	.byte (RLetterX >> 8 & $FF)
	.byte (RLetterY >> 8 & $FF)
	.byte (RLetterZ >> 8 & $FF)

	.byte (RNumber0 >> 8 & $FF)
	.byte (RNumber1 >> 8 & $FF)
	.byte (RNumber2 >> 8 & $FF)
	.byte (RNumber3 >> 8 & $FF)
	.byte (RNumber4 >> 8 & $FF)
	.byte (RNumber5 >> 8 & $FF)
	.byte (RNumber6 >> 8 & $FF)
	.byte (RNumber7 >> 8 & $FF)
	.byte (RNumber8 >> 8 & $FF)
	.byte (RNumber9 >> 8 & $FF)

RLogoColors:
	.byte $66
	.byte $66
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $66
	.byte $66
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $0E
	.byte $66
	.byte $66

RLogoPF:
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %01110000
	.byte %01000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %10110000
	.byte %11000000
	.byte %00110000
	.byte %10100000
	.byte %00000000
	.byte %00000000

RLogo0:
	.byte %11011101 ;maze
	.byte %10000001 ;maze
	.byte %00110101
	.byte %01000110
	.byte %01000101
	.byte %01000101
	.byte %00110110
	.byte %10000111 ;maze
	.byte %11011110 ;maze
	.byte %00000000
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00000000
	.byte %10100000 ;maze
	.byte %10111111 ;maze

RLogo1:
	.byte %11001110 ;maze
	.byte %00011000 ;maze
	.byte %01010010
	.byte %01010101
	.byte %01110101
	.byte %01010100
	.byte %00100100
	.byte %01101101 ;maze
	.byte %00001000 ;maze
	.byte %11001010
	.byte %00001010
	.byte %00001110
	.byte %00001010
	.byte %11000100
	.byte %01011000 ;maze
	.byte %01011011 ;maze

	;22 bytes in here...

	ORG $CC00 ;Used to hold more graphics data
	RORG $FC00

RAvatarDead:
	.byte %11111111
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %01111110
	.byte %01111110
	.byte %00011000
	.byte %00011000
RAvatarSad:
	.byte #%11111111
	.byte #%10000001
	.byte #%10100101
	.byte #%10011001
	.byte #%10000001
	.byte #%10100101
	.byte #%10000001
	.byte #%11111111
RAvatarNeutral:
	.byte #%11111111
	.byte #%10000001
	.byte #%10111101
	.byte #%10000001
	.byte #%10000001
	.byte #%10100101
	.byte #%10000001
	.byte #%11111111
RAvatarHappy:
	.byte #%11111111
	.byte #%10000001
	.byte #%10011001
	.byte #%10100101
	.byte #%10000001
	.byte #%10100101
	.byte #%10000001
	.byte #%11111111
RAvatarExcited:
	.byte #%11111111
	.byte #%10000001
	.byte #%10011001
	.byte #%10111101
	.byte #%10000001
	.byte #%10100101
	.byte #%10000001
	.byte #%11111111
RAvatarPain:	
	;.byte %11111111
	;.byte %11111111
	;.byte %11011011
	;.byte %11100111
	;.byte %10111101
	;.byte %11011011
	;.byte %11111111
	;.byte %11111111

	.byte %11010101
	.byte %10010100
	.byte %10011101
	.byte %11010101
	.byte %00000000
	.byte %11101110
	.byte %10101010
	.byte %11101010
RAvatarSleeping:
	.byte %11100000
	.byte %10000000
	.byte %01000000
	.byte %00101110
	.byte %11101000
	.byte %00000100
	.byte %00000010
	.byte %00001110
RAvatarSick:
	.byte #%11111111
	.byte #%10000001
	.byte #%10100101
	.byte #%10011001
	.byte #%11000011
	.byte #%10100101
	.byte #%10000001
	.byte #%11111111

RNumber0:
	.byte #%01111110
	.byte #%01100110
	.byte #%01100110
	.byte #%01100110
	.byte #%01100110
	.byte #%01100110
	.byte #%01100110
	.byte #%01111110
RNumber1:
	.byte #%00011000
	.byte #%00011000
	.byte #%00011000
	.byte #%00011000
	.byte #%00011000
	.byte #%00011000
	.byte #%00111000
	.byte #%00011000
RNumber2:
	.byte #%01111110
	.byte #%01100000
	.byte #%01100000
	.byte #%01100000
	.byte #%01111110
	.byte #%00000110
	.byte #%00000110
	.byte #%01111110
RNumber3:
	.byte #%01111110
	.byte #%00000110
	.byte #%00000110
	.byte #%00000110
	.byte #%00011110
	.byte #%00000110
	.byte #%00000110
	.byte #%01111110
RNumber4:
	.byte #%00000110
	.byte #%00000110
	.byte #%00000110
	.byte #%01111110
	.byte #%01100110
	.byte #%01100110
	.byte #%01100110
	.byte #%01100110
RNumber5:
	.byte #%01111110
	.byte #%00000110
	.byte #%00000110
	.byte #%00000110
	.byte #%01111110
	.byte #%01100000
	.byte #%01100000
	.byte #%01111110
RNumber6:
	.byte #%01111110
	.byte #%01100110
	.byte #%01100110
	.byte #%01100110
	.byte #%01111110
	.byte #%01100000
	.byte #%01100000
	.byte #%01111110
RNumber7:
	.byte #%00000110
	.byte #%00000110
	.byte #%00000110
	.byte #%00000110
	.byte #%00000110
	.byte #%00000110
	.byte #%00000110
	.byte #%01111110
RNumber8:
	.byte #%01111110
	.byte #%01100110
	.byte #%01100110
	.byte #%01100110
	.byte #%01111110
	.byte #%01100110
	.byte #%01100110
	.byte #%01111110
RNumber9:
	.byte #%00000110
	.byte #%00000110
	.byte #%00000110
	.byte #%01111110
	.byte #%01100110
	.byte #%01100110
	.byte #%01100110
	.byte #%01111110

RMoodLookupTable:
	.byte (RAvatarDead & $FF)
	.byte (RAvatarSad & $FF)
	.byte (RAvatarNeutral & $FF)
	.byte (RAvatarHappy & $FF)
	.byte (RAvatarExcited & $FF)
	.byte (RAvatarPain & $FF)
	.byte (RAvatarSleeping & $FF)
	.byte (RAvatarSick & $FF)

RArrowUp:
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %01111110
	.byte %00111100
	.byte %00011000
RArrowDiagonalUp:
	.byte %11000000
	.byte %11100000
	.byte %01110000
	.byte %00111001
	.byte %00011101
	.byte %00001111
	.byte %00000111
	.byte %00011111
RArrowRight:
	.byte %00000000
	.byte %00000100
	.byte %00000110
	.byte %11111111
	.byte %11111111
	.byte %00000110
	.byte %00000100
	.byte %00000000
RArrowDiagonalDown:
	.byte %00011111
	.byte %00000111
	.byte %00001111
	.byte %00011101
	.byte %00111001
	.byte %01110000
	.byte %11100000
	.byte %11000000
RArrowDown:
	.byte %00011000
	.byte %00111100
	.byte %01111110
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000

RNearFire:
	.byte %00111101 ;L13
	.byte %01101111 ;L10
	.byte %01100011 ;L7
	.byte %00000110 ;L4
	.byte %00000000 ;L1
	.byte %00011100 ;L14
	.byte %01101110 ;L11
	.byte %01111111 ;L8
	.byte %00100110 ;L5
	.byte %00000000 ;L2
	.byte %00001111 ;L15
	.byte %00110101 ;L12
	.byte %01110111 ;L9
	.byte %01000111 ;L6
	.byte %00000011 ;L3
	.byte $FF ;----
	.byte %10011100 ;R14
	.byte %11111110 ;R11
	.byte %00111110 ;R8
	.byte %01110000 ;R5
	.byte %10010000 ;R2
	.byte %11110000 ;R15
	.byte %11011110 ;R12
	.byte %01101110 ;R9
	.byte %01111010 ;R6
	.byte %00100000 ;R3
	.byte $FF ;----
	.byte %10110110 ;R13
	.byte %11001110 ;R10
	.byte %00111100 ;R7
	.byte %00100000 ;R4
	.byte $FF ;----

RLogo2:
	.byte %01110111 ;maze
	.byte %11000100 ;maze
	.byte %10011101
	.byte %01010001
	.byte %01010001
	.byte %01010001
	.byte %01010001
	.byte %10011101 ;maze
	.byte %11111000 ;maze
	.byte %00100010
	.byte %00100010
	.byte %00100011
	.byte %00100010
	.byte %01110001
	.byte %00010100 ;maze
	.byte %01110110 ;maze

RLogo3:
	.byte %10110111 ;maze
	.byte %00010001 ;maze
	.byte %11010100
	.byte %00011000
	.byte %10010100
	.byte %00010100
	.byte %11011000
	.byte %10001011 ;maze
	.byte %10111011 ;maze
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %00000000
	.byte %00000101 ;maze
	.byte %11101101 ;maze

	ORG $CD00 ;Used for holding the letters of the alphabet
	RORG $FD00

RNoCharacter:
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
RLetterA:
	.byte %01100010
	.byte %01100010
	.byte %01100010
	.byte %01100010
	.byte %01111110
	.byte %01100010
	.byte %01100010
	.byte %01111110
RLetterB:
	.byte %01111100
	.byte %01100010
	.byte %01100010
	.byte %01100010
	.byte %01111100
	.byte %01100010
	.byte %01100010
	.byte %01111100
RLetterC:
	.byte %00111100
	.byte %01110110
	.byte %01100000
	.byte %01100000
	.byte %01100000
	.byte %01100000
	.byte %01110110
	.byte %00111100
RLetterD:
	.byte %01111100
	.byte %01100110
	.byte %01100010
	.byte %01100010
	.byte %01100010
	.byte %01100010
	.byte %01100110
	.byte %01111100
RLetterE:
	.byte %01111110
	.byte %01100000
	.byte %01100000
	.byte %01100000
	.byte %01111000
	.byte %01100000
	.byte %01100000
	.byte %01111110
RLetterF:
	.byte %01100000
	.byte %01100000
	.byte %01100000
	.byte %01100000
	.byte %01111000
	.byte %01100000
	.byte %01100000
	.byte %01111110
RLetterG:
	.byte %00111100
	.byte %01110010
	.byte %01100010
	.byte %01100110
	.byte %01100000
	.byte %01100000
	.byte %01110010
	.byte %00111100
RLetterH:
	.byte %01100010
	.byte %01100010
	.byte %01100010
	.byte %01100010
	.byte %01111110
	.byte %01100010
	.byte %01100010
	.byte %01100010
RLetterI:
	.byte %01111110
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %01111110
RLetterJ:
	.byte %00110000
	.byte %01111000
	.byte %01011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %01111110
RLetterK:
	.byte %01100010
	.byte %01100100
	.byte %01101000
	.byte %01110000
	.byte %01110000
	.byte %01101000
	.byte %01100100
	.byte %01100010
RLetterL:
	.byte %01111110
	.byte %01100000
	.byte %01100000
	.byte %01100000
	.byte %01100000
	.byte %01100000
	.byte %01100000
	.byte %01100000
RLetterM:
	.byte %01100010
	.byte %01100010
	.byte %01100010
	.byte %01100010
	.byte %01100010
	.byte %01101010
	.byte %01110110
	.byte %01100010
RLetterN:
	.byte %01100010
	.byte %01100010
	.byte %01100010
	.byte %01100110
	.byte %01101010
	.byte %01110010
	.byte %01100010
	.byte %01100010
RLetterO:
	.byte %00111100
	.byte %01110110
	.byte %01100010
	.byte %01100010
	.byte %01100010
	.byte %01100010
	.byte %01110110
	.byte %00111100
RLetterP:
	.byte %01100000
	.byte %01100000
	.byte %01100000
	.byte %01111100
	.byte %01100110
	.byte %01100010
	.byte %01100110
	.byte %01111100
RLetterQ:
	.byte %00111010
	.byte %01110100
	.byte %01100010
	.byte %01100010
	.byte %01100010
	.byte %01100010
	.byte %01110110
	.byte %00111100
RLetterR:
	.byte %01100010
	.byte %01100100
	.byte %01101000
	.byte %01111100
	.byte %01100110
	.byte %01100010
	.byte %01100110
	.byte %01111100
RLetterS:
	.byte %01111110
	.byte %01100010
	.byte %00000010
	.byte %00000010
	.byte %01111110
	.byte %01100000
	.byte %01100110
	.byte %01111110
RLetterT:
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %01111110
RLetterU:
	.byte %00111100
	.byte %01110110
	.byte %01100010
	.byte %01100010
	.byte %01100010
	.byte %01100010
	.byte %01100010
	.byte %01100010
RLetterV:
	.byte %00011000
	.byte %00011000
	.byte %00110100
	.byte %00110100
	.byte %00110100
	.byte %01100010
	.byte %01100010
	.byte %01100010
RLetterW:
	.byte %01100010
	.byte %01110110
	.byte %01101010
	.byte %01100010
	.byte %01100010
	.byte %01100010
	.byte %01100010
	.byte %01100010
RLetterX:
	.byte %01000010
	.byte %01100110
	.byte %00111100
	.byte %00011000
	.byte %00011000
	.byte %00111100
	.byte %01100110
	.byte %01000010
RLetterY:
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00111100
	.byte %00111100
	.byte %01100110
	.byte %01100110
RLetterZ:
	.byte %01111110
	.byte %01100110
	.byte %00110000
	.byte %00011000
	.byte %00001100
	.byte %00000110
	.byte %01100010
	.byte %01111110

RCalculateDigitIndices: SUBROUTINE ;Will interpret whatever is in A when called as a decimal value, then return the character lookup indices in X and Y.
	tay
	and #$0F
	clc
	adc #27
	tax ;Character index corresponding to low digit of the specified decimal value is now in the x register.
	tya
	lsr
	lsr
	lsr
	lsr
	and #$0F
	clc
	adc #27
	tay ;Character index corresponding to high digit of the specified decimal value is now in the y register.
	rts

	;21 more bytes can fit here

	ORG $CE00 ;Used to hold maze rendering data
	RORG $FE00

RNearDoor: ;Used for PF1
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%10000000
	.byte #%10000000
	.byte #%10000000
	.byte #%10000000
	.byte #%10000000
	.byte #%10000000
	.byte #%10000010
	.byte #%10000011
	.byte #%10000011
	.byte #%10000011
	.byte #%10000011
	.byte #%10000011
	.byte #%10000011
	.byte #%10000011
	.byte #%10000011
	.byte #%10000011
	.byte #%10000011
	.byte #%10000011
	.byte #%10000011
	.byte #%10000011
	.byte #%10000011
	.byte #%10000011
	.byte #%10000011
	.byte #%10000011
	.byte #%10000010
	.byte #%10000000
	.byte #%10000000
	.byte #%10000000
	.byte #%10000000
	.byte #%10000000
	.byte #%10000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000

RNoNearDoor: ;Used for PF1
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%10000000
	.byte #%11000000
	.byte #%11100000
	.byte #%11110000
	.byte #%11111000
	.byte #%11111100
	.byte #%11111110
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111110
	.byte #%11111100
	.byte #%11111000
	.byte #%11110000
	.byte #%11100000
	.byte #%11000000
	.byte #%10000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000

RFarDoorOnlyTwo: ;Used for PF2
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000001
	.byte #%00000011
	.byte #%00000011
	.byte #%00000011
	.byte #%00010011
	.byte #%11110011
	.byte #%11110011
	.byte #%11110011
	.byte #%11110011
	.byte #%11110011
	.byte #%00010011
	.byte #%00000011
	.byte #%00000011
	.byte #%00000011
	.byte #%00000001
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000

RNoFarDoorOnlyTwo: ;Used for PF2
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000001
	.byte #%00000011
	.byte #%00000111
	.byte #%00001111
	.byte #%00011111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%00011111
	.byte #%00001111
	.byte #%00000111
	.byte #%00000011
	.byte #%00000001
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000

RFarDoor: ;Used for PF2
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000001
	.byte #%00000011
	.byte #%00000011
	.byte #%00000011
	.byte #%00010011
	.byte #%00110011
	.byte #%01110011
	.byte #%01110011
	.byte #%01110011
	.byte #%00110011
	.byte #%00010011
	.byte #%00000011
	.byte #%00000011
	.byte #%00000011
	.byte #%00000001
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000

RNoFarDoor: ;Used for PF2
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000001
	.byte #%00000011
	.byte #%00000111
	.byte #%00001111
	.byte #%00011111
	.byte #%00111111
	.byte #%01111111
	.byte #%01111111
	.byte #%01111111
	.byte #%00111111
	.byte #%00011111
	.byte #%00001111
	.byte #%00000111
	.byte #%00000011
	.byte #%00000001
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000

	;22 more bytes here

	ORG $CF00 ;Used to hold additional maze rendering data
	RORG $FF00

ROutermost: ;Used for PF0
	.byte #%00010000
	.byte #%00110000
	.byte #%01110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%11110000
	.byte #%01110000
	.byte #%00110000
	.byte #%00010000

RDeadEnd1: ;Used for PF1
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000

RDeadEnd2: ;Used for PF2
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000

ROnly1Room: ;Used for PF2
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%11111111
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000
	.byte #%00000000

RFarFire:
	.byte %11101111 ;7th
	.byte %10011011 ;4th
	.byte %00001000 ;1st
	.byte %01111111 ;8th
	.byte %11011011 ;5th
	.byte %00010001 ;2nd
	.byte %00111110 ;9th
	.byte %11001101 ;6th
	.byte %01011010 ;3rd

	ORG $CFB0
	RORG $FFB0

RLoadString:
	sta $1FF8 ;Go to bank 2
	nop
	nop
	nop
	nop
	nop
	nop
	rts

	ORG $CFC0
	RORG $FFC0

RCatchFromEnemyDrawing:
	nop
	nop
	nop
	jmp RPlaceBattleText
RGoToEnemyDrawingBank:
	sta $1FF8 ;Go to bank 2
	nop
	nop
	nop

	ORG $CFE0
	RORG $FFE0

RCatchFromVBlank:
	nop
	nop
	nop
	jmp RMainPicture
RGoToOverscan:
	sta $1FF9 ;Go to bank 3
	nop
	nop
	nop

	ORG $CFFA
	RORG $FFFA

	;NMI, IRQ, and RESET information
	.word RReset
	.word RReset
	.word RReset


