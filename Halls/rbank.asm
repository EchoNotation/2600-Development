	ORG $C000
	RORG $F000

	;BANK 0 - CONTAINS LOGIC AND DATA PERTAINING TO RENDERING THE MAZE VIEW AND THE MAJORITY OF THE BATTLE SCENE

;Sets all registers and RAM to 0. Also ensures that the processor is not in decimal mode.
RReset:
	nop $1FF7 ;Switch to bank 1, which contains the startup information

RMainPicture:
	ldx #$80
	stx VBLANK ;Disable blanking

	;Draw picture to the screen
	lda inBattle
	beq RRenderMazeView
	jmp RBattleRendering

RRenderMazeView:
RPlaceCompass:
	sta WSYNC
	lda mazeAndPartyLevel
	lsr
	lsr
	lsr
	lsr
	tay
	lda RMazeColors,y
	sta COLUP0
	sta COLUPF

	;Delay in order to put the compass in the middle of the screen
	jsr RSpinWheels
	nop
	lda #$C0
	sta HMP0
	sta RESP0
	sta WSYNC
	sta HMOVE
	ldy #CHARACTER_HEIGHT
RDrawCompass:
	sta WSYNC
	dey
	bmi RPrepareToDrawMaze
	lda (tempPointer1),y
	sta GRP0
	jmp RDrawCompass
RPrepareToDrawMaze:
	sta HMCLR
	iny
	sty GRP0
	ldy #MAZE_HEIGHT
RDrawMaze:
	sta WSYNC
	lda ROutermost,y
	sta PF0
	lda (tempPointer2),y
	sta PF1
	lda (tempPointer3),y
	sta PF2
	lda (temp4),y
	tax
	lda (temp5),y

	nop
	nop
	nop
	nop
	stx PF2
	sta PF1

	sta WSYNC
	lda ROutermost,y
	sta PF0
	lda (tempPointer2),y
	sta PF1
	lda (tempPointer3),y
	sta PF2
	lda (temp4),y
	tax
	lda (temp5),y

	nop
	nop
	nop
	nop
	stx PF2
	sta PF1

	sta WSYNC
	lda ROutermost,y
	sta PF0
	lda (tempPointer2),y
	sta PF1
	lda (tempPointer3),y
	sta PF2
	lda (temp4),y
	tax
	lda (temp5),y

	nop
	nop
	nop
	nop
	stx PF2
	sta PF1

	dey
	cpy #MAZE_HEIGHT+10
	bcc RDrawMaze
	sta WSYNC
	jsr RSpinWheels
	jsr RSpinWheels
	jsr RSpinWheels
	sta RESBL
	lda #1
	sta VDELBL
	sta WSYNC
	ldy #0
	sty PF0
	sty PF1
	sty PF2
	sty charIndex

RDrawPartyInfoMaze:
	ldx #$03 ;Triplicate
	stx NUSIZ0 ;Set both duplication registers to triplicate the sprites.
	stx NUSIZ1
	stx VDELP0
	stx VDELP1

	ldx #$10 ;Moves one color clock to the left.
	stx HMP1

	;Need some sort of delay here in order to more or less center this data.
	nop
	nop
	sta RESP0
	sta RESP1
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

	ldx #6 ;This number is extremely arbitrary-- Because I don't really understand exactly how long the rendering of party info takes.

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

	ldx #$10 ;Moves one color clock to the left.
	stx HMP1

	nop
	nop

	sta RESP0
	sta RESP1

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
RNoBlankLines:
	sta WSYNC
	ldx aoeTargetID
	cmp #$0
	bpl RShowBattlerName
	and #$C0
	cmp #$80
	beq RShowBattleOptions
RShowSpellOptions:
	lda menuLines,x
	and #$3F
	tay
	lda RSpellTextLookupTable,y
	sta tempPointer1
	lda #(RFireText >> 8 & $FF)
	sta tempPointer1+1
	lda #TEXT_COLOR
	cpx highlightedLine
	bne RDontHighlightSpell
	lda RSpellColors,y
RDontHighlightSpell
	sta COLUP0
	sta COLUP1

	jsr RDrawBattleMenuLine
	jmp RDrawBattleMenuLoop
RShowBattleOptions:
	lda menuLines,x
	and #$0F
	tay
	lda RBattleOptionsLookupTable,y
	sta tempPointer1
	lda #(RFightText >> 8 & $FF)
	sta tempPointer1+1
	lda #TEXT_COLOR
	cpx highlightedLine
	bne RDontHighlightBattleOption
	lda #TEXT_HIGHLIGHTED_COLOR
RDontHighlightBattleOption:
	sta COLUP0
	sta COLUP1
	jsr RDrawBattleMenuLine
	jmp RDrawBattleMenuLoop
RShowBattlerName:
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

	lda char1,y
	and #$0f
	tay
	lda RClassColors,y
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
	lda REnemyColorLookup,y
	sta COLUP0
	sta COLUP1

	jsr RDrawBattleMenuLine
	jmp RDrawBattleMenuLoop
RDrawBattleText:
	lda #0
	sta WSYNC
	jsr RSetBattleMessage
	jsr RSetTextPointers
	jsr RDrawText

	lda #1
	sta WSYNC
	jsr RSetBattleMessage
	jsr RSetTextPointers
	jsr RDrawText

	lda #2
	sta WSYNC
	jsr RSetBattleMessage
	jsr RSetTextPointers
	jsr RDrawText

RPostDrawingBattleText:
	sta WSYNC
	jsr RSpinWheels
	jsr RSpinWheels
	jsr RSpinWheels
	sta RESBL
	lda #1
	sta VDELBL

	ldx #1
RWaitToDrawPlayerText:
	sta WSYNC
	dex
	bne RWaitToDrawPlayerText

RDrawPartyInfoBattle:
	jsr RDrawCharacterInfo
	inc charIndex
	jsr RDrawCharacterInfo
	inc charIndex
	jsr RDrawCharacterInfo
	inc charIndex
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






RSpinWheels: SUBROUTINE
	rts

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
RXRushesY:
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
RXLevelsUp:
	.byte $0
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
	.byte $1F
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
RXEvades:
	.byte $0
	.byte $28
	.byte $FF
RXShieldFades:
	.byte $0
	.byte $19
	.byte $29

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
	tax
	beq .RGoToSourceBattlerName
	dex
	beq .RGoToTargetBattlerName
	dex
	beq .RGoToSpellName
	dex
	beq .RGoToHPCount
	dex
	jmp .RGeneralMessageStructure
.RGoToAllEmpty:
	jmp .RAllEmpty
.RGoToSourceBattlerName:
	jmp .RSourceBattlerName
.RGoToTargetBattlerName:
	jmp .RTargetBattlerName
.RGoToSpellName:
	jmp .RSpellName
.RGoToHPCount:
	jmp .RHPCount

.RGeneralMessageStructure:
	lda RMessagesLowLookup,x
	sta tempPointer1
	lda #(RStabsText >> 8 & $FF)
	cpx #$17
	bcc .RNormalMessagePage
.ROtherMessagePage:
	lda #(RToRunText >> 8 & $FF)
.RNormalMessagePage:
	sta tempPointer1+1
	jmp .RSetTempVariables
.RSourceBattlerName:
	ldx currentBattler
	cpx #4
	bcs .REnemySourceBattlerName
.RAllySourceBattlerName:
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
	bcs .REnemyIsTargeted
.RAllyIsTargeted:
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
	rts
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
	sta temp1
	sta temp2
	sta temp3
	sta temp4
	sta temp5
	sta temp6
	rts

RDrawCharacterInfo: SUBROUTINE
	ldx charIndex ;Determine which character's data is about to be drawn
	lda char1,x
	and #$0F ;Get the class of this character
	tay

	lda partyBattlePos
	and RPartyPositionMasks,x
	beq .RBackline
.RFrontline:
	lda #FRONTLINE_INDICATOR_COLOR
	jmp .RStoreIndicatorColor
.RBackline:
	lda #BACKLINE_INDICATOR_COLOR
.RStoreIndicatorColor:
	sta COLUPF

	lda #TEXT_HIGHLIGHTED_COLOR
	sta COLUP0
	sta COLUP1
	lda #LEFT_MASK
	and SWCHA
	sta temp1
	sta WSYNC

	cpx highlightedIndex
	bne .RNoHighlighting
	lda currentEffect ;Should be highlighting, and this is the currently hovered character
	cmp #$1
	bne .RNoHighlighting
	lda effectCountdown
	and #$10
	beq .RNoHighlighting
	bne .RPostClassColorSetting ;this should always branch, just saves one byte over jmp
.RNoHighlighting:
	lda RClassColors,y ;Get the color that corresponds with this class, and set both players to use that color
	sta COLUP0
	sta COLUP1
.RPostClassColorSetting:

	lda inBattle
	beq .RNotInBattle
	bne .RInBattle
.RGoToShowingHPAndMP:
	jmp .RShowingHPAndMP

.RNotInBattle:
	lda currentMenu
	bne .RInBattlePosMenu
	beq .RSetupMood
.RInBattle
.RInBattlePosMenu:
	lda temp1
	beq .RGoToShowingHPAndMP
	lda #RIGHT_MASK
	bit SWCHA
	bne .RSetupMood
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

	lda currentMenu
	bne .RCurrentlyInMenu
	sta WSYNC
.RCurrentlyInMenu:

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

	lda inBattle
	beq .RNoExtraLine
	sta WSYNC
.RNoExtraLine:

	lda #2
	sta ENABL
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
	sty ENABL
	sty GRP0 ;Clear player graphics while HP, MP data is being prepared
	sty GRP1
	sty ENABL
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

	lda inBattle
	beq .RSkipThisLine
	sta WSYNC
.RSkipThisLine:
	lda #2
	sta ENABL

	ldy #CHARACTER_HEIGHT
	dey
.RDrawHPAndMP:
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
	bpl .RDrawHPAndMP

	iny
	sty ENABL
	sty GRP0 ;Clear player graphics while HP, MP data is being prepared
	sty GRP1
	sty ENABL
	sty GRP0
	sty GRP1

	rts

.RShowingClassAndLevel:
	ldx charIndex
	lda char1,x ;Format is -MMM-CCC
	and #$0F
	tax
	lda RClassNameLookupTable,x
	sta tempPointer1
	lda #$FB
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
	lda inBattle
	beq .RSkipExtraClassLevelLine
	sta WSYNC
.RSkipExtraClassLevelLine:
	lda #2
	sta ENABL

	ldy #CHARACTER_HEIGHT
	dey
.RDrawClassAndLevel:
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
	bpl .RDrawClassAndLevel

	iny
	sty ENABL
	sty GRP0 ;Clear player graphics while HP, MP data is being prepared
	sty GRP1
	sty ENABL
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



RDrawText: SUBROUTINE ;Will update graphics registers accordingly as long as the sprites are positioned correctly and the pointers set.
	sta WSYNC
	ldy #CHARACTER_HEIGHT-1
	cmp temp1 ;Just used to delay 3 cycles
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

	ORG $C810 ;Used to hold enemy names, nothing else can go in this section
	RORG $F810

RZombieText:
	.byte #Z
	.byte #O
	.byte #M
	.byte #B
	.byte #I
	.byte #E
RGiantText:
	.byte #G
	.byte #I
	.byte #A
	.byte #N
	.byte #T
	.byte #EMPTY
RDragonText:
	.byte #D
	.byte #R
	.byte #A
	.byte #G
	.byte #O
	.byte #N

	ORG $C900 ;Used to hold text.
	RORG $F900

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
RPoisonText:
	.byte #P
	.byte #O
	.byte #I
	.byte #S
	.byte #O
	.byte #N
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
RDonateText:
	.byte #D
	.byte #O
	.byte #N
	.byte #A
	.byte #T
	.byte #E

RStabsText:
	.byte #S
	.byte #T
	.byte #A
	.byte #B
	.byte #S
	.byte #EMPTY
RShootsText:
	.byte #S
	.byte #H
	.byte #O
	.byte #O
	.byte #T
	.byte #S
RWastesText:
	.byte #W
	.byte #A
	.byte #S
	.byte #T
	.byte #E
RSlamsText:
	.byte #S ;Shared, saving 1 byte
	.byte #L
	.byte #A
	.byte #M
	.byte #S
	.byte #EMPTY
RBitesText:
	.byte #B
	.byte #I
	.byte #T
	.byte #E
	.byte #S
	.byte #EMPTY
RRushesText:
	.byte #R
	.byte #U
	.byte #S
	.byte #H
	.byte #E
	.byte #S
RCastsText:
	.byte #C
	.byte #A
	.byte #S
	.byte #T
	.byte #S
	.byte #EMPTY
RHealsText:
	.byte #H
	.byte #E
	.byte #A
	.byte #L
	.byte #S
	.byte #EMPTY
RLosesText:
	.byte #L
	.byte #O
	.byte #S
	.byte #E
	.byte #S
	.byte #EMPTY
RMissesText:
	.byte #M
	.byte #I
	.byte #S
	.byte #S
	.byte #E
	.byte #S
RLevelsText:
	.byte #L
	.byte #E
	.byte #V
	.byte #E
	.byte #L
	.byte #S
RUpText:
	.byte #U
	.byte #P
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
RMovesText:
	.byte #M
	.byte #O
	.byte #V
	.byte #E
	.byte #S
	.byte #EMPTY
RBacksText:
	.byte #B
	.byte #A
	.byte #C
	.byte #K
	.byte #S
	.byte #EMPTY
RLearnsText:
	.byte #L
	.byte #E
	.byte #A
	.byte #R
	.byte #N
RShieldMessageText:
	.byte #S
	.byte #H
	.byte #I
	.byte #E
	.byte #L
	.byte #D
RHasAText:
	.byte #H
	.byte #A
	.byte #S
	.byte #EMPTY
	.byte #A
	.byte #EMPTY
RPartyText:
	.byte #P
	.byte #A
	.byte #R
	.byte #T
	.byte #Y
	.byte #EMPTY
RDownText:
	.byte #D
	.byte #O
	.byte #W
	.byte #N
	.byte #EMPTY
	.byte #EMPTY
RAwayText:
	.byte #A
	.byte #W
	.byte #A
	.byte #Y
	.byte #EMPTY
	.byte #EMPTY
RWakesText:
	.byte #W
	.byte #A
	.byte #K
	.byte #E
	.byte #S
	.byte #EMPTY
RWasText:
	.byte #W
	.byte #A
	.byte #S
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
RCuredText:
	.byte #C
	.byte #U
	.byte #R
	.byte #E
	.byte #D
	.byte #EMPTY

	ORG $CA00 ;Used to hold more text data
	RORG $FA00

RFleesText:
	.byte #F
	.byte #L
	.byte #E
	.byte #E
	.byte #S
	.byte #EMPTY
RWinsText:
	.byte #W
	.byte #I
	.byte #N
	.byte #S
	.byte #EMPTY
	.byte #EMPTY
RTriesText:
	.byte #T
	.byte #R
	.byte #I
	.byte #E
	.byte #S
	.byte #EMPTY
RToRunText:
	.byte #T
	.byte #O
	.byte #EMPTY
	.byte #R
	.byte #U
	.byte #N
RNoText:
	.byte #N
	.byte #O
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
REffectText:
	.byte #E
	.byte #F
	.byte #F
	.byte #E
	.byte #C
	.byte #T
RCannotText:
	.byte #C
	.byte #A
	.byte #N
	.byte #N
	.byte #O
	.byte #T
REscapeText:
	.byte #E
	.byte #S
	.byte #C
	.byte #A
	.byte #P
RExiledText:
	.byte #E
	.byte #X
	.byte #I
	.byte #L
	.byte #E
	.byte #D
RFadesText:
	.byte #F
	.byte #A
	.byte #D
	.byte #E
	.byte #S
	.byte #EMPTY
REvadesText:
	.byte #E
	.byte #V
	.byte #A
	.byte #D
	.byte #E
	.byte #S
RIsText:
	.byte #I
	.byte #S
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
RAttackText:
	.byte #A
	.byte #T
	.byte #T
	.byte #A
	.byte #C
	.byte #K
RGuardsText:
	.byte #G
	.byte #U
	.byte #A
	.byte #R
	.byte #D
	.byte #S
RAsleepText:
	.byte #A
	.byte #S
	.byte #L
	.byte #E
	.byte #E
	.byte #P
RFellText:
	.byte #F
	.byte #E
	.byte #L
	.byte #L
	.byte #EMPTY
	.byte #EMPTY

RClassColors:
	.byte $8A ;Knight
	.byte $06 ;Rogue
	.byte $EC ;Cleric
	.byte $56 ;Wizard
	.byte $C8 ;Ranger
	.byte $36 ;Paladin

RCasterType: ;0 is no casting, 1 is full caster, FF is half-caster
	.byte $0 ;Knight
	.byte $0 ;Rogue
	.byte $1 ;Cleric
	.byte $1 ;Wizard
	.byte $FF ;Ranger
	.byte $FF ;Paladin

RMazeColors:
	.byte $2A ;Orange
	.byte $8A ;Blue
	.byte $6A ;Purple
	.byte $0A ;Gray

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
	.byte $DC ;POISON
	.byte $A8 ;SHARP
	.byte $CE ;BLIGHT
	.byte $9A ;TRIAGE
	.byte $BC ;WITHER
	.byte $48 ;BANISH
	.byte $56 ;TRANCE
	.byte $3A ;DONATE

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

REnemyNameLookup:
	.byte (RZombieText & $FF)
	.byte (RGiantText & $FF)
	.byte (RDragonText & $FF)

REnemyColorLookup:
	.byte $D6 ;Zombie
	.byte $EA ;Giant
	.byte $C8 ;Dragon

	ORG $CB00 ;Used to hold miscellaneous data/lookup tables and text
	RORG $FB00

RMessagesLowLookup:
	.byte (RStabsText & $FF) ;Rogue/Paladin
	.byte (RShootsText & $FF) ;Wizard/Ranger
	.byte (RSlamsText & $FF) ;Cleric
	.byte (RBitesText & $FF)
	.byte (RRushesText & $FF) ;Knight 
	.byte (RCastsText & $FF)
	.byte (RHealsText & $FF)
	.byte (RLosesText & $FF)
	.byte (RMissesText & $FF)
	.byte (RLevelsText & $FF)
	.byte (RUpText & $FF)
	.byte (RLearnsText & $FF)
	.byte (RMovesText & $FF)
	.byte (RBacksText & $FF)
	.byte (RDownText & $FF)
	.byte (RAwayText & $FF)
	.byte (RWastesText & $FF)
	.byte (RWasText & $FF)
	.byte (RCuredText & $FF)
	.byte (RWakesText & $FF)
	.byte (RHasAText & $FF)
	.byte (RShieldMessageText & $FF)
	.byte (RPartyText & $FF)
	.byte (RFleesText & $FF)
	.byte (RWinsText & $FF)
	.byte (RTriesText & $FF)
	.byte (RToRunText & $FF)
	.byte (RNoText & $FF)
	.byte (REffectText & $FF)
	.byte (RCannotText & $FF)
	.byte (REscapeText & $FF)
	.byte (RGuardsText & $FF)
	.byte (RAttackText & $FF)
	.byte (RFellText & $FF)
	.byte (RAsleepText & $FF)
	.byte (RIsText & $FF)
	.byte (REvadesText & $FF)
	.byte (RFadesText & $FF)
	.byte (RExiledText & $FF)

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
	.byte (RPoisonText & $FF)
	.byte (RSharpText & $FF)
	.byte (RBlightText & $FF)
	.byte (RTriageText & $FF)
	.byte (RWitherText & $FF)
	.byte (RBanishText & $FF)
	.byte (RTranceText & $FF)
	.byte (RDonateText & $FF)

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

RSpellListLookup:
	.byte #0
	.byte #0
	.byte (RClericSpellList & $FF)
	.byte (RWizardSpellList & $FF)
	.byte (RRangerSpellList & $FF)
	.byte (RPaladinSpellList & $FF)

RWizardSpellList:
	.byte #$0 ;BACK
	.byte #$1 ;FIRE
	.byte #$3 ;BLIZRD
	.byte #$4 ;DRAIN
	.byte #$2 ;SLEEP
	.byte #$5 ;THUNDR
	.byte #$6 ;SHIELD
	.byte #$8 ;CHAOS
	.byte #$7 ;METEOR
RClericSpellList:
	.byte #$0 ;BACK
	.byte #$9 ;HEAL
	.byte #$F ;WITHER
	.byte #$C ;SHARP
	.byte #$E ;TRIAGE
	.byte #$D ;BLIGHT
	.byte #$11 ;TRANCE
	.byte #$12 ;DONATE
	.byte #$10 ;BANISH
RPaladinSpellList:
	.byte #$0 ;BACK
	.byte #$FF 
	.byte #$9 ;HEAL
	.byte #$FF
	.byte #$A ;SMITE
	.byte #$FF
	.byte #$C ;SHARP
	.byte #$FF 
	.byte #$6 ;SHIELD
RRangerSpellList:
	.byte #$0 ;BACK
	.byte #$FF
	.byte #$B ;POISON
	.byte #$FF
	.byte #$9 ;HEAL
	.byte #$FF
	.byte #$2 ;SLEEP
	.byte #$FF
	.byte #$D ;BLIGHT

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

	ORG $CC00 ;Used to hold more graphics data
	RORG $FC00

RAvatarDead:
	.byte #%11111111
	.byte #%10000001
	.byte #%10000001
	.byte #%10000001
	.byte #%10000001
	.byte #%10000001
	.byte #%10000001
	.byte #%11111111
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
	.byte #%11111111
	.byte #%10011001
	.byte #%10111101
	.byte #%10011001
	.byte #%11000011
	.byte #%10100101
	.byte #%10000001
	.byte #%11111111
RAvatarParalyzed:
	.byte #%11111111
	.byte #%10000001
	.byte #%10101011
	.byte #%11010101
	.byte #%10000001
	.byte #%10100101
	.byte #%10000001
	.byte #%11111111
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

RNumberLookup:
	.byte 27
	.byte 28
	.byte 29
	.byte 30
	.byte 31
	.byte 32
	.byte 33
	.byte 34
	.byte 35
	.byte 36

RPartyPositionMasks:
	.byte $01
	.byte $02
	.byte $04
	.byte $08

RMoodLookupTable:
	.byte (RAvatarDead & $FF)
	.byte (RAvatarSad & $FF)
	.byte (RAvatarNeutral & $FF)
	.byte (RAvatarHappy & $FF)
	.byte (RAvatarExcited & $FF)
	.byte (RAvatarPain & $FF)
	.byte (RAvatarParalyzed & $FF)
	.byte (RAvatarSick & $FF)

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
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01111110
	.byte #%01100010
	.byte #%01100010
	.byte #%01111110
RLetterB:
	.byte #%01111110
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01111100
	.byte #%01100010
	.byte #%01100010
	.byte #%01111110
RLetterC:
	.byte #%01111110
	.byte #%01100000
	.byte #%01100000
	.byte #%01100000
	.byte #%01100000
	.byte #%01100000
	.byte #%01100000
	.byte #%01111110
RLetterD:
	.byte #%01111110
	.byte #%01100110
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01100110
	.byte #%01111110
RLetterE:
	.byte #%01111110
	.byte #%01100000
	.byte #%01100000
	.byte #%01100000
	.byte #%01111000
	.byte #%01100000
	.byte #%01100000
	.byte #%01111110
RLetterF:
	.byte #%01100000
	.byte #%01100000
	.byte #%01100000
	.byte #%01100000
	.byte #%01111000
	.byte #%01100000
	.byte #%01100000
	.byte #%01111110
RLetterG:
	.byte #%01111110
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01100110
	.byte #%01100000
	.byte #%01100000
	.byte #%01111110
RLetterH:
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01111110
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
RLetterI:
	.byte #%01111110
	.byte #%00011000
	.byte #%00011000
	.byte #%00011000
	.byte #%00011000
	.byte #%00011000
	.byte #%00011000
	.byte #%01111110
RLetterJ:
	.byte #%01111000
	.byte #%01111000
	.byte #%00011000
	.byte #%00011000
	.byte #%00011000
	.byte #%00011000
	.byte #%00011000
	.byte #%01111110
RLetterK:
	.byte #%01100010
	.byte #%01100100
	.byte #%01101000
	.byte #%01110000
	.byte #%01110000
	.byte #%01101000
	.byte #%01100100
	.byte #%01100010
RLetterL:
	.byte #%01111110
	.byte #%01100000
	.byte #%01100000
	.byte #%01100000
	.byte #%01100000
	.byte #%01100000
	.byte #%01100000
	.byte #%01100000
RLetterM:
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01101010
	.byte #%01110110
	.byte #%01100010
RLetterN:
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01100110
	.byte #%01101010
	.byte #%01110010
	.byte #%01100010
	.byte #%01100010
RLetterO:
	.byte #%00111100
	.byte #%01110110
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01110110
	.byte #%00111100
RLetterP:
	.byte #%01100000
	.byte #%01100000
	.byte #%01100000
	.byte #%01111110
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01111110
RLetterQ:
	.byte #%00111010
	.byte #%01110100
	.byte #%01101010
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01110110
	.byte #%00111100
RLetterR:
	.byte #%01100010
	.byte #%01100010
	.byte #%01100100
	.byte #%01100100
	.byte #%01111100
	.byte #%01100010
	.byte #%01100010
	.byte #%01111100
RLetterS:
	.byte #%01111110
	.byte #%01100010
	.byte #%01100010
	.byte #%00000010
	.byte #%01111110
	.byte #%01000000
	.byte #%01000110
	.byte #%01111110
RLetterT:
	.byte #%00011000
	.byte #%00011000
	.byte #%00011000
	.byte #%00011000
	.byte #%00011000
	.byte #%00011000
	.byte #%00011000
	.byte #%01111110
RLetterU:
	.byte #%01111110
	.byte #%01110110
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
RLetterV:
	.byte #%00011000
	.byte #%00111100
	.byte #%00100100
	.byte #%00100100
	.byte #%00100100
	.byte #%01000010
	.byte #%01000010
	.byte #%01000010
RLetterW:
	.byte #%01100010
	.byte #%01110110
	.byte #%01101010
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
	.byte #%01100010
RLetterX:
	.byte #%01000010
	.byte #%01100110
	.byte #%00111100
	.byte #%00011000
	.byte #%00011000
	.byte #%00111100
	.byte #%01100110
	.byte #%01000010
RLetterY:
	.byte #%00011000
	.byte #%00011000
	.byte #%00011000
	.byte #%00011000
	.byte #%00100100
	.byte #%00100100
	.byte #%01000010
	.byte #%01000010
RLetterZ:
	.byte #%01111110
	.byte #%01000000
	.byte #%00100000
	.byte #%00010000
	.byte #%00001000
	.byte #%00000100
	.byte #%00000010
	.byte #%01111110

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
	sta $1FF7 ;Go to bank 1
	nop
	nop
	nop

	ORG $CFFA
	RORG $FFFA

	;NMI, IRQ, and RESET information
	.word RReset
	.word RReset
	.word RReset


