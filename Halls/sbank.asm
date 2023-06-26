	;BANK 3 - CONTAINS SOUND EFFECT ROUTINES AND DATA (AS WELL AS LOTS OF MAZE AND BATTLE UI LOGIC)

	ORG $F000
	RORG $F000

SReset:
	sta $1FF7 ;Go to bank 1, the correct startup bank

SUpdateSound:
	rts

SGenerateMazeData: SUBROUTINE ;Will use the iterative algorithm I designed in order to generate a maze of size specified by #MAZE_WIDTH.
							 ;Make sure to clear maze data before use.
	;temp1 will act as X, temp2 as Y, temp3 as direction, temp4 as squaresRemaining, temp5 as the squaresRemaining generator
	lda #$10
	sta temp5
	lsr
	sta temp4
.SMazeGenerationLoop:
	lda #1
	sta tempPointer4 ;inlineExit
	sta tempPointer5 ;adjacentExit

	ldx temp1
	ldy temp2

	lda temp4
	cmp #1
	bne .SNotCorner
	lda temp3
	beq .SEastCorner
	cmp #2
	beq .SWestCorner
	cmp #3
	beq .SNorthCorner
.SSouthCorner:
	dex
.SNorthCorner
	jsr SRemoveVEdge
	jmp .SPrepareForNextIteration
.SWestCorner
	dey
.SEastCorner
	jsr SRemoveHEdge
	jmp .SPrepareForNextIteration

.SNotCorner:
	jsr SRandom ;The new random number is in A after returning
	and #$01
	bne .SSkipInlineExit
	sta tempPointer4 ;A is 0
.SSkipInlineExit:
	jsr SRandom
	and #$01
	bne .SSkipAdjacentExit
	sta tempPointer5 ;A is 0
.SSkipAdjacentExit
	and tempPointer4
	beq .SAtLeast1Exit
	jsr SRandom
	and #$01
	beq .SSaveInline
.SSaveAdjacent:
	lda #0
	sta tempPointer5
	beq .SAtLeast1Exit
.SSaveInline:
	sta tempPointer4
.SAtLeast1Exit
	lda tempPointer4
	bne .SNoInlineExit
	lda temp3
	beq .SEastInline
	cmp #$1
	beq .SSouthInline
	cmp #$2
	beq .SWestInline
.SNorthInline:
	dey
	jsr SRemoveHEdge
	jmp .SNoInlineExit
.SEastInline:
	jsr SRemoveVEdge
	jmp .SNoInlineExit
.SSouthInline:
	jsr SRemoveHEdge
	jmp .SNoInlineExit
.SWestInline:
	dex
	jsr SRemoveVEdge
	jmp .SNoInlineExit
.SNoInlineExit:
	ldx temp1
	ldy temp2

	lda tempPointer5
	bne .SNoAdjacentExit
	lda temp3
	beq .SEastAdjacent
	cmp #$1
	beq .SSouthAdjacent
	cmp #$2
	beq .SWestAdjacent
.SNorthAdjacent:
	jsr SRemoveVEdge
	jmp .SNoAdjacentExit
.SEastAdjacent:
	jsr SRemoveHEdge
	jmp .SNoAdjacentExit
.SSouthAdjacent:
	dex
	jsr SRemoveVEdge
	jmp .SNoAdjacentExit
.SWestAdjacent:
	dey
	jsr SRemoveHEdge
.SNoAdjacentExit
.SPrepareForNextIteration:
	dec temp4
	bne .SSkipTurning
	inc temp3
	lda temp3
	and #$03
	sta temp3 ;Turn to the right

	dec temp5 ;Get the number of steps needed before turning again
	lda temp5
	lsr
	beq .SMazeComplete
	sta temp4
.SSkipTurning
	ldx temp1
	ldy temp2
	lda temp3
	beq .SMoveEast
	cmp #$1
	beq .SMoveSouth
	cmp #$2
	beq .SMoveWest
.SMoveNorth:
	dey
	bpl .SNextIteration
.SMoveEast:
	inx
	bpl .SNextIteration
.SMoveSouth:
	iny
	bpl .SNextIteration
.SMoveWest:
	dex
.SNextIteration:
	stx temp1
	sty temp2
	jmp .SMazeGenerationLoop
.SMazeComplete:
	rts

SRemoveVEdge: SUBROUTINE ;Removes the specified vertical edge from the maze, using X and Y as x and y.
	lda #vEdges
	bne .SRemoveEdge ;RAM is located between $80 and $FF, so this is always true, and saves a byte over jmp
SRemoveHEdge:
	lda #hEdges
.SRemoveEdge:
	sta tempPointer1
	lda #0
	sta tempPointer1+1
	lda (tempPointer1),y ;Gets the relevant row of vertical edges
	sta temp6
	lda #1
.SShiftingLoop:
	cpx #0
	beq .SAfterShifting
	asl
	dex
	bpl .SShiftingLoop ;Should always be true, just saves a byte over jmp
.SAfterShifting:
	;A now contains a decoded y value
	eor #$FF
	;A now contains all 1s, except for a 0 in the correct spot for the edge to be removed
	and temp6
	sta (tempPointer1),y
	rts

SClearMazeData: SUBROUTINE ;Sets all the vertical and horizontal edges of the maze to 1 (walls).
	ldy #14
	lda #vEdges
	sta tempPointer1
	lda #0
	sta tempPointer1+1
	lda #%11111111
.SClearMazeLoop:
	sta (tempPointer1),y
	dey
	bpl .SClearMazeLoop
	rts

SGetMazeRoomData: SUBROUTINE ;Returns the four edges (0000NSEW) of the room specified by X and Y
	stx temp4
	sty temp5
	lda #0
	sta temp6 ;temp6 will store the value to return

	;Get north edge
	dey
	jsr SGetHEdge
	ora temp6
	asl
	sta temp6

	;Get south edge
	ldx temp4
	ldy temp5
	jsr SGetHEdge
	ora temp6
	asl 
	sta temp6

	;Get east edge
	ldx temp4
	;Don't need to reload Y
	jsr SGetVEdge
	ora temp6
	asl 
	sta temp6

	;Get west edge
	ldx temp4
	dex
	jsr SGetVEdge
	ora temp6
	sta temp6
	rts

SGetEdge: SUBROUTINE ;Uses X and Y to return a vertical or horizontal edge in A. Call SGetVEdge or SGetHEdge instead!
.SOutOfBounds:
	lda #1
	rts
SGetVEdge:
	cpx #0
	bmi .SOutOfBounds
	cpx #(MAZE_WIDTH-1)
	bcs .SOutOfBounds
	lda #vEdges
	bne .SGetNormalEdge
SGetHEdge:
	cpy #0
	bmi .SOutOfBounds
	cpy #(MAZE_WIDTH-1)
	bcs .SOutOfBounds
	lda #hEdges
.SGetNormalEdge:
	sta tempPointer1
	lda #0
	sta tempPointer1+1
	lda (tempPointer1),y
.SShiftingLoop:
	dex
	bmi .SDoneShifting
	lsr
	jmp .SShiftingLoop
.SDoneShifting:
	and #$1
	rts

SMazeLeftMask:
	.byte %00001000
SMazeForwardMask:
	.byte %00000010
SMazeRightMask:
	.byte %00000100
SMazeBackwardMask:
	.byte %00000001
	.byte %00001000
	.byte %00000010
	.byte %00000100

SUpdateMazeRenderingPointers: SUBROUTINE
	ldx playerX
	ldy playerY
	lda playerFacing
	beq .SFacingEast
	cmp #1
	beq .SFacingSouth
	cmp #2
	beq .SFacingWest
.SFacingNorth:
	dey
	jsr SGetMazeRoomData
	sta temp1
	ldx playerX
	dey
	jsr SGetMazeRoomData
	sta temp2
	jmp .SUpdatePointers
.SFacingEast:
	inx
	jsr SGetMazeRoomData
	sta temp1
	ldx playerX
	inx
	inx
	jsr SGetMazeRoomData
	sta temp2
	jmp .SUpdatePointers
.SFacingSouth:
	iny
	jsr SGetMazeRoomData
	sta temp1
	ldx playerX
	iny
	jsr SGetMazeRoomData
	sta temp2
	jmp .SUpdatePointers
.SFacingWest:
	dex
	jsr SGetMazeRoomData
	sta temp1
	ldx playerX
	dex
	dex
	jsr SGetMazeRoomData
	sta temp2
.SUpdatePointers:
	lda temp1
	ldy playerFacing
	and SMazeBackwardMask,y
	beq .SAtLeast1Room
	lda #(RDeadEnd1 & $FF)
	sta tempPointer2
	sta temp5
	lda #(RDeadEnd2 & $FF)
	sta tempPointer3
	sta temp4
	lda #(RDeadEnd1 >> 8 & $FF)
	sta tempPointer2+1
	sta tempPointer3+1
	sta tempPointer4
	sta tempPointer5
	rts
.SAtLeast1Room:
	lda #MAZE_POINTER_PAGE_1
	sta tempPointer2+1
	lda temp1
	and SMazeLeftMask,y
	bne .SNoNearLeftDoor
	lda #(RNearDoor & $FF)
	sta tempPointer2
	jmp .SCheckForNearRightDoor
.SNoNearLeftDoor:
	lda #(RNoNearDoor & $FF)
	sta tempPointer2
.SCheckForNearRightDoor:
	lda #MAZE_POINTER_PAGE_1
	sta tempPointer5
	lda temp1
	and SMazeRightMask,y
	bne .SNoNearRightDoor
	lda #(RNearDoor & $FF)
	sta temp5
	jmp .SCheckIfAtLeast2Rooms
.SNoNearRightDoor:
	lda #(RNoNearDoor & $FF)
	sta temp5
.SCheckIfAtLeast2Rooms:
	lda temp1
	and SMazeForwardMask,y
	beq .SAtLeast2Rooms
	lda #(ROnly1Room & $FF)
	sta tempPointer3
	sta temp4
	lda #(ROnly1Room >> 8 & $FF)
	sta tempPointer3+1
	sta tempPointer4
	rts
.SAtLeast2Rooms:
	lda temp2
	and SMazeForwardMask,y
	bne .S2Rooms
.SMoreThan2Rooms:
	lda #MAZE_POINTER_PAGE_1
	sta tempPointer3+1
	lda temp2
	and SMazeLeftMask,y
	bne .SNoFarLeftDoor1
	lda #(RFarDoor & $FF)
	sta tempPointer3
	jmp .SCheckIfFarRightDoor1
.SNoFarLeftDoor1:
	lda #(RNoFarDoor & $FF)
	sta tempPointer3
.SCheckIfFarRightDoor1:
	lda #MAZE_POINTER_PAGE_1
	sta tempPointer4
	lda temp2
	and SMazeRightMask,y
	bne .SNoFarRightDoor1
	lda #(RFarDoor & $FF)
	sta temp4
	rts
.SNoFarRightDoor1:
	lda #(RNoFarDoor & $FF)
	sta temp4
	rts
.S2Rooms:
	lda #MAZE_POINTER_PAGE_1
	sta tempPointer3+1
	lda temp2
	and SMazeLeftMask,y
	bne .SNoFarLeftDoor2
	lda #(RFarDoorOnlyTwo & $FF)
	sta tempPointer3
	jmp .SCheckIfFarRightDoor2
.SNoFarLeftDoor2:
	lda #(RNoFarDoorOnlyTwo & $FF)
	sta tempPointer3
.SCheckIfFarRightDoor2:
	lda #MAZE_POINTER_PAGE_1
	sta tempPointer4
	lda temp2
	and SMazeRightMask,y
	bne .SNoFarRightDoor2
	lda #(RFarDoorOnlyTwo & $FF)
	sta temp4
	rts
.SNoFarRightDoor2:
	lda #(RNoFarDoorOnlyTwo & $FF)
	sta temp4
	rts

STurnLeft:
	.byte 3
	.byte 0
STurnRight:
	.byte 1
	.byte 2
	.byte 3
	.byte 0

SUpdatePlayerMovement: SUBROUTINE
	lda currentMenu
	cmp #$80
	beq .SInPositionSwapMenu
	lda currentInput
	cmp previousInput
	beq .SReturnFromPlayerMovement2
	lda #$08
	bit currentInput
	beq .SGoToForwardMovement
	lda currentInput
	bpl .SRightPressed
	asl
	bpl .SLeftPressed
	asl
	bmi .SGoToCheckForForwardMovement
.SDownPressed:
	lda #$80
	sta currentMenu
	lda #3
	sta menuSize
	lda #1
	sta currentEffect
	rts	
.SRightPressed:
	ldy playerFacing
	lda STurnRight,y
	sta playerFacing
	rts
.SLeftPressed:
	ldy playerFacing
	lda STurnLeft,y
	sta playerFacing
	rts
.SInPositionSwapMenu:
	lda currentInput
	cmp previousInput
	beq .SReturnFromPlayerMovement2
	lda #$08 ;Mask for pressing button
	bit currentInput
	beq .SSwapBattlerPos
	ldy currentBattler
	lda #DOWN_MASK
	bit currentInput
	beq .SDownPressedInMenu
	lda #UP_MASK
	bit currentInput
	beq .SUpPressedInMenu
.SReturnFromPlayerMovement2:
	rts
.SGoToCheckForForwardMovement:
	jmp .SCheckForForwardMovement
.SGoToForwardMovement:
	jmp .SMoveForward
.SSwapBattlerPos:
	lda #1
	ldy currentBattler
	iny
.SPartyPosMaskLoop:
	dey
	beq .SAfterPartyPosMaskLoop
	asl
	jmp .SPartyPosMaskLoop
.SAfterPartyPosMaskLoop:
	eor partyBattlePos
	sta partyBattlePos
	rts
.SDownPressedInMenu:
	cpy menuSize
	bcc .SNotAtLastPosition
	rts
.SNotAtLastPosition
	iny
	sty currentBattler
	rts
.SUpPressedInMenu:
	ldy currentBattler
	bne .SNotAtFirstPosition
	lda #0
	sta currentMenu
	sta currentEffect
	rts
.SNotAtFirstPosition
	dey
	sty currentBattler
	rts

.SCheckForForwardMovement:
	lda currentInput
	and #$10
	bne .SReturnFromPlayerMovement

	;Code for checking if possible to move forward from current direction, and moving if so
.SMoveForward:
	ldx playerX
	ldy playerY
	jsr SGetMazeRoomData
	ldy playerFacing
	and SMazeForwardMask,y
	bne .SReturnFromPlayerMovement ;If equals 1, then there is a wall ahead in this direction

	;Possible to move in this direction, so do so.
	ldy playerFacing
	beq .SEast
	dey
	beq .SSouth
	dey
	beq .SWest

.SNorth:
	ldy playerY
	dey
	sty playerY
	rts
.SEast:
	ldx playerX
	inx
	stx playerX
	rts
.SSouth:
	ldy playerY
	iny
	sty playerY
	rts
.SWest:
	ldx playerX
	dex
	stx playerX
.SReturnFromPlayerMovement
	rts

SRandom: SUBROUTINE ;Ticks the random number generator when called
	lda rand8
	lsr
	bcc .SNoEOR
	eor #$B4
.SNoEOR:
	sta rand8
	rts

SUpdateCompassPointerBoss: SUBROUTINE ;Updates tempPointer1 in order to render an arrow at the top of the screen pointing towards this floor's exit
	;This is a good candidate for relocation to bank S
	lda exitLocation
	and #$0F
	sta temp2 ;Y location of boss
	lda exitLocation
	and #$F0
	jsr S4Lsr
	sta temp1 ;X location of boss
	sec
	sbc playerX
	sta temp1 ;X offset
	lda temp2
	sec
	sbc playerY
	sta temp2 ;Y offset

	lda temp1
	beq .SNoDeltaX
	bpl .SDeltaXPositive
.SDeltaXNegative:
	lda #$04
	sta temp3
	bne .SEncodeDeltaY
.SNoDeltaX:
	sta temp3
	beq .SEncodeDeltaY
.SDeltaXPositive
	lda #$08
	sta temp3
.SEncodeDeltaY:
	lda temp2
	beq .SNoDeltaY
	bpl .SDeltaYPositive
.SDeltaYNegative:
	lda #$01
	ora temp3
	bne .SGetArrowID
.SNoDeltaY:
	ora temp3
	bpl .SGetArrowID
.SDeltaYPositive:
	lda #$02
	ora temp3
.SGetArrowID:
	tax
	lda SArrows,x ;Get the correct arrowID if facing east
	cmp #$FF
	bne .SNotOnExit
	lda #(RLetterX & $FF)
	sta tempPointer1
	lda #(RLetterX >> 8 & $FF)
	sta tempPointer1+1
	rts

.SNotOnExit:
	;Get proper arrow ID according to facing bias
	ldy playerFacing
	iny
.SRotateLoop:
	dey
	beq .SDoneRotating
	sec
	sbc #2
	jmp .SRotateLoop
.SDoneRotating:
	cmp #0
	bpl .SNoUnderflow
	clc
	adc #8

.SNoUnderflow:
	;Set compass pointer and reflection state
	tax
	lda SArrowGraphicsLookup,X
	sta tempPointer1
	lda #(RArrowUp >> 8 & $FF)
	sta tempPointer1+1

	lda SArrowReflectionLookup,X
	sta REFP0
	rts

SUpdateMenuAdvancement: SUBROUTINE ;Checks if the button is pressed, and advances with the selected options if so.
	lda #$08 
	bit currentInput
	beq .SContinue ;Return if the button is not pressed
.SReturn:
	rts
.SContinue:
	lda currentMenu
	beq .SReturn
	ldx currentBattler
	cmp #$80
	beq .SBattleOptionsMenu
	cmp #$81
	beq .SGoToSelectEnemyMenu
	cmp #$82
	beq .SSelectAllyMenu
	cmp #$83
	beq .SSelectOtherAllyMenu
	cmp #$84
	beq .SGoToSelectSpellMenu
	cmp #$85
	beq .SNoSpellsKnownMenu
	rts

.SGoToSelectEnemyMenu:
	jmp .SSelectEnemyMenu
.SGoToSelectSpellMenu:
	jmp .SSelectSpellMenu

.SNoSpellsKnownMenu
	lda #$80
	sta currentMenu
	rts

.SSelectOtherAllyMenu:
	ldx cursorIndexAndMessageY
	inx
	ldy #0
.SIndexConversionLoop
	cpy currentBattler
	beq .SIsCurrent
	dex
	beq .SSaveAllyTargeting
.SIsCurrent:
	iny
	bpl .SIndexConversionLoop ;Saves byte over jmp


.SSelectAllyMenu:
	ldy cursorIndexAndMessageY
.SSaveAllyTargeting:
	tya
	jsr S5Asl
	ldx currentBattler
	ora battleActions,x
	sta battleActions,x
	jmp .SCheckNextBattler
.SBattleOptionsMenu:
	ldy highlightedLine
	lda menuLines,y
	cmp #$80
	beq .SSetFightAction
	cmp #$81
	beq .SEnterSpellMenu
	cmp #$82
	beq .SSetMoveAction
	cmp #$83
	beq .SSetRunAction
	cmp #$84
	beq .SSetGuardAction
.SSetParryAction:
	lda #$04
	sta battleActions,x
	jmp .SCheckNextBattler
.SSetGuardAction:
	lda #$03
	sta battleActions,x
	lda #$83
	sta currentMenu
	lda #0
	sta cursorIndexAndMessageY
	lda #2
	sta menuSize
	rts
.SSetRunAction:
	lda #$02
	sta battleActions,x
	jmp .SCheckNextBattler
.SSetMoveAction:
	lda #$01
	sta battleActions,x
	jmp .SCheckNextBattler
.SSetFightAction:
	lda #$00
	sta battleActions,x
	;Check how many enemies are alive, proceeding to $81 if more than 1 is alive
	jsr SCheckEnemies
	cpx #2
	bcs .SNeedToTargetFight
	;Only one enemy, so auto-target this fight
	ldx currentBattler ;Changed by LCheckEnemies
	tya
	jsr S5Asl
	ora battleActions,x
	sta battleActions,x
	jmp .SCheckNextBattler
.SNeedToTargetFight
	lda #$81
	sta currentMenu
	dex
	stx menuSize
	lda #0
	sta cursorIndexAndMessageY
	rts
.SEnterSpellMenu:
	;Check how many spells this party member has
	ldx currentBattler
	lda char1,x
	and #$0F ;Get just the class of this battler
	tay
	lda mazeAndPartyLevel
	and #$0F ;Get the level of the party
	tax
	cpy #4
	bcs .SIsHalfCaster
	cpx #9
	bcs .SClampLevel
	bcc .SDontClampLevel
.SClampLevel:
	dex
.SDontClampLevel:
	stx menuSize
	ldy #0
	sty cursorIndexAndMessageY
	lda #$84
	sta currentMenu
	rts
.SIsHalfCaster:
	txa
	lsr
	beq .SLevel1HalfCaster
	tax
	bne .SDontClampLevel
.SLevel1HalfCaster
	lda #$85 ;Show a special message if this party member knows no spells (only possible for level 1 Paladin and Ranger)
	sta currentMenu
	rts
.SSelectSpellMenu:
	lda highlightedLine
	and #$7F
	tay
	lda menuLines,y
	and #$1F ;Get just the spell ID
	bne .SCheckSpellLogic
	;Back button was selected
	lda #$80
	sta currentMenu
	lda #1 ;Put the cursor on CAST
	sta cursorIndexAndMessageY
	lda #3
	sta menuSize
	rts
.SCheckSpellLogic:
	ldy highlightedLine
	bpl .SConfirmSpell
	;Not enough mana to select this spell. Play an error sound effect
	rts
.SConfirmSpell:	
	;Need to determine what the targeting of this spell is in order to advance to none or correct targeting
	ldx currentBattler
	ora battleActions,x
	ora #$80 ;Set the spell flag of the action
	sta battleActions,x
	and #$1F ;Get just the spell ID again
	tax
	lda SSpellTargetingLookup,x
	beq .SNoSpellTargeting
	cmp #$01
	beq .STargetEnemy
	cmp #$82
	beq .SAllEnemies
	cmp #$03
	beq .STargetAlly
	cmp #$84
	beq .SAllAllies
	cmp #$05
	beq .SNoSpellTargeting ;Highest HP enemy, only used by THUNDR
	cmp #$86
	beq .STargetEnemy ;Currently only used by METEOR
	rts
.STargetEnemy:
	jsr SCheckEnemies
	cpx #2
	bcs .SNeedToTargetSpell
	;Only one enemy, so auto-target this spell
	ldx currentBattler ;Changed by LCheckEnemies
	tya
	jsr S5Asl
	ora battleActions,x
	sta battleActions,x
	jmp .SCheckNextBattler
.SNeedToTargetSpell:
	lda #$81
	sta currentMenu
	dex
	stx menuSize
	lda #0
	sta cursorIndexAndMessageY
	rts
.STargetAlly:
	lda #$82
	sta currentMenu
	lda #$03
	sta menuSize
	lda #0
	sta cursorIndexAndMessageY
	rts
.SAllEnemies:
.SAllAllies:
.SNoSpellTargeting:
	jmp .SCheckNextBattler
.SSelectEnemyMenu:
	lda #enemyHP
	sta tempPointer1
	lda #0
	sta tempPointer1+1
	ldx cursorIndexAndMessageY
	jsr SCursorIndexToBattlerIndex
	tya
	jsr S5Asl
	ldx currentBattler
	ora battleActions,x
	sta battleActions,x
.SCheckNextBattler:
	ldx currentBattler
	cpx #3
	bcs .SNoMoreActions
.SFindNextBattler:
	inx
	cpx #4
	bcs .SNoMoreActions ;Make sure to avoid checking enemyHP!
	lda hp1,x
	beq .SFindNextBattler
	stx currentBattler ;Found another party member, keep getting actions!
	lda #$80
	sta currentMenu
	lda #3
	sta menuSize
	lda #0
	sta cursorIndexAndMessageY
	rts
.SNoMoreActions:
	lda #$81 ;No more party member actions to collect.
	sta inBattle
	lda #0
	sta currentMenu
	rts

SUpdateMenuRendering: SUBROUTINE ;Updates the menuLines and highlightedLine according to the current menu state
	lda currentMenu
	cmp #$80
	beq .SSetupBattleOptions
	cmp #$81
	beq .SGoToEnemyTargeting
	cmp #$82
	beq .SSetupAllyTargeting
	cmp #$83
	beq .SSetupOtherAllyTargeting
	cmp #$84
	beq .SGoToSpellOptions
	cmp #$85
	beq .SGoToNoSpellsKnown
.SReturn
	rts
.SGoToEnemyTargeting:
	jmp .SSetupEnemyTargeting
.SGoToSpellOptions:
	jmp .SSetupSpellOptions
.SGoToNoSpellsKnown:
	jmp .SSetupNoSpellsKnown

.SSetupBattleOptions:
	ldx currentBattler
	lda char1,x
	and #$0F ;Get just the class of this party member
	tay
	lda SBattleTables,y
	sta tempPointer1
	lda #(SKnightBattleTable >> 8 & $FF)
	sta tempPointer1+1

	jsr SSetMenuActiveLine

	ldx #0
	ldy startingCursorIndexAndTargetID
.SOptionLinesLoop:
	lda (tempPointer1),y
	sta menuLines,x
	inx
	iny
	cpx #3
	bcs .SReturn
	bne .SOptionLinesLoop 

.SSetupOtherAllyTargeting:
	ldy cursorIndexAndMessageY
	sty highlightedLine
	ldx #0
	ldy #0
.SOtherAllyLinesLoop:
	cpx #3
	bcs .SReturn
	cpy currentBattler
	beq .SIsSelf
	sty menuLines,x
	inx
.SIsSelf:
	iny
	bne .SOtherAllyLinesLoop

.SSetupAllyTargeting:
	jsr SSetMenuActiveLine

	ldx #0
	ldy startingCursorIndexAndTargetID
.SAllyLoop:
	sty menuLines,x
	iny
	inx
	cpx #3
	bcc .SAllyLoop
	rts

.SSetupEnemyTargeting:
	lda #$FF
	sta menuLines+2 ;Make sure that the last line is cleared if there are only two enemies
	lda menuSize
	cmp #3
	bcs .SFourEnemies
	lda #0
	sta startingCursorIndexAndTargetID
	ldy cursorIndexAndMessageY
	sty highlightedLine
	bpl .SSetEnemyLines
.SFourEnemies:
	jsr SSetMenuActiveLine

.SSetEnemyLines:
	lda menuSize
	cmp #2
	bcs .SMoreThanTwo
	lda #2
	bne .SAfterLoadingCorrectSize
.SMoreThanTwo:
	lda #3
.SAfterLoadingCorrectSize:
	sta temp1
	ldx #0
	ldy startingCursorIndexAndTargetID
	iny
	iny
	iny
	iny
.SEnemyLineLoop:
	lda battlerHP,y
	beq .SNoEnemyHere
	sty menuLines,x
	inx
.SNoEnemyHere:
	iny
	cpx temp1
	bcc .SEnemyLineLoop

	lda #enemyHP
	sta tempPointer1
	lda #0
	sta tempPointer1+1
	ldx cursorIndexAndMessageY
	jsr SCursorIndexToBattlerIndex
	sty enemyAction
.SDone:
	rts

.SSetupNoSpellsKnown:
	ldx #$E1
	stx menuLines
	inx
	stx menuLines+1
	inx
	stx menuLines+2
	rts

.SSetupSpellOptions:
	lda #$FF
	sta menuLines+2 ;Set the third line to not show in case there are only two options
	ldx currentBattler
	lda char1,x
	and #$0F ;Get just the class of this party member
	sta temp2
	tax
	lda SSpellListLookup,x
	sta tempPointer1
	lda #(SWizardSpellList >> 8 & $FF)
	sta tempPointer1+1 ;tempPointer1 now points to the spell list for this class

	lda menuSize
	cmp #2
	bcs .SMoreThanTwoSpellOptions
	lda #2
	bne .SAfterLoadingCorrectSpellSize
.SMoreThanTwoSpellOptions:
	lda #3
.SAfterLoadingCorrectSpellSize:
	sta temp1

	jsr SSetMenuActiveLine

	ldx temp2
	lda SCasterType,x
	bmi .SIsHalfCaster
	ldy startingCursorIndexAndTargetID
	jmp .SSetSpells
.SIsHalfCaster:
	lda startingCursorIndexAndTargetID
	asl
	tay
.SSetSpells:
	ldx #0
.SSetSpellLoop:
	lda (tempPointer1),y
	cmp #$FF
	beq .SSkipThisSpell
	sta menuLines,x
	inx
	cpx temp1
	bcs .SCheckMana
.SSkipThisSpell:
	iny
	bne .SSetSpellLoop

.SCheckMana:
	ldy highlightedLine
	lda menuLines,y
	tay
	lda SSpellManaLookup,y
	sta temp1
	ldx currentBattler
	lda mp1,x
	cmp temp1
	bcs .SEnoughMana
	lda highlightedLine
	ora #$80
	sta highlightedLine
.SEnoughMana:
.SSpellIDsSet:
	ldx #2
.SEnforceSpellLoop:
	lda menuLines,x
	ora #$C0
	sta menuLines,x
	dex
	bpl .SEnforceSpellLoop
	rts

SSetMenuActiveLine: SUBROUTINE
	lda menuSize
	cmp #3
	bcs .SAtLeastThree
	lda cursorIndexAndMessageY
	sta highlightedLine
	lda #0
	sta startingCursorIndexAndTargetID
	rts
.SAtLeastThree:
	ldy cursorIndexAndMessageY
	beq .STopOfMenu
	cpy menuSize
	bcc .SMiddleOfMenu
.SBottomOfMenu:
	dey
	dey
	sty startingCursorIndexAndTargetID
	ldy #2
	sty highlightedLine
	rts
.SMiddleOfMenu:
	dey
	sty startingCursorIndexAndTargetID
	ldy #1
	sty highlightedLine
	rts
.STopOfMenu:
	sty startingCursorIndexAndTargetID
	sty highlightedLine
	rts

;Interprets X as the cursorPosition
SCursorIndexToBattlerIndex: SUBROUTINE ;Converts the position of a menu cursor into the correct location in the array of the target (based on tempPointer1)
	ldy #0
	inx
.SIndexConversionLoop
	lda (tempPointer1),y
	cmp #0
	beq .SNoHit
	dex
	beq .SDone
.SNoHit:
	iny
	bpl .SIndexConversionLoop ;Saves byte over jmp
.SDone:
	rts ;Y is the correct offset into the enemyID array

SCheckEnemies: SUBROUTINE ;Returns the number of enemies currently alive in X, and the last index of an alive enemy in Y
	ldx #0
	ldy #0
.SCheckEnemyLoop:
	lda enemyHP,y
	beq .SEnemyDead
	sty tempPointer6
	inx
.SEnemyDead
	iny
	cpy #4
	bcc .SCheckEnemyLoop
	ldy tempPointer6
	rts

S4Lsr: SUBROUTINE
	lsr
	lsr
	lsr
	lsr
	rts

S5Asl: SUBROUTINE
	asl
	asl
	asl
	asl
	asl
	rts

	;Arrow IDs start with 0 at straight east, then increasing moving clockwise
SArrows:
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

SArrowGraphicsLookup:
	.byte (RArrowUp & $FF)
	.byte (RArrowDiagonalUp & $FF)
	.byte (RArrowRight & $FF)
	.byte (RArrowDiagonalDown & $FF)
	.byte (RArrowDown & $FF)
	.byte (RArrowDiagonalDown & $FF)
	.byte (RArrowRight & $FF)
	.byte (RArrowDiagonalUp & $FF)

SArrowReflectionLookup:
	.byte #0
	.byte #0
	.byte #0
	.byte #0
	.byte #0
	.byte #%00001000
	.byte #%00001000
	.byte #%00001000

SNormalBattleTable:
	.byte $80
	.byte $81
	.byte $82
	.byte $83
SKnightBattleTable:
	.byte $80
	.byte $84
	.byte $82
	.byte $83
SRogueBattleTable:
	.byte $80
	.byte $85
	.byte $82
	.byte $83

SBattleTables:
	.byte (SKnightBattleTable & $FF)
	.byte (SRogueBattleTable & $FF)
	.byte (SNormalBattleTable & $FF)
	.byte (SNormalBattleTable & $FF)
	.byte (SNormalBattleTable & $FF)
	.byte (SNormalBattleTable & $FF)

SCasterType:
	.byte 0 ;Knight
	.byte 0 ;Rogue
	.byte 1 ;Cleric
	.byte 1 ;Wizard
	.byte $FF ;Ranger
	.byte $FF ;Paladin

SSpellListLookup:
	.byte (SEmptySpellList & $FF)
	.byte (SEmptySpellList & $FF)
	.byte (SClericSpellList & $FF)
	.byte (SWizardSpellList & $FF)
	.byte (SRangerSpellList & $FF)
	.byte (SPaladinSpellList & $FF)

SWizardSpellList:
	.byte #$0 ;BACK
	.byte #$1 ;FIRE
	.byte #$3 ;BLIZRD
	.byte #$4 ;DRAIN
	.byte #$2 ;SLEEP
	.byte #$5 ;THUNDR
	.byte #$6 ;SHIELD
	.byte #$8 ;CHAOS
	.byte #$7 ;METEOR
SClericSpellList:
	.byte #$0 ;BACK
	.byte #$9 ;HEAL
	.byte #$F ;WITHER
	.byte #$C ;SHARP
	.byte #$E ;TRIAGE
	.byte #$D ;BLIGHT
	.byte #$11 ;TRANCE
	.byte #$10 ;BANISH
	.byte #$12 ;WISH
SPaladinSpellList:
	.byte #$0 ;BACK
	.byte #$FF 
	.byte #$9 ;HEAL
	.byte #$FF
	.byte #$A ;SMITE
	.byte #$FF
	.byte #$C ;SHARP
	.byte #$FF 
	.byte #$6 ;SHIELD
SRangerSpellList:
	.byte #$0 ;BACK
	.byte #$FF
	.byte #$B ;POISON
	.byte #$FF
	.byte #$9 ;HEAL
	.byte #$FF
	.byte #$2 ;SLEEP
	.byte #$FF
	.byte #$D ;BLIGHT
SEmptySpellList:
	.byte #0
	.byte #$FF
	.byte #$FF
	.byte #$FF
	.byte #$FF
	.byte #$FF
	.byte #$FF
	.byte #$FF
	.byte #$FF

SSpellTargetingLookup:
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

SSpellManaLookup:
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

SHighLabelBytes:
	.byte (SGenerateMazeData >> 8 & $FF)
	.byte (SUpdateMazeRenderingPointers >> 8 & $FF)
	.byte (SUpdatePlayerMovement >> 8 & $FF)
	.byte (SClearMazeData >> 8 & $FF)
	.byte (SUpdateCompassPointerBoss >> 8 & $FF)
	.byte (SUpdateMenuAdvancement >> 8 & $FF)
	.byte (SUpdateMenuRendering >> 8 & $FF)

SLowLabelBytes:
	.byte (SGenerateMazeData & $FF)
	.byte (SUpdateMazeRenderingPointers & $FF)
	.byte (SUpdatePlayerMovement & $FF)
	.byte (SClearMazeData & $FF)
	.byte (SUpdateCompassPointerBoss & $FF)
	.byte (SUpdateMenuAdvancement & $FF)
	.byte (SUpdateMenuRendering & $FF)

	ORG $FFB0 ;Bankswitching nonsense
	RORG $FFB0

SRunFunctionForLBank:
	nop ;1
	nop ;sta $1FF9
	nop ;3
	lda SHighLabelBytes,y ;6
	sta tempPointer1+1 ;8
	lda SLowLabelBytes,y ;11
	sta tempPointer1 ;13
	lda #(SReturnLocation >> 8 & $FF) ;15
	pha ;16
	lda #(SReturnLocation & $FF) ;18
	pha ;19
SReturnLocation: 
	jmp (tempPointer1) ;22
	sta returnValue ;24
	sta $1FF7 ;Return to L bank ;27
	nop ;28

	ORG $FFFA
	RORG $FFFA

	;NMI, IRQ, and RESET information
	.word SReset
	.word SReset
	.word SReset

END