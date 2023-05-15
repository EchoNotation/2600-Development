	;BANK 3 - CONTAINS SOUND EFFECT ROUTINES AND DATA

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

S4Lsr: SUBROUTINE
	lsr
	lsr
	lsr
	lsr
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

SHighLabelBytes:
	.byte (SGenerateMazeData >> 8 & $FF)
	.byte (SUpdateMazeRenderingPointers >> 8 & $FF)
	.byte (SUpdatePlayerMovement >> 8 & $FF)
	.byte (SClearMazeData >> 8 & $FF)
	.byte (SUpdateCompassPointerBoss >> 8 & $FF)

SLowLabelBytes:
	.byte (SGenerateMazeData & $FF)
	.byte (SUpdateMazeRenderingPointers & $FF)
	.byte (SUpdatePlayerMovement & $FF)
	.byte (SClearMazeData & $FF)
	.byte (SUpdateCompassPointerBoss & $FF)

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