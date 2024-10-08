	;BANK 3 - THE STARTUP BANK. CONTAINS THE OVERARCHING GAME LOGIC AND EVERYTHING THAT DIDN'T FIT IN BANKS R, L, or E.

	ORG $F000
	RORG $F000

SReset:
	sta $1FF9 ;Stay in this bank
	ldx #0
	txa
	tay
SClear:
	dex
	txs
	pha	
	bne SClear
	cld

	lda #%00010001
	sta CTRLPF ;Sets the playfield to reflect, and makes the ball 4 clocks wide

	lda #0
	sta SWACNT

	;The top two lines here can be removed once mazes are no longer being generated without the setup screen.
	lda INTIM ;Seed the random number generator
	bne SSkipSeeding
	lda #$6B ;Extremely random random number generator here
SSkipSeeding:
	sta rand8

	lda #$F8
	sta currentInput
	sta previousInput

	ldx #19
	lda #A
SClearNames:
	sta name1,x
	dex
	bpl SClearNames

SSoftReset:
	ldx #$FF
	stx currentMenu
	inx
	stx VDELBL
	stx experienceToNextLevel
	stx cursorIndexAndMessageY
	stx inBattle
	stx currentSound
	lda #24
	sta menuSize

#if BUILD_DEBUG
	;Debug only code, do not include in final version!
	;ldy #0
	;sty cursorIndexAndMessageY
	lda #$09 ;Maze level 0, party level 9
	sta mazeAndPartyLevel
	;lda #24
	;sta cursorIndexAndMessageY
	;lda #0
	;sta currentMenu

	;lda #$20
	;sta campfireLocation
	;lda #$01
	;sta exitLocation
	;sta hp2
	;sta hp3
	;sta hp4
	;lda #$80
	;sta inBattle
	;sta hp1
	;lda #$81
	;sta currentMenu
	;lda #2
	;sta menuSize
	;sta enemyID
	;lda #$11
	;sta enemyID+2
	;lda #$1
	;sta enemyHP
	;sta battlerStatus
	;sta battlerStatus+4
	;lda #$88
	;sta hasAction
	;sta enemyHP+1
	;sta enemyHP+2
	;sta enemyHP+3
	;lda #$13
	;sta mazeAndEffectColor
	;ldy #6 ;Function ID
	;ldx #$07 ;Effect ID
	;jsr SRunFunctionInLBank
	;ldx #$14
	;jsr STryLoadSound
#endif
	
	ldx #$1D
	jsr STryLoadSound

SStartOfFrame:
	lda #$82
	sta VBLANK ;Enable blanking
	sta VSYNC ;Enable syncing signal
	sta WSYNC ;Requisite 3 scanlines of VSYNC
	sta WSYNC
	sta WSYNC
	lda #0
	sta VSYNC ;Stop broadcasting VSYNC signal


	jsr SRandom ;Tick the random number generator

	lda #1
	bit SWCHB
	beq SSoftReset ;Reset the game if the console reset switch is pressed

	lda #VBLANK_TIMER_DURATION
	sta TIM64T ;Set timer to complete at the end of VBLANK.

	lda inBattle
	bne SBattleLogicVBlank ;Skip this logic if we are not in maze mode...
	lda currentMenu
	cmp #$FF
	beq SWaitForVblankTimer ;Skip all VBlank logic if on the setup screen

SMazeLogicVBlank:
	jsr SUpdateCampfireRendering
	jsr SUpdateMazeRenderingPointers
SMazeLogicVBlankAbridged:
	jsr SUpdateCompassPointerBoss
	jmp SGoToUpdateEffects ;Necessary for cursor flashing in party pos menu, and transitions to battle

SBattleLogicVBlank:
	lda currentMenu
	bmi SUpdateMenuRenderingVBlank
	lda currentInput
	eor previousInput
	and #$08
	beq SWaitForVblankTimer ;Must be different from previousInput
	lda #$08
	bit currentInput
	bne SWaitForVblankTimer ;Button must be pressed in
	lda inBattle
	cmp #$81
	bne SDontNeedANewBattler
	ldy #1 ;Subroutine ID for LDetermineNextBattler
	jsr SRunFunctionInLBank
	lda inBattle
	and #$F0
	cmp #$F0
	beq SDontNeedEnemyAI
	ldx currentBattler
	cpx #4
	bcc SDontNeedEnemyAI
	jsr SDetermineEnemyAI
SDontNeedEnemyAI:
	lda inBattle
	cmp #$80
	beq SUpdateMenuRenderingVBlank ;Don't advance if we just entered the menu
SDontNeedANewBattler:
	ldy #2 ;Subroutine ID for LUpdateAvatars
	jsr SRunFunctionInLBank
	ldy #0 ;Subroutine ID for LDoBattle
	jsr SRunFunctionInLBank
	lda inBattle
	bmi SWaitForVblankTimer

SJustExitedBattle:
	lda #STEP_GRACE_PERIOD
	sta highlightedLineAndSteps
	lda #TRANSITIONING_TO_MAZE
	jsr SSetupTransitionEffect
	jmp SMazeLogicVBlankAbridged

SUpdateMenuRenderingVBlank:
	jsr SUpdateMenuRendering

SAfterEffectUpdate:
	jsr SUpdateMazeColor
SWaitForVblankTimer:
	lda INTIM
	bne SWaitForVblankTimer ;Is VBLANK over yet?
	sta WSYNC

	jmp SGoToMainPicture

SOverscan:
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
	jsr S4Lsr
	ora temp1
	sta currentInput

	jsr SUpdateSound

SChangePartyInfo: ;Change currently viewed party info based on whether or not we are in battle or maze view
	lda flags
	and #(TRANSITIONING_TO_BATTLE | TRANSITIONING_TO_MAZE | TRANSITIONING_TO_CAMPFIRE)
	bne SFinishedWithPartyInfo
	lda currentInput
	eor previousInput
	beq SFinishedWithPartyInfo
	ldx inBattle
	beq SChangePartyInfoMaze
SChangePartyInfoBattle:
	lda #RIGHT_MASK
	bit currentInput
	beq SIncrementPartyInfo
	lda #LEFT_MASK
	bit currentInput
	beq SDecrementPartyInfo
	bne SFinishedWithPartyInfo
SChangePartyInfoMaze:
	lda #$08
	bit currentInput
	bne SFinishedWithPartyInfo
SIncrementPartyInfo:
	ldx viewedPartyInfo
	inx
	cpx #3
	bcc SStoreAndFinish
	ldx #0
	beq SStoreAndFinish
SDecrementPartyInfo:
	ldx viewedPartyInfo
	dex
	bpl SStoreAndFinish
	ldx #2
SStoreAndFinish:
	stx viewedPartyInfo
SFinishedWithPartyInfo:

	lda inBattle
	beq SMazeLogicOverscan ;Skip the following logic if we are in maze mode...

SBattleLogicOverscan:
	lda currentMenu
	bpl SAfterMenuLogic
	lda currentInput
	eor previousInput
	and #$F0
	beq SNoMenuMovement
	jsr SUpdateMenuCursorUpDown
SNoMenuMovement:
	lda currentInput
	eor previousInput
	and #$08
	beq SNoMenuAdvancement	
	jsr SUpdateMenuAdvancement
SNoMenuAdvancement:
SAfterMenuLogic:
	jmp SWaitForOverscanTimer

STryEnterCampfire:
	lda flags
	and #CAMPFIRE_USED
	bne SDidNotTriggerCampfire
	lda #TRANSITIONING_TO_CAMPFIRE
	jsr SSetupTransitionEffect
	lda #$25 ;"Enemy" ID for Campfire
	sta enemyID
	lda #$FF
	sta enemyID+1
	sta enemyID+2
	sta enemyID+3
	ldy #3 ;LLoadEnemyHP
	jsr SRunFunctionInLBank
	jmp SPartyDidNotMove

SMazeLogicOverscan:
	lda currentMenu
	cmp #$FF
	beq SSetupLogicOverscan
SMazeLogicWithoutSetupCheck:
	lda flags
	and #(TRANSITIONING_TO_BATTLE | TRANSITIONING_TO_CAMPFIRE | TRANSITIONING_TO_MAZE)
	bne SPartyDidNotMove ;Party cannot move if in a transition
	jsr SUpdatePlayerMovement
	cmp #$FF
	bne SPartyDidNotMove
	;Need to check for the maze exit and campfire location
	;Need to determine if a random encounter occurs
	lda playerX
	jsr S4Asl
	ora playerY
	cmp campfireLocation
	beq STryEnterCampfire
SDidNotTriggerCampfire:
	cmp exitLocation
	bne SDidNotTriggerExit
	beq SGenerateEncounter ;Always trigger an encounter if stepping onto the exit
SDidNotTriggerExit:
	;Check to see if a random encounter should occur
	ldx #$17 ;Footstep
	jsr STryLoadSound
	ldx highlightedLineAndSteps
	bne SNoRandomEncounter
	lda rand8
	and #ENCOUNTER_RATE_MASK
	bne SPartyDidNotMove
SGenerateEncounter:	
	;Need to generate a random enocunter!
	jmp SGoToGenerateEncounter ;Go to bank 2 to generate...
SEncounterGenerated:
	ldy #3 ;LLoadEnemyHP
	jsr SRunFunctionInLBank
	lda #TRANSITIONING_TO_BATTLE
	jsr SSetupTransitionEffect
	jmp SPartyDidNotMove
SNoRandomEncounter:
	dec highlightedLineAndSteps
SPartyDidNotMove:

	lda flags
	and #(TRANSITIONING_TO_BATTLE | TRANSITIONING_TO_CAMPFIRE | TRANSITIONING_TO_MAZE)
	beq SNotTransitioning
	jsr SPerformTransitionLogic
	jmp SWaitForOverscanTimer

SSetupLogicOverscan:
	lda currentInput
	and #$08
	beq STryStartGame

	;More advanced input logic
	lda currentInput
	cmp previousInput
	beq SSameInputAsLastFrame
	sta battleActions ;If different, use the current one for 1 frame
	ldx #25 ;How many frames to wait before repeating input
	bne SAfterInputControl
SSameInputAsLastFrame:
	ldx effectCountdown
	beq SInputTimerOver
	dex
	lda #$F8
	sta battleActions ;Nothing is pressed 
	bne SAfterInputControl
SInputTimerOver:
	sta battleActions
	ldx #6 ;Frequency in frames for the input to be repeated while held
SAfterInputControl:
	stx effectCountdown
	jsr SUpdateMenuCursorLeftRight
	jsr SUpdateBallPosition
	jsr SCheckCursorChange
	jmp SWaitForOverscanTimer

STryStartGame:
	ldy cursorIndexAndMessageY
	cpy #24 ;The ready button
	bne SWaitForOverscanTimer
	;If here, that means that the button was pressed when on the ready option
	ldx #$15 ;Menu confirm
	jsr STryLoadSound
	lda #$01 ;Maze level 1, party level 1
	sta mazeAndPartyLevel
	lda #15
	sta experienceToNextLevel

	ldx #3
SForceHappyMood:
	lda char1,x
	and #$0F
	ora #$30
	sta char1,x
	dex
	bpl SForceHappyMood

	lda #NEED_NEW_MAZE
	ora flags
	sta flags
	lda #0
	sta viewedPartyInfo ;Needed because this can actually be set by the SChangePartyInfo on the main screen
	sta currentMenu
	lda #TRANSITIONING_TO_MAZE
	jsr SSetupTransitionEffect
	lda #$0A
	sta effectCountdown
	dec effectCounter
	jmp SMazeLogicWithoutSetupCheck

SNotTransitioning:

SWaitForOverscanTimer:
	lda INTIM
	bne SWaitForOverscanTimer

	jmp SStartOfFrame

SCheckCursorChange: SUBROUTINE ;Facilitates changing party member's names and classes
	ldy cursorIndexAndMessageY
	cpy #24
	bcs .SReturn ;Just return if on the ready button
	lda battleActions
	ldy enemyID ;The ID of the party member who's data is being modified
	and #UP_MASK
	beq .SUpPressed
	lda battleActions
	and #DOWN_MASK
	beq .SDownPressed
.SReturn:
	rts
.SDownPressed:
	lda #1
	sta temp6 ;The delta to apply to the class or character
	ldx enemyID+2 ;The index of the data that is being modified
	beq .SApplyClassDelta
	bne .SApplyNameDelta
.SUpPressed:
	lda #$FF ;-1
	sta temp6 ;The delta to apply to the class or character
	ldx enemyID+2 ;The index of the data that is being modified
	beq .SApplyClassDelta
.SApplyNameDelta:
	dex ;X already contains enemyID+2
	lda SNameLocations,x
	sta tempPointer1
	lda #0
	sta tempPointer1+1
	lda (tempPointer1),y
	clc
	adc temp6
	bmi .SNameUnderflow
	cmp #27
	bcs .SNameOverflow
.SStoreName:
	sta (tempPointer1),y
	rts
.SNameOverflow:
	lda #0
	beq .SStoreName
.SNameUnderflow:
	lda #26
	bne .SStoreName

.SApplyClassDelta:
	lda char1,y
	and #$0F
	clc
	adc temp6
	bmi .SClassUnderflow
	cmp #6
	bcs .SClassOverflow
.SStoreClass:
	sta temp1
	lda char1,y
	and #$F0
	ora temp1
	sta char1,y
	rts
.SClassOverflow:
	lda #0
	beq .SStoreClass
.SClassUnderflow:
	lda #5
	bne .SStoreClass

SNameLocations:
	.byte (name1)
	.byte (name2)
	.byte (name3)
	.byte (name4)
	.byte (name5)

SBallFineCoarsePositions:
	.byte $04
	.byte $75
	.byte $F5
	.byte $66
	.byte $E6
	.byte $57

SUpdateBallPosition: SUBROUTINE ;Calculates the fine and coarse position of the cursor on the setup screen.
	lda #0
	sta enemyID ;Used for which line the cursor should appear on
	lda cursorIndexAndMessageY
	cmp #24
	beq .SOnReadyButton
.SPlaceBallLoop:
	sec
	sbc #6
	bmi .SExitLoop
	inc enemyID
	bpl .SPlaceBallLoop
.SExitLoop:
	clc
	adc #6
	tay
	lda SBallFineCoarsePositions,y
	sta enemyID+1
	sty enemyID+2 ;The x id of the character that is being changed
	rts
.SOnReadyButton:
	lda #$E5
	sta enemyID+1
	lda #4
	sta enemyID
	rts

SMazeEntrances:
	.byte $10
	.byte $71
	.byte $77
	.byte $06
	.byte $37
	.byte $63
	.byte $46
	.byte $55

SMazeExits:
	.byte $76
	.byte $14
	.byte $22
	.byte $50
	.byte $00
	.byte $15
	.byte $11
	.byte $41

SPerformTransitionLogic: SUBROUTINE ;Performs individual logic during each transition based on transition type.
	cmp #TRANSITIONING_TO_BATTLE
	beq .SCheckBattleTransitionLogic
	cmp #TRANSITIONING_TO_CAMPFIRE
	beq .SCheckCampfireTransitionLogic
	cmp #TRANSITIONING_TO_MAZE
	beq .SCheckMazeTransitionLogic
	rts
.SCheckBattleTransitionLogic:
	lda currentEffect
	bne .SEffectStillPlaying ;Wait if the effect is still playing
	;Time to enter battle!
	ldy #4 ;LEnterBattleSetup
	jsr SRunFunctionInLBank
	jmp .SSharedExit
.SCheckCampfireTransitionLogic:
	lda currentEffect
	bne .SEffectStillPlaying
	lda #$80
	sta inBattle
	lda #$86
	sta currentMenu
	ldx #1 ;Two options
	stx menuSize
	dex
	stx cursorIndexAndMessageY
	beq .SSharedExit
.SCheckMazeTransitionLogic:
	lda flags
	and #NEED_NEW_MAZE
	beq .SSkipMazeGeneration
	lda effectCountdown
	cmp #$0A
	beq .SFirstGeneration
	cmp #$2
	bcs .SUsualGeneration
.SPlaceObjectives
	ldy #5 ;LLoadPlayerVars
	jsr SRunFunctionInLBank ;This operation restores all party members to max HP and MP
	ldy #2 ;LUpdateAvatars
	jsr SRunFunctionInLBank

	;Need to position player, exit, and campfire
	jsr SRandom
	and #$07
	tay
	lda SMazeExits,y
	sta exitLocation
	lda SMazeEntrances,y
	and #$0F
	sta playerY
	lda SMazeEntrances,y
	jsr S4Lsr
	sta playerX
	jsr SRandom
	and #$77 ;Only 3 bits needed for each of x and y
	bne .SNoRandomORForCampfireLoc
	ora #$41 ;suitably random number
.SNoRandomORForCampfireLoc:
	eor exitLocation
	sta campfireLocation

	lda flags
	and #(~(NEED_NEW_MAZE | CAMPFIRE_USED)) ;Finished generating new maze!
	sta flags
	rts
.SFirstGeneration:
	jsr SClearMazeData ;Only used here... Can save 4 bytes by injecting that code here.
	jsr SGenerateMazeData
	rts	
.SUsualGeneration:
	jsr SGenerateMazeDataHotDrop
	rts	
.SSkipMazeGeneration
	lda currentEffect
	bne .SEffectStillPlaying
.SSharedExit:
	lda flags
	and #(~(TRANSITIONING_TO_BATTLE | TRANSITIONING_TO_CAMPFIRE | TRANSITIONING_TO_MAZE)) ;Disable all transition flags
	sta flags
.SEffectStillPlaying:
	rts

SSetupTransitionEffect: SUBROUTINE ;Interprets A as the transition flag constant
	tay
	ora flags
	sta flags
	cpy #TRANSITIONING_TO_BATTLE
	beq .STransitionToBattle
	cpy #TRANSITIONING_TO_CAMPFIRE
	beq .STransitionToCampfire
	cpy #TRANSITIONING_TO_MAZE
	beq .STransitionToMaze
	rts
.STransitionToBattle:
	ldx #$2
	bne .SLoadEffect
.STransitionToCampfire:
	ldx #$3
	bne .SLoadEffect
.STransitionToMaze:
	ldx #$4
.SLoadEffect:
	ldy #6 ;LLoadEffect
	jsr SRunFunctionInLBank
	rts

SUpdateMazeColor: SUBROUTINE ;Updates the mazeAndEffectColor variable to the appropriate value based on the current game state
	lda mazeAndPartyLevel
	jsr S4Lsr
	and #$0F ;A now contains the current maze level
	tay
	lda flags
	and #(TRANSITIONING_TO_BATTLE | TRANSITIONING_TO_CAMPFIRE | TRANSITIONING_TO_MAZE)
	beq .SNoTransition
	cmp #TRANSITIONING_TO_MAZE
	beq .STransitioningToMaze
.STransitioningToBattle:
.STransitioningToCampfire:
	tya
	asl
	asl
	asl ;Current mazeLevel * 8
	clc
	adc effectCounter ;effectCounter range should be 0-7 or 0-3
.SSharedColorUpdating:
	tay
	lda SMazeColorsTransition,y
	sta mazeAndEffectColor
	lda #$FF
	sta aoeValueAndCampfireControl ;Do not show the campfire if transitioning
	rts
.STransitioningToMaze:
	tya
	asl
	asl
	asl ;Current mazeLevel * 8
	sta temp1
	lda #3
	sec
	sbc effectCounter
	clc
	adc temp1 ;mazeLevel * 8 + (3 - effectCounter)
	bpl .SSharedColorUpdating
.SNoTransition:
	;Otherwise not in any transition, so just show normal colors
	lda SMazeColors,y
	sta mazeAndEffectColor
	rts

SInlineExits:
	.byte 0
	.byte 0
	.byte 1
	.byte 1
	.byte 1
	.byte 0
	.byte 0
	.byte 0

SAdjacentExits:
	.byte 0
	.byte 0
	.byte 0
	.byte 0
	.byte 0
	.byte 1
	.byte 1
	.byte 1

SGenerateMazeData: SUBROUTINE ;Will use the iterative algorithm I designed in order to generate a maze of size specified by #MAZE_WIDTH.
							 ;Make sure to clear maze data before use.
	;battleActions will act as X, battleActions+1 as Y, battleActions+2 as direction, battleActions+3 as squaresRemaining, enemyHP+3 as the squaresRemaining generator
	lda #0
	sta battleActions
	sta battleActions+1
	sta battleActions+2

	lda #$10
	sta enemyHP+3
	lsr
	sta battleActions+3
SGenerateMazeDataHotDrop:
	lda #7
	sta enemyHP+1 ;The number of squares to do this iteration
.SMazeGenerationLoop:
	lda battleActions+3 ;squaresRemaining
	cmp #1
	bne .SNotCorner
	ldx battleActions ;X
	ldy battleActions+1 ;Y
	lda battleActions+2 ;direction
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
	and #$07
	tay
	lda SAdjacentExits,y
	sta tempPointer5
	lda SInlineExits,y

	bne .SNoInlineExit
	ldx battleActions
	ldy battleActions+1
	lda battleActions+2
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
	ldx battleActions
	ldy battleActions+1

	lda tempPointer5
	bne .SNoAdjacentExit
	lda battleActions+2
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
	dec battleActions+3
	bne .SSkipTurning
	inc battleActions+2
	lda battleActions+2
	and #$03
	sta battleActions+2 ;Turn to the right

	dec enemyHP+3 ;Get the number of steps needed before turning again
	lda enemyHP+3
	beq .SMazeComplete
	lsr
	sta battleActions+3
.SSkipTurning
	lda battleActions+2
	beq .SMoveEast
	cmp #$1
	beq .SMoveSouth
	cmp #$2
	beq .SMoveWest
.SMoveNorth:
	dec battleActions+1
	bpl .SNextIteration
.SMoveEast:
	inc battleActions
	bpl .SNextIteration
.SMoveSouth:
	inc battleActions+1
	bpl .SNextIteration
.SMoveWest:
	dec battleActions
.SNextIteration:
	dec enemyHP+1
	beq .SMazeChunkFinished
	jmp .SMazeGenerationLoop
.SMazeChunkFinished
.SMazeComplete:
	rts

SEdgeRemoverLookup:
	.byte $FE
	.byte $FD
	.byte $FB
	.byte $F7
	.byte $EF
	.byte $DF
	.byte $BF
	.byte $7F

SRemoveHEdge: SUBROUTINE
	lda hEdges,y
	and SEdgeRemoverLookup,x
	sta hEdges,y
	rts

SRemoveVEdge: SUBROUTINE
	lda vEdges,y
	and SEdgeRemoverLookup,x
	sta vEdges,y
	rts

SClearMazeData: SUBROUTINE ;Sets all the vertical and horizontal edges of the maze to 1 (walls).
	ldy #14
	lda #%11111111
.SClearMazeLoop:
	sta #vEdges,y
	dey
	bpl .SClearMazeLoop
	rts

SGetMazeRoomData: SUBROUTINE ;Returns the four edges (0000NSEW) of the room specified by X and Y
	dec $FC ;Remove the unused byte from the brk instruction
	lda $FC
	cmp #$FF
	bne .SNoExtraDecrementNeeded
	dec $FD
.SNoExtraDecrementNeeded:
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
	rti

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

SUpdateMazeRenderingPointers: SUBROUTINE ;Updates the 6 main pointers to point to maze graphics information. Use immediately before rendering maze view!
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
	brk ;SGetMazeRoomData
	sta temp1
	ldx playerX
	dey
	brk ;SGetMazeRoomData
	sta temp2
	jmp .SUpdatePointers
.SFacingEast:
	inx
	brk ;SGetMazeRoomData
	sta temp1
	ldx playerX
	inx
	inx
	brk ;SGetMazeRoomData
	sta temp2
	jmp .SUpdatePointers
.SFacingSouth:
	iny
	brk ;SGetMazeRoomData
	sta temp1
	ldx playerX
	iny
	brk ;SGetMazeRoomData
	sta temp2
	jmp .SUpdatePointers
.SFacingWest:
	dex
	brk ;SGetMazeRoomData
	sta temp1
	ldx playerX
	dex
	dex
	brk ;SGetMazeRoomData
	sta temp2
.SUpdatePointers:
	lda temp1
	ldy playerFacing
	and SMazeBackwardMask,y
	beq .SAtLeast1Room
	lda #$FF
	sta aoeValueAndCampfireControl ;Do not show campfire if looking at a dead end
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
	lda aoeValueAndCampfireControl
	cmp #1
	bne .SCampfireIsFine
	lda #$FF
	sta aoeValueAndCampfireControl
.SCampfireIsFine:
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

SUpdateCampfireRendering: SUBROUTINE ;Updates the campfire control variable according to the player and campfire information
	lda flags
	and #CAMPFIRE_USED
	bne .SCampfireNotVisible ;Do not show the campfire if it has already been used!
	lda campfireLocation
	and #$F0
	jsr S4Lsr
	sta temp1 ;Campfire X
	lda campfireLocation
	and #$0F
	sta temp2 ;Campfire Y
	ldx playerX
	ldy playerY
	lda playerFacing
	beq .SFacingEast
	sec
	sbc #1
	beq .SFacingSouth
	sbc #1
	beq .SFacingWest
.SFacingNorth:
	stx temp3
	dey
	sty temp4
	dey
	stx temp5
	sty temp6
	jmp .SCheckRooms
.SFacingSouth:
	stx temp3
	iny
	sty temp4
	iny
	stx temp5
	sty temp6
	jmp .SCheckRooms
.SFacingEast:
	inx
	stx temp3
	sty temp4
	inx
	stx temp5
	sty temp6
	jmp .SCheckRooms
.SFacingWest:
	dex
	stx temp3
	sty temp4
	dex
	stx temp5
	sty temp6
.SCheckRooms:
	lda temp1
	eor temp3
	sta temp3
	lda temp2
	eor temp4
	ora temp3
	beq .SCampfireIsNear
	lda temp1
	eor temp5
	sta temp5
	lda temp2
	eor temp6
	ora temp5
	beq .SCampfireIsFar
.SCampfireNotVisible:
	lda #$FF
	bne .SStoreAndReturn
.SCampfireIsFar:
	lda #$01
	bne .SStoreAndReturn
.SCampfireIsNear:
	lda #$00
.SStoreAndReturn:
	sta aoeValueAndCampfireControl
	rts

SUpdatePlayerMovement: SUBROUTINE ;Checks the joystick input to see if the player should move in the maze
	lda currentInput
	cmp previousInput
	beq .SReturnFromPlayerMovement2
	lda currentInput
	bpl .SRightPressed
	asl
	bpl .SLeftPressed
	asl
	bmi .SCheckForForwardMovement
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
.SReturnFromPlayerMovement2:
	rts

.SCheckForForwardMovement:
	lda currentInput
	and #$10
	beq .SMoveForward
	rts

	;Code for checking if possible to move forward from current direction, and moving if so
.SMoveForward:
	ldx playerX
	ldy playerY
	brk ;SGetMazeRoomData
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
	lda #$FF
	rts
.SEast:
	ldx playerX
	inx
	stx playerX
	lda #$FF
	rts
.SSouth:
	ldy playerY
	iny
	sty playerY
	lda #$FF
	rts
.SWest:
	ldx playerX
	dex
	stx playerX
	lda #$FF
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
	sta temp6
	lda #(RLetterX >> 8 & $FF)
	sta tempPointer6
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
	sta temp6
	lda #(RArrowUp >> 8 & $FF)
	sta tempPointer6

	lda SArrowReflectionLookup,X
	sta enemyAction
	rts

SUpdateMenuAdvancement: SUBROUTINE ;Checks if the button is pressed, and advances with the selected options if so.
	lda #$08 
	bit currentInput
	beq .SContinue ;Return if the button is not pressed
.SReturn:
	rts
.SContinue:
	ldx #$15 ;Menu confirm
	jsr STryLoadSound
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
	cmp #$86
	beq .SCampingMenu
	rts

.SGoToSelectEnemyMenu:
	jmp .SSelectEnemyMenu
.SGoToSelectSpellMenu:
	jmp .SSelectSpellMenu

.SNoSpellsKnownMenu
	lda #$80
	sta currentMenu
	rts

.SCampingMenu:
	lda #0
	sta currentMenu
	ldx cursorIndexAndMessageY
	beq .SDecidedToCamp
.SNotCamping:
	sta inBattle
	sta cursorIndexAndMessageY
	lda #TRANSITIONING_TO_MAZE ;LDoBattle never gets called when not using the campfire, so this is required to show the transition back to the maze
	jsr SSetupTransitionEffect
	rts
.SDecidedToCamp:
	lda #$D0
	sta inBattle
	lda #$27 ;PARTY HEALS FULLY
	sta currentMessage
	;Need to fully heal the party!
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
	ldy highlightedLineAndSteps
	lda menuLines,y
	cmp #$80
	beq .SSetFightAction
	cmp #$81
	beq .SEnterSpellMenu
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
.SSetFightAction:
	lda #$00
	sta battleActions,x
	;Check how many enemies are alive, proceeding to $81 if more than 1 is alive
	jsr SCheckEnemies
	cpx #2
	bcs .SNeedToTargetFight
	;Only one enemy, so auto-target this fight
	ldx currentBattler ;Changed by LCheckEnemies
	tya ;The last index of an alive enemy
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
	lda highlightedLineAndSteps
	and #$7F
	tay
	lda menuLines,y
	and #$1F ;Get just the spell ID
	bne .SCheckSpellLogic
	;Back button was selected
	lda #$80
	sta currentMenu
	ldx #1 ;Put the cursor on CAST
	stx cursorIndexAndMessageY
	inx
	stx menuSize
	rts
.SCheckSpellLogic:
	ldy highlightedLineAndSteps
	bpl .SConfirmSpell
	;Not enough mana to select this spell. Play an error sound effect
	ldx #$16 ;Menu nope
	jsr STryLoadSound
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
	ldx currentBattler ;X is changed by SCheckEnemies
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
	lda #2
	sta menuSize
	lda #0
	sta cursorIndexAndMessageY
	rts
.SNoMoreActions:
	lda #$81 ;No more party member actions to collect.
	sta inBattle
	lda #0
	sta currentMenu
	sta currentEffect
	rts

SUpdateMenuRendering: SUBROUTINE ;Updates the menuLines and highlightedLineAndSteps according to the current menu state
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
	cmp #$86
	beq .SCampingMenu
.SReturn
	rts
.SGoToEnemyTargeting:
	jmp .SSetupEnemyTargeting
.SGoToSpellOptions:
	jmp .SSetupSpellOptions
.SGoToNoSpellsKnown:
	jmp .SSetupNoSpellsKnown

.SCampingMenu:
	ldx #$E4
	stx menuLines
	inx
	stx menuLines+1
	lda #$FF
	sta menuLines+2
	jsr SSetMenuActiveLine
	rts

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

	ldy #0
.SOptionLinesLoop:
	lda (tempPointer1),y
	sta menuLines,y
	iny
	cpy #3
	bcc .SOptionLinesLoop
	rts

.SSetupOtherAllyTargeting:
	ldy cursorIndexAndMessageY
	sty highlightedLineAndSteps
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
	sty highlightedLineAndSteps
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
	lda #$FF
	sta highlightedLineAndSteps
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
	ldy highlightedLineAndSteps
	lda menuLines,y
	tay
	lda SSpellManaLookup,y
	sta temp1
	ldx currentBattler
	lda mp1,x
	cmp temp1
	bcs .SEnoughMana
	lda highlightedLineAndSteps
	ora #$80
	sta highlightedLineAndSteps
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

SSetMenuActiveLine: SUBROUTINE ;Determines which of the three menu lines is currently hovered by the cursor
	lda menuSize
	cmp #3
	bcs .SAtLeastThree
	lda cursorIndexAndMessageY
	sta highlightedLineAndSteps
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
	sty highlightedLineAndSteps
	rts
.SMiddleOfMenu:
	dey
	sty startingCursorIndexAndTargetID
	ldy #1
	sty highlightedLineAndSteps
	rts
.STopOfMenu:
	sty startingCursorIndexAndTargetID
	sty highlightedLineAndSteps
	rts

SUpdateMenuCursor: SUBROUTINE
SUpdateMenuCursorUpDown:
	ldy currentInput
	lda #UP_MASK
	ldx #DOWN_MASK
	bne .SUpdateMenuCursor
SUpdateMenuCursorLeftRight:
	ldy battleActions
	lda #LEFT_MASK
	ldx #RIGHT_MASK
.SUpdateMenuCursor:
	sty tempPointer4 ;The input byte to use
	ldy cursorIndexAndMessageY
	bit tempPointer4 ;Decrement mask check
	beq .SDecrement
	txa 
	bit tempPointer4 ;Increment mask check
	beq .SIncrement
	rts
.SDecrement:
	dey
	bpl .SStoreAndReturn
	rts
.SIncrement:
	cpy menuSize
	bcs .SReturn
	iny
.SStoreAndReturn:
	sty cursorIndexAndMessageY
	lda currentInput
	cmp previousInput
	beq .SReturn
	ldx #$14 ;Menu move
	jsr STryLoadSound
.SReturn:
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

SClassTargetingBias:
	.byte 3 ;Knight
	.byte 2 ;Rogue
	.byte 2 ;Cleric
	.byte 1 ;Wizard
	.byte 2 ;Ranger
	.byte 3 ;Paladin

SDetermineEnemyAI: SUBROUTINE ;Sets the enemyAction byte.
	jmp SLoadEnemyAI
SAfterLoadingEnemyAI:
	;A now contains the selected AI card for this particular enemy
	sta temp5 ;temp5 will hold the pulled card
	and #$9F ;Copy the spellcasting bit, and the ID over to the final action
	sta temp6 ;temp6 will hold the constructed action

	lda temp5
	and #$60 ;Get just the targeting mode
	beq .STargetsNone
	cmp #$20
	beq .STargetsParty
	cmp #$40
	beq .STargetsEnemies
.STargetsSelf:
	lda currentBattler
	and #$03
	jmp .SSaveAndExit
	
.STargetsParty:
.SPopulatePlayerList:
	ldx #0
.SPopulatePlayerListLoop:
	lda hp1,x
	beq .SMemberUnconscious
.SMemberConscious:
	lda char1,x
	and #$0F
	tay
	lda SClassTargetingBias,y
.SMemberUnconscious:
	sta tempPointer1,x
	inx
	cpx #4
	bcc .SPopulatePlayerListLoop

	ldx #0
	jsr SRandom
	and #$0F
.SFindPartyTargetLoop:
	sec
	sbc tempPointer1,x
	bmi .SFoundPartyTarget
	inx
	cpx #4
	bcc .SFindPartyTargetLoop
	ldx #0
	beq .SFindPartyTargetLoop
.SFoundPartyTarget:
	txa
	jmp .SSaveAndExit

.STargetsEnemies:
	jsr SRandom
	and #$03 ;A contains a random number between 0 and 3
	tax
	stx tempPointer5 ;Infinite loop breaking...
	ldy #4
.SFindEnemyLoop:
	lda temp6
	cmp #$03 ;GUARD action
	bne .SNormalCheck
	;If here, then this is a guard action, so need to make sure Y is not the current battler
	cpy currentBattler
	beq .SNoEnemyHere
.SNormalCheck:
	lda battlerHP,y
	beq .SNoEnemyHere
	dex
	bmi .SFoundEnemy
.SNoEnemyHere:
	iny
	cpy #8
	bcc .SFindEnemyLoop
	cpx tempPointer5
	beq .SBreakInfiniteLoop
	ldy #4
	bne .SFindEnemyLoop
.SBreakInfiniteLoop:
	lda #$04 ;PARRY
	sta enemyAction
	rts
.SFoundEnemy:
	tya ;A now contains the absolute index of the enemy to target
	sec
	sbc #4
.STargetsNone:
.SSaveAndExit:
	jsr S5Asl
	ora temp6
	sta enemyAction
	rts

S4Lsr: SUBROUTINE
	lsr
	lsr
	lsr
	lsr
	rts

S5Asl: SUBROUTINE
	asl
S4Asl:
	asl
	asl
	asl
	asl
	rts

SNormalBattleTable:
	.byte $80
	.byte $81
	.byte $83
SKnightBattleTable:
	.byte $80
	.byte $84
	.byte $83
SRogueBattleTable:
	.byte $80
	.byte $85
	.byte $83

	ORG $FC00
	RORG $FC00

SFireVoices:
	.byte $8
	.byte $8
	.byte $8
	.byte $8
	.byte $8
	.byte $8
	.byte $8
	.byte $8
SSleepVoices:
	.byte $C
	.byte $C
	.byte $6
	.byte $6
	.byte $6
	.byte $6
SBlizrdVoices:
	.byte $8
	.byte $8
	.byte $4
	.byte $4
	.byte $8
	.byte $8
	.byte $4
	.byte $4
	.byte $8
	.byte $8
	.byte $4
	.byte $8
	.byte $8
SDrainVoices:
	.byte $3
	.byte $3
	.byte $3
	.byte $3
	.byte $3
	.byte $3
SThundrVoices:
	.byte $2
	.byte $2
	.byte $2
	.byte $2
	.byte $3
	.byte $3
	.byte $8
	.byte $8
	.byte $8
	.byte $8
SShieldVoices:
	.byte $7
	.byte $A
	.byte $7
	.byte $A
	.byte $7
	.byte $A
	.byte $7
	.byte $A
SMeteorVoices:
	.byte $8
	.byte $8
	.byte $8
	.byte $8
	.byte $8
	.byte $8
	.byte $8
	.byte $8
	.byte $8
	.byte $C
	.byte $C
	.byte $C
	.byte $C
	.byte $C
	.byte $C
	.byte $C
SChaosVoices:
	.byte $D
	.byte $D
	.byte $D
	.byte $6
	.byte $6
	.byte $F
	.byte $F
	.byte $3
	.byte $3
	.byte $7
	.byte $7
SHealSpellVoices:
	.byte $6
	.byte $6
	.byte $6
	.byte $6
SSmiteVoices:
	.byte $3
	.byte $3
	.byte $3
	.byte $8
	.byte $0
	.byte $6
	.byte $6
	.byte $6
	.byte $6
	.byte $6
SVolleyVoices:
	.byte $8
	.byte $8
	.byte $0
	.byte $8
	.byte $8
	.byte $0
	.byte $8
	.byte $8
	.byte $0
	.byte $8
	.byte $8
SSharpVoices:
	.byte $4
	.byte $4
	.byte $4
	.byte $0
	.byte $6
	.byte $6
	.byte $6
	.byte $6
SBlightSpellVoices:
	.byte $E
	.byte $E
	.byte $E
	.byte $E
	.byte $E
	.byte $0
	.byte $0
	.byte $4
	.byte $4
	.byte $4
	.byte $4
	.byte $C
STriageVoices:
	.byte $6
	.byte $6
	.byte $6
	.byte $6
	.byte $0
	.byte $6
	.byte $6
	.byte $6
	.byte $6
SWitherVoices:
	.byte $7
	.byte $7
	.byte $7
	.byte $F
	.byte $F
	.byte $F
	.byte $F
	.byte $F
	.byte $F
SBanishSpellVoices:
	.byte $3
	.byte $F
	.byte $3
	.byte $3
	.byte $8
	.byte $8
	.byte $8
	.byte $8
	.byte $8
	.byte $8
STranceVoices:
	.byte $E
	.byte $E
	.byte $E
	.byte $E
	.byte $0
	.byte $E
	.byte $E
	.byte $E
SWishVoices:
	.byte $C
	.byte $C
	.byte $5
	.byte $5
	.byte $5
	.byte $C
	.byte $C
	.byte $C
SMenuMoveVoices:
	.byte $C
	.byte $C
SFootstepVoices:
	.byte $6
	.byte $6
SHitVoices:
	.byte $8
	.byte $8
	.byte $8
	.byte $8
SSwingVoices:
	.byte $C
	.byte $C
	.byte $C
	.byte $C
STinkVoices:
	.byte $4
	.byte $4
	.byte $4
	.byte $4
SHealVoices:
	.byte $E
	.byte $E
	.byte $E
	.byte $0
	.byte $E
	.byte $E
SDeadVoices:
	.byte $6
	.byte $6
	.byte $6
	.byte $6
	.byte $6
	.byte $6
	.byte $6
	.byte $6
SBlightVoices:
	.byte $E
	.byte $E
	.byte $E
	.byte $E
	.byte $0
	.byte $0
	.byte $0
	.byte $E
	.byte $E
	.byte $E

	ORG $FD00
	RORG $FD00

SFirePitches:
	.byte $1F
	.byte $18
	.byte $18
	.byte $1F
	.byte $1F
	.byte $18
	.byte $1F
	.byte $18
SSleepPitches:
	.byte $1F
	.byte $10
	.byte $6
	.byte $3
	.byte $7
	.byte $4
SBlizrdPitches:
	.byte $E
	.byte $F
	.byte $6
	.byte $9
	.byte $F
	.byte $E
	.byte $C
	.byte $A
	.byte $E
	.byte $F
	.byte $D
	.byte $E
	.byte $F
SDrainPitches:
	.byte $7
	.byte $3
	.byte $2
	.byte $6
	.byte $5
	.byte $1
SThundrPitches:
	.byte $C
	.byte $8
	.byte $5
	.byte $3
	.byte $1
	.byte $0
	.byte $1
	.byte $1E
	.byte $1F
	.byte $1D
SShieldPitches:
	.byte $3
	.byte $4
	.byte $4
	.byte $6
	.byte $5
	.byte $6
	.byte $7
	.byte $8
SMeteorPitches:
	.byte $F
	.byte $F
	.byte $F
	.byte $F
	.byte $F
	.byte $F
	.byte $F
	.byte $F
	.byte $0
	.byte $17
	.byte $16
	.byte $15
	.byte $14
	.byte $13
	.byte $12
	.byte $11
SChaosPitches:
	.byte $3
	.byte $3
	.byte $6
	.byte $7
	.byte $4
	.byte $1
	.byte $3
	.byte $10
	.byte $3
	.byte $1F
	.byte $5
SHealSpellPitches:
	.byte $3
	.byte $5
	.byte $6
	.byte $7
SSmitePitches:
	.byte $2
	.byte $10
	.byte $1
	.byte $0
	.byte $0
	.byte $0
	.byte $2
	.byte $1
	.byte $2
	.byte $3
SVolleyPitches:
	.byte $2
	.byte $1
	.byte $0
	.byte $1
	.byte $0
	.byte $0
	.byte $1
	.byte $2
	.byte $0
	.byte $0
	.byte $1
SSharpPitches:
	.byte $3
	.byte $3
	.byte $5
	.byte $0
	.byte $3
	.byte $1
	.byte $3
	.byte $1
SBlightSpellPitches:
	.byte $1
	.byte $0
	.byte $1
	.byte $0
	.byte $1
	.byte $0
	.byte $0
	.byte $F
	.byte $1F
	.byte $10
	.byte $F
	.byte $1F
STriagePitches:
	.byte $2
	.byte $3
	.byte $4
	.byte $5
	.byte $0
	.byte $3
	.byte $5
	.byte $6
	.byte $7
SWitherPitches:
	.byte $14
	.byte $11
	.byte $F
	.byte $C
	.byte $A
	.byte $F
	.byte $C
	.byte $A
	.byte $F
SBanishSpellPitches:
	.byte $1
	.byte $5
	.byte $5
	.byte $4
	.byte $3
	.byte $1
	.byte $1F
	.byte $1
	.byte $1F
	.byte $1
STrancePitches:
	.byte $A
	.byte $9
	.byte $4
	.byte $2
	.byte $0
	.byte $2
	.byte $2
	.byte $7
SWishPitches:
	.byte $3
	.byte $5
	.byte $D
	.byte $F
	.byte $11
	.byte $9
	.byte $7
	.byte $7
SMenuMovePitches:
	.byte $5
	.byte $4
SMenuConfirmPitches:
	.byte $3
	.byte $7
SFootstepPitches:
	.byte $B
SMenuNopePitches:
	.byte $F
SHitPitches:
	.byte $6
	.byte $2
	.byte $3
	.byte $2
SSwingPitches:
	.byte $10
	.byte $D
	.byte $D
	.byte $10
STinkPitches:
	.byte $3
	.byte $3
	.byte $3
	.byte $5
SHealPitches:
	.byte $1
	.byte $2
	.byte $4
	.byte $0
	.byte $2
	.byte $4
SDeadPitches:
	.byte $F
	.byte $F
	.byte $C
	.byte $A
	.byte $8
	.byte $5
	.byte $5
	.byte $F
SBlightPitches:
	.byte $1
	.byte $0
	.byte $1
	.byte $0
	.byte $0
	.byte $0
	.byte $0
	.byte $1
	.byte $0
	.byte $1

	ORG $FDF0
	RORG $FDF0

STryLoadSound: SUBROUTINE ;Attempts to set the sound effect X for loading
	lda currentSound
	cmp #$15
	beq .SForceLoad
	cpx currentSound
	bcc .SDontLoad ;Don't load a sound if ID is lower than one that is already playing
.SForceLoad:
	stx currentSound
	lda SSoundLengths,x
	sta soundOffset
	lda #1
	sta soundFrequency
.SDontLoad:
	rts

SUpdateSound: SUBROUTINE ;Handles the loading and playback of sound effects
	ldx currentSound
	beq .SReturn
	;Sound is already playing...
	dec soundFrequency
	bne .SReturn
.SNextSample:
	dec soundOffset
	bmi .SSoundFinished
	ldy soundOffset
	lda SSoundFrequencies,x
	sta soundFrequency
	
	lda SVoices,x
	sta tempPointer1
	lda #(SFireVoices >> 8 & $FF)
	sta tempPointer1+1
	lda (tempPointer1),y
	sta AUDC0

	lda SPitches,x
	sta tempPointer1
	lda #(SFirePitches >> 8 & $FF)
	sta tempPointer1+1
	lda (tempPointer1),y
	sta AUDF0
	lda #3
	sta AUDV0
	rts
.SSoundFinished:
	ldx #5
	lda #0
	sta currentSound
.SStopSoundLoop:
	sta AUDC0,x
	dex
	bpl .SStopSoundLoop
.SReturn:
	rts

SSoundLengths:
	.byte 0 ;No sound
	.byte 8 ;FIRE
	.byte 6 ;SLEEP
	.byte 13 ;BLIZRD
	.byte 6 ;DRAIN
	.byte 10 ;THUNDR
	.byte 8 ;SHIELD
	.byte 16 ;METEOR
	.byte 11 ;CHAOS
	.byte 4 ;HEAL
	.byte 10 ;SMITE
	.byte 11 ;VOLLEY
	.byte 8 ;SHARP
	.byte 12 ;BLIGHT spell
	.byte 9 ;TRIAGE
	.byte 9 ;WITHER
	.byte 10 ;BANISH
	.byte 8 ;TRANCE
	.byte 8 ;WISH
	.byte 0
	.byte 2 ;Menu move
	.byte 2 ;Menu confirm
	.byte 1 ;Menu nope
	.byte 2 ;Footstep
	.byte 4 ;Hit
	.byte 4 ;Swing
	.byte 4 ;Tink
	.byte 6 ;Heal
	.byte 8 ;Dead
	.byte 10 ;Blight

SSoundFrequencies:
	.byte 0 ;No sound
	.byte 10 ;FIRE
	.byte 8 ;SLEEP
	.byte 5 ;BLIZRD
	.byte 4 ;DRAIN
	.byte 5 ;THUNDR
	.byte 4 ;SHIELD
	.byte 5 ;METEOR
	.byte 4 ;CHAOS
	.byte 5 ;HEAL
	.byte 5 ;SMITE
	.byte 3 ;VOLLEY
	.byte 5 ;SHARP
	.byte 2 ;BLIGHT spell
	.byte 5 ;TRIAGE
	.byte 4 ;WITHER
	.byte 6 ;BANISH
	.byte 6 ;TRANCE
	.byte 6 ;WISH
	.byte 0
	.byte 1 ;Menu move
	.byte 4 ;Menu confirm
	.byte 6 ;Menu nope
	.byte 2 ;Footstep
	.byte 3 ;Hit
	.byte 4 ;Swing
	.byte 4 ;Tink
	.byte 4 ;Heal
	.byte 3 ;Dead
	.byte 2 ;Blight

SVoices:
	.byte 0
	.byte (SFireVoices & $FF)
	.byte (SSleepVoices & $FF)
	.byte (SBlizrdVoices & $FF)
	.byte (SDrainVoices & $FF)
	.byte (SThundrVoices & $FF)
	.byte (SShieldVoices & $FF)
	.byte (SMeteorVoices & $FF)
	.byte (SChaosVoices & $FF)
	.byte (SHealSpellVoices & $FF)
	.byte (SSmiteVoices & $FF)
	.byte (SVolleyVoices & $FF)
	.byte (SSharpVoices & $FF)
	.byte (SBlightSpellVoices & $FF)
	.byte (STriageVoices & $FF)
	.byte (SWitherVoices & $FF)
	.byte (SBanishSpellVoices & $FF)
	.byte (STranceVoices & $FF)
	.byte (SWishVoices & $FF)
	.byte 0
	.byte (SMenuMoveVoices & $FF)
	.byte (SMenuMoveVoices & $FF) ;Confirm and move are the same length using the same voices
	.byte (SWitherVoices & $FF) ;Menu nope only uses 1 sample of voice 7
	.byte (SFootstepVoices & $FF)
	.byte (SHitVoices & $FF)
	.byte (SSwingVoices & $FF)
	.byte (STinkVoices & $FF)
	.byte (SHealVoices & $FF)
	.byte (SDeadVoices & $FF)
	.byte (SBlightVoices & $FF)

SPitches:
	.byte 0
	.byte (SFirePitches & $FF)
	.byte (SSleepPitches & $FF)
	.byte (SBlizrdPitches & $FF)
	.byte (SDrainPitches & $FF)
	.byte (SThundrPitches & $FF)
	.byte (SShieldPitches & $FF)
	.byte (SMeteorPitches & $FF)
	.byte (SChaosPitches & $FF)
	.byte (SHealSpellPitches & $FF)
	.byte (SSmitePitches & $FF)
	.byte (SVolleyPitches & $FF)
	.byte (SSharpPitches & $FF)
	.byte (SBlightSpellPitches & $FF)
	.byte (STriagePitches & $FF)
	.byte (SWitherPitches & $FF)
	.byte (SBanishSpellPitches & $FF)
	.byte (STrancePitches & $FF)
	.byte (SWishPitches & $FF)
	.byte 0
	.byte (SMenuMovePitches & $FF)
	.byte (SMenuConfirmPitches & $FF)
	.byte (SMenuNopePitches & $FF)
	.byte (SFootstepPitches & $FF)
	.byte (SHitPitches & $FF)
	.byte (SSwingPitches & $FF)
	.byte (STinkPitches & $FF)
	.byte (SHealPitches & $FF)
	.byte (SDeadPitches & $FF)
	.byte (SBlightPitches & $FF)

	ORG $FEC0
	RORG $FEC0

SLoadEnemyAI:
	sta $1FF8 ;Go to bank 2
	nop
	nop
	nop
	nop
	nop
	nop
	jmp SAfterLoadingEnemyAI

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
	.byte $82 ;VOLLEY
	.byte $84 ;SHARP
	.byte $1 ;BLIGHT
	.byte $84 ;TRIAGE
	.byte $1 ;WITHER
	.byte $82 ;BANISH
	.byte $0 ;TRANCE
	.byte $84 ;WISH

	;There is 33 bytes in here...

	ORG $FF00
	RORG $FF00

SMazeColors:
	.byte $C6 ;Green --- THE GROUNDS
	.byte $0A ;Gray --- THE CASTLE
	.byte $96 ;Blue --- THE CATACOMBS
	.byte $6A ;Purple --- THE ABYSS

SMazeColorsTransition:
	.byte $00
	.byte $C0
	.byte $C2
	.byte $C4
	.byte $C6
	.byte $0F
	.byte $C6
	.byte $0F

	.byte $00
	.byte $04
	.byte $06
	.byte $08
	.byte $0A
	.byte $0F
	.byte $0A
	.byte $0F

	.byte $00
	.byte $90
	.byte $92
	.byte $94
	.byte $96
	.byte $0F
	.byte $96
	.byte $0F

	.byte $00
	.byte $60
	.byte $64
	.byte $66
	.byte $6A
	.byte $0F
	.byte $6A
	.byte $0F

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
	.byte #$B ;VOLLEY
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

	ORG $FF80
	RORG $FF80

SLoadSoundEffectFromL:
	nop
	nop
	nop
	jsr STryLoadSound
	sta $1FF7
	nop

	ORG $FFA3 ;Bankswitching nonsense
	RORG $FFA3

SGoToGenerateEncounter:
	nop $1FF8 ;Go to bank 2
	nop
	nop
	nop
	nop
	nop
	nop
	jmp SEncounterGenerated

	ORG $FFB0
	RORG $FFB0

SRunFunctionInLBank:
	nop $1FF7 ;Go to bank 1
SSpellManaLookup:
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
	.byte 0 ;SHIFT
	nop
	nop
	nop
	nop
	rts

	ORG $FFD0
	RORG $FFD0

SGoToUpdateEffects:
	nop $1FF8 ;Go to bank 2
	nop
	nop
	nop
	nop
	nop
	nop
	jmp SAfterEffectUpdate

	ORG $FFE0
	RORG $FFE0

SGoToMainPicture:
	nop $1FF6 ;Go to bank 0, it is time to render the picture
STurnLeft:
	.byte 3
	.byte 0
STurnRight:
	.byte 1
SCatchFromMainPicture:
	.byte 2
	.byte 3
	.byte 0
	jmp SOverscan

	ORG $FFFA
	RORG $FFFA

	;NMI, IRQ, and RESET information
	.word SReset
	.word SReset
	.word SGetMazeRoomData

END