	processor 6502
	include "vcs.h"

; --- Constants ---
;Kernel related
VBLANK_TIMER_DURATION=44
OVERSCAN_TIMER_DURATION=36

;Input related
UP_MASK = %00010000
DOWN_MASK = %00100000
LEFT_MASK = %01000000
RIGHT_MASK = %10000000 

;Rendering related
CHARACTER_HEIGHT=8
MAZE_HEIGHT=38
BATTLE_BOX_COLOR=$02
TEXT_COLOR=$06
TEXT_HIGHLIGHTED_COLOR=$0E
TEXT_INVALID_COLOR=$36
FRONTLINE_INDICATOR_COLOR=$2A
BACKLINE_INDICATOR_COLOR=$8A

EMPTY=0
A=1
B=2
C=3
D=4
E=5
F=6
G=7
H=8
I=9
J=10
K=11
L=12
M=13
N=14
O=15
P=16
Q=17
R=18
S=19
T=20
U=21
V=22
W=23
X=24
Y=25
Z=26
NUMBER0=27
NUMBER1=28
NUMBER2=29
NUMBER3=30
NUMBER4=31
NUMBER5=32
NUMBER6=33
NUMBER7=34
NUMBER8=35
NUMBER9=36

;Memory related --- This is actually a bad idea and should maybe be changed
MAZE_POINTER_PAGE_1 = $FE

;Maze related
NORTH = $08
SOUTH = $04
EAST = $02
WEST = $01
MAZE_WIDTH = 8

;Battle related
GUARDED_MASK = $40
SHIELDED_MASK = $20
ASLEEP_MASK = $18
PARRYING_MASK = $04
SHARPENED_MASK = $02
BLIGHTED_MASK = $01

LEGENDARY_RESIST_MASK = $80
PHYSICAL_RESIST_MASK = $40
FIRE_RESIST_MASK = $20
ICE_RESIST_MASK = $10
HOLY_RESIST_MASK = $08
ELECTRIC_RESIST_MASK = $04
POISON_RESIST_MASK = $02

	SEG.U Variables
	ORG $80

charIndex ds 1
char1 ds 1 ; -MMM-CCC : M - Mood, C - Class
char2 ds 1
char3 ds 1
char4 ds 1
name1 ds 4
name2 ds 4
name3 ds 4
name4 ds 4
name5 ds 4
hp1 ds 1
hp2 ds 1
hp3 ds 1
hp4 ds 1
mp1 ds 1
mp2 ds 1
mp3 ds 1
mp4 ds 1
partyBattlePos ds 1 ; ----4321 a 1 in that position indicates that that character is in the frontline ;Might be able to share
playerX ds 1 ;0000XXXX
playerY ds 1 ;0000YYYY
playerFacing ds 1 ; 000000XX 00 is east, 01 is south, 10 is west, 11 is north
exitLocation ds 1
campfireLocation ds 1
previousInput ds 1
temp1 ds 1
temp2 ds 1
temp3 ds 1
temp4 ds 1
tempPointer4 ds 1
temp5 ds 1
tempPointer5 ds 1
temp6 ds 1
tempPointer6 ds 1
tempPointer1 ds 2
tempPointer2 ds 2
tempPointer3 ds 2
rand8 ds 1
experienceToNextLevel ds 1
currentMenu ds 1

cursorIndexAndMessageY ds 1
startingCursorIndexAndTargetID ds 1
aoeTargetID ds 1
highlightedIndex ds 1
menuSize ds 1
inBattle ds 1
currentBattler ds 1
currentBattlerAttack ds 1
currentBattlerSpeed ds 1

vEdges ds 8 ;Vertical edges of the maze   0xxxxxxx
hEdges ds 7 ;Horizontal edges of the maze xxxxxxxx

battleActions ds 4
enemyHP ds 4
enemyID ds 4
battlerStatus ds 8 ;TGSSlPAB - T:Guard/shield timer, G:Guard flag, S:Shield flag, Sl:Sleep timer, P:Parrying, A:Sharpened, B:Blighted
enemyAction ds 1
hasAction ds 1
currentMessage ds 1
menuLines ds 3
highlightedLine ds 1
currentSound ds 1
soundOffset ds 1
currentEffect ds 1
effectCounter ds 1
effectCountdown ds 1
mazeAndPartyLevel ds 1
returnValue ds 1

	SEG CODE 
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

	sta WSYNC
	lda currentMenu
	beq .RNoHighlighting
	cpx highlightedIndex
	bne .RNoHighlighting
	lda currentEffect ;Should be highlighting, and this is the currently hovered character
	cmp #$1
	bne .RNoHighlighting
	lda effectCountdown
	and #$10
	beq .RNoHighlighting
	lda #TEXT_HIGHLIGHTED_COLOR
	sta COLUP0
	sta COLUP1
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

.RInBattle:
	lda #LEFT_MASK ;Show HP and MP if the joystick is being held down, else show name and avatar
	bit SWCHA
	beq .RGoToShowingHPAndMP
	lda #RIGHT_MASK
	bit SWCHA
	bne .RSetupMood
	jmp .RShowingClassAndLevel
.RNotInBattle:
	lda currentMenu
	bne .RSetupMood
	lda #DOWN_MASK
	bit SWCHA
	bne .RSetupMood
	jmp .RShowingClassAndLevel ;Should this be class and level or hp and mp? --Currently going with class and level, because it runs fast enough.

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

	lda #$80
	sta inBattle
	lda #$FF
	sta hasAction
	;lda #$80
	;sta currentMenu
	;lda #$03
	;sta menuSize
	lda #1
	sta enemyHP
	sta enemyHP+1
	sta enemyHP+2
	sta enemyHP+3

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

	lda INPT4
	bmi LButtonNotPressed
	and #$80
	lsr
	lsr
	lsr
	lsr
	sta temp1
	lda previousInput
	and #$08
	eor temp1
	beq LButtonNotPressed
	lda currentMenu
	bne LExitPosSwapMenu
LEnterPosSwapMenu:
	lda #$82
	sta currentMenu
	lda #1
	sta currentEffect
	sta effectCounter
	sta effectCountdown
	lda #$03
	sta menuSize
	bne LButtonNotPressed ;Should always be true, just saves one byte over jmp
LExitPosSwapMenu:
	lda #$00
	sta currentEffect
	sta effectCounter
	sta currentMenu
	sta menuSize
LButtonNotPressed:
	lda currentMenu
	beq LNoPartyPosSwapLogic
	lda previousInput
	lda SWCHA
	and #$F0
	sta temp1
	lda previousInput
	and #$F0
	cmp temp1
	beq LNoPartyPosSwapLogic
	jsr LUpdateMenuCursorPos
	lda cursorIndexAndMessageY
	sta highlightedIndex
	lda #LEFT_MASK
	bit SWCHA
	bne LNoPartyPosSwapLogic
	;Left is held down, so swap this person
	lda #1
	ldy highlightedIndex
	iny
LPartyPosMaskLoop:
	dey
	beq LAfterPartyPosMaskLoop
	asl
	jmp LPartyPosMaskLoop
LAfterPartyPosMaskLoop:
	eor partyBattlePos
	sta partyBattlePos
LNoPartyPosSwapLogic:
	

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

	;BANK 2 - CONTAINS LOGIC AND DATA USED FOR THE RENDERING OF ENEMIES IN BATTLE

	ORG $E000
	RORG $F000

EReset:
	sta $1FF7 ;Go to bank 1, the correct startup bank

ETestEffect:
	.byte $10
	.byte $20
	.byte $30
	.byte $40
	.byte $50
	.byte $60
	.byte $70
	.byte $80

EEffectLowLookup:
	.byte 0 ;No effect
	.byte 0 ;Party member highlighting
	.byte (ETestEffect & $FF)

EEffectHighLookup:
	.byte 0 ;No effect
	.byte 0 ;Party member highlighting
	.byte (ETestEffect >> 8 & $FF)

EEffectLength:
	.byte #0
	.byte #0
	.byte #8
EEffectFrequency:
	.byte #0
	.byte #32
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
	nop
	nop
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
	sta WSYNC
	sta WSYNC
	jsr EDrawMediumEnemy
	sta WSYNC
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
	adc #32
	sta tempPointer3
	adc #32
	sta tempPointer2
	adc #32
	sta tempPointer1
	lda EEnemyColorsHighLookup,x
	sta tempPointer5
	sta tempPointer6
	lda EEnemyColorsLowLookup,x
	sta temp6
	adc #32
	sta temp5
	sta WSYNC
	jsr EDrawLargeEnemy
	ldx temp3
	inx
	inx
	inx
	inx
	stx temp3
	lda temp1
	sec
	sbc #69
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
	sty GRP0
	sty GRP1
	sty COLUP0
	sty COLUP1
ESpinWheels:
	rts

EDrawLargeEnemy: SUBROUTINE; This subroutine is used for drawing enemies that are 32x32 in size. Graphical information is pulled from tempPointers1-4, color information for columns 0 and 2 is pulled from tempPointer5, and color information for columns 1 and 3 is pulled from tempPointer6.
	lda #$10 ;Moves one pixel to the left
	sta HMP1
	jsr ESpinWheels
	jsr ESpinWheels
	nop
	nop
	cmp temp1
	sta RESP0
	sta RESP1
	sta WSYNC
	jsr ESpinWheels
	jsr ESpinWheels
	jsr ESpinWheels
	jsr ESpinWheels
	jsr ESpinWheels
	cpx temp1
	cpx temp1
	cpx temp1
	;sta WSYNC
	sta HMOVE ;need to lose 5
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
	cpx #$1 ;party member highlighting, which is the only effect handled outside this bank
	beq .EHighlightEffect
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

EEnemySizes: ;Stores the size of each enemy by enemyID. 0 if the enemy is 16x16, 1 if the enemy is 32x32
	.byte 0
	.byte 1
	.byte 2


	ORG $EFC0
	RORG $FFC0

EGoToDrawingBattleText:
	sta $1FF6 ;Go to bank 0
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
	sta $1FF7 ;Go to bank 1
	nop
	nop
	nop

	ORG $EFFA
	RORG $FFFA

	;NMI, IRQ, and RESET information
	.word EReset
	.word EReset
	.word EReset

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
	.byte 1
	.byte 2
STurnRight:
	.byte 1
	.byte 2
	.byte 3
	.byte 0

SUpdatePlayerMovement: SUBROUTINE
	lda currentMenu
	bne .SReturnFromPlayerMovement
	lda previousInput
	and #$F0
	sta temp1
	lda SWCHA
	and #$F0
	cmp temp1
	beq .SReturnFromPlayerMovement
	lda SWCHA
	bpl .SRightPressed
	asl
	bpl .SLeftPressed
	jmp .SCheckForForwardMovement
.SRightPressed
	ldy playerFacing
	lda STurnRight,y
	sta playerFacing
	jmp .SCheckForForwardMovement
.SLeftPressed
	ldy playerFacing
	lda STurnLeft,y
	sta playerFacing
.SCheckForForwardMovement
	lda SWCHA
	and #$10
	bne .SReturnFromPlayerMovement

	;Code for checking if possible to move forward from current direction, and moving if so
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

SHighLabelBytes:
	.byte (SGenerateMazeData >> 8 & $FF)
	.byte (SUpdateMazeRenderingPointers >> 8 & $FF)
	.byte (SUpdatePlayerMovement >> 8 & $FF)
	.byte (SClearMazeData >> 8 & $FF)

SLowLabelBytes:
	.byte (SGenerateMazeData & $FF)
	.byte (SUpdateMazeRenderingPointers & $FF)
	.byte (SUpdatePlayerMovement & $FF)
	.byte (SClearMazeData & $FF)

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