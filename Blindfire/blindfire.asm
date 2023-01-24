	processor 6502
	include "vcs.h"
	include "macro.h"

; -- Constants --
PICTURE_HEIGHT = 192
PICTURE_CUTOFF = 177
SCORE_DELAY = 1
SCORE_HEIGHT = 9
INITIAL_CITY_DELAY = 128
PLAYER_DELAY = 152 ;One higher than the actual amount to account for the early decrement in the new kernel
PLAYER_HEIGHT = 9 ;Needs to be one less than actual. This is to facilitate graphics pointers
ENEMY_HEIGHT = 9
CITY_HEIGHT = 11
LOWER_UI_DELAY = 164
LOWER_UI_HEIGHT = 9
MAX_PADDLE_VALUE = 160
MIN_PADDLE_VALUE = 27
PLAYER_COLOR = $76 ;Formerly $76
CITYSCAPE_COLOR = $76
SOUND_PAGE = $FE
GRAPHICS_PAGE = $FF
MISSILE_TRAVEL_SPEED = $F9
MISSILE_DEPLOY_TOP_HEIGHT = 147
MISSILE_DEPLOY_BOTTOM_HEIGHT = 153
MISSILE_OFFSCREEN_CONSTANT = 200
VBLANK_TIMER_VALUE = 39 ;This number has been the result of fudging, so scanline count issues might very well be resolved here.
OVERSCAN_TIMER_VALUE = 36 ; ^
MAX_ENEMY_COUNT = 6
MAX_WAVE_INDEX = 14
MAX_WAVE_INDEX_NOVICE = 4
BASE_ENEMIES_PER_WAVE = 5
UI_BACKGROUND_COLOR = $03 ;Deprecated
DEFAULT_ENEMY_COLOR = $37
SOUND_EFFECT_VOLUME = 3
RANGEFINDER_VOLUME = 3
ENEMY_SPAWN_INTERVAL = 120

BRIGHTNESS_COUNTER_TRANSITION = 128 ;These numbers are arbitrary. However, they will be needed to compute indices into color lookup tables.
BRIGHTNESS_COUNTER_SHOOT = 8

	SEG.U Variables
	ORG $80

rawPaddle ds 1
tgtPlayerXPos ds 1
playerXPos ds 1
playerFineCoarse ds 1
rand8 ds 1
inGame ds 1 ;High bit is 1 when game is able to be played, 0 when game is over.
inWave ds 1 ;High bit is 0 when waiting to start next wave, 1 otherwise.
inTransition ds 1 ;0 for no transition, 1 for transitioning to wave, 2 for transitioning from wave
waveIndex ds 1
maxWaveIndex ds 1
everyOtherFrame ds 1
enemySpeed ds 1
enemiesLeftInWave ds 1
enemySpawnTimer ds 1
enemyColor ds 1
backgroundColor ds 1
brightnessCounter ds 1
cityLevel ds 1
cityDelayMinusFour ds 1
cityDelay ds 1
rangefinderVolume ds 1
rangefinderTimer ds 1
currentSound ds 1
currentSoundPointer ds 2
soundOffset ds 1
soundEffectTimer ds 1
scoreModifierLo ds 1
scoreModifierHi ds 1
scoreLo ds 1 
scoreMd ds 1
scoreHi ds 1
extraLifeThreshold ds 1
missileCount ds 1
genericPointer ds 2
scorePointer6 ds 2 ;Highest digit
scorePointer5 ds 2
scorePointer4 ds 2
scorePointer3 ds 2
scorePointer2 ds 2
scorePointer1 ds 2 ;Lowest digit
missileCountPointer2 ds 2
missileCountPointer1 ds 2
cityscape0Pointer ds 2
cityscape1Pointer ds 2
cityscape2Pointer ds 2
cityscapeGraphicsIndex ds 1
enemyGraphicsIndex ds 1
missileFineCoarse ds 1
missileStartY ds 1
missileEndY ds 1
nextEnemyY ds 1
tempDigit ds 1 ;Used for the 6-digit score routine
loopCount ds 1 ; ^
enemyCount ds 1
enemyIndex ds 1

;Only 6 enemies can be on screen at a time.
enemyFineCoarsePositions ds 6 ;FFFFCCCC
enemyXPositions ds 6
enemyYPositions ds 6
enemyYPositionsTerminator ds 1

;Each enemy/explosion requires 4 bytes of memory.
;0: Describes the type of entity it is. 00000TTT
;1: Describes the fine and coarse positioning of the enemy, used for placing it on the screen FFFFCCCC
;2: The screen x position of the enemy, where a greater number is further right, used to calculate the fine/coarse pos.
;3: The screen y position of the enemy, where a greater number is higher on the screen

	MAC READ_PADDLE
	lda INPT0
	bmi .NoUpdatePaddle
	stx rawPaddle
.NoUpdatePaddle:
	ENDM

	SEG CODE
	ORG $F800

;Sets all registers and RAM to 0.
Reset:
	ldx #0
	txa
	tay
Clear:
	dex
	txs
	pha	
	bne Clear
	cld

InitalizeValues:
	lda #GRAPHICS_PAGE ;The (indirect),y addressing mode requires 2 byte pointers. The high bytes are configured here.

	ldy #20
PointerHighByteLoop:
	sta #(scorePointer6+1),y
	dey
	dey
	bpl PointerHighByteLoop 

	sta missileStartY
	sta missileEndY
	sta enemyYPositionsTerminator

	lda #SOUND_PAGE
	sta currentSoundPointer+1

	lda #CITYSCAPE_COLOR
	sta COLUPF

	lda #$8C
	sta backgroundColor
	sta inGame

	lda INTIM ;Seed the random number generator
	bne SkipSeeding
	lda #$6B ;Extremely random random number generator here
SkipSeeding:
	sta rand8

	lda #4
	sta cityLevel
	sta soundEffectTimer
	sta missileFineCoarse ;Must prevent underflow when coarse positioning

	lda #$01
	sta extraLifeThreshold

	;Determine what the current difficulty is.
	;$80 indicates expert mode, $00 indicates novice mode.
	lda SWCHB
	and #%01000000
	asl
	bpl NoviceMode
ExpertMode:
	lda #MAX_WAVE_INDEX
	jmp AssignMaxWaveIndex
NoviceMode:
	lda #MAX_WAVE_INDEX_NOVICE
AssignMaxWaveIndex:
	sta maxWaveIndex

StartOfFrame:
	;Check if reset switch is currently held
	lda #%00000001
	bit SWCHB
	beq Reset

	;Start of VBLANK processing
	lda #%10000010 ;D7 is for dumping the paddle capacitor
	sta VBLANK
	lda #2
	sta VSYNC

	;3 Scanlines of VSYNC
	sta WSYNC
	jsr Random ;Tick the random number generator
	sta WSYNC
	sta WSYNC

	;37 Scanlines of VBLANK
	lda #0
	sta VSYNC
	lda #VBLANK_TIMER_VALUE
	sta TIM64T ;Set timer to reach 0 six scanlines before the end of VBLANK.

	lda #DEFAULT_ENEMY_COLOR
	sta enemyColor

	;Update sound effects if on correct frame
	dec soundEffectTimer
	bne WaitForVblankTimer
	lda #4
	sta soundEffectTimer

UpdateSoundEffects:
	lda currentSound
	beq SoundEffectFinished ;If no sound effect is currently playing, set the volume of that channel to 0.
	dec soundOffset ;Otherwise if the sound effect is still playing, decrease this offset to find the next tone.
	beq SoundEffectFinished ;This probably covers a strange edge case where an invalid tone is loaded.
	lda #SOUND_EFFECT_VOLUME
	sta AUDV0 ;Turn on the sound channel.
	ldy soundOffset
	lda (currentSoundPointer),y ;Load the tone associated with this sound effect and offset.
	sta AUDF0
	jmp WaitForVblankTimer
SoundEffectFinished:
	ldy #0
	sty currentSound
	sty AUDV0

WaitForVblankTimer:
	lda INTIM
	bne WaitForVblankTimer
	sta WSYNC
	ldy cityLevel
	lda CityDelayLookupTable,y 
	sta cityDelay ;Update the variables responsible for making the enemies contact the city at decreasing height as it is destroyed.
	sec
	sbc #4
	sta cityDelayMinusFour ; Update the variables responsible for...
	sta WSYNC

	;Position missile --- This level of delay places the missile one pixel to the left of the player's center
	
	;Is this all really necessary? Why doesn't the missile placement code use the RESMP0 register? Wouldn't that be far simpler and partially bypass the problem
	;of the missile being one pixel off of the player? 
	lda missileFineCoarse ;3 cycles
	sta HMM0 ;3 cycles
	and #$0f ;2 cycles
	tay ;y now contains the coarse position of the missile. 2 cycles
	nop
	nop
	nop
	nop
	cpx waveIndex
CoarsePositionMissile:
	dey
	bne CoarsePositionMissile
	sta RESM0 ;Set the missile's coarse x position to right here.
	sta WSYNC
	sta HMOVE ;Now the missile is one pixel to the left of the player's center. 

	sty enemyIndex ;Y is conveniently 0 at this point of the code, so it is a fine time to clear the progress into the list of enemies.

	;Code that will probably move elsewhere.
	lda enemyYPositions
	sta nextEnemyY ;Set the first enemy to be drawn as the first one in the list.
	sta WSYNC

	sta HMCLR ;Clear all fine positioning movement data.
	sta CXCLR ;Clear all collision data from the previous frame.
	sta WSYNC
	lda #%01000000
	sta VBLANK ;Turn the electron beam on, and allow the paddle capacitor to start charging.
	ldx #SCORE_DELAY
	tya
	sta rawPaddle ;Clear the current raw paddle value, to be calculated again.






	;Draw the actual picture to the screen, and continuously poll to find the paddle value.
WaitForScore:
	dex
	sta WSYNC
	bne WaitForScore
ScorePrep:
	ldx #$10 ;Moves one color clock to the left.
	stx HMP1
	ldx #$03 ;Triplicate
	stx NUSIZ0 ;Set both duplication registers to triplicate the sprites.
	stx NUSIZ1
	stx VDELP0
	stx VDELP1
	lda #PLAYER_COLOR
	sta COLUP0 ;Set the color of both sprites to the designated score color.
	sta COLUP1
	nop ;Timing for centering-ish the score. This process is also required for the later timing as well.
	nop
	nop
	sta RESP0 ;Set the first sprite here
	sta RESP1 ;And the second here
	sta WSYNC
	sta HMOVE ;Move the second sprite to be only one color clock behind the first.
	ldy #SCORE_HEIGHT
ScoreLoop: ;Stolen from KABOOM!
	sty loopCount
	lda (scorePointer1),y
	sta tempDigit
	sta WSYNC
	lda (scorePointer6),y
	sta GRP0
	lda (scorePointer5),y
	sta GRP1
	lda (scorePointer4),y
	sta GRP0
	lda (scorePointer2),y
	tax
	lda (scorePointer3),y
	ldy tempDigit
	sta GRP1
	stx GRP0
	sty GRP1
	sta GRP0
	ldy loopCount
	dey
	bne ScoreLoop
	sty NUSIZ1
	sty GRP0 ;Clear the graphics registers.
	sty GRP1
	sta WSYNC
	sta HMCLR ;Clear the horizontal motion.
	sty VDELP0 ;Clear the vertical delay registers.
	sty VDELP1
	ldy #$10
	sty NUSIZ0
	ldx #0
	sta WSYNC
	
	lda backgroundColor
	sta COLUBK

	lda brightnessCounter
	bne MainLoop
	sta enemyColor



MainLoop:
	sta WSYNC
	READ_PADDLE
	inx ;Increase the current y-value being drawn.	
	cpx missileEndY
	bcs MissileOff
	cpx missileStartY
	bcc MissileOff
MissileOn:
	ldy #2
	sty ENAM0
	jmp PostMissile
MissileOff:
	ldy #0
	sty ENAM0
PostMissile:
	cpx nextEnemyY ;Check to see if we are at the y-value of the next enemy to be drawn.
	beq PlaceEnemyPrep
	cpx cityDelayMinusFour ;Check to see if we have reached the bottom of the enemy-area
	bcc MainLoop ;Not reached the bottom yet, loop again.
	jmp PrepareToDrawCity ;Reached the city! Prepare to draw the cityscape to the screen.
PlaceEnemyPrep:
	lda enemyColor
	sta COLUP1
	sta WSYNC
	ldy enemyIndex
	lda enemyFineCoarsePositions,y ;Load the fine and coarse positioning of this enemy.
	sta HMP1
	and #$0F ;Get just the coarse position of the current enemy.
	tay
	nop ;Delay 7 more cycles
	nop
	cpx waveIndex
PlaceEnemy:
	dey
	bne PlaceEnemy
	sta RESP1 ;Coarsely place the enemy at this location.
	sta WSYNC
	sta HMOVE ;Finely place the enemy just placed.
DrawEnemyPrep:
	inx
	inx
	ldy #ENEMY_HEIGHT
DrawEnemy:
	READ_PADDLE
	lda EnemyGraphics,y ;Load the current row of graphics data for the currently rendering enemy.
	sta GRP1
	inx
	cpx missileEndY
	bcs MissileOff2
	cpx missileStartY
	bcc MissileOff2
MissileOn2:
	lda #2
	sta ENAM0
	jmp PostMissile2
MissileOff2:
	lda #0
	sta ENAM0
PostMissile2:
	sta WSYNC
	cpx cityDelayMinusFour ;Check to see if we have reached the bottom of the enemy-area
	bcs PrepareToDrawCity ;Reached the city! Prepare to draw the cityscape to the screen.
	dey
	bne DrawEnemy
	sty GRP1
	lda enemyIndex
	clc
	adc #1 ;Increase the index of the enemy to draw, now that this one has finished rendering.
	sta enemyIndex
	tay
	lda enemyYPositions,y ;Load the y value of the next enemy in the array.
	sta nextEnemyY
	jmp MainLoop ;Not reached the bottom yet, loop again.





PrepareToDrawCity:
	ldy #0
	sty GRP1
	ldy cityLevel
	lda CityscapeHeightLookupTable,y
	tay
WaitForCity:
	READ_PADDLE
	sta WSYNC
	inx
	cpx cityDelay
	bcc WaitForCity
DrawCity:
	lda (cityscape0Pointer),y
	sta PF0
	lda (cityscape1Pointer),y
	sta PF1
	lda (cityscape2Pointer),y
	sta PF2
	inx
	cpx missileEndY
	bcs DisablePlayerMissileCity
	cpx missileStartY
	bcc DisablePlayerMissileCity
EnablePlayerMissileCity:
	lda #2
	sta ENAM0
	jmp CheckIfDoneCity
DisablePlayerMissileCity:
	lda #0
	sta ENAM0
CheckIfDoneCity:
	READ_PADDLE
	sta WSYNC
	cpx #PLAYER_DELAY
	bcc DrawCity2
	lda #0
	sta PF0
	sta PF1
	sta PF2
	jmp PlacePlayerPrep
DrawCity2:
	lda (cityscape0Pointer),y
	sta PF0
	lda (cityscape1Pointer),y
	sta PF1
	lda (cityscape2Pointer),y
	sta PF2
	dey
	inx
	cpx missileEndY
	bcs DisablePlayerMissileCity2
	cpx missileStartY
	bcc DisablePlayerMissileCity2
EnablePlayerMissileCity2:
	lda #2
	sta ENAM0
	jmp CheckIfDoneCity2
DisablePlayerMissileCity2:
	lda #0
	sta ENAM0
CheckIfDoneCity2:
	READ_PADDLE
	sta WSYNC
	cpx #PLAYER_DELAY
	bcc DrawCity
	lda #0
	sta PF0
	sta PF1
	sta PF2




PlacePlayerPrep:
	lda inGame
	bpl SkipPlayer
	lda #PLAYER_COLOR
	sta COLUP0
	lda playerFineCoarse
	sta HMP0
	and #$0f
	tay
	sta WSYNC
	lda #0
	sta COLUBK
	nop
	nop
	jsr SpinWheels ;Delays 12 cycles
PlacePlayer:
	dey
	bne PlacePlayer
	sta RESP0 ;Place player coarsely.
	sta WSYNC
	sta HMOVE ;Update the player's fine position.
	ldy #PLAYER_HEIGHT
DrawPlayerLoop:
	lda PlayerGraphics,y ;Load the current row of the player's graphics data.
	sta GRP0
	READ_PADDLE
	sta WSYNC
	inx
	dey
	bne DrawPlayerLoop
	jmp PrepLowerUI

SkipPlayer:
	lda #0
	sta WSYNC
	sta COLUBK
	lda #PLAYER_COLOR
	sta COLUP0
	sta COLUP1
	sta WSYNC
PrepLowerUI:
	lda #0
	sta GRP0 ;Clear the sprite's graphics data.
	sta HMP0
	sta ENAM0 ;Make sure the missile is not on
	lda #PLAYER_COLOR
	sta COLUP1
WaitForLowerUI:
	sta WSYNC
	inx
	cpx #LOWER_UI_DELAY
	bne WaitForLowerUI
PositionLowerUI:
	lda #$10
	sta HMP1
	jsr SpinWheels ;Waste time until the electronbeam hits where the missile count indicator should be.
	jsr SpinWheels
	nop
	cpx waveIndex
	sta RESP0 ;Place the missile count indicator here.
	sta RESP1
DrawLowerUI:	
	ldy #LOWER_UI_HEIGHT
	sta WSYNC
	sta HMOVE	
DrawLowerUILoop:
	lda (missileCountPointer2),y ;Load the graphics data for the first digit of the missile count.
	sta GRP0
	lda (missileCountPointer1),y ;Load the graphics data for the second digit of the missile count.
	sta GRP1
	sta WSYNC
	inx
	dey
	bne DrawLowerUILoop
	inx
	sty GRP0
	sty GRP1




FinishDrawing:
	sta WSYNC
	inx
	cpx #PICTURE_CUTOFF
	bne FinishDrawing
	sty GRP0 ;Clears VDEL as well
	sty GRP1
	;End of drawing-- Enter VBLANK
	lda #%01000010
	sta VBLANK
	sta HMCLR
	lda #OVERSCAN_TIMER_VALUE
	sta TIM64T ;Set timer to reach 0 at the end of overscan.s
	
	lda inTransition
	cmp #1
	beq UpdateTransitionToWave
	cmp #2
	beq UpdateTransitionFromWave
	jmp DecodePlayerPosition
UpdateTransitionToWave: ;Determines if the screen is finished darkening after initiating transition.
	lda brightnessCounter
	beq EndTransitionToWave
	dec brightnessCounter ;Background counter is not yet at 0, so decrement it again.
	lda brightnessCounter
	jsr FourLsr
	tay
	lda ColorLookupTransition,y
	sta backgroundColor

	;Update alarm sound effect
	tya ;1
	lsr ;1
	and #$01 ;2
	sta AUDF1 ;2
	lda #$0e ;2
	sta AUDC1 ;2
	lda #SOUND_EFFECT_VOLUME ;2
	sta AUDV1 ;2

	jmp DecodePlayerPosition
UpdateTransitionFromWave: ;Determines if the screen is finished brightening after wave completion.
	lda brightnessCounter
	cmp #BRIGHTNESS_COUNTER_TRANSITION-1
	bcs EndTransition
	inc brightnessCounter ;Background counter is not high enough yet, so increment it again.
	lda brightnessCounter
	jsr FourLsr
	tay
	lda ColorLookupTransition,y
	sta backgroundColor
	lda missileCount
	beq DecodePlayerPosition
	sec
	sed
	sbc #1
	sta missileCount
	lda #$25
	sta scoreModifierLo
	lda #0
	sta AUDV1 ;Cancel rangefinder sound
	sta scoreModifierHi
	jsr AddToScore

	;Queue up sound effect for scoring...
	ldx #1
	jsr InitSoundEffect

	jmp DecodePlayerPosition
EndTransitionToWave:
	lda #$8C
	sta inWave
	sta AUDC1
EndTransition:
	lda #0
	sta inTransition

	lda cityLevel
	bne SkipGameOver
	sta inGame
	ldx #4
	jsr InitSoundEffect
SkipGameOver:


DecodePlayerPosition: ;Convert the rawPaddle value into the player's fine coarse position.
	lda rawPaddle
	cmp #MIN_PADDLE_VALUE
	bcc AtMax
	cmp #MAX_PADDLE_VALUE
	bcs AtMin
	sec
	lda #MAX_PADDLE_VALUE
	sbc rawPaddle
	jmp NoChange
AtMin:
	lda #$05 ;Formerly $00
	jmp NoChange
AtMax:
	lda #$80 ;Formerly $85
NoChange:
	sta tgtPlayerXPos
CalculatePlayerAcceleration:
	clc
	adc playerXPos
	bcs HandleOverflow
	lsr ;Calculate the average of the tgt position, and the player's current position
	jmp SkipOverflowHandling
HandleOverflow:
	lda #$80 ;Formerly $85
SkipOverflowHandling:
	sta playerXPos
	jsr CalculateFineCoarse
	sta playerFineCoarse

ParseTrigger:
	;Determine if the trigger button was pressed, and perform the correct actions if so.
	lda inGame
	bpl GoToSkipWaveLogic	
	lda SWCHA
	bmi ButtonNotPressed
	lda inWave
	bmi AttemptToFireMissile
InitWave:
	;Button was pressed, so it is time to initialize the wave!
	lda inTransition
	bne ButtonNotPressed ;Prevents the fire button from being held down, and remaining locked in an endless transition
	lda #1
	sta inTransition
	lda #BRIGHTNESS_COUNTER_TRANSITION
	sta brightnessCounter

	ldy waveIndex
	tya
	clc
	adc #BASE_ENEMIES_PER_WAVE
	sta enemiesLeftInWave
	lda EnemySpeedPerWave,y
	sta enemySpeed
	lda AmmunitionPerWave,y
	sta missileCount
	
	iny
	cpy maxWaveIndex
	bcc SkipWaveClamping
	ldy maxWaveIndex
SkipWaveClamping
	sty waveIndex

	jmp ButtonNotPressed
AttemptToFireMissile:
	lda inTransition
	bne GoToSkipWaveLogic
	lda inWave
	beq GoToSkipWaveLogic
	lda missileStartY
	cmp #$FF
	bne ButtonNotPressed
	lda missileCount
	beq ButtonNotPressed
	;Need to fire a missile!
	sed
	sec
	sbc #1 ;Subtract one from the current missile count
	cld
	sta missileCount
	lda #BRIGHTNESS_COUNTER_SHOOT
	sta brightnessCounter
	tay
	lda ColorLookupExplosion,y
	sta backgroundColor
	lda playerXPos
	clc
	adc #4
	jsr CalculateFineCoarse
	sta missileFineCoarse
	lda #MISSILE_DEPLOY_TOP_HEIGHT
	sta missileStartY
	lda #MISSILE_DEPLOY_BOTTOM_HEIGHT
	sta missileEndY

	ldx #2
	jsr InitSoundEffect

	jmp DecreaseExplosionMissileBrightness

GoToSkipWaveLogic:
	jmp SkipWaveLogic

ButtonNotPressed:
	lda inWave
	bpl GoToSkipWaveLogic

;Check collisions here
CheckCollision:
	lda CXM0P
	bpl NoCollision
	;Missile hit an enemy!
	ldx #0
FindCollidedEnemy:
	lda enemyYPositions,x
	clc
	adc #10
	cmp missileStartY
	bcs FoundEnemy
	inx
	jmp FindCollidedEnemy
FoundEnemy:
	stx tempDigit
	lda #enemyXPositions
	sta genericPointer
	ldy tempDigit
	jsr RemoveAtY

	lda #enemyYPositions
	sta genericPointer
	ldy tempDigit
	jsr RemoveAtY

	dec enemyCount

	lda #$ff
	sta missileStartY
	sta missileEndY

	lda #$00
	sta scoreModifierLo
	lda #$01
	sta scoreModifierHi
	jsr AddToScore

	;Play hit sound effect
	ldx #3
	jsr InitSoundEffect

NoCollision:

DecreaseExplosionMissileBrightness:
	lda inTransition
	bne GoToSkipWaveLogic
	lda inWave
	bpl GoToSkipWaveLogic
	lda brightnessCounter
	beq CheckIfWaveEnded
	ldy brightnessCounter
	dey
	sty brightnessCounter
	lda ColorLookupExplosion,y
	sta backgroundColor

CheckIfWaveEnded:
	lda enemyCount
	ora enemiesLeftInWave
	beq EndWave ;There are no enemies left in this wave, and no enemies currently on screen, so the wave is over.
WaveManagement:
	dec enemySpawnTimer
EnemySpawnCheck:
	lda enemiesLeftInWave
	beq UpdateEnemyPositions
	lda enemySpawnTimer
	beq AttemptEnemySpawn
	lda brightnessCounter
	bne UpdateEnemyPositions
	lda enemyCount
	bne UpdateEnemyPositions
AttemptEnemySpawn:
	cmp #MAX_ENEMY_COUNT
	bcs GoToSkipWaveLogic
SpawnEnemy:
	inc enemyCount

	;Calculate x position
	lda rand8
	and #$7f
	clc
	adc #3
	ldx #enemyXPositions
	stx genericPointer
	jsr InsertAtFront

	;Insert y position
	lda #1
	ldx #enemyYPositions
	stx genericPointer
	jsr InsertAtFront

	lda #ENEMY_SPAWN_INTERVAL
	sta enemySpawnTimer
	dec enemiesLeftInWave
	jmp UpdateEnemyPositions
EndWave:
	sta inWave
	sta brightnessCounter
	lda #2
	sta inTransition
	jmp UpdateEnemyPositions

GoToSkipWaveLogic2:
	jmp SkipWaveLogic

UpdateEnemyPositions:
	;Load each enemy's y position, and increase it by the appropriate value.
	lda enemyCount
	beq GoToSkipWaveLogic2
	ldx #0
	lda enemySpeed
	tay
	bpl UpdateEnemyPosition
	lda everyOtherFrame
	eor #$80
	sta everyOtherFrame
	bpl CalculateEnemyFineCoarsePositions
	lda enemySpeed
	and #$0f
	tay
UpdateEnemyPosition:
	stx enemyIndex
	tya
	clc
	adc enemyYPositions,x
	cmp cityDelay
	bcc SkipDeletion
	lda enemyCount
	sbc #1
	sta enemyCount
	lda #$FF
	sta enemyYPositions,x

	;Queue up city breaking sound effect?
	ldx #4
	jsr InitSoundEffect

	ldx cityLevel
	beq CalculateEnemyFineCoarsePositions
	dex
	stx cityLevel

	jmp CalculateEnemyFineCoarsePositions
SkipDeletion:
	sta enemyYPositions,x
	cpx enemyCount
	bcs	CalculateEnemyFineCoarsePositions
	inx
	jmp UpdateEnemyPosition
CalculateEnemyFineCoarsePositions:
	;Load each enemies x position, and calculate the corresponding FineCoarse value
	lda enemyCount
	beq SkipWaveLogic
	ldx #0
CalculateFineCoarsePosition:
	cpx enemyCount
	bcs	UpdateRangefinder
	stx enemyIndex
	lda enemyXPositions,x
	jsr CalculateFineCoarse
	ldx enemyIndex
	sta enemyFineCoarsePositions,x
	inx
	jmp CalculateFineCoarsePosition

UpdateRangefinder:
	dex
	lda enemyXPositions,x
	sec
	sbc playerXPos
	bpl SkipNegation
	eor #$FF
	clc
	adc #1
SkipNegation:
	cmp #$40
	bcc DontClamp
	lda #$3F
DontClamp:
	and #$3F
	lsr
	lsr
	tay
	lda RangefinderFrequency,y
	sta AUDF1
	
	lda enemyYPositions,x
	and #$60
	jsr FiveLsr
	tay
	lda RangefinderIntervals,y
	inc rangefinderTimer
	cmp rangefinderTimer
	bcs SkipToggleRangefinder
	lda #0
	sta rangefinderTimer
	lda rangefinderVolume
	eor #RANGEFINDER_VOLUME
	sta rangefinderVolume
SkipToggleRangefinder:
	lda rangefinderVolume
	sta AUDV1
	;Set volume accordingly
SkipWaveLogic:
UpdateMissilePosition:
	lda missileStartY
	cmp #$FF
	beq CalculateScorePointers
	clc
	adc #MISSILE_TRAVEL_SPEED
	cmp #MISSILE_OFFSCREEN_CONSTANT
	bcs DestroyMissile
	sta missileStartY
	lda missileEndY
	clc
	adc #MISSILE_TRAVEL_SPEED
	sta missileEndY
	jmp CalculateScorePointers
DestroyMissile:
	lda #$FF
	sta missileStartY
	sta missileEndY

CalculateScorePointers:
	ldx scoreLo ;Load the low two digits of the player score.
	lda #scorePointer1
	sta genericPointer
	jsr CalculateDigitGraphicsPointer
	ldx scoreMd ;Load the middle two digits of the player's score.
	lda #scorePointer3
	sta genericPointer
	jsr CalculateDigitGraphicsPointer
	ldx scoreHi ;Load the two highest digits of the player's score.
	lda #scorePointer5
	sta genericPointer
	jsr CalculateDigitGraphicsPointer
CalculateRocketCountPointers:
	ldx missileCount
	lda #missileCountPointer1
	sta genericPointer
	jsr CalculateDigitGraphicsPointer
CalculateCityscapePointers:
	ldx cityLevel
	lda CityscapeGraphicsLookupTable,x
	sta cityscape0Pointer
	sec
	adc CityscapeHeightLookupTable,x
	sta cityscape1Pointer
	sec
	adc CityscapeHeightLookupTable,x
	sta cityscape2Pointer

CleanupEnemyArrays:
	;Fill all unused spots in enemyArrays with $ff
	ldx enemyCount
	cpx #MAX_ENEMY_COUNT
	bcs WaitForOverscanTimer
	lda #$ff
CleanupLoop:
	sta enemyXPositions,x
	sta enemyYPositions,x
	inx
	cpx #MAX_ENEMY_COUNT
	bcc CleanupLoop

WaitForOverscanTimer:
	lda INTIM
	bne WaitForOverscanTimer

	sta WSYNC
	jmp StartOfFrame

InitSoundEffect: ;Must only be called when X contains the id of the sound to play (0 indicates no sound).
	cpx currentSound
	bcc ReturnFromInitSoundEffect ;Just return, the incoming sound is not higher priority than the currently playing one.
	;Need to initialize new sound effect!
	stx currentSound
	lda SoundPointers,x
	sta currentSoundPointer
	lda SoundLengths,x
	sta soundOffset
	lda SoundEffectWaveforms,x
	sta AUDC0
ReturnFromInitSoundEffect:
	rts

AddToScore:
	sed
	lda scoreLo
	clc
	adc scoreModifierLo
	sta scoreLo
	lda scoreMd
	adc scoreModifierHi
	sta scoreMd
	lda #0
	adc scoreHi
	bcs MaxOut
	sta scoreHi
	;Check to reward extra life
	cmp extraLifeThreshold
	bcc NoExtraLife
	lda extraLifeThreshold
	clc
	adc #$01
	sta extraLifeThreshold
	ldx cityLevel
	cpx #4
	bcs NoExtraLife ;Don't increase the cityLevel beyond fully repaired!
	inx
	stx cityLevel
	ldx #5
	jsr InitSoundEffect ;Play city repaired sound
NoExtraLife:
	cld
	rts
MaxOut:
	lda #0
	sta inGame
	lda #$99
	sta scoreLo
	sta scoreMd
	sta scoreHi
	rts

CalculateFineCoarse: ;Must be called when A contains the x value to convert to FineCoarse, with X and Y containing nonimportant values
	tay
	and #$0F
	tax ;X now contains the low byte of the x value, which will become the offset into the FinePosLookupTable
	tya
	jsr FourLsr ;Extract the coarse position from the high byte of the x value
	and #$0f
	clc
	adc #1 ;Now A contains the coarse position
	ora FinePosLookupTable,x ;Now A contains the final FineCoarse value for the given x value
	rts

CalculateDigitGraphicsPointer: ;Must only be called when X contains the byte (in decimal mode) to be encoded, and genericPointer has been set.
	txa
	and #$0F ;Get just the low digit of the number of available missiles.
	tay
	lda DigitLookupTable,y
	ldy #0
	sta (genericPointer),y
	txa
	and #$F0 ;Get just the high digit of the number of available missiles.
	jsr FourLsr
	tay
	lda DigitLookupTable,y
	dec genericPointer
	dec genericPointer
	ldy #0
	sta (genericPointer),y ;This saves one byte since CalculateDigitGraphicsPointer no longer needs an rts.
FiveLsr:
	lsr
FourLsr:
	lsr
	lsr
	lsr
	lsr
SpinWheels: ;This function exists solely to waste 12 cycles-- takes up less code than 6 nops over and over again.
	rts

Random:
	lda rand8
	lsr
	bcc NoEOR
	eor #$B4
NoEOR:
	sta rand8
	rts

InsertAtFront: ;Uses the genericPointer variable to place the value in A at the front of the designated array, moving other values up
	ldy #0
	ldx enemyCount
	cpx #MAX_ENEMY_COUNT
	bcs ReturnFromInsert
MoveUp:
	pha ;This is the value that will be placed at this position
	lda (genericPointer),y ;Save the previous value in A
	tax
	pla ;Recover the new value
	sta (genericPointer),y ;Store the new value at this postion
	txa
	iny
	cpy enemyCount
	bcc MoveUp
ReturnFromInsert:
	rts

RemoveAtY: ;Uses the genericPointer variable to destroy the value at the Yth position in the designated array, moving the appropriate values down
	lda enemyCount
	beq ReturnFromInsert ;Just return
	cpy enemyCount
	bcs ReturnFromInsert
RemovalLoop:
	;Need to copy the space above this, and put it into this space
	iny
	cpy enemyCount
	bcs ReturnFromInsert
	lda (genericPointer),y
	tax
	dey
	sta (genericPointer),y
	iny
	jmp RemovalLoop

SoundEffectWaveforms:
	.byte $00
	.byte $0c ;Scoring
	.byte $03 ;Fire
	.byte $08 ;Hit
	.byte $0f ;City Damaged
	.byte $0c ;City Repaired

ScoringSound:
	.byte $00
	.byte $1f
HitSound:
	.byte $00
	.byte $0f
	.byte $0f
FireSound:
	.byte $00
	.byte $02
	.byte $04
	.byte $05
CityDamagedSound:
	.byte $00
	.byte $09
	.byte $09
	.byte $09
CityRepairedSound:
	.byte 0
	.byte 13
	.byte 15
	.byte 18
	.byte 15
	.byte 18
	.byte 20
	.byte 18
	.byte 20
	.byte 26

RangefinderFrequency:
	.byte $09 ;Formerly $10
	.byte $11
	.byte $12
	.byte $13
	.byte $14
	.byte $15
	.byte $16
	.byte $17
	.byte $18
	.byte $19
	.byte $1A
	.byte $1B
	.byte $1C
	.byte $1D
	.byte $1E
	.byte $1F

FinePosLookupTable:
	.byte $70
	.byte $60
	.byte $50
	.byte $40
	.byte $30
	.byte $20
	.byte $10
	.byte $00
	.byte $F0
	.byte $E0
	.byte $D0
	.byte $C0
	.byte $B0
	.byte $A0
	.byte $90
	.byte $80

DigitLookupTable:
	.byte (Digit0Graphics & $FF)
	.byte (Digit1Graphics & $FF)
	.byte (Digit2Graphics & $FF)
	.byte (Digit3Graphics & $FF)
	.byte (Digit4Graphics & $FF)
	.byte (Digit5Graphics & $FF)
	.byte (Digit6Graphics & $FF)
	.byte (Digit7Graphics & $FF)
	.byte (Digit8Graphics & $FF)
	.byte (Digit9Graphics & $FF)

CityscapeGraphicsLookupTable:
	.byte (Destruction4 & $FF) 
	.byte (Destruction3 & $FF)
	.byte (Destruction2 & $FF)
	.byte (Destruction1 & $FF)
	.byte (Destruction0 & $FF)

CityscapeHeightLookupTable:
	.byte 3
	.byte 5
	.byte 7
	.byte 10
	.byte 11

CityDelayLookupTable:
	.byte 144
	.byte 140
	.byte 136
	.byte 130
	.byte 128

EnemySpeedPerWave:
	.byte $81 ;5 enemies High byte of 8 indicates that this speed is added every other frame
	.byte $81 ;6
	.byte $81 ;7
	.byte $01 ;8
	.byte $01 ;9
	.byte $01 ;10
	.byte $83 ;11
	.byte $83 ;12
	.byte $83 ;13
	.byte $02 ;14
	.byte $02 ;15
	.byte $02 ;16
	.byte $03 ;17
	.byte $03 ;18
	.byte $04 ;19

AmmunitionPerWave: ;This will need to be updated later, these values will actually be interpreted as how they appear in decimal
	.byte #$20
	.byte #$20
	.byte #$20
	.byte #$30
	.byte #$30
	.byte #$30
	.byte #$40
	.byte #$40
	.byte #$40
	.byte #$50
	.byte #$50
	.byte #$50
	.byte #$60
	.byte #$60
	.byte #$60

ColorLookupTransition:
	.byte $00
	.byte $80
	.byte $82
	.byte $84
	.byte $86
	.byte $88
	.byte $8A
	.byte $8C

ColorLookupExplosion:
	.byte $00
	.byte $20
	.byte $22
	.byte $24
	.byte $26
	.byte $28
	.byte $2A
	.byte $2A
	.byte $2A

	ORG $FF00 ;Contains all graphics data

EnemyGraphics
	.byte $FF
	.byte $FF
	.byte $FF
	.byte $81
	.byte $81
	.byte $81
	.byte $81
	.byte $FF
	.byte $FF
	.byte $FF

;Cityscape graphics data
Destruction0:
	.byte $F0
	.byte $F0
	.byte $F0
	.byte $F0
	.byte $F0
	.byte $F0
	.byte $F0
	.byte $F0
	.byte $30
	.byte $30
	.byte $30
	.byte $30

	.byte $6E
	.byte $6E
	.byte $6E
	.byte $6E
	.byte $6E
	.byte $66
	.byte $66
	.byte $66
	.byte $64
	.byte $64
	.byte $04
	.byte $00

	.byte $77
	.byte $77
	.byte $77
	.byte $77
	.byte $77
	.byte $77
	.byte $77
	.byte $77
	.byte $77
	.byte $27
	.byte $27
	.byte $20

Destruction1:
	.byte $F0
	.byte $F0
	.byte $F0
	.byte $F0
	.byte $F0
	.byte $F0
	.byte $B0
	.byte $10
	.byte $10
	.byte $10
	.byte $10

	.byte $6E
	.byte $6E
	.byte $6E
	.byte $6E
	.byte $6E
	.byte $66
	.byte $66
	.byte $46
	.byte $44
	.byte $04
	.byte $00

	.byte $77
	.byte $77
	.byte $77
	.byte $77
	.byte $77
	.byte $77
	.byte $77
	.byte $37
	.byte $17
	.byte $06
	.byte $04

Destruction2:
	.byte $F0
	.byte $F0
	.byte $F0
	.byte $F0
	.byte $F0
	.byte $F0
	.byte $30
	.byte $10

	.byte $6E
	.byte $6E
	.byte $6E
	.byte $6E
	.byte $6E
	.byte $42
	.byte $00
	.byte $00

	.byte $77
	.byte $77
	.byte $77
	.byte $77
	.byte $77
	.byte $37
	.byte $03
	.byte $03

Destruction3:
	.byte $F0
	.byte $F0
	.byte $F0
	.byte $F0
	.byte $20
	.byte $00

	.byte $6E
	.byte $6E
	.byte $6E
	.byte $4C
	.byte $04
	.byte $00

	.byte $77
	.byte $77
	.byte $77
	.byte $67
	.byte $01
	.byte $01

Destruction4:
	.byte $F0
	.byte $70
	.byte $20
	.byte $00

	.byte $6E
	.byte $6C
	.byte $40
	.byte $00

	.byte $77
	.byte $65
	.byte $41
	.byte $01

Digit0Graphics
	.byte $00
	.byte $1C
	.byte $22
	.byte $22
	.byte $32
	.byte $2A
	.byte $26
	.byte $22
	.byte $22

Digit1Graphics
	.byte $1C ;Highest byte of Digit0
	.byte $08
	.byte $08
	.byte $08
	.byte $08
	.byte $08
	.byte $08
	.byte $08
	.byte $18

Digit2Graphics
	.byte $08
	.byte $3E
	.byte $20
	.byte $30
	.byte $18
	.byte $0C
	.byte $06
	.byte $02
	.byte $32

Digit3Graphics
	.byte $1C
	.byte $1C
	.byte $32
	.byte $02
	.byte $02
	.byte $0C
	.byte $02
	.byte $02
	.byte $32

Digit4Graphics
	.byte $1C
	.byte $04
	.byte $04
	.byte $04
	.byte $04
	.byte $3E
	.byte $24
	.byte $24
	.byte $24

Digit5Graphics
	.byte $24
	.byte $1C
	.byte $32
	.byte $02
	.byte $02
	.byte $1C
	.byte $20
	.byte $20
	.byte $20

Digit6Graphics
	.byte $3E
	.byte $1C
	.byte $22
	.byte $22
	.byte $22
	.byte $3C
	.byte $20
	.byte $20
	.byte $22

Digit7Graphics
	.byte $1C
	.byte $08
	.byte $08
	.byte $08
	.byte $08
	.byte $04
	.byte $04
	.byte $04
	.byte $02

Digit8Graphics
	.byte $3E
	.byte $1C
	.byte $22
	.byte $22
	.byte $22
	.byte $1C
	.byte $22
	.byte $22
	.byte $22

Digit9Graphics
	.byte $1C
	.byte $1C
	.byte $22
	.byte $02
	.byte $02
	.byte $1E
	.byte $22
	.byte $22
	.byte $22

	.byte $1C

PlayerGraphics:
	.byte $FE
	.byte $FE
	.byte $FE
	.byte $7C
	.byte $7C
	.byte $7C
	.byte $6C
	.byte $28
	.byte $28
	.byte $28

RangefinderIntervals:
	.byte 8
	.byte 3
	.byte 2
SoundPointers:
	.byte $0
	.byte (ScoringSound & $FF)
	.byte (FireSound & $FF)
	.byte (HitSound & $FF)
	.byte (CityDamagedSound & $FF)
	.byte (CityRepairedSound & $FF)
SoundLengths:
	.byte 0
	.byte 2
	.byte 4
	.byte 3
	.byte 4
	.byte 10

	ORG $FFFA

	.word Reset
	.word Reset
	.word Reset

END
