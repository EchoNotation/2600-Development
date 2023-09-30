	;BANK 2 - CONTAINS LOGIC AND DATA USED FOR THE RENDERING OF ENEMIES IN BATTLE

	ORG $E000
	RORG $F000

EReset:
	nop $1FF9 ;Go to bank 3, the correct startup bank

;This function needs to be in a location where the branches are never in danger of crossing a page boundary.
ECheckDamageTarget: SUBROUTINE ;Determines whether or not this enemy needs to do a damage flash effect. Takes 32 cycles.
	lda temp3 ;3
	clc ;2
	adc #4 ;2
	cmp startingCursorIndexAndTargetID ;3
	beq .EIsTarget ;2/3
.ENotTarget:
	lda #$00 ;2
	beq .ESaveAndReturn ;3
.EIsTarget:
	nop ;Lose 2 cycles
	lda #$FF ;2
.ESaveAndReturn:
	sta temp2 ;3
	rts

EEnemyAI: ;This table must be in order by enemy ID
WolfAI:
	.byte %00000000 ;Attack frontline
	.byte %00000000
	.byte %00000000
	.byte %01000000 ;Attack any
DruidAI:
	.byte %11000011 ;Cast BLIZRD
	.byte %11001111 ;Cast WITHER any
	.byte %11001111 ;Cast WITHER any
	.byte %11101001 ;Cast HEAL self
ShroomAI:
	.byte %11001101 ;Cast BLIGHT any
	.byte %11001101 ;
	.byte %11001101 ;
	.byte %10000010 ;Cast SLEEP frontline
SquireAI:
	.byte %00000000 ;Attack frontline
	.byte %00000000 ; 
	.byte %00000100 ;Parry
	.byte %00000100 ;
ArcherAI:
	.byte %01000000 ;Attack any
	.byte %01000000 ;
	.byte %01000000 ;
	.byte %11001011 ;Cast VOLLEY
PriestAI:
	.byte %11001001 ;Cast HEAL any
	.byte %11001001 ;
	.byte %10001010 ;Cast SMITE frontline
	.byte %10000001 ;Cast FIRE any
GiftAI:
	.byte %00000101 ;Special
	.byte %00000101 ;
	.byte %00000101 ;
	.byte %00000101 ;
SwordAI:
	.byte %00000000 ;Attack frontline
	.byte %01000000 ;Attack any
	.byte %00000100 ;Parry
	.byte %00000100 ;
ShieldAI:
	.byte %00000000 ;Attack frontline
	.byte %01000011 ;Guard any
	.byte %01000011 ;Guard any
	.byte %11000110 ;Cast SHIELD any
ZombieAI:
	.byte %00000000 ;Attack frontline
	.byte %00000000 ;
	.byte %00000000 ;
	.byte %01000000 ;Attack any
SkltonAI:
	.byte %01000000 ;Attack any
	.byte %01000000 ;
	.byte %01000000 ;
	.byte %11001011 ;Cast VOLLEY
MageAI:
	.byte %10000010 ;Cast SLEEP frontline
	.byte %11000001 ;Cast FIRE any
	.byte %11000011 ;Cast BLIZRD
	.byte %11100110 ;Cast SHIELD self
GoopAI:
	.byte %00000000 ;Attack frontline
	.byte %00000000 ;
	.byte %00000000 ;
	.byte %00000000 ;
WarlokAI:
	.byte %10000100 ;Cast DRAIN frontline
	.byte %11000101 ;Cast THUNDR 
	.byte %11001100 ;Cast SHARP
	.byte %10001101 ;Cast BLIGHT frontline
ImpAI:
	.byte %00000000 ;Attack frontline
	.byte %01000000 ;Attack any
	.byte %11001000 ;Cast CHAOS
	.byte %11001000 ;
WispAI:
	.byte %11001001 ;Cast HEAL any
	.byte %11001001 ;
	.byte %11001001 ;
	.byte %11001110 ;Cast TRIAGE
RedOrbAI:
	.byte %11000001 ;Cast FIRE any
	.byte %11000001 ;
	.byte %11000001 ;
	.byte %10000111 ;Cast METEOR frontline
BluOrbAI:
	.byte %11001001 ;Cast HEAL any
	.byte %11001001 ;
	.byte %11001110 ;Cast TRIAGE
	.byte %11001110 ;Cast TRIAGE
GrnOrbAI:
	.byte %10000010 ;Cast SLEEP frontline
	.byte %11001101 ;Cast BLIGHT any
	.byte %11001000 ;Cast CHAOS
	.byte %11000110 ;Cast SHIELD any
GldOrbAI:
	.byte %10001010 ;Cast SMITE frontline
	.byte %10001010 ;
	.byte %11000101 ;Cast THUNDR
	.byte %11000101 ;
BearAI:
	.byte %00000000 ;Attack frontline
	.byte %00000000 ;
	.byte %01000000 ;Attack any
	.byte %01000011 ;Guard any
UnicrnAI:
	.byte %00000000 ;Attack frontline
	.byte %00000000 ;
	.byte %11001001 ;Cast HEAL any
	.byte %11001001 ;
VolcioAI:
	.byte %11000001 ;Cast FIRE any
	.byte %11000001 ;
	.byte %11000001 ;
	.byte %00000000 ;Attack frontline
GlaciaAI:
	.byte %11000011 ;Cast BLIZRD
	.byte %11000011 ;
	.byte %00000000 ;Attack frontline
	.byte %01000000 ;Attack any
GrgoylAI:
	.byte %00000000 ;Attack frontline
	.byte %00000000 ;
	.byte %01000000 ;Attack any
	.byte %01000000 ;
MimicAI:
	.byte %00000000 ;Attack frontline
	.byte %00000000 ;
	.byte %01000000 ;Attack any
	.byte %10000100 ;Cast DRAIN frontline
JesterAI:
	.byte %01000000 ;Attack any
	.byte %01000000 ;
	.byte %00000101 ;Special
	.byte %00000101 ;
ArmorAI:
	.byte %11101001 ;Cast HEAL self
	.byte %00000101 ;Special
	.byte %00000101 ;
	.byte %00000101 ;
SpiderAI:
	.byte %00000000 ;Attack frontline
	.byte %00000000 ;
	.byte %01000000 ;Attack any
	.byte %11001101 ;Cast BLIGHT any
SlimeAI:
	.byte %00000101 ;Special
	.byte %00000101 ;
	.byte %00000101 ;
	.byte %00000101 ;
LichAI:
	.byte %11001111 ;Cast WITHER any
	.byte %11101001 ;Cast HEAL self
	.byte %00000101 ;Special
	.byte %00000101 ;
ShfflrAI:
	.byte %00100000 ;Attack backline
	.byte %01000000 ;Attack any
	.byte %11001011 ;Cast VOLLEY
	.byte %11001011 ;
ShmblrAI:
	.byte %00000000 ;Attack frontline
	.byte %00000000 ;
	.byte %01000000 ;Attack any
	.byte %10001101 ;Cast BLIGHT frontline
TrophyAI:
	.byte %00000000 ;
	.byte %00000000 ;
	.byte %00000000 ;
	.byte %00000000 ;
ThicktAI:
	.byte %01000000 ;Attack any
	.byte %01000000 ;
	.byte %11001101 ;Cast BLIGHT any
	.byte %10000100 ;Cast DRAIN frontline
OozeAI:
	.byte %00000101 ;Special
	.byte %00000101 ;
	.byte %00000101 ;
	.byte %00000101 ;
HorrorAI:
	.byte %01000000 ;Attack any
	.byte %01000000 ;
	.byte %01000000 ;
	.byte %01000000 ;
CampfireAI:
	.byte %00000000 ;
	.byte %00000000 ;
	.byte %00000000 ;
	.byte %00000000 ;

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
	cpx #1
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
	cmp #$14 ;First medium enemy ID
	bcc EPrepSmallEnemy
	cmp #$22 ;First large enemy ID
	bcc EPrepMediumEnemy
	jmp EPrepLargeEnemy
EPrepSmallEnemy:
	sta returnValue ;enemyID
	tax
	ldy #$FD
	sty tempPointer1+1
	asl
	asl
	asl
	asl
	clc
	adc #$80
	sta tempPointer1
	cpx #$8
	bcc .ESmallOnFirstPage
	inc tempPointer1+1
.ESmallOnFirstPage:
	clc
	adc #8
	sta temp5
	sta WSYNC
	lda tempPointer1+1
	sta tempPointer5

	jsr EDrawSmallEnemy
	inc temp3
	lda temp1
	sec
	sbc #19
	sta temp1
	jmp EEnemyRenderingLoop
EPrepMediumEnemy:
	sec
	sbc #20
	sta returnValue ;modified enemyID

	lda #$FA ;Starting page for medium enemy graphics
	sta tempPointer1+1
	lda returnValue
	lsr
	lsr ;4 medium enemies per page
	clc
	adc tempPointer1+1
	sta tempPointer1+1
	sta tempPointer2+1
	sta tempPointer5
	sta tempPointer6

	lda returnValue
	jsr E6Asl
	sta tempPointer2
	clc
	adc #16
	sta tempPointer1
	adc #16
	sta temp6
	adc #16
	sta temp5

	;Insert trophy "shimmer" effect here
	lda currentEffect
	cmp #$6
	bne .ENoShimmer
	lda #(TrophyColors & $FF)
	clc
	adc effectCounter
	sta temp6
	sta temp5
.ENoShimmer:
	sta WSYNC
	lda #$40
	sta HMP0
	lda #$50
	sta HMP1
	sta WSYNC
	jsr EDrawMediumEnemy
	inc temp3
	inc temp3
	lda temp1
	sec
	sbc #38
	sta temp1
	jmp EEnemyRenderingLoop
EPrepLargeEnemy:
	;X currently contains the enemyID
	;graphics are stored in temp4, tempPointer3, tempPointer2, tempPointer1 order low to high addresses
	;color are stored ub temp6, temp5 order low to high addresses
	sta WSYNC
	sec
	sbc #34 ;Currently, large enemies are at the end of the enemyID list.
	sta returnValue

	lda #$F7 ;Starting page for large enemy graphics
	sta tempPointer1+1
	lda returnValue
	cmp #2
	bcc .ESkipLargePointerIncrement
	inc tempPointer1+1
.ESkipLargePointerIncrement:
	and #$1
	jsr E7Asl
	sta temp4
	clc
	adc #32
	sta tempPointer3
	adc #32
	sta tempPointer2
	adc #32
	sta tempPointer1
	lda tempPointer1+1
	sta tempPointer2+1
	sta tempPointer3+1
	sta tempPointer4

	lda #$F9 ;High byte for the color data pointers
	sta tempPointer5
	sta tempPointer6
	lda returnValue ;modified enemyID
	jsr E6Asl
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

E7Asl:
	asl
E6Asl:
	asl
E5Asl:
	asl
E4Asl:
	asl
	asl
	asl
	asl
	rts

EDrawSmallEnemy: SUBROUTINE ;This subroutine is used for drawing enemies that are 8x8 pixels i size. Graphical information is interpreted from tempPointer1, and color information is interpreted from tempPointer5
	jsr ECheckDamageTarget ;Takes exactly 32 cycles to run
	sta RESP0
	ldy #8
	lda #0
	sta NUSIZ0
	sta NUSIZ1
	lda #$00
	sta HMP0
	sta HMOVE
.EDrawSmallEnemyLoop:
	dey
	sta WSYNC
	bmi .EDoneDrawingSmallEnemy
	lda (tempPointer1),y
	sta GRP0

	lda currentEffect
	and temp2
	cmp #$5
	bne .ENormalDrawing
	lda effectCounter
	and #$04
	bne .ENormalDrawing
.EColorDrawing:
	lda mazeAndEffectColor
	sta COLUP0
	jmp .EExitLoop
.ENormalDrawing:
	lda (temp5),y
	sta COLUP0
.EExitLoop:
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
	jsr ECheckDamageTarget ;Takes exactly 32 cycles
	nop
	nop
	cmp temp1
	sta RESP0
	sta RESP1
	jsr ESpinWheels
	ldy #16 ;Height of the enemy
	lda #0 ;No duplication
	sta HMOVE ;Need to make this happen on cycle 73 exactly...
	sta NUSIZ0
	sta NUSIZ1

	lda currentEffect
	and temp2
	cmp #$5
	bne .EDrawMediumEnemyLoop
	lda effectCounter
	and #$04
	bne .EDrawMediumEnemyLoop
.EDrawMediumEnemyLoopColorful:
	dey
	sta WSYNC
	bmi .EDoneDrawingMediumEnemy 
	lda (tempPointer1),y
	sta GRP0
	lda (tempPointer2),y
	sta GRP1
	lda mazeAndEffectColor
	sta COLUP0
	sta COLUP1
	sta WSYNC
	jmp .EDrawMediumEnemyLoopColorful
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
	lda #$20
	sta HMP0 ;Large enemies are actually 1 pixel left of center for timing reasons
	lda #$30
	sta HMP1

	jsr ESpinWheels
	cmp temp1

	ldy #32
	lda #1 ;Two copies close
	sta NUSIZ0
	sta NUSIZ1

	sta RESP0
	sta RESP1
	jsr ESpinWheels
	nop
	nop
	sta HMOVE ;Needs to hit on exactly cycle 67
	
	;Prepare enemy for damage effect
	jsr ECheckDamageTarget ;Takes 32 cycles

	lda currentEffect
	and temp2 ;Controls whether or not this is the correct enemy to affect.
	cmp #$05 ;Enemy damage flash
	bne .EDrawLargeEnemyLoop
	lda effectCounter
	and #$04
	beq .EDrawLargeEnemyLoopColorful
.EDrawLargeEnemyLoop:
	lda #1
	sta charIndex
	dey
.EDrawLargeEnemyInnerLoop:
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
	dec charIndex
	bmi .EDrawLargeEnemyLoop
	bpl .EDrawLargeEnemyInnerLoop
.EDrawLargeEnemyLoopColorful:
	lda #1
	sta charIndex
	dey
.EDrawLargeEnemyInnerLoopColorful:
	sta WSYNC
	bmi .EDoneDrawingLargeEnemy
	lda (tempPointer1),y
	sta GRP0
	lda (tempPointer2),y
	sta GRP1
	lda mazeAndEffectColor
	sta COLUP0
	sta COLUP1
	lda (tempPointer3),y
	tax
	lda (temp4),y
	nop
	nop
	nop
	stx GRP0
	sta GRP1
	dec charIndex
	bmi .EDrawLargeEnemyLoopColorful
	bpl .EDrawLargeEnemyInnerLoopColorful

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
	cpx #$8
	bcc .ESpecialEffect
.ENormalEffect:
	ldy mazeAndEffectColor
	dey
	lda effectCounter
	and #$3
	asl
	adc ESpellEffectBaseColors,y
	sta COLUBK
	lda #1
	sta temp2
	rts
.EEndEffect:
	lda currentEffect
	cmp #$6
	beq .EDontEndShimmer
	cmp #$7
	beq .EStartSpellEffect
	sty currentEffect
	sty effectCounter
	sty effectCountdown
	sty COLUBK
	lda #1
	sta temp2
	rts
.EDontEndShimmer:
	lda #2
	sta effectCounter
	lda #3
	sta effectCountdown
	lda #1
	sta temp2
	rts
.EHighlightEffect:
	lda #1
	sta effectCounter
	sta temp2
	rts
.ESpecialEffect:
	lda #1
	sta temp2
	rts
.EStartSpellEffect:
	ldy mazeAndEffectColor
	dey
	lda ESpellEffectLengths,y
	sta effectCounter
	lda #8
	sta currentEffect
	ldx #1
	stx effectCountdown
	nop
	nop
	nop
	stx temp2
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
	lda #0
	sta temp3
	cpx #$14
	bcc .ESmall
	cpx #$22
	bcc .EMedium
.ELarge:
	inc temp3
	inc temp3
.EMedium:
	inc temp3
.ESmall:
	inc temp3
	lda temp1
	cmp temp3
	bcs .EEnemyFits
.EEnemyDoesNotFit
	iny
	lda #1
	sta temp3
	lda (tempPointer1),y
	tax
.EEnemyFits:
	;X now contains the enemyID that will be added to the encounter next
	ldy temp2
	stx enemyID,y

	lda temp2
	clc
	adc temp3
	sta temp2

	lda temp1
	sec
	sbc temp3
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
	lda #(EStabsText >> 8 & $FF)
	sta tempPointer1+1
	txa
	sec
	sbc #4
	cmp #42
	bcc .ENoSubtractionNecessary
	sec
	sbc #42
	inc tempPointer1+1
.ENoSubtractionNecessary:
	sta tempPointer1
	ldy #5
.EAdditionLoop:
	clc
	adc tempPointer1
	dey
	bne .EAdditionLoop
	sta tempPointer1

	;Y should be 0 here.
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

ELoadEffect: SUBROUTINE ;Loads the effect of ID X.
	stx currentEffect
	cpx #7
	beq .ESpellDelayEffect
	lda EEffectLength,x
	sta effectCounter
	lda #1
	sta effectCountdown
	jmp EAfterLoadingEffect
.ESpellDelayEffect:
	ldy mazeAndEffectColor
	dey
	lda ESpellDelays,y
	sta effectCountdown
	lda #0
	sta effectCounter
	jmp EAfterLoadingEffect

ELoadEnemyAI: SUBROUTINE
	lda currentBattler
	and #$03
	tax
	lda enemyID,x
	asl
	asl
	clc ;this may not be necessary
	adc #(EEnemyAI & $FF)
	sta tempPointer1
	lda #(EEnemyAI >> 8 & $FF)
	sta tempPointer1+1
	jsr ERandom
	and #$03 ;Only 4 slots per enemy
	tay
	lda (tempPointer1),y
	jmp EAfterLoadingEnemyAI

	ORG $E500
	RORG $F500

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
ESlicesText:
	.byte #S
	.byte #L
	.byte #I
	.byte #C
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
ELearnsText:
	.byte #L
	.byte #E
	.byte #A
	.byte #R
	.byte #N
	.byte #S
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
EWastesText:
	.byte #W
	.byte #A
	.byte #S
	.byte #T
	.byte #E
	.byte #S
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
EWakesText:
	.byte #W
	.byte #A
	.byte #K
	.byte #E
	.byte #S
	.byte #EMPTY
EHasAText:
	.byte #H
	.byte #A
	.byte #S
	.byte #EMPTY
	.byte #A
	.byte #EMPTY
EShieldMessageText:
	.byte #S
	.byte #H
	.byte #I
	.byte #E
	.byte #L
	.byte #D
EPartyText:
	.byte #P
	.byte #A
	.byte #R
	.byte #T
	.byte #Y
	.byte #EMPTY
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
EClawsText:
	.byte #C
	.byte #L
	.byte #A
	.byte #W
	.byte #S
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
	.byte #E
EGuardsText:
	.byte #G
	.byte #U
	.byte #A
	.byte #R
	.byte #D
	.byte #S
EAttackText:
	.byte #A
	.byte #T
	.byte #T
	.byte #A
	.byte #C
	.byte #K
EFellText:
	.byte #F
	.byte #E
	.byte #L
	.byte #L
	.byte #EMPTY
	.byte #EMPTY
EAsleepText:
	.byte #A
	.byte #S
	.byte #L
	.byte #E
	.byte #E
	.byte #P
EIsText:
	.byte #I
	.byte #S
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
ECastleText:
	.byte #C
	.byte #A
	.byte #S
	.byte #T
	.byte #L
	.byte #E
EFadesText:
	.byte #F
	.byte #A
	.byte #D
	.byte #E
	.byte #S
	.byte #EMPTY
EExiledText:
	.byte #E
	.byte #X
	.byte #I
	.byte #L
	.byte #E
	.byte #D
EGameText:
	.byte #G
	.byte #A
	.byte #M
	.byte #E
	.byte #EMPTY
	.byte #EMPTY
EClearText:
	.byte #C
	.byte #L
	.byte #E
	.byte #A
	.byte #R
	.byte #EMPTY
EOverText:
	.byte #O
	.byte #V
	.byte #E
	.byte #R
	.byte #EMPTY
	.byte #EMPTY

	ORG $E600
	RORG $F600

ETheText:
	.byte #T
	.byte #H
	.byte #E
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
EIntoText:
	.byte #I
	.byte #N
	.byte #T
	.byte #O
	.byte #EMPTY
	.byte #EMPTY
EAbyssText:
	.byte #A
	.byte #B
	.byte #Y
	.byte #S
	.byte #S
	.byte #EMPTY
ECryptText:
	.byte #C
	.byte #R
	.byte #Y
	.byte #P
	.byte #T
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
EEmptyText:
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
	.byte #EMPTY
ENoText:
	.byte #N
	.byte #O
	.byte #EMPTY
	.byte #EMPTY
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
	.byte #EMPTY
EFormAText:
	.byte #F
	.byte #O
	.byte #R
	.byte #M
	.byte #EMPTY
	.byte #A
EWhipsText:
	.byte #W
	.byte #H
	.byte #I
	.byte #P
	.byte #S
	.byte #EMPTY
ETeamText:
	.byte #EMPTY
	.byte #T
	.byte #E
	.byte #A
	.byte #M
	.byte #EMPTY
EPlayText:
	.byte #EMPTY
	.byte #P
	.byte #L
	.byte #A
	.byte #Y
	.byte EMPTY
ESmitesText:
	.byte #S
	.byte #M
	.byte #I
	.byte #T
	.byte #E
	.byte #S
EShotAText:
	.byte #S
	.byte #H
	.byte #O
	.byte #T
	.byte #EMPTY
	.byte #A
EMiresText:
	.byte #M
	.byte #I
	.byte #R
	.byte #E
	.byte #S
	.byte #EMPTY

	ORG $E700 ;Contains the first 2 large enemies graphics data
	RORG $F700

ThicktGraphics:
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %01100000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %10000000
	.byte %01100000
	.byte %00000000
	.byte %00000000
	.byte %00110000
	.byte %11000000
	.byte %10000000
	.byte %01100000
	.byte %00010000
	.byte %10000000
	.byte %01000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
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
	.byte %00000000
	.byte %00100000
	.byte %01100000
	.byte %11010000
	.byte %10011100
	.byte %11000110
	.byte %11000011
	.byte %01000000
	.byte %01000000
	.byte %11000000
	.byte %10000000
	.byte %01110000
	.byte %00111100
	.byte %00000111
	.byte %00000001
	.byte %00000001
	.byte %00000011
	.byte %00001110
	.byte %00001000
	.byte %01111000
	.byte %11100000
	.byte %01100000
	.byte %00110000
	.byte %00011000
	.byte %00001100
	.byte %11111111
	.byte %11001011
	.byte %11100111
	.byte %11111111
	.byte %11011011
	.byte %10111101
	.byte %11111111
	.byte %11111111
	.byte %00000000
	.byte %00000000
	.byte %10000010
	.byte %01000100
	.byte %00100100
	.byte %11100011
	.byte %10111110
	.byte %00001000
	.byte %00001000
	.byte %10010000
	.byte %01000001
	.byte %00100010
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %10000001
	.byte %01000001
	.byte %00100010
	.byte %00111100
	.byte %11000111
	.byte %01000010
	.byte %01000010
	.byte %01000001
	.byte %00000000
	.byte %00000000
	.byte %00000001
	.byte %10000001
	.byte %10000001
	.byte %10000001
	.byte %10000001
	.byte %00000001
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000001
	.byte %01000001
	.byte %01100011
	.byte %01110010
	.byte %00110110
	.byte %00111110
	.byte %00001100
	.byte %00111000
	.byte %01110000
	.byte %01110000
	.byte %01000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000011
	.byte %00000110
	.byte %00001100
	.byte %00011000
	.byte %01111110
	.byte %11100111
	.byte %11001011
	.byte %11111111
	.byte %11011011
	.byte %10111101
	.byte %11111111
	.byte %01111110

OozeGraphics:
	.byte %11111000
	.byte %00011100
	.byte %00011100
	.byte %00001110
	.byte %00000111
	.byte %00000011
	.byte %00000011
	.byte %00000011
	.byte %00000011
	.byte %00000011
	.byte %01100011
	.byte %00000011
	.byte %00000110
	.byte %00000110
	.byte %00000110
	.byte %00000110
	.byte %00001110
	.byte %00001100
	.byte %11111100
	.byte %11110000
	.byte %01000000
	.byte %11111111
	.byte %11110000
	.byte %11000000
	.byte %10000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11111111
	.byte %00000000
	.byte %00000000
	.byte %11111000
	.byte %01110000
	.byte %10101000
	.byte %11111000
	.byte %11000000
	.byte %00000000
	.byte %00000000
	.byte %00000011
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11011000
	.byte %00000000
	.byte %00000001
	.byte %00000001
	.byte %00000011
	.byte %00000011
	.byte %00000011
	.byte %00000001
	.byte %00000111
	.byte %00011111
	.byte %00111100
	.byte %11110000
	.byte %11100000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %11111111
	.byte %00000000
	.byte %00000000
	.byte %10000001
	.byte %11000011
	.byte %11001001
	.byte %10000011
	.byte %00000001
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
	.byte %10000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00010000
	.byte %00001000
	.byte %10000100
	.byte %11100000
	.byte %11111000
	.byte %00111111
	.byte %00001111
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00111111
	.byte %00111000
	.byte %01101100
	.byte %11010111
	.byte %10000011
	.byte %11111110
	.byte %10000011
	.byte %10011111
	.byte %11110000
	.byte %01100000
	.byte %01100100
	.byte %01100100
	.byte %01110010
	.byte %01110000
	.byte %00110000
	.byte %00110000
	.byte %00110010
	.byte %00110001
	.byte %00111000
	.byte %00011100
	.byte %00011100
	.byte %00001110
	.byte %00001111
	.byte %00000111
	.byte %00000011
	.byte %00000001
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000

	ORG $E800 ;Contains the next two large enemies graphics data
	RORG $F800

LargeTestEnemyGraphics3:
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

	ORG $E900 ;Contains the large enemies' color data
	RORG $F900

ThicktColors:
	.byte $42
	.byte $42
	.byte $46
	.byte $42
	.byte $42
	.byte $c4
	.byte $c6
	.byte $42
	.byte $42
	.byte $42
	.byte $42
	.byte $46
	.byte $42
	.byte $42
	.byte $42
	.byte $46
	.byte $42
	.byte $42
	.byte $c4
	.byte $c4
	.byte $42
	.byte $42
	.byte $46
	.byte $42
	.byte $42
	.byte $52
	.byte $52
	.byte $52
	.byte $52
	.byte $52
	.byte $52
	.byte $42
	.byte $c2
	.byte $c2
	.byte $c4
	.byte $c6
	.byte $c4
	.byte $c4
	.byte $c4
	.byte $c4
	.byte $c4
	.byte $c4
	.byte $c6
	.byte $c4
	.byte $c2
	.byte $c6
	.byte $c6
	.byte $c4
	.byte $c4
	.byte $c4
	.byte $c4
	.byte $c6
	.byte $c4
	.byte $c6
	.byte $c2
	.byte $c4
	.byte $52
	.byte $6e
	.byte $6c
	.byte $6e
	.byte $6c
	.byte $6c
	.byte $6e
	.byte $52

OozeColors:
	.byte $b2
	.byte $c4
	.byte $b2
	.byte $d2
	.byte $b2
	.byte $c4
	.byte $c2
	.byte $d2
	.byte $b0
	.byte $c2
	.byte $d0
	.byte $c2
	.byte $d4
	.byte $c2
	.byte $c4
	.byte $c2
	.byte $d0
	.byte $c4
	.byte $c2
	.byte $8
	.byte $6
	.byte $f2
	.byte $c4
	.byte $c2
	.byte $c4
	.byte $d2
	.byte $c2
	.byte $d0
	.byte $c2
	.byte $c2
	.byte $c2
	.byte $c2
	.byte $d0
	.byte $b4
	.byte $d4
	.byte $c2
	.byte $a
	.byte $a
	.byte $a
	.byte $b2
	.byte $d0
	.byte $c2
	.byte $d0
	.byte $b2
	.byte $d2
	.byte $d4
	.byte $c2
	.byte $b4
	.byte $d2
	.byte $b2
	.byte $d0
	.byte $c2
	.byte $c4
	.byte $b4
	.byte $b2
	.byte $d2
	.byte $d0
	.byte $d4
	.byte $d0
	.byte $b2
	.byte $b4
	.byte $ba
	.byte $ba
	.byte $ba

LargeTestEnemyColors3:
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

	ORG $EA00 ;Contains medium enemy data
	RORG $FA00

BearGraphics:
	.byte %00000000
	.byte %00000000
	.byte %00010000
	.byte %01001000
	.byte %00101000
	.byte %11110000
	.byte %11100000
	.byte %00000000
	.byte %00000000
	.byte %11100000
	.byte %10100000
	.byte %01010000
	.byte %11110000
	.byte %11110000
	.byte %00000000
	.byte %00000000
	.byte %11110000
	.byte %11111000
	.byte %11111100
	.byte %11111110
	.byte %11111111
	.byte %11111111
	.byte %11111011
	.byte %11111000
	.byte %01111100
	.byte %00111111
	.byte %00111111
	.byte %01111111
	.byte %01111111
	.byte %01111011
	.byte %01111110
	.byte %00110000
BearColors:
	.byte $e
	.byte $f0
	.byte $c
	.byte $c
	.byte $c
	.byte $f0
	.byte $f0
	.byte $f0
	.byte $f2
	.byte $f2
	.byte $e
	.byte $e
	.byte $f0
	.byte $f0
	.byte $6
	.byte $6
	.byte $f2
	.byte $f2
	.byte $f2
	.byte $f2
	.byte $f2
	.byte $f2
	.byte $f2
	.byte $f2
	.byte $f2
	.byte $f2
	.byte $f2
	.byte $f2
	.byte $f2
	.byte $f2
	.byte $f2
	.byte $f2

UnicrnGraphics:
	.byte %00010000
	.byte %00010000
	.byte %10010000
	.byte %10100000
	.byte %11000000
	.byte %11000000
	.byte %11000000
	.byte %11000000
	.byte %11100000
	.byte %11110000
	.byte %11111110
	.byte %11111110
	.byte %11101000
	.byte %01110000
	.byte %00001000
	.byte %00000100
	.byte %10010010
	.byte %10001001
	.byte %01001000
	.byte %01010000
	.byte %01111111
	.byte %01111111
	.byte %01111111
	.byte %10111111
	.byte %00000001
	.byte %00000111
	.byte %00000011
	.byte %00000011
	.byte %00000001
	.byte %00000000
	.byte %00000000
	.byte %00000000
UnicrnColors:
	.byte $f4
	.byte $5e
	.byte $c
	.byte $5e
	.byte $5c
	.byte $c
	.byte $c
	.byte $5c
	.byte $5e
	.byte $c
	.byte $e
	.byte $5c
	.byte $5e
	.byte $e
	.byte $9c
	.byte $ae
	.byte $f4
	.byte $a
	.byte $5a
	.byte $a
	.byte $5c
	.byte $c
	.byte $e
	.byte $5e
	.byte $5e
	.byte $7a
	.byte $7c
	.byte $7c
	.byte $7e
	.byte $6a
	.byte $0
	.byte $42

FireDrakeGraphics:
	.byte %00000101
	.byte %11000011
	.byte %11110011
	.byte %01111011
	.byte %00111110
	.byte %00111100
	.byte %00011110
	.byte %11111111
	.byte %11100111
	.byte %00000111
	.byte %00001111
	.byte %11111110
	.byte %11011100
	.byte %01111000
	.byte %01001100
	.byte %00100100
	.byte %00111110
	.byte %01111111
	.byte %01100011
	.byte %01000000
	.byte %11100000
	.byte %01100000
	.byte %01000000
	.byte %00000001
	.byte %00000000
	.byte %00000001
	.byte %00011000
	.byte %01100110
	.byte %10010100
	.byte %00101000
	.byte %11010000
	.byte %00000000
FireDrakeColors:
	.byte $e
	.byte $42
	.byte $44
	.byte $42
	.byte $44
	.byte $42
	.byte $44
	.byte $42
	.byte $44
	.byte $42
	.byte $44
	.byte $44
	.byte $42
	.byte $44
	.byte $36
	.byte $36
	.byte $44
	.byte $42
	.byte $44
	.byte $42
	.byte $36
	.byte $36
	.byte $36
	.byte $e
	.byte $36
	.byte $e
	.byte $3a
	.byte $1a
	.byte $3a
	.byte $1a
	.byte $3a
	.byte $36

IceDrakeGraphics:
	.byte %11111100
	.byte %11111110
	.byte %10000110
	.byte %00000010
	.byte %00000111
	.byte %00000110
	.byte %00000010
	.byte %10000000
	.byte %00000000
	.byte %10000000
	.byte %00011000
	.byte %01100110
	.byte %00101001
	.byte %00010100
	.byte %00001011
	.byte %00000000
	.byte %10100000
	.byte %11000011
	.byte %11001111
	.byte %11011110
	.byte %01111100
	.byte %00111100
	.byte %01110000
	.byte %11101111
	.byte %11100111
	.byte %11100000
	.byte %11110000
	.byte %01111111
	.byte %00111011
	.byte %00011110
	.byte %00110010
	.byte %00100100
IceDrakeColors:
	.byte $86
	.byte $94
	.byte $86
	.byte $94
	.byte $ae
	.byte $ae
	.byte $ae
	.byte $e
	.byte $e
	.byte $e
	.byte $ae
	.byte $8e
	.byte $ae
	.byte $8e
	.byte $ae
	.byte $8e
	.byte $e
	.byte $94
	.byte $86
	.byte $94
	.byte $86
	.byte $94
	.byte $86
	.byte $94
	.byte $86
	.byte $94
	.byte $86
	.byte $86
	.byte $94
	.byte $86
	.byte $ae
	.byte $ae

	ORG $EB00 ;Contains medium enemy data
	RORG $FB00

GrgoylGraphics:
	.byte %11001110
	.byte %10001100
	.byte %10011100
	.byte %11111000
	.byte %11111000
	.byte %11110000
	.byte %11110001
	.byte %11111001
	.byte %11111110
	.byte %01110000
	.byte %01111000
	.byte %11111111
	.byte %11011111
	.byte %11011010
	.byte %10011100
	.byte %10100000
	.byte %00000001
	.byte %00000001
	.byte %00000001
	.byte %00011100
	.byte %00100110
	.byte %01000001
	.byte %00011000
	.byte %11111000
	.byte %11111000
	.byte %11111100
	.byte %11111100
	.byte %11111110
	.byte %01111111
	.byte %00011111
	.byte %00000111
	.byte %00000000
GrgoylColors:
	.byte $4
	.byte $6
	.byte $6
	.byte $6
	.byte $6
	.byte $4
	.byte $6
	.byte $6
	.byte $6
	.byte $6
	.byte $6
	.byte $6
	.byte $6
	.byte $6
	.byte $6
	.byte $6
	.byte $4
	.byte $6
	.byte $6
	.byte $2
	.byte $2
	.byte $2
	.byte $a
	.byte $a
	.byte $c
	.byte $a
	.byte $a
	.byte $c
	.byte $c
	.byte $a
	.byte $6
	.byte $8

MimicGraphics:
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11111111
	.byte %11110000
	.byte %01111000
	.byte %00011000
	.byte %00001100
	.byte %00000110
	.byte %00000010
	.byte %00000100
	.byte %00100000
	.byte %11000000
	.byte %00000011
	.byte %00000011
	.byte %00000011
	.byte %00000011
	.byte %00000011
	.byte %00000011
	.byte %00000011
	.byte %00000100
	.byte %00001100
	.byte %00001100
	.byte %00011110
	.byte %00011110
	.byte %00011110
	.byte %00011111
	.byte %00001111
	.byte %00000011
MimicColors:
	.byte $f4
	.byte $f2
	.byte $f0
	.byte $f0
	.byte $f4
	.byte $f0
	.byte $f2
	.byte $64
	.byte $64
	.byte $66
	.byte $66
	.byte $68
	.byte $66
	.byte $64
	.byte $a
	.byte $a
	.byte $f0
	.byte $f0
	.byte $f2
	.byte $f0
	.byte $f2
	.byte $f0
	.byte $f0
	.byte $f2
	.byte $f0
	.byte $f2
	.byte $f0
	.byte $f4
	.byte $f0
	.byte $f0
	.byte $f2
	.byte $f4

JesterGraphics:
	.byte %11100100
	.byte %11100100
	.byte %11101000
	.byte %11110000
	.byte %11000000
	.byte %00000000
	.byte %10000000
	.byte %01000000
	.byte %11100000
	.byte %10100000
	.byte %11000000
	.byte %11100100
	.byte %11111000
	.byte %11100000
	.byte %10011000
	.byte %00000100
	.byte %00000111
	.byte %00000111
	.byte %00110111
	.byte %01101111
	.byte %01000011
	.byte %11100000
	.byte %01000001
	.byte %01000010
	.byte %00100111
	.byte %00000101
	.byte %00000011
	.byte %00100111
	.byte %00011111
	.byte %00000111
	.byte %00011000
	.byte %00100001
JesterColors:
	.byte $54
	.byte $d8
	.byte $54
	.byte $d8
	.byte $54
	.byte $e
	.byte $8
	.byte $8
	.byte $8
	.byte $8
	.byte $8
	.byte $2c
	.byte $82
	.byte $42
	.byte $42
	.byte $2c
	.byte $d8
	.byte $54
	.byte $d8
	.byte $54
	.byte $d8
	.byte $f4
	.byte $e
	.byte $e
	.byte $e
	.byte $e
	.byte $e
	.byte $2c
	.byte $42
	.byte $82
	.byte $82
	.byte $2c

ArmorGraphics:
	.byte %10000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %01000000
	.byte %01000000
	.byte %01000000
	.byte %01000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00011000
	.byte %00111000
	.byte %01111000
	.byte %11100000
	.byte %01110011
	.byte %00110011
	.byte %00110011
	.byte %00000000
	.byte %00101101
	.byte %10111111
	.byte %10111111
	.byte %10111111
	.byte %10111111
	.byte %00011110
	.byte %00000000
	.byte %00011110
	.byte %00111111
	.byte %00100001
	.byte %00111111
	.byte %00011110
ArmorColors:
	.byte $f2
	.byte $0
	.byte $0
	.byte $0
	.byte $f4
	.byte $6
	.byte $8
	.byte $8
	.byte $d4
	.byte $0
	.byte $d4
	.byte $d4
	.byte $d4
	.byte $d4
	.byte $d4
	.byte $d4
	.byte $f2
	.byte $f2
	.byte $f2
	.byte $8
	.byte $6
	.byte $6
	.byte $8
	.byte $8
	.byte $d4
	.byte $6
	.byte $8
	.byte $6
	.byte $6
	.byte $8
	.byte $8
	.byte $8

	ORG $EC00 ;Contains medium enemy data
	RORG $FC00

MediumTestEnemyGraphics7:
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
MediumTestEnemyColors7:
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

SlimeGraphics:
	.byte %11111110
	.byte %00000111
	.byte %00000001
	.byte %00000011
	.byte %00000010
	.byte %10100110
	.byte %00001100
	.byte %00011100
	.byte %00111000
	.byte %11110000
	.byte %11000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %01111111
	.byte %11100000
	.byte %10000000
	.byte %11001000
	.byte %01100100
	.byte %00100000
	.byte %00110100
	.byte %00010010
	.byte %00011000
	.byte %00001111
	.byte %00000111
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
	.byte %00000000
SlimeColors:
	.byte $c4
	.byte $b4
	.byte $c6
	.byte $b6
	.byte $c6
	.byte $b4
	.byte $c4
	.byte $c6
	.byte $c2
	.byte $b4
	.byte $c6
	.byte $0
	.byte $0
	.byte $0
	.byte $0
	.byte $0
	.byte $b4
	.byte $c6
	.byte $c4
	.byte $c2
	.byte $c6
	.byte $c6
	.byte $b6
	.byte $b8
	.byte $c6
	.byte $c2
	.byte $c4
	.byte $0
	.byte $0
	.byte $0
	.byte $0
	.byte $0

MediumTestEnemyGraphics9:
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
MediumTestEnemyColors9:
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

MediumTestEnemyGraphics10:
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
MediumTestEnemyColors10:
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

	ORG $ED00 ;Contains medium enemy data
	RORG $FD00

MediumTestEnemyGraphics11:
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
MediumTestEnemyColors11:
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

TrophyGraphics:
	.byte %11110000
	.byte %11100000
	.byte %11100000
	.byte %11000000
	.byte %11000000
	.byte %11000000
	.byte %11100000
	.byte %11110000
	.byte %00111100
	.byte %01111010
	.byte %01111001
	.byte %01111001
	.byte %01111010
	.byte %01111100
	.byte %11111100
	.byte %11111100
	.byte %00001111
	.byte %00000111
	.byte %00000111
	.byte %00000011
	.byte %00000011
	.byte %00000011
	.byte %00000111
	.byte %00001111
	.byte %00111100
	.byte %01011110
	.byte %10011110
	.byte %10011110
	.byte %01011100
	.byte %00111110
	.byte %00111111
	.byte %00111111
TrophyColors:
	.byte $2e
	.byte $2a
	.byte $2a
	.byte $2e
	.byte $2a
	.byte $2a
	.byte $2e
	.byte $2a
	.byte $2a
	.byte $2e
	.byte $2a
	.byte $2a
	.byte $2e
	.byte $2a
	.byte $2a
	.byte $2e
	.byte $2a ;2 bigger than expected
	.byte $2a

	ORG $ED80 ;Contains small enemy data
	RORG $FD80

SmallTestGraphics1:
	.byte %11111111
	.byte %11000001
	.byte %10100001
	.byte %10010001
	.byte %10001001
	.byte %10000101
	.byte %10000011
	.byte %11111111
SmallTestColors1:
	.byte $60
	.byte $62
	.byte $64
	.byte $66
	.byte $68
	.byte $6a
	.byte $6c
	.byte $6e

DruidGraphics:
	.byte %00101001
	.byte %00101001
	.byte %00110011
	.byte %01111101
	.byte %10110001
	.byte %00110000
	.byte %11001100
	.byte %01001010
DruidColors:
	.byte $c8
	.byte $d6
	.byte $c6
	.byte $d8
	.byte $b6
	.byte $b6
	.byte $f2
	.byte $f2

ShroomGraphics:
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %11110111
	.byte %10111101
	.byte %11101111
	.byte %00111100
ShroomColors:
	.byte $fe
	.byte $fc
	.byte $fa
	.byte $fa
	.byte $46
	.byte $44
	.byte $42
	.byte $42

SquireGraphics:
	.byte %10010110
	.byte %10010100
	.byte %10011000
	.byte %01011010
	.byte %00111100
	.byte %00011000
	.byte %00010100
	.byte %00111100
SquireColors:
	.byte $8
	.byte $8
	.byte $f4
	.byte $42
	.byte $44
	.byte $fe
	.byte $a6
	.byte $f2

ArcherGraphics:
	.byte %01010100
	.byte %01010010
	.byte %01110001
	.byte %10101101
	.byte %01110010
	.byte %00100100
	.byte %01110000
	.byte %01110000
ArcherColors:
	.byte $f2
	.byte $f6
	.byte $f2
	.byte $d6
	.byte $d6
	.byte $d2
	.byte $fc
	.byte $fc

SmallTestGraphics6:
	.byte %11111111
	.byte %11000001
	.byte %10100001
	.byte %10010001
	.byte %10001001
	.byte %10000101
	.byte %10000011
	.byte %11111111
SmallTestColors6:
	.byte $60
	.byte $62
	.byte $64
	.byte $66
	.byte $68
	.byte $6a
	.byte $6c
	.byte $6e

GiftGraphics:
	.byte %01111110
	.byte %01111110
	.byte %01111110
	.byte %01111110
	.byte %01111110
	.byte %00010000
	.byte %00010000
	.byte %00001000
GiftColors:
	.byte $96
	.byte $82
	.byte $96
	.byte $82
	.byte $96
	.byte $f2
	.byte $36
	.byte $2c

SwordGraphics:
	.byte %00011000
	.byte %00011000
	.byte %00111100
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00011000
	.byte %00001000
SwordColors:
	.byte $f4
	.byte $f2
	.byte $d4
	.byte $8
	.byte $a
	.byte $a
	.byte $a
	.byte $a

ShieldGraphics:
	.byte %00011000
	.byte %00111100
	.byte %01111110
	.byte %01100110
	.byte %01100110
	.byte %01100110
	.byte %01111110
	.byte %01111110
ShieldColors:
	.byte $8
	.byte $8
	.byte $8
	.byte $6
	.byte $d4
	.byte $d4
	.byte $6
	.byte $8

ZombieGraphics:
	.byte %00101100
	.byte %00101000
	.byte %00110000
	.byte %00110010
	.byte %00111100
	.byte %00110000
	.byte %00101000
	.byte %00110000
ZombieColors:
	.byte $f0
	.byte $f2
	.byte $98
	.byte $98
	.byte $96
	.byte $d4
	.byte $42
	.byte $d4

SkltonGraphics:
	.byte %01010000
	.byte %01010100
	.byte %01110010
	.byte %10101101
	.byte %01110001
	.byte %00100010
	.byte %01110100
	.byte %01010000
SkltonColors:
	.byte $e
	.byte $a
	.byte $e
	.byte $a
	.byte $e
	.byte $a
	.byte $e
	.byte $42

MageGraphics:
	.byte %01111110
	.byte %00111100
	.byte %10111101
	.byte %01111110
	.byte %00111000
	.byte %00100100
	.byte %00110100
	.byte %00011000
MageColors:
	.byte $96
	.byte $98
	.byte $9a
	.byte $9a
	.byte $96
	.byte $98
	.byte $9a
	.byte $9a

GoopGraphics:
	.byte %01111110
	.byte %10000001
	.byte %01000001
	.byte %01001010
	.byte %00100010
	.byte %00100100
	.byte %00011000
	.byte %00000000
GoopColors:
	.byte $c2
	.byte $b4
	.byte $c2
	.byte $c4
	.byte $b4
	.byte $c2
	.byte $d4
	.byte $c2

WarlokGraphics:
	.byte %01111110
	.byte %00111100
	.byte %10111101
	.byte %01111110
	.byte %00111000
	.byte %00100100
	.byte %00110100
	.byte %00011000
WarlokColors:
	.byte $40
	.byte $42
	.byte $42
	.byte $44
	.byte $40
	.byte $42
	.byte $42
	.byte $44

ImpGraphics:
	.byte %00010110
	.byte %01010100
	.byte %10101100
	.byte %10011111
	.byte %01001100
	.byte %10001111
	.byte %00001010
	.byte %00011100
ImpColors:
	.byte $64
	.byte $62
	.byte $64
	.byte $64
	.byte $62
	.byte $64
	.byte $46
	.byte $66

WispGraphics:
	.byte %00000000
	.byte %00011100
	.byte %00111100
	.byte %00110000
	.byte %00100000
	.byte %00100000
	.byte %00010000
	.byte %00000000
WispColors:
	.byte $aa
	.byte $a4
	.byte $a6
	.byte $a8
	.byte $aa
	.byte $ac
	.byte $ae
	.byte $ac

RedOrbGraphics:
	.byte %00111100
	.byte %01001110
	.byte %10111111
	.byte %01111111
	.byte %11111111
	.byte %11111101
	.byte %01111010
	.byte %00111100
RedOrbColors:
	.byte $30
	.byte $30
	.byte $42
	.byte $42
	.byte $44
	.byte $44
	.byte $46
	.byte $46

BluOrbGraphics:
	.byte %00111100
	.byte %01001110
	.byte %10111111
	.byte %01111111
	.byte %11111111
	.byte %11111101
	.byte %01111010
	.byte %00111100
BluOrbColors:
	.byte $92
	.byte $92
	.byte $94
	.byte $94
	.byte $96
	.byte $96
	.byte $98
	.byte $98

GrnOrbGraphics:
	.byte %00111100
	.byte %01001110
	.byte %10111111
	.byte %01111111
	.byte %11111111
	.byte %11111101
	.byte %01111010
	.byte %00111100
GrnOrbColors:
	.byte $c2
	.byte $c2
	.byte $c4
	.byte $c4
	.byte $c6
	.byte $c6
	.byte $d8
	.byte $d8

GldOrbGraphics:
	.byte %00111100
	.byte %01001110
	.byte %10111111
	.byte %01111111
	.byte %11111111
	.byte %11111101
	.byte %01111010
	.byte %00111100
GldOrbColors:
	.byte $18
	.byte $18
	.byte $1a
	.byte $1a
	.byte $1c
	.byte $1c
	.byte $1e
	.byte $1e

	ORG $EEC0
	RORG $FEC0

ELoadEnemyAIForS:
	nop
	nop
	nop
	jmp ELoadEnemyAI
EAfterLoadingEnemyAI:
	sta $1FF9 ;Go to S bank	

EEncounterSizes:
	.byte 2
	.byte 3
	.byte 4
	.byte 4

	;Encounter tables must ALWAYS end with a small enemy, and every instance of a medium or large enemy MUST be IMMEDIATELY followed by a small enemy
	;Encounter tables must be a multiple of 2 in size. 16 happens to be the most convenient size.
EGroundsEnemies:
	.byte $14 ;Bear
	.byte $00 ;Wolf
	.byte $15 ;Unicrn
	.byte $01 ;Druid
	.byte $14 ;Bear
	.byte $00 ;Wolf
	.byte $15 ;Unicrn
	.byte $01 ;Druid
	.byte $00 ;Wolf
	.byte $01 ;Druid
	.byte $02 ;Shroom
	.byte $00 ;Wolf
	.byte $01 ;Druid
	.byte $02 ;Shroom
	.byte $00 ;Wolf
	.byte $01 ;Shroom
	; .byte $00 ;Wolf
	; .byte $00 ;Wolf
	; .byte $00 ;Wolf
	; .byte $00 ;Wolf
	; .byte $00 ;Wolf
	; .byte $00 ;Wolf
	; .byte $00 ;Wolf
	; .byte $00 ;Wolf
	; .byte $00 ;Wolf
	; .byte $00 ;Wolf
	; .byte $00 ;Wolf
	; .byte $00 ;Wolf
	; .byte $00 ;Wolf
	; .byte $00 ;Wolf
	; .byte $00 ;Wolf
	; .byte $00 ;Wolf

ECastleEnemies:
	.byte $18 ;Grgoyl
	.byte $03 ;Squire
	.byte $18 ;Grgoyl
	.byte $04 ;Archer
	.byte $18 ;Grgoyl
	.byte $05 ;Priest
	.byte $19 ;Mimic
	.byte $03 ;Squire
	.byte $19 ;Mimic
	.byte $04 ;Archer
	.byte $05 ;Priest
	.byte $03 ;Squire
	.byte $04 ;Archer
	.byte $05 ;Priest
	.byte $03 ;Squire
	.byte $04 ;Archer

ECatacombsEnemies:
	.byte $1C ;Spider
	.byte $09 ;Zombie
	.byte $1D ;Slime
	.byte $0A ;Sklton
	.byte $1C ;Spider
	.byte $0B ;Mage
	.byte $1D ;Slime
	.byte $09 ;Zombie
	.byte $1D ;Slime
	.byte $0A ;Sklton
	.byte $0B ;Mage
	.byte $09 ;Zombie
	.byte $0A ;Sklton
	.byte $0B ;Mage
	.byte $09 ;Zombie
	.byte $0A ;Sklton

EAbyssEnemies:
	.byte $1F ;Shfflr
	.byte $0D ;Warlok
	.byte $20 ;Shmblr
	.byte $0E ;Imp
	.byte $20 ;Shmblr
	.byte $0F ;Wisp
	.byte $20 ;Shmblr
	.byte $0D ;Warlok
	.byte $1F ;Shfflr
	.byte $0E ;Imp
	.byte $0F ;Wisp
	.byte $0D ;Warlok
	.byte $0E ;Imp
	.byte $0F ;Wisp
	.byte $0D ;Warlok
	.byte $0E ;Imp

EBossEncounters:
	;GROUNDS BOSS 1
	.byte $22 ;Thickt
	.byte $FF
	.byte $FF
	.byte $FF
	;GROUNDS BOSS 2
	.byte $16 ;Volcio
	.byte $FF
	.byte $17 ;Glacia
	.byte $FF
	;CASTLE BOSS 1
	.byte $1A ;Jester
	.byte $FF
	.byte $FF
	.byte $FF
	;CASTLE BOSS 2
	.byte $07 ;Sword
	.byte $1B ;Armor
	.byte $FF
	.byte $08 ;Shield
	;CRYPT BOSS 1
	.byte $23 ;Ooze
	.byte $FF
	.byte $FF
	.byte $FF
	;CRYPT BOSS 2
	.byte $1E ;Lich
	.byte $FF
	.byte $FF
	.byte $FF
	;ABYSS BOSS 1
	.byte $24 ;Horror
	.byte $FF
	.byte $FF
	.byte $FF
	;ABYSS BOSS 2
	.byte $10 ;RedOrb
	.byte $11 ;BluOrb
	.byte $12 ;GrnOrb
	.byte $13 ;GldOrb

EEffectLength:
	.byte 0 ;No effect
	.byte 1 ;Party member highlighting
	.byte 8 ;Transition to battle
	.byte 4 ;Transition to fire
	.byte 4 ;Transition to maze
	.byte 16 ;Enemy damage flash
	.byte 3 ;Trophy shimmer
	.byte 0 ;Pre-spell delay
	.byte 0 ;Spell effect
EEffectFrequency:
	.byte 0
	.byte 30
	.byte 10
	.byte 10
	.byte 10
	.byte 1 ;Enemy damage flash
	.byte 4 ;Shimmer
	.byte 0 ;Pre-spell delay
	.byte 4 ;Spell effect

ESpellDelays:
	.byte 8
	.byte 8
	.byte 8
	.byte 8
	.byte 8
	.byte 8
	.byte 8
	.byte 8
	.byte 8
	.byte 8
	.byte 8
	.byte 8
	.byte 8
	.byte 8
	.byte 8
	.byte 8
	.byte 8
	.byte 8
	.byte 8

ESpellEffectLengths:
	.byte 32
	.byte 32
	.byte 32
	.byte 32
	.byte 32
	.byte 32
	.byte 32
	.byte 32
	.byte 32
	.byte 32
	.byte 32
	.byte 32
	.byte 32
	.byte 32
	.byte 32
	.byte 32
	.byte 32
	.byte 32
	.byte 32

ESpellEffectBaseColors:
	.byte $F8
	.byte $B8
	.byte $98
	.byte $38
	.byte $18
	.byte $78
	.byte $F8
	.byte $58
	.byte $C8
	.byte $18
	.byte $08
	.byte $A8
	.byte $C8
	.byte $98
	.byte $B8
	.byte $48
	.byte $58
	.byte $28
	.byte $48

	ORG $EF90
	RORG $FF90

ECatchAndLoadEffect:
	nop
	nop
	nop
	jmp ELoadEffect
EAfterLoadingEffect:
	sta $1FF7 ;Go to bank 1
	nop
	nop
	nop

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


