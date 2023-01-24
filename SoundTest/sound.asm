	processor 6502
	include "vcs.h"
	
	SEG.U variables
	org $80

currentAttribute ds 1
soundType ds 1
frequency ds 1
volume ds 1
previousInputs ds 1

	SEG CODE
	ORG $F000


;Clear all registers and RAM
Reset
	ldx #0
	txa
	tay
Clear
	dex
	txs
	pha
	bne Clear
	cld

StartOfFrame
	;Start of VBLANK
	lda #$0
	sta VBLANK
	lda #$2
	sta VSYNC

	;3 lines of VSYNC
	sta WSYNC
	sta WSYNC
	sta WSYNC
	lda #0
	sta VSYNC

	;37 more lines of VBLANK
	REPEAT 33
		sta WSYNC
	REPEND

	;Interpret controller inputs
	lda SWCHA
	eor #$FF
	cmp previousInputs
	sta previousInputs
	beq Skip
	tay
	and #$80
	bne InputRight
	tya
	and #$40
	bne InputLeft
	tya
	and #$20
	bne InputDown
	tya
	and #$10
	bne InputUp
	jmp Skip
InputDown ;Update values according to new inputs
	sta WSYNC
	lda currentAttribute
	clc
	adc #$1
	cmp #$3
	sta currentAttribute
	bmi UpdateAudioRegisters
	lda #$0
	sta currentAttribute
	jmp UpdateAudioRegisters
InputRight
	sta WSYNC
	ldx currentAttribute
	lda soundType,x
	clc
	adc #$1
	sta soundType,x
	jmp UpdateAudioRegisters
InputLeft
	sta WSYNC
	ldx currentAttribute
	lda soundType,x
	sec
	sbc #$1
	sta soundType,x
	jmp UpdateAudioRegisters
InputUp
	sta WSYNC
	lda currentAttribute
	beq InputUpWrapAround
	sec
	sbc #$1
	sta currentAttribute
	jmp UpdateAudioRegisters
InputUpWrapAround
	lda #$2
	sta currentAttribute
UpdateAudioRegisters
	sta WSYNC
	lda soundType
	sta AUDC0
	lda frequency
	sta AUDF0
	lda volume
	sta AUDV0
	sta WSYNC
	jmp DrawPicture
Skip
	sta WSYNC
	sta WSYNC
	sta WSYNC
DrawPicture
	;Draw the picture to the screen.
	ldx #$97
	stx COLUBK
	ldx #$64
	stx COLUPF
	sta WSYNC

	lda soundType
	and #$0f
	sta PF2
	sta WSYNC
	sta WSYNC
	lda frequency
	and #$1f
	sta PF2
	sta WSYNC
	sta WSYNC
	lda volume
	and #$0f
	sta PF2
	sta WSYNC
	sta WSYNC
	lda #$0
	sta PF2

	REPEAT 185
		sta WSYNC
	REPEND

	;End of picture creation-- Start overscan.
	lda #%01000010
	sta VBLANK

	REPEAT 30
		sta WSYNC
	REPEND

	jmp StartOfFrame

	ORG $FFFA

	.word Reset
	.word Reset
	.word Reset

END