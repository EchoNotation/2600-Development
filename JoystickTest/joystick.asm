	processor 6502
	include "vcs.h"
	
	SEG.U variables
	org $80

currentColor ds 1
currentValue ds 1

	SEG CODE
	ORG $F000


;Clear all registers and RAM
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

StartOfFrame:
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
	REPEAT 37
		sta WSYNC
	REPEND

	lda INPT4
	bmi ButtonNotPressed
	inc currentColor
	inc currentColor ;Necessary because the low bit of color is unused
ButtonNotPressed:
	lda SWCHA
	bpl RightPressed
	asl
	bpl LeftPressed
	asl
	bpl DownPressed
	asl
	bpl UpPressed
	bmi DrawPicture
UpPressed:
	lda #10
	jmp Add
DownPressed:
	lda #$F5
	jmp Add
LeftPressed:
	lda #$FF
	jmp Add
RightPressed:
	lda #1
Add:
	clc
	adc currentValue
	sta currentValue
DrawPicture:
	lda currentColor
	sta COLUPF

	;Draw the picture to the screen.
	REPEAT 90
		sta WSYNC
	REPEND

	lda currentValue
	sta PF1

	REPEAT 20
		sta WSYNC
	REPEND

	lda #0
	sta PF1

	REPEAT 82
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