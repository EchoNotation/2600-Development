	processor 6502
	include "vcs.h"

;Constants
VBLANK_TIMER_DURATION=44
OVERSCAN_TIMER_DURATION=36

	SEG CODE
	ORG $E000
	RORG $F000

Reset:
	nop
	nop
	nop
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
	lda #2
	sta VBLANK
	sta VSYNC
	sta WSYNC
	sta WSYNC
	sta WSYNC
	lda #0
	sta VSYNC

	lda #VBLANK_TIMER_DURATION
	sta TIM64T
WaitForVblankTimer:
	lda INTIM
	bne WaitForVblankTimer
	
	sta WSYNC
	ldx #0
	stx VBLANK
MainPicture:
	stx COLUBK
	sta WSYNC
	inx
	cpx #192
	bcc MainPicture

	lda #2
	sta VBLANK

	lda #OVERSCAN_TIMER_DURATION
	sta TIM64T
WaitForOverscanTimer:
	lda INTIM
	bne WaitForOverscanTimer

	sta WSYNC
	jmp StartOfFrame

	ORG $EFFA
	RORG $FFFA

	;NMI, IRQ, and RESET information
	.word Reset
	.word Reset
	.word Reset

	ORG $F000
	RORG $F000

Reset:
	nop $1FF8 ;Switch to bank 1, ensuring that the game always starts in bank 1
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
	lda #2
	sta VBLANK
	sta VSYNC
	sta WSYNC
	sta WSYNC
	sta WSYNC
	lda #0
	sta VSYNC

	lda #VBLANK_TIMER_DURATION
	sta TIM64T
WaitForVblankTimer:
	lda INTIM
	bne WaitForVblankTimer
	
	sta WSYNC
	ldx #0
	stx VBLANK
MainPicture:
	stx COLUBK
	sta WSYNC
	inx
	cpx #192
	bcc MainPicture

	lda #2
	sta VBLANK

	lda #OVERSCAN_TIMER_DURATION
	sta TIM64T
WaitForOverscanTimer:
	lda INTIM
	bne WaitForOverscanTimer

	sta WSYNC
	jmp StartOfFrame

	ORG $FFFA
	RORG $FFFA

	;NMI, IRQ, and RESET information
	.word Reset
	.word Reset
	.word Reset

END