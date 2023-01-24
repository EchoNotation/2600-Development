	PROCESSOR 6502
; paddle demo
; Darrell Spice Jr.
; February 17, 2008
;
; paddle results will be shown by positiong 4 objects:
; paddle 1 = player 0
; paddle 2 = player 1
; paddle 3 = missile 0
; paddle 4 = missile 1

TIA_BASE_READ_ADDRESS = $30
        include VCS.H
        include macro.h
	
	SEG.U VARS
	ORG $80
Frame	     ds 1
Paddle1      ds 1
Paddle2      ds 1
Paddle3      ds 1
Paddle4      ds 1
Paddles2Read ds 1

        MAC READ_PADDLE_1
        lda INPT0         ; 3   - always 9
        bpl .save         ; 2 3
        .byte $2c         ; 4 0
.save   sty Paddle1       ; 0 3
        ENDM

        MAC READ_PADDLE_2
        lda INPT1         ; 3   - always 9
        bpl .save         ; 2 3
        .byte $2c         ; 4 0
.save   sty Paddle2       ; 0 3
        ENDM

        MAC READ_PADDLE_3
        lda INPT2         ; 3   - always 9
        bpl .save         ; 2 3
        .byte $2c         ; 4 0
.save   sty Paddle3       ; 0 3
        ENDM

        MAC READ_PADDLE_4
        lda INPT3         ; 3   - always 9
        bpl .save         ; 2 3
        .byte $2c         ; 4 0
.save   sty Paddle4       ; 0 3
        ENDM


        MAC READ_PADDLE_1_OR_2
        ldx Paddles2Read  ; 13-14  3
        lda INPT0,x       ; |      4
        bpl .save         ; |      2 3
        .byte $2c         ; |      4 0
.save   sty Paddle1,x     ; |      0 4
                          ; +-14 worse case scenerio
        ENDM

        MAC READ_PADDLE_3_OR_4
        ldx Paddles2Read  ; 13-14  3
        lda INPT2,x       ; |      4
        bpl .save         ; |      2 3
        .byte $2c         ; |      4 0
.save   sty Paddle3,x     ; |      0 4
                          ; +-14 worse case scenerio

        ENDM

        MAC READ_TWO_PADDLES
        ldx Paddles2Read   ; 21-23  3
        lda INPT0,x        ; |      4
        bpl .save1         ; |      2 3
        .byte $2c          ; |      4 0
.save1  sty Paddle1,x      ; |      0 4
        lda INPT2,x        ; |      4
        bpl .save2         ; |      2 3
        .byte $2c          ; |      4 0
.save2  sty Paddle3,x      ; |      0 4
                           ; +-23 worse case scenerio
        ENDM
	
	SEG CODE
	
	org $F000

InitSystem:
	CLEAN_START
	
	lda #%00110000 ; set both missiles to 8 wide
	sta NUSIZ0
	sta NUSIZ1
	
VerticalBlank:
        lda #$82
        sta WSYNC
        sta VSYNC         ; 3    start vertical sync, D1=1
        sta VBLANK        ; 3  6 start vertical blank and dump paddles to ground
        lda #$2C          ; 2  8 set timer for end of Vertical Blank
        sta TIM64T        ; 4 12
        sta WSYNC         ; 1st line of vertical sync
	inc Frame
	lda Frame
	and #1            ; prep which pair of paddles to read for current frame
	sta Paddles2Read  ;    we'll be reading 1 & 3 or 2 & 4
        sta WSYNC         ; 2nd line of vertical sync
        lda #0
        sta WSYNC         ; 3rd line of vertical sync
        sta VSYNC         ; stop vertical sync, D1=0

	inc Paddle3       ; missiles off by 1, so compensate
	inc Paddle4       ; missiles off by 1, so compensate
	
        ldx #3
PosObjectLoop
        lda Paddle1,X   ;+4    9
        sta WSYNC
DivideLoop
        sbc #15
        bcs DivideLoop   ;+4   13
        eor #7
        asl
        asl
        asl
        asl         
        sta.wx HMP0,X  ;+4   17
        sta RESP0,X      ;+4   23
        dex              ;+2    2
        bpl PosObjectLoop;+3    5
	
	dec Paddle3      ; remove missile compensation
	dec Paddle4      ; remove missile compensation
	
	sta WSYNC
	sta HMOVE

	lda #$0f    ; set color for paddle 1 display
	sta COLUP0
	lda #$8f    ; set color for paddle 2 display
	sta COLUP1
	
	 
	ldx Paddles2Read
	lda #153       ; prep paddle results with highest possible value
	sta Paddle1,x  ; our initial paddle results will be 1-153
	sta Paddle3,x  ; and will be adjusted to 0-152 in overscan
	ldx #0
	
VblankWait
        lda INTIM
        bpl VblankWait
	
	sta WSYNC
	sta HMCLR         ; clear hmoves for next time around
        stx VBLANK        ; turn on video output & remove paddle dump to ground
        ldy #152
ReadLoop
	sta WSYNC
	sty COLUBK
	READ_TWO_PADDLES ; reads the paddles
	dey
	bne ReadLoop
	
; display Paddle 1's value by using Player 0
	sta WSYNC
	lda #$FF
	sta GRP0
	ldy #8
P0loop	
	sta WSYNC
	dey
	bne P0loop
	
; display Paddle 2's value by using Player 1	
	
	lda #$FF
	ldx #0
	sta WSYNC
	stx GRP0
	sta GRP1
	ldy #8
P1loop
	sta WSYNC
	dey
	bne P1loop
	
; display Paddle 3's value by using Missile 0
	lda #$4f    ; set color for paddle 3 display
	sta COLUP0
	ldx #0
	lda #%10
	sta WSYNC
	stx GRP1
	sta ENAM0
	ldy #8
	
M0loop
	sta WSYNC
	dey
	bne M0loop
	
; display Paddle 4's value by using Missile 1
	lda #$cf    ; set color for paddle 4 display
	sta COLUP1
	ldx #0
	lda #%10
	sta WSYNC
	stx ENAM0
	sta ENAM1
	ldy #8
	
M1loop
	sta WSYNC
	dey
	bne M1loop
	
	
	
	lda #$26   ; prep overscan delay
	sta WSYNC
	sta TIM64T ; set overscan delay
	
	sty ENAM1
	
	
	ldx Paddles2Read
	dec Paddle1,x
	dec Paddle3,x
	
	;ldy Paddle1,x
	;dey
	;sty Paddle1,x
	;ldy Paddle3,x
	;dey
	;sty Paddle3,x
	
OSwait	
	lda INTIM
	bpl OSwait
	jmp VerticalBlank

	
	org $FFFA
        .word InitSystem ; NMI
        .word InitSystem ; RESET
        .word InitSystem ; IRQ
	