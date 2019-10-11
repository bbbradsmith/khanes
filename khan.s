;
; khan.s
; NES tribute to KHAN
; Brad Smith, 2019
; http;//rainwarrior.ca
;

COL1 = 5
COL2 = 3
COL3 = 3
COL4 = 3
COLUMNS = COL1 + COL2 + COL3 + COL4 ; animated columns
ROWST = 12 ; tile rows to update
SPEED_MIN = 256/4
SPEED_MAX = 2*256

; ===
; RAM
; ===

.segment "ZEROPAGE"
mouth: .res 1
i:     .res 1
j:     .res 1
ptr:   .res 2
pal:   .res 1 ; 0 = NTSC timing, 1 = PAL timing

.segment "BSS"
nmt:  .res (ROWST*32)
rot0: .res 16
rot1: .res 16
rott: .res 32

.assert (COLUMNS<16), error, "rot0/rot1 array size"

.segment "OAM"
.align 256
oam: .res 256

; ====
; Data
; ====

.segment "RODATA"

palette:
.byte $0F, $0F, $0F, $0F
.byte $0F, $0F, $06, $00
.byte $0F, $06, $16, $10
.byte $0F, $06, $26, $30
.byte $0F, $01, $11, $21
.byte $0F, $02, $12, $22
.byte $0F, $03, $13, $23
.byte $0F, $04, $14, $24

attribute_row: ; same row used for all rows
.byte %01000100, %11101110, $FF, $FF, $FF, $FF, $FF, $FF

; speed of each column

speed0:
.repeat COLUMNS, I
	.byte <(SPEED_MIN+(((SPEED_MAX-SPEED_MIN)*I)/(COLUMNS-1)))
.endrepeat

speed1:
.repeat COLUMNS, I
	.byte >(SPEED_MIN+(((SPEED_MAX-SPEED_MIN)*I)/(COLUMNS-1)))
.endrepeat

; width of each column

width:
.repeat COL1
	.byte 1
.endrepeat
.repeat COL2
	.byte 2
.endrepeat
.repeat COL3
	.byte 3
.endrepeat
.repeat COL4
	.byte 4
.endrepeat

; modulo of each column

mod:
.repeat COL1
	.byte 8
.endrepeat
.repeat COL2
	.byte 16
.endrepeat
.repeat COL3
	.byte 24
.endrepeat
.repeat COL4
	.byte 32
.endrepeat

; base tile of each tile column

BASE1 = $F8
BASE2 = $D8
BASE3 = $90
BASE4 = $10

baset:
.repeat COL1
	.byte BASE1
.endrepeat
.repeat COL2
	.byte BASE2, BASE2+16
.endrepeat
.repeat COL3
	.byte BASE3, BASE3+24, BASE3+48
.endrepeat
.repeat COL4
	.byte BASE4, BASE4+32, BASE4+64, BASE4+96
.endrepeat
.assert (*-baset)=32, error, "baset size"

; modulo of each tile column

modt:
.repeat COL1
	.byte 8
.endrepeat
.repeat COL2
	.byte 16, 16
.endrepeat
.repeat COL3
	.byte 24, 24, 24
.endrepeat
.repeat COL4
	.byte 32, 32, 32, 32
.endrepeat
.assert (*-modt)=32, error, "modt size"

; =========
; Utilities
; =========

animate:
	; animate mouth/eyes with a CHR page flip
	inc mouth
	inc mouth
	; animate parallax
	ldx #0
	stx j ; tile index
	@loop:
		; add speed to rot
		lda rot0, X
		clc
		adc speed0, X
		sta rot0, X
		lda rot1, X
		adc speed1, X
		cmp mod, X
		bcc :+
			;sec
			sbc mod, X
		:
		sta rot1, X
		; copy rot to rott
		ldy width, X
		:
			sty i
			ldy j ; tile index
			sta rott, Y
			iny
			sty j
			ldy i
			dey
			bne :-
		inx
		cpx #COLUMNS
		bcc @loop
	rts

nmt_row:
	; ptr = nmt row
	ldy #0
	@loop:
		; generate tile
		lda rott, Y
		clc
		adc baset, Y
		sta (ptr), Y
		; advance by 8 pixels backward (modulo)
		lda rott, Y
		sec
		sbc #8
		bcs :+
			;clc
			adc modt, Y
		:
		sta rott, Y
		iny
		cpy #32
		bcc @loop
	; advance row
	lda ptr+0
	clc
	adc #32
	sta ptr+0
	bcc :+
		inc ptr+1
	:
	rts

nmt_rows:
	lda #<nmt
	sta ptr+0
	lda #>nmt
	sta ptr+1
	.repeat ROWST
		jsr nmt_row
	.endrepeat
	rts

.segment "ALIGN"

; upload nmt buffer to $2007
; aligned for fixed timing
.align 256
nmt_apply:
	UNROLL = 16
	ldx #0
	:
		.repeat UNROLL, I
			lda nmt+I, X
			sta $2007
		.endrepeat
		txa
		clc
		adc #UNROLL
		tax
		bne :-
	.assert (ROWST*32)>256, error, "ROWST size too large"
	:
		.repeat UNROLL, I
			lda nmt+256+I, X
			sta $2007
		.endrepeat
		txa
		clc
		adc #UNROLL
		tax
		cpx #((ROWST*32)-256)
		bcc :-
	.assert (ROWST*32)<512, error, "ROWST size too lage"
	rts

; timed rendering code with forced black
; 16:9 letterboxing (48 lines of blank at top and bottom)
; aligned for fixed timing
.align 256
render:
	; forced blank
	lda #%00000000
	sta $2001
	lda pal
	beq :+ ; PAL should OAMDMA at start
		jsr oamdma
	:
	bit $2002
	lda #$20
	sta $2006
	lda #$00
	sta $2006
	jsr nmt_apply
	jsr delay0 ; delay until line 48
	lda pal
	bne :+
		jsr oamdma
	:
	; set CHR page and other $2000 properties
	lda mouth
	lsr
	ora mouth
	lsr
	lsr
	lsr
	and #%00011000 ; mouth high bit selects both CHR pages
	ora #%10100000 ; NMI on, high sprites, +1 increment, nametable 00
	sta $2000
	; set scroll 8,0
	lda #0
	ldx #1
	sta $2006
	sta $2005
	sta $2005
	stx $2006
	; unblank
	lda #%00011110
	sta $2001
	.assert ROWST=12, error, "render timing needs adjustment"
	jsr delay1 ; wait 96 lines
	; TODO reset scroll 8,0
	;lda #0
	;ldx #1
	;sta $2006
	;sta $2005
	;sta $2005
	;stx $2006
	jsr delay2 ; wait 48 lines
	; TODO blank the bottom for symmetry
	;lda #%00000000
	;sta $2001
	rts
delay0:
	lda pal
	bne @pal
@ntsc:
	rts
@pal:
	rts
delay1:
	lda pal
	bne @pal
@ntsc:
	rts
@pal:
	rts
delay2:
	lda pal
	bne @pal
@ntsc:
	rts
@pal:
	rts

oamdma:
	lda #0
	sta $2003
	lda #>oam
	sta $4014
	rts

; Detects NTSC vs PAL
; http://forums.nesdev.com/viewtopic.php?p=163258#p163258
; A = 0 NTSC
;     1 PAL
;     2 Dendy
;     3+ ?
.align 32
cpu_speed_detect:
	; count increments between 2 vblanks
	ldx #0
	ldy #0
	bit $2002
:
	bit $2002
	bpl :-
:
	inx
	bne :+
		iny
	:
	bit $2002
	bpl :--
	; compensate for a double-frame in case $2002 was accidentally suppressed once
	tya
	cmp #16
	bcc :+
	lsr
:
	sec
	sbc #9
	rts

; ====
; Main
; ====

.segment "CODE"

nmi:
	pha
	txa
	pha
	tya
	pha
	jsr render
	jsr animate
	jsr nmt_rows
	; DO sound
	pla
	tay
	pla
	tax
	pla
	rti

irq:
	; not used
	rti

reset:
	sei       ; disable maskable interrupts
	lda #0
	sta $2000 ; disable non-maskable interrupt
	lda #0
	sta $2001 ; rendering off
	sta $4010 ; disable DMC IRQ
	sta $4015 ; disable APU sound
	lda #$40
	sta $4017 ; disable APU IRQ
	cld       ; disable decimal mode
	ldx #$FF
	txs       ; setup tack
	; wait for vblank #1
	bit $2002
	:
		bit $2002
		bpl :-
	; preserve positions on reset
	lda mouth
	pha
	ldx #0
	:
		lda rot1, X
		pha
		inx
		cpx #COLUMNS
		bcc :-
	; clear RAM
	lda #0
	tax
	:
		sta $0000, X
		;sta $0100, X ; don't clear stack yet
		sta $0200, X
		sta $0300, X
		sta $0400, X
		sta $0500, X
		sta $0600, X
		sta $0700, X
		inx
		bne :-
	; restore positions from stack
	ldx #COLUMNS
	@loop:
		pla
		dex
		: ; make sure within modulus range
			cmp mod, X
			bcc :+
			;sec
			sbc mod, X
			jmp :-
		:
		sta rot1, X
		cpx #0
		bne @loop
	pla
	sta mouth
	; clear stack, separately wipe OAM
	lda #0
	:
		sta $0100, X
		inx
		bne :-
	lda #$FF
	:
		sta oam, X
		inx
		bne :-
	; wait for vblank #2
	:
		bit $2002
		bpl :-
	; ready
	;jmp main

main:
	jsr cpu_speed_detect
	cmp #1
	beq :+
		lda #0 ; anything not PAL is assumed NTSC
	:
	sta pal
	; load palettes
	bit $2002
	lda #$3F
	sta $2006
	lda #$00
	sta $2006
	ldx #0
	:
		lda palette, X
		sta $2007
		inx
		cpx #32
		bcc :-
	; load attributes
	lda #$23
	sta $2006
	lda #$C0
	sta $2006
	ldy #8
	ldx #0
	:
		lda attribute_row, X
		sta $2007
		inx
		cpx #8
		bcc :-
		ldx #0
		dey
		bne :-
	; fill render data for first frame
	jsr animate
	jsr nmt_rows
	; turn on NMI
	lda #%10000000
	sta $2000
	; infinite loop
:
	jmp :-

; =======
; NES ROM
; =======

.segment "CHR"

.incbin "khan.chr"

.segment "HEADER"

INES_MAPPER     = 0 ; NROM
INES_MIRROR     = 0 ; vertical nametables
INES_PRG_16K    = 1 ; 16K
INES_CHR_8K     = 1 ; 8K
INES_BATTERY    = 0
INES2           = %00001000 ; NES 2.0 flag for bit 7
INES2_SUBMAPPER = 0
INES2_PRGRAM    = 0
INES2_PRGBAT    = 0
INES2_CHRRAM    = 0
INES2_CHRBAT    = 0
INES2_REGION    = 2 ; 0=NTSC, 1=PAL, 2=Dual

; iNES 1 header
.byte 'N', 'E', 'S', $1A ; ID
.byte <INES_PRG_16K
.byte INES_CHR_8K
.byte INES_MIRROR | (INES_BATTERY << 1) | ((INES_MAPPER & $f) << 4)
.byte (<INES_MAPPER & %11110000) | INES2
; iNES 2 section
.byte (INES2_SUBMAPPER << 4) | (INES_MAPPER>>8)
.byte ((INES_CHR_8K >> 8) << 4) | (INES_PRG_16K >> 8)
.byte (INES2_PRGBAT << 4) | INES2_PRGRAM
.byte (INES2_CHRBAT << 4) | INES2_CHRRAM
.byte INES2_REGION
.byte $00 ; VS system
.byte $00, $00 ; padding/reserved
.assert * = 16, error, "NES header must be 16 bytes."

.segment "VECTORS"
.word nmi
.word reset
.word irq

; end of file
