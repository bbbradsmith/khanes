;
; khan.s
; NES tribute to KHAN
; Brad Smith, 2019
; http;//rainwarrior.ca
;

USE_DPCM = 1
COL1 = 5
COL2 = 3
COL3 = 3
COL4 = 3
COLUMNS = COL1 + COL2 + COL3 + COL4 ; animated columns
ROWST = 12 ; tile rows to update
SPEED_MIN = 256/4
SPEED_MAX = 3*256

; ===
; RAM
; ===

.segment "ZEROPAGE"
mouth:   .res 1
i:       .res 1
j:       .res 1
ptr:     .res 2
pal:     .res 1 ; 0 = NTSC timing, 1 = PAL timing
nmi_now: .res 1 ; prevents reentry

.segment "BSS"
nmt:  .res (ROWST*32)
rot0: .res 16
rot1: .res 16

.assert (COLUMNS<16), error, "rot0/rot1 array size"

.segment "OAM"
.align 256
oam: .res 256

; ====
; Data
; ====

.segment "CHR"
.incbin "khan.chr"

.segment "DPCM"

; creates a DPCM address constant with the same name as label_ but with "_dpcm" appended
.macro DPCM_ADDR label_
	.local label_dpcm
	label_dpcm = (((label_)-$C000)>>6)
	.ident(.concat(.string(label_),"_dpcm")) = <label_dpcm
	.assert .ident(.concat(.string(label_),"_dpcm")) = label_dpcm, error, "DPCM not in range"
.endmacro

.align 64
scream: .incbin "khan.dmc"
DPCM_ADDR scream

.align 64
silent: .byte 0
DPCM_ADDR silent


.segment "RODATA"

palette:
.byte $0F, $0F, $0F, $0F
.byte $0F, $0F, $05, $00
.byte $0F, $05, $16, $10
.byte $0F, $06, $26, $30
.byte $0F, $0F, $0F, $0F
.byte $0F, $01, $11, $21
.byte $0F, $02, $12, $22
.byte $0F, $03, $13, $23

attribute_row: ; same row used for all visible rows
.byte %01000100, %10101010, %10101010, %11101110, $FF, $FF, $FF, $FF

; speed of each column

speed0:
.repeat COLUMNS, I
	;.byte <(SPEED_MIN+(((SPEED_MAX-SPEED_MIN)*I)/(COLUMNS-1))) ; linear
	.byte <(SPEED_MIN+(((SPEED_MAX-SPEED_MIN)*(I*I))/((COLUMNS-1)*(COLUMNS-1)))) ; quadratic
.endrepeat

speed1:
.repeat COLUMNS, I
	;.byte >(SPEED_MIN+(((SPEED_MAX-SPEED_MIN)*I)/(COLUMNS-1)))
	.byte >(SPEED_MIN+(((SPEED_MAX-SPEED_MIN)*(I*I))/((COLUMNS-1)*(COLUMNS-1))))
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

; modulo of each tile column, baked into tables

;modt1:
;.repeat 8, I
;	.byte BASE1 + ((I+(8-8)) .MOD  8)
;.endrepeat

modt2:
.repeat 16, I
	.byte BASE2 + ((I+(16-8)) .MOD 16)
.endrepeat
.repeat 16, I
	.byte BASE2 + 16 + ((I+(16-8)) .MOD 16)
.endrepeat

modt3:
.repeat 24, I
	.byte BASE3 + ((I+(24-8)) .MOD 24)
.endrepeat
.repeat 24, I
	.byte BASE3 + 24 + ((I+(24-8)) .MOD 24)
.endrepeat
.repeat 24, I
	.byte BASE3 + 48 + ((I+(24-8)) .MOD 24)
.endrepeat

modt4:
.repeat 32, I
	.byte BASE4 + ((I+(32-8)) .MOD 32)
.endrepeat
.repeat 32, I
	.byte BASE4 + 32 + ((I+(32-8)) .MOD 32)
.endrepeat
.repeat 32, I
	.byte BASE4 + 64 + ((I+(32-8)) .MOD 32)
.endrepeat
.repeat 32, I
	.byte BASE4 + 96 + ((I+(32-8)) .MOD 32)
.endrepeat

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
		; copy rot to first row of nmt
		ldy width, X
		:
			sty i
			ldy j ; tile index
			clc
			adc baset, Y
			sta nmt, Y
			sec
			sbc baset, Y
			iny
			sty j
			ldy i
			dey
			bne :-
		inx
		cpx #COLUMNS
		bcc @loop
	rts

.macro NMT_ROW1 nmt_, index_
	lda nmt_+index_, Y
	sta nmt_+index_+32, Y
.endmacro

.macro NMT_ROW2 nmt_, index_
	ldx nmt_+index_, Y
	lda modt2-BASE2, X
	sta nmt_+index_+32, Y
	ldx nmt_+index_+1, Y
	lda modt2-BASE2, X
	sta nmt_+index_+33, Y
.endmacro

.macro NMT_ROW3 nmt_, index_
	ldx nmt_+index_, Y
	lda modt3-BASE3, X
	sta nmt_+index_+32, Y
	ldx nmt_+index_+1, Y
	lda modt3-BASE3, X
	sta nmt_+index_+33, Y
	ldx nmt_+index_+2, Y
	lda modt3-BASE3, X
	sta nmt_+index_+34, Y
.endmacro

.macro NMT_ROW4 nmt_, index_
	ldx nmt_+index_, Y
	lda modt4-BASE4, X
	sta nmt_+index_+32, Y
	ldx nmt_+index_+1, Y
	lda modt4-BASE4, X
	sta nmt_+index_+33, Y
	ldx nmt_+index_+2, Y
	lda modt4-BASE4, X
	sta nmt_+index_+34, Y
	ldx nmt_+index_+3, Y
	lda modt4-BASE4, X
	sta nmt_+index_+35, Y
.endmacro

nmt_row0:
	; Y = first tile
	.repeat COL1, I
		NMT_ROW1 nmt, I
	.endrepeat
	.repeat COL2, I
		NMT_ROW2 nmt, (COL1+(2*I))
	.endrepeat
	.repeat COL3, I
		NMT_ROW3 nmt, (COL1+(2*COL2)+(3*I))
	.endrepeat
	.repeat COL4, I
		NMT_ROW4 nmt, (COL1+(2*COL2)+(3*COL3)+(4*I))
	.endrepeat
	tya
	clc
	adc #32
	tay
	rts

nmt_row1:
	; Y = first tile
	.repeat COL1, I
		NMT_ROW1 nmt+256, I
	.endrepeat
	.repeat COL2, I
		NMT_ROW2 nmt+256, (COL1+(2*I))
	.endrepeat
	.repeat COL3, I
		NMT_ROW3 nmt+256, (COL1+(2*COL2)+(3*I))
	.endrepeat
	.repeat COL4, I
		NMT_ROW4 nmt+256, (COL1+(2*COL2)+(3*COL3)+(4*I))
	.endrepeat
	tya
	clc
	adc #32
	tay
	rts

nmt_rows:
	ldy #0
	.repeat (256/32)
		jsr nmt_row0
	.endrepeat
	.assert (ROWST*32)>256, error, "ROWST size too large"
	.repeat ROWST-(1+(256/32))
		jsr nmt_row1
	.endrepeat
	.assert (ROWST*32)<512, error, "ROWST size too lage"
	rts

.segment "ALIGN"

; upload nmt buffer to $2007
; aligned for fixed timing
.align 256
nmt_apply:
	; would normally unroll this, but the 48 line forced blank is pretty generous
	ldx #0
	:
		lda nmt, X
		sta $2007
		inx
		bne :-
	.assert (ROWST*32)>256, error, "ROWST size too large"
	:
		lda nmt+256, X
		sta $2007
		inx
		cpx #((ROWST*32)-256)
		bcc :-
	.assert (ROWST*32)<512, error, "ROWST size too lage"
	rts

; X/Y scroll split at a fixed x/y/nametable
.macro SCROLL_SPLIT x_, y_, n_
	lda #((y_ & %11000000) >> 6) | ((y_ & %00000011) << 4) | (n_ << 2)
	sta $2006
	lda #y_
	sta $2005
	lda #x_
	sta $2005
	lda #((x_ & %11111000) >> 3) | ((y_ & %00111000) << 2)
	sta $2006
.endmacro

; timed rendering code with forced black
; 16:9 letterboxing (48 lines of blank at top and bottom)
; aligned for fixed timing
.align 256
render:
	; forced blank
	lda #%00000000
	sta $2001
	lda pal
	beq :+
		jsr oam_dma ; PAL should OAM DMA at start of NMI blank
	:
	bit $2002
	lda #$20
	sta $2006
	lda #$00
	sta $2006
	jsr nmt_apply
	jsr render_wait0
	lda pal
	bne :+
		jsr oam_dma ; NTSC should OAM DMA at end of forced blank
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
	SCROLL_SPLIT 8,48,0
	; unblank
	lda #%00011010
	sta $2001 ; line 47 hblank > dot 257
	.assert ROWST=12, error, "render timing needs adjustment"
	jsr render_wait1
	SCROLL_SPLIT 8,0,0  ; line 95 hblank > dot 257
	rts

.if USE_DPCM
; DPCM sample fetch adds cycles, delays must be adjusted
; These are tuned specifically for sample rate $E

render_wait0:
	lda pal
	bne @pal
@ntsc:
	jsr delay_1536
	jsr delay_192
	jsr delay_48
	jsr delay_48
	jsr delay_12
	rts
@pal:
	jsr delay_6144
	jsr delay_384
	jsr delay_96
	jsr delay_12
	rts

render_wait1:
	lda pal
	bne @pal
@ntsc:
	jsr delay_3072
	jsr delay_1536
	jsr delay_768
	rts
@pal:
	jsr delay_3072
	jsr delay_1536
	jsr delay_384
	jsr delay_24
	jsr delay_12
	nop
	nop
	nop
	nop
	rts

.else

render_wait0:
	lda pal
	bne @pal
@ntsc:
	jsr delay_1536
	jsr delay_192
	jsr delay_96
	jsr delay_48
	nop
	nop
	nop
	nop
	nop
	rts
@pal:
	jsr delay_6144
	jsr delay_384
	jsr delay_192
	nop
	rts

render_wait1:
	lda pal
	bne @pal
@ntsc:
	jsr delay_3072
	jsr delay_1536
	jsr delay_768
	jsr delay_24
	jsr delay_12
	nop
	rts
@pal:
	jsr delay_3072
	jsr delay_1536
	jsr delay_384
	jsr delay_48
	jsr delay_24
	nop
	nop
	nop
	nop
	nop
	rts

.endif

oam_dma:
	lda #0
	sta $2003
	lda #>oam
	sta $4014
	rts

; some compact cycle delay routines
delay_24576: jsr delay_12288
delay_12288: jsr delay_6144
delay_6144:  jsr delay_3072
delay_3072:  jsr delay_1536
delay_1536:  jsr delay_768
delay_768:   jsr delay_384
delay_384:   jsr delay_192
delay_192:   jsr delay_96
delay_96:    jsr delay_48
delay_48:    jsr delay_24
delay_24:    jsr delay_12
delay_12:    rts

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
	lda nmi_now
	bne :+
		lda #1
		sta nmi_now
		jsr render
		jsr animate
		jsr nmt_rows
		; DO sound
		lda #0
		sta nmi_now
	:
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
	; wipe nametables
	bit $2002
	lda #$20
	sta $2006
	lda #$00
	sta $2006
	;lda #0
	tax
	ldy #16
	:
		sta $2007
		inx
		bne :-
		dey
		bne :-
	; load palettes
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
	ldy #3
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
	; place 8 black blocking sprites on line 192
	ldx #0
	:
		lda #191
		sta oam+0, X
		lda #$30 ; a conveniently solid tile
		sta oam+1, X
		lda #$00
		sta oam+2, X
		sta oam+3, X
		inx
		inx
		inx
		inx
		cpx #(8*4)
		bcc :-
	; turn on NMI
	lda #%10000000
	sta $2000
	; HACK test of DPCM
	.if USE_DPCM
		lda #$4E
		sta $4010
		;lda #scream_dpcm
		lda #silent_dpcm
		sta $4012
		;lda #$FF
		lda #0
		sta $4013
		lda #$10
		sta $4015
	.endif
	; infinite loop
:
	jmp :-

; =======
; NES ROM
; =======

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
