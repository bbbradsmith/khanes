;
; khan.s
; NES tribute to KHAN
; Brad Smith, 2019
; http;//rainwarrior.ca
;

; ===
; RAM
; ===

.segment "ZEROPAGE"

.segment "BSS"

.segment "OAM"
.align 256
_oam: .res 256

.segment "CODE"

nmi:
	rti

irq:
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
	; TODO preserve positions
	lda #0 ; clear RAM
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
	; TODO restore positions
	; clear stack, separately wipe OAM
	lda #0
	:
		sta $0100, X
		inx
		bne :-
	lda #$FF
	:
		sta _oam, X
		inx
		bne :-
	; wait for vblank #2
	:
		bit $2002
		bpl :-
	; begin
:
	jmp :- ; TODO

; =======
; NES ROM
; =======

.segment "CHR"

.incbin "khan.chr"

.segment "HEADER"

INES_MAPPER     = 0 ; NROM
INES_MIRROR     = 1 ; horizontal nametables
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
