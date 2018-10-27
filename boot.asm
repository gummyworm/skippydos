.include "irq.inc"
.include "zeropage.inc"
.macpack cbm

BITMAP_ADDR = $1100
COLMEM_ADDR = $9400

ESCAPE_CHARACTER = $ff
ESCAPE_RVS_ON = $01
ESCAPE_RVS_OFF = $02
STATUS_LINE = 23
STATUS_COL  = 0

.setcpu "6502"

; screenbuff holds a virtual representation of the screen

.BSS
.org $2000
screenbuff: .res 25*40
linebuffer: .res 40
curx: .byte 0
cury: .byte 0
rownum: .byte 0

.export memstart
memstart:

.CODE
;.org $1ffe
;.word $2000
.org $a000

;--------------------------------------
vectab:
	.word boot
	.word redraw

;--------------------------------------
boot:
	lda #$aa
	sta $2000
	lda $2000
	cmp #$aa
	beq start

;--------------------------------------
; no expansion detected- error out
error:
	lda #$2a
	sta $900f
	lda #(1<<1)
	sta $9003
	lda #$c0
	sta $9005

@cls:	ldx #$00
	lda #$20
:	sta $1200,x
	dex
	bne :-
@l0:	lda @errmsg,x
	sta $1200,x
	inx
	cpx #@errlen
	bne @l0
	jmp *
@errmsg: 
	scrcode ">=8K MEMORY REQUIRED"
@errlen=*-@errmsg

;--------------------------------------
start:
@clrdst=zp::tmp0
	clc
        lda #$10
        tay
@0:     sta $0ff0,y
        adc #$0c
        bcc @1
        sbc #$ef
@1:     iny
        bne @0

	; clear color and bitmap
	lda #>BITMAP_ADDR
	sta @clrdst+1
	ldx #$0f
@2:	lda #$00
	sta @clrdst
	sta (@clrdst),y
	lda #$07
	sta $9400,y
	sta $9500,y
	dey
	bne @2
	inc @clrdst+1
	dex
	bne @2

	lda #$cc
	sta $9005
	lda #$08
	sta $900f

	lda #$20
	sta $0288	; set HIBASE, point screen buffer to $2000.

	jsr $c642
	rts
	;jmp ($c000)	; BASIC coldstart

;--------------------------------------
.export __bm_columns
__bm_columns: 
.word $1100
.word $11c0
.word $1280
.word $1340
.word $1400
.word $14c0
.word $1580
.word $1640
.word $1700
.word $17c0
.word $1880
.word $1940
.word $1a00
.word $1ac0
.word $1b80
.word $1c40
.word $1d00
.word $1dc0
.word $1e80
.word $1f40

;--------------------------------------
.proc topetscii
	cmp #$20
	bcs :+
	adc #$40	; $00-$1f
	rts
:	cmp #$40
	bcs :+
	rts		; $20-$3f
:	cmp #$60
	bcs :+
	adc #$80	; $40-$5f
	rts
:	cmp #$80
	bcs :+
	adc #$40	; $60-$7f
	rts
:	rts
.endproc

;--------------------------------------
_t40_puts:
txtbyte  = $24
txtleft  = $25
txtright = $27
txtdst   = $29
txtsrc   = $4e
@rvs = $50
        sta txtsrc
        stx txtsrc+1

	lda rownum
        asl
        asl
        asl
        sta txtdst
        lda #$11
        sta txtdst+1

        ldy #$00
@l0:    lda #$00
	sta @rvs
	lda (txtsrc),y
	jsr topetscii

	asl
	rol @rvs
	lsr

        iny
        sta txtleft
        lda #$00
        asl txtleft
        rol
        asl txtleft
        rol
        asl txtleft
        rol
        sta txtleft+1
        clc
        lda txtleft
        adc #<((__text_charmap-256) .mod 256)
        sta txtleft
        lda txtleft+1
        adc #<((__text_charmap-256) / 256)
        sta txtleft+1
        lda (txtsrc),y
	jsr topetscii

	asl
	rol @rvs
	lsr

        iny
        sta txtright
        lda #$00
        asl txtright
        rol
        asl txtright
        rol
        asl txtright
        rol
        sta txtright+1
        clc
        lda txtright
        adc #<((__text_charmap-256) .mod 256)
        sta txtright
        lda txtright+1
        adc #<((__text_charmap-256) / 256)
        sta txtright+1
        tya
        pha
        ldy #$00
@l1:    lda (txtleft),y
        and #$f0
        sta txtbyte
        lda (txtright),y
        and #$0f
        ora txtbyte

	ldx @rvs
	eor rvstab,x

        sta (txtdst),y
        iny
        cpy #8
        bne @l1 
        pla
        tay
        clc
        lda txtdst
        adc #192
        sta txtdst
        lda txtdst+1
        adc #0
        sta txtdst+1
	cpy #40
	bcs :+
	jmp @l0

:	rts

rvstab:
.byte $00,$0f,$f0,$ff

.export __text_charmap
.align 256
__text_charmap:
.byte   0,   0,   0,   0,   0,   0,   0,   0
;.byte   0,  34,  34,  34,  34,   0,  34,   0

.byte %010101010
.byte %011101110
.byte %011101110
.byte %011101110
.byte %011101110
.byte %011101110
.byte %001000100
.byte %000000000

.byte   0,  85,  85,   0,   0,   0,   0,   0
.byte   0,  85, 119,  85,  85, 119,  85,   0
.byte   0,  34,  51, 102,  51, 102,  34,   0
.byte   0,  85,  17,  34,  34,  68,  85,   0
.byte   0, 102, 102,  51, 102, 102,  51,   0
.byte  34,  34,   0,   0,   0,   0,   0,   0
.byte   0,  17,  34,  34,  34,  34,  17,   0
.byte   0,  68,  34,  34,  34,  34,  68,   0
.byte   0,   0,   0,  85,  34,  85,   0,   0
.byte   0,   0,   0,  34, 119,  34,   0,   0
.byte   0,   0,   0,   0,   0,   0,  34,  68
.byte   0,   0,   0,   0, 119,   0,   0,   0
.byte   0,   0,   0,   0,   0,   0,  34,   0
.byte   0,  17,  17,  34,  34,  68,  68,   0
.byte   0, 119,  85,  85,  85,  85, 119,   0
.byte   0,  17,  17,  17,  17,  17,  17,   0
.byte   0, 119,  17, 119,  68,  68, 119,   0
.byte   0, 119,  17, 119,  17,  17, 119,   0
.byte   0,  85,  85, 119,  17,  17,  17,   0
.byte   0, 119,  68, 119,  17,  17, 119,   0
.byte   0, 119,  68, 119,  85,  85, 119,   0
.byte   0, 119,  17,  17,  17,  17,  17,   0
.byte   0, 119,  85, 119,  85,  85, 119,   0
.byte   0, 119,  85, 119,  17,  17, 119,   0
.byte   0,   0,   0,  34,   0,   0,  34,   0
.byte   0,   0,   0,  34,   0,   0,  34,  68
.byte   0,   0,  17,  34,  68,  34,  17,   0
.byte   0,   0,   0, 119,   0, 119,   0,   0
.byte   0,   0,  68,  34,  17,  34,  68,   0
.byte   0,  34,  85,  17,  34,   0,  34,   0
.byte   0,  51,  85,  85,  85,  68,  51,   0
.byte   0,  34,  85,  85, 119,  85,  85,   0
.byte   0, 102,  85, 102,  85,  85, 102,   0
.byte   0,  51,  68,  68,  68,  68,  51,   0
.byte   0, 102,  85,  85,  85,  85, 102,   0
.byte   0, 119,  68, 102,  68,  68, 119,   0
.byte   0, 119,  68, 102,  68,  68,  68,   0
.byte   0,  51,  68,  68,  85,  85,  51,   0
.byte   0,  85,  85, 119,  85,  85,  85,   0
.byte   0, 119,  34,  34,  34,  34, 119,   0
.byte   0,  51,  17,  17,  17,  85,  34,   0
.byte   0,  85,  85, 102,  85,  85,  85,   0
.byte   0,  68,  68,  68,  68,  68, 119,   0
.byte   0,  85, 119,  85,  85,  85,  85,   0
.byte   0, 102,  85,  85,  85,  85,  85,   0
.byte   0,  34,  85,  85,  85,  85,  34,   0
.byte   0, 102,  85,  85, 102,  68,  68,   0
.byte   0,  34,  85,  85,  85, 102,  51,   0
.byte   0, 102,  85,  85, 102,  85,  85,   0
.byte   0,  51,  68,  34,  17,  17, 102,   0
.byte   0, 119,  34,  34,  34,  34,  34,   0
.byte   0,  85,  85,  85,  85,  85,  51,   0
.byte   0,  85,  85,  85,  85,  34,  34,   0
.byte   0,  85,  85,  85,  85, 119,  85,   0
.byte   0,  85,  85,  34,  85,  85,  85,   0
.byte   0,  85,  85,  85,  34,  34,  34,   0
.byte   0, 119,  17,  34,  34,  68, 119,   0
.byte   0,  51,  34,  34,  34,  34,  51,   0
.byte   0,  68,  68,  34,  34,  17,  17,   0
.byte   0, 102,  34,  34,  34,  34, 102,   0
.byte  34,  85,   0,   0,   0,   0,   0,   0
.byte   0,   0,   0,   0,   0,   0,   0, 255
.byte  34,  17,   0,   0,   0,   0,   0,   0
.byte   0,   0,   0,  51,  85,  85,  51,   0
.byte   0,  68,  68, 102,  85,  85, 102,   0
.byte   0,   0,   0,  51,  68,  68,  51,   0
.byte   0,  17,  17,  51,  85,  85,  51,   0
.byte   0,   0,   0,  34,  85, 102,  51,   0
.byte   0,  17,  34, 119,  34,  34,  34,   0
.byte   0,   0,   0,  51,  85,  51,  17, 102
.byte   0,  68,  68, 102,  85,  85,  85,   0
.byte   0,  34,   0,  34,  34,  34,  34,   0
.byte   0,  34,   0,  34,  34,  34,  34,  68
.byte   0,  68,  68,  85, 102,  85,  85,   0
.byte   0,  34,  34,  34,  34,  34,  34,   0
.byte   0,   0,   0,  85, 119,  85,  85,   0
.byte   0,   0,   0, 102,  85,  85,  85,   0
.byte   0,   0,   0,  34,  85,  85,  34,   0
.byte   0,   0,   0, 102,  85, 102,  68,  68
.byte   0,   0,   0,  51,  85,  51,  17,  17
.byte   0,   0,   0, 102,  85,  68,  68,   0
.byte   0,   0,   0,  51, 102,  51, 102,   0
.byte   0,  34,  34, 119,  34,  34,  51,   0
.byte   0,   0,   0,  85,  85,  85,  51,   0
.byte   0,   0,   0,  85,  85,  34,  34,   0
.byte   0,   0,   0,  85,  85, 119,  85,   0
.byte   0,   0,   0,  85,  34,  34,  85,   0
.byte   0,   0,   0,  85,  85,  51,  17, 102
.byte   0,   0,   0, 119,  17,  34, 119,   0
.byte   0,  51,  34,  68,  34,  34,  51,   0
.byte   0,  34,  34,   0,  34,  34,  34,   0
.byte   0, 102,  34,  17,  34,  34, 102,   0
.byte   0,   0,   0,  85, 170,   0,   0,   0
.byte   0,   0,   0,   0,   0,   0,   0,   0
;CUSTOM CHARS. starting @ 128
.byte %010101010
.byte %011101110
.byte %011101110
.byte %011101110
.byte %011101110
.byte %011101110
.byte %001000100
.byte %000000000

.byte   $44,$44,$44,$44,$44,$44,$44,$44        ; |
.byte   $44,$44,$44,$44,$44,$44,$44,$44        ; |
.byte   $ff,$00,$00,$00,$00,$00,$00,$ff
.byte   $88,$88,$88,$88,$88,$88,$88,$88
.byte   $ff,$00,$00,$00,$00,$00,$00,$00

;--------------------------------------
; hiline highlights the row in .A with the color in .X
.export __text_hiline
.proc __text_hiline
	stx @hicolor
	ldx #<@hiirq
	ldy #>@hiirq
	asl
	asl
	adc #13
	jsr irq::raster
	rts

@hiirq: ldx #65/5-1
	dex
	bne *-1
@hicolor=*+1
	lda #$00
	sta $900f
	ldx #(65)/5*8-3
:	dex
	bne :-
	nop
	lda #$08
	sta $900f
	jmp $eabf
.endproc

;--------------------------------------
.proc redraw
@src=zp::tmp0
	lda #$00
	sta @src+1
	lda $D6	; only redraw the line that the cursor is on
	sta rownum
	asl
	asl
	asl
	adc rownum
	adc rownum
	asl
	rol @src+1
	asl
	rol @src+1
	sta @src

	lda @src+1
	adc #>screenbuff
	sta @src+1

	lda @src
	ldx @src+1
	jsr _t40_puts
	rts
.endproc

;--------------------------------------
chrget:


.export end
end:
.res $2000-(end-vectab)-$67
