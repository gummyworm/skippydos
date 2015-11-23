; da65 V2.13.2 - (C) Copyright 2000-2009,  Ullrich von Bassewitz
; Created:    2011-06-07 21:53:11
; Input file: kernal.901486-06.bin
; Page:       1
;
; assemble command:
; cl65 --start-addr 0xe000 -t none thisfile.a65 -o outputfile.bin
;
;
; IMPORTANT!
; if this isn't defined, the kernal is assembled for PAL
;

; uncomment for NTSC
NTSC = 1

        .setcpu "6502"

L0014           := $0014
L0079           := $0079
L028F           := $028F
L0314           := $0314
L0316           := $0316
L0318           := $0318
L031A           := $031A
L031C           := $031C
L031E           := $031E
L0320           := $0320
L0322           := $0322
L0324           := $0324
L0326           := $0326
L0328           := $0328
L032A           := $032A
L032C           := $032C
L0330           := $0330
L0332           := $0332
L0DA3           := $0DA3
L1D3B           := $1D3B
L2026           := $2026
L2A2A           := $2A2A
L3256           := $3256
L414F           := $414F
L4154           := $4154
L4243           := $4243
L435A           := $435A
L4552           := $4552
L4C50           := $4C50
L4E4F           := $4E4F
L5245           := $5245
L5246           := $5246
L5942           := $5942
L91CC           := $91CC
LA000           := $A000
LA002           := $A002
LC000           := $C000
LC002           := $C002
LC408           := $C408
LC437           := $C437
LC474           := $C474
LC52A           := $C52A
LC533           := $C533
LC644           := $C644
LC663           := $C663
LC677           := $C677
LC67A           := $C67A
LC68E           := $C68E
LCB1E           := $CB1E
LCD8A           := $CD8A
LCD9E           := $CD9E
LCEFD           := $CEFD
LCF08           := $CF08
LD6A3           := $D6A3
LD79E           := $D79E
LD7F7           := $D7F7
LD849           := $D849
LD850           := $D850
LD853           := $D853
LD867           := $D867
LD8D7           := $D8D7
LDA28           := $DA28
LDAB9           := $DAB9
LDAD4           := $DAD4
LDB07           := $DB07
LDB0F           := $DB0F
LDBA2           := $DBA2
LDBC7           := $DBC7
LDBCA           := $DBCA
LDBD4           := $DBD4
LDC0C           := $DC0C
LDC2B           := $DC2B
LDCCC           := $DCCC
LDDCD           := $DDCD
LDFB4           := $DFB4
        .byte   $0F
        .byte   $DC
        lda     $61
        cmp     #$88
        bcc     LE00B
LE008:  jsr     LDAD4
LE00B:  jsr     LDCCC
        lda     $07
        clc
        adc     #$81
        beq     LE008
        sec
        sbc     #$01
        pha
        ldx     #$05
LE01B:  lda     $69,x
        ldy     $61,x
        sta     $61,x
        sty     $69,x
        dex
        bpl     LE01B
        lda     $56
        sta     $70
        jsr     LD853
        jsr     LDFB4
        lda     #$C4
        ldy     #$DF
        jsr     LE056
        lda     #$00
        sta     $6F
        pla
        jsr     LDAB9
        rts
LE040:  sta     $71
        sty     $72
        jsr     LDBCA
        lda     #$57
        jsr     LDA28
        jsr     LE05A
        lda     #$57
        ldy     #$00
        jmp     LDA28
LE056:  sta     $71
        sty     $72
LE05A:  jsr     LDBC7
        .byte   $B1
LE05E:  adc     ($85),y
        .byte   $67
        ldy     $71
        iny
        tya
        bne     LE069
        inc     $72
LE069:  sta     $71
        ldy     $72
LE06D:  jsr     LDA28
        lda     $71
        ldy     $72
        clc
        adc     #$05
        bcc     LE07A
        iny
LE07A:  sta     $71
        sty     $72
        jsr     LD867
        lda     #$5C
        ldy     #$00
        dec     $67
        bne     LE06D
        rts
        tya
        and     $44,x
        .byte   $7A
        brk
        pla
        plp
        lda     ($46),y
        brk
        jsr     LDC2B
        bmi     LE0D0
        bne     LE0BB
        jsr     LFFF3
        stx     $22
        sty     $23
        ldy     #$04
        lda     ($22),y
        sta     $62
        iny
        lda     ($22),y
        sta     $64
        ldy     #$08
        lda     ($22),y
        sta     $63
        iny
        lda     ($22),y
        sta     $65
        jmp     LE0E0
LE0BB:  lda     #$8B
        ldy     #$00
        jsr     LDBA2
        lda     #$8A
        ldy     #$E0
        jsr     LDA28
        lda     #$8F
        ldy     #$E0
        jsr     LD867
LE0D0:  ldx     $65
        lda     $62
        sta     $65
        stx     $62
        ldx     $63
        lda     $64
        sta     $63
        stx     $64
LE0E0:  lda     #$00
        sta     $66
        lda     $61
        sta     $70
        lda     #$80
        sta     $61
        jsr     LD8D7
        ldx     #$8B
        ldy     #$00
LE0F3:  jmp     LDBD4
;--------------------------------------
BIOERROR:
;handle I/O error in BASIC
; This routine is called whenever BASIC wishes to call one of the KERNAL 
; I/O routines. It is also used to handle I/O errors in BASIC.
;
LE0F6:  cmp     #$F0
        bne     LE101
        sty     $38
        stx     $37
        jmp     LC663
LE101:  tax
        bne     LE106
        ldx     #$1E
LE106:  jmp     LC437
;--------------------------------------
BCHOUT:
;output character
; This routine uses the KERNAL rutine CHROUT to output the character in
; (A) to an available output channel. A test is made for a possible I/O error.
;
LE109:  jsr     LFFD2
        bcs     LE0F6
        rts

;--------------------------------------
BCHIN:
;input character
; This routine uses the KERNAL routine CHRIN to input a character to (A) from
; an available input channel. A test is made for a possible I/O error.
        jsr     LFFCF
        bcs     LE0F6
        rts

;--------------------------------------
BCKOUT:
;set up for output
; This routine uses the KERNAL routine CHKOUT to open an output channel,
; and tests for possible I/O error. On entry (X) must hold the the logical
; file number as used in OPEN.;
;
        jsr     LFFC9
        bcs     LE0F6
        rts

;-------------------------------------
BCKIN:
;set up for input
; This routine uses the KERNAL routine CHKIN to open an input channel.
; A test as made for possible I/O error.
;
        jsr     LFFC6
        bcs     LE0F6
        rts

;--------------------------------------
BGETIN:
;get one character
; This routine uses the KERNAL routine GETIN to get a character from
; the keyboard buffer into (A). A test is made for possible I/O error.
        jsr     LFFE4
        bcs     LE0F6
        rts
;--------------------------------------
SYS:
;perform SYS
;This routine enables machine language routines to be executed from
; BASIC. The routine evaluates the address and confirms that it is a
; numeric number. The return address is set up, and the user routine
; is executed.
;
        jsr     $CD8A
        jsr     $D7F7
        lda     #$E1
        pha
        lda     #$43
        pha
        lda     $030F
        pha
        lda     $030C
        ldx     $030D
        ldy     $030E
        plp
        jmp     (L0014)
        php
        sta     $030C
        stx     $030D
        sty     $030E
        pla
        sta     $030F
        rts

;--------------------------------------
SAVET:
;perform save
; This routine is sets parameters for save, and calls the save routine.
; The start and end addresses are obtained from TXTTAB and VARTAB. Finally,
; a test is made if any errors ocured.
;
        jsr     LE1D1
        ldx     $2D
        ldy     $2E
        lda     #$2B
        jsr     LFFD8
        bcs     LE0F6
        rts

;--------------------------------------
VERFYT:
;perform save/load
; This routine is essentially the same for both LOAD and VERIFY. The entry
; point determines which is performed, by setting VERCK accordingly. The
; LOAD/VERIFY parameters, filename, device etc. are obtained from text before
; the KERNAL routine LOAD is called. A test is made for I/O errors. At this
; point, the two functios are distiguished. VERIFY reads the the status word
; and prints the message OK or ?VERIFY error depending on the result of the test.
; LOAD reads the I/O status word for a possible ?LOAD error, then updates the
; pointers to text and variables, exiting via CLR.
; 
        lda     #$01		;flag verify
        bit     a:$A9		;mask, will execute lda #$01 if address $e168
        sta     $0A			;store in VRECK, LOAD/VERIFY flag
        jsr     LE1D1		;get LOAD/VERIFY parameters from text
        lda     $0A			;get VRECK
        ldx     $2B			;TXTTAB, start of BASIC
        ldy     $2C
        jsr     LFFD5		;execute LOAD, KERNAL routine
        bcs     LE1CE		;if carry set, handle error
        lda     $0A			;test VRECK for LOAD or VERIFY
        beq     LE195		;do LOAD
LE17B:  ldx     #$1C		;set error $1C, VERIFY error
        jsr     LFFB7		;do READST, get status I/O word
        and     #$10		;%00010000, test for mismatch
        beq     LE187		;data match, continue
        jmp     LC437		;data mismatch, do error
LE187:  lda     $7A			;<TXTPTR
        cmp     #$02
        beq     LE194
        lda     #$64		;set address to text OK
        ldy     #$C3		;at $a364
        jmp     LCB1E		;output string in (A/Y)
LE194:  rts
LE195:  jsr     LFFB7		;do READST, get status I/O for LOAD
        and     #$BF		;%10111111, test all but EOI
        beq     LE1A1		;nope, no errors
        ldx     #$1D		;set error $1D, LOAD error
        jmp     LC437		;do error
LE1A1:  lda     $7B			;>TXTPTR
        cmp     #$02
        bne     LE1B5
LE1A7:  stx     $2D			;set VARTAB, start of variables
        sty     $2E
        lda     #$76		;set address to text READY
        ldy     #$C3		;at $C376
        jsr     LCB1E		;output string in (A/Y)
        jmp     LC52A		;do CLR and restart BASIC
LE1B5:  jsr     LC68E		;reset TXTPTR
        jmp     LE800

;--------------------------------------
OPENT:
;perform open
; This routine extracts paramerters from text and performs the OPEN
; routine in KERNAL. A test is made for I/O errors.
;
        jsr     LE216
        jsr     LFFC0
        bcs     LE1CE
        rts

;--------------------------------------
CLOSET:
;perform CLOSE
; The parameters for CLOSE are obtained from text, and the logical
; filenumber placed in (A), The KERNAL routine CLOSE is performed,
; and a test is made for I/O errors.
;
        jsr     LE216
        lda     $49
        jsr     LFFC3
        bcc     LE194
LE1CE:  jmp     LE0F6

;--------------------------------------
SLPARA:
;get parameters for LOAD/SAVE
;This routine gets the filename, devicenumber and secondary address for
; LOAD/VERIFY and SAVE operations. The KERNAL routines SETNAM and SETLFS
; are used to do this. Default parameters are set up, and a new JiffyDOS
; routine is called at $xxxx. It jumps to $xxxx where the original SETLFS
; is performed, but also makes a test to find the first serial device number,
; and pokes it into FA. Then tests are made if any of the parameters were
; given. If so, these are set up as wanted.
LE1D1:  lda     #$00
        jsr     LFFBD
        ldx     #$01
        ldy     #$00
        jsr     LF7BD           ; SETFLS, and device number in new JiffyDOS routine
        jsr     LE203
        jsr     LE254
        jsr     LE203
        jsr     LE1FD
        ldy     #$00
        stx     $49
        jsr     LFFBA
        jsr     LE203
        jsr     LE1FD
        txa
        tay
        ldx     $49
        jmp     LFFBA

;--------------------------------------
COMBYT:
;get next one-byte parameter
; This routine checks if the next character of text is a comma, and then
; inputs the parameter following into (X).
;
LE1FD:  jsr     LE20B
        jmp     LD79E

;--------------------------------------
DEFLT:
;check default parameters
; This routine tests CHRGOT to see if a optional parameter was included in
; the text. If it was, a normal exit is performed via RTS. If not, the return
; address on the stack is discarded, and the routine exits both this and the
; calling routine.
;
LE203:  jsr     L0079
        bne     LE20A
        pla
        pla
LE20A:  rts

;--------------------------------------
CMMERR:
;check for comma
; This routine confirms that the next character in the text is a comma. It also
; test that the comma is not immediately followed by a terminator. If so, exit
; and do SYNTAX error.
;
LE20B:  jsr     LCEFD
LE20E:  jsr     L0079
        bne     LE20A
        jmp     LCF08

;--------------------------------------
OCPARA:
;get parameters for OPEN/CLOSE
; This routine gets the logical file number, device number, secondary
; address and filename for OPEN/CLOSE. Initially the default filename
; is set to null, and the device number to #1. The logical filenumber
; is compulsory, and is obtained from text and placed in <FORPNT. The
; other parameters are optinal and are obtained if present. The device
; number is stored in >FORPNT. The parameters are set via the KERNAL
; routines SETNAM and SETLFS.A
;
LE216:  lda     #$00
        jsr     LFFBD
        jsr     LE20E
        jsr     LD79E
        stx     $49
        txa
        ldx     #$01
LE226:  ldy     #$00
        jsr     LFFBA
        jsr     LE203
        jsr     LE1FD
        stx     $4A
        ldy     #$00
        lda     $49
        cpx     #$03
        bcc     LE23C
        dey
LE23C:  jsr     LFFBA
        jsr     LE203
        jsr     LE1FD
        txa
        tay
        ldx     $4A
        lda     $49
        jsr     LFFBA
        jsr     LE203
        jsr     LE20B
LE254:  jsr     LCD9E
LE257:  jsr     LD6A3
        ldx     $22
        ldy     $23
        jmp     LFFBD

;--------------------------------------
COS:
;perform COS
; This routine manipulates the input COS to be calcuated with SIN. COS(X)
; = SIN(X+pi/2), where  X is in radians. We use it as Fac#1=SIN(fac#1+pi/2),
; ie pi/2 is added to fac#1 and the following SIN is performed.
;
        lda     #$DD
        ldy     #$E2
        jsr     LD867

;--------------------------------------
SIN:
;perform SIN
;
LE268:  jsr     LDC0C
        lda     #$E2
        ldy     #$E2
        ldx     $6E
        jsr     LDB07
        jsr     LDC0C
        jsr     LDCCC
        lda     #$00
        sta     $6F
        jsr     LD853
        lda     #$E7
        ldy     #$E2
        jsr     LD850
        lda     $66
        pha
        bpl     LE29A
        jsr     LD849
        lda     $66
        bmi     LE29D
        lda     $12
        eor     #$FF
        sta     $12
LE29A:  jsr     LDFB4
LE29D:  lda     #$E7
        ldy     #$E2
        jsr     LD867
        pla
        bpl     LE2AA
        jsr     LDFB4
LE2AA:  lda     #$EC
        ldy     #$E2
        jmp     LE040

;--------------------------------------
TAN:
;perform TAN
;
        jsr     LDBCA
        lda     #$00
        sta     $12
        jsr     LE268
        ldx     #$4E
        ldy     #$00
        jsr     LE0F3
        lda     #$57
        ldy     #$00
        jsr     LDBA2
        lda     #$00
        sta     $66
        lda     $12
        jsr     LE2D9
        lda     #$4E
        ldy     #$00
        jmp     LDB0F
LE2D9:  pha
        jmp     LE29A

;--------------------------------------
; table of trigonometry constants
;
        sta     ($49,x)
        .byte   $0F
        .byte   $DA
        ldx     #$83
        eor     #$0F
        .byte   $DA
        ldx     #$7F
        brk
        brk
        brk
        brk
        ora     $84
        inc     $1A
        and     $861B
        plp
        .byte   $07
        .byte   $FB
        sed
        .byte   $87
        sta     $8968,y
        ora     ($87,x)
        .byte   $23
        and     $DF,x
        sbc     ($86,x)
        lda     $5D
        .byte   $E7
        plp
        .byte   $83
        eor     #$0F
        .byte   $DA
        .byte   $A2
;END OF TRIG CONSTANT TABLE
;--------------------------------------
ATN:
;perform ATN
;
        lda     $66
        pha
        bpl     LE313
        jsr     LDFB4
LE313:  lda     $61
        pha
        cmp     #$81
        bcc     LE321
        lda     #$BC
        ldy     #$D9
        jsr     LDB0F
LE321:  lda     #$3B
        ldy     #$E3
        jsr     LE040
        pla
        cmp     #$81
        bcc     LE334
        lda     #$DD
        ldy     #$E2
        jsr     LD850
LE334:  pla
        bpl     LE33A
        jmp     LDFB4
LE33A:  rts

;--------------------------------------
;ATNCON: table of ATN constants
        .byte   $0B
        ror     $B3,x
        .byte   $83
        lda     $79D3,x
        asl     $A6F4,x
        sbc     $7B,x
        .byte   $83
        .byte   $FC
        bcs     LE35B
        .byte   $7C
        .byte   $0C
        .byte   $1F
        .byte   $67
        dex
        .byte   $7C
        dec     $CB53,x
        cmp     ($7D,x)
        .byte   $14
        .byte   $64
        bvs     LE3A6
        .byte   $7D
LE35B:  .byte   $B7
        nop
        eor     ($7A),y
        adc     $3063,x
        dey
        ror     $927E,x
        .byte   $44
        sta     $7E3A,y
        jmp     L91CC
        .byte   $C7
        .byte   $7F
        tax
        tax
        tax
        .byte   $13
        sta     ($00,x)
        brk
        brk
        brk
;END OF ATN CONSTANT TABLE
        

;--------------------------------------
INIT:
;BASIC cold start
; This is the BASIC cold start routine that is vectored at the very start of
; the BASIC ROM. BASIC vectors and variables are set up, and power-up message
; is output, and BASIC is restarted.
;
LE378:  jsr     LE480           ; initialize JiffyDOS commands and function keys
        jsr     LE3A4           ; Initialize BASIC
        jsr     LE404           ; output power-up message
        ldx     #$FB            ; reset stack
        txs
        jmp     LC474   ; BASIC_START           ; print READY, and restart BASIC


;--------------------------------------
INITAT:
;CHRGET for zeropage
; This is the CHRGET routine which is transferred to RAM starting at
; $0073 on power-up or reset.
;
LE387:  inc     $7A
LE389:  bne     LE38D
LE38B:  inc     $7B
LE38D:  lda     LEA60
        cmp     #$3A
        bcs     LE39E
        cmp     #$20
        beq     LE387
        sec
        sbc     #$30
        sec
        sbc     #$D0
LE39E:  rts

;--------------------------------------
RNDSED:
;random seed for zeropage
; This is the initial value of the seed for the random number function.
; It is copied into RAM from $008b-$008f. Its fltp value is 0.811635157.
;
        .byte   $80
        .byte   $4F
        .byte   $C7
        .byte   $52
        cli
        
;END OF RNDSEED
;--------------------------------------
INITCZ:
;initialize BASIC RAM
;This routine sets the USR jump instruction to point to ?ILLIGAL QUANTITY
; error, sets ADRAY1 and ADRAY2, copies CHRGET and RNDSED to zeropage, sets
; up the start and end locations for BASIC text and sets the first text byte
; to zero.
LE3A4:  lda     #$4C            ; put opcode for JMP at $54
LE3A6:  sta     $54
        sta     $00             ; USRPOK, set USR JMP instruction
        lda     #$48
        ldy     #$D2            ; vector to $D248, ?ILLEGAL QUANTITY
        sta     $01
        sty     $02
        lda     #$91            ; vector to $D391
        ldy     #$D3
        sta     $05             ; store in ADRAY2
        sty     $06
        lda     #$AA            ; vector to D1AA
        ldy     #$D1
        sta     $03     
        sty     $04             ; store in ADRAY1
        ldx     #$1C            ; copy the CHRGET and RNDSED to RAM
LE3C4:  lda     LE387,x         ; source address
        sta     $73,x           ; destination address
        dex                     ; next byte
        bpl     LE3C4           ; til ready
        lda     #$03     
        sta     $53             ; store #3 in FOUR6, garbage collection
        lda     #$00
        sta     $68             ; init BITS, fac#1 overflow
        sta     $13
        sta     $18             ; init LASTPT
        ldx     #$01
        stx     $01FD
        stx     $01FC
        ldx     #$19
        stx     $16             ; TEMPPT, pointer to descriptor stack
        sec                     ; set carry to indicate read mode
        jsr     LFF9C           ; read MEMBOT
        stx     $2B             ; set TXTTAB, bottom of RAM
        sty     $2C
        sec                     ; read mode
        jsr     LFF99           ; read MEMTOP
        stx     $37             ; set MEMSIZ, top of RAM
        sty     $38
        stx     $33             ; set FRETOP = MEMTOP
        sty     $34
        ldy     #$00
        tya
        sta     ($2B),y         ; store zero at start of BASIC
        inc     $2B             ; increment TXTTAB to next memory position
        bne     LE403           ; skip MSB
        inc     $2C
LE403:  rts                     ; return

;--------------------------------------
INITMS:
;output power-up message
; This routine outputs the startup message. It then calcuates the number
; of BASIC bytes free by subatracting the TXTTAB from MEMSIZ, and outputs
; this number. The routine exits via NEW.
LE404:  lda     $2B
        ldy     $2C
        jsr     LC408
        lda     #<POWER_UP_MESSAGE_TEXT
        ldy     #>POWER_UP_MESSAGE_TEXT     ; pointer to first string displayed
        jsr     LCB1E
        lda     $37
        sec
        sbc     $2B
        tax
        lda     $38
        sbc     $2C
        jsr     LDDCD
        lda     #<POWER_UP_MESSAGE
        ldy     #>POWER_UP_MESSAGE    ; pointer to 'BYTES FREE'
        jsr     LCB1E
        jmp     LC644

;--------------------------------------
;VECTORS
;
LE44F:  .word   $F7E6   ; IERROR vector, print BASIC error message
        .word   $C483   ; IMAIN vector, BASIC warm start
        .word   $EAF2   ; ICRNCH vector, tokenize BASIC 
        .word   $C71A   ; IQPLOP vector, list BASIC text
        .word   $C7E4   ; IGONE vector, BASIC character dispatch
        .word   $CE86   ; IEVAL vector, evaluate BASIC token 


;--------------------------------------
INIT_JIFFY_COMMANDS:
; This routine transfers the vectors $0300-$030b to set up the
; JiffyDOS commands.
;
LE45B:  ldx     #$0B
LE45D:  lda     LE44F,x
        sta     $0300,x
        dex
        bpl     LE45D
        rts

;--------------------------------------
POWER_UP_MESSAGE:
;
.byte " BYTES FREE",$0d,0
POWER_UP_MESSAGE_TEXT:
.byte " IFFYDOS 2012 CMD ETC",$0d,0
.res 8                  ; FREE

;--------------------------------------
BASSFT:
;BASIC warm start
; This is the BASIC warm start routine that is vectored at the
; very start of the BASIC ROM. The routine is called by the 6502
; BRK instruction, or STOP/RESTORE being pressed. It outputs the
; READY prompt via the IERROR vector at $0300. The original IERROR
; vector points to $e38b, but JiffyDOS uses the error routine as
; an input to check new commands. If the error code, in (X) is larger
; than $80, then only the READY text will be displayed.
;
LE467:  jsr     $FFCC           ; CLRCHN, close all I/O channels
        lda     #$00
        sta     $13             ; input prompt flag
        jsr     $C67A           ; do CLR
        cli                     ; enable IRQ
BASIC_READY:
        ldx     #$80            ; error code $80 
        jmp     ($0300)         ; perform error, JiffyDOS @ JIFFY_IERROR
IERROR_OLD:   
LE47E:  txa
        bmi     READY           ; larger than $80
        jmp     $C43A           ; nope, print error
READY:  jmp     $C474           ; print READY


;--------------------------------------
INIT_JIFFY_COMMANDS_AND_FKEYS:
; This routine initialises the JiffyDOS commands by jumping 
; to $e453 where the $0300-vectors are set up. Then it sets 
; up the vectors at $b0 to point to the funktionkey table. 
; The entry at $e4c2 disables the funktionkeys after a @f 
; command.
;
LE480:  jsr     LE45B           ; init JiffyDOS command vectors
        lda     #$0A            ; set up JiffyDOS function key vector
        sta     $B0
        lda     #$F7
        sta     $B1
LE48B:  inx
        stx     $9B             ; AKTFLT, activate/deactivate function keys
        rts
LE48F:  lda     #$6F            ; $6F=command channel
        jsr     LF199           ; prepare for input
        jsr     LFFCF           ; input byte from command channel
        cmp     #$35            ; equal to $35
        rts


;.res 6                  ; FREE

;--------------------------------------
BRING_SERIAL_DATA_LINE_HIGH:
;bring the serial bus data line high. 
;routine
;
LE4A0:  lda     $912C
        and     #$DF
        sta     $912C
        rts
        
;--------------------------------------       
BRING_SERIAL_DATA_LINE_LOW:
;bring the serial bus data line low.
;
LE4A9:  lda     $912C
        ora     #$20
        sta     $912C
        rts
        
;--------------------------------------
CHECK_SERIAL_STABLE:
;make sure the byte that is being read from $911F (serial port) is the same
;as the value the port is currently reading. After verifying it stable, CLK is
;shifted into the carry and DATA into bit 0.
;
LE4B2:  lda     $911F
        cmp     $911F
        bne     LE4B2
        lsr     a
        rts
		
LE4C1:  txa
        bne     LE4CC
        lda     $C3
        sta     $AE
        lda     $C4
        sta     $AF
LE4CC:  jmp     LF66A

;--------------------------------------
JIFFY_TEST_DEVICE:
; The following routine tests if a device is present. On entry (X) 
; holds the device to be tested. Open to the device is performed, 
; and afterwards the statusword can be read for result.
;
LE4CF:  stx     $BA             ; store .X in FA
LE4D1:  tya
        pha
        jsr     JIFFY_OPEN_COMMAND_CHANNEL           ; open 15,x,15
        jsr     LF825           ; set command channel 15 as output
        php
        jsr     LF391           ; close command channel
        plp
        pla
        tay
        ldx     $BA
        rts

.res 21                     ; FREE


;--------------------------------------
RS232_PATCH:
;This patch has been added to the RS232 input routine in KERNAL v.3. 
;It initialises the RS232 parity byte, RIPRTY, on reception of a start bit.
        sta $a9                 ; RINONE, check for start bit
        lda #$01
        sta $ab                 ; RIPRTY, RS232 input parity
        rts

;--------------------------------------
RESET_CHARACTER_COLOR:
; This routine is a patch in KERNAL version 3 to fix a bug with
; the colour code. The routine is called by 'clear a screen line',
; and sets the character colour to COLOR.
;
        lda     $0286           ; get COLOR
        sta     ($F3),y         ; and store in current screen position
        rts

;--------------------------------------
IOBASE:
;get I/O address
; from Kernal location $FFF3 returns the base I/O location in X/Y
LE500:  ldx     #$10
        ldy     #$91
        rts

;--------------------------------------
SCREEN:
; returns the size of the screen.  columns-1 in .X, rows-1 in .Y
;
LE505:  ldx     #$16
        ldy     #$17
        rts

;--------------------------------------
PLOT:
;put/get row and column
LE50A:  bcs     LE513
LE50C:  stx     $D6
        sty     $D3
        jsr     SET_SCREEN_POINTERS 
LE513:  ldx     $D6
        ldy     $D3
        rts

;--------------------------------------
CINIT1:
;initialize I/O
;This routine is part of the KERNAL CINT init routine. I/O default
; values are set, <shift+cbm> keys are disabled, and cursor is switched
; off. The vector to the keyboard table is set up, and the length of
; the keyboardbuffer is set to 10 characters. The cursor color is set, 
; and the key-repeat parameters are set up.
; VIC-20: the ROM charset seems to be enabled and the screen address set
LE518:  jsr     LE5BB
        lda     $0288
        and     #$FD
        asl     a
        asl     a
        ora     #$80
        sta     $9005
        lda     $0288
        and     #$02
        beq     LE536
        lda     #$80
        ora     $9002
        sta     $9002
LE536:  lda     #$00
        sta     $0291
        sta     $CF
        lda     #$DC
        sta     L028F
        lda     #$EB
        sta     $0290
        lda     #$0A
        sta     $0289
        sta     $028C
        lda     #$06
        sta     $0286
        lda     #$04
        sta     $028B
        lda     #$0C
        sta     $CD
        sta     $CC

;--------------------------------------
CLEAR_SCREEN:
; This routine sets up the screen line link table ($d9 - $f2), LDTB1,
; which is used to point out the address to the screen. The later part
; of the routine performs the screen clear, line by line, starting at
; the bottom line. It continues to the next routine which is used to
; home the cursor.
;
LE55F:  lda     $0288           ; get HIBASE, top of screen memory
        ora     #$80
        tay
        lda     #$00
        tax
LE568:  sty     $D9,x           ; store in screen line link table, LDTB1
        clc
        adc     #$16            ; add screen width to next line  
        bcc     LE570
        iny                     ; inc page number
LE570:  inx                     ; next
        cpx     #$18            ; until all rows are done
        bne     LE568
        lda     #$FF
        sta     $D9,x           ; last pointer is $FF
        ldx     #$16            ; start clear screen with line $16 (bottom line)
LE57B:  jsr     LEA8D           ; erase line .X
        dex                     ; next
        bpl     LE57B           ; until screen is empty

;--------------------------------------
HOME_CURSOR:
; This routine puts the cursor in the top left corner by writing its
; column and line to zero.
;
LE581:  ldy     #$00
        sty     $D3
        sty     $D6

;--------------------------------------
SET_SCREEN_POINTERS:
;This routine positions the cursor on the screen and sets up the
; screen pointers. On entry, TBLX must hold the line number, and
; PNTR the column number of the cursor position. A major bug has
; been removed from the original commodore KERNAL. It sometimes caused
; the computer to crash, when deleting characters from the bottom line.
;
LE587:  ldx     $D6             ; read TBLX
        lda     $D3             ; read PNTR
LE58B:  ldy     $D9,x           ; read value 
        bmi     LE597           ; heavy calculations??? jump when ready
        clc
        adc     #$16            ;
        sta     $D3             ; PNTR
        dex
        bpl     LE58B
LE597:  jsr     SET_START_OF_LINE  ; set start of line .X

        lda     #$15            ; 
        inx
LE59D:  ldy     $D9,x           ; LDTB1
        bmi     LE5A2
        clc
        adc     #$16            ; 
        inx
        bpl     LE59D

LE5A2:  sta     $D5             ; store in LMNX, physical screen line length
        jmp     SYNCHRONIZE_COLOR_POINTER       ; sync color pointer
LE5A7:  cpx     $C9             ; read LXSP, check cursor at start of input
        beq     @0

        jmp     RETREAT_CURSOR
@0:     rts

        .byte   0               ; FREE

;--------------------------------------
SET_IO_DEFAULTS:
;The default output device is set to 3 (screen), and the default
; input device is set to 0 (keyboard). The VIC chip registers are
; set from the video chip setup table. The cursor is then set to
; the home position.
        jsr     LE5BB
        jmp     LE581
LE5BB:  lda     #$03
        sta     $9A
        lda     #$00
        sta     $99
		
LE5C3:  ldx     #$10
LE5C5:  lda     LEDE3,x
        sta     $8FFF,x
        dex
        bne     LE5C5
        rts

;--------------------------------------
LP2:
;get character from keyboard buffer
; It is assumed that there is at leaset one character in the keyboard
; buffer. This character is obtained and the rest of the queue is moved
; up one by one to overwrite it. On exit, the character is in (A). 
;
LE5CF:  ldy     $0277
        ldx     #$00
LE5D4:  lda     $0278,x
        sta     $0277,x
        inx
        cpx     $C6
        bne     LE5D4
        dec     $C6
        tya
        cli
        clc
        rts

;--------------------------------------
INPUT_FROM_KEYBOARD:
; This routine uses the previous routine to get characters from
; the keyboard buffer. Each character is output to the screen, unless
; it is <shift/RUN>. If so, the contents of the keyboard buffer is
; replaced with LOAD <CR> RUN <CR>. The routine ends when a carriage
; routine is encountered. The JSR at $e5e7 is o patch in JiffyDOS to
; test if the F-keys or other valid JiffyDOS keys are pressed. If not,
; this routine continues as normal.
; 
LE5E5:  jsr     LE742           ; output to screen
LE5E8:  lda     $C6             ; read NDX, number of characters in queue
        sta     $CC             ; BLNSW, cursor blink enable
        sta     $0292           ; AUTODN, auto scroll down flag
        beq     LE5E8           ; loop till key is pressed
        sei                     ; disable interrupt
        lda     $CF             ; BLNON, last cursor blink (on/off)
        beq     LE602
        lda     $CE             ; GDBLN character under cursor
        ldx     $0287           ; GDCOL, background color under cursor
        ldy     #$00
        sty     $CF             ; clear BLNON
        jsr     LEAA1           ; print to screen

LE602:  jsr     JIFFY_GET_CHARACTER     ; get character from keyboard buffer. JiffyDOS fix
        cmp     #$83            ; test if shift/RUN is pressed
        bne     LE619           ; nope
        ldx     #$09            ; transfer 'LOAD <CR> RUN <CR>' to keyboard buffer
        sei
        stx     $C6             ; store #9 in NDX, number of characters in buffer
LE60E:  lda     LEDF3,x         ; 'LOAD <CR> RUN <CR>' message in ROM
        sta     $0276,x         ; store in keyboard buffer
        dex
        bne     LE60E           ; all 9 characters
        beq     LE5E8           ; always jump
LE619:  cmp     #$0D            ; carriage return pressed?
        bne     LE5E5           ; nope, go to start
        ldy     $D5             ; get LNMX, screen line length
        sty     $D0             ; CRSV, flag input/get from keyboard
LE621:  lda     ($D1),y         ; PNT, screen address
        cmp     #$20            ; space?
        bne     LE62A           ; nope
        dey
        bne     LE621           ; next
LE62A:  iny                     
        sty     $C8             ; store in INDX, end of logical line for input;
        ldy     #$00
        sty     $0292           ; AUTOD, auto scroll down
        sty     $D3             ; PNTR, cursor column
        sty     $D4             ; QTSW, reset quote mode
        lda     $C9             ; LXSP, cursor X/Y positition
        bmi     LE657
        ldx     $D6             ; TBLX, cursor line number
        jsr     LE719           ; retreat cursor
        cpx     $C9             ; LXSP
        bne     LE657
        bne     LE657
        lda     $CA
        sta     $D3             ; PNTR
        cmp     $C8             ; INDX
        bcc     LE657
        bcs     LE691


;--------------------------------------
INPUT_FROM_SCREEN_OR_KEYBOARD:
;This routine is used by INPUT to input data from devices not on the
; serial bus, ie. from screen or keyboard. On entry (X) and (Y) registers
; are preserved. A test is made to determine which device the input is
; to be from. If it is the screen, then quotes and <RVS> are tested for
; and the character is echoed on the screen. Keyboard inputs make use of
; the previous routine.
;
LE64F:  tya             ; preserve .X and .Y
        pha
        txa
        pha
        lda     $D0     ; CRSW, INPUT/GET from keyboard or screen
        beq     LE5E8   ; input from keyboard
LE657:  ldy     $D3     ; PNTR, cursor column
        lda     ($D1),y ; read from current screen address

        sta     $D7     ; temp store
        and     #$3F
        asl     $D7
        bit     $D7
        bpl     LE67E
        ora     #$80
LE67E:  bcc     LE684
        ldx     $D4     ; QTSW, editor in quotes mode
        bne     LE688   ; yep
LE684:  bvs     LE688
        ora     #$40
LE688:  inc     $D3     ; PNTR
        jsr     LE6B8   ; do quotes test
        cpy     $C8     ; INDX, end of logical line for input
        bne     LE6A8
LE691:  lda     #$00
        sta     $D0     ; CRSW
        lda     #$0D
        ldx     $99     ; DFLTN, default input device
        cpx     #$03    ;screen?
        beq     LE6A3   ; yes
        ldx     $9A     
        cpx     #$03
        beq     LE6A6
LE6A3:  jsr     LE742   ; output to screen

LE6A6:  lda     #$0D
LE6A8:  sta     $D7
        pla             ; restore .X and .Y
        tax
        pla
        tay
        lda     $D7
        cmp     #$DE
        bne     LE6B6
        lda     #$FF
LE6B6:  clc
        rts

;--------------------------------------
QUOTES_TEST:
; On entry, (A) holds the character to be tested. If (A) holds
; ASCII quotes, then the quotes flag is toggled.
;
LE6B8:  cmp     #$22
        bne     LE6C4
        lda     $D4
        eor     #$01
        sta     $D4
        lda     #$22
LE6C4:  rts


;--------------------------------------
DISK_TALK:
;SJLoad routine
;
        pha
        lda     #$00
        sta     $90     ; IECSTAT
        
        lda     $BA     ; SY_DN
        jsr     JIF_TALK
        pla
        jmp     TKSA    ; TALKSA


;--------------------------------------
STORE_LIST_FROM_ADDR:
;store the address that the BASIC_DISC_LIST will list from
        ldy     #$01
        sta     ($5F),y
        dey
        txa
        sta     ($5F),y
        rts
;--------------------------------------
SET_UP_SCREEN_PRINT:
;The RVS flag is tested to see if reversed characters are to be printed.
; If insert mode is on, the insert counter is decremented by one. When
; in insert mode, all characters will be displayd, ie. DEL RVS etc. The
; character
;
LE6C5:  ora     #$40
LE6C7:  ldx     $C7
        beq     LE6CD
LE6CB:  ora     #$80
LE6CD:  ldx     $D8
        beq     LE6D3
        dec     $D8
LE6D3:  ldx     $0286
        jsr     LEAA1
        jsr     LE6EA
LE6DC:  pla
        tay
        lda     $D8
        beq     LE6E4
        lsr     $D4
LE6E4:  pla
        tax
        pla
        clc
        cli
        rts

;--------------------------------------
ADVANCE_CURSOR:
;The cursor is advanced one position on the screen. If this puts it beyond
; the 40th column, then it is placed at the beginning of the next line. If
; the length of that line is less than 80, then this new line is linked to
; the previous one. A space is opened if data already exists on the new line.
; If the cursor has reached the bottom of the screen, then the screen is
; scrolled down.
;
LE6EA:  jsr     LE8FA
        inc     $D3
        lda     $D5
        cmp     $D3
        bcs     LE72C
        cmp     #$57
        beq     LE723
        lda     $0292
        beq     LE701
        jmp     LE9F0
LE701:  ldx     $D6
        cpx     #$17
        bcc     LE70E
        jsr     LE975
        dec     $D6
        ldx     $D6
LE70E:  asl     $D9,x
        lsr     $D9,x
        jmp     LED5B
LE715:  adc     #$16
        sta     $D5

;--------------------------------------
RETREAT_CURSOR:
; The screen line link table is searched, and then the start of line is
; set. The rest of the routine sets the cursor onto the next line for
; the previous routine.
;
LE719:  lda     $D9,x
        bmi     LE720
        dex
        bne     LE719
LE720:  jmp     LEA7E
LE723:  dec     $D6
        jsr     LE8C3
        lda     #$00
        sta     $D3
LE72C:  rts

;--------------------------------------
BAC_ON_TO_PREVIOUS_LINE:
;This routine is called when using <DEL> and <cursor LEFT>. The
; line number is tested, and if the cursor is already on the top line,
; then no further action is taken. The screen pointers are set up and
; the cursor placed at the end of the previous line.
;
LE72D:  ldx     $D6
        bne     LE737
        stx     $D3
        pla
        pla
        bne     LE6DC
LE737:  dex
        stx     $D6
        jsr     LE587
        ldy     $D5
        sty     $D3
        rts

;--------------------------------------
OUTPUT_TO_SCREEN:
;This routine is part of the main KERNAL CHROUT routine. It prints CBM
; ASCII characters to the screen and takes care of all the screen editing
; characters. The cursor is automatically updated and scrolling occurs if
; necessary. On entry, (A) must hold the character to be output. On entry
; all registers are stored on the stack. For convinience, the routine is
; slpit into sections showing the processing of both shifted and unshifted
; character.
;
LE742:  pha                     ; store .A, .X, and .Y on the stack
        sta     $D7             ; temp store
        txa
        pha
        tya
        pha
        lda     #$00
        sta     $D0             ; store in CRSW
        ldy     $D3             ; PNTR, cursor positions on line
        lda     $D7             ; retrieve from temp store
        bpl     LE756           ; do unshifted characters 
        jmp     SHIFTED_CHARACTERS    ; do shifted characters

;UNSHIFTED CHARACTERS
;Ordinary unshifted ASCII characters and PET graphics are output directly
; to the screen. The following control codes are trapped and precessed:
; <RETURN>, <DEL>, <CRSR RIGHT>, <CRSR DOWN>. If either insert mode is on
; or quotes are open (except for <DEL>) then the control characters are not
; processed, but output as reversed ASCII literals.

LE756:  cmp     #$0D            ; <RETURN>?
        bne     LE75D           ; nope
        jmp     LE8D8           ; execute return
LE75D:  cmp     #$20            ; <SPACE>?
        bcc     LE771
        cmp     #$60            ; #$60, first PET graphic character?
        bcc     LE769
        and     #$DF            ; %11011111
        bne     LE76B
LE769:  and     #$3F            ; %00111111
LE76B:  jsr     LE6B8           ; do quotes test
        jmp     LE6C7           ; setup screen print
LE771:  ldx     $D8             ; INSRT, insert mode flag
        beq     LE778           ; mode not set
        jmp     LE6CB           ; output reversed character
LE778:  cmp     #$14            ; <DEL>?
        bne     LE7AA           ; nope
        tya                     ; .Y holds cursor column
        bne     LE785           ; not start of line
        jsr     LE72D           ; back on previous line
        jmp     LE79F
LE785:  jsr     LE8E8           ; check line decrement
        dey                     ; decrement cursor column
        sty     $D3             ; and store in PNTR
        jsr     LEAB2           ; synchronize color pointer
LE78E:  iny                     ; copy character at cursor position .Y+1 to .Y
        lda     ($D1),y
        dey
        sta     ($D1),y
        iny
        lda     ($F3),y         ; move color back as well
        dey
        sta     ($F3),y
        iny                     ; more characters to move
        cpy     $D5             ; compare with LNMx, length of physical screen line
        bne     LE78E           ; if not equal, move more characters
LE79F:  lda     #$20
        sta     ($D1),y         ; store <SPACE> at end of line
        lda     $0286           ; COLOR, current character color
        sta     ($F3),y         ; store color at end of line
        bpl     LE7F7           ; always jmp
LE7AA:  ldx     $D4             ; QTSW, editor in quotes mode
        beq     LE7B1           ; no
        jmp     LE6CB           ; output reversed character
LE7B1:  cmp     #$12            ; <RVS>?
        bne     LE7B7           ; no
        sta     $C7             ; RVS, reversed character output flag
LE7B7:  cmp     #$13            ; <HOME>?
        bne     LE7BE           ; no
        jsr     LE581           ; home cursor
LE7BE:  cmp     #$1D            ; <CRSR RIGHT>?
        bne     LE7D9           ; nope
        iny                     ; increment .Y, internal counter for column
        jsr     LE8FA           ; check line increment
        sty     $D3             ; store .Y in PNTR
        dey                     ; decrement .Y 
        cpy     $D5             ; and compare to LNMX
        bcc     LE7D6           ; not exceeded line length
        dec     $D6             ; TBLX, current physical line number
        jsr     LE8C3           ; goto next line
        ldy     #$00
LE7D4:  sty     $D3             ; set PNTR to zero, cursor to the left
LE7D6:  jmp     LE6DC           ; finish screen print
LE7D9:  cmp     #$11            ; <CRSR DOWN>?
        bne     LE7FA           ; no
        clc                     ; prepare for add
        tya                     ; .Y holds cursor column
        adc     #$16            ; add screen w
        tay                     ; to .Y
        inc     $D6             ; increment TBLX, physical line number
        cmp     $D5             ; compare to LNMX
        bcc     LE7D4           ; finish screen print
        beq     LE7D4           ; finish screen print
        dec     $D6             ; restore TBLX
LE7EC:  sbc     #$16            ;
        bcc     LE7F4
        sta     $D3             ; store PNTR
        bne     LE7EC
LE7F4:  jsr     LE8C3           ; go to next line
LE7F7:  jmp     LE6DC           ; finish screen print
LE7FA:  jsr     LE912           ; set color code
        jmp     LED21           ; do graphics/text control

LE800:  jsr     LC533           ;rechain BASIC lines this had to be moved here
        jmp     LC677


        .res    15              ; FREE

;--------------------------------------
JIFFY_X_COMMAND:
;
        jsr     $D79B           ; GTBYTC, get destination device
        stx     $BF
        rts

;--------------------------------------
SHIFTED_CHARACTERS:
;SHIFTED CHARACTERS. These are dealt with in the following order: Shifted
; ordinart ASCII and PET graphics characters, <shift RETURN>, <INST>, <CRSR UP>,
; <RVS OFF>, <CRSR LEFT>, <CLR>. If either insert mode is on, or quotes are
; open, then the control character is not processed but reversed ASCII literal
; is printed.

        and     #$7F            ; clear bit7
        cmp     #$7F            ; compare to $7F
        bne     LE81D           ; not equal
        lda     #$5E            ; if $7F, load $5E
LE81D:  cmp     #$20            ; ASCII <SPACE>?
        bcc     LE82A
        jmp     LE6C5           ; set up screen print
LE82A:  cmp     #$0D            ; <RETURN>?
        bne     LE831           ; nope
        jmp     LE8D8           ; do return
LE831:  ldx     $D4             ; read QTSW
        bne     LE874           ; if quotes mode, jump
        cmp     #$14            ; <INST>?
        bne     LE870           ; nope
        ldy     $D5             ; LNMX
        lda     ($D1),y         ; get screen character
        cmp     #$20            ; <SPACE>?
        bne     LE845           ; nope
        cpy     $D3             ; PNTR equal to LNMX
        bne     LE84C           ; nope
LE845:  cpy     #$57            ; $57=87, last character
        beq     LE86D           ; end of logical line, can not insert
        jsr     LE9EE           ; open space on line
LE84C:  ldy     $D5             ; LNMX
        jsr     LEAB2           ; synchronize color pointer
LE851:  dey                     ; prepare for move
        lda     ($D1),y         ; read character at pos .Y
        iny
        sta     ($D1),y         ; and move one step to the right
        dey
        lda     ($F3),y         ; read character color
        iny
        sta     ($F3),y         ; move one step to the right
        dey                     ; decrement counter
        cpy     $D3             ; compare with PNTR
        bne     LE851           ; til all characters right of cursror are moved
        lda     #$20            ; <SPACE>, ASCII $20
        sta     ($D1),y         ; store at new character position
        lda     $0286           ; COLOR, current character color
        sta     ($F3),y         ; store at new color position
        inc     $D8             ; INSRT FLAG
LE86D:  jmp     LE6DC           ; finish screen print
LE870:  ldx     $D8             ; INSRT FLAG
        beq     LE879           ; insert mode is off
LE874:  ora     #$40
        jmp     LE6CB           ; set up screen print
LE879:  cmp     #$11            ; <CRSR UP>?
        bne     LE893           ; nope
        ldx     $D6             ; read TBLX
        beq     LE8B8           ; at topline, do nothing
        dec     $D6             ; else decrement TBLX
        lda     $D3             ; PNTR
        sec                     ; prepare for subtract
        sbc     #$16            ; back screen w/ columns 
        bcc     LE88E           ; skip
        sta     $D3             ; store PNTR
        bpl     LE8B8           ; finish screen print
LE88E:  jsr     LE587           ; set screen pointer
        bne     LE8B8           ; finish screen print
LE893:  cmp     #$12            ; <RVS OFF>?
        bne     LE89B           ; nope
        lda     #$00
        sta     $C7             ; RVS, disable reverse print
LE89B:  cmp     #$1D            ; <CRSR LEFT>?
        bne     LE8B1           ; nope
        tya                     ; .Y holds cursor column
        beq     LE8AB           ; at first position
        jsr     LE8E8           ; check line decrement
        dey                     ; one position left
        sty     $D3             ; store in PNTR
        jmp     LE6DC           ; finish screen print
LE8AB:  jsr     LE72D           ; back to previous line
        jmp     LE6DC           ; finish screen print
LE8B1:  cmp     #$13            ; <CLR>?
        bne     LE8BB           ; nope
        jsr     LE55F           ; clear screen
LE8B8:  jmp     LE6DC           ; finish screen print
LE8BB:  ora     #$80
        jsr     LE912           ; set color code
        jmp     LED30           ; set graphics/text mode

;--------------------------------------
GO_TO_NEXT_LINE:
;The cursor is placed at the start of the next logical screen line. This
; involves moving down two lines for a linked line. If this places the
; cursor below the bottom of the screen, then the screen is scrolled.
;
LE8C3:  lsr     $C9             ;LXSP, cursor X-Y position 
        ldx     $D6             ; TBLX, current line number
LE8C7:  inx                     ; next line
        cpx     #$17            ;
        bne     LE8CF           ; nope, scroll not needed
        jsr     LE975           ; scroll down
LE8CF:  lda     $D9,x           ; test LTDB1, screen line link table if first of two
        bpl     LE8C7           ; yes, jump down another line
        stx     $D6             ; store in TBLX
        jmp     LE587           ; set screen pointers

;--------------------------------------
OUTPUT_CR:
; All editor modes are swithed off and the cursor placed at the start
; of the next line.
;
LE8D8:  ldx     #$00
        stx     $D8
        stx     $C7
        stx     $D4
        stx     $D3
        jsr     LE8C3
        jmp     LE6DC

;--------------------------------------
CHECK_LINE_DECREMENT:
; When the cursor is at the beginning of a screen line, if it is moved
; backwards, this routine places the cursor at the end of the line above.
; It tests both column 0 and column 22.
;
LE8E8:  ldx     #$04
        lda     #$00
LE8EC:  cmp     $D3
        beq     LE8F7
        clc
        adc     #$16
        dex
        bne     LE8EC
        rts
LE8F7:  dec     $D6
        rts

;--------------------------------------
CHECK_LINE_INCREMENT:
; When the cursor is at the end of the screen, if it is moved forward, this
; routine places the cursor at the start of the line below.
;
LE8FA:  ldx     #$04
        lda     #$15
LE8FE:  cmp     $D3
        beq     LE909
        clc
        adc     #$16
        dex
        bne     LE8FE
        rts
LE909:  ldx     $D6
        cpx     #$17
        beq     LE911
        inc     $D6
LE911:  rts

;--------------------------------------
SET_COLOR_CODE:
; This routine is called by the output to screen routine. The Commodore
; ASCII code in (A) is compared with the ASCII colout code table. If a
; match is found, then the table offset (and hence the colour value) is
; stored in COLOR.
;
LE912:  ldx     #$07
LE914:  cmp     LE921,x
        beq     LE91D
        dex
        bpl     LE914
        rts
LE91D:  stx     $0286
        rts

;--------------------------------------
; COLOR CODE TABLE
LE921:  .byte   $90             ; black
        .byte   $05             ; white
        .byte   $1C             ; red
        .byte   $9F             ; cyan
        .byte   $9C             ; purple
        .byte   $1E             ; green
        .byte   $1F             ; blue
        .byte   $9E             ; yellow

;--------------------------------------
; SOME TABLE
        .byte   $EF
        lda     ($DF,x)
        ldx     $E1
        lda     ($E2),y
        .byte   $B2
        .byte   $E3
        .byte   $B3
        cpx     $B4
        sbc     $B5
        inc     $B6
        .byte   $E7
        .byte   $B7
        inx
        clv
        sbc     #$B9
        .byte   $FA
        tsx
        .byte   $FB
        .byte   $BB
        .byte   $FC
        ldy     $BDEC,x
        inc     $84BE,x
        .byte   $BF
        .byte   $F7
        cpy     #$F8
        .byte   $DB
        sbc     LEADD,y
        dec     LE05E,x
        .byte   $5B
        sbc     ($5D,x)
        .byte   $E2
        rti
        bcs     LE9BD
        lda     ($78),y
        .byte   $DB
        adc     $66DD,y
        ldx     $77,y
        cpy     #$70
        beq     LE9D9
        sbc     ($72),y
        .byte   $F2
        .byte   $73
        .byte   $F3
        .byte   $74
        .byte   $F4
        adc     $F5,x
        ror     $F6,x
        .byte   $7D
        .byte   $FD

;--------------------------------------
SCROLL_SCREEN:
;This routine scrolls the screen down by one line. If the top 
;two lines are linked togeather, then the scroll down is repeated. 
;The screen line link pointers are updated, each screen line is 
;cleared and the line below is moved up. The keyboard is directly 
;read from CIA#1, and the routine tests if <CTRL> is pressed. A 
;JiffyDOS feature is the <CTRL S> option, which freezes the scroll 
;till another key is pressed.
;
LE975:  lda     $AC         ;temp store SAL on stack
        pha
        lda     $AD
        pha
        lda     $AE         ;temp store EAL on stack
        pha
        lda     $AF
        pha
LE981:  ldx     #$FF
        dec     $D6         ;decrement TBLX
        dec     $C9         ;decrement LXSP
        dec     $F2         ;temp store for line index
LE989:  inx
        jsr     LEA7E       ;set start of line (.X)
        cpx     #$16
        bcs     LE99D
        lda     LEDFE,x     ;read low-byte screen address
        sta     $AC
        lda     $DA,x
        jsr     LEA56       ;move a screen line
        bmi     LE989
LE99D:  jsr     LEA8D       ;clear a screen line
        ldx     #$00
LE9A2:  lda     $D9,x       ;calculate new screen line link table
        and     #$7F        ;clear bit 7
        ldy     $DA,x
        bpl     LE9AC
        ora     #$80        ;set bit 7
LE9AC:  sta     $D9,x       ;store new value in table
        inx                 ;next line
        cpx     #$16        ;til all 22 are done 
        bne     LE9A2
        lda     $EF         ;bottom line link
        ora     #$80        ;unlink it
        sta     $EF         ;and store back
        lda     $D9         ;test top line link
        bpl     LE981       ;line is linked, scroll again
LE9BD:  inc     $D6         ;increment TBLX
        inc     $F2 


;TODO: stop scrolling on <CTRL>+<S> not working properly CRITICAL
LE9C1:  sei
LE9C2:  jsr     SCAN_KEYBOARD
        jsr     SCAN_CONTROL    ;lda #$FB, sta $9120, lda $9121, cmp #$FE, php,
                                ;lda #$F7, sta $9120, plp, rts

        bne     LE9DF       ;<CTRL> not pressed? then we're done
        ldx     $C6         ;NDX, number of characters in keyboard buffer
        beq     LE9C2       ;as long as <CTRL> is pressed, freeze scroll
        
        lda     $0276,x     ;read character from keyboard buffer
        sbc     #$53        ;subtract $13, "S"
        bne     LE9DF       ;nope, did not press "S"

        sta     $C6         ;clear NDX
LE9D8:  cli                 ;allow interrupts
LE9D9:  cmp     $C6         ;any new character in buffer?
        beq     LE9D8       ;nope, still freeze
        sta     $C6         ;clear NDX

LE9DF:  ldx     $D6         ;read TBLX
        pla                 ;retreive EAL
        sta     $AF
        pla
        sta     $AE
        pla                 ;retrieve SAL
        sta     $AD
        pla
        sta     $AC
        rts                 ;exit

.res 1                      ;FREE

;--------------------------------------
OPEN_A_SPACE_ON_SCREEN:
; This routine opens a space on the screen for use with <INST>. If needed,
; the screen is then scrolled down, otherwise the screen line is moved and
; cleared. Finally the screen line link table is adjusted and updated.
;
LE9EE:  ldx     $D6                 ; TBLX, current cursor line number
LE9F0:  inx                         ; test next
        lda     $D9,x               ; LDTB1, screen line link table
        bpl     LE9F0
        stx     $F2                 ; temp line for index
        cpx     #$16                ; bottom of screen
        beq     LEA08               ; yes
        bcc     LEA08               ; above bottom line
        jsr     LE975               ; scroll screen down
        ldx     $F2                 ; temp line for index
        dex
        dec     $D6                 ; TBLX
        jmp     LE70E               ; adjust link table and end
LEA08:  lda     $AC                 ; push SAL, scrolling pointer
        pha
        lda     $AD
        pha
        lda     $AE                 ; push EAL, end of program
        pha
        lda     $AF
        pha
        ldx     #$17
LEA16:  dex
        jsr     LEA7E               ; set start of line
        cpx     $F2                 ; temp line for index
        bcc     LEA2C
        beq     LEA2C
        lda     LEDFC,x             ; screen line address table
        sta     $AC                 ; SAL
        lda     $D8,x               ; LDTB1
        jsr     LEA56               ; move screen line
        bmi     LEA16
LEA2C:  jsr     LEA8D               ; clear screen line
        ldx     #$15                ; fix screen line link table
LEA31:  cpx     $F2                 ; temp line for index
        bcc     LEA44
        lda     $DA,x               ; LDTB1+1
        and     #$7F
        ldy     $D9,x               ; LDTB1
        bpl     LEA3F
        ora     #$80
LEA3F:  sta     $DA,x
        dex                         ; next line
        bne     LEA31               ; til line zero
LEA44:  ldx     $F2                 ; temp line for index
        jsr     LE70E               ; adjust link table
        pla 
        sta     $AF
        pla                         ; EAL, end of program
        sta     $AE
        pla
        sta     $AD
        pla                         ; SAL, scrolling pointer
        sta     $AC
        rts                         ; done

;--------------------------------------
MOVE_A_SCREEN_LINE:
; This routine synchronises colour transfer, and then moves the screen
; line pointed to down, character by character. The colour codes for each
; character are also moved in the same way.
;
LEA56:  and     #$03
        ora     $0288               ; HIBASE, top of screen page
        sta     $AD                 ; store >SAL, screen scroll pointer
        jsr     LEA6E               ; synchronize color transfer
LEA60:  ldy     #$15                ; offset for character on screen line
LEA62:  lda     ($AC),y             ; move screen character
        sta     ($D1),y
        lda     ($AE),y             ; move character color
        sta     ($F3),y
        dey                         ; next character
        bpl     LEA62               ; til all 22 are done
        rts

;--------------------------------------
SYNCHRONIZE_COLOR_TRANSFER:
; This routine setd up a temporary pointer in EAL to the colour RAM address
; that corresponts to the temporary screen address held in EAL.
;
LEA6E:  jsr     LEAB2               ; synchronize color pointer
        lda     $AC                 ; SAL, pointer for screen scroll
        sta     $AE                 ; EAL
        lda     $AD
        and     #$03
        ora     #$94                ; stup color RAM to $9400
        sta     $AF
        rts

;--------------------------------------
SET_START_OF_LINE:
; On entry, (X) holds the line number. The low byte of the address
; is set from the ROM table, and the highbyte derived from the screen
; link and HIBASE.
;
LEA7E:  lda     LEDFD,x             ; table of screen line to bytes
        sta     $D1                 ; <PNT, current screen line address
        lda     $D9,x               ; LDTB1, screen line link table
        and     #$03
        ora     $0288               ; HIBASE, page of top screen
        sta     $D2                 ; >PNT
        rts

;--------------------------------------
CLEAR_SCREENLINE:
; The start of line is set and the screen line is cleared by filloing it
; with ASCII spaces. The corresponding line of colour RAM is also cleared to
; the value held in COLOR.
;
LEA8D:  ldy     #$15            ;
        jsr     SET_START_OF_LINE
        jsr     SYNCHRONIZE_COLOR_POINTER
LEA95:  jsr     RESET_CHARACTER_COLOR           ; reset character color to COLOR
        lda     #$20            ; ASCII space
        sta     ($D1),y         ; store character to screen
        dey                     ; next
        bpl     LEA95           ; until whole line is done
        rts

        .byte   0               ; FREE

;--------------------------------------
PRINT_TO_SCREEN:
; The colour pointer is synchronised, and the character in (A) directly
; stored in the screen RAM. The character colour in (X) is stored at the
; equivalent point in the colour RAM.
LEAA1:  tay
        lda     #$02
        sta     $CD
        jsr     LEAB2
        tya
LEAAA:  ldy     $D3
        sta     ($D1),y
        txa
        sta     ($F3),y
        rts

;--------------------------------------
SYNCHRONIZE_COLOR_POINTER:
; The pointer to the colour RAM is set up according to the current screen
; line address. This is done by reading the current screen line address and
; modefying it to colour RAM pointers and write it to USER at $f3/$f4
;
LEAB2:  lda     $D1
        sta     $F3
        lda     $D2
        and     #$03
        ora     #$94
        sta     $F4
        rts

;--------------------------------------
MAIN_IRQ_ENTRY_POINT:
;This routine services the normal IRQ that jumps through the hardware vector
; to $ff48, and then continues to the CINV vector at $0314. First it checks if
; the <STOP> key was pressed and updates the realtime clock. Next, the cursor
; is updated (if it is enabled, BLNSW). The blink counter, BLNCT, is
; decremented. When this reaches zero, the cursor is toggled (blink on/off).
; Finally it scans the keyboard. The processor registers are then restored on
; exit. 
;Area from $ea64 to $ea7b has been changed in the JiffyDOS system. 
;Some routintes to handle the casetterecorder has been removed.
;
        jsr     LFFEA           ; update realtime clock, UDTIM
        lda     $CC             ; read BLNSW to see if cursor is enabled
        bne     LEAEF           ; nope
        dec     $CD             ; read BLCNT
        bne     LEAEF           ; if zero, toggle cursor, else jump
        lda     #$14            ; blink speed
        sta     $CD             ; restore BLCNT
        ldy     $D3             ; get PNTR, cursor column
        lsr     $CF             ; BLNON, flag last cursor blink on/off
        ldx     $0287           ; get background color under cursor, GDCOL
        lda     ($D1),y         ; get screen characer
        bcs     LEAEA
        inc     $CF             ; increment BLNON
        sta     $CE             ; temporary stroe character under cursor
LEADD:  jsr     LEAB2           ; synchronize color pointer
        lda     ($F3),y         ; get color under character
        sta     $0287           ; store in GDCOL
        ldx     $0286           ; get current COLOR
        lda     $CE             ; retrieve character under cursor
LEAEA:  eor     #$80            ; toggle cursor by inverting character
        jsr     LEAAA           ; print to screen by using part of 'print
LEAEF:  jmp     LEB12           ; skip

;--------------------------------------
JIFFY_CRNCH:
; the ICRNCH VECTOR points to this routine after the JiffyDOS init
;
LEAF2:  pla                     ; get last stack entry
        pha                     ; put it back
        cmp     #$98            ; equal to $98? 
        beq     @1              ; yep, do JiffyDOS CRNCH
@0:     jmp     $C57C           ; do original CRNCH
@1:     jsr     TEST_JIFFY_COMMAND ; test if key in buffer is a JiffyDOS command
        bne     @0              ; no command
        ldx     $7A             ; position i keyboard buffer (comment messed up??)
        ldy     #$04            ; setup values for old routine
        tya
        jmp     $C5E3           ; back into old CRNCH

        .res    10              ; FREE

;--------------------------------------
QUICK_IRQ_ENTRY_POINT:
;
LEB12:  jsr     LEB1E
        bit     $9124
        pla
        tay
        pla
        tax
        pla
        rti

;--------------------------------------
SCAN_KEYBOARD:
;The KERNAL routine SCNKEY ($ff9f) jumps to this routine. First, the
; shift-flag, SHFLAG, is cleared, and the keyboard tested for nokey. The
; keyboard is set up as a 8 * 8 matrix, and is read one row at a time. $ff
; indicates that no key has been pressed, and a zerobit, that one key has
; been pressed.
;
LEB1E:  lda     #$00
        sta     $028D           ; clear SHFLAG
        ldy     #$40
        sty     $CB
        sta     $9120           ; store in keyboard write register
        ldx     $9121           ; keyboard read register
        cpx     #$FF            ; no key pressed 
        beq     LEB8F           ; skip
        lda     #$FE
        sta     $9120
        ldy     #$00
        lda     #$5E            ; point KEYTAB vector to $EC5E
        sta     $F5
        lda     #$EC            ; bit0 = 0
        sta     $F6             ; will test first row in matrix
LEB40:  ldx     #$08            ; scan 8 rows in matrix
        lda     $9121           ; read
        cmp     $9121           ; wait for value to settle (key bouncing)
        bne     LEB40
LEB4A:  lsr     a               ; test bit0
        bcs     LEB63           ; no key pressed
        pha
        lda     ($F5),y         ; get key from KEYTAB
        cmp     #$05            ; value less than 5?
        bcs     LEB60           ; nope
        cmp     #$03            ; value = 3?
        beq     LEB60           ; nope
        ora     $028D
        sta     $028D           ; store in SHFLAG
        bpl     LEB62
LEB60:  sty     $CB             ; store keynumber in SFDX
LEB62:  pla
LEB63:  iny                     ; key counter
        cpy     #$41            ; all 64 keys (8*8)
        bcs     LEB71           ; jump if ready
        dex                     ; next key in row
        bne     LEB4A           ; row ready
        sec                     ; prepare for rol
        rol     $9120           ; next row
        bne     LEB40           ; always jmp

;--------------------------------------
PROCESS_KEY_IMAGE:
; This routine decodes the pressed key, and calcuates its ASCII value,
; by use of the four tables. If the pressed key is the same key as in the
; former interrupt, then the key-repeat-section is entered. The routine
; tests the RPTFLG if the key shall repeat. The new key is stored in the
; keyboard buffer, and all pointers are uppdated.
;
LEB71:  jmp     (L028F)         ; jump through KEYLOG vector, points to $EBDC 
LEB74:  ldy     $CB             ; SFDX, number of the key we pressed
        lda     ($F5),y         ; get ASCII value from decode table
        tax                     ; temp store
        cpy     $C5             ; same key as former interrupt
        beq     LEB84           ; yep
        ldy     #$10            ; restore the repeat delay counter
        sty     $028C           ; DELAY
        bne     LEBBA           ; always jmp
LEB84:  and     #$7F
        bit     $028A           ; RPTFLG, test repeat mode
        bmi     LEBA1           ; repeat all keys
        bvs     LEBD6           ; repeat none- exit routine
        cmp     #$7F
LEB8F:  beq     LEBBA
        cmp     #$14            ; <DEL> key pressed
        beq     LEBA1           ; yep
        cmp     #$20            ; <SPACE> key pressed
        beq     LEBA1           ; yep
        cmp     #$1D            ; <CRSR LEFT/RIGHT>
        beq     LEBA1           ; yep
        cmp     #$11            ; <CRSR DOWN/UP>
        bne     LEBD6           ; yep
LEBA1:  ldy     $028C           ; DELAY
        beq     LEBAB           ; skip
        dec     $028C           ; decrement delay
        bne     LEBD6           ; end
LEBAB:  dec     $028B           ; decrement KOUNT, repeat speed counter
        bne     LEBD6           ; end
        ldy     #$04
        sty     $028B           ; init KOUNT
        ldy     $C6             ; read NDX, number of keys in keyboard queue
        dey
        bpl     LEBD6           ; end
LEBBA:  ldy     $CB             ; read SFDX
        sty     $C5             ; store in LSTX
        ldy     $028D           ; read SHFLAG
        sty     $028E           ; restore in LSTSH, last keyboard shift pattern
        cpx     #$FF            ; no valid key pressed
        beq     LEBD6           ; end
        txa
        ldx     $C6             ; NDX, number of keys in buffer
        cpx     $0289           ; compare to XMAX, max number of characters in buffer
        bcs     LEBD6           ; buffer is full, end
        sta     $0277,x         ; store new character in keyboard buffer
        inx                     ; increment counter
        stx     $C6             ; and store in NDX
LEBD6:  lda     #$F7
        sta     $9120           ; keyboard write register
        rts                     ; exit

;--------------------------------------
PROCESS_KEY_IMAGE_CONTD:
;this is where the KEYLOG vector (at $028F) points initially.
;
LEBDC:  lda     $028D           ; SHFLAG
        cmp     #$03            ; <SHIFT> and <CBM> at the same time
        bne     LEC0F           ; nope
        cmp     $028E           ; same as LSTSH
        beq     LEBD6           ; if so, end
        lda     $0291           ; read MODE, shift key enable flag
        bmi     LEC43           ; end
        nop
        nop
        nop
        nop
        nop
        lda     $9005           ; get current charset
        eor     #$02            ; toggle upper/lower case
        sta     $9005           ; store 
        nop
        nop
        nop
        nop
        jmp     LEC43           ; process key image
LEC0F:  asl     a
        cmp     #$08            ; test <CTRL>
        bcc     @0              ; nope
        lda     #$06            ; set offset for ctrl
@0:     tax                     ; to .X 
        lda     LEC46,x         ; read keyboard select vectors, low byte
        sta     $F5             ; store in KEYTAB, decode table vector
        lda     LEC46+1,x       ; read keyboard select vectors, hi byte
        sta     $F6             ; KEYTAB+1
LEC43:  jmp     LEB74           ; process image
        nop
        nop
LEC18:  nop
        nop
        nop
        nop

;-------------------------------------- 
;JIFFY_SCREEN_DUMP (from FXXX something)
;LEC1C:  lda     #$04            ; printer device #4
;        jsr     LFFB1           ; send LISTEN to device #4
;        lda     $9005           ; test upper/lower charset
;        and     #$02
;        beq     @0              ; if 0, lower charset
;        lda     #$07            ; set SA=$67
;@0:     ora     #$60            ; set SA=$60
;        jsr     LFF93           ; send SA after LISTEN
;        lda     $D3             ; PNTR, cursor column
;        pha 
                            ; save
;        lda     $D6             ; TBLX, cursor line
;        pha                     ; save
;        jmp     LEDAA           ; next part of the routine is at $EDAA



        
        .res 7

;------------------------------------
CHECK_STOP_KEY:
; before checking if the STOP key is pressed, ensure that the caller of the STOP
; key wasn't from the BASIC DISC list. If the BASIC DISC list function were to 
; directly check for STOP during that routine it would exit prematurely.
;
LE4E3:  txa                 ; save .X
        pha
        tsx
        lda     #$F6        ; check up the stack to find who called this,
        cmp     $0107,x     ; if it was $F635 or $F62F (from the BASIC LIST DISC),
        bne     @0          ; ignore it
        lda     $0106,x
        cmp     #$35
        beq     @1          ; yes, exit
        cmp     #$2F        ; $2F?
        beq     @1          ; yes, exit
@0:     pla                 ; retrieve .X
        tax                 ; give it back to .X
        jmp     (L0328)     ; STOP, check STOP key
@1:     pla
        pla
        pla
        pla
        pla
        rts

.res 3                          ; FREE

;--------------------------------------
KEYBOAD_SELECT_VECTORS:
; This is a table of vectors pointing to the start of four
; decode tables
;
LEC46:  .word   $EC5E           ; unshifted decode table
        .word   $EC9F           ; Shifted decode table
        .word   $ECE0           ; CBM decode table
        .word   $ED69           ; CTRL decode table
        ;.byte   $A3
        ;sbc     LEC5E
        ;.byte   $9F
        ;cpx     LED69
        ;.byte   $A3
        ;sbc     LED21
;--------------------------------------
JIFFY_BYTE_IN:
; reconstruct the byte from the bits that were read into the form that it 
; will be used in.  This is done by putting the contents of $B3 into the
; low nybble of $B3, and $C0 into the high nybble.
; basically: $B3 = ($B3 & 0x0f) | ($C0 << 4)
;
LEC4E:  lda     $B3     ;get part of the byte
        and     #$0F    ;mask the lower nybble
        sta     $B3     ;save
        lda     $C0     ;get the other part
        asl     a       ;bring its little nybble into its big nybble
        asl     a
        asl     a
        asl     a
        ora     $B3     ;OR $B3 into the low nybble
LEC5C:  rts

;--------------------------------------
;KEYBOARD 1 - UNSHIFTED
; This is the first of four keybboard decode tables. The ASCII code for
; the key pressed is at the intersection of the row (written to $dc00)
; and the column (read from $dc01). The matrix values are shown below.
; Note that left and right shift keys are seperated. 
;
LEC5E:  .byte   $ED
        .word   $3331
        and     $37,x
        and     $5C2B,y
        .byte   $14
        .byte   $5F
        .byte   $57
        .byte   $52
        eor     $5049,y
        rol     a
        ora     $4104
        .byte   $44
        .byte   $47
        lsr     a
        jmp     L1D3B
        .byte   $03
        ora     ($58,x)
        lsr     $4E,x
        bit     $112F
        jsr     L435A
        .byte   $42
        eor     $012E
        sta     $02
        .byte   $53
        lsr     $48
        .byte   $4B
        .byte   $3A
        and     $5186,x
        eor     $54
        eor     $4F,x
        rti
        lsr     $3287,x
        .byte   $34
        rol     $38,x
        bmi     LECC9
        .byte   $13
        dey
        .byte   $FF

;--------------------------------------
;KEYBOARD 2 - SHIFTED
; This is the second of four keyboard decode tables. The ASCII code for
; the key pressed is at the intersection of the row (written to $dc00) and
; the column (read from $dc01). The matrix values are shown below.
;
LEC9F:  and     ($23,x)
        and     $27
        and     #$DB
        lda     #$94
        .byte   $5F
        .byte   $D7
        .byte   $D2
        cmp     $D0C9,y
        cpy     #$8D
        .byte   $04
        cmp     ($C4,x)
        .byte   $C7
        dex
        cpy     $9D5D
        .byte   $83
        ora     ($D8,x)
LECBA:  dec     $CE,x
        .byte   $3C
        .byte   $3F
        sta     ($A0),y
        .byte   $DA
        .byte   $C3
        .byte   $C2
        cmp     $013E
        .byte   $89
        .byte   $02
        .byte   $D3
LECC9:  dec     $C8
        .byte   $CB
        .byte   $5B
        and     $D18A,x
        cmp     $D4
        cmp     $CF,x
        tsx
        dec     $228B,x
        bit     $26
        plp
        bmi     LECBA
        .byte   $93
        .byte   $8C
        .byte   $FF

;--------------------------------------
;KEYBOARD 3 - COMMODORE
; This is the third of four keyboard decode tables. The ASCII code
; for the key pressed is at the intersection of the ro (written to $dc00)
; and hte column (read from $dc01). The matrix values are shown below.
;
        .byte   $21
        .byte   $23
        and     $27
        and     #$A6
        tay
        sty     $5F,x
        .byte   $B3
        .byte   $B2
        .byte   $B7
        ldx     #$AF
        .byte   $DF
        sta     $B004
        ldy     $B5A5
        ldx     $5D,y
        sta     $0183,x
LECFA:  lda     $AABE,x
        .byte   $3C
        .byte   $3F
        sta     ($A0),y
        lda     $BFBC
        .byte   $A7
        rol     $8901,x
        .byte   $02
        ldx     $B4BB
        lda     ($5B,x)
        and     $AB8A,x
        lda     ($A3),y
        clv
        lda     $DEA4,y
        .byte   $8B
        .byte   $22
        bit     $26
        plp
        bmi     LECFA
        .byte   $93
        .byte   $8C
        .byte   $FF
;--------------------------------------
GRAPHICS_TEXT_CONTROL:
; This routine is used to toggle between text and graphics character set,
; and to enable/disable the <shift-CBM> keys. The routine is called by the
; main 'output to screen' routine, and (A) holds a CBM ASCII code on entry.
;
LED21:  cmp     #$0E
        bne     LED30
        lda     #$02
        ora     $9005
        sta     $9005
        jmp     LE6DC
LED30:  cmp     #$8E
        bne     LED3F
        lda     #$FD
        and     $9005
        sta     $9005
LED3C:  jmp     LE6DC
LED3F:  cmp     #$08
        bne     LED4D
        lda     #$80
        ora     $0291
        sta     $0291
        bmi     LED3C
LED4D:  cmp     #$09
        bne     LED3C
        lda     #$7F
        and     $0291
        sta     $0291
        bpl     LED3C
LED5B:  inx
        lda     $D9,x
        ora     #$80
        sta     $D9,x
        dex
        lda     $D5
        clc
        jmp     LE715

;--------------------------------------
;KEYBOARD 4 - CONTROL
; This is the last keyboard decode table. The ASCII code for the key pressed
; is at the intersection of the row (written to $dc00) and the column
; (read from $dc01). The matrix values are shown below.
; A few special funktion are found in this table ie. 
; <ctrl H> - disables the upper/lower case switch
; <ctrl I> - enables the upper/lower case switch
; <ctrl S> - homes the cursor
; <ctrl T> - delets character
; Note that the italic keys only represent a ASCII code, and not a CBM character.
; Future implementations: Change some of the $ff values which represents 'no key'
; to a valid ASCII code. ESC ($1b) and why not use the F-keys for something useful. 
;
LED69:  .byte   $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
        .byte   $FF
        .byte   $04
        .byte   $FF
        .byte   $FF
        .byte   $FF
        .byte   $FF
        .byte   $FF
        .byte   $E2
LED79:  sta     $0183,x
        .byte   $FF
        .byte   $FF
        .byte   $FF
        .byte   $FF
        .byte   $FF
        sta     ($A0),y
        .byte   $FF
        .byte   $FF
        .byte   $FF
        .byte   $FF
        inc     $8901
        .byte   $02
        .byte   $FF
        .byte   $FF
LED8D:  .byte   $FF
        .byte   $FF
        sbc     ($FD,x)
        txa
        .byte   $FF
        .byte   $FF
        .byte   $FF
        .byte   $FF
        .byte   $FF
        bcs     LED79
        .byte   $8B
        .byte   $F2
        .byte   $F4
        inc     $FF,x
        beq     LED8D
        .byte   $93
        sty     $90FF
        .byte   $1C
        .byte   $9C
        .byte   $1F
        .byte   $12
        .byte   $FF


        .byte   0       ;FREE

;--------------------------------------
; JIFFY_SCREENDUMP continue #2
; in another free area to be original
;LEDAA:  ldy     #$00            ; column counter
;        sty     $D4             ; clear quotes mode by writing 0 into QTSW
;        jsr     LE50C           ; PLOT, put row and column
;        inc     $D5             ; increment LNMX, maximum screen length
;LEDB3:  jsr     LF221           ; input from screen
;        jsr     LEEE4           ; CIOUT, send data to serial bus (printer)
;        cmp     #$0D            ; carriage return
;        bne     LEDB3           ; next char
;        inx                     ; increment .X, line #
;        cpx     #$17            ; til all rows are done. 
;        bcs     LEDC9           ; exit
;        asl     $D5             
;        bpl     LEDAA           ; next line
;        inx
;        bne     LEDAA           ; next line
;LEDC9:  jsr     LFFAE           ; UNLISTEN
;        pla                     ; restore .X and .Y
;        tax
;        pla
;        tay
;        jsr     LE50C           ; PLOT, restore cursor position
;        pla                     ; return to original GETCHAR routine with keycode in .A
;LEDD5:  rts

.res 57-25-16

;--------------------------------------
DO_CHECK_UNEXPANDED_POINTERS:
;
    ldx #$00
	jsr INIT_VIA_DDA_AND_DDB    ; INIT VIA's for keyboard reading
	jsr SCAN_CONTROL            ; is control pressed?
	beq @0
	jmp EXPANDED				;key not pressed, run as expanded
@0:	jmp UNEXPANDED				;key pressed, run with unexpanded pointers
	
;--------------------------------------
CHECK_AUTOSTART:
;	
    stx $9123					;set data direction for PORTB to input (.X contains 0)
	lda #$ff					;set data direction for PORTA to output
	sta $9122		
	lda #$df
	sta $9120					;keyboard column scan
	lda $9121					;get row scan
	lsr							;Commodore key is bit 0
	bcc @1  					;if bit 0 wasn't set, C= key is pressed, don't start
	pla							;get rid of return address
	pla
@0:	jmp ($a000)					;start cartridge
@1: rts

;--------------------------------------
VIC_SETUP_TABLE:
; a table of initial values for the VIC chip at start up
LEDE3:  .byte   $FF

;horizontal centering
.IFDEF NTSC
        .byte   $05 
.ELSE
        .byte   $0C
.ENDIF

;vertical centering
.IFDEF NTSC
        .byte   $19
.ELSE
        .byte   $26
.ENDIF
        .byte   $16             ; # of columns
        .byte   $2E             ; # of rows
        .byte   $00             ; raster beam
        .byte   $C0             ; character location ($0) and screen matrix ($C)
        .byte   $00
        .byte   $00
        .byte   $00
        .byte   $00
        .byte   $00
        .byte   $00
        .byte   $00
        .byte   $00
        .byte   $00
LEDF3:  .byte   $1B

        jmp     L414F
        .byte   $44
        ora     $5552
        .byte   $4E
LEDFC:  .byte   $0D
LEDFD:  brk
LEDFE:  asl     $2C,x
        .byte   $42
        cli
        ror     $9A84
        .byte   $B0,$C6
        .byte   $DC
        .byte   $F2
        php
        asl     $4A34,x
        rts
        ror     $8C,x
        ldx     #$B8
        .byte   $CE
        .byte   $E4
;--------------------------------------
JIF_TALK:
;
LEE14:  ora     #$40                ;OR $40 (TALK) with current device
        .byte   $2C
;--------------------------------------
JIF_LISTEN:
;
LEE17:  ora     #$20                ;OR $20 (LISTEN) with current device
        jsr     SERIAL_BUS_IDLE     ;check RS232 transmission/reception
LEE1C:  pha                         ;save .A
        bit     $94                 ;is there a character awaiting transmission?
        bpl     LEE2B               ;no, continue
        sec                         ;yes, set carry so ROR brings in a bit
        ror     $A3                 ;set bit 7 of LDFLAG
        jsr     NEW_IECOUT          ;new transmit the byte out
        lsr     $94                 ;flag that the byte has been sent
        lsr     $A3                 ;
LEE2B:  pla                         ;get current device OR'd with $20
        sta     $95                 ;store to BSOUR
        
        jsr     JIFFY_SERIAL_OUT_1  ;

        cmp     #$3F                ;was $912C $3F?
        bne     LEE38               ;no, don't turn CLOCK on
        jsr     SERIAL_CLOCK_ON     ;yes, turn CLOCK on
LEE38:  lda     $911F               ;read serial bus
        ora     #$80                ;
        sta     $911F
LEE40:  jsr     SERIAL_CLOCK_OFF                    ;turn CLOCK off
        jsr     BRING_SERIAL_DATA_LINE_HIGH         ;turn DATA on
        jsr     LEF96

;--------------------------------------
OLD_IECOUT:
;
LEE49:  sei
        jsr     LE4A0           ; DAV lo
        jsr     LE4B2           ; NRFD hi
        lsr     a
        bcs     LEEB4           ; err DEV NOT PRESENT

        jsr     LEF84           ; NDAC lo
        bit     $A3
        bpl     LEE66
LEE5A:  jsr     LE4B2           ; NRFD hi
        lsr     a
        bcc     LEE5A
LEE60:  jsr     LE4B2
        lsr     a
        bcs     LEE60
LEE66:  jsr     LE4B2
        lsr     a
        bcc     LEE66
        jsr     LEF8D
        
        txa
        pha 
        ldx #$08                ; 8 BIT
        ;lda     #$08
        ;sta     $A5

LEE73:  lda     $911F
        and     #$02
        bne     @0
        pla
        tax
        jmp $EEB7               ; err TIMEOUT 

@0:     jsr     LE4A0           ; DAV hi
        ror     $95             ; new
        bcs     @1              ; new
        jsr     $E4A9           ; new DAV lo 
        
@1:     jsr     LEF84           ; NDAC lo
        lda     $912C
        and     #$DD
        ora     #$02
        php
        pha

        jsr     LF96E
        pla
        plp
        dex
        bne     LEE73

        pla
        tax
        nop
LEEA0:  lda     #$04
        sta     $9129
LEEA5:  lda     $912D
        and     #$20
        bne     LEEB7
        jsr     LE4B2
        lsr     a
        bcs     LEEA5
        cli
        rts


;--------------------------------------
FLAG_ERRORS:
;(A) is loaded with one of the two error flags, depending on the entry
; point. #$80 signifies the device was not present, and #$03 signifies
; a write timeout. The value is then set into the I/O status word, ST.
; The routine exits by clearing ATN and giving the final handshake.
;
LEEB4:  lda     #$80                ; flag ?DEVICE NOT PRESENT
        .byte   $2C                 ; mask
LEEB7:  lda     #$03                ; flag write timeout
LEEB9:  jsr     LFE6A               ; set I/O status word
        cli
        clc
        bcc     LEF09               ; always jump, do final handshake


;--------------------------------------
SECOND:
;send listen SA
;The KERNAL routine SECOND ($ff93) is vectored here. On entry, (A) holds
; the secondary address. This is placed in the serial buffer and sent to the
; serial bus "under attension". Finally the routine drops through to the next
; routine to set ATN false.
;
LEEC0:  sta     $95                 ; store .A in BSOUT, buffer for serial bus
        jsr     LEE40               ; handshake an send byte

;--------------------------------------
CLEAR_ATN:
; The ATN, attention, line on the serial bus is set to 1, ie. ATN is now
; false and data sent on the serial bus will not be interpreted as a command.
;
LEEC5:  lda     $911F               ; serial bus I/O port
        and     #$7F                ; clear bit 7, i.e. ATN 1
        sta     $911F               ; store to port
        rts

;--------------------------------------
TKSA:
;send talk SA
;The KERNAL routine TKSA ($ff96) is vectored here. On entry, (A) holds the
; secondary address. This is placed in the serial buffer and sent out to the
; serial bus "under attension". The routine drops through to the next routine
; to wait for CLK and clear ATN.
;
LEECE:  sta     $95                 ; BSOUR, ther serial bus buffer
        jsr     LEE40               ; handshake and send byte to the bus

;--------------------------------------
WAIT_FOR_CLOCK:
; This routine sets data = 0, ATN = 1 and CLK = 1. It then waits to recieve
; CLK = 0 from the serial bus.
;
LEED3:  sei                         ; disable interrupts
        jsr     LE4A9               ; set data 0
        jsr     LEEC5               ; st ATN 1
        jsr     LEF84               ; set CLK 1
LEEDD:  jsr     LE4B2               ; read serial bus I/O port
        bcs     LEEDD               ; wait for CLK = 0
        cli                         ; enable interrupt
        rts

;--------------------------------------
CIOUT:
;aka JIF_IECOUT in SJLoad
;send serial deferred
; The KERNAL routine CIOUT ($ffa8) jumps to this routine. If there is a
; character awaiting output in the buffer, then it is sent on the bus to
; the new JiffyDOS send routine. The output flag, C3PO is set (ie. bit 7 = 1)
; and the contents of (A) is placed in the serial buffer. 
;
LEEE4:  bit     $94             ;is there a byte waiting to be sent?
        bmi     LEEED           ;yes, send it
        sec                     ;no, flag that a byte should be sent...
        ror     $94             ;...
        bne     LEEF2           ;and store the character in the buffer
LEEED:  pha                     ;save the byte to be sent
        jsr     NEW_IECOUT      ;send the byte
        pla                     ;restore the byte that we just sent
LEEF2:  sta     $95             ;store it in BSOUR - the serial character buffer
        clc
        rts


;--------------------------------------
JIF_UNTALK:
;
LEEF6:  lda     $911F
        ora     #$80
        sta     $911F
        jsr     LEF8D
        lda     #$5F
        .byte   $2C

;--------------------------------------
JIF_UNLISTEN:
;
LEF04:  lda     #$3F
        jsr     LEE1C           ; part of listen
LEF09:  jsr     LEEC5
LEF0C:  txa
        ldx     #$0B
LEF0F:  dex
        bne     LEF0F
        tax
        jsr     LEF84
        jmp     LE4A0


;--------------------------------------
ACPTR:
;The KERNAL routine ACPTR ($ffa5) points to this routine in the original
; Commodore KERNAL. JiffyDOS uses a routine at $fbaa, which is the new ACPTR
; pointer. This routine is used when a device is not JiffyDOS equiped. A timing
; loop is entered using the VIA timer, and if a byte is not received in 65 ms, ST
; is set to #$02, ie. a read timeout. A test is made for EOI and if this occurs,
; ST is set to #$40, indicating end of file. The byte is then received from the
; serial bus and built up bit by bit in the temporary stora at #$a4. This is
; transfered to (A) on exit, unless EOI has occured.
;
LEF19:  jmp     JIF_IECIN       ;jump to JiffyDOS ACPTR, return if no JiffyDOS

LEF1C:  sta     $A5             ; CNTDN, counter
        jsr     LEF84           ; set CLK 1
LEF21:  jsr     LE4B2           ; get serial in and clock
        bcc     LEF21           ; wait for CLK = 1
        jsr     LE4A0
LEF29:  lda     #$01
        sta     $9129           ; setup timer 2, high byte
LEF2E:  lda     $912D           ; read timer 2
        and     #$20            ; test if timer 2 reaches zero
        bne     LEF3C           ; timeout
        jsr     LE4B2           ; get serial in and clock
        bcs     LEF2E           ; CLK 1
        bcc     LEF54           ; CLK 0
LEF3C:  lda     $A5             ; CNTDN
        beq     LEF45
        lda     #$02            ; flag read timeout
        jmp     LEEB9           ; set I/O status word
LEF45:  jsr     LE4A9           ; set data 1
        jsr     LEF0C           ; set CLK 1
        lda     #$40            ; flag EOI
        jsr     LFE6A           ; set status word
        inc     $A5             ; increment CNTDN, counter
        bne     LEF29           ; again
LEF54:  lda     #$08            ; set up CNTDN to receive 8 bits
        sta     $A5
LEF58:  lda     $911F           ; serial bus I/O port
        cmp     $911F           ; compare
        bne     LEF58           ; wait for serial bus to settle
        lsr     a
        bcc     LEF58           ; wait for data in = 1
        lsr     a               ; roll data into carry
        ror     $A4             ; roll data (carry) into temp storage
LEF66:  lda     $911F           ; serial bus I/O port
        cmp     $911F           ; compare
        bne     LEF66           ; wait for bus to settle
        lsr     a
        bcs     LEF66           ; wait for data in = 0
        dec     $A5             ; one bit received
        bne     LEF58           ; repeat for all 8 bits
        jsr     LE4A9           ; set data 1
        lda     $90             ; STATUS, I/O status word
        beq     LEF7F           ; not EOI
        jsr     LEF0C           ; handshake and exit without byte
LEF7F:  lda     $A4             ; read received byte
        cli                     ; enable interrupts
        clc                     ; clear carry, no errors
        rts

;--------------------------------------
SERIAL_CLOCK_ON:
; This routine sets the clock outline on the serial bus to 1. This means
; writing a 0 to the port. This value is reversed by hardware on the bus.
;
LEF84:  lda     $912C           ; serial port I/O register
        and     #$FD            ; clear bit 1, CLK out
        sta     $912C           ; store
        rts

;--------------------------------------
SERIAL_CLOCK_OFF:
; This routine sets the clock outline on the serial bus to 0. This means
; writing a 1 to the port. This value is reversed by hardware on the bus.
;
LEF8D:  lda     $912C           ; serial port I/O register
        ora     #$02            ; set bit 1, CLK out
        sta     $912C           ; store
        rts

;--------------------------------------
SERIAL_OUTPUT_1:
;
LEF96:  lda     #$04
        sta     $9129
LEF9B:  lda     $912D
        and     #$20
        beq     LEF9B
        rts
;--------------------------------------
RS232_SEND:
; This routine is concerned with sending a byte on the RS232 port. The
; data is actually written to the port under NMI interrupt control. The CTS
; line generates an NMI when the port is ready for data. If all the bits
; in the byte have been sent, then a new RS232 byte is set up. Otherwise,
; this routine calculates parity and number of stop bits set up in the OPEN
; command. These bits are added to the end of the byte being sent.
;
LEFA3:  lda     $B4             ; BITTS, RS232 out bit count
        beq     LEFEE           ; send new RS232 byte
        bmi     LEFE8
        lsr     $B6             ; RODATA, RS232 out byte buffer
        ldx     #$00
        bcc     LEFB0
        dex
LEFB0:  txa
        eor     $BD             ; ROPRTY, RS232 out parity
        sta     $BD
        dec     $B4             ; BITTS
        beq     LEFBF
LEFB9:  txa
        and     #$20
        sta     $B5             ; NXTBIT, next RS232 bit to send
        rts
LEFBF:  lda     #$20
        bit     $0294           ; 6522 command register image
        beq     LEFDA           ; no parity
        bmi     LEFE4           ; mark/space transmit
        bvs     LEFDE           ; even parity
        lda     $BD             ; ROPRTY, out parity
        bne     LEFCF
LEFCE:  dex
LEFCF:  dec     $B4             ; BITTS, out bit count
        lda     $0293           ; 6552 control register image
        bpl     LEFB9           ; one stop bit only
        dec     $B4             ; BITTS
        bne     LEFB9
LEFDA:  inc     $B4             ; BITTS
        bne     LEFCE
LEFDE:  lda     $BD             ; ROPRTY
        beq     LEFCF
        bne     LEFCE
LEFE4:  bvs     LEFCF
        bvc     LEFCE
LEFE8:  inc     $B4             ; BITTS
        ldx     #$FF
        bne     LEFB9

;--------------------------------------
SEND_NEW_RS232_BYTE:
;This routine sets up the system variables ready to send a new byte to
; the RS232 port. A test is made for 3-line or X-line modus. In X-line
; mode, DSR and  CTS are checked.
;
LEFEE:  lda     $0294           ; 6522 command register
        lsr     a               ; test handshake mode
        bcc     LEFFB           ; 3-line mode (no handshake)
        bit     $9120           ; RS232 port
        bpl     LF016           ; no DSR, error
        bvc     LF019           ; no CTS, error
LEFFB:  lda     #$00
        sta     $BD             ; ROPRTY, RS232 out parity
        sta     $B5             ; NXTBIT, next bit to send
        ldx     $0298           ; BITNUM, number of bits left to send
        stx     $B4             ; BITTS, RS232 out bit count
        ldy     $029D           ; RODBS, start page of out buffer
        cpy     $029E           ; RODBE, index to end if out buffer
        beq     LF021           ; disable timer
        lda     ($F9),y         ; RS232 out buffer
        sta     $B6             ; RODATA, RS232 out byte buffer
        inc     $029D           ; RODBS
        rts

;--------------------------------------
NO_DSR_CTS_ERROR:
; (A) is loaded with the error flag - $40 for no DSR, and $10 for no
; CTS. This is then ORed with 6551 status image and stored in RSSTAT.
;
LF016:  lda     #$40            ; entrypoint for 'NO DSR'
        .byte   $2C
LF019:  lda     #$10            ; entrypoint for 'NO CTS'
        ora     $0297           ; RSTAT, 6522 status register image
        sta     $0297

LF021:  lda     #$40
        sta     $911E
        rts
;--------------------------------------
COMPUTE_BIT_COUNT:
; This routine computes the number of bits in the word to be sent. The word
; length information is held in biits 5 & 6 of M51CTR. Bit 7 of this register
; indicates the number of stop bits. On exit, the number of bits is held in (X).
;
LF027:  ldx     #$09
        lda     #$20
        bit     $0293           ; 6522 control register image
        beq     LF031
        dex
LF031:  bvc     LF035
        dex
        dex
LF035:  rts

;--------------------------------------
RS232_RECEIVE:
; This routine builds up the input byte from the RS232 port in RIDATA.
; Each bit is input from the port under NMI interrupt control. The bit is
; placed in INBIT before being passed to this routine, where it is shifted
; into the carry flag and then rotated into RIDATA. The bit count is
; decremented and parity updated.
;
LF036:  ldx     $A9             ; RINONE, check for start bit
        bne     LF068
        dec     $A8             ; BITC1, RS232 in bit count
        beq     LF06F           ; process received byte
        bmi     LF04D
        lda     $A7             ; INBIT, RS232 in bits
        eor     $AB             ; RIPRTY, RS232 in parity
        sta     $AB
        lsr     $A7             ; INBIT, put input bit into carry
        ror     $AA             ; RIDATA
LF04A:  rts
LF04B:  dec     $A8             ; BITC1
LF04D:  lda     $A7             ; INBIT
        beq     LF0B3
        lda     $0293           ; 6522 control register image
        asl     a
        lda     #$01
        adc     $A8             ; BITC1
        bne     LF04A           ; end

;--------------------------------------
SETUP_TO_RECEIVE:
; This routine sets up the I.C.R. to wait for the receiver edge, and flags
; this into ENABL. It then flags the check for a start bit.
;
LF05B:  lda     #$90
        sta     $911E           ; VIA #2 I.C.R.
        sta     $A9             ; RINONE, check for start bit
        lda     #$20
        sta     $911E           ; disable timer 
        rts                     ; exit

;--------------------------------------
PROCESS_RS232_BYTE:
; The byte recieved from the RS232 port is checked against parity.
; This involvs checking the input parity options selected, and then
; verifying the parity bit calculated against that input. If the
; test is passed, then the byte is stored in the in-buffer. Otherwise
; an error is flagged into RSSTAT.
;
; A patch in KERNAL version 3, could be added to the input routine at $ef94 (if there 
; was room) to initialise the RS232 parity byte, RIPRTY, on reception of a start bit.
;
LF068:  lda     $A7             ; INBIT, RS232 in bits
        bne     LF05B           ; set up to receive
        jmp     RS232_PATCH     ; RS232 patch, was just: sta $a9
LF06F:  ldy     $029B           ; RIDBE, index to the end of in buffer
        iny
        cpy     $029C           ; RIDBS, start page of in buffer
        beq     LF0A2           ; receive overflow error
        sty     $029B           ; RIDBE
        dey
        lda     $AA             ; RIDATA, RS232 in byte buffer
        ldx     $0298           ; BITNUM, number of bits left to send
LF081:  cpx     #$09            ; full word to come?
        beq     LF089           ; yes
        lsr     a
        inx
        bne     LF081
LF089:  sta     ($F7),y         ; RIBUF, RS232 in buffer
        lda     #$20
        bit     $0294           ; 6522 command register image
        beq     LF04B           ; parity disabled
        bmi     LF04A           ; parity check disabled, TRS
        lda     $A7             ; INBIT, parity check
        eor     $AB             ; RIPRTY, RS232 in parity
        beq     LF09D           ; receive parity error
        bvs     LF04A
        .byte   $2C             ; mask
LF09D:  bvc     LF04A
        lda     #$01            ; receive parity error
        .byte   $2C
LF0A2:  lda     #$04            ; receive overflow
        .byte   $2C
LF0A5:  lda     #$80            ; framing break
        .byte   $2C
LF0A8:  lda     #$02            ; framing error
        ora     $0297           ; RSSTAT, 6522 status register image
        sta     $0297
        jmp     LF05B           ; set up to receive 
LF0B3:  lda     $AA             ; RIDATA
        bne     LF0A8           ; framing error
        beq     LF0A5           ; receive break
LF0B9:  jmp     LF796           ; I/O error #9, illegal device number

;--------------------------------------
SUBMIT_TO_RS232:
; This routine is called when data is required from the RS232 port. Its
; funktion is to perform the handshaking on the poort needed to receive
; the data. If 3 line mode is used, then no handshaking is implemented
; and the routine exits.
;
LF0BC:  sta     $9A                 ; DFLTO, default output device
        lda     $0294               ; 6522 command register image
        lsr     a
        bcc     LF0EB               ; 3 line mode, no handshaking, exit
        lda     #$02
        bit     $9110               ; RS23 I/O port
        bpl     LF0E8               ; no DRS, error
        bne     LF0EB
LF0CD:  lda     $911E               ; ENABL, RS232 enables????
        and     #$30
        bne     LF0CD
LF0D4:  bit     $9110               ; RS232 I/O port
        bvs     LF0D4               ; wait for no CTS
        lda     $9110
        ora     #$02
        sta     $9110               ; set RTS
LF0E1:  bit     $9110
        bvs     LF0EB               ; CTS set
        bmi     LF0E1               ; wait for no DSR
        
;--------------------------------------
NO_DSR_ERROR:
;This routine sets the 6522 status register image to $40 when a no DSR error
;has occurred.
;
LF0E8:  jsr     LF016
LF0EB:  clc
        rts

;--------------------------------------
SEND_TO_RS232_BUFFER:
;
LF0ED:  ldy     $029E
        iny
        cpy     $029D
        beq     LF0ED
        sty     $029E
        dey
        sta     ($F9),y
        bit     $911E
        bvc     LF102
        rts
LF102:  lda     $0299
        sta     $9114
        lda     $029A
        sta     $9115
        lda     #$C0
        sta     $911E
        jmp     LEFEE

;--------------------------------------
INPUT_FROM_RS232:
;
LF116:  sta     $99
        lda     $0294
        lsr     a
        bcc     LF146
        and     #$08
        beq     LF146
        lda     #$02
        bit     $9110
        bpl     LF0E8
        beq     LF144
LF12B:  bit     $911E
        bvs     LF12B
        lda     $9110
        and     #$FD
        sta     $9110
LF138:  lda     $9110
        and     #$04
        beq     LF138
LF13F:  lda     #$90
        sta     $911E
LF144:  clc
        rts
LF146:  lda     $911E
        and     #$30
        beq     LF13F
        clc
        rts

;--------------------------------------
GET_FROM_RS232:
;
LF14F:  ldy     $029C
        cpy     $029B
        beq     LF15D
        lda     ($F7),y
        inc     $029C
        rts
LF15D:  lda     #$00
        rts

;--------------------------------------
SERIAL_BUS_IDLE:
; This routine checks the RS232 bus for data transmission/reception.
; The routine waits for any activity on the bus to end before setting
; I.C.R. The routine is called by serial bus routines, since these devices
; use IRQ generated timing, and conflicts may occur if they are all used
; at once.
LF160:  pha                     ; store .A
        lda     $911E           ; ENABL, RS232 enables
        beq     LF172           ; bus not in use
LF166:  lda     $911E           ; ENABL
        and     #$60            ; test RS232
        bne     LF166           ; yes, wait for port clear
        lda     #$10
        sta     $911E           ; set up I.C.R
LF172:  pla                     ;retrieve .A
        rts

;--------------------------------------
; TABLE OF KERNAL I/O MESSAGES 1
;
LF174:  .byte   $0D,$49,$2F,$4F,$20,$45,$52,$52,$4F,$52,$20,$A3         ; i/o error #
        .byte   $0D,$53,$45,$41,$52,$43,$48,$49,$4E,$47,$A0             ; searching
        .byte   $46,$4F,$52,$A0                                         ; for
        


;--------------------------------------
JIFFY_SET_CHKIN:
; This routine is a new JiffyDOS routine which clears all
; I/O and sets up the current JiffyDOS filenumber as default
; inputdevice by calling CHKIN.
;
LF197:  lda     $9F             ; JiffyDOS logical filenumber
LF199:  pha                     ; store .A
        jsr     $FFCC           ; CLRCHN
        pla                     ; retrieve .A
        tax
        jmp     $FFC6           ; CHKIN vector, opne channel for input
        
;--------------------------------------
JIFFY_SERIAL_OUT_1:
; clear $A3, the load flag and sets the data out line
; on the serial bus to 1.
;
        sei
        lda     #$00            ; clear JiffyDOS device flag
        sta     $A3
        jmp     LE4A0           ; serial output 1
        
;--------------------------------------
JIFFY_SEND_DRIVE_COMMAND:
; This routine uses the values in (X) and (Y) to send a command to
; the drive. (X) contains a offset to the command, and (Y) contains
; the length of the command.
;
        txa                     ; temp store
        pha
        jsr     LF825           ; open command channel for output
        pla
        tax                     ; retrieve .X
@0:     lda     JIFFY_DIRECT_DRIVE_COMMANDS,x   ; read command from table
        jsr     $FFD2           ; output character to drive
        inx
        dey
        bne     @0
        rts

;--------------------------------------
; TABLE OF KERNAL I/O MESSAGES 2
;
        .byte   $0D,$4C,$4F,$41,$44,$49,$4E,$C7         ; loading
        .byte   $0D,$53,$41,$56,$49,$4E,$47,$A0         ; saving
        .byte   $0D,$56,$45,$52,$49,$46,$59,$49,$4E,$C7 ; verifying
        .byte   $0D,$46,$4F,$55,$4E,$44,$A0             ; found
        .byte   $0D,$4F,$4B,$8D                         ; OK
;END OF TABLE

;--------------------------------------
INIT_VIA_DDA_AND_DDB:
;
	stx $9123
	lda #$ff
	sta $9122
	rts
;	.res    9               ; FREE
        
;---------------------------------------
PRINT_MESSAGE_IF_DIRECT:
; This is a routine to output a message from the I/O messages table
; at $xxxx. On entry, (Y) holds the offset to control which message
; is printed. The routine tests if we are in program mode or direct
; mode. If in program mode, the routine exits. Else, the routine prints
; character after caracter untill it reaches a character with bit7 set.
;
LF1E2:  bit     $9D             ; MSGFLG, test if direct or program mode
LF1E4:  bpl     LF1F3           ; program mode, don't print message
LF1E6:  lda     LF174,y         ; get output character from table
        php                     ; store processor status
        and     #$7F            ; clear bit 7
        jsr     LFFD2           ; output character using CHROUT
        iny                     ; increment pointer to next character
        plp                     ; retrieve message
        bpl     LF1E6           ; until bit 7 was set
LF1F3:  clc                     ; clear carry to indicate no error
        rts

;--------------------------------------
GETIN:
; The KERNAL routine GETIN ($ffe4) is vectored to this routine. It load
; a character into fac#1 from the external device indicated by DFLTN. Thus,
; if device = 0, GET is from the keyboard buffer. If device = 2, GET is from
; the RS232 port. If niether of these devices then GET is further handled
; by the next routine, INPUT.
;
        
LF1F5:  lda     $99             ; DFLTN, default input device
        bne     LF1FF           ; not keyboard
        lda     $C6             ; NDX, number of keys in keyboard queue
        beq     LF26A           ; buffer empty, exit
        sei                     ; disable interrupts
        jmp     LE5CF           ; get character from keyboard buffer, and exit
LF1FF:  cmp     #$02            ; RS232
        bne     LF21D           ; nope, try next device
LF205:  sty     $97             ; temp store
        jsr     LF14F           ; get character from RS232
        ldy     $97             ; retrieve .Y
        clc
        rts

;--------------------------------------
CHRIN:
;input a byte
; The KERNAL routine CHRIN ($ffcf) is vectored to this routine. It is
; similar in function to the GET routine above, and also provides a
; continuation to that routine. If the input device is 0 or 3, ie. keyboard
; or screen, then input takes place from the screen. INPUT/GET from other
; devices are performed by calls to the next routine. Two bytes are input
; from the device so that end of file can be set if necessary (ie. ST = #40)
;
        lda     $99             ; DFLTN, default input
        bne     LF260           ; not keyboard, next device 
        lda     $D3             ; PNTR, cursor column on screen
        sta     $CA             ; >LXSP, cursor position at start
        lda     $D6             ; TBLX, cursor line number
        sta     $C9             ; LXSP
        jmp     LE64F           ; input from screen or keyboard
LF21D:  cmp     #$03            ; screen?
        bne     LF22A           ; nope, next device
LF221:  sta     $D0             ; CRSw, flag INPUT/GET from keyboard
        lda     $D5             ; LNMX, physical screen line length
        sta     $C8             ; end of logical line for input
        jmp     LE64F           ; input from keyboard
LF22A:  bcs     LF264
        cmp     #$02            ; RS232
        beq     LF26F           ; yes, get data from RS232 port

LF230:  jsr     JIF_IECIN       ; JifyyDOS IECIN, get a byte from serial bus 
        pha                     ; temp store
        bit     $A3             ; test bit 6, if serial device is a JiffyDOS device
        bvc     @1              ; not JiffyDOS
        cpx     #$00
        bne     @0
        lda     $C4             ; ????????
@0:     cmp     #$04
        bcc     @1
        ldy     #$00            ; clear offset
        lda     ($BB),y         ; FNADR, pointer to current filename
        cmp     #$24            ; first character is $, i.e. directory
        beq     @1              ; yes, exit
        inc     $B9             ; increment SA
        jsr     JIFFY_TALK_AND_TKSA
        dec     $B9             ; decrement SA
        asl     $A3
@1:     pla
        rts

;--------------------------------------
OLD_IVECTORS_TABLE:
;
.word   IERROR_OLD              ; IERROR vector
.word   $c483                   ; IMAIN vector
.word   $c57c                   ; ICRNCH vector

.res 11-6               ; FREE

	
LF260:  cmp     #$04    ; RS232?
        bcc     LF21D   ; nope, go back and check for 
;--------------------------------------
GET_FROM_SERIAL_RS232:
;These routines, actually two different, is entered from the
; previous routine. The serial sectionchecks the state of ST. If zero,
; then the data is recieved from the bus, otherwise carriage return
; (#0d) is returned in (A). In the second section, the recieved byte
; is read from the RS232 port.
;
LF264:  lda     $90             ; status, I/O status word
        beq     LF26C           ; status OK
LF268:  lda     #$0D            ; else, return <CR> and exit
LF26A:  clc
LF26B:  rts
LF26C:  jmp     JIF_IECIN       ; JiffyDOS ACPTR, get byte from serial bus 
LF26F:  jsr     LF205           ; receive from RS232
LF272:  bcs     LF279
LF274:  cmp     #$00
LF276:  beq     LF26F           ; repeat
        clc
LF279:  rts

;--------------------------------------
CHROUT:
;output one character
; The KERNAL routine CHROUT ($ffd2) is vectored to this routine. 
; On entry, (A) must hold the character to be output. The default 
; output device number is examined, and output directed to relevant 
; device. The screen, serial bus and RS232 all use previously described 
; routines for their output.
; Some old taperoutines have been removed in the middle of this 
; routine, and been changed to a JiffyDOS routine.
;
        pha                     ; temp store to stack
        lda     $9A             ; DFLTO, default output device
        cmp     #$03            ; screen?
        bne     LF285           ; nope, test next device
        pla                     ; retrieve .A
        jmp     LE742           ; output to screen
LF285:  bcc     LF28B           ; device < 3
        pla                     ; retrieve .A
        jmp     LEEE4           ; send serial deferred

; removed the tape junk leftover in the C64 JiffyDOS
LF28B:  cmp     #$02            ; device 2?
        beq     LF2B9           ; yes, RS232
        
        pla                     ; retrieve .A
        jmp     LF78A           ; output device not present
LF293:  jsr     LF943           ; entry point in JIFFY_OPEN_COMMAND_CHANNEL
        jsr     LE48F           ; input byte from command channel
        cmp     #$30
        rts
        
;--------------------------------------
JIFFY_DEFAULT_DEVICE:
;The following routine sets the default device number. It uses 
;the GTBYTC procedure to read the specifyed device number.
;
        jsr     $D79B           ; GTBYTC, read device # from keyboard 
        stx     $BA             ; store in FA, current device #
        jsr     LF7DF           ; test if FA is present
        stx     $BE             ; if everything's OK store in default device #
        rts

;--------------------------------------
SCAN_CONTROL:
;scan the keyboard to see if control is pressed. This is used in the SCROLL_SCREEN
;routine
;   
        lda #$FB
        sta $9120
        lda $9121
        
        cmp #$FE
        php
        lda #$F7
        sta $9120
        plp
        rts       

;--------------------------------------
CHROUT_2:
; This is the second part of the CHROUT routine. It contains
; the last parts of the RS232 output routine.
;
LF2B9:  pla
        stx     $97
        sty     $9E
        jsr     SEND_TO_RS232_BUFFER
        ldx     $97
        ldy     $9E             ;PTR1
        clc
        rts

;--------------------------------------
CHKIN:
;set input device
; The KERNAL routine CHKIN ($ffc6) is vectored to this routine.
; On entry, (X) must hold the logical file number. A test is made
; to see if the file is open, or ?FILE NOT OPEN. If the file is not
; an input file then ?NOT INPUT FILE. If the device is on the serial
; bus then it is commanded to TALK  and secondary address is sent.
; ST is then checked, and if non-zero, ?DEVICE NOT PRESENT. Finally,
; the device number is stored in DLFTN.
;
        jsr     LF3CF           ; search logical file # .X
        beq     LF2CF           ; OK
        jmp     LF784           ; I/O error #3, file not open

LF2CF:  jsr     LF3DF           ; set file param
        lda     $BA             ; FA, current device number
        beq     LF2EC           ; keyboard
        cmp     #$03            ; screen?
        beq     LF2EC           ; yes
        bcs     LF2F0           ; serial bus device
        cmp     #$02            ; RS232
        bne     LF2E3           ; nope
        jmp     LF116           ; input from RS232

LF2E3:  ldx     $B9             ; SA, current secondary address
        cpx     #$60
        beq     LF2EC           ; not output file error
        jmp     LF78D           ; I/O error #6, not output file
LF2EC:  sta     $99             ; DFLTN, default input device
        clc                     ; clear carry to indicate no errors
        rts
        
LF2F0:  tax                     ; file .X to .A
        jsr     LEE14           ; send LISTEN to serial device
        lda     $B9             ; SA
        bpl     LF2FE           ; send SA
        jsr     LEED3           ;
        jmp     LF301
LF2FE:  jsr     LEECE           ; send listen secondary address
LF301:  txa
        bit     $90             ; staus, I/O status word
        bpl     LF2EC           ; OK, set output device
        jmp     LF78A           ; I/O error #5, device not present

;--------------------------------------
CHKOUT:
;set output device
; The KERNAL routine CHKOUT ($ffc9) is vectored to this routinr. On
; entry (X) must hold the logical filenumber. A test is made to see
; if the file is open, or ?FILE NOT OPEN error. If the device is 0, ie.
; the keyboard, or the file is not an output file, then ?FILE OUTPUT FILE
; error is generated. If the device is on the serial bus, then it commanded
; to LISTEN and the secondary address is sent. ST is then checked and if
; non-zero, then ?DEVICE NOT PRESENT error. Finally, the device number is
; stored in DFLTO.
;
        jsr     LF3CF           ; find file number (.X)
        beq     LF311           ; OK
        jmp     LF784           ; I/O error #3, file not open
LF311:  jsr     LF3DF           ; set file values
        lda     $BA             ; FA, current device number
        bne     LF31B           ; not keyboard
LF318:  jmp     LF790           ; I/O error #7, not output file
LF31B:  cmp     #$03            ; screen?
        beq     LF32E           ; yes
        bcs     LF332           ; serial bus device
        cmp     #$02            ; RS232
        bne     LF328           ; nope
        jmp     LF0BC           ; submit to RS232
LF328:  ldx     $B9             ; SA, current secondary address
        cpx     #$60
        beq     LF318           ; not output file error
LF32E:  sta     $9A             ; DFLTO, default output device
        clc                     ; clear carry to indicate no errors
        rts
LF332:  tax                     ; file .X to .A
        jsr     LEE17           ; send LISTEN to serial device
        lda     $B9             ; SA
        bpl     LF33F           ; send SA
        jsr     LEEC5           ; clear ATN
        bne     LF342
LF33F:  jsr     LEEC0           ; send listen secondary address
LF342:  txa
        bit     $90             ; STATUS, I/O status word
        bpl     LF32E           ; OK, set output device
        jmp     LF78A           ; I/O error #5, device not present

;--------------------------------------
CLOSE:
;close file, part 1
; The KERNAL routine CLOSE ($ff3c) is vectored here. The file parameters
; are fetched, and if not found, the routine exits without any action.
; It checks the device number associated with the file. If it is RS232,
; then the RS232 port is reset. If it is a serial device, the device is
; UNTALKed, or UNLISTENed. Finally the number of open logical files are
; decremented, and the table of active file numbers are updated. On entry (A)
; holds the file number to close. Old tape routines have been
; removed for new JiffyDOS routines.
;
        jsr     LF3D4               ;find logical file, .X holds location i table
        beq     LF351               ;OK
        clc                         ;file not found
        rts                         ;and exit
LF351:  jsr     LF3DF               ;get file values from table, position .X
        txa
        pha                         ;store position
        lda     $BA                 ;FA, current device number
        beq     LF3B1               ;keyboard?, update file table
        cmp     #$03                ;screen?
        beq     LF3B1               ;yep, update file table
        bcs     LF3AE               ;serial bus
        cmp     #$02                ;RS232
        bne     LF38D               ;nope, serial
        pla                         ;retrieve .A (table position)
        jsr     LF3B2               ;remove entry .A from file table
        lda     #$7D                ;init RS232 port
        sta     $911E   
        lda     #$06
        sta     $9110
        lda     #$EE
        sta     $911C
        jsr     LFE75               ;MEMTOP, read top of memory (.X/.Y)
        lda     $F8                 ;>RIBUF, RS232 input buffer
        beq     LF37F
        iny
LF37F:  lda     $FA                 ;>ROBUF, RS232 output buffer
        beq     LF384
        iny
LF384:  lda     #$00                ;clear RS232 input/output buffers
        sta     $F8
        sta     $FA
        jmp     LF53C               ;set new ROBUF values and new MEMTOP

LF38D:  pla                     ;retrieve .A
        jmp     LF796           ;error #9, illegal device #
LF391:  jsr     $FFCC           ;CLRCHN, close all channels
LF394:  lda     #$6F
        jsr     LF3D4           ;FIND FILE, test if file $6F is open
        bne     LF3CE           ;file not open, return
        jmp     LF3B3           ;close file $6F

;--------------------------------------
;JIFFY DATA (from SJLoad)
LF39E: .byte $00,$20,$00,$20,$02,$22,$02,$22,$00,$20,$00,$20,$02,$22,$02,$22
;END OF TABLE
;--------------------------------------
CLOSE2:
;part 2 of CLOSE
;
LF3AE:  jsr     LF6DA               ;UNTALK/UNLISTEN serial device
LF3B1:  pla
LF3B2:  tax
LF3B3:  dec     $98                 ;decrement LDTND, number of open files
        cpx     $98                 ;compare LDTND to .X
        beq     LF3CD               ;equal, closed file = last file in table
        ldy     $98                 ;else, move last entry to position of closed entry
        lda     $0259,y             ;LAT, active file numbers
        sta     $0259,x
        lda     $0263,y             ;FAT, active device numbers
        sta     $0263,x
        lda     $026D,y             ;SAT, active secondary addresses
        sta     $026D,x
LF3CD:  clc
LF3CE:  rts                         ;return

;--------------------------------------
FIND_FILE:
; This routine finds a logical file from it's file number. On entry,
; (X) must hold the logical file number to be found. LAT, the table
; of file numbers is searched, and if found (X) contains the offset
; to the position of the file in the table, and the Z flag is set. If
; not found, Z=0.
;
LF3CF:  lda     #$00
        sta     $90             ;clear STATUS
        txa                     ;file number to search for
LF3D4:  ldx     $98             ;LDTND, number of open files
LF3D6:  dex 
        bmi     LF3EE           ;end of table, return
        cmp     $0259,x         ;compare file number with LAT, table of open files
        bne     LF3D6           ;not equal, try next
        rts                     ;back with Z flag set

;--------------------------------------
SET_FILE_VALUES:
; This routine sets the current logical file number, device number and
; secondary address from the file parameter tables. On entry (X) must
; hold the offset to the position of the file in the table.
;
LF3DF:  lda     $0259,x
        sta     $B8
        lda     $0263,x
        sta     $BA
        lda     $026D,x
        sta     $B9
LF3EE:  rts

;--------------------------------------
CLALL:
;abort all files
; The KERNAL routine CLALL ($ffe7) is vectored here. The number of
; open files are set to zero, and the next routine is performed.
;
        lda     #$00
        sta     $98             ;clear LDTND, no open files

;--------------------------------------
CLRCHN:
;restore to default I/O
; The KERNAL routine CLRCHN ($ffcc) is vectored here. The default output
; device is UNLISTENed, if it is on the serial bus, and the default output
; is set to the screen. The default input device is UNTALKed, if it is on the
; serial bus, and the default input device is set to keyboard.
;
        ldx     #$03            ;check if device > 3 (serial bus is 4,5,...)
        cpx     $9A             ;test DFLTO, default open device
        bcs     LF3FC           ;nope, no serial device
        jsr     LEF04           ;send UNLISTEN to serial bus
LF3FC:  cpx     $99             ;test DFLTI, default input device
        bcs     LF403           ;nope, no serial device
        jsr     LEEF6           ;send UNTALK to serial bus
LF403:  stx     $9A             ;store screen as DFLTO
        lda     #$00
        sta     $99             ;store keyboard as DFLTI
        rts

;--------------------------------------
OPEN:
;open file
; The KERNAL routine OPEN ($ffc0) is vectored here. The file paramerters
; must be set before entry. The routine reads the LAT, to see if file already
; exists, which will result in I/O error #2, ?FILE OPEN. A test is made to
; see if more than 10 files are open. If so, I/O error #1, ?TOO MANY FiLES,
; will occur. The file parameters are set, and put in their respective tables.
; The device number is checked, and each kind of device jumps to their own
; routine. Keyboard and screen will exit here with no further actions. RS232
; is opened via a seperate routine. SA, secondary address, and filename will
; be sent on the serial bus. Some tape routines are removed, and replaced with
; JiffyDOS code.
;
        ldx     $B8             ;LA, current logical number
        bne     LF411
        jmp     LF78D           ;I/O error #6, not input file
LF411:  jsr     LF3CF           ;find file (.X)
        bne     LF419
        jmp     LF781           ;I/O error #2, file exists
LF419:  ldx     $98             ;LDTND, number of open files
        cpx     #$0A            ;more than 10
        bcc     LF422           ;nope
        jmp     LF77E           ;I/O error #1, too many files
LF422:  inc     $98             ;increment LDTND
        lda     $B8             ;LA
        sta     $0259,x         ;store in LAT, table of active file numbers
        lda     $B9             ;LA
        ora     #$60            ;fix
        sta     $B9             ;store in SA
        sta     $026D,x         ;store in SAT, table of active secondary addresses
        lda     $BA             ;FA
        sta     $0263,x         ;store in FAT, table of active device numbers
        beq     LF493           ;keyboard, end
        cmp     #$03            ;screen?
        beq     LF493           ;yep, end
        bcc     LF444           ;less than 3, not serial bus
        jsr     LF495           ;send SA
        bcc     LF493           ;end
LF444:  cmp     #$01            ;TAPE
        beq     LF4AF           ;I/O error #5, device not present
        jmp     LF4C7           ;open RS232 file

;--------------------------------------
JIFFY_TALK_AND_TKSA:
;This is a routine used by JiffyDOS to untalk device (A), then 
;TALK and TKSA is executed to current device with current secondary 
;address.
;
LF44B:  jsr     LFFAB           ; UNTALK
        lda     $BA             ; FA, current device #
        jsr     $FFB4           ; TALK
        lda     $B9             ; SA, current secondary address
        jmp     LFF96           ; TKSA, send SA after talk

brk                     ; garbage?
asl     $1C             ; garbage

;--------------------------------------
JIFFY_DIRECT_DRIVE_COMMANDS:
;The following text/code is used to transfer, and is transfered to 
;a selected drive. The first section is a $22 byte long block used 
;by the lock/unlock a file. The second section is code to execute a 
;drive program at $0600. The third section sets a byte in the drive 
;memory to control the interleave. The fourth section sets a byte in 
;the drive memory to control the 1541 head rattle.
; TODO: don't know about this stuff
;
        lda     $0261           ; the following code is transferred to the drive at $0600
        sta     $07
        lda     #$12
        sta     $06
        ldx     #$00
        stx     $F9
        jsr     $D586
        ldy     $0267
        lda     ($30),y
        eor     #$40
        sta     ($30),y
        jmp     $D58A
.byte   $4D, $2D, $45, $00, $06         ; M-E 00 06, i.e. a memory execute at $0600
.byte   $4D, $2D, $57, $6A, $00, $01    ; M-W 6A 00 01, i.e. memory write one byte at $006a
.byte   $4D, $2D, $57, $69, $00, $01    ; M-W $69 00 01, i.e. memory write one byte at $0069
        ora     ($50,x)
        ror     a:$01
        .byte   $53     ; sed $, I guess
        .byte   $3A     ; rol $
        sta     $A6

LF493:  clc
LF494:  rts
;--------------------------------------
SEND_SA:
; This routine exits if there is no secondary address or filename specified.
; The I/O status word, ST, is reset, and the serial device is commanded to
; LISTEN. A check is made for a possible ?DEVICE NOT PRESENT error. Finally,
; the filename is sent to the device.
;
LF495:  lda     $B9             ;SA, current secondary address
        bmi     LF4C5           ;exit
        ldy     $B7             ;FNLEN, length of filename
        beq     LF4C5           ;exit
        
        lda     $BA             ;FA, current device number
        jsr     LEE17           ;send LISTEN to serial bus
        lda     $B9             ;SA
        ora     #$F0
        jsr     LEEC0           ;send LISTEN SA
        lda     $90             ;STATUS
        bpl     LF4B2           ;OK
LF4AD:  pla                     ;remove return address
        pla
LF4AF:  jmp     LF78A           ;I/O error #5, device not present
LF4B2:  lda     $B7             ;FNLEN
        beq     LF4C2           ;unlisten and exit
        ldy     #$00            ;clear offset
LF4B8:  lda     ($BB),y         ;FNADR, pointer to filename
        jsr     LEEE4           ;send byte on serial bus
        iny                     ;next character
        cpy     $B7             ;until entire filename is sent
        bne     LF4B8           ;again
LF4C2:  jsr     LEF04           ;unlisten 
LF4C5:  clc
        rts                     ;exit

;--------------------------------------
OPEN_RS232:
;
LF4C7:  lda     #$06
        sta     $9112
        sta     $9110
        lda     #$EE
        sta     $911C
        ldy     #$00
        sty     $0297
LF4D9:  cpy     $B7
        beq     LF4E7
        lda     ($BB),y
        sta     $0293,y
        iny
        cpy     #$04
        bne     LF4D9
LF4E7:  jsr     LF027
        stx     $0298
        lda     $0293
        and     #$0F
        bne     LF4F4
LF4F4:  asl     a
        tax
        lda     LFF5A,x
        asl     a
        tay
        lda     LFF5B,x
        rol     a
        pha
        tya
        adc     #$C8
        sta     $0299
        pla
        adc     #$00
        sta     $029A
        lda     $0294
        lsr     a
        bcc     LF51B
        lda     $9120
        asl     a
        bcs     LF51B
        jmp     LF016
LF51B:  lda     $029B
        sta     $029C
        lda     $029E
        sta     $029D
        jsr     LFE75
        lda     $F8
        bne     LF533
        dey
        sty     $F8
        stx     $F7
LF533:  lda     $FA
        bne     LF53C
        dey
        sty     $FA
        stx     $F9
LF53C:  sec
        lda     #$F0
        jmp     LFE7B

;--------------------------------------
LOAD:
;load RAM
; The kernal routine LOAD ($ffd5) is vectoed here. If a relocated load
; is desired, then the start address is set in MEMUSS. The load/verify
; flag is set, and the I/O status word is reset. A test is done on the
; device number, less than 3 results in illigal device number.
;
LF542:  stx     $C3			;MEMUSS, relocated load address
        sty     $C4
        jmp     (L0330)		;ILOAD vector. points to $f549 (next instruction)
LF549:  sta     $93			;VRECK, load/verify flag
        lda     #$00
        sta     $90			;clear STATUS, I/O status 
        lda     $BA			;get FA, current device
        bne     LF556		;keyboard
LF553:  jmp     LF796		;I/O error #9, illegal device
LF556:  cmp     #$03		;screen?
        beq     LF553		;yes, illegal device

;--------------------------------------
LOAD_FROM_SERIAL_BUS:
;A filename is assumed by the routine, and if not present, a jump is made
; to a new JiffyDSO routine that sets filename to ':*'. The message 'SEARCHING'
; is printed and the filename is sent with the TALK command and secondary
; address to the serial bus. If EOI occurs at this point, then ?FILE NOT FOUND
; is displayed. The message 'LOADING' or 'VERIFYING' is output and a loop is
; entered, which recieves a byte from the serial bus, checks the <STOP> key
; and either stores the received byte, or compares it to the memory, depending
; on the state of VERCK. Finally the bus is UNTALKed.
;
        bcc     LF553           ; device <3, e.g. tape or RS232- illegal device
        ldy     $B7             ; FNLEN, length of filename
        bne     LF563           ; if length is not zero,
        jmp     JIFFYDOS_DEFAULT_FILENAME           ; fix filename, JiffyDOS patch
LF563:  ldx     $B9             ; .X = secondary address
        jsr     LF647           ; print "SEARCHING"
        lda     #$60
        sta     $B9             ; set SA to $60
        jsr     LF495           ; send SA and filename
NORMAL_LOAD:
        lda     $BA             ; FA, current device number
        jsr     LEE14           ; send TALK to serial bus
        lda     $B9             ; SA
        jsr     LEECE           ; send TALK SA
        jsr     LEF19           ; receive from serial bus
        sta     $AE             ; load address, <EAL
        lda     $90             ; check STATUS
        lsr     a
        lsr     a
        bcs     LF5C7           ; EOI set, file not found
        jsr     LF230           ; receive from serial bus
        sta     $AF             ; load address, >EAL
        jmp     PATCH_TO_ORIGINAL_LOAD ;formerly: jsr     LE4C1
;LF58A:  lda     #$FD
;        and     $90
;        sta     $90
LF58A:  jsr     LFFE1           ; scan <STOP>
        bne     LF592           ; not stopped
        jmp     LF6CB           ; exit
LF592:  jsr     JIF_IECIN       ; JiffyDOS ACPTR, retrieve from serial bus

        lda     $90             ; read ST
        and     #$FD            ; mask %11111101
        cmp     $90
        sta     $90
        bne     LF58A           ; EOI set
        ldy     #$00
        ldx     $A3
        lda     $A4
        cpy     $93             ; VERIFY or LOAD flag
        beq     LF5B3           ; LOAD
        cmp     ($AE),y         ; compare with memory
        beq     LF5B5           ; verified byte OK
        
        jsr     LFCDE           ; lda #$10, jmp LFE6A
        
        .byte   $2C             ; mask next write command
LF5B3:  sta     ($AE),y         ; store in memory
LF5B5:  stx     $A3
        inc     $AE             ; increment <EAL, next address

        bne     LF5BB           ; skip MSB
        inc     $AF             ; increment >EAL
LF5BB:  bit     $90             ; test STATUS
        bvc     LF58A           ; get next byte
LF5BF:  jsr     LEEF6           ; send UNTALK to serial bus
        jsr     LF6DA
        bcc     LF642           ; continue to LOAD_END + 1 (no need to clc)
LF5C7:  jmp     LF787           ; send file not found message

;--------------------------------------
JIFFY_AT_COMMAND:
; The following routine executes the @ command. First it tests
; if additional parameters are entered.
;
LF5CA:  lda     $B7     ; FNLEN, length of current filename
        beq     LF5DB   ; no filename
        lda     ($BB),y ; test filename for...
        cmp     #$24    ; $ (directory)
        beq     LF604   ; list directory
LF5D4:  jmp     NORMAL_LOAD     ; load

;--------------------------------------
JIFFY_LIST_ASCII_FROM_DISK:
; This routine lists an ascii file from disk. It reads one block of text
; from the disk (254 bytes) into the filename area. The text is then output
; using the 'print filename' routine.
;
LF5D7:  tya             ; .Y contains the command number
LF5D8:  pha             ; store on stack
        jsr     LF943   ; 
        pla             ; retrieve
LF5DB:  sta     $A6     ; store
LF5DD:  jsr     JIFFY_DISPLAY_ASCII_FILE   ; input characters to buffer (filename area)
        bne     @done   ; exit if errors occured
        lda     $A6     ; get command number, should be $0F
        php
        beq     @0
        jsr     LE48F   ; input byte from command channel
        beq     @1      ; if byte =#, exit
@0:     jsr     LF81D
        jsr     PRINT_FILENAME   ; print filename, i.e. the input buffer
        lda     $91     ; STKEY FLAG, test if <STOP> is pressed
        lsr     a
        bcc     @1      ; exit
        plp
        bne     LF5DB
        bvc     LF5DB
        .byte   $24     ; mask one byte (the plp)
@1:     plp
@done:  rts

;--------------------------------------
JIFFY_BASIC_DISC_LIST:
;The following routine reads the specifyed basic-file 
;from disk and displays it to the screen. The entrypoint
;at $f56c is used for showing the directory. First, the
;routine opens the file specifyed. IERROR vector is 
;changed to $f739, so a RTS command will be performed
;when a error occurs. Then the start address is read,
;and thrown away.
;
        ldx     #$6C    ; get byte for SA, list basic program
        .byte   $2C     ; mask next 2 bytes
LF604:  ldx     #$60    ; get byte for SA, list directory
        jsr     LF945   ; open file with current parameters
        lda     #$BC    ; setup JiffyDOS IERROR vector to point to
        sta     $0300   ; $F7BC, a RTS command
        ldy     #$FC    ; set up .Y pointer to 252
        jsr     READ_INTO_BUFFER   ; read 2 garbage bytes (program start address)
LF613:  ldy     #$00    ; set up .Y pointer to 0
LF615:  jsr     READ_INTO_BUFFER   ; read 254 bytes, store in input buffer
        bvs     LF63B   ; EOI, exit
        cpy     #$02    ; if .Y == 2
        beq     LF63B   ; exit
        cpy     #$06
        bcc     LF615

        ldx     $BB     ; read FNADR pointer, vector to input buffer        
        lda     $BC     ; FNADR HI
        stx     $5F     
        sta     $60     ; store in temp vector

        jsr     STORE_LIST_FROM_ADDR   ; store .A in ($5F)+1, $BC in ($5F)
LF62F:  jsr     $C6C3   ; use part of LIST routine to output text
        jsr     LF81D
LF635:  jsr     $C6D4
        lda     $91     ; check STKEY Flag
        lsr     a
        bcs     LF613   ; not pressed, continue
LF63B:  lda     #$E6    ; restore JiffyDOS IERROR vector to $F7E6 
        sta     $0300
        rts

;--------------------------------------
LOAD_END:
;This is the last part of the loader routine which 
;sets the (X/Y) register with the endaddress of the
;loaded program, clears carry and exit.
;
LF641:  clc
LF642:  ldx     $AE
        ldy     $AF
LF646:  rts

;--------------------------------------
PRINT_SEARCHING:
;If MSGFLG indicates program mode then the message is
;not printed, otherwise the message "SEARCHING" is
;printed from the KERNAL I/O message table. If the
;length of filename > 0 then the message "FOR" is
;printed, and the routine drops through to print the
;filename.
;
LF647:  lda     $9D             ;MSGFLG, direct or program mode?
        bpl     LF669           ;program mode, don't print, exit
        ldy     #$0C
        jsr     LF1E6           ;print "SEARCHING"
        lda     $B7             ;FNLEN, length of current filename
        beq     LF669           ;no name, exit
        ldy     #$17
        jsr     LF1E6           ;print "FOR"
 
;--------------------------------------
PRINT_FILENAME:
;Filename is pointed to by FNADR, and length in FNLEN.
;The KERNAL routine CHROUT is used to print filename.
;
LF659:  ldy     $B7             ;FNLEN, length of current filename
        beq     LF669           ;exit
        ldy     #$00
LF65F:  lda     ($BB),y         ;get character in filename
        jsr     LFFD2           ;output
        iny                     ;next character
        cpy     $B7             ;ready?
        bne     LF65F
LF669:  rts                     ;back

;--------------------------------------
PRINT_LOAD_VERIFYING:
;The load/verify flag is checked, and if the message
;to be output is flagged according to the result. This
;message is printed from the KERNAL I/O messages table.
;
LF66A:  ldy     #$40            ; "LOADING" offset from table of KERNAL I/O messages
        lda     $93
        beq     LF672
        ldy     #$50            ; "VERIFYING" offset from table of KERNAL I/O messages
LF672:  jmp     LF1E2

;--------------------------------------
SAVE:
;The KERNAL routine SAVE ($ffd8) jumps to this routine.
;On entry, (X/Y) must hold the end address+1 of the
;area of memory to be saved. (A) holds the pointer to
;the start address of the block, held in zeropage. The
;current device number is checked to ensure that it is
;niether keyboard (0) or screen (3). Both of these
;result in ?ILLIGAL DEVICE NUMBER.
;
LF675:  stx     $AE             ; EAL, end address of block +1
        sty     $AF
        tax                     ; move start pointer to .X
        lda     $00,x
        sta     $C1             ; STAL, start address of block
        lda     $01,x
        sta     $C2
        jmp     (L0332)         ; vector ISAVE, points to $F685
LF685:  lda     $BA             ; FA, current device number
        bne     LF68C           ; OK
LF689:  jmp     LF796           ; I/O error #9, illegal device #
LF68C:  cmp     #$03            ; screen?
        beq     LF689           ; yep, output error
        bcc     LF689           ; less than 3, tape, output error.  JiffyDOS has no tape, so just go to drive message

;--------------------------------------
SAVE_TO_SERIAL_BUS:
;A filename is assumed by the routine, or ?MISSING FILENAME
;error is called. The serial device is commanded to
;LISTEN, and the filename is sent along with the
;secondary address. The message 'SAVING' is printed,
;and a loop sends a byte to the serial bus and checks
;<STOP> key until the whole specifyed block of memory
;has been saved. Note that the first two bytes sent are
;the start address of the block. Finally the serial bus
;is UNLISTENed.
;
        lda     #$61
        sta     $B9             ;set SA, secondary address to #1
        ldy     $B7             ;FNLEN, length of current filename
        bne     LF69D           ;OK
LF69A:  jmp     LF793           ;I/O error #8, missing filename
LF69D:  jsr     LF495           ;send SA & filename
        jsr     LF728           ;print "SAVING" and filename
        lda     $BA             ;FA, current device number
        jsr     LEE17           ;send LISTEN
        lda     $B9             ;SA
        jsr     LEEC0           ;send LISTEN SA
        ldy     #$00
        jsr     LFBD2           ;reset pointer
        lda     $AC             ;SAL, holds start address
        jsr     LEEE4           ;send low byte of start address
        lda     $AD
        jsr     LEEE4           ;send high byte of start address
LF6BC:  jsr     LFD11           ;check read/write pointer
        bcs     LF6D7
        lda     ($AC),y         ;get character from memory
        jsr     LEEE4           ;send byte to serial device
        jsr     LFFE1           ;test <STOP> key
        bne     LF6D2           ;not pressed
LF6CB:  jsr     LF6DA           ;exit and unlisten
        lda     #$00            ;flag break
        sec
        rts
LF6D2:  jsr     LFD1B           ;bump r/w pointer
        bne     LF6BC           ;save next byte
LF6D7:  jsr     LEF04           ;send UNLISTEN
LF6DA:  bit     $B9             ;SA
        bmi     LF6EF
        lda     $BA             ;FA
        jsr     LEE17           ;send LISTEN
        lda     $B9
        and     #$EF
        ora     #$E0
        jsr     LEEC0           ;send UNLISTEN SA
        jsr     LEF04           ;send UNLISTEN
LF6EF:  clc
        rts

;--------------------------------------
JIFFYDOS_DEFAULT_FILENAME:
;The following routine is executed when a missing 
;filename is detected in the original loader routine. 
;If so, the filename is set to ':*', wildcard filename. 
;On exit, a jump is made to the original loader with
;new filename parameters set.
;
LF6F1:  lda     $C6             ;NDX, number of characters in keyboard buffer
        beq     LF69A           ;if zero, output missing filename error
        lda     #$02            ;store $02
        sta     $B9             ;in SA, default secondary address
        ldx     #$0C            ;set up filename pointer to $F70C
        ldy     #$F7            ;i.e. ':*'
        jsr     LFFBD           ;SETNAM
        jmp     LF563           ;back to loader routine
        
LF703:  ldx     #$33            ;offset
        ldy     #$04            ;length
        jmp     LF9B9           ;drive command

;--------------------------------------
JIFFY_FUNCTION_KEYS:
;
.byte   $40,$24,$3A,$2A,$0D,$00         ;F1='@$:*'
.byte   $2F,$00                         ;F2='/'
.byte   $5E,$00                         ;F5=arrow up
.byte   $25,$00                         ;F7='%'
.byte   $40,$44,$00                     ;F2='@d'
.byte   $40,$54,$00                     ;F4='@t'
.byte   $5F,$00                         ;F6=arrow left
.byte   $40,$20,$20,$22,$53,$3a,$00     ;F8='@  "S:'
;-------------------------------------
        .byte   $24

LF726:  clc
LF727:  rts

;--------------------------------------
PRINT_SAVING:
; MSGFLG is checked, and if direct mode is on, then the message
; 'SAVING' is flagged and printed from the KERNAL I/O message table.
;
LF728:  lda     $9D
        bpl     LF727
        ldy     #$48            ; SAVING message offset from table of KERNAL I/O messages
        jsr     LF1E6
        jmp     LF659

;--------------------------------------
UDTIM:
;bump clock
; The KERNAL routine UDTIM ($ffea) jumps to this routine. The
; three byte jiffy clock in RAM is incremented. If it has reached
; $4f1a01, then it is reset to zero. this number represents 5184001
; jiffies (each jiffy is 1/60 sec) or 24 hours. finally, the next
; routine is used to log the CIA key reading.
;
LF734:  ldx     #$00
        inc     $A2             ;low byte of jiffy clock
        bne     LF740
        inc     $A1             ;mid byte of jiffy clock
        bne     LF740
        inc     $A0             ;high byte of jiffy clock
LF740:  sec
        lda     $A2             ;subtract $4f1a01
        sbc     #$01
        lda     $A1
        sbc     #$1A
        lda     $A0
        sbc     #$4F
        bcc     LF755           ;and test carry if 24 hours
        stx     $A0             ;yup, reset jiffy clock
        stx     $A1
        stx     $A2

;--------------------------------------
LOG_VIA_READING:
; This routine tests the keyboard for either <STOP> or <RVS> pressed.
; If so, the keypress is stored in STKEY.
;
LF755:  lda     $912F           ; read value
        cmp     $912F           
        bne     LF755           ; wait for value to settle
        sta     $91
        rts

;--------------------------------------
RDTIM:
;get time
; The KERNAL routine RDTIM ($ffde) jumps to this routine. The three
; byte jiffy clock is read into (A/X/Y) in the format high/mid/low.
; The routine exits, setting the time to its existing value in the
; next routine. The clock resolution is 1/60 second. SEI is included
; since part of the IRQ routine is to update the clock.
;
LF760:  sei
        lda     $A2
        ldx     $A1
        ldy     $A0

;--------------------------------------
SETTIM:
;set time
; The KERNAL routine SETTIM ($ffdb) jumps to this routine. On entry,
; (A/X/Y) must hold the value to be stored in the clock. The forman
; is high/mid/low, and clock resolution is 1/60 second. SEI is included
; since part of the IRQ routine is to update the clock.
;
LF767:  sei
        sta     $A2
        stx     $A1
        sty     $A0
        cli
        rts

;--------------------------------------
STOP:
;check STOP key
; The KERNAL routine STOP ($ffe1) is vectored here. If STKEY = #FE,
; then <STOP> was pressed and logged while the jiffy clock was
; being updated, so all I/O channels are closed and the keyboard buffer
; reset.
;
LF770:  lda     $91             ; STKEY
        cmp     #$FE            ; <STOP>?
        bne     LF77D           ; nope
        php
        jsr     LFFCC           ; CLRCHN, close all I/O channels
        sta     $C6             ; NDX, number of characters in keyboard buffer
        plp
LF77D:  rts

;--------------------------------------
OUTPUT_KERNAL_MESSAGES:
; The error message to be output is flagged into (A) depending on the entry
; point. I/O channels are closed, and then if KERNAL messages are enabled,
; "I/O ERROR #" is printed along with the error number.
;
LF77E:  lda     #$01            ;error #1, too many files
        .byte   $2C
LF781:  lda     #$02            ;error #2, file open
        .byte   $2C
LF784:  lda     #$03            ;error #3, file not open
        .byte   $2C
LF787:  lda     #$04            ;error #4, file not found
        .byte   $2C
LF78A:  lda     #$05            ;error #5, device not found
        .byte   $2C
LF78D:  lda     #$06            ;error #6, not input file
        .byte   $2C
LF790:  lda     #$07            ;error #7, not output file
        .byte   $2C
LF793:  lda     #$08            ;error #8, missing filename
        .byte   $2C
LF796:  lda     #$09            ;error #9, illegal device number
        pha
        jsr     LFFCC           ;CLRCHN close all I/O channels
        ldy     #$00
        bit     $9D             ;test MSGFLAG, KERNAL messages enabled
        bvc     LF7AC           ;no, we're done
        jsr     LF1E6           ;print "I/O ERROR #"
        pla                     ;get the error #
        pha                     ;save
        ora     #$30            ;convert to ASCII 
        jsr     LFFD2           ;print the #
LF7AC:  pla                     ;restore error code
        sec 
        rts                     ;done

;--------------------------------------
TEST_JIFFY_COMMAND:
; This routine test the character in the current key in the buffer
; if it is a JiffyDOS command character. Output from this routine is
; (Y) which contains the value of the selected command. (Y)=$ff if no
; command was found.
;
LF7AF:  ldy     #$0C            ; number of characters to test
        jsr     L0079           ; CHARGOT, read current character in buffer again
LF7B4:  cmp     LF861,y         ; equal to byte in JiffyDOS command table
        beq     @0              ; yep, return
        dey                     ; test next
        bpl     LF7B4           ; till .Y=$FF
@0:     rts                     ; done

;--------------------------------------
JIFFY_SLPARA:
; This routine is executed from the original SLPARA. It executes SETLFS
; to set logical file parameters, as normal. But it also continues through
; the next routine to find a present device number.
;
LF7BD:  jsr     LFFBA           ; SETLFS

;--------------------------------------
JIFFY_TEST_SERIAL_DEVICE:
; check if a device $08-$1f is present.  Two checks are
; performed. After the counter is reset the second
; time, error #5 is returned
;
LF7C0:  clc                     ;clear carry
        php                     ;store carry
        ldx     $BE             ;internal counter for device number
        cpx     #$08            ;device $8
        bcc     @1              ;less than $8 (not serial device)
@0:     cpx     #$1F            ;serial device must be less than $1f (31)
        bcc     @2              ;less than $1f
@1:     plp                     ;if carry set, this is second time
        bcs     LF7E4           ;do error 
        sec                     ;set carry to indicate first reset
        php                     ;store carry
        ldx     #$08            ;start at $08 again
@2:     stx     $BE             ;store
        jsr     LE4CF           ;test devicenumber (.X)
        bcc     LF7DD           ;OK, device .X is next present device
        inx                     ;next devicenumber
        bne     @0              ;test next

LF7DD:  pla                     ;clean up stack
LF7DE:  rts                     ;exit

LF7DF:  jsr     LE4D1           ;test devicenumber in FA
        bcc     LF7DE           ;OK
LF7E4:  ldx     #$05            ;ERROR, device not present


;--------------------------------------
JIFFY_IERROR:
; ERROR vector points here.  If it is a syntax error ($0b)
; JiffyDOS checks if it caused it, otherwise it is 
; handled as it normally would be.
;  .X= error # 
;
LF7E6:  cpx     #$0B            ; SYNTAX ERROR
        beq     LF7ED           ; yes, jump to command test
LF7EA:  jmp     LE47E           ; nope, normal error handler

;--------------------------------------
JIFFY_COMMAND_TEST:
; test if a JiffyDOS command has been entered
;
LF7ED:  jsr     LF7AF           ; test JiffyDOS command
        bne     LF7EA           ; no JiffyDOS command
        sty     $27             ; temp store
        tax
        bmi     LF7F9
        pla
        pla
LF7F9:  jsr     LF7C0           ; test serial device, if any present
        jsr     LF8BC           ; command after '@?'?  Setname and open specified file
        lda     $27             ; retrieve temp, command number
        ldy     #$00
        asl     a               ; *2
        tax                     ; to .X
        lda     LF879,x         ; get low command vector
        sta     $55             ; store
        lda     LF879+1,x       ; get hi command vector
        sta     $56             ; store
LF80F:  jsr     $54             ; execute JiffyDOS command
        jsr     $C8F8           ; ignore next statement
        jsr     LF391           ; close all channels and file 15 if open
        lda     $9F             ; JiffyDOS default file #
        jsr     LFFC3           ; CLOSE

LF81D:  jsr     LFFCC           ; CLRCHN- close all I/O channels
        ldx     $13             ; CHANNL
        beq     LF7DE           ; screen/keyboard are current devices, exit
        .byte   $2C             ; else mask next LDX and perform CHKOUT
LF825:  ldx     #$6F            ; command channel
        jmp     LFFC9           ; CHKOUT, open channel for output

;--------------------------------------
JIFFY_ML_LOAD:
; entry point for load machine language (% and £)
;
        tya
        iny
        .byte   $2C             ; skip next 2 commands

;--------------------------------------
JIFFY_VERIFY:
; entry point for file verification command
;
        iny                     ; contrary to the C64 docs, having tya, then iny will VERIFY when it should
                                ; load, and load when it should verify. this seems correct...

;--------------------------------------
JIFFY_BASIC_LOAD:
; This is the entrypoint for / and 'arrow up' which loads a basic program.
; The LOAD/VERIFY is performed. Depending on what command is executed, various
; end routines are performed.
;
        tya 
        sty     $B9             ;SA
        ldx     $2B             ; TXTTAB- start of BASIC
        ldy     $2C
        jsr     LFFD5           ; LOAD
        bcc     LF843           ; load OK
        jmp     LE0F6           ; handle I/O error
LF83D:  jmp     LE195           ; load OK?
LF840:  jmp     LE17B           ; verify OK?
LF843:  lda     $27             ; test command #
        cmp     #$0B            ; verify command (´)
        beq     LF840           ; output verify OK
        bcs     LF80F           ; command # > $0B
        cmp     #$08            ; load ml (%)?
        bne     LF850           ; if not, continue
        rts
LF850:  bcc     LF83D           ; if command # < 8, test if OK and exit
        stx     $2D             ; VARTAB, set start of BASIC
        sty     $2E
        pla                     ; remove return address
        pla
        jsr     $CAD7           ; output CR/LF
        jsr     LC533           ; rechain BASIC lines
        jmp     $C871           ; perform RUN


;JIFFY COMMAND TABLE-------------------
; @, <-, *, *., ", "., /, /.
LF861:  .byte   $40, $5F, $2A, $AC, $22, $12, $2f, $AD
; %, 'arrow up', 'arrow up'., ´, £
LF869:  .byte   $25, $5E, $AE, $27, $5C
; the following chars must be entered after '@'
; D, L, T, #, B, F, O, P, Q, X, G
LF86E:  .byte   $44, $4C, $54, $23, $42, $46, $4F, $50, $51, $58, $47
;-------------------JIFFY COMMAND TABLE

;JIFFY COMMAND VECTORS----------------
; in the order of the above command characters
LF879: 
.word   $F5CA   ; @
.word   $E156   ; <-
.word   $FA81   ; *
.word   $FA81   ; XX
.word   $F7AE   ; "
.word   $F7AE   ; .
.word   $F82E   ; /
.word   $F82E   ; XX
.word   $F82A   ; %
.word   JIFFY_BASIC_LOAD   ; arrow up 
.word   $F82E   ; XX
.word   $F82D   ; ´
.word   $F82A   ; £

; these commands come after the @ character
.word   $F601   ; D
.word   $F958   ; L
.word   $F5D7   ; T
.word   $F29C   ; # 
.word   $F9B3   ; B 
.word   $E48b ;7   ; F - disable function keys
.word   $F8A9   ; O
.word   $FADF   ; P
.word   DISABLE_JIFFYDOS_COMMANDS ;$FCEF   ; Q
.word   JIFFY_X_COMMAND ;$FCD3   ; X
.word   $F9AB   ; G
;----------------JIFFY COMMAND VECTORS
;---------------------------------------
JIFFY_OLD:
; performs an OLD after NEW or a reset.  rechains pointers, etc.
; of this routine. investigate.
;
        iny
        tya
        sta     ($2B),y
        jsr     LC533           ; LINKPRG, rechain BASIC lines
        txa
        adc     #$02
        tax
        lda     $23
        adc     #$00
        tay                     ; .X and .Y contain start of variables
        jmp     LE1A7           ; set start of variables and restart BASIC


;--------------------------------------
JIFFY_COMMAND_2:
;This routine is called from the JiffyDOS COMMAND routine and 
;make a test for additional command characters after the 
;'@' character. Only the command number $0d-$17 is tested. 
;If text after '@' is not a JiffyDOS command 
;(ie. a normal DOS command', or JiffyDOS command number less than
;$10, a filename is expected. Tests are made for colon and 
;quotes, the filname is evaluated, and parts of the OPEN/CLOSE 
;routine is used to SETNAM. A test is made for additional device
;number after a comma. A free line on the screen is found, and 
;some string-house keeping is done. 
;Finally, the routine continues through to the next routine to 
;open the command channel.
;
LF8BC:  tya
        bne     LF8D7
LF8BF:  sta     $B7
        jsr     $0073           ; CHRGET, get a char from buffer
        beq     LF90B           ; if terminator found, exit
        ldy     #$17            ; set pointer to start of command
LF8C8:  jsr     LF7B4           ; test if character is JiffyDOS command
        bne     LF8DC           ; nope, no command
        cpy     #$0D            ; test $0D to $17 only
        bcc     LF8DC           ; if less than that range, exit
        sty     $27             ; temp store
        cpy     #$10            ; read command value
        bcs     LF90B           ; if >$10, filename not expected
LF8D7:  lda     #$01            
        jsr     $C8FC           ; add TXTPTR by one
LF8DC:  ldy     #$FF            ; init pointer
LF8DE:  iny
        lda     ($7A),y         ; read character from keyboard buffer
        beq     LF8ED           ; terminator found
        cmp     #$22            ; quotes?
        beq     LF8F6
        cmp     #$3A            ; colon?
        bne     LF8DE
LF8ED:  bit     $9D             ; test MSGFLAG if direct mode
        bpl     LF8F9           
        clc
        jsr     $CEBD           ; 
        jmp     LF8FC
LF8F6:  jsr     $C8FB           ; add value in .Y to TXTPTR
LF8F9:  jsr     LCD9E           ; evaluate expression in text
LF8FC:  jsr     LE257           ; use part of OPEN/CLOSE to SETNAM
        jsr     L0079           ; CHRGET
        cmp     #$2C            ; test for comma
        bne     LF90B           ; nope
        jsr     $D79B           ; use GTBYTC to read character after comma
LF909:  stx     $BA             ; store it in FA (device #)
LF90B:  ldy     #$00
        bit     $9D             ; test MSGFLAG if direct mode
        bpl     LF91E
LF911:  lda     ($D1),y         ; current screen line address, read from screen
        cmp     #$20            ; SPACE
        beq     LF91E           ; yep
        lda     #$0D            ; carriage return
        jsr     LE742           ; output to screen
        bne     LF911
LF91E:  jsr     LF7DF           ; test if device FA is present
        lda     #$FF
        jsr     $D475           ; 
        lda     $B7             ; FNLEN
        ldx     $BB             ; FNAD/DR, pointer to current filename
        ldy     $BC
        jsr     $D4C7           ;
        jsr     $D6A3           ; do string housekeeping 
        stx     $BB             ; store in FNADR, pointer to current filename
        sty     $BC

;--------------------------------------
JIFFY_OPEN_COMMAND_CHANNEL:
; open the command channel. A test is done to see if
; it is already open.  If so, the command channel is 
; closed before opened.
;
LF936:  jsr     LF394           ; close command channel if open
        lda     $B7             ; read FNLEN, length of current file
        ldx     #$00            ; store 0
        stx     $B7             ; in FNLEN
LF93F:  ldx     #$6F
        bne     LF947           ; jmp LF947
LF943:  ldx     #$6E
LF945:  lda     $B7
LF947:  stx     $B9             ; store in SA, current second address
        stx     $9F             ; store in JiffyDOS default file number
LF94B:  pha
        stx     $B8             ; store in LA, current logical file number
        jsr     LFFCC           ; CLRCHN, close all I/O
        jsr     LFFC0           ; OPEN
        pla
        sta     $B7             ; restore FNLEN, length of file name
LF957:  rts

;--------------------------------------
JIFFY_LOCK_UNLOCK_FILE:
;This routine locks/unlocks specifyed file. The file 
;is opened, and tests are made to check that everything
;is OK. If so a bunch of code are transfered to the 
;drive, and executed. The code to be transfered is 
;found at $f398, after the memory-write command.
;
        jsr     LF293           ; open file and test that all is OK
        bne     LF957           ; not OK
        ldx     #$00            ; set up drive commadn @ $f398+0 (address is probably wrong)
        ldy     #$22            ; length of string
        jsr     LF968           ; execute
        ldy     #$05            ; set up drive command @ $f398+$22, length 5 bytes
        ldx     #$22
LF968:  jsr     JIFFY_SEND_DRIVE_COMMAND           ; execute direct drive command
        jmp     LFFCC           ; CLRCHN, close all I/O channels

;--------------------------------------
JIFFY_PATCH:
;serial send
; This is a patch to the original Commodore KERNAL to send data on
; the serial bus. 
;
;from SJLoad. see label 'lF96E'
;TODO: possibly timing sensitive
;

LF96E:  sta     $912C           ; store in serial bus I/O port
        bit     $911F           ; test ATN, attention
        bpl     LF997           ; ATN=1, done
        cpx     #$02            ; 
        bne     LF997           ; done
        lda     #$02            ; test bit 1 (DATA) of serial bus

        ldx     #$20            ; times to loop before timeout

LF97E:  bit     $911F           ; test DATA
        beq     LF988           ; data is low, continue
        dex                     ; timeout counter
        bne     LF97E           ; loop until timeout or DATA low
        beq     LF995           ; timeout
LF988:  bit     $911F           ; test bit DATA again
        beq     LF988           ; wait for value to settle
        lda     $95             ; BSOUR, buffered character for bus
        ror     a
        ror     a
        ora     #$40
        sta     $A3
LF995:  ldx     #$02
LF997:  rts

;--------------------------------------
JIFFY_DISPLAY_ASCII_FILE:
;The following routine is called by the LIST ASCII from
;disk. It clears the command channel and calls a routine
;that reads maximum 254 character from  the file. This 
;is repeated until the entire file is displayed.
;
LF998:  ldy     #$00
        jsr     JIFFY_SET_CHKIN           ; CLRCHN and perform CHKIN on .A 
LF99D:  jsr     READ_INTO_BUFFER+3           ; read text into buffer
        bvs     LF9A4           ; finish
        bcc     LF99D           ; next
LF9A4:  sty     $B7             ; FNLEN, length of current file name
        lda     $90             ; status
        and     #$82
        rts

;--------------------------------------
JIFFY_INTERLEAVE:
;The following routine sets the interleave gapsize by 
;writing the selected value to drive memory
;at position $0069.
;
        jsr     $D79B           ; GETBYTC, get byte from keyboard 
        txa                     ; transfer to gapsize in .A 
        ldx     #$2D            ; setup drive command at $f398+$2D, M-W 69 00 01
        bne     LF9B7           ; jmp
 
;--------------------------------------
JIFFY_BUMP_DISABLE:
;The following routine disables the 1541 head rattle. 
;This is done by writing the value $85 to drivememory 
;at position $006a.
;
        lda     #$85
        ldx     #$27            ; setup drive command at $f398+$27, M-W 69 00 01
LF9B7:  ldy     #$06
LF9B9:  pha
        jsr     JIFFY_SEND_DRIVE_COMMAND           ; execute drive command
        pla
        jmp     LFFD2           ; write byte in .A to drive and return

;--------------------------------------
JIFFY_MARK_FILE_FOR_COPY:
;This routine toggles the copy flag for one file, of 
;for all selected files depending on the entry point. 
;If entry at $f93a, the copy flags for all files will 
;be toggled, and if entry at $f93d only one will be 
;affected.
LF9C1:  ldx     #$00            ; toggle flag for all files
        .byte   $2C             ; mask LDX
LF9C4:  ldx     #$06            ; toggle flag for current file
        jsr     LC68E           ; STXPT, reset TXTPTR to start of BASIC
        ldy     #$05
        lda     ($7A),y         ; test 5th character
        cmp     #$12            ; <RVS ON>?
        bne     LFA37           ; if not, directory not loaded, exit
        pla
        txa                     ; store .X, the toggle flag, on the stack
        pha
        ldy     #$23            ; set offset to $23
LF94F:  ldx     #$22            ; search for a quote mark (")
        jsr     $C917           ; use part of DATAN to search 
        dey
        jsr     $C8FB           ; add offset in .Y to TXTPTR
        pla                     ; read flag, set at start
        pha
        beq     LF9F3           ; toggle all flags are set
        sta     $D3
        ldy     #$01
LF9E7:  iny
        jsr     LF221           ; use part of 'input from screen'
        cmp     ($7A),y
        bne     LF9FE
        sbc     #$22
        bne     LF9E7
LF9F3:  tay
        lda     ($7A),y         ; get character
        eor     #$0A            ; toggle between $20 (space) and $2A (*)
        sta     ($7A),y         ; store it
        ldy     #$04
        sta     ($D1),y
LF9FE:  jsr     $C8F8           ; DATA, perform data, skip line like REM 
        ldy     #$05
        sec
        lda     ($7A),y
        sbc     #$42
        bne     LF94F           ; next line
        ldy     #$02
        sta     ($7A),y
        pla                     ; set flag, read from stack
        beq     LFA14           ; if 0, all files were marked/unmarked, do LIST
        lda     #$8D
        rts
LFA14:  jmp     $C6A4           ; LIST 

;--------------------------------------
JIFFY_TOGGLE_DRIVE_COMMANDS:
;This routine is continued from JiffyDOS get character.
;It tests if the keys <CTRL D> are pressed. If so, it 
;increments the internal device counter and tests if it
;is present. The routine will return the new device number 
;in (X), which will be printed, and the routine exits. 
;If <CTRL D> were not pressed, it continues to test 
;<CTRL A> and <CTRL W>. If not, the routine continues 
;to the funktion key test.
;
LFA17:  bit     $9D             ; test MSGFLAG
        bpl     LFA37           ; exit
        tsx
        ldy     $0107,x
        cpy     #$E1
        bne     LFA37           ; exit
        cmp     #$04            ; test code $04, <CTRL D>, toggle drive
        bne     LFA39           ; if not, go to next test
        inc     $BE             ; increment JiffyDOS drive #
        jsr     LF7C0           ; test device # in $BE, output in .X
        lda     #$00
        jsr     LDDCD           ; print numberic value in (A/X)
        jsr     $CAD7           ; output CR/LF
        jsr     LF81D           ;
LFA37:  pla                     ; retrieve .A
        rts                     ; and exit
LFA39:  cmp     #$01            ; test code #$01 <CTRL-A>, toggle all files for copy
        beq     LF9C1           ; toggle all files for copy
        cmp     #$17            ; test code #$17 <CTRL-W>, toggle one file for copy
        beq     LF9C4           ; toggle one file for copy

;--------------------------------------
TEST_JIFFY_FUNCTION_KEYS:
;This routine test if a shifted, or unshifted function 
;key were pressed. If so, it sends a string containing 
;the command to the keyboard buffer. The vector in $b0 
;points to the command sting table. The strings are in 
;numerical order, and seperated by a null byte. To find
;the right string, the routine counts through them all
;till it reaches the X:th string.
;
        ldy     $9B             ; internal JiffyDOS flag, must be 0
        bne     LFA37           ; exit
        cmp     #$8D            ; test keys F1-F8
        bcs     LFA37           ; larger than F8, exit
        cmp     #$85
        bcc     LFA37           ; smaller than F1, exit
        pla
        sbc     #$85            ; get # of F key
        tax
        beq     LFA5C           ; if F1, skip to action
LFA53:  iny                     ; increment pointer
        lda     ($B0),y         ; read and skip string in function key table
        bne     LFA53           ; repeat for all bytes of string
        dex
        bne     LFA53           ; repeat until .X strings are skipped
LFA5B:  iny
LFA5C:  lda     ($B0),y         ; read command from corresponding string
        beq     LFA69           ; if final character, exit
        cmp     #$0D            ; <RETURN>
        beq     LFA6B
        jsr     LE742           ; output to screen
        bne     LFA5B           ; next char
LFA69:  sta     $D4
LFA6B:  rts

;--------------------------------------
JIFFY_GET_CHARACTER:
;This routine is a new JiffyDOS routine to handle 
;extended functions. It is called from $e5ec, and 
;starts with the original jump. The routine test the 
;F-keys, and if a valid combination of <CTRL xx> is 
;pressed. If quote mode or insert mode is activated, 
;then this routine will exit.
;
LFA6C:  jsr     LE5CF           ; get character from keyboard buffer
        pha                     ; save it
        ldx     $D4             ; test QTSV, if quote mode is activated
        bne     LFA7F           ; if not zero, quote mode is on, exit
        ldx     $D8             ; test INSRT, if insert mode is activated
        bne     LFA7F           ; if not zero, insert mode is on, exit
        cmp     #$10            ; test code $10 <CTRL-P>, screen dump
        bne     LFA17           ; if not pressed, jump and test other keys

;--------------------------------------
;JIFFY_SCREEN_DUMP:
;THIS FUNCTION HAS BEEN REMOVED!!!!!!!
;This routine performs a screen dump when the keys 
;<CTRL P> are pressed. It reads $d018 to determine if 
;upper or lower character set is used, and sends the 
;proper SA after LISTEN. The routine stores the cursor 
;positions on the stack, and retrieves them, and 
;replaces the cursor on exit. To print a character to 
;the serial bus, the routine uses part of the KERNAL 
;CIOUT routine.
;

.res 3                      ;FREE

LFA7F:  pla
LFA80:  rts

;--------------------------------------
JIFFY_COPY_COMMAND:
; this routine is used to copy files
;
        sty     $26             ; save .Y
        jsr     LF293           ; open command channel and read status
        bne     LFA80           ; not okay, exit
        jsr     L0079           ; CHRGET
        cmp     #$52            ; 'R'
        bne     LFAA2
LFA8F:  dec     $26
        lda     $26
        jsr     LF703
        jsr     LE48F           ; input byte from command channel and compare to 5
        beq     LFA8F           ; yep, equal
        lda     #$00
        jsr     LF703
        lda     #$4C            ; 'L'
LFAA2:  pha
        ldx     $BF
        cpx     $BA             ; compare to FA, current device #
        beq     LFA7F           ; exit
        jsr     LF909
        ldx     #$37            ; setup drive command at $f398+$37
        ldy     #$02            ; 2 bytes long
        jsr     JIFFY_SEND_DRIVE_COMMAND           ; send it
        jsr     LF659           ; print filename
        lda     #$2C            ; ,
        sta     ($BB),y         ; store in filename buffer
        iny
        pla                     ; retrieve command
        sta     ($BB),y         ; store in filename  buffer
        iny
        lda     #$2C
        sta     ($BB),y         ; ,
        iny
        lda     $26
        pha
        bne     LFACB
        lda     #$57
LFACB:  sta     ($BB),y         ; W
        iny
        sty     $B7             ; update FNLEN, length of current filename
        ldy     #$0C
LFAD2:  jsr     LFAFA           ; set SA to .Y and more
        jsr     LF7C0           ; test for present device
        jsr     JIFFY_OPEN_COMMAND_CHANNEL  ; open command channel
        pla
        jsr     LF5D8           ; use list ASCII from disk to perform copy

;--------------------------------------
TOGGLE_PRINTER:
;The following routine toggles the printer output function.
;It reads the CHANNL to determine if printmode is to be 
;turned on or off.
;
        lda     $13             ; CHANNL, contains 00 if current output is screen
        beq     LFAEF           ; toggle printer on
        cmp     #$7F            ; CHANNL contains 7F if current output is printer
        bne     LFA80           ; jump to RTS
        jsr     $CBB7           ; CLRCHN, clear all channels, and set CHANNL=0 
        lda     #$7F
        jmp     $FFC3           ; close file $7F

LFAEF:  ldx     #$04            ; device #4 = printer
        jsr     $0073           ; CHRGET
        jsr     LE226           ; use part of OPEN routine to open device #4
        jsr     LF7DF           ; test device # in FA
LFAFA:  sty     $B9             ; SA, current secondary address
        ldx     #$7F
        stx     $13             ; CHANNL, current I/O channel
        lda     $B7             ; FNLEN, length of current filename
        jmp     LF94B           ; perform CLRCHN and OPEN file, .X

;---------------------------------------
PATCH_TO_ORIGINAL_LOAD:
; This routine is a patch to the original load routine and tests is the
; current device is a JiffyDOS device. If not, the routine jumps back to
; the original loader at $f4f3. The routine disables the sprites and
; calculates the timing parameters to $b1. Some handshaking is done
;
        jsr     LE4C1           ; print "LOADING/VERIFYING"
        tsx                     ; test if some return pointer on the stack is $F8
        lda     $0102,x
        cmp     #$F8            ; C64 JiffyDOS has $F7
        bne     LFB18			; if not, don't store the $ae/$af parameters
        lda     $AE
        sta     $55
        lda     $AF
        sta     $56
LFB18:  bit     $A3             ; LDFLG, are we talking to a JiffyDOS device?
        bmi     LFB1F           ; yes, continue to fastload routine
        jmp     LF58A           ; nope, return to the original load routine
		
;--------------------------------------
;JIFFY FASTLOAD START from SJLoad label '.FB1F'
JIFFY_XFER_ROUTINE_FOR_LOAD:
;LOAD/VERIFY a file.  If $93 = 0, the file is loaded, otherwise it is verified.
;
;
LFB1F:  jsr     JIF_UNTALK
        lda     #$61
        jsr     DISK_TALK
        sei
        lda     $B2        
        pha
        ldy     #$00            ; offset to address to store/verify byte from ($AE) (always 0)
LFB25:  jsr     LF755           ; read $912F and wait for value to settle
        cmp     #$FE
        beq     _STOP           ; 
        
;timing:
;NTSC: 35*(1/1.022727) = 34.222231348 microseconds
;PAL: 35*(1/1.1108405) = 31.507673694 microseconds
        lda     $912C           ;4 read peripheral control register
        and     #$DD            ;2 turn off bits 5 and 1 (bring serial bus data line high)
        tax                     ;2 save value to bring serial bus high in .X
        ora     #$20            ;2 value to bring serial bus data line high
        sta     $B2             ;3 save the value to bring serial bus low in $BA
        stx     $912C           ;4 bring serial bus data line high
        lda     #$80            ;2
        sta     $9C             ;3
LFB3D:  lda     $911F           ;4 read serial bus
        lsr                     ;2 shift out CLK
        bcc     LFB3D           ;2 wait for a 1
        and     #$01            ;2 is DATA 1?
        beq     LFB67           ;3 no, skip

        ;timeout/done
        ldx     #$6D            ; number of times to loop before timeout
LFB4A:  bit     $911F           ; was DATA high, and is DATA currently high?
        beq     @2              ; no, skip
        dex                     ; yes, decrement .X
        bne     LFB4A           ;3/2 loop until timeout

        lda     #$42            ; timeout message
        .byte   $2C             ; skip next instruction
@2:     lda     #$40            ; success
        jsr     LFE6A           ; SETMSG, set the result of the communication
        clc                     ; success
        .byte   $24             ; skip 'sec'
_STOP:  sec                     ; failure
        pla                     ; restore $B2
        sta     $B2             ; set $B2 to its old value
        bcs     @0              ; if communication failed, BREAK
        jmp     $F5BF           ; success: UNLISTEN, CLOSE
@0:     jmp     $F6CB           ; failure: UNLISTEN, CLOSE, BREAK


LFB67:  lda     #$02            ; bit 1 (DATA)
;--------------------------------------
;timing (resync point):
;total timing
;NTSC: (6+25+13+10+25)*(1/1.022727) = 75.288908966 microseconds
;PAL: (6+25+15+12+25)*(1/1.1108405) = 74.718197617 microseconds
;NTSC: 6*(1/1.022727)
;PAL: 6*(1/1.1108405)
@0:     bit     $911F           ;4 check DATA
        beq     @0              ;2 loop until DATA is low

STOREBYTE_LOOP:
;timing:
;NTSC: 23*(1/1.022727) = 22.4888948859 microseconds
;PAL: 25*(1/1.1108405) = 22.50548121 microseconds
        pha                     ;3 timing
        pla                     ;4 timing 
.IFNDEF NTSC
        nop                     ;2
.ENDIF
        lda     $B2             ;3 value to store in $912C, handshaking control register
        sta     $912C           ;4 set peripheral control register
        lda     #$01            ;2 bit 0 (CLK)
        bit     $911F           ;4 check CLK
        beq     LFB25           ;2/3 if zero, start over
        
;timing (1 + 14):
;NTSC: 13*(1/1.022727) = 14.666670578 microseconds
;PAL: 15*(1/1.1108405) = 13.503288726 microseconds
        stx     $912C           ;4 handshaking - bring serial bus DATA line high
        lda     $911F           ;4 read serial bus
        ror     a               ;2 shift bit 0 into carry and bit 1 into bit 0
        ror     a               ;2 shift bit 0 into bit 7 and bit 1 into carry
        
.IFNDEF NTSC
        nop                     ;2
.ENDIF

;timing (12):
;NTSC: 10*(1/1.022727) = 11.733336462 microseconds
;PAL 12*(1/1.1108405)  = 10.802630981 microseconds
        and     #$80            ;2 clear the bits we don't care about
        ora     $911F           ;4 read serial bus to get next 2 bits
        rol     a               ;2 shift back first two bits into bits 0 and 1, and
        rol     a               ;2 the 2 new bits into bits 2 and 3
.IFNDEF NTSC
        nop                     ;2
.ENDIF

;timing: (25)
;NTSC: 25*(1/1.022727) = 24.444450963 microseconds
;PAL: 25*(1/1.1108405) = 22.505481201 microseconds
        sta     $B3             ;3 save first nybble
        lda     $911F           ;4 read serial bus again
        ror     a               ;2 CLK
        ror     a               ;2 DATA
        and     $9C             ;3 (and $80) - isolate CLK (bit 7) and DATA (carry)
        ora     $911F           ;4 read serial bus to get the final 2 bits
        rol     a               ;2 CLK
        rol     a               ;2 DATA
        sta     $C0             ;3 save the other nybble
;-end of timing sensitive portion

        jsr     JIFFY_BYTE_IN   ; reconstruct the byte from the bits that were read

STOREBYTE:
        cpy     $93             ; verify?
        bne     LFBB0           ; yes
        sta     ($AE),y         ; no, load, store the byte

LFBA7:  inc     $AE             ; increment low byte of address
        bne     STOREBYTE_LOOP
        inc     $AF             ; increment hi byte of address
        jmp     STOREBYTE_LOOP  ; continue

LFBB0:  cmp     ($AE),y         ; verify byte
        beq     LFBA7           ; byte is the same, continue
        lda     #$10            ; set STATUS
        sta     $90             ; STATUS
        bne     LFBA7           ; continue verifying
        

;---------JIFFY DATA
LFBB9:  .byte $00,$00,$20,$20,$00,$00,$20,$20,$02,$02,$22,$22,$02,$02,$22,$22
;--------JIFFY DATA

;.res    8       ; FREE

.IFDEF NTSC
        .res    3
.ENDIF

LFBD2:  lda     $C2
        sta     $AD
        lda     $C1
        sta     $AC
        rts

;----------------JIFFY BYTE IN
JIF_IECIN:
LFBDB:  sei
        bit     $A3         ;is the device JiffyDOS equipped? 
        bvs     JIFFY_IN    ;yes
        lda     #$00        ;no, 
        jmp     LEF1C       ;drive not jiffydos equipped, use original routine

;--------------------------------------
JIFFY_IN:
;Read a byte in using the SJLoad routine.
;total time for 1 byte received:
;NTSC: (18+11+14+8+8+23+19) * (1/1.022727) = 98.75558189 microseconds
;PAL:  (18+14+14+9+10+25+19) * (1/1.108405) = 98.339505867 microseconds
;if the PAL version were to be used on NTSC, it would take 106.577806199 microseconds.
;

;timing:
;NTSC: 18*(1/1.022727) microseconds = 17.6000004693 microseconds 
;PAL: 18*(1/1.108405) microseconds = 16.239551428 microseconds
LFBE5:  lda     $911F       ;4 serial bus
        and     #$03        ;2 mask clock-in and data-in bits
        beq     LFBE5       ;2 wait for one of them to be high 
        lda     #$80        ;2 initialize the byte-received flag
        sta     $9C         ;3 to $80
        txa                 ;2 save .X
        pha                 ;3 i'm serious

;timing:
;NTSC: 11*(1/1.022727) microseconds = 10.755558424 microseconds
;PAL: 14*(1/1.108405) microseconds = 12.630762221 microseconds
        pha                 ;3
        pla                 ;4
.IFDEF NTSC
        nop                 ;2
        nop                 ;2
.ENDIF
.IFNDEF NTSC
        pha                 ;3 
        pla                 ;4 
.ENDIF

;timing:
;NTSC: 14*(1/1.022727) microseconds = 13.688892539 microseconds
;PAL: 14*(1/1.022727) microseconds = 12.630762221 microseconds
        lda     $912C       ;4 handshaking (bring serial bus data line high)
        and     #$DD        ;2 handshaking cont'd
        sta     $912C       ;4 handshaking cont'd
        ora     #$20        ;2 set bit 5=1
        tax                 ;2 store

;timing:
;NTSC: 8*(1/1.022727) microseconds = 7.822224308 microseconds
;PAL: 9*(1/1.108405) microseconds = 8.302200083 microseconds
;
        bit     $9C         ;timing
.IFNDEF NTSC
        bit     $9C         ;timing
.ENDIF
        bit     $9C         ;timing
.IFDEF NTSC
        nop
.ENDIF

;timing:
;NTSC: 8*(1/1.022727) microseconds = 7.822224308 microseconds
;PAL: 10*(1/1.108405) microseconds = 9.02197305 microseconds
;
        lda     $911F       ;4 read bits 0 and 1
        ror     a           ;2 get bit 0 (clock)
        ror     a           ;2 get bit 1 (data)
.IFNDEF NTSC
        nop                 ;2 
.ENDIF

;timing:
;NTSC: 23*(1/1.022727) microseconds = 22.488894886 microseconds
;PAL: 25*(1/1.108405) microseconds = 22.554932538 microseconds
        and     #$80        ;2 clear everything that isn't the bits we want
        ora     $911F       ;4 read bits 2 & 3
        rol     a           ;2 get bit 2 (clock)
        rol     a           ;2 get bit 3 (data), now we have a nybble
        sta     $B3         ;3 store the low nybble of the byte we are reading in $B3
        
        lda     $911F       ;4 read serial bus again for the high nybble
        ror     a           ;2 get bit 4 (clock)
        ror     a           ;2 get bit 5 (data)
        and     #$80        ;2 clear everything that isn't the bits we want
.IFNDEF NTSC
        nop                 ;2
.ENDIF

;timing:
;NTSC: 19*(1/1.022727) microseconds = 18.577782732 microseconds
;PAL: 19*(1/1.108405) microseconds = 17.141748729 microseconds
        ora     $911F       ;4 read bits 6 & 7
        rol     a           ;2 get bit 6 (clock)
        rol     a           ;2 and bit 7 (data)
        sta     $C0         ;3 store the most significant nybble in $C0
        lda     $911F       ;4 read clock and data once more to set recieved flag
        stx     $912C       ;4 handshaking - bring serial bus data line low
;end of timing sensitive portion       

        sta     $9C         ;set byte-received flag
        jsr     JIFFY_BYTE_IN   ;assemble the byte that was read

        sta     $A4
        pla             ;restore .X
        tax             ;yep
        lda     $9C     ;check byte-recieved flag
        ror     a       ;bit 0
        ror     a       ;bit 1
        bpl     LFC50   ;was bit 0 zero? If so, we're done
        bcc     LFC4B   ;was bit 1 zero? If so, set status and we're done
        lda     #$42    ;ERROR: both were one's
        jmp     LEEB9   ;ERR STATUS, UNLISTEN

.IFDEF NTSC
        .res    3
.ENDIF

NEW_IECOUT:
LFC3D:  sei
        bit     $A3
        bvs     JIFFY_OUT
        lda     $A3
        cmp     #$A0
        bcs     JIFFY_OUT
        jmp     OLD_IECOUT

LFC4B:  lda     #$40
        jsr     LFE6A   ; SET STATUS
LFC50:  lda     $A4
JIFFY_OUT_DONE:
LFC52:  cli
        clc
        rts

;--------------------------------------
JIFFY_OUT:
;JIFFYDOS PATCH SEND DATA ON SERIAL LINE in C64 docs
; the bits in BSOUR are sent in the following order %22114334
;
LFC55:  txa             ; store .X on stack
        pha
        lda     $95     ; BSOUR, the byte to send
        lsr     a       ; put MSB in LSB
        lsr     a
        lsr     a
        lsr     a
        tax             ; give to .X
        lda     LFCCE,x ; get the corresponding data from the send table 
        pha             ; save it
        txa             ; restore .A to .X
        lsr     a       ; next 2 bits
        lsr     a
        tax             ; give to .X
        lda     LFCCE,x ; get the corresponding send table data again
        sta     $B3     
        lda     $95     ; restore BSOUR
        and     #$0F    ; get LSB of BSOUR
        tax             ; give to .X
        lda     #$02

;start of timing sensitive portion
;total time:
;NTSC: (15+14+20+17+18/19+13) * (1/1.022727) = 94.844469736 microseconds
;PAL:  (15+16+22+19+20/21+13) * (1/1.108405) = 94.73071666 microseconds

;timing:
;NTSC: 15
;PAL: 15
LFC72:  bit     $911F   ;4 wait for bit 1 (data) of $911F to be set
        beq     LFC72   ;2 loop until data is 1
        
        lda     $912C   ;4 handshaking - (bring serial bus data line high)
        and     #$DD    ;2 yep
        sta     $9C     ;3 save what we want to handshake.
;timing
;NTSC: 14 * 
;PAL: 20 * 
        pha             ;3
        pla             ;4
        pha             ;3
        pla             ;4
.IFNDEF NTSC
        nop             ;2
        nop             ;2
        nop             ;2
.ENDIF
;timing
;NTSC: 14
;PAL: 16
        sta     $912C   ;4 handshaking - bring the data line high 
        pla             ;3 restore .A (gotten from send table earlier)
        ora     $9C     ;3 OR with handshake value to get value to send

.IFNDEF NTSC
        nop             ;2
.ENDIF
        sta     $912C   ;4 send to drive over serial bus

;timing
;NTSC: 20
;PAL: 22
        lda     $B3     ;3 get 2nd value to send
        ora     $9C     ;3 OR with old $912C 
        ora     $9C     ;3 timing
        sta     $912C   ;4 send to drive over serial bus
        
        lda     LFBB9,x ;4 Get third value to send from table
        ora     $9C     ;3 OR with old $912C 
.IFNDEF NTSC
        nop             ;2
.ENDIF

;timing
;NTSC: 17
;PAL: 19
        sta     $912C   ;4 send to drive over serial bus
        lda     LF39E,x ;4 Get fourth value to send from table
        ora     $9C     ;3 OR with old $912C
        nop             ;2 timing
        sta     $912C   ;4 send to drive over serial bus
.IFNDEF NTSC
        nop             ;2
.ENDIF

;timing
;NTSC: 18/19 
;PAL: 20/21
        and     #$DD    ;2 
        bit     $A3     ;3 is bit 7 of LDFLAG set?
        bmi     LFCB3   ;2/3 yes, don't bring data line low yet
        ora     #$02    ;2 no, OR to bring serial bus data line low
LFCB3:  sta     $912C   ;4 handshaking - bring data line low

        pla             ;4 restore .X
        tax             ;2
.IFNDEF NTSC
        nop             ;2
.ENDIF

;timing
;NTSC: 13
;PAL: 13
        lda     $9C     ;3 get old $912C
        ora     #$02    ;2 OR to bring data line low
        sta     $912C   ;4 handshaking - bring data line low
        lda     $911F   ;4 read serial bus
        and     #$02    ;2 is data line low?
;end of timing sensitive portion

        beq     JIFFY_OUT_DONE  ; yes, we're done
        jmp     LEEB7           ; no, err TIME OUT
;------------JIFFY BYTE OUT

.IFDEF NTSC 
        .res 7
.ENDIF

; LFBDB-$FD10 deleted (looks like old drive out/in code)

;----------------JIFFY DATA TABLE
LFCCE:  .byte $00,$02,$20,$22,$00,$02,$20,$22,$00,$02,$20,$22,$00,$02,$20,$22
;---------------JIFFY DATA TABLE

.res    1       ; FREE


;TODO: this needed?
;LFCD3:  beq     RENAME_THIS     ; 
;        ldx     #$F7            ; 
;        jmp     PRINT_FILENAME

;--------------------------------------
JIFFYDOS_X_COMMAND:
;The following routine sets the  destination devicenumber when using the 
;JiffyDOS copyroutine.
        jsr $d79b               ; GTBYTC, get destination device
        stx $bf                 ; store in $BF
        rts
        .byte 0

;--------------------------------------
READ_INTO_BUFFER:
; The following routine is used by the LIST ASCII and LIST BASIC
; directly from disk. It reads a number of bytes into the filename
; buffer area. 
;
        jsr     JIFFY_SET_CHKIN
@0:     jsr     $FFCF           ; CHRIN, get a character
        sta     ($BB),y         ; store in buffer for current filename
        iny                     ; next character
        bit     $90             ; test STATUS
        bvs     RENAME_THIS     ; exit
        cpy     #$FE            ; max length
        bcs     RENAME_THIS     ; yep
        cmp     #$01            ; larger than 1
        bcs     @0              ; yep, repeat
RENAME_THIS:  rts

;--------------------------------------
DISABLE_JIFFYDOS_COMMANDS:
; The following routine is called by the @X command and restores the
; IERROR, IMAIN and ICRNCH vector.
;
        ldx     #$05
@0:     lda     OLD_IVECTORS_TABLE,x        ; table with original vectors
        sta     $0300,x                     ; store in vector table
        dex
        bpl     @0

        stx     $9B                         ; .X = 255, JiffyDOS not activated
        rts
		
        lda     $AC
        ora     ($29,x)
        sbc     $0185,x
		
LFD11:  sec
        lda     $AC
        sbc     $AE
        lda     $AD
        sbc     $AF
        rts
LFD1B:  inc     $AC
        bne     LFD21
        inc     $AD
LFD21:  rts

;--------------------------------------
POWER_RESET_ENTRY_POINT:
;The system hardware reset vector ($FFFC) points here. This is the first
; routine executed when the computer is switched on. The routine firstly
; sets the stackpointer to #ff, disables interrupts and clears the decimal
; flag. It jumps to a routine at $fd3f which checks for autostart-cartridges.
; If so, an indirectjump is performed to the cartridge coldstart vector at
; $A000. I/O chips are initiated, and system constants are set up. Finaly
; the IRQ is enabled, and an indirect jump is performed to $c000, the basic
; cold start vector. 
;
LFD22:  ldx     #$FF
        sei
        txs					;initialize stack pointer
        cld					;disable decimal mode
        jsr     LFD3F		;check if there is a cartridge present
        bne     LFD2F		;if not, continue
        jsr     CHECK_AUTOSTART	;($A000)		;otherwise see if C= key is pressed, autostart if it isn't
LFD2F:  jsr     RAMTAS		;initialize RAM constants
        jsr     LFD52		;BASIC warm start
        jsr     LFDF9		;Initialize I/O (set VIA registers)
        jsr     LE518		;CINIT: set cursor color, screen address.
        cli
        jmp     ($C000) 	;BASIC coldstart

;--------------------------------------
CHECK_A_ROM:
; Checks for the ROM autostartparametrar at $A004-$A008.
; It compares data with $fd10, and if equal, set Z=1.
;
LFD3F:  ldx     #$05		;5 bytes to check
LFD41:  lda     LFD4C,x		;identifier
        cmp     $A003,x		;compare with cartridge header
        bne     LFD4C		;not equal
        dex
        bne     LFD41		;repeat until all bytes checked
LFD4C:  rts

;--------------------------------------
; A0CBM - string to look for at start of cartridge
;
        .byte   $41,$30,$C3,$C2,$CD
        ;eor     ($30,x)
        ;.byte   $C3
        ;.byte   $C2
        ;.byte   $CD
;--------------------------------------
RESTOR:
;KERNAL reset
;The KERNAL routine RESTOR ($ff8a) jumps to this routine. It restores
; (copys) the KERNAL vectors at $fd30 to $0314-$0333. Continues through
; VECTOR.
;
LFD52:  ldx     #$6D
        ldy     #$FD
        clc

;--------------------------------------
KERNAL_MOVE:
;The KERNAL routine VECTOR ($ff8d) jumps to this routine. It reads or
; sets the vactors at $0314-$0333 depending on state of carry. X/Y
; contains the adress to read/write area, normally $fd30. See $fd15.
;
LFD57:  stx     $C3
        sty     $C4
        ldy     #$1F
LFD5D:  lda     ($C3),y
        bcc     LFD66
        lda     L0314,y
        sta     ($C3),y
LFD66:  sta     L0314,y
        dey
        bpl     LFD5D
        rts

;--------------------------------------
; $FD6D (KERNAL vectors)
.word   MAIN_IRQ_ENTRY_POINT    ; $0314 interrupt vector = $EABF
.word   WARM_START_BASIC        ; $0316 break vector = $FED2
.word   $FEAD                   ; $0318 NMI vector = $FEAD
.word   OPEN                    ; $031a OPEN vector = $F40A
.word   CLOSE                   ; $031c CLOSE vector = $F34A
.word   CHKIN                   ; $031e set-input vector = $F2C7
.word   CHKOUT                  ; $0320 set output vector = $F309
.word   CLRCHN                  ; $0322 restore I/O vector = $F3F3
.word   CHRIN                   ; $0324 input vector = $F20E
.word   CHROUT                  ; $0326 output vector = $F27A
.word   STOP                    ; $0328 Test-Stop vector = $F770
.word   GETIN                   ; $032a GET vector = $F1F5
.word   CLALL                   ; $032c abort I/O vector = $F3EF
.word   WARM_START_BASIC        ; $032e user vector = $FED2
.word   $F549                   ; $0330 link to load RAM = $F549
.word   $F685                   ; $0332 link to save RAM = $F685

;--------------------------------------
RAMTAS:
;init system constants
;The KERNAL routine RAMTAS($ff87) jumps to this routine. It
; clears the pages 0,2 and 3 by writing 00 into them. It also sets
; the start of the cassette buffer - $033c, and determins how much
; free RAM-memory there is. (The tapebuffer could probably be removed,
; since JiffyDOS doesn't use tapes at all.) The memorycheck is performed
; by writing two different bytes into all memory positions, starting at
; $0400, till it reaches the ROM (the byte read is not the same as the
; one you wrote.) Note that the contents of the memory is restored afterwards.
; Finally, bottom of the memory, and top of screen-pointers are set. 
;
LFD8D:  lda     #$00
        tax
LFD90:  sta     $00,x			;fill pages 0, 2, and 3 with zeroes
        sta     $0200,x
        sta     $0300,x
        inx
        bne     LFD90			;all 256 bytes
        ldx     #$3C			;set tapebuffer to $033c
        ldy     #$03
        stx     $B2				;variables TAPE1 is used
        sty     $B3
        sta     $C1				;set $C1 to 0
        sta     $97				;set $97 to 0
        sta     $0281			;set start of memory LO to a page boundary ($xx00)
        tay
        lda     #$04			;perform memory test, starting at $0400
        sta     $C2				;>MemoryToTest
LFDAF:  inc     $C1				;increment byte to test
        bne     LFDB5
        inc     $C2				;if rollover, increment page
LFDB5:  jsr     MEMTEST			;test the byte
        lda     $97
        beq     LFDDE 			;if zero, we haven't found RAM continue looking
        bcs     LFDAF			;if carry set, we haven't found RAM continue looking
        ldy     $C2				;get byte to test HI
        ldx     $C1				;byte to test LO
        cpy     #$20			;was there RAM at $2000? 
        bcc     LFDEB			;
        cpy     #$21			;was there RAM at $2100?
        bcs     CHECK_UNEXPANDED_POINTERS ;LFDD2			;yes, the system is expanded

UNEXPANDED:
        ldy     #$1E			;unexpanded system: set video page to $1e00
        sty     $0288			;set video page
LFDCF:  jmp     LFE7B			;set memory top (if carry clear) and return

LFDD2:  
EXPANDED:
		lda     #$12			;expanded system: set start of RAM to $1200
        sta     $0282			;set start of RAM
        lda     #$10			;expanded system: set video matrix page to $1000
        sta     $0288			;set video page
        bne     LFDCF			;read memory top and return
		
LFDDE:  bcc     LFDAF			;if we haven't found RAM, continue testing
        lda     $C2				;else, set start of memory HI
        sta     $0282			;set start of memory to where we found RAM
        sta     $97				;save this position
        cmp     #$11			;is start of memory $1100?
LFDE9:  bcc     LFDAF			;no, continue looking

LFDEB:  jsr     LE5C3			;initialize VIC registers, this code is not normally executed
        jmp     LFDEB			;keep initializing VIC registers (endless loop)

;-------------------------------------
;TAPE IRQ VECTORS (free)
;

;--------------------------------------
CHECK_UNEXPANDED_POINTERS:
;
    jmp DO_CHECK_UNEXPANDED_POINTERS

.res    5                       ; FREE

	
;        tay
;        .byte   $FC
;        .byte   $0B
;        .byte   $FC
;        .byte   $BF
;        nop
;        .byte   $8E
;        .byte   $F9

;--------------------------------------
INIT_IO:
; The KERNAL routine IOINIT ($ff84) jumps to this routine. It sets the
; init-values for the VIAs (IRQ, DDRA, DRA etc.)
;
LFDF9:  lda     #$7F
        sta     $911E
        sta     $912E
        lda     #$40
        sta     $912B
        lda     #$40
        sta     $911B
        lda     #$FE
        sta     $911C
        lda     #$DE
        sta     $912C
        ldx     #$00
        stx     $9112
        ldx     #$FF
        stx     $9122
        ldx     #$00
        stx     $9123
        ldx     #$80
        stx     $9113
        ldx     #$00
        stx     $911F
        jsr     LEF84
        lda     #$82
        sta     $911E
        jsr     LEF8D
LFE39:  lda     #$C0
        sta     $912E

;--------------------------------------
ENABLE_TIMER:
;This routine inits and starts the CIA#1 timer A according to the definition of
;NTSC on compilation.
;Different system clocks rates are used in PAL/NTSC systems.
;
.IFDEF NTSC
        lda     #$89
.ELSE
        lda     #$26
.ENDIF
        sta     $9124
      
.IFDEF NTSC
        lda     #$42
.ELSE     
        lda     #$48
.ENDIF
        sta     $9125
        rts

;--------------------------------------
SETNAM:
;save filename data
;The KERNAL routine SETNAM ($ffbd) jumps to this routine. On entry,
; A-reg holds the length of the filename, and X/Y the address in mem
; to the filename.
LFE49:  sta     $B7
        stx     $BB
        sty     $BC
        rts

;--------------------------------------
SETLFS:
;save file details
; The KERNAL routine SETLFS ($ffba) jumps to this routine. On entry A-reg
; holds the logical filenumber, X the device number, and Y the secondary
; address.
LFE50:  sta     $B8			;store logical filenumber in LA
        stx     $BA			;store devicenumber in FA
        sty     $B9			;store secondary address in SA
        rts

;--------------------------------------
READST:
;read status
; The KERNAL routine READST ($ffb7) jumps to this routine. The routine
; checks if the current devicenumber is 2, (ie RS232) then the value of
; RSSTAT (the ACIA 6551 status)is returned in (A), and RSSTAT is cleared.
; Else it reads and returnes the value of STATUS.
;
LFE57:  lda     $BA			;read current device number from FA
        cmp     #$02		;device = RS232?
        bne     LFE68		;nope, read STATUS
        lda     $0297		;RSSTAT
        lda     #$00
        sta     $0297		;clear RSSTAT
        rts

;--------------------------------------
SETMSG:
;flag status
; The KERNAL routine SETMSG ($ff90) jumps to this routine. On entry, the
; value in (A) is stored in MSGFLG, then the I/O status is placed in (A).
; If routine is entered at $fe6a the contents in (A) will be stored in STATUS.
LFE66:  sta     $9D         ; store MSGFLG
LFE68:  lda     $90         ; read STATUS
LFE6A:  ora     $90
        sta     $90         ; store STATUS
        rts

;--------------------------------------
SETTMO:
;set timeout
; The KERNAL routine SETTMO ($ffa2) jumps to this routine. On entry the
; value in (A) is stored in the IEEE timeout flag. (Who uses IEEE nowadays?)
;
LFE6F:  sta     $0285
        rts

;--------------------------------------
MEMTOP:
;read/set top of memory
;The KERNAL routine MEMTOP ($ffa9) jumps to this routine. If carry is set
; on entry, the top of memory address will be loaded into (X/Y). If carry
; is clear on entry, the top of memory will be set according to the contents
; in (X/Y)
;
LFE73:  bcc     LFE7B
LFE75:  ldx     $0283
        ldy     $0284
LFE7B:  stx     $0283
        sty     $0284
        rts

;--------------------------------------
MEMBOT:
;read/set bottom of memory
; The KERNAL routine MEMBOT ($ff9c) jumps to this routine. If carry is set
; on entry, the bottom of memory address will be loaded into (X/Y). If carry
; is clear on entry, the bottom of memory will set according to the contents
; in (X/Y)
;
LFE82:  bcc     LFE8A
        ldx     $0281
        ldy     $0282
LFE8A:  stx     $0281
        sty     $0282
        rts

;--------------------------------------
MEMTEST:
;test memory
; RAMTAS calls this proc.
; it's combined with RAMTAS in the C64 Kernal. 
;
LFE91:  lda     ($C1),y
        tax
        lda     #$55
        sta     ($C1),y		;write $55 to memory
        cmp     ($C1),y		;and compare
        bne     LFEA4		;if not equal...ROM
        ror     a
        sta     ($C1),y		;write $22 to memory (and set carry)
        cmp     ($C1),y		;and compare
        bne     LFEA4		;if not equal...ROM
        .byte   $A9			;LDA $18 (use the next instruction, CLC, as data)
LFEA4:  clc
        txa
        sta     ($C1),y
        rts

;--------------------------------------
NMI_ENTRY_POINT:
;The processor jumps to this routine every time a NMI occurs
; (see jump vector at $fffa). On entry all processor registers will be put
; on the stack. The routine will check the presence of a ROM cartridge at
; $A000 with autostart, and warm start it. Otherwise, the following warm
; start routine is called.
;
        sei                 ; disable interrupts
        jmp     (L0318)     ; jump to NMINV, points normally to $fead
LFEAD:  pha                 ; store .A, .X, .Y on the stack
        txa
        pha
        tya
        pha
        lda     $911D       ; IFR, interrupt flag register check if interrupt from VIA#1
        bpl     LFEFF       ; nope, done
        and     $911E       ; IER, interrupt enable register
        tax
        and     #$02        ; NMI caused by RS232?
        beq     LFEDE       ; if so, jump
        jsr     LFD3F       ; check for autostart @ $A000
        bne     LFEC7       ; don't autostart, continue
        jmp     (LA002)     ; jump to warm start vector
LFEC7:  bit     $9111       ; 
        jsr     LF734       ; bump clock and read keyboard
        jsr     LFFE1       ; check $91 to see if <STOP> was pressed
        bne     LFEFF       ; <STOP> not pressed, skip the following part

;--------------------------------------
WARM_START_BASIC:
; This routine is called from the NMI routine above. If <STOP> was
; pressed, then KERNAL vectors are restored to default values, I/O
; vectors initialised and a jump to ($a002), the Basic warm start vector.
; The NMI routine continues at $fxxx by checking the RS232, if there is anyting to send.
;
        jsr     LFD52           ; KERNAL reset
        jsr     LFDF9           ; init I/O
        jsr     LE518           ; init I/O
        jmp     (LC002)         ; jump to BASIC warm start vector
LFEDE:  lda     $911E
        ora     #$80
        pha
        lda     #$7F
        sta     $911E
        txa
        and     #$40
        beq     LFF02
        lda     #$CE
        ora     $B5
        sta     $911C
        lda     $9114
        pla
        sta     $911E
        jsr     LEFA3
LFEFF:  jmp     LFF56
LFF02:  txa
        and     #$20
        beq     LFF2C
        lda     $9110
        and     #$01
        sta     $A7
        lda     $9118
        sbc     #$16
        adc     $0299
        sta     $9118
        lda     $9119
        adc     $029A
        sta     $9119
        pla
        sta     $911E
        jsr     LF036
        jmp     LFF56
LFF2C:  txa
        and     #$10
        beq     LFF56
        lda     $0293
        and     #$0F
        bne     LFF38
LFF38:  asl     a
        tax
        lda     LFF5A,x
        sta     $9118
        lda     LFF5B,x
        sta     $9119
        lda     $9110
        pla
        ora     #$20
        and     #$EF
        sta     $911E
        ldx     $0298
        stx     $A8
LFF56:  pla                     ; restore registers .Y, .X, and .A
        tay
        pla
        tax
LFF5A:  pla
LFF5B:  rti                     ; back from NMI

;--------------------------------------
;RS232 timing table
; Timing table for RS232 NMI. The table
; contains 10 entries which corresponds to one of the fixed RS232 rates,
; starting with lowest (50 baud) and finishing with the highest (2400 baud).
; Since the clock frequency is different between NTSC and PAL systems, the table
; is different depending on whether this is compiled for NTSC or PAL
;
.IFNDEF NTSC
.byte $e6, $2a, $78
.byte $1c, $49, $13, $b1
.byte $0f, $0a, $0e, $d3
.byte $06, $38, $03, $6a
.byte $01, $d0, $00, $83
.byte $00, $36, $00
.ELSE
.byte $92, $27, $40
.byte $1a, $c6, $11, $74
.byte $0e, $ee, $0c, $45
.byte $06, $f1, $02, $46
.byte $01, $b8, $00, $71
.byte $00, $2a, $00
.ENDIF

;--------------------------------------
IRQ_ENTRY:
; This routine is pointed to by the hardware IRQ vector at $fffe. This routine
; is able to distinguish between s hardware IRQ, and a software BRK. The two
; types of interrupts are processed by its own routine.
;
LFF72:  pha                 ; store .A
        txa 
        pha                 ; store .X
        tya
        pha                 ; store .Y
        tsx
        lda     $0104,x     ; read byte on stack written by processor?
        and     #$10        ; check bit 4 to determine HW or SW interrupt
        beq     LFF82   
        jmp     (L0316)     ; jump to CBINV, points to $fed2, BASIC warm start
LFF82:  jmp     (L0314)     ; jump to CINV, points to , $eabf main IRQ entry

;--------------------------------------
LFCDE:  lda     #$10
        jmp     LFE6A
        
;--------------------------------------
        jmp     LFD52   ; $FF8A, RESTOR
LFF8D:  jmp     LFD57   ; $FF8D, VECTOR
        jmp     LFE66   ; $FF90, SETMSG
LFF93:  jmp     LEEC0   ; $FF93, SECOND
LFF96:  jmp     LEECE   ; $FF96, TKSA
LFF99:  jmp     LFE73   ; $FF99, MEMTOP
LFF9C:  jmp     LFE82   ; $FF9C, MEMBOT
        jmp     LEB1E   ; $FF9F, SCNKEY
        jmp     LFE6F   ; $FFA2, SETTMO
LFFA5:  jmp     JIF_IECIN   ; $FFA5, ACPTR
        jmp     LEEE4   ; $FFA8, CIOUT
LFFAB:  jmp     LEEF6   ; $FFAB, UNTALK
LFFAE:  jmp     LEF04   ; $FFAE, UNLSN
LFFB1:  jmp     LEE17   ; $FFB1, LISTEN
LFFB4:  jmp     LEE14   ; $FFB4, TALK
LFFB7:  jmp     LFE57   ; $FFB7, READST
LFFBA:  jmp     LFE50   ; $FFBA, SETLFS
LFFBD:  jmp     LFE49   ; $FFBD, SETNAM
LFFC0:  jmp     (L031A) ; $FFC0, OPEN
LFFC3:  jmp     (L031C) ; $FFC3, CLOSE
LFFC6:  jmp     (L031E) ; $FFC6, CHKIN
LFFC9:  jmp     (L0320) ; $FFC9, CHKOUT
LFFCC:  jmp     (L0322) ; $FFCB, CLRCHN
LFFCF:  jmp     (L0324) ; $FFCF, CHRIN
LFFD2:  jmp     (L0326) ; $FFD2, CHROUT
LFFD5:  jmp     LF542   ; $FFD5, LOAD
LFFD8:  jmp     LF675   ; $FFD8, SAVE
        jmp     LF767   ; $FFDB, SETTIM
        jmp     LF760   ; $FFDE, RDTIM
LFFE1:  jmp     LE4E3   ; $FFE1, STOP (calls ROM routine before jumping to ($0328)
LFFE4:  jmp     (L032A) ; $FFE4, GETIN
        jmp     (L032C) ; $FFE7, CLALL
LFFEA:  jmp     LF734   ; $FFEA, UDTIM, increment real time clock
        jmp     LE505   ; $FFED, SCREEN
        jmp     LE50A   ; $FFF0, PLOT
LFFF3:  jmp     LE500   ; $FFF3, IOBASE
;--------------------------------------
; FREE?
        .byte   $FF
        .byte   $FF
        .byte   $FF
        .byte   $FF
;--------------------------------------
; HARDWARE VECTORS
;
.word   $FEA9           ; NMI hardware vector
.word   $FD22           ; system reset vector
.word   $FF72           ; IRQ hardware vector

