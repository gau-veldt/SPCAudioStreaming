;
;  Implements shift and add-3 to convert binary
;  valued numbers to BCD
;

; 32-bit conversions require 10 digits
; 32-bit input binary
BINVAL = $210
; BCD buffer (5 bytes padded to 6)
BCDBUF = $214
; output digits (10 bytes)
DIGITS = $21a
; counts number of shifts
B2D_SHIFTS = $224
B2D_TODO   = $225

Bin2Dec:
Bin2Dec32:
    ; converts 32-bit value in BINVAL
    ; to 10-digit BCD in DIGITS
    ; preserves X and Y
    lda #$20
    sta B2D_TODO
    bra Bin2DecX
Bin2Dec8:
    ; converts 8-bit value in BINVAL
    ; to 10-digit BCD in DIGITS
    ; preserves X and Y
    lda #$08
    sta B2D_TODO
    lda BINVAL
    sta BINVAL+3                    ; left-justify 8-bit value
    bra Bin2DecX
Bin2Dec16:
    ; converts 16-bit value in BINVAL
    ; to 10-digit BCD in DIGITS
    ; preserves X and Y
    lda #$10
    sta B2D_TODO
    %MC16()
    lda BINVAL
    sta BINVAL+2                    ; left-justify 16-bit value
    %MC8()
    bra Bin2DecX
Bin2Dec24:
    ; converts 24-bit value in BINVAL
    ; to 10-digit BCD in DIGITS
    ; preserves X and Y
    lda #$18
    lda BINVAL+2 : sta BINVAL+3
    lda BINVAL+1 : sta BINVAL+2
    lda BINVAL+0 : sta BINVAL+1     ; left-justify 24-bit value
    sta B2D_TODO
Bin2DecX:
    ; converts N-bit value in BINVAL
    ; N (bits to convert) is in B2D_TODO
    ; to 10-digit BCD in DIGITS
    ; preserves X and Y
    phx
    phy
    
    ldx.w #$0e
    lda #$0
    -   sta BCDBUF,x
        dex
    bpl -
    
    stz B2D_SHIFTS
    
    -   ;shift binary value
        rep #$20
        clc
        lda BINVAL   : rol : sta BINVAL
        lda BINVAL+2 : rol : sta BINVAL+2
        lda BCDBUF   : rol : sta BCDBUF
        lda BCDBUF+2 : rol : sta BCDBUF+2
        lda BCDBUF+4 : rol : sta BCDBUF+4
        sep #$20
        inc B2D_SHIFTS
        lda B2D_SHIFTS
        cmp B2D_TODO
        beq B2D_Done
        ldy.w #$04
        ; add 3 test is in a lookup table
        --  lda BCDBUF,y
            xba
            lda #$00
            xba
            rep #$20
            tax
            sep #$20
            lda.l Add3Tbl,x
            sta BCDBUF,y
            dey
        bpl --
    bra -
 
    B2D_Done:
    ; copy bcd into digits
    ldy.w #$09
    ldx.w #$00
    -   lda BCDBUF,x
        pha
        and #$0f
        sta DIGITS,y
        dey
        pla
        lsr : lsr : lsr : lsr
        and #$0f
        sta DIGITS,y
        dey
        inx
        cpx.w #$05
    bne -

    ply
    plx
    rts

Add3Tbl:
    db $00,$01,$02,$03,$04,$08,$09,$0a,$0b,$0c,$00,$00,$00,$00,$00,$00
    db $10,$11,$12,$13,$14,$18,$19,$1a,$1b,$1c,$00,$00,$00,$00,$00,$00
    db $20,$21,$22,$23,$24,$28,$29,$2a,$2b,$2c,$00,$00,$00,$00,$00,$00
    db $30,$31,$32,$33,$34,$38,$39,$3a,$3b,$3c,$00,$00,$00,$00,$00,$00
    db $40,$41,$42,$43,$44,$48,$49,$4a,$4b,$4c,$00,$00,$00,$00,$00,$00
    db $80,$81,$82,$83,$84,$88,$89,$8a,$8b,$8c,$00,$00,$00,$00,$00,$00
    db $90,$91,$92,$93,$94,$98,$99,$9a,$9b,$9c,$00,$00,$00,$00,$00,$00
    db $a0,$a1,$a2,$a3,$a4,$a8,$a9,$aa,$ab,$ac,$00,$00,$00,$00,$00,$00
    db $b0,$b1,$b2,$b3,$b4,$b8,$b9,$ba,$bb,$bc,$00,$00,$00,$00,$00,$00
    db $c0,$c1,$c2,$c3,$c4,$c8,$c9,$ca,$cb,$cc,$00,$00,$00,$00,$00,$00
