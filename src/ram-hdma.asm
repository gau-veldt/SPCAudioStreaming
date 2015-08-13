;
;  Create a WRAM-based (banks 7e/7f)
;  color table with HDMA routines to
;  utilize it.
;
;  The 960 (overscan) bytes needed in WRAM is a bit of a space
;  shock at first but you gain the ability to modify the table
;  in place rather than require several massive pre-rendered
;  (read-only) tables in ROM for gradients or effects.
;
;  Operating on the table with non-contiguous values
;  would be a headache and seven halves to routines
;  trying to modfy them so the solution is to use an
;  indirect table (in ROM) to reference the WRAM cells
;  in a contiguous manner.
;
;  When I figure out SA-1's BWRAM banking stuff I want to
;  move this table into the BWRAM area instead since SA-1
;  has no access to WRAM.  Furthermore doing so allows the
;  effect to be double-buffered by using two different
;  00:4000-00:6fff banks to prevent modifications
;  performed outside of VBlank from being visible
;  mid-progress while the HDMA is active.
;

RegisterStart = $2000
BWRAM_Top = $6000
HDMA_Table_Size = 240*4
HDMA_Table_Half = 120*4
FullScanlines = 240
HalfScanlines = 120

; Sets up an HDMA table suitable for
; any of the two-word transfer modes
; (intended for mode 3 but mode 4 works two)
Init_HDMA_Table_WRAM:
    clc
    bra Init_HDMA_Table
Init_HDMA_Table_BWRAM:
    sec
Init_HDMA_Table:
    rep #$30 ; xy 16 bit
    ldx #HDMA_Table_Size
    -
        dex
        dex
        ; if bwram skip wram store
        bcs ++
            stz RegisterStart-HDMA_Table_Size,x
        ; if wram skip bwram store
        bcc +
        ++
            stz BWRAM_Top-HDMA_Table_Size,x
        +
        bne -
    rts

; arguments:
;   A.l: channel 0-7
Start_BWRAM_HDMA:
    rep #$30
    ldx #HDMA_Table_BWRAM
    bra Start_HDMA
Start_WRAM_HDMA:
    rep #$30
    ldx #HDMA_Table_WRAM
Start_HDMA:
    and.w #$0007
    sep #$20
    ; save channel
    pha
    rep #$20
    ; A=$010*channel
    asl : asl : asl : asl
    tay
    sep #$20
    ; mem-->ppu indirect mode 3 (word1.l->reg1 word2.l->reg2 word1.h->reg1 word2.h->reg2)
    lda #$43 : sta $4300,y
    ; target register ($2121,$2122)
    lda #$21 : sta $4301,y
    ; location
    rep #$20
    txa
    ; address
    sta $4302,y
    ; bank
    sep #$20 : phk : pla : sta $4304,y
    ; bank for indirect
    lda #$00
    sta $4307,y
    ; recall channel
    pla
    ; enable HDMA (channel in A)
    rep #$20
    and.l #$0007
    tax
    sep #$20
    lda BitValue8,x
    tsb sbHDMAEN
    rts
 
Stop_HDMA_7:
    sep #$20
    lda #$80
    trb sbHDMAEN
    rts

HDMA_Table_WRAM:
    db $80+HalfScanlines
    dw RegisterStart-HDMA_Table_Size
    db $80+HalfScanlines
    dw RegisterStart-HDMA_Table_Half
    db $00

HDMA_Table_BWRAM:
    db $80+HalfScanlines
    dw BWRAM_Top-HDMA_Table_Size
    db $80+HalfScanlines
    dw BWRAM_Top-HDMA_Table_Half
    db $00
