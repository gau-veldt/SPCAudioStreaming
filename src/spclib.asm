; High-level interface to SPC-700 bootloader
;
; 1. Call spc_wait_boot
; 2. To upload data:
;       A. Call spc_begin_upload
;       B. Call spc_upload_byte any number of times
;       C. Go back to A to upload to different addr
; 3. To begin execution, call spc_execute
;
; Have your SPC code jump to $FFC0 to re-run bootloader.
; Be sure to call spc_wait_boot after that.


; Waits for SPC to finish booting. Call before first
; using SPC or after bootrom has been re-run.
; Preserved: X, Y
spc_wait_boot:
    lda #$AA
    -   cmp $2140
        bne -

    ; Clear in case it already has $CC in it
    ; (this actually occurred in testing)
    sta $2140

    lda #$BB
    -   cmp $2141
        bne -

    rts


; Starts upload to SPC addr Y and sets Y to
; 0 for use as index with spc_upload_byte.
; Preserved: X
spc_begin_upload:
    sty $2142

    ; Send command
    lda $2140
    clc
    adc #$22
    bne +       ; special case fully verified
        inc
    +
    sta $2141
    sta $2140

    ; Wait for acknowledgement
    -   cmp $2140
        bne -

    ; Initialize index
    ldy.w #0

    rts


; Uploads byte A to SPC and increments Y. The low byte
; of Y must not changed between calls.
; Preserved: X
spc_upload_byte:
    sta $2141

    ; Signal that it's ready
    tya
    sta $2140
    iny

    ; Wait for acknowledgement
    -   cmp $2140
        bne -

    rts


; Starts executing at SPC addr Y
; Preserved: X, Y
spc_execute:
    sty $2142

    stz $2141

    lda $2140
    clc
    adc #$22
    sta $2140

    ; Wait for acknowledgement
    -   cmp $2140
        bne -

    rts

;
; Writes high byte of X to SPC-700 DSP register in low byte of X
;
write_dsp:
    phx
    ; Just do a two-byte upload to $00F2-$00F3, so we
    ; set the DSP address, then write the byte into that.
    ldy.w #$00F2
    jsr spc_begin_upload
    pla
    jsr spc_upload_byte     ; low byte of X to $F2
    pla
    jsr spc_upload_byte     ; high byte of X to $F3
    rts

macro setdsp(reg,val)
    ldy.w #$00F2
    jsr spc_begin_upload
    lda #<reg>
    jsr spc_upload_byte
    lda #<val>
    jsr spc_upload_byte
endmacro