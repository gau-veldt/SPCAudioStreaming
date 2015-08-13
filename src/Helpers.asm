;
;  Helper routines
;

FrameCount = $0200
PPUsecs    = $0204
APUsecs    = $0206
vblsM60    = $0208
bSeconds   = $0209
bMinutes   = $020a
helprTemp  = $020b
wHours     = $020c
clkPrevRd  = $020e
bHlprRsvd1 = $020f

HelpersInit:
    ; zero frame counter
    %MC16()
    stz FrameCount : stz FrameCount+2
    stz PPUsecs
    stz APUsecs
    stz vblsM60     ; (also clears bSeconds)
    stz bMinutes    ; (also clears helprTemp)
    stz wHours
    stz clkPrevRd   ; (also clears bHlprRsvd1)
    rts

BumpFrameCount:
    php
    %MC16()
    clc
    lda.w #$01
    adc   FrameCount
    sta   FrameCount
    lda.w #$00
    adc   FrameCount+2
    sta   FrameCount+2
    ; advance frames%60 counter
    %MC8()
    inc vblsM60
    lda vblsM60
    cmp #60
    bcc +
        sec
        sbc #60
        sta vblsM60
        ; a wraparound means a second advanced
        %MC16()
        inc PPUsecs
        %MC8()
    +
    lda dbSPCRDY : beq +
        ; whenever SPC is ready advance clock
        lda $2141 : pha
        cmp clkPrevRd
        beq ++++
            bcc ++
                ; cur > prev
                sec
                sbc clkPrevRd                       ; cur - prev
                bra +++
            ++
                ; cur < prev (wrapped)
                lda clkPrevRd
                eor #$ff : inc a                    ; a=256-old (eg: 2 if a was $fe)
                sta helprTemp
                pla : pha                           ; a=new
                clc : adc helprTemp                 ; a=(256-old)+new
            +++
            pha
            %MC16()
            and.w #$00ff
            clc : adc APUsecs : sta APUsecs
            %MC8()
            pla
            ; advance clock (delta seconds in a)
            clc
            adc bSeconds
            sta bSeconds
            cmp #60
            bcc ++++
            sec : sbc #60 : sta bSeconds
            inc bMinutes : lda bMinutes
            cmp #60
            bcc ++++
            sec : sbc #60 : sta bMinutes
            %MC16() : inc wHours : %MC8()
        ++++
        pla : sta clkPrevRd
    +
    plp
    rts
