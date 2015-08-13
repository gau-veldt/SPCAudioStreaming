incsrc "src/macros.asm"

AUDBANK_LO = $01
;AUDBANK_HI = $b6   ; for candy.brr
;AUDBANK_HI = $92   ; for top-dream.brr
AUDBANK_HI = $40   ; for yururenai.brr
;AUDBANK_HI = $cd   ; for hurricane.brr

SLICE_LEN = $00000ee0   ; slice count of hurricane.brr

;sa1rom 0,1,2,3
lorom

org $00ffc0
;db "Anime Mayhem RPG     "          ; 21-byte game title
;db $23                              ; Use SA-1
;db $35                              ; SA-1+ROM+RAM+BATTERY
;db $0D                              ; ROM SIZE > 32 Mbit
;db $07                              ; 128 KB BW-RAM
;db $01                              ; NTSC
;db $01                              ; hk
;db $00                              ; Rom Version
db "Anime Mayhem RPG     "          ; 21-byte game title
db $20                              ; LoROM only
db $02                              ; ROM+BATTERY
db $0C                              ; ROM SIZE > 32 Mbit
db $05                              ; 32 KB BRAM
db $01                              ; NTSC
db $01                              ; hk
db $00                              ; Rom Version

org $00FFE0
dw $ffff
dw $ffff
dw onDummy                          ; cop
dw onDummy                          ; brk
dw onDummy                          ; abt
dw onNMI                            ; NMI Vector (NATIVE)
dw $FFFF
dw onIRQ                            ; IRQ Vector (NATIVE)
dw $ffff
dw $ffff
dw onDummy                          ; cop            (EM)
dw $ffff
dw onDummy                          ; abt            (EM)
dw onDummy                          ; NMI Vector     (EM)
dw onReset                          ; Reset Vector   (EM)
dw onDummy                          ; IRQ/BRK Vector (EM)

; shadowed register labels
!SHADOW_REGS = $1c40
sbHDMAEN  = !SHADOW_REGS-1
sbTEXTEN  = !SHADOW_REGS-2
sbSWBRITE = !SHADOW_REGS-3
sbMOSAIC  = !SHADOW_REGS-4

dbAUDBANK  = $00
dbAUDSYNC  = $01
ddAUDCOUNT = $02
dwSaveApos = $06
dbAUDWAVE  = $08
dbAUDVU    = $09
dbSPCRDY   = $0a
dbHasMSU1  = $0b

vuWindow   = $10    ; 16-vblank (16 bytes) window for vu calc
vuTempW    = $20

temp       = $fc

org $008000                         ; Beginning of ROM

incsrc "src/InitSNES.asm"
incsrc "src/Helpers.asm"
incsrc "src/spclib.asm"
incsrc "src/ram-hdma.asm"

;------------------------------------------------------------------------------
;-- Stub handler for unused/unspported vectors
;------------------------------------------------------------------------------

onDummy:
    rti

;------------------------------------------------------------------------------
;-- ROM default NMI Handler
;------------------------------------------------------------------------------

onNMI:
    phb : phd
    rep #$38 : pha : phx : phy

    %MC8()
    lda $4210

    ; set PPU/REG shadows
    lda sbSWBRITE : sta $2100
    lda sbMOSAIC : sta $2106
    lda sbHDMAEN : sta $420c

    ; DMA text screen from RAM if enabled
    lda sbTEXTEN
    beq +
        lda #$80     : sta $2115    ; port mode: vram=vram+1
        ldx #$1c00   : stx $2116    ; VRAM address=$1c00
        %DMAc0($01,$2118,$7e2000,$800)
    +

    jsr BumpFrameCount
    jsr shifty

    ; scroll VU window
    %MC8()
    ldx.w #0
    -   lda vuWindow+1,x
        sta vuWindow,x
        inx : cpx.w #15
    bne -
    ; store wave/vu point
    lda $2142
    sta dbAUDWAVE
    cmp #$80
    bcc +
        eor #$ff
        inc a
    +
    asl : asl : and #$7f        ; kludge???
    sta vuWindow+15
    ; calc VU
    ; sum of vuWindow samples / 16
    stz vuTempW
    stz vuTempW+1
    ldx.w #$0f
    -   clc
        lda vuTempW
        adc vuWindow,x
        sta vuTempW
        lda vuTempW+1
        adc #0
        sta vuTempW+1
        dex
    bpl -
    %MC16()
    lda vuTempW
    lsr : lsr : lsr : lsr
    %MC8()
    sta dbAUDVU
    
    rep #$38 : ply : plx : pla
    pld : plb
    rti                             ; Done with interrupt

;------------------------------------------------------------------------------
;-- ROM default IRQ Handler
;------------------------------------------------------------------------------

onIRQ:
    phb : phd
    rep #$38 : pha : phx : phy
    %MC8()
    
    rep #$38 : ply : plx : pla
    pld : plb
    rti

;------------------------------------------------------------------------------
;-- ROM Reset Handler
;------------------------------------------------------------------------------
onReset:
    sei : clc : xce                 ; 65816 mode
    rep #$38                        ; 16-bit AXY/binary math
    ldx #$01FF                      ; Initialize stack
    txs

    ; init 8MB MMC banking
    ; hirom hole takes rom4-rom7
    ; bit 7 is off meaning lorom hole
    ; has rom0-rom3
;    %MC8XY8()
;    ldx #$00
;    lda #$04
;    -
;        sta $2220,X
;        inc a
;        inx
;        cpx #$04
;    bne -

    %MC8XY16()
    jsr InitPPU
    jsr HelpersInit    
    jsr Init_HDMA_Table_WRAM

    %MC8()

    stz dbSPCRDY    ; SPC ready flag
    stz dbHasMSU1   ; MSU1 Present flag
    
    ; test for MSU1
    lda $2002 : cmp #'S' : bne +
    lda $2003 : cmp #'-' : bne +
    lda $2004 : cmp #'M' : bne +
    lda $2005 : cmp #'S' : bne +
    lda $2006 : cmp #'U' : bne +
    lda $2007 : cmp #'1' : bne +
        lda #$01 : sta dbHasMSU1
    +
    
    ; init shadowed registers and other
    ; controls updated/synchronized by NMI
    stz sbSWBRITE   ; screen enable/brightness
    stz sbMOSAIC    ; screen pixellation
    stz sbHDMAEN    ; HDMA enabled shadow
    stz sbTEXTEN    ; disable screen shadow

    ; clear text buffer
    ldx #$7ff
    lda #$00
    -   sta.l $7e2000,x
        dex
    bpl -
        
    ; wait until SPC-700 is ready
    jsr spc_wait_boot
    
    ; upload the driver
    ldx.w #0
    ldy SPCDriver+2         ; location
    jsr spc_begin_upload
    -   lda SPCDriver+4,x
        jsr spc_upload_byte
        inx
        cpx SPCDriver
    bne -
    
    ; start SPC-700 executing driver
    ldy SPCDriver+2
    jsr spc_execute
    
    ; turn on NMI
    -   lda $4210
        bpl -
    lda #$80
    sta $4200

    ; ensure vblank then blank screen
    lda #$80 : sta sbSWBRITE
    wai

    ; video mode 1 p0=4bpp 16x16,p1=4bpp 16x16,p2=2bpp 8x8
    lda #$31 : sta $2105
    lda #$ff
    stz $2126       ; window 1 left  = 0
    sta $2127       ; window 1 right = 255
    stz $2128       ; window 2 left  = 0
    sta $2127       ; window 2 right = 255
    stz $212a
    stz $212b       ; all window modes: OR
    lda #$0f
    sta $212c       ; all main screen things active (except sprites)
    stz $212d       ; all subscreen things off
    stz $212e       ; no masking (main)
    stz $212f       ; no masking (subscreen)
    lda #$30
    sta $2130       ; no color math
    stz $2131       ; no color math

    ; NB: VRAM addresses are in words!!!
    ; VRAM words (bytes) mapping
    ; 0000-1bff (0000-37ff): 2bpp P3 8x8 characters (allow tiles 000-37f)
    ;                        (leaves enough space for 256x224 bitmap)
    ; 1c00-1fff (3800-3fff): P3 playfield 32x32 size=0400w (0800b)
    ; 2000-2fff (4000-5fff): P2 playfield 64x64 size=1000w (2000b)
    ; 3000-3fff (6000-7fff): P1 playfield 64x64 size=1000w (2000b)
    ; 4000-7fff (8000-ffff): P1/P2 4bpp characters 16x16 size=4000w (8000b)

    ; 32x32 P3 playfield at @vram:$1c00 ($400 words)
    lda #0+$1c : sta $2109
    ; 64x64 P2 playfield at @vram:$2000 ($400 words x4)
    lda #3+$20 : sta $2108
    ; 64x64 P1 playfield at @vram:$3000 ($400 words x4)
    lda #3+$30 : sta $2107
    ; P2 characters @vram:$4000, P1 characters @vram:$4000
    lda #$44 : sta $210b
    ; P3 characters @vram:$0000
    lda #$00 : sta $210c
    ; Sprites are 16x16 or 32x32 and share tiles with P1/P2 (vram:$4000 onward)
    lda #$62 : sta $2101
    ; clear all vram $0000-$ffff
    wai
    jsr ClearVRAM

    wai
    ; store a 2bpp font @vram:$0020
    lda #$80 : sta $2115
    ldx.w #$0020 : stx $2116            ; charset to vram:0x0020
    %DMAc0($01,$2118,CharSet,16*48)

    wai
    ; set colors
    stz $2121                           ; CGRAM addr = $00
    %DMAc0($02,$2122,Palette,$200)
    
    ; start the HDMA
    lda #$07
    jsr Start_WRAM_HDMA
    ; for some reason my first few colors are scribbled on
    ; when the HDMA starts...  If I wait a frame and reset
    ; the first few colors it's fixed...
    wai
    lda #1
    sta $2121
    %DMAc0($02,$2122,Palette+2,6)
    ; enable the screen
    lda #$0f
    sta sbSWBRITE
    lda #$01
    sta sbTEXTEN
    
    ; wait message
    ldx.w #$2042
    ldy.w #0
    -   lda WaitMsg,y
        sta.l $7e0000,x : inx
        lda #0
        sta.l $7e0001,x : inx
        iny : cpy.w #15
    bne -

    -   ldx $2140 : cpx #$7733 : bne -  ; ensure SPC has intialized

    ; clear message
    lda #0
    ldx.w #$2042
    -   sta.l $7e2042,x
        inx : cpx.w #30
    bne -

    wai
    stz vblsM60
    stz PPUsecs
    stz APUsecs
    ldx.w #$0000
    stx FrameCount : stx FrameCount+2   ; reset frame based counters

    lda #$73 : sta $2140                ; acknowledge
    inc dbSPCRDY                        ; set SPC ready flag

    ; stream audio (may occur outside of vblank)
    lda dbHasMSU1
    bne StreamFromMSU1
    jmp StreamFromROM
    
    StreamFromMSU1:
    stz dbAUDBANK                           ; unused - set to 0

    ldx.w #$0000
    stx ddAUDCOUNT
    stx ddAUDCOUNT+2                        ; zero audio slice counter
    
    stx $2000
    stx $2002                               ; seek to $00000000
    - bit $2000 : bmi -                     ; wait for seek to finish

    -   lda $2143 : sta dbAUDSYNC           ; check if transfer needed
        beq +                               ; 0 if not
            lda #$ff
            sta $2140 : sta $2141
            sta $2142 : sta $2143           ;FF FF FF FF = transfer cmd
            ldy.w #1
            --  lda $2140 : bne --          ; wait for SPC acknowledge
            --  lda $2001 : sta $2140       ; send next...
                lda $2001 : sta $2141       ; three bytes...
                lda $2001 : sta $2142       ; to SPC
                tya : sta $2143             ; send count to SPC
                --- cmp $2140 : bne ---     ; wait count to be echoed
                iny : cpy.w #223
            bne --
            ; bump slice count
            %MC16()
            lda ddAUDCOUNT
            clc
            adc.w #$0001
            sta ddAUDCOUNT
            lda ddAUDCOUNT+2
            adc.w #$0000
            sta ddAUDCOUNT+2
            
            lda ddAUDCOUNT : cmp.w #SLICE_LEN&$ffff : bne ++
            lda ddAUDCOUNT+2 : cmp.w #SLICE_LEN>>16 : bne ++
                stz ddAUDCOUNT
                stz ddAUDCOUNT+2            ; reset slice count to 0
                stz $2000
                stz $2002                   ; seek to position $00000000
                %MC8()
                -- bit $2000 : bmi --       ; wait for seek to finish
            ++
            %MC8()
        +

        jsr stats

        wai
    bra -

    StreamFromROM:
    ldx.w #$0000
    stx ddAUDCOUNT
    stx ddAUDCOUNT+2            ; zero audio slice counter
    ldx.w #$8000                ; assumes AUDBANK_LO starts in lorom
    lda.b #AUDBANK_LO           ; start bank of sample
    sta dbAUDBANK
    macro nextbank()
        ldx.w #$8000            ; assume offset for lorom
        lda.l dbAUDBANK
        inc a
        
        cmp.b #$40
        bne ++++++++
            lda #$80            ; move to 80-bf area when 00-3f area exhausted
            bra ++++++++++++
        ++++++++
        cmp.b #$c0
        bcc ++++++++++++
            ldx.w #$0000        ; makes start offset $0000 when hirom bank
        ++++++++++++
        cmp.b #AUDBANK_HI
        bcc ++++++++
            lda #$00
            sta.l ddAUDCOUNT
            sta.l ddAUDCOUNT+1
            sta.l ddAUDCOUNT+2
            sta.l ddAUDCOUNT+3          ; reset slice counter
            ldx.w #$8000                ; lorom offset
            lda.b #AUDBANK_LO           ; wrap back to start of stream
            cmp #$c0 : bcc ++++++++
                ldx.w #$0000            ; hirom offset
        ++++++++
        sta.l dbAUDBANK
        %tcb()
    endmacro
    -   lda $2143 : sta dbAUDSYNC           ;check if transfer needed
        bne ++
        jmp +                               ;no transfer needed
            ++
            lda #$ff
            sta $2140 : sta $2141
            sta $2142 : sta $2143           ;FF FF FF FF = transfer cmd
            phb : phd                       ;save current D/B regs
            lda dbAUDBANK : %tcb()          ;data bank  = current audio bank
            ldy.w #$2100 : phy : pld        ;direct reg = $2100 to access PPU/APU
            ldy.w #1
            --  lda $40 : bne --            ;wait SPC ready to transfer
            cpx.w #$fd66 : bcs ++           ;test for bank cross
            ; slice does not cross bank
            --- lda.w $0,x : sta $40 : inx  ;2140
                lda.w $0,x : sta $41 : inx  ;2141
                lda.w $0,x : sta $42 : inx  ;2142
                tya : sta $43               ;2143
                -- cmp $40 : bne --
                iny : cpy.w #223
                bne ---
                jmp +++
            ++
            ; slice crosses bank
            --- lda.w $0,x : sta $40 : inx  ;2140
                bne ++ : %nextbank() : ++
                lda.w $0,x : sta $41 : inx  ;2141
                bne ++ : %nextbank() : ++
                lda.w $0,x : sta $42 : inx  ;2142
                bne ++ : %nextbank() : ++
                tya : sta $43               ;2143
                -- cmp $40 : bne --
                iny : cpy.w #223
                beq +++
                jmp ---
            +++
            pld : plb                       ;restore D/B regs
            ; bump slice count
            %MC16()
            lda ddAUDCOUNT
            clc
            adc.w #$0001
            sta ddAUDCOUNT
            lda ddAUDCOUNT+2
            adc.w #$0000
            sta ddAUDCOUNT+2
            %MC8()
        +

        jsr stats
    
        wai
    jmp -           ;make 65816 dizzy

stats:
    phy
    stx dwSaveApos

    ; write audio ptr
    lda dbAUDBANK
    ldx #$2046
    jsr WriteHexByte                            ; bank
    %MC16() : lda dwSaveApos : tay : %MC8()
    jsr WriteHexWord                            ; address

    ; write SPC REGS
    ldx #$2054
    lda $2140
    jsr WriteHexByte
    lda $2141
    jsr WriteHexByte
    phx
    %MC16()
    txa
    clc
    adc.w #$38
    tax
    %MC8()
    lda $2142
    jsr WriteHexByte
    lda dbAUDSYNC
    jsr WriteHexByte
    plx
    
    ; write audio slice
    ldx #$2082
    ldy ddAUDCOUNT+2
    jsr WriteHexWord
    ldy ddAUDCOUNT
    jsr WriteHexWord
    
    ; write vbl seconds vs. apu seconds
    ldx #$2076
    ldy PPUsecs
    jsr WriteHexWord
    ldx #$20b6
    ldy APUsecs
    jsr WriteHexWord
    
    ; write clock time
    ; display hours
    ldx wHours
    stx BINVAL
    jsr Bin2Dec16
    ldx #$209e
    ldy.w #0
    -   lda DIGITS+5,y
        clc : adc #4
        sta.l $7e0000,x
        lda #0
        sta.l $7e0001,x
        inx : inx
        iny
        cpy.w #5
    bne -
    lda #$28 : sta.l $7e0000,x  ; display :
    lda #$00 : sta.l $7e0001,x
    inx : inx
    ; display minutes
    lda bMinutes
    sta BINVAL
    jsr Bin2Dec8
    ldy.w #0
    -   lda DIGITS+8,y
        clc : adc #4
        sta.l $7e0000,x
        lda #0
        sta.l $7e0001,x
        inx : inx
        iny
        cpy.w #2
    bne -
    lda #$28 : sta.l $7e0000,x  ; display :
    lda #$00 : sta.l $7e0001,x
    inx : inx
    ; display seconds
    lda bSeconds
    sta BINVAL
    jsr Bin2Dec8
    ldy.w #0
    -   lda DIGITS+8,y
        clc : adc #4
        sta.l $7e0000,x
        lda #0
        sta.l $7e0001,x
        inx : inx
        iny
        cpy.w #2
    bne -
    
    ; display elapsed vblanks
    ldy FrameCount
    sty BINVAL
    ldy FrameCount+2
    sty BINVAL+2
    jsr Bin2Dec
    ; dump bcd digit buffer
    ldx #$2060
    ldy.w #$0000
    -   lda DIGITS,y
        clc
        adc #$04
        sta.l $7e0000,x
        lda #$00
        sta.l $7e0001,x
        inx : inx
        iny
        cpy.w #$0a
    bne -

    ; display VU bar
    ldx #$210e
    lda #$32 : sta.l $7e0000,x
    lda #$00 : sta.l $7e0001,x
    inx : inx
    lda dbAUDVU             ; in range 0-128
    ;lda FrameCount
    pha : xba : pla         ; a.h = a.l
    and #$07
    clc
    adc #$29                ; <zero hbar chr>+a
    xba
    lsr : lsr : lsr         ; a=a/8 (0-16)
    sta temp
    ldy.w #0
    -   cpy temp
        beq +
        lda #$31 : sta.l $7e0000,x
        lda #$00 : sta.l $7e0001,x
        inx : inx
        iny
        bra -
    +
    xba
    cmp #$29
    beq +
        sta.l $7e0000,x
        lda #0 : sta.l $7e0001,x
        inx : inx
        iny
    +
    -   cpy.w #16
        bcs +
        lda #$29 : sta.l $7e0000,x
        lda #0 : sta.l $7e0001,x
        inx : inx
        iny
        bra -
    +
    lda #$33 : sta.l $7e0000,x
    lda #$00 : sta.l $7e0001,x
    
    ldx dwSaveApos
    ply    
    rts

WriteHexWord:
    ; write value in Y into next four VRAM words
    phy
    pla
    xba
    pla
    jsr WriteHexByte
    xba
    jsr WriteHexByte
    rts

WriteHexByte:
    ; write value in A.l in hex into 7e0000+x
    ; in format of VRAM tilemap word
    ; input value to write in A
    ; location to write in X
    ; precondition: set VRAM address to desired output location
    ; assumes 8x8 tile indices 4-19 define characters: 0123456789abcdef
    ; X is incremented for each character
    ; on exit will be the following character location
    pha
    lsr : lsr : lsr : lsr
    clc
    adc #$04
    sta.l $7e0000,x
    lda #$00
    sta.l $7e0001,x
    inx : inx
    pla
    and #$0f
    clc
    adc #$04
    sta.l $7e0000,x
    lda #$00
    sta.l $7e0001,x
    inx : inx
    rts

shifty:
    ; short piece of code to write frame counter into HDMA table
    ; "Hello, HDMA!"  :P
    %MC16XY16()
    
    ; scrolls existing colors up the table
    ldx.w #0
    -   lda $1c46,x
        sta $1c42,x
        inx
        inx
        inx
        inx
        cpx #$3bc
        bne -

    ; makes the frame count into a pretty color gradation
    lda FrameCount
    lsr
    and.w #$00ff
    tax
    %MC8()
    lda.l SinTbl,x
    bpl .blue
    .green:
    eor #$ff
    inc a
    %MC16()
    and.w #$00ff
    asl : asl : asl
    and.w #%000001111100000
    bra .cont
    .blue:
    %MC16()
    and.w #$00ff
    xba ; takes the place of 8 asl's
    and.w #%111110000000000
    .cont
    eor.w #%111111111100000
    
    ; store the color at bottom of table (scrolls up every frame)
    sta $1ffe
    
    %MC8()
    rts

resetVox:
    %setdsp($6c,$20) ; unmuted, echo off
    %setdsp($4c,$00) ; clear key-on all voices
    %setdsp($5c,$ff) ; key-off (mute) all voices
    %setdsp($5d,$02) ; set sample directory to $0200
    %setdsp($6d,$80) ; echo buffer at $8000
    %setdsp($7d,$08) ; max echo delay

    %setdsp($0f,$7f)
    %setdsp($1f,$00)
    %setdsp($2f,$00)
    %setdsp($3f,$00)
    %setdsp($4f,$00)
    %setdsp($5f,$00)
    %setdsp($6f,$00)
    %setdsp($7f,$00) ; echo FIR coefficients 127,0,0,0,0,0,0,0

    %setdsp($00,$7f) ; V0 L VOL=max
    %setdsp($01,$7f) ; V0 R VOL=max
    ; pitch needs to be a bit below 24 kHz (0c00)
    ; due to the fact that vblank is not exactly 1/60sec
    %setdsp($02,$f5) ; PITCH L
    %setdsp($03,$0b) ;   ... H -> $0bf5 (~24 kHz)
                     ;   (exactly 23414 1/16 Hz)
                     ;   so we are getting 58.53515625 Hz VBlanks
    %setdsp($04,$00) ; Sample #0
    %setdsp($05,$11) ; A=0 D=0
    %setdsp($06,$11) ; S=0 R=0
    %setdsp($07,$cf) ; GAIN=$1F (ignore ADSR)

    %setdsp($5c,$00) ; key-off (mute) all voices
    %setdsp($3d,$00) ; noise off all voices
    %setdsp($4d,$01) ; enable echo voice 0
    %setdsp($0c,$57) ; master vol L max
    %setdsp($1c,$57) ; master vol R max

    rts

BitValue8:
    db $01,$02,$04,$08,$10,$20,$40,$80

WaitMsg:
    ; "WAITING FOR SPC"
    db 13+23,13+01,13+09,13+20,13+09,13+14,13+07,0,13+06,13+15,13+18,0,13+19,13+16,13+03

incsrc "src/bcdlib.asm"
incsrc "src/sincos.asm"
incsrc "src/charset.asm"
incsrc "src/palette.asm"

incsrc "src/spcdriver.asm"

org $018000
    ;incbin "Audio/candy.brr" -> $018000
    ;incbin "Audio/top-dream.brr" -> $018000
    incbin "Audio/yururenai.brr" -> $018000
    ;incbin "Audio/hurricane_1.brr" -> $018000
;sa1rom 4,5,6,7
;org $008000
;    incbin "Audio/hurricane_2.brr" -> $008000
