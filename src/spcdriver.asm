SPCDriver:
arch spc700-inline

org $0400

TOSNES0 = $f4
TOSNES1 = $f5
TOSNES2 = $f6
TOSNES3 = $f7

SAMPLEDIR = $0200
STBUF     = $3728
STBUFSIZE = $48d8   ; 2072 brr chunks, 28 frames (666 bytes/frame)
STLASTSLC = $7d66   ; STBUF+STBUFSIZE-666
STLASTBRR = $7ff7   ; STBUF+STBUFSIZE-9
STBUFTOP  = $8000   ; STBUF+STBUFSIZE
tempPtr   = $00
tempW1    = $02
tempW2    = $04
tempW3    = $06
secCountW = $08     ; ticks / 50
tickCount = $0a     ; ticks % 50
bufHalfs  = $0b
bufTODO   = $0c
brrHdrMsk = $0d
bufWrPtr  = $0e
bufWrPtr1 = $10
bufWrPtr2 = $12
bufWrPtr3 = $14
bufWrPtr4 = $16
bufWrPtr5 = $18
bufWrPtr6 = $1a
bufWrPtr7 = $1c
bufWrPtr8 = $1e

; macro implements a "movw immediate"
macro movwi(mem,word)
    push a
    mov  a,#<word>&$ff
    mov  <mem>,a
    mov  a,#<word>>>8
    mov  <mem>+1,a
    pop  a
endmacro

; move 16-bit memory to memory
macro movwabs(memd,mems)
    mov <memd>,<mems>
    mov <memd>+1,<mems>+1
endmacro

; macro implements a "movw ya,#immediate"
macro i2ya(word)
    mov a,#<word>&$ff
    mov y,#<word>>>8
endmacro

; 16-bit move ya to word at 16-bit address
macro ya2abs(mem)
    mov <mem>,a
    mov <mem>+1,y
endmacro

; immediate to dsp
macro movdsp(reg,val)
    mov a,#<reg>
    mov $f2,a
    mov a,#<val>
    mov $f3,a
endmacro

    ; set sample directory index 0
    ; to be the stream buffer
    %i2ya(STBUF)        ; movw ya,#STBUF
    %ya2abs(SAMPLEDIR)
    %ya2abs(SAMPLEDIR+2)
        
    ; zero out the streaming buffer
    %movwi(tempW1,STLASTBRR)
    %movwi(tempW2,STBUFTOP)
    %movwi(tempW3,1)
    %i2ya(STBUF)
    movw tempPtr,ya

    -   mov x,#$00
        cmpw ya,tempW1  ; cmpw ya,#STLASTBRR
        bne +
            ; if on last brr chunk write loop+end flags
            mov x,#$03
        +
        push y
        push a
        mov  a,x
        mov  y,#0
        mov  (tempPtr)+y,a
        pop  a
        pop  y
        movw ya,tempPtr
        clrc
        addw ya,tempW3  ; addw ya,#$0001
        movw tempPtr,ya
        cmpw ya,tempW2  ; cmpw ya,#STBUFTOP
    bne -
    
    ; reset voice
    call spcResetVox

    %movdsp($2c,$00)  ; echo L vol=0
    %movdsp($3c,$00)  ; echo R vol=0
    %movdsp($6c,$00)  ; echo on

    ; set up frame timer (8kHz timer)
    ; counts passing of half the 74-chunk frame in time
    mov a,#160
    mov $fb,a       ; $FE counts at 50 fps
    mov a,#148
    mov $fa,a       ; $FD counts every 37 BRRs (32 kHz)
    mov a,#1+2
    mov $f1,a
    mov a,$fd
    mov a,$fe       ; clear counters
    
    ; voice 0 on
    %movdsp($4c,$01)

    ; wait 32 frames for the echo to stabilize
    mov x,#32*2
    -   mov a,$fd
        beq -
        dec x
        bne -
    ; enable echo volume and feedback
    %movdsp($2c,$40)                ; set echo vol L
    %movdsp($3c,$40)                ; set echo vol R
    %movdsp($0d,$50)                ; echo feedback
    %movdsp($00,$7f)                ; V0 L VOL=max
    %movdsp($01,$7f)                ; V0 R VOL=max
    ; set buffer write pointer 
    %movwi(bufWrPtr,STBUF+(16*666))

    ; initialize counters
    mov a,#0
    mov bufTODO,a
    mov bufHalfs,a
    mov tickCount,a
    mov secCountW,a
    mov secCountW+1,a
    ; signal S-CPU we are ready
    mov a,#$33
    mov $f4,a
    mov a,#$77
    mov $f5,a
    ; wait for S-CPU acknowledge
    -   mov a,$f4
        cmp a,#$73
    bne -
    ; reset timer counters
    mov a,$fd
    mov a,$fe
    
    spcMainLoop:
        mov a,$fd
        beq +
            ; count half frames (quarter @16 kHz)
            clrc
            adc  a,bufHalfs
            mov  bufHalfs,a
;            --  cmp  a,#2 : bcc  +              ; /2 for 32 kHz
            --  cmp  a,#4 : bcc  +              ; /4 for 16 kHz
                dec bufHalfs : dec bufHalfs
                dec bufHalfs : dec bufHalfs
                inc bufTODO
                mov  a,bufHalfs
            bne --
        +
        mov a,$fe
        beq +
            ; count ticks
            clrc
            adc a,tickCount
            mov tickCount,a
            cmp  a,#50 : bcc +
            --  incw secCountW
                mov  a,tickCount
                setc
                sbc  a,#50
                mov  tickCount,a
                cmp  a,#50
            bcs --
        +
        mov $f7,bufTODO
        mov $f2,#$09
        mov $f6,$f3
        mov $f5,secCountW
        ; check for transfer frame command from SNES
        TransferCmd:
            mov a,$f7 : cmp a,#$ff : bne +              ; f7 (2143)==ff
            mov a,$f6 : cmp a,#$ff : bne +              ; f6 (2142)==ff
            mov a,$f5 : cmp a,#$ff : bne +              ; f5 (2141)==ff
            mov a,$f4 : cmp a,#$ff : beq .y : + jmp .n  ; f4 (2140)==ff
            .y
                ; got the sentinel
                mov  $f4,#0                         ; acknowledge
                %movwi(tempW2,1)
                %movwi(tempW3,254)
                clrc
                movw ya,bufWrPtr                    ;    bufWrPtr
                addw ya,tempW2 : movw bufWrPtr1,ya  ; S1=bufWrPtr+$001
                addw ya,tempW2 : movw bufWrPtr2,ya  ; S2=bufWrPtr+$002
                addw ya,tempW3 : movw bufWrPtr3,ya  ; S3=bufWrPtr+$100
                addw ya,tempW2 : movw bufWrPtr4,ya  ; S4=bufWrPtr+$101
                addw ya,tempW2 : movw bufWrPtr5,ya  ; S5=bufWrPtr+$102
                addw ya,tempW3 : movw bufWrPtr6,ya  ; S6=bufWrPtr+$200
                addw ya,tempW2 : movw bufWrPtr7,ya  ; S4=bufWrPtr+$201
                addw ya,tempW2 : movw bufWrPtr8,ya  ; S5=bufWrPtr+$202
                mov  x,#1                           ; transfer loop counter
                mov  y,#0                           ; index for writing to buffer
                %movwi(tempW1,666)                  ; slice size
                mov  brrHdrMsk,#0                   ; 0 for normal
                cmp  bufWrPtr+1,#STLASTSLC>>8       ; check if this transfer is going
                bne .xfer                           ; to the ring buffer's
                cmp  bufWrPtr,#STLASTSLC&$ff        ; last slice
                bne .xfer
                    mov brrHdrMsk,#3                ; sets LOOP+END bits
                .xfer
                -   -- cmp x,$f7 : bne --           ; wait for nonce
                    mov  a,$f4                      ; get first byte
                    mov  (bufWrPtr)+y,a             ; store 1st byte
                    mov   a,$f5
                    mov  (bufWrPtr1)+y,a            ; store 2nd byte
                    mov   a,$f6
                    mov  (bufWrPtr2)+y,a            ; store 3rd byte
                    mov   $f4,x                     ; acknowledge (ASAP)
                    inc y : inc y : inc y           ; y=y+3
                    inc   x                         ; ready for next write
                    cmp   y,#2                      ; y wrapped
                bne -
                ; x=087 y=002 (258)
                -   -- cmp x,$f7 : bne --           ; wait for nonce
                    mov  a,$f4                      ; get first byte
                    mov  (bufWrPtr3)+y,a            ; store 1st byte
                    mov   a,$f5
                    mov  (bufWrPtr4)+y,a            ; store 2nd byte
                    mov   a,$f6
                    mov  (bufWrPtr5)+y,a            ; store 3rd byte
                    mov   $f4,x                     ; acknowledge (ASAP)
                    inc y : inc y : inc y           ; y=y+3
                    inc   x                         ; ready for next write
                    cmp   y,#1                      ; y wrapped
                bne -
                ; x=172 y=001 (513)
                -   -- cmp x,$f7 : bne --           ; wait for nonce
                    mov  a,$f4                      ; get first byte
                    mov  (bufWrPtr6)+y,a            ; store 1st byte
                    mov   a,$f5
                    mov  (bufWrPtr7)+y,a            ; store 2nd byte
                    mov   a,$f6
                    mov  (bufWrPtr8)+y,a            ; store 3rd byte
                    mov   $f4,x                     ; acknowledge (ASAP)
                    inc y : inc y : inc y           ; y=y+3
                    inc   x                         ; ready for next write
                    cmp   x,#220                    ; got to last brr of slice/buffer
                bne -
                ; x=220 y=145 (657)
                ; unroll the iteration with final brr headerbyte
                -- cmp x,$f7 : bne --               ; wait for nonce (x=223)
                mov  a,$f4                          ; get first byte
                or   a,brrHdrMsk                    ; sets LOOP+END on correct slice
                mov  (bufWrPtr6)+y,a                ; store 1st byte
                mov   a,$f5
                mov  (bufWrPtr7)+y,a                ; store 2nd byte
                mov   a,$f6
                mov  (bufWrPtr8)+y,a                ; store 3rd byte
                mov   $f4,x                         ; acknowledge (ASAP)
                inc y : inc y : inc y               ; y=y+3
                inc   x                             ; ready for next write
                ; x=221 y=148 (660)
                -   -- cmp x,$f7 : bne --           ; wait for nonce
                    mov  a,$f4                      ; get first byte
                    mov  (bufWrPtr6)+y,a            ; store 1st byte
                    mov   a,$f5
                    mov  (bufWrPtr7)+y,a            ; store 2nd byte
                    mov   a,$f6
                    mov  (bufWrPtr8)+y,a            ; store 3rd byte
                    mov   $f4,x                     ; acknowledge
                    inc y : inc y : inc y           ; y=y+3
                    inc   x                         ; ready for next write
                    cmp   x,#223                    ; check for done
                bne -
                ; x=223 y=154 (666)
                clrc : movw ya,bufWrPtr
                addw ya,tempW1 : movw bufWrPtr,ya   ; bufWrPtr+=666
                cmp bufWrPtr+1,#STBUFTOP>>8         ; if buffer is at end rewind
                bne ++
                cmp bufWrPtr,#STBUFTOP&$ff          ; bufWrPtr == STBUFTOP
                bne ++
                    %movwi(bufWrPtr,STBUF)
                ++
                dec  bufTODO                        ; decrement TODO count
            .n
        EndOfCommands:
    jmp spcMainLoop
    
spcResetVox:
    %movdsp($6c,$20) ; unmuted, echo off
    %movdsp($4c,$00) ; clear key-on all voices
    %movdsp($5c,$ff) ; key-off (mute) all voices
    %movdsp($5d,$02) ; set sample directory to $0200
    %movdsp($6d,$80) ; echo buffer at $8000
    %movdsp($7d,$08) ; max echo delay

    %movdsp($0f,$7f)
    %movdsp($1f,$00)
    %movdsp($2f,$00)
    %movdsp($3f,$00)
    %movdsp($4f,$00)
    %movdsp($5f,$00)
    %movdsp($6f,$00)
    %movdsp($7f,$00) ; echo FIR coefficients 127,0,0,0,0,0,0,0

    %movdsp($00,$00) ; V0 L VOL=max
    %movdsp($01,$00) ; V0 R VOL=max
;    %movdsp($02,$00) ; PITCH L
;    %movdsp($03,$10) ;   ... H -> $1000 (32 kHz)
    %movdsp($02,$00) ; PITCH L
    %movdsp($03,$08) ;   ... H -> $0800 (16 kHz)
    %movdsp($04,$00) ; Sample #0
    %movdsp($05,$00) ; A=0 D=0
    %movdsp($06,$00) ; S=0 R=0
    %movdsp($07,$7f) ; GAIN=$7F (full, ignore ADSR)

    %movdsp($5c,$00) ; key-off (mute) all voices
    %movdsp($3d,$00) ; noise off all voices
    %movdsp($4d,$01) ; enable echo voice 0
    %movdsp($0c,$7f) ; master vol L max
    %movdsp($1c,$7f) ; master vol R max
    ret

arch 65816
