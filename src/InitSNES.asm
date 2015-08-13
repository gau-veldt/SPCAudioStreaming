InitPPU:
   sep #$20
   lda #$80
   sta $2100
   stz $2101
   stz $2102
   stz $2103
   stz $2104
   stz $2105
   stz $2106
   stz $2107
   stz $2108
   stz $2109
   stz $210a
   stz $210b
   stz $210c
   stz $210d
   stz $210d
   stz $210e
   stz $210e
   stz $210f
   stz $210f
   stz $2110
   stz $2110
   stz $2111
   stz $2111
   stz $2112
   stz $2112
   stz $2113
   stz $2113
   stz $2114
   stz $2114
   lda #$80
   sta $2115
   stz $2116
   stz $2117
   stz $211a
   stz $211b
   lda #$01
   sta $211b
   stz $211c
   stz $211c
   stz $211d
   stz $211d
   stz $211e
   lda #$01
   sta $211e
   stz $211f
   stz $211f
   stz $2120
   stz $2120
   stz $2121
   lda #$00
   ldx #$2123           ;regs $2123-$2133
   -   stz $00,X	;turn off windows, main screens, sub screens, color addition,
       inx	        ;fixed color = $00, no super-impose (external synchronization),
       cpx #$2134	;no interlaced mode, normal resolution
   bne -
   stz $4200
   lda #$ff
   sta $4201
   stz $4202
   stz $4203
   stz $4204
   stz $4205
   stz $4206
   stz $4207
   stz $4208
   stz $4209
   stz $420a
   stz $420b
   stz $420c
   stz $420d
   rts

ClearVRAM:
    ; NB: must call during vblank or fblank
    ; preserved: X,Y
    phx
    stz temp : stz temp+1
    lda #$80     : sta $2115    ; port mode: vram=vram+1
    stz $2116    : stz $2117    ; port = vram:$0000
    %DMAc0(8+1,$2118,temp,$10000)
    plx
    rts
