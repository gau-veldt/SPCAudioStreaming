macro MC8()
    sep #$20
endmacro

macro XY8()
    sep #$10
endmacro

macro MC16()
    rep #$20
endmacro

macro XY16()
    rep #$10
endmacro

macro MC8XY16()
    %MC8() : %XY16()
endmacro

macro MC16XY8()
    %MC16() : %XY8()
endmacro

macro MC8XY8()
    sep #$30
endmacro

macro MC16XY16()
    rep #$30
endmacro

macro DMAc0(mode,reg,addr,size)
    lda.b #<mode>       : sta $4300
    lda.b #<reg>&$ff    : sta $4301
    ldx.w #<addr>       : stx $4302
    lda.b #<addr>>>16   : sta $4304
    ldx.w #<size>&$ffff : stx $4305
    lda.b #$01          : sta $420b
endmacro

macro txy()
    phx : ply
endmacro

macro tyx()
    phy : plx
endmacro

macro tcb()
    pha : plb
endmacro

macro tbc()
    phb : pla
endmacro
