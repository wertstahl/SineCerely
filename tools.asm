;------ Gfxtools --------------------------------------------------- CBMprgStudio 3.9.0
;
;       (c) wertstahl 02/2013 #it is a beautiful day, and then you die.
;
;       finished on 2.1.2017  #anonymous downvoters have small testicles 
;
;--------------------------------------------------------------------------------------

;-------------------------
; BLENDIE uses ZP f9-ff

;----------- screen init

tools       jsr   mk_speedclear

            jsr   cls

            jsr   scrcol

            jsr   blendie

            jsr   blendifont

            rts
            
;-----------------------------------------------------

screen_col  = #$00
border_col  = #$0b

scrcol      lda   border_col
            sta   $d020    
            lda   screen_col   
            sta   $d021       
            rts


;-----------------------------------------------------

clscol      = #$00
clschar     = #$00

cls         ldy   #$00        ; clear screen & color

clr         lda   clschar
            sta   $0400,y
            sta   $0500,y
            sta   $0600,y
            sta   $06e8,y

            iny
            cpy   #$00
            bne   clr

            lda   clscol      
            jsr   quickd8kill

            rts

;-----------------------------------------------------

;+$28

plus0x28
            byte $28,$29,$2a,$2b,$2c,$2d,$2e,$2f,$30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,$40,$41,$42,$43,$44
            byte $45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,$50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$5b,$5c,$5d,$5e,$5f,$60,$61
            byte $62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,$70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d,$7e
            byte $7f,$80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f,$90,$91,$92,$93,$94,$95,$96,$97,$98,$99,$9a,$9b
            byte $9c,$9d,$9e,$9f,$a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,$b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8
            byte $b9,$ba,$bb,$bc,$bd,$be,$bf,$c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,$d0,$d1,$d2,$d3,$d4,$d5
            byte $d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df,$e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef,$f0,$f1,$f2
            byte $f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff,$00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f
            byte $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,$20,$21,$22,$23,$24,$25,$26,$27


;-----------------------------------------------------

sprspread = #$30

spriteup    lda #$be
            sta $07f8
            sta $07f9
            sta $07fa
            sta $07fb
            sta $07fc
            sta $07fe
            sta $07ff
            lda #$bd
            sta $07fd

            clc
            lda #$19
            sta $d000
            adc sprspread
            sta $d002
            adc sprspread
            sta $d004
            adc sprspread
            sta $d006
            adc sprspread
            sta $d008
            adc sprspread
            sta $d00c
            adc sprspread
            sta $d00e
            dec $d00e

            lda #$e0
            sta $d010 ;sprite x-hi

            lda #$00
            sta $d001
            sta $d003
            sta $d005
            sta $d007
            sta $d009
            sta $d00d
            sta $d00f

            lda #$ff
            sta $d015 ;enable
            lda #$df
            sta $d017 ;x stretch
            sta $d01d ;y stretch

            lda #$00
            sta $d01c ;multicolor
            sta $d01b ;before gfx
            
            lda #$00
            sta $d027
            sta $d028
            sta $d029
            sta $d02a
            sta $d02b
            sta $d02d
            sta $d02e

            rts

;-----------------------------------------------------

blendifont  lda $DD00
            and #%11111100
            ora #%00000011 ;<- your desired VIC bank value
            sta $DD00

            lda #%00011100 ;screenmem at $0400, font at $3000
            sta $d018       
            rts            

; ------------------------blendpaint

optom       byte $10

blendie     ldx #$00
            lda #<blending          ;adresse der blending-tabelle lo
            sta $fe             ;
            lda #>blending          ;adresse der blending-tabelle hi
            sta $ff

            lda #$50
            sta $fc
            lda #$04
            sta $fd

            ldx #$16 ;24 lines on screen

s_o_mirrcop lda #$13 ;<<<------
            sta $fa
            lda #$14 ;------>>>
            sta $fb
                                
            ldy #$00
            sty $f9

mirrorcopy  
            ldy $f9

            ;------------ woah

            lda $d012
            and optom
            adc $dc09
            adc ($fe),y

            ;------------

            ldy $fa
            sta ($fc),y
            ldy $fb
            sta ($fc),y
                                    
            inc $f9
            inc $fb
            dec $fa
            bpl mirrorcopy
                                    
            dex
            bmi exit

            lda $fc
            clc
            adc #$28
            bcc nohiup1
            inc $fd
nohiup1     sta $fc

            lda $fe
            clc
            adc #$15
            bcc nohiup2
            inc $ff
nohiup2     sta $fe
            jmp s_o_mirrcop

exit        rts

;-----------------------------------------------------

            
mk_speedclear
            ldx #$00
copycode    lda codeline,x
trgetadr    sta codebuffer,x
            inx
            cpx #$03
            bne copycode
            
            ;------

            clc
            lda trgetadr+1
            adc #$03
re_insert   bcc no_u2
            inc trgetadr+2
no_u2       sta trgetadr+1

            ;-------

            inc codeline+1
            lda codeline+1
            cmp #$e8
            beq endcheck

            cmp #$80
            beq intercheck

            cmp #$00
            bne mk_speedclear

            ;-------

            inc codeline+2
            jmp mk_speedclear

intercheck  lda codeline+2
            cmp #$da
            beq inter_break

            jmp mk_speedclear

            ;-------

endcheck    lda codeline+2
            cmp #$db
            bne mk_speedclear

            rts

            ;--------

inter_break lda trgetadr+1
            sta here1+1
            lda trgetadr+2
            sta here1+2

            lda #$60
here1       sta $1234

            clc
            lda trgetadr+1
            adc #$01
            bcc no_u3
            inc trgetadr+2
no_u3       sta trgetadr+1
            jmp mk_speedclear

            ;-----------------

codeline    sta $d800
            byte $60

;------------------------------------------------------            

quickd8kill
            
codebuffer

incbin      "codebuffer.bin"

;------------------------------------------------------

            ;-----------------------------------------------------------------------------
            ;temporary tables and counter for some onscreen fx

colout      byte 0
pointox     byte 0
flicka      byte $b,$b,$b,$b,$b,$c,$b,$5,$b,$c,$b,$c,$b,$c,$b,$c,$b,$c,$b,$c,$b,$c,$b,$c,$b,$5,$b,$c,$b,$c,$b,$c ;32
            byte $b,$b,$c,$b,$c,$b,$5,$b,$c,$b,$b,$b,$b,$5,$b,$b,$b,$b,$b,$c,$b,$5,$b,$b,$b,$b,$b,$b,$c,$b,$5,$b ;64
            byte $c,$b,$b,$b,$b,$b,$b,$5,$b,$c,$b,$c,$b,$5,$b,$c,$b,$c,$b,$c,$b,$5,$b,$c,$b,$c,$b,$5,$b,$5,$b,$5 ;
            byte $b,$c,$b,$c,$b,$5,$3,$d,$3,$5,$b,$b,$c,$b,$c,$b,$c,$b,$b,$b,$b,$b,$b,$c,$b,$5,$b,$c,$b,$c,$b,$c ;128
            byte $b,$c,$b,$c,$b,$c,$b,$5,$b,$c,$b,$c,$b,$5,$b,$c,$b,$c,$b,$c,$b,$5,$b,$b,$b,$b,$b,$b,$c,$b,$5,$b ;
            byte $5,$b,$b,$b,$b,$b,$9,$9,$5,$5,$3,$3,$7,$7,$d,$d,$1,$1,$3,$3,$5,$5,$b,$b,$0,$0,$0,$0,$0,$0,$0,$0 ;
            byte $b,$b,$5,$b,$5,$b,$5,$b,$5,$b,$b,$b,$b,$c,$b,$b,$b,$b,$b,$5,$b,$c,$b,$b,$b,$b,$b,$b,$c,$b,$c,$b ;
            byte $c,$7,$5,$d,$5,$b,$0,$0,$0,$0,$0,$0,$b,$c,$b,$c,$b,$c,$b,$c,$b,$c,$b,$5,$b,$c,$b,$c,$b,$c,$b,$0 ;

;-------------------------------------------------------------------------------------

            ;-----------------------------------------------------------------------------
            ;scrolltext

lyrics      text  "                                          achtung!                                   "
            text  "... version 2.0 - now with the actual events!                    . . . . .             " 
            text  " just a color aliased blocky scroll with a rohrschachishly broken ditherism .... and it is           "
            text  " ... to say                i love you - dear c64 ....  really, most of what lately was released leaves me"
            text  " just sitting here with my jaw dropped.... mouth is getting dry, and i know - i need a very stiff drink,"
            text  " once more!  "
            text  " it is always a damn challenge to create something visually appealing for the c64.      "
            text  " greetings to all the wizards and alchemists in the scene, "
            text  " we might not always agree, but the demoforce is with us!   "
            text  "       big shoutouts to all my cracker fellows: keep digging!       "
            text  " and thank you for your really soothing welcome back in 2012 and all the pleasant vibes....        "
            text  " wertstahl of genesis project sends his best byte:       good byte!                                        "
            text  " greetings to all the nice peeps in genesis project."
            text  " special triple heart to hedning - let it crack! or better let me crack!"
            text  " this previously was a new years eve release of the last hours of 2016"
            text  " - until i fixed it - so, just get the booze."
            text  " credits: code, gfx, music by wertstahl.   check out my album"
            text  " megademon by psaux and all the wertstahl and fleischdolls releases!        "
            text  " ws signing off .. thanks for watching!                               "
            text  "           ..-.-.-...-.-.-. . .   .                      "

            byte  $ff               ;end of text

;-----------------------------------------------------

*=$2d00
            ; this contains half a blended round screen and is used by blendie for making a whole screen.
            ; it has been cropped by 3 lines

blending    byte    $0d,$0d,$0d,$0e,$0f,$10,$11,$12,$13,$14,$16,$17,$17,$18,$18,$18,$18,$19,$19,$19,$19
            byte    $0c,$0c,$0c,$0c,$0d,$0e,$0f,$10,$11,$12,$14,$15,$16,$17,$17,$18,$18,$18,$18,$19,$19
            byte    $0b,$0b,$0b,$0b,$0c,$0d,$0e,$0e,$0f,$10,$12,$13,$14,$15,$16,$17,$17,$18,$18,$18,$19
            byte    $0a,$0a,$0a,$0a,$0b,$0c,$0c,$0d,$0e,$0f,$10,$11,$13,$14,$15,$16,$17,$17,$18,$18,$18
            byte    $08,$08,$08,$08,$09,$0a,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$13,$14,$15,$16,$17,$17,$18
            byte    $07,$07,$07,$07,$08,$09,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$12,$13,$14,$15,$17,$17,$18
            byte    $06,$06,$06,$07,$07,$08,$08,$09,$0a,$0b,$0c,$0e,$0f,$10,$11,$12,$14,$15,$16,$17,$17
            byte    $05,$05,$06,$06,$06,$07,$07,$08,$09,$0a,$0b,$0c,$0e,$0f,$10,$11,$13,$14,$15,$17,$17
            byte    $04,$04,$05,$05,$06,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0f,$10,$11,$12,$13,$14,$16,$17
            byte    $03,$03,$04,$04,$05,$06,$07,$07,$08,$09,$0a,$0b,$0c,$0e,$0f,$10,$11,$12,$13,$15,$17
            byte    $02,$02,$03,$04,$05,$06,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$14,$15
            byte    $02,$02,$03,$04,$05,$06,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$12,$14,$15
            byte    $03,$03,$04,$04,$05,$06,$07,$07,$08,$09,$0a,$0b,$0c,$0e,$0f,$10,$11,$12,$13,$15,$17
            byte    $04,$04,$05,$05,$06,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0f,$10,$11,$12,$13,$14,$16,$17
            byte    $05,$05,$06,$06,$06,$07,$07,$08,$09,$0a,$0b,$0c,$0e,$0f,$10,$11,$13,$14,$15,$17,$17
            byte    $06,$06,$06,$07,$07,$08,$08,$09,$0a,$0b,$0c,$0e,$0f,$10,$11,$12,$14,$15,$16,$17,$17
            byte    $07,$07,$07,$07,$08,$09,$09,$0a,$0b,$0c,$0d,$0e,$0f,$10,$12,$13,$14,$15,$17,$17,$18
            byte    $08,$08,$08,$08,$09,$0a,$0a,$0b,$0c,$0d,$0e,$0f,$10,$11,$13,$14,$15,$16,$17,$17,$18
            byte    $0a,$0a,$0a,$0a,$0b,$0c,$0c,$0d,$0e,$0f,$10,$11,$13,$14,$15,$16,$17,$17,$18,$18,$18
            byte    $0b,$0b,$0b,$0b,$0c,$0d,$0e,$0e,$0f,$10,$12,$13,$14,$15,$16,$17,$17,$18,$18,$18,$19
            byte    $0c,$0c,$0c,$0c,$0d,$0e,$0f,$10,$11,$12,$14,$15,$16,$17,$17,$18,$18,$18,$18,$19,$19
            byte    $0d,$0d,$0d,$0e,$0f,$10,$11,$12,$13,$14,$16,$17,$17,$18,$18,$18,$18,$19,$19,$19,$19
            byte    $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

;----------------------------------------------------

            ;------------------------------------------------------------
            ; logo and shadingsprites

*=$2f40 
cola
incbin      "gpsprite.bin"

*=$2f80 
fanta
incbin      "shadesprite_down.bin"

*=$2fc0     
sprite
incbin      "shadesprite.bin"

;----------------------------------------------------

            ;------------------------------------------------------------
            ; this dither works best with the blended screen from blendie.

*=$3000
ditta       
incbin      "dithernew.bin"
incbin      "dithernew.bin"
incbin      "dithernew.bin"
incbin      "dithernew.bin"
incbin      "dithernew.bin"

;----------------------------------------------------

            ;------------------------------------------------------------
            ; music

*=$3800
music
incbin      "sine_cerely.bin" ;Music was written with Goattracker on Dec.29.2016 from 12AM to 6PM - I screamed in anger three times at least.

;----------------------------------------------------

            ;------------------------------------------------------------
            ; exit program after pressing space

*=$3fc0     
wrapup
            sei
            lda #$00
            sta $d011
            lda $0800
            sta $3fff
            jsr cls
            lda #$37
            sta $01
            jmp $Fce2

;-------------------------------------------------
;-------------------------------------------------
;-------------------------------------------------
;EOF