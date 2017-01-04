;------ MAIN ------------------------------------------------------- CBMprgStudio 3.9.0
;
;       This is a demoscene product. Continue with caution - strong language, motherfucker!
;
;       Programming Language: Commodore C=64 MOS 6510 Assembly
;
;       Development IDE: CBM Prg Studio 3.9.0 by Arthur Jordison (Freeware)
;
;       Documentation Language: English (and partially German)
;
;
;       License CC Attribution: No parts of this source code may be used in 
;       commercial productions, -documentations, -collections in any form that
;       requires paying a fee. Reproduction in motion pictures or printed form
;       is not allowed without prior written permission of the author of the work.
;       Author and owner of the copyrights of the source code and comments
;       is Sebastian I. Hartmann aka Wertstahl (www.wertstahl.com / battlecommand.org) 
;       Author contact: info@battlecommand.org
;
;       If you use parts of this code in your non-commercial production, please give
;       proper credit.
;
;       This source code may NOT be used in schools/universities.
;
;
;       Binary release: http://csdb.dk/release/?id=152528
;
;
;       (c) wertstahl 06/2014 | "it is a beautiful day, and then you die."
;
;       finished on 2.1.2017 | anonymous downvoters have small testicles 
;
;
;       This was actually called "Spontabounce" and existed as a scribble after i
;       finished coding the "Alienintro" http://csdb.dk/release/?id=127588
;
;       The original scrolltext where i stated that i was ready to quit coding on the
;       C=64 was written at that time. The effort is really in no sane relation to the
;       result, so after this ICC partiticipation, and having to deal with people 
;       who have absolutely no idea about coding and who never contributed shit to 
;       the scene except their mental illness, i think i won't do it again.
;
;------ BASIC - bootloader ------------------------------------------------------------

*=$0801
            byte  $0c,$08,$01,$00,$9e,$20,$32,$30,$36,$32,$00,$00,$00; 0 sys2062 ($080e)

*=$080e
            jmp   appstart

;-------------------------------------------------------------------------------------

appstart    cld
            sei

            lda   #$01
            sta   $d01a
            sta   $dc0d

            lda   #$35
            sta   $01

            lda   #$00         ; close all curtains
            sta   $d011

            sta   $dc0e        ; Set TOD Clock Frequency to 60Hz
            sta   $dc0f        ; Enable Set-TOD-Clock
            sta   $dc0b        ; Set TOD-Clock to 0 (hours)
            sta   $dc0a        ; - (minutes)
            sta   $dc09        ; - (seconds)
            sta   $dc08        ; - (deciseconds)

            jsr   spriteup

            jsr   tools

            jsr   makeascii
            jsr   sinit  

            lda   #$00
            jsr   music

            lda   #$1b
            sta   $d011     

            lda   #$c0
            sta   $d016

            lda   $3fff
            sta   $0800
            lda   #<mainloop
            sta   $FFFE
            ldy   #>mainloop
            sty   $FFFF

            lda   #$ff        ; ghostbite
            sta   $3fff  

            lda   firstscan   ; first interrupt line
            sta   $d012
            lda   #$7f
            sta   $dc0d
            lda   $dc0d       
            
            cli

idle        jsr   blendie     ; print screen background here

            lda   $dc01
            cmp   #$ff        ; check space pressed
            beq   idle

            jmp   wrapup


;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================
;--------------------------------------------------------------------------------------------------------------------

*=$08a0

firstscan    = #$88   
secondscan   = #$d4
noborderline = #$f9


mainloop    pha
            txa
            pha
            tya
            pha

            inc   $d019
            lda   $dc0d

vwait       lda   $d012
            cmp   firstscan
            beq   interrupt1
            cmp   secondscan
            beq   interrupt2
            cmp   noborderline
            beq   interrupt3

reentry     pla
            tay
            pla
            tax
            pla
            rti

            ;-----------------

interrupt1  jmp   inter1
interrupt2  jmp   inter2
interrupt3  jmp   inter3


;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================
;--------------------------------------------------------------------------------------------------------------------

inter1      ;------------------------------------------
            ; color flicker to make the picture more alive.

            ldx   pointox     ; counter
            lda   flicka,x        
            sta   colout             
            inc   pointox     ; flicka is now 256 bytes long

            ;------------------------------------------
            ; quickd8kill

            lda   #$00                ; color to fill the screen with
            jsr   quickd8kill+240     ; upper 3 lines untouched, skip

            ;-----------------------
            ; put spritey into lower border

            lda #$be
            sta $07f8
            sta $07f9
            sta $07fa
            sta $07fb
            sta $07fc
            sta $07fe
            sta $07ff

            lda #$d9
            sta $d001
            sta $d003
            sta $d005
            sta $d007
            sta $d009
            sta $d00d
            sta $d00f


            ;-----------------------
            ;gpsprite

            lda yoffsets             ; bewegtes sitzen
            lsr 
            lsr
            lsr
            lsr
            lsr
            clc
            adc #$2b
            sta $d00a

            lda yoffsets+20          ; bewegtes sitzen
            lsr
            lsr
            lsr
            lsr
            clc
            adc #$50
            sta $d00b

            lda colout               ; clone flicker
            sta $d02c

            ;----------------
            ; musicplay

            jsr   music+3


            ;----------------
            ; get some controlbytes from music

timere      lda   #$30
            dec   timere+1
            bne   noupdate
            lda   #$30
            sta   timere+1

            lda   $3af0
            sta   optom

noupdate

            ;-------

            lda   secondscan
            sta   $d012
            jmp   reentry

;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================
;--------------------------------------------------------------------------------------------------------------------

inter2      
            ;-------------------
            ; quickD8kill

            lda   #$00                ; color to fill the screen with
            jsr   quickd8kill+1921    ; part 1 kills d800-d9ff // part 2 kills da00-dbe0

            ;-------
      
            lda   noborderline      
            sta   $d012
            jmp   reentry

;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================
;--------------------------------------------------------------------------------------------------------------------

inter3      ;-------------
            ; no border

            ;------------------------------------------
            ; Make sure there'll be no garbage in the open borders.

            lda   #$ff
            sta   $3fff

            lda   colout      ;flicker!     
            sta   $d021 

            ; Trick the VIC and open the border!!
            lda   $d011
            and   #$f7
            sta   $d011

            ; Wait until scanline 255
            lda   #$fe
vwt_bkill   cmp   $d012
            bne   vwt_bkill

            ; Reset bit 3 for the next frame
            lda   $d011
            ora   #$08
            sta   $d011

            ;-----------------------
            ; put spritey into upper border


            lda   #$bf
            sta   $07f8
            sta   $07f9
            sta   $07fa
            sta   $07fb
            sta   $07fc
            sta   $07fe
            sta   $07ff

            lda   #$42
            sta   $d001
            sta   $d003
            sta   $d005
            sta   $d007
            sta   $d009
            sta   $d00d
            sta   $d00f
 
            ;-----------------
            ;sinepaint

            jsr   sinpaint

            ;----------------
            ;txplot

            jsr   txplot

            ;-----------------

            lda   firstscan        
            sta   $d012

            jmp   reentry
                

;--------------------------------------------------------------------------------------------------------------------
;====================================================================================================================
;--------------------------------------------------------------------------------------------------------------------


;-------------------------------------------------

incasm      "sinescrollV2.asm"
            
incasm      "tools.asm"

;-------------------------------------------------
;-------------------------------------------------
;-------------------------------------------------
;EOF
