;------ SINESCROLL - D8 Version ------------------------------------ CBMprgStudio 3.9.0
;
;       (c) wertstahl 02/2013 #it is a beautiful day, and then you die.
;
;       finished on 2.1.2017  #anonymous downvoters have small testicles 
;
;--------------------------------------------------------------------------------------
;
;     INIT:
;     jsr sinit
;     jsr makeascii           ;creates adress-table for chars
;
;     USAGE:
;     jsr sinpaint            ;paint the sine
;     jsr txplot              ;draw a new row of a scrolltextchar into the buffer (scrollolane) thats then being sined
;
;     IMPORTANT NOTE! THIS VERSION IS 2 CHARACTERS SMALLER IN X SIZE ON SCREEN FOR TIMING REASONS

*=$0a00

cha_lo_fetch = $1c            ;
cha_hi_fetch = $1d            ; hi-lo for to fetch kerned chars that have differnt size in mem

testval = #$ff
screen1hi = #$D8              ; screen position hi
screen2hi = #$04              ; only necessary for double buffering
screenslo = #$a1              ; screen position lo
screenxsize = #$26

ladder_px   = $1e
temp_px     = $1f             
                              
                              ; px_out = where in ram is the sinescroll painted
                              ; for px_out to be calculated, the value currpage and
                              ; must hold the the base-adress of the screen position

pixelcup    = $23             ; container for tile value
firstdup    = $24             ; duplicate of x position to read from scrollolane
totalsteps  = $25             ; general counter of how many rows to display
parent      = $26             ; pixel above indicator
parent2     = $27             ; pixel above indicator swap
rowdepth    = $28             ; pointer of which line in the row we process
wavestart   = $29             ; pointer for the y-displacement readout start
fdkeep      = $2a             ; y-position pointer for source read
rowmirr     = $2b             ; for using the dec-bne strategy
stepmirr    = $2c             ; dec-bne strategy mirror for the total steps

;-------- values that need to be externally set

currlow     = $2d             ; this value should hold the current screen adress low byte
currpage    = $2e             ; this value should hold the current screen adress hi-byte
sin_width   = $2f             ; width of the sine wave
sin_speed   = $30             ; advance-speed of the sine wave
screen_ready = $31
irq_feedback = $32

sin_reference = $33           ; sin-width, is covered
sin_timer   = $34             ; 00-$90
sin_alt_pnt = $35             ; $ff


                              ; 1. make new sinewave
                              ; 2. get current row pointer position that marks the beginning of the gfx in scrollolane
                              ; 2b. careful with the wrapping here!!!!
                              ;
                              ; 3. create y-displaced row by combining sintab value (from the start) and then
                              ;    descending into the row, analyzing if the prior pixel was filled and if the current
                              ;    pixel is filled and if not, add it´s displaced counter part
                              ; 4. loop for all 40 rows
                              ; 5. special: to not having to erase too much, always erase the topmost and lowest positions
                              ; 6. this will later run in a double buffer while the soft scrolling is active
                              ;    so we can propably use one full frame of screen time for calculations
                              ;    if we set the maximum possible scroll speed to 4 px
                              ;
                              ; scrollolane: into this area, the scroller is being painted and scrolled there
                              ; then we take the scroller from there and plot it in a sine to the screen


;-----------------------------------------------------------------

sinit       lda   screenslo     
            sta   currlow     
            lda   screen1hi
            sta   currpage    

            lda   #$00
            sta   wavestart  

            lda   #$02
            sta   sin_width       ;sine width

            rts

:-----------------------------------------------------------------

sinpaint    ;-----------------
            ; 1. update wave values

no_sin_alt  ldx   wavestart

            ldy   #$28
scrollsin   lda   yoffsets-1,y ;scroll sine values
            sta   yoffsets,y
            dey
            bne   scrollsin
            lda   256sine,x  
            sta   yoffsets

            lda   wavestart
            clc
            adc   sin_width
            sta   wavestart

            ;------------------
            ; setup things

            lda   #$00
            sta   totalsteps  ; reset the number of total rows to process

            lda   screenxsize
            sta   stepmirr    ; setup the mirror of total steps for the dec-bne strategy

            ldx   firstrow    ; load start of row to scan, GENERATED BY TXPLOT
            stx   firstdup    ; and copy to temporary pointer

            ;------------------

next_c_row  lda   #$00        
            sta   rowdepth    ; set topmost line of row for processing

            ;---- acquire displacement

            ldx   totalsteps  ; read the y displacement sine pointer
            ldy   yoffsets,x  ; fetch current displacement value (which also equals the char number to be printed on the screen)

            ldx   div16,y     ; calculate the hard pixel displacement for the entire row 0-f = 0 | f-1f = 1 and so on
                              ; = move high nybble to low nybble so we get a value range of 0,1,2,3...
                              ; ... which equals the number of lines to initially displace a row
                              ; /16 because we use double precision
                              ; = we could have 512 y values

            stx   ladder_px   ;this tells which color is to be picked from the table and generates the color blending effect
                              ;was formerly used to choose a y displaced char

            lda   mult40hi,x  ; and now we calc the hi byte of the y displacement adress on screen
            clc               ;
            adc   currpage    ;
            sta   px_out+2    ;

            lda   mult40lo,x  ; this calculates the lobyte base y-displacement on the screen
            clc
            adc   currlow     ; add to upper left corner
            bcc   nobchange   ; check if we
            clc 
            inc   px_out+2    ; have a blockchange
nobchange   sta   px_out+1    ; this is now the base value of the row that we´re going to paint

            ;-----cyclesavingdez2016------------------

            ldy   px_out+2
            adc   #$28
            bcc   nobbc1
            clc
            iny
nobbc1      sta   px_out2+1
            sty   px_out2+2

            adc   #$28
            bcc   nobbc2
            clc
            iny
nobbc2      sta   px_out3+1
            sty   px_out3+2

            adc   #$28
            bcc   nobbc3
            clc
            iny
nobbc3      sta   px_out4+1
            sty   px_out4+2

            adc   #$28
            bcc   nobbc4
           ; clc
            iny
nobbc4      sta   px_out5+1
            sty   px_out5+2

            ;--------------------------------------------
            ; process the row based on precalcs


            ldy totalsteps  ; how far on the screen in x direction have we gone?
                            ; -- optimized out of the inner loop on 18.10.2013
                            ; completely rewritten 29.12.16
            ldx firstdup    ; now we point to the pixels that we want to read
            stx fdkeep      ; and we go to the vertical position thats currently needed

            stx temp_px

            lda scrollolane,x
            beq rsk1
            ora ladder_px
            tax
            lda disptrans,x
px_out      sta 1234,y
            ldx temp_px
rsk1
            lda scrollolane+40,x
            beq rsk2
            ora ladder_px
            tax
            lda disptrans,x
px_out2     sta 1234,y
            ldx temp_px
rsk2
            lda scrollolane+80,x
            beq rsk3
            ora ladder_px
            tax
            lda disptrans,x
px_out3     sta 1234,y
            ldx temp_px
rsk3
            lda scrollolane+120,x
            beq rsk4
            ora ladder_px
            tax
            lda disptrans,x
px_out4     sta 1234,y
            ldx temp_px
rsk4
            lda scrollolane+160,x
            beq rsk5
            ora ladder_px
            tax
            lda disptrans,x
px_out5     sta 1234,y
rsk5

emptyfinish ldx   firstdup
            inx               ; step on to the next row
            cpx   #$28
            bne   no_t_wrap   ;
            ldx   #$00        ; by checking if we wrapped, we avoid having to scroll everything.
no_t_wrap   stx   firstdup    ; first understood this technique by analyzing some censor demo

            inc   totalsteps  ; control how many steps we have taken

            ;--------------------------------------------------------------------------------------------------------------------
            
;---------------------------------
; OPTIONAL MANIPULATION
;            ldx   stepmirr
;            cpx   #$12        ; this number controls where in the loop we make a break for d011 manipulation
;                              ; just a little interruption, that is preferredly offscreen, so we get no garbage
;                              ; the value is inversed, so 0 is a position more down on the screen
;                              ; and 28, which should be the stepmirr max is close to the interrupt, which
;                              ; has to be very low on the screen so we dont get printing blackouts (which happen when 
;                              ; the timing of the routine overlaps with onscreen printing area, sadly)
;            
;           ... TODO
;
;    
;skippity

            ;--------------------------------------------------------------------------------------------------------------------

            dec   stepmirr    ; if this goes to 0 we have taken (screenxsize) steps
            beq   c_row_end   ;
            jmp   next_c_row  ; if not enough, its not enough

c_row_end
            rts               ; phew. finished.

;----------------------------------------------


                ;0 1 2 3 4 5 6 7 8 9 a b c d e f
div16       byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            byte 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
            byte 2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
            byte 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3
            byte 4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
            byte 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
            byte 6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6
            byte 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
            byte 8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8

            byte 9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9
            byte $a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a
            byte $b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b,$b
            byte $c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c,$c
            byte $d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d,$d
            byte $e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e,$e
            byte $f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f,$f


mult40lo    byte   $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8,$e0,$08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98,$c0
mult40hi    byte   $00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$02,$02,$02,$02,$02,$02,$02,$03,$03,$03,$03,$03

            ; disptrans is currently suitable for a max displacement of 5 lines, thats 5x8x2=80 pixels 
            ; (Because the char is double resolution) the sine should currently
            ; not be higher than a max of decimal 80 
            ; update: no more complex (slow) cleanup. no risk of torn leftovers.

                  ; 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
disptrans   byte   $0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0,$0 ;00 green=impossibul
dispscrl    byte   $0,$f,$3,$c,$c,$6,$6,$6,$6,$b,$b,$b,$b,$b,$0,$9 ;10 top
            byte   $0,$f,$3,$c,$c,$6,$6,$6,$6,$b,$b,$b,$b,$b,$9,$2 ;20 bottom
            byte   $0,$1,$f,$f,$f,$3,$3,$3,$c,$c,$c,$6,$6,$9,$2,$8 ;30 center
            byte   $0,$f,$3,$c,$b,$b,$b,$b,$b,$b,$b,$b,$9,$2,$8,$a ;40 solo
            byte   $0,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a,$a ;50 pink=impossibul   

sin_mod_tab byte   $0,$f,$4,$9,$2,$11,$25,$2,$8,$7,$3,$0,$8,$f,$1,$12


            ;SINE LUT. 256 BYTES. range 00-ff.
256sine     byte $80,$83,$86,$89,$8c,$8f,$92,$95,$98,$9b,$9e,$a2,$a5,$a7,$aa,$ad
            byte $b0,$b3,$b6,$b9,$bc,$be,$c1,$c4,$c6,$c9,$cb,$ce,$d0,$d3,$d5,$d7
            byte $da,$dc,$de,$e0,$e2,$e4,$e6,$e8,$ea,$eb,$ed,$ee,$f0,$f1,$f3,$f4
            byte $f5,$f6,$f8,$f9,$fa,$fa,$fb,$fc,$fd,$fd,$fe,$fe,$fe,$ff,$ff,$ff
            byte $ff,$ff,$ff,$ff,$fe,$fe,$fe,$fd,$fd,$fc,$fb,$fa,$fa,$f9,$f8,$f6
            byte $f5,$f4,$f3,$f1,$f0,$ee,$ed,$eb,$ea,$e8,$e6,$e4,$e2,$e0,$de,$dc
            byte $da,$d7,$d5,$d3,$d0,$ce,$cb,$c9,$c6,$c4,$c1,$be,$bc,$b9,$b6,$b3
            byte $b0,$ad,$aa,$a7,$a5,$a2,$9e,$9b,$98,$95,$92,$8f,$8c,$89,$86,$83
            byte $80,$7c,$79,$76,$73,$70,$6d,$6a,$67,$64,$61,$5d,$5a,$58,$55,$52
            byte $4f,$4c,$49,$46,$43,$41,$3e,$3b,$39,$36,$34,$31,$2f,$2c,$2a,$28
            byte $25,$23,$21,$1f,$1d,$1b,$19,$17,$15,$14,$12,$11,$0f,$0e,$0c,$0b
            byte $0a,$09,$07,$06,$05,$05,$04,$03,$02,$02,$01,$01,$01,$00,$00,$00
            byte $00,$00,$00,$00,$01,$01,$01,$02,$02,$03,$04,$05,$05,$06,$07,$09
            byte $0a,$0b,$0c,$0e,$0f,$11,$12,$14,$15,$17,$19,$1b,$1d,$1f,$21,$23
            byte $25,$28,$2a,$2c,$2f,$31,$34,$36,$39,$3b,$3e,$41,$43,$46,$49,$4c
            byte $4f,$52,$55,$58,$5a,$5d,$61,$64,$67,$6a,$6d,$70,$73,$76,$79,$7c
            ;some online sine lut generator


                 ;0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0   
yoffsets    byte $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f,$0e,$0d,$0c,$0b,$0a
            byte $0b,$0c,$0d,$0e,$0f,$10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e


scrollolane byte   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            byte   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            byte   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            byte   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            byte   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
            
            byte   $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80
            byte   $80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80,$80 ;this last line is never printed to 
                                                                                                   ;and must always be 0




;###########################################################################################################################
;###########################################################################################################################
;###########################################################################################################################
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------
;------------------ adresstabelle für scrollchar herstellen


makeascii   lda   charzor     ;zuerst die exotisch angelegten zeichen
            cmp   lowbar      ;konvertieren, sodass für die routine
            bne   nextzei     ;lesbare werte herauskommen
            lda   #$00
            jmp   backwrite
nextzei     cmp   dblcrss
            bne   nextzei2
            lda   #$01
            jmp   backwrite
nextzei2    cmp   smallo
            bne   nextzei3
            lda   #$fe
            jmp   backwrite
nextzei3    cmp   smallx      ;
            bne   zeiend      ; if this byte is none of the above, it must be $ff = end of charmap
            lda   #$fa
backwrite   sta   charzor
            inc   backwrite+1
            inc   makeascii+1
            bne   makeascii
            inc   makeascii+2
            inc   backwrite+2
            jmp   makeascii
            
            ; conversion is now done, now register start of each character.

zeiend      ldx   #$00
            lda   #<charzor   ;lo adresse char start laden
            sta   charlo      ;in tabelle schreiben
            sta   countup+1   ;in pointer lo schreiben
            lda   #>charzor   ;hi adresse char start laden
            sta   charhi      ;in tabelle schreiben
            sta   countup+2   ;in pointer hi schreiben
            
countup     lda   $1234       ;zählpointer, platzhalterwert
            cmp   #$fa        ;zeichenende?
            beq   cliffhanger ;ja, über die kante gehen
            cmp   #$ff        ;ende aller zeichen?
            beq   ndmakeascii ;ja. raus
            
            inc   countup+1   ;nein, pointer
            lda   countup+1   ;hochzählen. blockbreak?
            cmp   #$00
            bne   countup     ;nein, einfach weiter
            inc   countup+2   ;ja, näxter block
            jmp   countup     
            
cliffhanger inc   countup+1   ;über die kante
            lda   countup+1   ;gehen
            cmp   #$00        ;
            bne   nocbrk      ;
            inc   countup+2   ;
nocbrk      inx               ;adresstabellenpointer 1 weiter
            lda   countup+1   ;und lo wert der aktuellen adresse
            sta   charlo,x    ;in tabelle schreiben

            lda   countup+2   ;und hi wert der aktuellen adresse
            sta   charhi,x    ;in tabelle schreiben
            jmp   countup     ;und weiter
            
ndmakeascii jsr   post_processing ;precalc aliasing of chars
            rts

;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------

;#############################################
;
; (2013)PROBLEM:
; THIS POST PROCESSING ROUTINE RELYS ON A PREFORMATTED MATRIX BUT NOW CALCS DIRECTLY IN THE RAW CHAR MATRIX.
; TWO KINDS OF BUGS APPLY HERE:
; #1 THE UPPER ROW CHECK DOES *PROBABLY* NOT WORK SINCE THE CHARS HAVE NO BLANK SPACE AROUND THEM
; #2 THE KERNING CHECK IS NOT IMPLEMENTED (CHECK $FE = WIDTH OF CHAR REACHED) SINCE THESE ARE RAW CHARACTERS
;
; #1= solved. routine is split into 5 segments, check1 only goes here and down,
; checks 2-4 go up, here and down, check 5 only goes here and up
; #2= solved. 

pp_buff     byte 0,0,0,0,0,0  ;buffer that makes sure, the new principle is working so advancing does only take 1 and not 28
pp_sidestep byte  0           ;which row of the current char do we process?
pp_total    byte  0           ;total number of already processed chars

post_processing               ;this obviously converts the chars into a quickly readable matrix that
                              ;tells by reading a byte at which position this byte is, so we precalc
                              ;what was previously done in realtime: what "aliasing" style chars must be inserted

            lda   #$00        
            sta   pp_total          ;just needs initial nulling
            sta   pp_sidestep       ;this is nulled later near the end
   
pp_digger   ldx   pp_total          ;total number of processed chars
            
            lda   charlo,x          ;base adress of char
            sta   cha_lo_fetch
            lda   charhi,x    
            sta   cha_hi_fetch
            
            ldy   #$00        
            lda   (cha_lo_fetch),y
            cmp   #$ff        
            beq   pp_all_end        ;must be last char, game over

pp_rower    ldy   pp_sidestep       ;check if the char is finished 
            lda   (cha_lo_fetch),y
            cmp   #$fe        
            beq   pp_this_end       ;kerning reached. char border reached. next char.
            
            ; check always again how wide the char is. 
            ldy   #$00        
w_chk_pp    lda   (cha_lo_fetch),y  ;walk through first char line and look for kerning-indicator-byte
            cmp   #$fe                      
            beq   w_chk_ppnd
            iny
            jmp   w_chk_pp  
            
w_chk_ppnd  jsr   pp_tobuf         ;copy this row from the char to the buffer

            ; char-row is now ready, reformatted in buffer
            ldy   #$00        
            lda   pp_buff,y   
            
            ;check+process pix1
            cmp   #$00              ;is this pixel empty?
            beq   pp_2to4           ;yes, next pixel.
                                    ;no, this pixel is not empty
            iny                     ;is the next one empty?
            lda   pp_buff,y
            beq   pp1_single        ;yes, so the top one is a single pixel
            dey                     ;no, so the top one is a top pixel
            lda   #$11              ;top pixel
            sta   pp_buff,y   
            jmp   pp_2to4           ;next pixel  
            
pp1_single  dey
            lda   #$41              ;single pixel
            sta   pp_buff,y

            ;---------------------------------------------

pp_2to4     jsr   pp_234      ;check+process pix2
            jsr   pp_234      ;check+process pix3
            jsr   pp_234      ;check+process pix4
            
            ;---------------------------------------------
            
            ;check+process pix5

            iny   ;next this
            lda   pp_buff,y   
            beq   pp5_finish        ;this pixel is empty, thats it.

            dey   ;prev             ;this pixel is filled, was the previous filled?
            lda   pp_buff,y         
            beq   pp5_single        ;this pixel is filled, previous empty, single pixel.           
            
            lda   #$21              ;this pixel is filled, the previous was filled. bottom pixel.
            iny   ;this
            sta   pp_buff,y   
            jmp   pp5_finish

pp5_single  lda   #$41              ;this pixel is filled, the previous was empty. single pixel.
            iny   ;this
            sta   pp_buff,y

pp5_finish
            ;buffer is now finished and processed

            ;copy buffer back
            jsr   pp_frombuf

            ;next row
            inc   pp_sidestep       ;next row in char
            jmp   pp_rower


pp_this_end lda   #$00
            sta   pp_sidestep       ;reset row counter
            inc   pp_total          ;next char
            jmp   pp_digger   
            
pp_all_end  rts
            

            ;---------------------------------------------

pp_234      iny                     ;next pixel position
            lda   pp_buff,y         ;is this pixel empty?
            beq   pp2_exit           ;yes, next pixel
            dey   ;prev             ;so here is something. was the previous pixel empty?
            lda   pp_buff,y         ;
            beq   pp2_top           ;yes, it was empty. this one is filled, is the next one filled?
            iny   ;this
            iny   ;next             ;no, this one is filled - is the next one empty?
            lda   pp_buff,y         ;
            beq   pp2_bottom        ;yes. so: this pixel is filled, the previous was filled and the next is empty.
                                    ;this is a bottom pixel, then
            lda   #$31              ;no, so: this pixel is filled, the previous was filled and the next is filled,too.
            dey   ;this             ;this is an enclosed pixel, then
            sta   pp_buff,y   
            rts      
pp2_top     iny   ;this             ;this one is filled, previous was empty
            iny   ;next
            lda   pp_buff,y 
            beq   pp2_single        ;this one is filled, previous was empty, next one is empty. this is a single pixel.
            dey   ;this             ;this one is filled, previous was empty, next one is filled. this is a top pixel.
            lda   #$11              ;top pixel
            sta   pp_buff,y   
            rts      
pp2_single  lda   #$41              ;single pixel
            dey   ;this
            sta   pp_buff,y   
            rts     
pp2_bottom  lda   #$21              ;bottom pixel
            dey   ;this
            sta   pp_buff,y   
pp2_exit    rts


pp_stor     byte  $0
pp_parent   byte  $0


            ;copy row of char to buffer for easier processing

pp_tobuf    iny
            sty   cur_char_width    ;width is now stored        
            ldy   pp_sidestep       ;which row of the char do we want to output?
            lda   (cha_lo_fetch),y  ;load good pixel from current line and current row
            sta   pp_buff           ;output pixel to current buffer
            tya
            clc
            adc   cur_char_width    ;increase source pointer by char width
            tay
            lda   (cha_lo_fetch),y  ;load good pixel from current line and current row
            sta   pp_buff+1         ;;output pixel to current buffer
            tya
            clc
            adc   cur_char_width    ;increase source pointer by char width
            tay
            lda   (cha_lo_fetch),y  ;load good pixel from current line and current row
            sta   pp_buff+2         ;output pixel to current buffer
            tya
            clc
            adc   cur_char_width    ;increase source pointer by char width
            tay
            lda   (cha_lo_fetch),y  ;load good pixel from current line and current row
            sta   pp_buff+3         ;output pixel to current buffer
            tya
            clc
            adc   cur_char_width    ;increase source pointer by char width
            tay
            lda   (cha_lo_fetch),y  ;load good pixel from current line and current row
            sta   pp_buff+4         ;output pixel to current buffer
            rts

            ;copy processed row buffer back to char matrix

pp_frombuf  ldy   pp_sidestep
            lda   pp_buff
            sta   (cha_lo_fetch),y
            tya
            clc
            adc   cur_char_width
            tay
            lda   pp_buff+1
            sta   (cha_lo_fetch),y
            tya
            clc
            adc   cur_char_width
            tay
            lda   pp_buff+2
            sta   (cha_lo_fetch),y
            tya
            clc
            adc   cur_char_width
            tay
            lda   pp_buff+3
            sta   (cha_lo_fetch),y
            tya
            clc
            adc   cur_char_width
            tay
            lda   pp_buff+4
            sta   (cha_lo_fetch),y
            rts


;--------------------------------------------------------------------------------------
;--------------------------------------------------------------------------------------
;-------------------------- scrolltext to screen --------------------------------------

txpoint           byte  $0
txcurr            byte  $0
txtemp            byte  $0
firstrow          byte  $0
fr_backup         byte  $0
cur_char_width    byte  $0


txplot      ldx   txcurr      ;aktuelles zeichen laden
                              ;konvertierter wert aus chartab
            lda   charlo,x    ;anfangsadresse
            sta   cha_lo_fetch;der bits
            lda   charhi,x    ;des zeichens
            sta   cha_hi_fetch;in adresspointer laden
            ldy   txpoint     ;aktuelle reihe des zeichens laden
            lda   (cha_lo_fetch),y      ;yes, y-ind crosses block borders
            cmp   #$fe        ;zeichen zuende? (Kerning indicator)
            bne   txthis      ;nein, dieses weiterscrollen

txretry     inc   txlyr+1     ;ja, neues zeichen laden
            lda   txlyr+1
            cmp   #$00
            bne   txlyr
            inc   txlyr+2
txlyr       lda   lyrics      ;zeichen laden

                              ;--------------------------------------
                              ; text controlbytes - optional
                              ;--------------------------------------

;            cmp   #$f1        ;standard mode
;            bne   txcheck2
;            lda #$01
;            sta trajstate
;            jmp   txretry

;txcheck2    cmp   #$f2        ;turning mode
;            bne   txcheck8
;            lda #$02
;            sta trajstate
;            jmp   txretry


                              ;--------------------------------------

txcheck8    cmp   #$ff        ;textende?
            bne   txflow      ;nein, text geht weiter
            lda   #<lyrics
            sta   txlyr+1
            lda   #>lyrics
            sta   txlyr+2
            jmp   txlyr       ;wrap, neuer versuch

                              ;--------------------------------------

txflow      sec
            sbc   #$20              ;zeichen für tabelle umrechnen
            tax                     ;und zeichenwert aus
            lda   chartab,x         ;tabelle lesen
            sta   txcurr            ;und ergebnis speichern

            ldx   #$00
            stx   txpoint           ;falls zeichenstart, reihenpointer zurücksetzen

            ; load current, parsed char
txthis      ldx   txcurr            ;current, parsed char
            lda   charlo,x
            sta   cha_lo_fetch
            lda   charhi,x
            sta   cha_hi_fetch
            
            ; check how wide the char is. 
            ldy   #$00        
w_chk_cont  lda   (cha_lo_fetch),y  ;walk through first char line and look for kerning-indicator-byte
            cmp   #$fe              ;13.9.2014 been hunting 2 hours for a bug which turned out this was checked WRONG. STUPID!        
            beq   w_checkend
            iny
            jmp   w_chk_cont  
            
w_checkend  iny
            sty   cur_char_width    ;width is now stored

            ldy   txpoint           ;which row of the char do we want to output?
            ldx   firstrow          ;and where in the auto scrolling scrollolane are we?

            lda   (cha_lo_fetch),y  ;load good pixel from current line and current row
            sta   scrollolane,x     ;output pixel to current line and current row
            
            tya
            clc
            adc   cur_char_width    ;increase source pointer by char width
            tay
            lda   (cha_lo_fetch),y  ;load good pixel from current line and current row
            sta   scrollolane+40,x  ;output pixel to current line and current row

            tya
            clc
            adc   cur_char_width    ;increase source pointer by char width
            tay
            lda   (cha_lo_fetch),y  ;load good pixel from current line and current row
            sta   scrollolane+80,x  ;output pixel to current line and current row
            
            tya
            clc
            adc   cur_char_width    ;increase source pointer by char width
            tay
            lda   (cha_lo_fetch),y  ;load good pixel from current line and current row
            sta   scrollolane+120,x ;output pixel to current line and current row

            tya
            clc
            adc   cur_char_width    ;increase source pointer by char width
            tay
            lda   (cha_lo_fetch),y  ;load good pixel from current line and current row
            sta   scrollolane+160,x ;output pixel to current line and current row

txfinish    inc   txpoint           ;for the next time, get next row of source char

            inc   firstrow          ;for the next time, put to next row of target (scrollolane)
            lda   firstrow
            cmp   #$28
            bne   n_t_reset
            lda   #$00
            sta   firstrow
n_t_reset   rts





;----------------------------------------------------------------------------
;----------------------------------------------------------------------------

; Old versions of CBMPrgStudio had a wrong ASCII to PETSCII conversion table.

 ; NOW --------------------- BEFORE

lowbar  = #$00            ; _ ($5f) = $00  kein bit
dblcrss = #$23            ; # ($23) = $01  ein bit
smallo  = #$4f            ; o ($6f) = $fe  kerning ende (Kerning indicator)
smallx  = #$58            ; x ($78) = $fa  zeichenblock ende


charzor     text    "____o"
            text    "____o"
            text    "____o"
            text    "____o"
            text    "____x"

            text    "_###__o"
            text    "##_##_o"
            text    "#####_o"
            text    "##_##_o"
            text    "##_##_x"

            text    "####__o"
            text    "##_##_o"
            text    "####__o"
            text    "##_##_o"
            text    "####__x"

            text    "_###_o"
            text    "##___o"
            text    "##___o"
            text    "##___o"
            text    "_###_x"

            text    "####__o"
            text    "##_##_o"
            text    "##_##_o"
            text    "##_##_o"
            text    "####__x"

            text    "####_o"
            text    "##___o"
            text    "###__o"
            text    "##___o"
            text    "####_x"

            text    "####_o"
            text    "##___o"
            text    "###__o"
            text    "##___o"
            text    "##___x"

            text    "_####_o"
            text    "##____o"
            text    "##_##_o"
            text    "##_##_o"
            text    "_####_x"

            text    "##_##_o"
            text    "##_##_o"
            text    "#####_o"
            text    "##_##_o"
            text    "##_##_x"

            text    "##_o"
            text    "##_o"
            text    "##_o"
            text    "##_o"
            text    "##_x"

            text    "__##_o"
            text    "__##_o"
            text    "__##_o"
            text    "__##_o"
            text    "###__x"

            text    "##_##_o"
            text    "##_##_o"
            text    "####__o"
            text    "##_##_o"
            text    "##_##_x"

            text    "##___o"
            text    "##___o"
            text    "##___o"
            text    "##___o"
            text    "####_x"

            text    "#___#_o"
            text    "##_##_o"
            text    "#_#_#_o"
            text    "##_##_o"
            text    "##_##_x"

            text    "#__##_o"
            text    "##_##_o"
            text    "#####_o"
            text    "##_##_o"
            text    "##__#_x"

            text    "_###__o"
            text    "##_##_o"
            text    "##_##_o"
            text    "##_##_o"
            text    "_###__x"

            text    "####__o"
            text    "##_##_o"
            text    "####__o"
            text    "##____o"
            text    "##____x"

            text    "_###__o"
            text    "##_##_o"
            text    "##_##_o"
            text    "#####_o"
            text    "_##_#_x"

            text    "####__o"
            text    "##_##_o"
            text    "####__o"
            text    "##_##_o"
            text    "##_##_x"

            text    "_###__o"
            text    "##____o"
            text    "_###__o"
            text    "___##_o"
            text    "####__x"

            text    "####_o"
            text    "_##__o"
            text    "_##__o"
            text    "_##__o"
            text    "_##__x"

            text    "##_##_o"
            text    "##_##_o"
            text    "##_##_o"
            text    "##_##_o"
            text    "_###__x"

            text    "##_##_o"
            text    "##_##_o"
            text    "##_##_o"
            text    "_###__o"
            text    "__#___x"

            text    "##_##_o"
            text    "##_##_o"
            text    "#_#_#_o"
            text    "##_##_o"
            text    "#___#_x"

            text    "##_##_o"
            text    "##_##_o"
            text    "_###__o"
            text    "##_##_o"
            text    "##_##_x"

            text    "##_##_o"
            text    "##_##_o"
            text    "_###__o"
            text    "_##___o"
            text    "##____x"

            text    "#####_o"
            text    "__##__o"
            text    "_##___o"
            text    "##____o"
            text    "#####_x"
                
            text    "##_o"
            text    "##_o"
            text    "##_o"
            text    "___o"
            text    "##_x"

            text    "___o"
            text    "___o"
            text    "___o"
            text    "##_o"
            text    "_#_x"

            text    "___o"
            text    "___o"
            text    "___o"
            text    "##_o"
            text    "##_x"

            text    "_____o"
            text    "_____o"
            text    "###__o"
            text    "_____o"
            text    "_____x"

            text    "####__o" ;1f
            text    "___##_o"
            text    "_###__o"
            text    "______o"
            text    "_##___x"

            text    "_##_o" ;20
            text    "###_o"
            text    "_##_o"
            text    "_##_o"
            text    "_##_x"

            text    "###__o"
            text    "__##_o"
            text    "_##__o"
            text    "##___o"
            text    "####_x"

            text    "###__o"
            text    "__##_o"
            text    "_##__o"
            text    "__##_o"
            text    "###__x"

            text    "##___o"
            text    "##_#_o"
            text    "####_o"
            text    "__##_o"
            text    "__##_x"
                
            text    "####_o"
            text    "##___o"
            text    "###__o"
            text    "__##_o"
            text    "###__x"

            text    "_###__o"
            text    "##____o"
            text    "####__o"
            text    "##_##_o"
            text    "_###__x"

            text    "####_o" 
            text    "__##_o"
            text    "_##__o"
            text    "##___o"
            text    "##___x"

            text    "_###__o"
            text    "##_##_o"
            text    "_###__o"
            text    "##_##_o"
            text    "_###__x"

            text    "_###__o" 
            text    "##_##_o"
            text    "_####_o"
            text    "___##_o"
            text    "_###__x" ;$28

            text    "____o" 
            text    "_#__o"
            text    "###_o"
            text    "_#__o"
            text    "____x" ;$29
            
            text    "____o" 
            text    "##__o"
            text    "____o"
            text    "##__o"
            text    "____x" ;$2a
            
            text    "____o" 
            text    "###_o"
            text    "____o"
            text    "###_o"
            text    "____x" ;$2b


charend     byte   $ff,$ff,$ff

            ;--------------------------------------------------

charlo      byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

charhi      byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
            byte   $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

            ;subtract $20
                   ;sp   !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /
chartab     byte   $00,$1b,$00,$00,$00,$00,$00,$00,$00,$00,$00,$29,$1c,$1e,$1d,$00
                   ; 0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?
            byte   $0f,$20,$21,$22,$23,$24,$25,$26,$27,$28,$2a,$00,$00,$2b,$00,$1f
                   ; @   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o
            byte   $00,$01,$02,$03,$04,$05,$06,$07,$08,$09,$0a,$0b,$0c,$0d,$0e,$0f
                   ; p   q   r   s   t   u   v   w   x   y   z   [   §   ]
            byte   $10,$11,$12,$13,$14,$15,$16,$17,$18,$19,$1a,$00,$00,$00,$00,$00



;-------------------------------------------------
;-------------------------------------------------
;-------------------------------------------------
;EOF
