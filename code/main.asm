;==========================================================
; C-64 6502 assembly code to solve N Queens puzzle
; created 2023 by Vittorio Nardone
;
; https://www.vittorionardone.it
;==========================================================

;==========================================================
; LABELS
; Comment or uncomment the lines below 
; Depending on your target machine
;==========================================================

; VC20 (not tested)
;BGCOLOR       = $900f
;BORDERCOLOR   = $900f
;BASIC         = $1001
;SCREENRAM     = $1e00

; C16, C116, Plus/4 (not tested)
;BGCOLOR      = $ff15
;BORDERCOLOR  = $ff19
;BASIC        = $1001
;SCREENRAM    = $0c00

; C128 (not tested)
;BGCOLOR       = $d020
;BORDERCOLOR   = $d021
;BASIC         = $1c01
;SCREENRAM     = $0400

; C64
BGCOLOR       = $d020
BORDERCOLOR   = $d021
BASIC         = $0801
SCREENRAM     = $0400
CLEARSCREEN   = $e544  
BSOUT         = $ffd2                ;kernel character output sub
BSOUTPTR      = $fb                  ;zero page pointer
BUINTOUT      = $bdcd                ;basic print XA as unsigned integer
CURSORPOS     = $e50a                ;current cursor position

;==========================================================
; BASIC header
;==========================================================

* = BASIC

                !byte $0b, $08
                !byte $E7                     ; BASIC line number:  $E2=2018 $E3=2019 etc       
                !byte $07, $9E
                !byte '0' + entry % 10000 / 1000        
                !byte '0' + entry %  1000 /  100        
                !byte '0' + entry %   100 /   10        
                !byte '0' + entry %    10             
                !byte $00, $00, $00           ; end of basic

;==========================================================
; SETTINGS
;==========================================================

NDIM            !byte $08               ; in 4-12 range
QUEENCHAR       !byte $77
BOARDCHAR       !byte $A6
COLOR           !byte $01

;==========================================================
; INCLUDE
;==========================================================

!src "code/seconds.asm"

;==========================================================
; CODE
;==========================================================
!zn
entry

                ; lda COLOR               ; load color value
                ; sta BGCOLOR             ; change background color
                ; sta BORDERCOLOR         ; change border color
                jsr CLEARSCREEN         ; clear the screen
;
                ldx #<.welcome          ; print welcome message
                ldy #>.welcome          
                jsr sprint              
;
                ldx #<.dimension        ; print dimension message
                ldy #>.dimension        
                jsr sprint  

                jsr waitkey             ; wait for dimension
                sec
                sbc #$30
                cmp #$01                ; is it starting with 1?
                bne .entrycheck         
                jsr waitkey             ; wait for dimension
                sec                     
                sbc #$26    
                cmp #$0D                ; is it more then 12?
                bcs entry
                jmp .entrystore                
.entrycheck     cmp #$04                ; is it less than 4?
                bcc entry
                cmp #$0A                ; is it more then 9?
                bcs entry

               
.entrystore     sta NDIM                ; store dimension

                lda #$00                ; print dimension
                ldx NDIM
                jsr BUINTOUT
                lda #$0D                
                jsr BSOUT
;
                jsr starttimer	        ; start seconds counter
;
                jsr init                ; init memory
;              
                ldx #$00                ; start from row 0
                jsr run
;
                jsr stoptimer	        ; stop seconds counter
;
                rts                     ; exit the program

.welcome        !pet $0D, "c64 n queens puzzle", $0D, $00 
.dimension      !pet "dimension (4-12)? ", $00

;==========================================================
; Init
;==========================================================
!zn
CHESSBOARD      !fill $FF, $00	        ; reserve 255 bytes
NDIML1          !byte $00    
NDIMP1          !byte $00 

init            ldx NDIM                ; calculate NDIM-1
                dex 
                stx NDIML1
                inx                     ; calculate NDIM+1
                inx
                stx NDIMP1
                lda #$FF
                sta CURSORX
                lda BOARDCHAR
                ldx #$00
                stx FOUNDCOUNTH
                stx FOUNDCOUNTL
                
;
.initloop       sta CHESSBOARD,x        ; fill board 
                inx
                cpx #$FF
                bne .initloop
                rts

;==========================================================
; Run
; Usage:
;  ldx #$00         ; (row no, 0..7)
;  jsr run
;==========================================================
!zn
.RUNX           !byte $00
.RUNY           !byte $00  
.LASTRUNY       !fill $0C, $00 
FOUNDCOUNTL     !byte $00
FOUNDCOUNTH     !byte $00

run
                stx .RUNX             ; store location
                ldy #$00

.rungo          sty .RUNY   
                jsr offsetcalc          ; calculate mem offset
                ldy OFFSET              ; get offset from stack
                lda CHESSBOARD,y        ; get chessboard cell
                cmp BOARDCHAR           ; check if empty
                bne .runnext
                ldx .RUNX               ; place queen
                lda .RUNY
                sta .LASTRUNY, x
                tay
                jsr place 
                ldx .RUNX               ; move to next row
                inx 
                cpx NDIM                ; last row?
                beq .runfound           
                jmp run                                     

.runnext        ldx .RUNX               ; try next cell in row
                ldy .RUNY
.runmoveright   iny
                cpy NDIM                ; end of row?
                bne .rungo 
                
.runprevrow     dex                     ; try previous row
                cpx #$FF                ; already first row?
                beq .runend             

                stx .RUNX
                jsr undoqueen           ; remove queen from row

.runnexty       ldx .RUNX               ; try next cell in row
                ldy .LASTRUNY, x
                jmp .runmoveright

.runfound       inc FOUNDCOUNTL        ; result found, inc
                bne .runfoundprint
                inc FOUNDCOUNTH

.runfoundprint  jsr printboard          ; print founded result
                ldx .RUNX
                jsr undoqueen           ; remove last queen
                jmp .runnexty           ; try next cell in row

.runend         
                ldx #<.found            ; print results message
                ldy #>.found 
                jsr sprint
                lda FOUNDCOUNTH
                ldx FOUNDCOUNTL
                jsr BUINTOUT

                ldx #<.timeelapsed       ; print results message
                ldy #>.timeelapsed 
                jsr sprint
                jsr printtimer

                lda #$0D     
                jsr BSOUT

                rts                     ; end of the run

.found          !pet $0D, "solutions found: ", $00 
.timeelapsed    !pet $0D, "duration in seconds: ", $00

;==========================================================
; Wait for a key pressed (result in A)
; Usage:
;   jsr waitkey  
;==========================================================
!zn
waitkey         jsr $FFE4              ; wait for key pressed
                beq waitkey         
                rts                    

;==========================================================
; Remove a queen from a specific row and empty all 
; cells blocked by the removed queen
; Usage:
;  ldx #$01 (queen row no, 0..7)
;  jsr undoqueen
;==========================================================
!zn
.CLEANVALUE     !byte $00
.CLEANX         !byte $00
.CLEANCOUNTER   !byte $00

undoqueen        
                stx .CLEANX             ; search for queen
                ldy #$00
                jsr offsetcalc          ; calculate mem offset
                ldy OFFSET              
                ldx #$00
                lda QUEENCHAR 
.undoqueenl1    cmp CHESSBOARD,y        ; queen found?
                beq .undoqueenstart
                iny
                inx                     ; increase counter
                cpx NDIM                ; end of row?
                bne .undoqueenl1  
                
.undoqueenstart lda BOARDCHAR           ; remove queen
                sta CHESSBOARD,y 
                ldy #$00                ; start cleaning
                ldx #$00
                sty .CLEANCOUNTER
                lda .CLEANX             ; calculate value to search for
                clc         
                adc #$30
                sta .CLEANVALUE         
                
.undoqueenl2    lda .CLEANVALUE         
                cmp CHESSBOARD,y        ; correct value?
                bne .undoqueennext
                lda BOARDCHAR
                sta CHESSBOARD,y        ; replace with empty char

.undoqueennext  iny                     ; increase counters
                inx
                cpx NDIM                ; end of the row?
                bne .undoqueenl2
                ldx #$00
                inc .CLEANCOUNTER
                lda .CLEANCOUNTER   
                cmp NDIM                ; last row?
                bne .undoqueenl2
;
                rts                     ; return

;==========================================================
; Place a queen and block cells
; Usage:
;  ldx #$01         ; (row no, 0..7)
;  ldy #$01         ; (column no, 0..7)
;  jsr place        
;==========================================================
!zn
.PLACEX         !byte $00
.PLACEY         !byte $00
.PLACEXLOOP     !byte $00
.PLACEYLOOP     !byte $00
.PLACEXCHAR     !byte $00

place
                stx .PLACEX             ; store location
                sty .PLACEY
                txa
                clc
                adc #$30
                sta .PLACEXCHAR
;
.placerow                               ; fill the row
                ldy #$00
                jsr offsetcalc          ; calculate mem offset
                ldy OFFSET              ; get offset from stack                
                ldx #$00                ; first column               
;
.placerowloop   
                lda CHESSBOARD,y        ; get chessboard cell
                cmp BOARDCHAR           ; check if empty
                bne .placerownext
                lda .PLACEXCHAR         ; load filling char 
                sta CHESSBOARD,y        ; set chessboard cell
.placerownext   iny                     ; increase counters
                inx
                cpx NDIM                ; end of row?
                bne .placerowloop                
;
.placecol                               ; fill the column
                ldy .PLACEY
                ldx #$00
                jsr offsetcalc          ; calculate mem offset
                ldy OFFSET              ; get offset from stack                
                ldx #$00                ; first row
;
.placecolloop   
                lda CHESSBOARD,y        ; get chessboard cell
                cmp BOARDCHAR           ; check if empty
                bne .placecolnext
                lda .PLACEXCHAR         ; load filling char
                sta CHESSBOARD, y       ; set chessboard cell
.placecolnext   lda OFFSET              ; decrease offset by 8
                clc
                adc NDIM
                sta OFFSET              ; store new offset
                tay
                inx                     ; increase counter
                cpx NDIM                ; end of row?
                bne .placecolloop     
;
.placediag                              ; fill diag (->)
                ldy .PLACEY
                ldx .PLACEX
                stx .PLACEXLOOP         ; store indexes
                sty .PLACEYLOOP
                jsr offsetcalc          ; calculate mem offset
;
.placediagloop  ldx .PLACEXLOOP         ; move indexes ->
                ldy .PLACEYLOOP
                inx
                iny
                cpx NDIM                ; end of row?
                beq .placeback     
                cpy NDIM                ; end of column?
                beq .placeback  
                stx .PLACEXLOOP         ; store indexes
                sty .PLACEYLOOP    
                lda OFFSET              ; increase offset
                clc
                adc NDIMP1
                sta OFFSET       
                tay 
                lda CHESSBOARD,y        ; get chessboard cell
                cmp BOARDCHAR           ; check if empty
                bne .placediagloop                 
                lda .PLACEXCHAR         ; load filling char
                sta CHESSBOARD, y       ; set chessboard cell
                jmp .placediagloop         
.placeback                              ; fill diag (<-)
;
                ldy .PLACEY             
                ldx .PLACEX
                stx .PLACEXLOOP         ; restore indexes
                sty .PLACEYLOOP
                jsr offsetcalc          ; calculate mem offset
;
.placebackloop  ldx .PLACEXLOOP         ; move indexes <-
                ldy .PLACEYLOOP
                dex
                dey
                cpx #$FF                ; end of row?
                beq .placediag2     
                cpy #$FF                ; end of column?
                beq .placediag2  
                stx .PLACEXLOOP         ; store indexes
                sty .PLACEYLOOP    
                lda OFFSET              ; decrease offset
                sec
                sbc NDIMP1
                sta OFFSET
                tay
                lda CHESSBOARD,y        ; get chessboard cell
                cmp BOARDCHAR           ; check if empty
                bne .placebackloop   
                lda .PLACEXCHAR         ; load filling char 
                sta CHESSBOARD, y       ; set chessboard cell   
                jmp .placebackloop
;
.placediag2                             ; fill diag (->)
                ldy .PLACEY
                ldx .PLACEX
                stx .PLACEXLOOP         ; store indexes
                sty .PLACEYLOOP
                jsr offsetcalc          ; calculate mem offset
; 
.placediag2loop ldx .PLACEXLOOP         ; move indexes ->
                ldy .PLACEYLOOP
                dex
                iny
                cpx #$FF                ; end of row?
                beq .placeback2     
                cpy NDIM                ; end of column?
                beq .placeback2  
                stx .PLACEXLOOP         ; store indexes
                sty .PLACEYLOOP    
                lda OFFSET              ; increase offset by 9
                sec
                sbc NDIML1
                sta OFFSET       
                tay 
                lda CHESSBOARD,y        ; get chessboard cell
                cmp BOARDCHAR           ; check if empty
                bne .placediag2loop                 
                lda .PLACEXCHAR         ; load filling char
                sta CHESSBOARD, y       ; set chessboard cell
                jmp .placediag2loop  
.placeback2                             ; fill diag (->)
                ldy .PLACEY
                ldx .PLACEX
                stx .PLACEXLOOP         ; store indexes
                sty .PLACEYLOOP
                jsr offsetcalc          ; calculate mem offset
;
.placeback2loop ldx .PLACEXLOOP         ; move indexes ->
                ldy .PLACEYLOOP
                inx
                dey
                cpx NDIM                ; end of row?
                beq .placequeen     
                cpy #$FF                ; end of column?
                beq .placequeen  
                stx .PLACEXLOOP         ; store indexes
                sty .PLACEYLOOP    
                lda OFFSET              ; increase offset by 9
                clc
                adc NDIML1
                sta OFFSET       
                tay 
                lda CHESSBOARD,y        ; get chessboard cell
                cmp BOARDCHAR           ; check if empty
                bne .placeback2loop                 
                lda .PLACEXCHAR         ; load filling char
                sta CHESSBOARD, y       ; set chessboard cell
                jmp .placeback2loop                  
.placequeen                   
;
                ldx .PLACEX
                ldy .PLACEY
                jsr offsetcalc          ; calculate mem offset
                ldy OFFSET              ; get offset from stack
                lda QUEENCHAR           ; load filling char
                sta CHESSBOARD,y        ; set chessboard cell

                rts



;==========================================================
; Calculate memory offset
; Usage:
;  ldx #$01         ; (row no, 0..7)
;  ldy #$01         ; (column no, 0..7)
;  jsr offsetcalc   ; result in OFFSET 
;  lda OFFSET 
;==========================================================
!zn
OFFSET         !byte $00

offsetcalc      
                lda #$00                ; reset offset
.offsetrow      cpx #$00                ; last row?
                beq .offsetcolumn       ; offset calculated, jmp
                dex                     ; decrease row no
                clc                     ; clear carry (for adc)
                adc NDIM                ; add one row offset
                jmp .offsetrow          ; loop
.offsetcolumn   sta OFFSET              ; store row offset in menory
                tya                     ; move column no (y) to a
                clc                     ; clear carry (for adc)
                adc OFFSET              ; add column offset
                sta OFFSET              ; store again in memory
                rts

;==========================================================
; Print board contents
; Usage:
;  jsr printboard
;==========================================================
!zn
CURSORX         !byte $FF
.CURSORY        !byte $00
.ROWCOUNT       !byte $00

printboard      lda CURSORX            ; first time call? 
                cmp #$FF
                bne .printsetcursor
                sec
                jsr CURSORPOS           ; get & store cursor position 
                stx CURSORX            
                sty .CURSORY
                jmp .printboardinit
;
.printsetcursor ldx CURSORX            ; restore cursor position
                ldy .CURSORY
                clc
                jsr CURSORPOS 
;                               
.printboardinit ldx #$00                ; reset chars counter
                stx .ROWCOUNT
                lda #$0D                ; go to next line
                jsr BSOUT               
; 
.printrow       ldy #$00                ; reset row counter
.printrowloop   lda CHESSBOARD,x        ; get chessboard content
                cmp QUEENCHAR           ; is it a queen?
                beq .printrowchar
                lda BOARDCHAR           ; load board fill char
.printrowchar   jsr BSOUT               ; print it
                inx                     ; increase counters                
                iny                     
                cpy NDIM                ; end of row?
                bne .printrowloop
;
                lda #$0D                ; go to next line
                jsr BSOUT
;
                inc .ROWCOUNT           ; increase row counter
                lda .ROWCOUNT
                cmp NDIM                ; end of board?
                bne .printrow
;
                rts                     ; return
                

;==========================================================
; SPRINT (null terminated string print) 
; Usage:
;  ldx #<.hello ;string address LSB
;  ldy #>.hello ;string address MSB
;  jsr sprint
;==========================================================
!zn
sprint          stx BSOUTPTR          ;save string pointer LSB
                sty BSOUTPTR+1        ;save string pointer MSB
                ldy #0                ;starting string index
;
.sprint01       lda (BSOUTPTR),y      ;get a character
                beq .sprint02         ;end of string
;
                jsr BSOUT             ;print character
                iny                   ;next
                bne .sprint01
;
.sprint02       rts                   ;exit

!eof