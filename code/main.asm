;==========================================================
; C-64 6502 assembly code to solve N Queens puzzle
; created 2023 by Vittorio Nardone
;
; http://www.vittorionardone.it
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
CURSORPOS     = $e50a  

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

NDIM            !byte $08              ; max is $0C / 12
QUEENCHAR       !byte $77
BOARDCHAR       !byte $A6

;==========================================================
; CODE
;==========================================================

entry

                lda #$01                ; load color value
                sta BGCOLOR             ; change background color
                sta BORDERCOLOR         ; change border color
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
                cmp #$01
                bne _entrycheck
                jsr waitkey             ; wait for dimension
                sec
                sbc #$26
                cmp #$0D
                bcs entry
                jmp _entrystore                
_entrycheck     cmp #$04
                bcc entry
                cmp #$0A
                bcs entry

               
_entrystore     sta NDIM                ; store dimension

                lda #$00
                ldx NDIM
                jsr BUINTOUT
                lda #$0D                
                jsr BSOUT
;
                jsr init                ; init memory
;
                ldx #$00                ; start from row 0
                jsr run
;
                rts                     ; exit the program

.welcome        !pet "c64 n queens puzzle", $0D, $00 
.dimension      !pet "dimension (4-12): ", $00


;==========================================================
; Init
;==========================================================

CHESSBOARD      !fill $FF, $00	        ; reserve 255 bytes
NDIML1          !byte $00    
NDIMP1          !byte $00 

init            ldx NDIM
                dex 
                stx NDIML1
                inx
                inx
                stx NDIMP1
                lda BOARDCHAR
                ldx #$00
;
_initloop       sta CHESSBOARD,x 
                inx
                cpx #$FF
                bne _initloop
                rts

;==========================================================
; Run
; Usage:
;  ldx #$00         ; (row no, 0..7)
;  jsr run
;==========================================================

_RUNX           !byte $00
_RUNY           !byte $00  
_LASTRUNY       !fill $0C, $00 
_FOUNDCOUNTL    !byte $00
_FOUNDCOUNTH    !byte $00

run
                stx _RUNX             ; store location
                ldy #$00

_rungo          sty _RUNY   
                jsr offsetcalc          ; calculate mem offset
                ldy _OFFSET             ; get offset from stack
                lda CHESSBOARD,y        ; get chessboard cell
                cmp BOARDCHAR           ; check if empty
                bne _runnext
                ldx _RUNX
                lda _RUNY
                sta _LASTRUNY, x
                tay
                jsr place 
                ldx _RUNX
                inx 
                cpx NDIM
                beq _runfound
                jmp run                                     

_runnext        ldx _RUNX
                ldy _RUNY
                iny
                cpy NDIM 
                bne _rungo 
                
_runprevrow     dex
                cpx #$FF
                beq _runend

                stx _RUNX
                jsr cleanrow

_runnexty       ldx _RUNX
                ldy _LASTRUNY, x
                iny
                cpy NDIM 
                beq _runprevrow    

                jmp _rungo 

_runfound       inc _FOUNDCOUNTL
                bne _runfoundprint
                inc _FOUNDCOUNTH

_runfoundprint  jsr printboard
                ldx _RUNX
                jsr cleanrow
                jmp _runnexty

_runend         
                ldx #<.found ;string address LSB
                ldy #>.found ;string address MSB
                jsr sprint
                lda _FOUNDCOUNTH
                ldx _FOUNDCOUNTL
                jsr BUINTOUT
                lda #$0D     ; go to next line
                jsr BSOUT
                rts                

.found          !pet $0D, "solutions found: ", $00 


;==========================================================
; Wait for a key pressed
;==========================================================
waitkey         jsr $FFE4
                beq waitkey
                rts


;==========================================================
; Clean the board by specific queen
; Usage:
;  ldx #$01 (queen row no, 0..7)
;  jsr cleanrow
;==========================================================
_CLEANVALUE     !byte $00
_CLEANX         !byte $00
_CLEANCOUNTER   !byte $00

cleanrow        
                stx _CLEANX             ; search for queen
                ldy #$00
                jsr offsetcalc          ; calculate mem offset
                ldy _OFFSET             ; get offset from stack
                ldx #$00
                lda QUEENCHAR 
_cleanrowloop1  cmp CHESSBOARD,y
                beq _cleanrowstart
                iny
                inx                     ; increase counter
                cpx NDIM                ; end of row?
                bne _cleanrowloop1  
                
_cleanrowstart  lda BOARDCHAR           ; remove queen
                sta CHESSBOARD,y 
                ldy #$00                ; start cleaning
                ldx #$00
                sty _CLEANCOUNTER
                lda _CLEANX             ; calculate value
                clc         
                adc #$30
                sta _CLEANVALUE         
                
_cleanrowloop2  lda _CLEANVALUE
                cmp CHESSBOARD,y 
                bne _cleanrownext
                lda BOARDCHAR
                sta CHESSBOARD,y 

_cleanrownext   iny                     ; increase counters
                inx
                cpx NDIM
                bne _cleanrowloop2
                ldx #$00
                inc _CLEANCOUNTER
                lda _CLEANCOUNTER
                cmp NDIM
                bne _cleanrowloop2
;
                rts                     ; return

;==========================================================
; Place and fill cells
; Usage:
;  ldx #$01         ; (row no, 0..7)
;  ldy #$01         ; (column no, 0..7)
;  jsr place        
;==========================================================

_PLACEX         !byte $00
_PLACEY         !byte $00
_PLACEXLOOP     !byte $00
_PLACEYLOOP     !byte $00
_PLACEXCHAR     !byte $00

place
                stx _PLACEX             ; store location
                sty _PLACEY
                txa
                clc
                adc #$30
                sta _PLACEXCHAR
;
_placerow                               ; fill the row
                ;ldx _PLACEX
                ldy #$00
                jsr offsetcalc          ; calculate mem offset
                ldy _OFFSET             ; get offset from stack                
                ldx #$00                ; first column
                
;
_placerowloop   
                lda CHESSBOARD,y        ; get chessboard cell
                cmp BOARDCHAR                ; check if empty
                bne _placerownext
                lda _PLACEXCHAR         ; load filling char 
                sta CHESSBOARD,y        ; set chessboard cell
_placerownext   iny                     ; increase counters
                inx
                cpx NDIM                ; end of row?
                bne _placerowloop                
;
_placecol                               ; fill the column
                ldy _PLACEY
                ldx #$00
                jsr offsetcalc          ; calculate mem offset
                ldy _OFFSET             ; get offset from stack                
                ldx #$00                ; first row
;
_placecolloop   
                lda CHESSBOARD,y        ; get chessboard cell
                cmp BOARDCHAR                ; check if empty
                bne _placecolnext
                lda _PLACEXCHAR         ; load filling char
                sta CHESSBOARD, y       ; set chessboard cell
_placecolnext   lda _OFFSET             ; decrease offset by 8
                clc
                adc NDIM
                sta _OFFSET             ; store new offset
                tay
                inx                     ; increase counter
                cpx NDIM                ; end of row?
                bne _placecolloop     
;
_placediag                              ; fill diag (->)
                ldy _PLACEY
                ldx _PLACEX
                stx _PLACEXLOOP         ; store indexes
                sty _PLACEYLOOP
                jsr offsetcalc          ; calculate mem offset
;
_placediagloop  ldx _PLACEXLOOP         ; move indexes ->
                ldy _PLACEYLOOP
                inx
                iny
                cpx NDIM                ; end of row?
                beq _placeback     
                cpy NDIM                ; end of column?
                beq _placeback  
                stx _PLACEXLOOP         ; store indexes
                sty _PLACEYLOOP    
                lda _OFFSET             ; increase offset by 9
                clc
                adc NDIMP1
                sta _OFFSET       
                tay 
                lda CHESSBOARD,y        ; get chessboard cell
                cmp BOARDCHAR                ; check if empty
                bne _placediagloop                 
                lda _PLACEXCHAR         ; load filling char
                sta CHESSBOARD, y       ; set chessboard cell
                jmp _placediagloop         
_placeback                              ; fill diag (<-)
;
                ldy _PLACEY             
                ldx _PLACEX
                stx _PLACEXLOOP         ; restore indexes
                sty _PLACEYLOOP
                jsr offsetcalc          ; calculate mem offset
;
_placebackloop  ldx _PLACEXLOOP         ; move indexes <-
                ldy _PLACEYLOOP
                dex
                dey
                cpx #$FF                ; end of row?
                beq _placediag2     
                cpy #$FF                ; end of column?
                beq _placediag2  
                stx _PLACEXLOOP         ; store indexes
                sty _PLACEYLOOP    
                lda _OFFSET             ; decrease offset by 9
                sec
                sbc NDIMP1
                sta _OFFSET
                tay
                lda CHESSBOARD,y        ; get chessboard cell
                cmp BOARDCHAR                ; check if empty
                bne _placebackloop   
                lda _PLACEXCHAR         ; load filling char 
                sta CHESSBOARD, y       ; set chessboard cell   
                jmp _placebackloop
;
_placediag2                              ; fill diag (->)
                ldy _PLACEY
                ldx _PLACEX
                stx _PLACEXLOOP         ; store indexes
                sty _PLACEYLOOP
                jsr offsetcalc          ; calculate mem offset
;
_placediag2loop ldx _PLACEXLOOP         ; move indexes ->
                ldy _PLACEYLOOP
                dex
                iny
                cpx #$FF                ; end of row?
                beq _placeback2     
                cpy NDIM                ; end of column?
                beq _placeback2  
                stx _PLACEXLOOP         ; store indexes
                sty _PLACEYLOOP    
                lda _OFFSET             ; increase offset by 9
                sec
                sbc NDIML1
                sta _OFFSET       
                tay 
                lda CHESSBOARD,y        ; get chessboard cell
                cmp BOARDCHAR                ; check if empty
                bne _placediag2loop                 
                lda _PLACEXCHAR         ; load filling char
                sta CHESSBOARD, y       ; set chessboard cell
                jmp _placediag2loop  
_placeback2                              ; fill diag (->)
                ldy _PLACEY
                ldx _PLACEX
                stx _PLACEXLOOP         ; store indexes
                sty _PLACEYLOOP
                jsr offsetcalc          ; calculate mem offset
;
_placeback2loop ldx _PLACEXLOOP         ; move indexes ->
                ldy _PLACEYLOOP
                inx
                dey
                cpx NDIM                ; end of row?
                beq _placequeen     
                cpy #$FF                ; end of column?
                beq _placequeen  
                stx _PLACEXLOOP         ; store indexes
                sty _PLACEYLOOP    
                lda _OFFSET             ; increase offset by 9
                clc
                adc NDIML1
                sta _OFFSET       
                tay 
                lda CHESSBOARD,y        ; get chessboard cell
                cmp BOARDCHAR                ; check if empty
                bne _placeback2loop                 
                lda _PLACEXCHAR         ; load filling char
                sta CHESSBOARD, y       ; set chessboard cell
                jmp _placeback2loop                  
_placequeen                   
;
                ldx _PLACEX
                ldy _PLACEY
                jsr offsetcalc          ; calculate mem offset
                ldy _OFFSET             ; get offset from stack
                lda QUEENCHAR                ; load filling char
                sta CHESSBOARD,y        ; set chessboard cell

                rts



;==========================================================
; Calculate memory offset
; Usage:
;  ldx #$01         ; (row no, 0..7)
;  ldy #$01         ; (column no, 0..7)
;  jsr offsetcalc   ; result in _OFFSET 
;  lda _OFFSET 
;==========================================================

_OFFSET         !byte $00

offsetcalc      
                lda #$00                ; reset offset
_offsetrow      cpx #$00                ; last row?
                beq _offsetcolumn       ; offset calculated, jmp
                dex                     ; decrease row no
                clc                     ; clear carry (for adc)
                adc NDIM                ; add one row offset
                jmp _offsetrow          ; loop
_offsetcolumn   sta _OFFSET             ; store row offset in menory
                tya                     ; move column no (y) to a
                clc                     ; clear carry (for adc)
                adc _OFFSET             ; add column offset
                sta _OFFSET             ; store again in memory
                rts

;==========================================================
; Print board contents
; Usage:
;  jsr printboard
;==========================================================

_CURSORX        !byte $FF
_CURSORY        !byte $00
_ROWCOUNT       !byte $00

printboard      lda _CURSORX
                cmp #$FF
                bne _printsetcursor
                sec
                jsr CURSORPOS
                stx _CURSORX
                sty _CURSORY
                jmp _printboardinit
;
_printsetcursor ldx _CURSORX
                ldy _CURSORY
                clc
                jsr CURSORPOS 
;                               
_printboardinit ldx #$00                ; reset chars counter
                stx _ROWCOUNT
                lda #$0D                ; go to next line
                jsr BSOUT               
; 
_printrow       ldy #$00                ; reset row counter
_printrowloop   lda CHESSBOARD,x        ; get chessboard content
                cmp QUEENCHAR           ; is it a queen?
                beq _printrowchar
                lda BOARDCHAR           ; load board fill char
_printrowchar   jsr BSOUT               ; print it
                inx                     ; increase counters                
                iny                     
                cpy NDIM                ; end of row?
                bne _printrowloop
;
                lda #$0D                ; go to next line
                jsr BSOUT
;
                inc _ROWCOUNT           ; increase row counter
                lda _ROWCOUNT
                cmp NDIM                ; end of board?
                bne _printrow
;
                rts                     ; return
                

;==========================================================
; SPRINT (null terminated string print) 
; Usage:
;  ldx #<.hello ;string address LSB
;  ldy #>.hello ;string address MSB
;  jsr sprint
;==========================================================

sprint          stx BSOUTPTR          ;save string pointer LSB
                sty BSOUTPTR+1        ;save string pointer MSB
                ldy #0                ;starting string index
;
_sprint01       lda (BSOUTPTR),y      ;get a character
                beq _sprint02         ;end of string
;
                jsr BSOUT             ;print character
                iny                   ;next
                bne _sprint01
;
_sprint02       rts                  ;exit
