; From this thread: https://www.lemon64.com/forum/viewtopic.php?t=70222
; 
;==========================================================
; Start a timer to count seconds (tenths of second precision)
; It uses CIA2 NMI (combination of timer A and B)
; Usage:
;  jsr starttimer
;  jsr stoptimer
;==========================================================

!zn
ELAPSED_SECONDS_H     !byte $00                
ELAPSED_SECONDS_L     !byte $00    
ELAPSED_TENTHS_L      !byte $00   

starttimer:                
                jsr     stoptimer

                lda     #<.tickerisr
                sta     $318
                lda     #>.tickerisr
                sta     $319

                ; 985248 (=$F08A0) cycles per second 
                ; 98525 cycles per tenth of second
                ; load timer A with $0180 (-1) and timer B with $0100 (-1)

                lda     #$7f
                sta     $dd04
                lda     #$01
                sta     $dd05
                lda     #$ff
                sta     $dd06
                lda     #$0
                sta     $dd07

                lda     #$11
                sta     $dd0e           ; start timer A counting cycles
                lda     #$51
                sta     $dd0f           ; start timer B counting t.A underflows

                lda     #$82
                sta     $dd0d           ; enable NMI on t.B underflows

                lda     #$00            ; reset counter value
                sta     ELAPSED_SECONDS_H
                sta     ELAPSED_SECONDS_L      
                sta     ELAPSED_TENTHS_L                    
                
                rts

.tickerisr:     pha
                lda     $dd0d            ; ack
                bpl     .done            ; NMI not from CIA2
                inc     ELAPSED_TENTHS_L ; inc tenths
                lda     ELAPSED_TENTHS_L 
                cmp     #$0A             ; >= 100?
                bcc     .done
                lda     #$00
                sta     ELAPSED_TENTHS_L   ; reset tenths
                inc     ELAPSED_SECONDS_L  ; inc seconds
                bne     .done
                inc     ELAPSED_SECONDS_H     
.done:          pla
                rti
         
stoptimer:                
                lda     #$7f
                sta     $dd0d            ; disable all CIA2 NMIs 
                rts

;==========================================================
; Print time elapsed in format "seconds.tenth of seconds"
; Usage:
;  jsr printtimer
;==========================================================
printtimer:
                lda ELAPSED_SECONDS_H
                ldx ELAPSED_SECONDS_L
                jsr $bdcd               ;BUINTOUT    
                lda #$2e
                jsr $ffd2               ;BSOUT
                lda #$00
                ldx ELAPSED_TENTHS_L
                jsr $bdcd               ;BUINTOUT    
                rts
