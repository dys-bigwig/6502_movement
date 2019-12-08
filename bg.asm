;- RPG walk
include header.asm

;- defines -
include define.asm

;- macros -
include macros.asm

;- reset -
include reset.asm


; - load first screen ;

LoadPalettes:
    LDA $2002             ; read PPU status to reset the high/low latch
    LDA #$3F
    STA $2006             ; write the high byte of $3F00 address
    LDA #$00
    STA $2006             ; write the low byte of $3F00 address
    LDX #$00              ; start out at 0
LoadPalettesLoop:
    LDA palette, x        ; load data from address (palette + the value in x)
                          ; 1st time through loop it will load palette+0
                          ; 2nd time through loop it will load palette+1
                          ; 3rd time through loop it will load palette+2
                          ; etc
    STA $2007             ; write to PPU
    INX                   ; X = X + 1
    CPX #$20              ; Compare X to hex $10, decimal 16 - copying 16 bytes = 4 sprites
    BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
                          ; if compare was equal to 32, keep going down

LoadSprites:
    LDX #$00              ; start at 0
LoadSpritesLoop:
    LDA sprites, x        ; load data from address (sprites +  x)
    STA $0200, x          ; store into RAM address ($0200 + x)
    INX                   ; X = X + 1
    CPX #$04              ; Compare X to hex $10, decimal 16
    BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was Not Equal to zero


LoadBackground:
    LDA $2002             ; read PPU status to reset the high/low latch
    LDA #$20
    STA $2006             ; write the high byte of $2000 address
    LDA #$00
    STA $2006             ; write the low byte of $2000 address
InitCounters:
    LDX #$04
    LDY #$00
SetPtr:
    LDA #<background
    STA bg_ptr_lo
    LDA #>background
    STA bg_ptr_hi
LoadBackgroundLoop:
    LDA (bg_ptr_lo),y
    STA $2007
    INY
    BNE LoadBackgroundLoop
    INC bg_ptr_hi
    DEX
    BNE LoadBackgroundLoop

; - enable rendering -

    LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
    STA $2000

    LDA #%00011110   ; enable sprites, enable background, no clipping on left side
    STA $2001

InitPlayerPos:
    LDA #$80
    STA player_y
    LDA #$80
    STA player_x

; - actual game loop start here -

WaitNMI:
    LDA nmi_counter
  - CMP nmi_counter
    BEQ -

Main:
    JSR ReadJoy
IfMoving:
    LDA moving?
    BNE ButtonsDone     ;if we're already moving, ignore player input

TestDirections:
    LDA buttons
    AND #%00001111      ;is U D L or R pressed?
    BEQ ButtonsDone     ;if not, we're done checking buttons

    STA direction       ;otherwise, store the button as the current direction
    SEC             
    SBC #$01            ;A AND (A - 1) is the same as ensuring only one bit is set
    AND direction       ;this is to prevent multiple directions/diagonal movement
    BNE ButtonsDone     ;if A is not zero, more than one direction is pressed, so we do nothing and skip ahead
    LDA #$01
    STA moving?         ;otherwise, we set moving to true (1 = true, 0 = false)

ButtonsDone:
    LDA moving?         ;we need to check if we're moving again here,
                        ;as we don't know if we jumped here from the IfMoving check,
                        ;or continued onto here after testing buttons,
                        ;the act of which may have caused moving to be set to true from false
    BEQ EndMain         ;if not moving, we're done, wait for NMI and go around again
    JSR MovePlayer      ;otherwise, move the player

EndMain:
    LDA player_x
    STA $0203           ;0203 is the location of the x_position of the
                        ;sprite which represents the player
    LDA player_y
    STA $0200           ;ditto above for y
    JMP WaitNMI         ;from the top!


MovePlayer:

IfRight:
    LDA direction       ;this is set by the button/direction checking code above
    AND #%00000001      ;is it right? (ABSSUDLR)
    BEQ IfLeft          ;if no, see if it was left

    LDA player_x_sub    ;if yes, get the numbers after the decimal point from player_x
    CLC                 ;clear carry before ADC
    ADC #$4A            ;add 74 ($4A in hex) to the fractional part of player_x
    STA player_x_sub    
    BCC CheckAlignment  ;if the carry was set, that means after addition the fractional part was >= 1
                        ;so we need to increment the number before the decimal point
    INC player_x

IfLeft:
    LDA direction
    AND #%00000010
    BEQ IfDown

    LDA player_x_sub
    SEC
    SBC #$4A
    STA player_x_sub
    BCS CheckAlignment
    DEC player_x

IfDown:
    LDA direction
    AND #%00000100
    BEQ IfUp

    LDA player_y_sub
    CLC
    ADC #$4A
    STA player_y_sub
    BCC CheckAlignment
    INC player_y

IfUp:
    LDA direction
    AND #%00001000
    BEQ CheckAlignment

    LDA player_y_sub
    SEC
    SBC #$4A
    STA player_y_sub
    BCS CheckAlignment
    DEC player_y

CheckAlignment:
    LDA player_x        ;if we AND player_x with player_x - 1
    AND #$07            ;that's equivalent to checking if it's evenly divisible by 8
                        ;which allows us to check whether we're aligned with the grid
                        ;as the grid tiles are 8x8 pixels in size

    BNE MoveDone        ;if A is not 0 after the above AND, 
                        ;we're not yet aligned with grid, so stay in moving state for next frame

    LDA player_y        ;ditto above for y
    AND #$07
    BNE MoveDone

    LDA #$00            ;if we get here, that means we're aligned,
    STA moving?         ;so set moving to false (0) as one full tile movement is complete

MoveDone:
    RTS

;;;;;;;;;

NMI:
    INC nmi_counter
    
    PushRegs

    LDA #$00
    STA $2003       ; set the low byte (00) of the RAM address
    LDA #$02
    STA $4014       ; set the high byte (02) of the RAM address, start the transfer

PPUCleanup:
  ;;This is the PPU clean up section, so rendering the next frame starts properly.
    LDA #%10010000   ; enable NMI, sprites from Pattern Table 0, background from Pattern Table 1
    STA $2000
    LDA #%00011110   ; enable sprites, enable background, no clipping on left side
    STA $2001
    LDA #$00        ;;tell the ppu there is no background scrolling
    STA $2005
    STA $2005

    PullRegs

    RTI


ReadJoy:
    ;A B Select Start Up Down Left Right

    LDA #$01          ; While the strobe bit is set, buttons will be continuously reloaded.
                      ; This means that reading from JOYPAD1 will only return the state of the
                      ; first button: button A.
    STA JOYPAD1
    STA buttons
    LSR A             ; now A is 0
                      ; By storing 0 into JOYPAD1, the strobe bit is cleared and the reloading stops.
                      ; This allows all 8 buttons (newly reloaded) to be read from JOYPAD1.
    STA JOYPAD1
  - LDA JOYPAD1
    LSR A	      ; bit 0 -> Carry
    ROL buttons       ; Carry -> bit 0; bit 7 -> Carry
    BCC -
    RTS



;;;;;;;;;;;;;;  
  
  
  

  .org $E000
palette:
  .db $22,$29,$1A,$0F,  $22,$36,$17,$0F,  $22,$30,$21,$0F,  $22,$27,$17,$0F   ;;background palette
  .db $22,$1C,$15,$14,  $22,$02,$38,$3C,  $22,$1C,$15,$14,  $22,$02,$38,$3C   ;;sprite palette

sprites:
     ;vert tile attr horiz
  .db $80, $00, $00, $80   ;sprite 0

background:
  .bin bg.nam

attribute:
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
  .db %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

  .org $FFFA
  .dw NMI
  .dw RESET
  .dw 0 

;;;;;;;;;;;;;;  

  .incbin "bg.chr"   ;includes 8KB graphics file from SMB1
