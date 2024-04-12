
RELEASE =   1
ptr_src =   $90
ptr_dst =   $92
tmp0    =   $94
event_data ds 16

KEY_LEFT =  #$B8
KEY_RIGHT = #$B9
KEY_SPACE = #$20
KEY_CTRL =  #$02
KEY_ESC_BACK = #$92

; MAIN PGZ ENTRY POINT
start   mx  %11
        ldax #event_data
        stax kernel_args_events


Attract
        jsr TextClearBuffer
        PRINTSTRXY ImpasseTitleStr;#36;#10
        PRINTSTRSXY StartInstructionsStrs;#20;#15

debugfunk
        jsr SetSprits
        jsr SetSpritesPalClut0
        jsr TestASprite
        * jmp MINIMON
        lda #$02                ; Swap I/O Page 2 into bank 6
        sta $0001

:lp     jsr WaitVBLPoll
        inc $c000+{80*18}+56    ; char
        jsr kernel_NextEvent
        bcs :no_events
        jsr HandleKernelEvent
        jsr IsKeyHit
        bcs StartGame
:no_events
        bra :lp


StartGame
        lda #1
        sta Level
:startRound
        jsr InitRound
        jsr InitShots
:loop
:input
        jsr kernel_NextEvent
        bcs :input_done
        jsr HandleKernelEvent
        jsr IsKeyHit
        bcc :input_done

        * GOXY #10;#10
        * lda KEY
        * jsr PrHex
:input_done

:update
        jsr UpdateShotCreation  ; shot before move?
        jsr UpdatePlayerXVel
                                ; or after?
        jsr UpdateObjectPositions
        jsr DrawSprites

        inc $c000
        GOXY #30;#0
        lda PlayerInputRaw+1
        jsr PrHex
        lda PlayerInputRaw
        jsr PrHex

        jsr WaitVBLPoll
        bra :loop




DrawSprites
        lda MMU_IO_CTRL
        pha
        stz MMU_IO_CTRL

                                ;; PLAYER
        lda PlayerXPos
        sta VKY_SP0_POS_X_L     ; (x, y) = (32, 32)... should be upper-left corner of the screen
        lda PlayerXPos+1
        sta VKY_SP0_POS_X_H

        lda PlayerYPos
        sta VKY_SP0_POS_Y_L
        lda PlayerYPos+1
        sta VKY_SP0_POS_Y_H

                                ;; SHOTS
        ldx #MaxShots
        lda #<ShotSprTbl
        sta ptr_src
        lda #>ShotSprTbl
        sta ptr_src+1
        lda #<ShotSprVky
        sta ptr_dst
        lda #>ShotSprVky
        sta ptr_dst+1
        ldy #16*8
:looop  lda (ptr_src),y
        sta (ptr_dst),y
        dey
        bpl :looop

        pla
        sta MMU_IO_CTRL
        rts

InitRound
        jsr TextClearBuffer
        jsr InitPlayer
        PRINTSTRXY #LevelStr;#20;#2
        lda Level
        jsr PrHex
        rts

InitPlayer
        lda #<PlayerXStart
        sta PlayerXPos
        lda #>PlayerXStart
        sta PlayerXPos+1

        lda #<PlayerYStart
        sta PlayerYPos
        lda #>PlayerYStart
        sta PlayerYPos+1
        rts

InitShots
        lda MMU_IO_CTRL
        pha
        stz MMU_IO_CTRL

        lda #<ShotSprTbl
        sta ptr_dst
        lda #>ShotSprTbl
        sta ptr_dst+1

        ldx #MaxShots
:init_shot ldy #0
        lda #%01000000
        sta (ptr_dst),y
        iny
        lda #<bullet_spr
        sta (ptr_dst),y
        iny
        lda #>bullet_spr
        sta (ptr_dst),y
        iny
        lda #^bullet_spr
        sta (ptr_dst),y
        iny
        lda #0
        sta (ptr_dst),y
        iny
        sta (ptr_dst),y
        iny
        sta (ptr_dst),y
        iny
        sta (ptr_dst),y
        lda ptr_dst
        clc
        adc #8                  ; next sprite start
        sta ptr_dst
        lda #0
        adc ptr_dst+1
        sta ptr_dst+1
        dex
        bne :init_shot
        pla
        sta MMU_IO_CTRL
        rts
* ** Sprite Controls Offsets
* SPR_CTRL =  $0                  ; 65=size  43=layer  21=lut  0=enable
* SPR_DATA_ADR = $1               ; 3-byte address to sprite data
* SPR_X   =   $4                  ; 2-byte xpos
* SPR_Y   =   $6                  ; 2-byte xpos

UpdateObjectPositions
        GOXY #40;#40
        lda PlayerXSpeed
        jsr PrHex
        lda PlayerXSpeed

        bmi :neg
        lsr
        lsr

:pos    clc
        adc PlayerXPos
        sta PlayerXPos
        lda #0
        adc PlayerXPos+1
        sta PlayerXPos+1
        bra :end
:neg    eor #$FF
        inc                     ; 2s compliment
        lsr
        lsr
        sta tmp0
        sec
        lda PlayerXPos
        sbc tmp0
        sta PlayerXPos
        lda PlayerXPos+1
        sbc #0
        sta PlayerXPos+1
:end    GOXY #70;#20
        lda PlayerXPos+1
        jsr PrHex
        lda PlayerXPos
        jsr PrHex

                                ;; SHOTS
        ldx #0
        ldy #0
:search_one lda ShotSprTbl,x
        bit #1
        beq :next
:up     lda ShotSprTbl+6,x      ; yval
        sec
        sbc #16
        sta ShotSprTbl+6,x      ; yval
        cmp #16
        bcs :next
:off    lda ShotSprTbl,x
        and #%11111110
        sta ShotSprTbl,x
:next   txa
        clc
        adc #8
        tax
        iny
        cpy #MaxShots
        bne :search_one
        rts

UpdateShotCreation
        lda PlayerInputRaw+1
:chk_a  bit #>INPUT_A
        beq :chk_done
        jsr FindEmptyShot
        bcc :chk_done
        pha
        GOXY #10;#10
        pla
        pha
        jsr PrHex
        pla
        asl
        asl
        asl                     ; *8
        tax
        lda ShotSprTbl,x
        ora #1
        sta ShotSprTbl,x
        lda PlayerXPos
        clc
        adc #8
        sta ShotSprTbl+4,x
        lda PlayerXPos+1
        sta ShotSprTbl+5,x
        lda PlayerYPos
        sta ShotSprTbl+6,x
        lda PlayerYPos+1
        sta ShotSprTbl+7,x
:chk_done
        rts


UpdatePlayerXVel
        lda PlayerInputRaw
:chk_l  bit #INPUT_LEFT
        beq :chk_r
        inc $c001
        lda PlayerXSpeed
        cmp #PlayerMinXSpeed
        beq :chk_done
        dec PlayerXSpeed
        bra :chk_done


:chk_r  bit #INPUT_RIGHT
        beq :chk_done
        inc $c002

        lda PlayerXSpeed
        cmp #PlayerMaxXSpeed
        beq :chk_done
        inc PlayerXSpeed
        bra :chk_done

:chk_done
        rts




FindEmptyShot
        ldx #0
        ldy #0
:search_one lda ShotSprTbl,x
        bit #1
        bne :next
        tya
        sec
        rts
:next   txa
        clc
        adc #8
        tax
        iny
        cpy #MaxShots
        bne :search_one
        clc
        rts



MaxShots =  16

ShotSprTbl ds 8*16
ShotSprVky = $D900+{16*8}       ; start at sprite 16




INPUT_A =   %0000_1000_0000_0000
INPUT_X =   %0000_0100_0000_0000
INPUT_L =   %0000_0010_0000_0000
INPUT_R =   %0000_0001_0000_0000
INPUT_B =   %0000_0000_1000_0000
INPUT_Y =   %0000_0000_0100_0000
INPUT_SELECT = %0000_0000_0010_0000
INPUT_START = %0000_0000_0001_0000
INPUT_UP =  %0000_0000_0000_1000
INPUT_DOWN = %0000_0000_0000_0100
INPUT_LEFT = %0000_0000_0000_0010
INPUT_RIGHT = %0000_0000_0000_0001

PlayerInputRaw dw 0

PlayerXStart = {320/2}+32
PlayerYStart = 180+32

PlayerXPos dw 0
PlayerYPos dw 0
PlayerXSpeed db 0
PlayerMaxXSpeed = 16
PlayerMinXSpeed = {0-PlayerMaxXSpeed}

Level   db  0

HandleKernelEvent
        GOXY #70;#11
        lda event_data+kernel_event_t
        jsr PrHex
        lda event_data+kernel_event_t
        cmp #kernel_event_key_PRESSED
        beq HandleKeyDown
        cmp #kernel_event_key_RELEASED
        beq HandleKeyUp
        rts

* reads the key to KEY and sets KEYHIT
HandleKeyDown
        GOXY #70;#12
        lda event_data+kernel_event_event_t_key_raw
        sta KEY
        jsr PrHex
        lda #-1
        sta KEYHIT

                                ; FOR GAME
        lda KEY
:k0     cmp #KEY_LEFT
        bne :k1
        lda #INPUT_LEFT
        tsb PlayerInputRaw
        rts
:k1     cmp #KEY_RIGHT
        bne :k2
        lda #INPUT_RIGHT
        tsb PlayerInputRaw
        rts
:k2     cmp #KEY_SPACE
        bne :k3
        lda #>INPUT_A
        tsb PlayerInputRaw+1
        rts
:k3
        rts

HandleKeyUp
        GOXY #74;#12
        lda event_data+kernel_event_event_t_key_raw
        sta KEY
        jsr PrHex
                                ; FOR GAME
        lda KEY
:k0     cmp #KEY_LEFT
        bne :k1
        lda #INPUT_LEFT
        trb PlayerInputRaw
        rts
:k1     cmp #KEY_RIGHT
        bne :k2
        lda #INPUT_RIGHT
        trb PlayerInputRaw
        rts
:k2     cmp #KEY_SPACE
        bne :k3
        lda #>INPUT_A
        trb PlayerInputRaw+1
        rts
:k3
        rts

* returns in carry, but clears so this is one-shot
IsKeyHit lda KEYHIT
        bne :hit
        clc
        rts
:hit    stz KEYHIT
        sec
        rts

KEY     db  0
KEYHIT  db  0






SetSprits
        lda MMU_IO_CTRL
        pha
        lda #$00                ; Switch to I/O Page #1
        sta MMU_IO_CTRL


        lda #$24                ; Graphics and Sprite engines enabled
        lda #%00111111
        sta VKY_MSTR_CTRL_0
        stz VKY_MSTR_CTRL_1     ; 320x240 @ 60Hz
        lda #$26                ; Background: lavender
        sta VKY_BKG_COL_R
        lda #$1B
        sta VKY_BKG_COL_G
        lda #$36
        sta VKY_BKG_COL_B
        pla
        sta MMU_IO_CTRL
        rts


BGtoAXY
        lda MMU_IO_CTRL
        pha
        lda #$00                ; Switch to I/O Page #1
        sta MMU_IO_CTRL


        lda #$26                ; Background: lavender
        sta VKY_BKG_COL_R
        lda #$1B
        sta VKY_BKG_COL_G
        lda #$36
        sta VKY_BKG_COL_B
        pla
        sta MMU_IO_CTRL
        rts

TestASprite
        lda MMU_IO_CTRL
        pha
        lda #$00                ; Switch to I/O Page #1
        sta MMU_IO_CTRL

init_sp0: lda #<ship_spr        ; Address = balls_img_start
        sta VKY_SP0_AD_L
        lda #>ship_spr
        sta VKY_SP0_AD_M
        stz VKY_SP0_AD_H

        lda #32
        sta VKY_SP0_POS_X_L     ; (x, y) = (32, 32)... should be upper-left corner of the screen
        stz VKY_SP0_POS_X_H

        lda #32
        sta VKY_SP0_POS_Y_L
        stz VKY_SP0_POS_Y_H

        lda #SPR_CTL_SIZE_32X32.SPR_CTL_LAYER_0.SPR_CTL_LUT_0.SPR_CTL_ENABLE
        * lda #$41                ; Size = 16x16, Layer = 0, LUT = 0, Enabled
        sta VKY_SP0_CTRL
        pla
        sta MMU_IO_CTRL
        rts

SetSpritesPalClut0
        lda MMU_IO_CTRL
        pha
        lda #$01                ; Switch to I/O Page #1
        sta MMU_IO_CTRL

        lda #<clut_x            ; Set the source pointer to the palette data
        sta ptr_src
        lda #>clut_x
        sta ptr_src+1

        lda #<VKY_GR_CLUT_0     ; Set the destination pointer to Graphics CLUT 1
        sta ptr_dst
        lda #>VKY_GR_CLUT_0
        sta ptr_dst+1

        ldx #0                  ; X is a counter for the number of colors copied
:color_loop ldy #0              ; Y is a pointer to the component within a CLUT color
:comp_loop lda (ptr_src),y      ; Read a byte from the code
        sta (ptr_dst),y         ; And write it to the CLUT
        iny                     ; Move to the next byte
        cpy #4
        bne :comp_loop          ; Continue until we have copied 4 bytes

        inx                     ; Move to the next color
        cpx #clut_x_len
        beq :done_lut           ; Until we have copied all 16

        clc                     ; Advance ptr_src to the next source color entry
        lda ptr_src
        adc #4
        sta ptr_src
        lda ptr_src+1
        adc #0
        sta ptr_src+1

        clc                     ; Advance ptr_dst to the next destination color entry
        lda ptr_dst
        adc #4
        sta ptr_dst
        lda ptr_dst+1
        adc #0
        sta ptr_dst+1

        bra :color_loop         ; And start copying that new color

:done_lut pla
        sta MMU_IO_CTRL
        rts


; 16 bit load
ldax    mac
        if  #=]1

        lda #<]1
        ldx #>]1

        else

        lda ]1
        ldx ]1+1

        fin
        <<<


; 16 bit store
stax    mac

        sta ]1
        stx ]1+1

        <<<



ImpasseTitleStr asc 'Impasse',00
StartInstructionsStrs asc 'Use arrows and space to fire, or joystick.',00
        asc ' ',00,' ',00
        asc '        Press key/button to start.',00,00
LevelStr asc 'Level ',00

