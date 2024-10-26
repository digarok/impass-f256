** SPE - "Simple PSG Engine" byDagenG.Brocktheoneandonly
**
** So I started this as an immediate "play sound" set of routines, but we
** need a way to shape sounds over time.
** I've decided to implement this by splitting up the calls to play sounds
** and the engine that actually plays them and updates them.
** When you make a call to play a sound it creates the sound parameters in
** a table.
** Every tick, whether called directly, or via interrupt, we trigger the
** main sound engine routine to do the needful. (Does it know about new sounds vs updates?)

**
** PSG Notes
**   - 3 square waves + 1 noise channel
**   - 16 levels of volume (attenuation of "0" is loudest, "F" is silent)
**   - noise generator can be tied to tone 3, not sure if I will support yet
**   - PSG Left is 0xD600    \
**   - PSG Right is 0xD610    >  I/O page 0
**   - PSG Both is 0xD608    /
**   - PSG tone is 10 bit (2-byte)
**
**
** D7 D6 D5 D4 D3 D2 D1 DO | Purpose
** == == == == == == == == |
**  1 R2 R1 R0 F3 F2 F1 F0 | Set the low four bits of the frequency
**  0  x F9 F8 F7 F6 F5 F4 | Set the high six bits of the frequency
**  1  1  1  0  x FB F1 F0 | Set the type and frequency of the noise generator
**  1 R2 R1 R0 A3 A2 A1 A0 | Set the attenuation (four bits)

** ZP usage
]speptr         =   $E0         ; put me wherever works for your program
]spetblptr      =   $E2
]spechn         =   $E4         ; quick access to current channel (byte)
]spetmp         =   $E5         ; tmp byte for bitlogicking



* table of the registers for channel frequency 0-3 (3=noise)
* ignore low nibble
PSGChanRegFreq  db  %0000_0000  ; Tone 1 - Frequency Register
                db  %0010_0000  ; Tone 2 - Frequency Register
                db  %0100_0000  ; Tone 3 - Frequency Register
                db  %0110_0000  ; Noise  - Control Register
* table of the registers for channel attenuation 0-3 (3=noise)
PSGChanRegAttn  db  %1001_0000  ; Tone 1 - Attenuation Register
                db  %1011_0000  ; Tone 2 - Attenuation Register
                db  %1101_0000  ; Tone 3 - Attenuation Register
                db  %1111_0000  ; Noise  - Attenuation Register


PSNDTypeTone    equ #%00000001
PSNDTypeNoise   equ #%00000010

PSNDToneUp      equ #%00000100
PSNDToneDown    equ #%00001000
PSNDToneShape   equ #%01000000  ; undefined but cool idea

PSNDVolDown     equ #%00010000
PSNDVolUp       equ #%00100000
PSNDVolShape    equ #%10000000  ; undefined but cool idea




SPEStopAllTriggers lda #$FF
                sta SPESnd0+#7
                sta SPESnd1+#7
                sta SPESnd2+#7
                sta SPENoise3+#7
                rts

SPEStopAll
                lda MMU_IO_CTRL
                pha
                lda #$00        ; Switch to I/O Page #1
                sta MMU_IO_CTRL

                lda #%1001_1111 ;($9f)
                sta $D608
                lda #%1011_1111
                sta $D608
                lda #%1101_1111
                sta $D608
                lda #%1111_1111
                sta $D608
                pla
                sta MMU_IO_CTRL
                rts

SPEPlayInfoDbgStr asc 'SPEPlaySnd',00

**  SPETRIGGERSND PSNDShot;#1;#$00  ; sound on channel 1, full vol L (0) full vol R (0)
SPETRIGGERSND   MAC
                lda #<]1
                sta ]speptr
                lda #>]1
                sta ]speptr+1
                lda #]3
                sta ]spetmp
                lda #]2
                sta ]spechn
                jsr SPETriggerSnd
                <<<

* value passed in A (0-3)
SPESetSndPtr    asl
                tax
                lda SPESndsPtrs,X ; set ptr to snds table
                sta ]spetblptr
                lda SPESndsPtrs+1,X
                sta ]spetblptr+1
                rts

** Do we ever need to check the current sound and turn it off? I dunno.
** A contains channel/num
SPETriggerSnd   jsr SPESetSndPtr
                GOXY #1;#35
                ldy #0          ; going to copy 6 bytes
:copysndtotbl   lda (]speptr),y
                sta (]spetblptr),y

                pha
                phx
                phy
                jsr PrHexSafe
                ply
                plx
                pla


                iny
                cpy #6
                bne :copysndtotbl
                sta (]spetblptr),y ; copies volume delay to current
                iny
                lda ]spetmp
                sta (]spetblptr),y ; copies initial volume to current volume
                rts

** just play?  or also update next vals (i think yes)
SPEProcessSndTbl
                lda #0
                sta ]spechn
                jsr SPEProcessSnd ; @todo: the rest of 'em ... lol
                rts


** FOR REF FROM BELOW
* SPESND          MAC
*                 db  00          ; * 00 - parameter byte
*                 dw  0000        ; * 01/02 - current freq (10 bits)
*                 dw  0000        ; * 03/04 - freq shift amount (tone factor)
*                 db  0           ; * 05 - volume delay factor
*                 db  0           ;D* 06 - current volume delay
*                 db  0           ;D* 07 - current volume
*                 <<<


** channel/num in A
SPEProcessSnd   jsr SPESetSndPtr ; sets ]spetblptr

                GOXY #1;#30     ; debug
                lda #$8f        ;
                jsr PrintCharSafe ;

                lda MMU_IO_CTRL
                pha
                lda #$00        ; Switch to I/O Page #1
                sta MMU_IO_CTRL

                ldy #07         ; current volume
                lda (]spetblptr),y
                lsr
                lsr
                lsr
                lsr             ; left value
                ldx ]spechn
                ora PSGChanRegAttn,x
                sta $D600       ; left channel vol

                jsr PrHexSafe   ; debug
                ldy #07
                ldx ]spechn     ;

                lda (]spetblptr),y
                and #$0F        ; right value
                ora PSGChanRegAttn,x
                sta $D610       ; right channel vol

                jsr PrHexSafe   ; debug

                lda #$90        ;
                jsr PrintCharSafe ;
                lda #' '         ;
                jsr PrintCharSafe ;
                lda #' '         ;
                jsr PrintCharSafe ;
                lda #' '         ;
                jsr PrintCharSafe ;

                ldx ]spechn     ;



                ldy #1          ; freq low
                lda (]spetblptr),y
                and #$0F        ; set low 4 bits
                ora PSGChanRegFreq,x
                ora #%1000_0000 ; low byte command first
                sta $D608       ; both channels

                jsr PrHexSafe   ; debug

                ldy #1          ; freq low (other nibble)
                lda (]spetblptr),y
                lsr
                lsr
                lsr
                lsr             ; shift 4 high bit low
                sta ]spetmp     ; temp holds freq partial xxxxNNNN

                ldy #2
                lda (]spetblptr),y ; now freq high
                asl
                asl
                asl
                asl
                and #%0011_1111 ; for safety.. this protects from bad values but if you think your data is good, remove. lol ;)
                ora ]spetmp
                sta $D608
                jsr PrHexSafe   ; debug


                GOXY #13;#31    ; debug
                ldy #2          ; freq high
                lda (]spetblptr),y
                jsr PrHexSafe
                ldy #1          ; freq low
                lda (]spetblptr),y
                jsr PrHexSafe


                                ;; post process attributes
                lda (]spetblptr) ; attrib byte
:checktone      bit #PSNDToneUp
                beq :nodectone
:dectone        ldy #1
                lda (]spetblptr),y
                ldy #3
                sec
                sbc (]spetblptr),y
                ldy #1
                sta (]spetblptr),y
                ldy #2
                lda (]spetblptr),y
                ldy #4
                sbc (]spetblptr),y
                ldy #2
                sta (]spetblptr),y
                bra :checkvol
:nodectone      bit #PSNDToneDown
                beq :checkvol
:inctone        ldy #1
                lda (]spetblptr),y
                ldy #3
                clc
                adc (]spetblptr),y
                ldy #1
                sta (]spetblptr),y
                ldy #2
                lda (]spetblptr),y
                ldy #4
                adc (]spetblptr),y
                ldy #2
                sta (]spetblptr),y
                                ; bra :checkvol
:checkvol       lda (]spetblptr) ; attrib byte
                bit #PSNDVolUp
                beq :noincvol
:incvol         ldy #6
                lda (]spetblptr),y ; current vol delay
                beq :incvolnow
                dec
                sta (]spetblptr),y
                bra :checkout
:incvolnow      ldy #5
                lda (]spetblptr),y
                iny
                sta (]spetblptr),y ; reset vol delay

                ldy #7
                lda (]spetblptr),y
                and #$0F
                sta ]spetmp
                beq :r_already_0
                dec
                sta ]spetmp
:r_already_0    lda (]spetblptr),y
                and #$f0
                beq :l_already_0
                sec
                sbc #$10
:l_already_0
                ora ]spetmp
                sta (]spetblptr),y
                bra :checkout
:noincvol
                bit #PSNDVolDown
                beq :checkout
:decvol         ldy #6
                lda (]spetblptr),y ; current vol delay
                beq :decvolnow
                dec
                sta (]spetblptr),y
                bra :checkout
:decvolnow      ldy #5
                lda (]spetblptr),y
                iny
                sta (]spetblptr),y ; reset vol delay

                ldy #7
                lda (]spetblptr),y
                and #$0F
                cmp #$0F
                beq :r_already_F
                inc
:r_already_F    sta ]spetmp
                lda (]spetblptr),y
                and #$F0
                cmp #$F0
                beq :l_already_F
                clc
                adc #$10
:l_already_F
                ora ]spetmp
                sta (]spetblptr),y
                bra :checkout
:checkout
                pla
                sta MMU_IO_CTRL
                rts



SPESndsPtrs     adr SPESnd0
                adr SPESnd1
                adr SPESnd2
                adr SPENoise3

SPESndsTable
SPESnd0         SPESND
SPESnd1         SPESND
SPESnd2         SPESND
SPENoise3       SPESND


SPESND          MAC
                db  00          ; * 00 - parameter byte
                dw  0000        ; * 01/02 - current freq (10 bits)
                dw  0000        ; * 03/04 - freq shift amount (tone factor)
                db  0           ; * 05 - volume delay factor
                db  0           ;D* 06 - current volume delay
                db  0           ;D* 07 - current volume
                <<<


*SND is:
* 00 - parameter byte
* 01/02 - initial freq (10 bits)
* 03/04 - freq shift amount
* 05 - vol shift delay

* how would I define a pew pew?
PSNDShot0       db  PSNDTypeTone.PSNDToneDown.PSNDVolDown
                dw  $01FE       ; initial frequency concert A
                dw  0001        ; tone factor
                db  01          ; volume delay factor
                adr 0000        ; pointer to toneshape  - not implemented
                adr 0000        ; pointer to volshape   - not implemented


* how would I define a pew pew?
FFFF1           db  PSNDTypeTone&PSNDToneDown.PSNDVolDown
                db  PSNDTypeTone!PSNDToneDown&PSNDVolDown

PSNDShot1       db  PSNDTypeTone.PSNDToneDown.PSNDVolDown
                dw  $0320       ; initial frequency concert A
                dw  0002        ; tone factor
                db  02          ; volume delay factor
                adr 0000        ; pointer to toneshape  - not implemented
                adr 0000        ; pointer to volshape   - not implemented



** DEPRECATED... THIS IS JUST AN EXAMPLE OF IMMEDIATE TRIGGERING
                DO  0
** Example:
**  SPEPLAYSND PSNDShot;#1  ; sound on channel 1
SPEPLAYSND      MAC
                lda #<]1
                ldx #>]1
                ldy #]2
                jsr SPEPlaySnd
                <<<


* sound address in AX on channel y
SPEPlaySnd      sta ]speptr
                stx ]speptr+1
                sty ]spechn


                lda MMU_IO_CTRL
                pha
                lda #$00        ; Switch to I/O Page #1
                sta MMU_IO_CTRL


                ldx ]spechn
                lda PSGChanRegAttn,x
                ldy #1          ; initial atten
                ora (]speptr),y
                sta $D608       ; both channels

                ldx ]spechn
                ldy #3          ; freq low
                lda (]speptr),y
                and #$0F        ; set low 4 bits
                ora PSGChanRegFreq,x

                ora #%1000_0000 ; low byte command first

                sta $D608       ; both channels

                ldx ]spechn
                ldy #3          ; freq low
                lda (]speptr),y
                lsr
                lsr
                lsr
                lsr             ; shift 4 high bit low
                sta ]spetmp     ; temp holds freq partial xxxxNNNN

                ldy #4
                lda (]speptr),y ; now freq high
                asl
                asl
                asl
                asl
                and #%0011_1111 ; for safety.. this protects from bad values but if you think your data is good, remove. lol ;)
                ora ]spetmp
                sta $D608

                pla
                sta MMU_IO_CTRL
                rts

                fin
