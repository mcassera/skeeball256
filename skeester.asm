; Skee-Ster Ball or Rolling with your Peeps!
; 2025 Michael Cassera
; skee-ball inpired Easter Jam Game

.cpu "w65c02"				                ; set the cpu to Western Digital 65C02
.include "setup.asm"		                ; all of our initial settings

tmp = $84
tx 	= $86
ty	= $87
bx	= $88 
by	= $89 
aa	= $8a 
bb	= $8c 
cc 	= $8e

*=$a0										; Set up buffer for Kernel communication
.dsection zp						        ; Define position for zp (zero page)
.cerror * > $af, "Too many Zero page variables"

*=$1ffd

start:										; ***TEMP CODE FOR PROGRAMMING***
		jmp SC								; Start of the program - We put this jump here so you can load the PGZ into the computer.
											; With the self running code below, you can boot into it in RAM. Without this jump, loading a PGZ will
											; hit the self running code the kernel needs at the start of the slot at $2000 and look like a freeze.
											; hitting the reset on the back of the F256k will start the program.



.include "api.asm"							; This is the Kernel API for communication

;SetupKernel:								; Set up the API to work with

.section zp									; Zero page section $a0 to $a8
event:	.dstruct	kernel.event.event_t
.send


; ************************************************************************************************************************************
*=$2000										; ***TEMP CODE FOR PROGRAMMING***
											; Self running code to send via USB port the F256. Allows me to quickly load into emulator
											; dipswitch 1 should be set to on.
		.byte $f2,$56						; Required bytes for the Kernel to identify
		.byte $04,$01						; how big is the program in 8K sections, What slot to map to
		.byte $0b,$20						; the starting address of your program
		.byte $00,$00,$00,$00				; reserved
		.byte $00							; terminating byte
; ************************************************************************************************************************************
											; *****My program starts here!*****
SC:


		stz MMU_IO_CTRL						; should do this on every program



; ************************************************************************************************************************************

;init_events:
        lda #<event
        sta kernel.args.events
        lda #>event
        sta kernel.args.events+1

; ************************************************************************************************************************************
;Set up TinyVicky to display sprites
		lda #%00110111						; Graphic, Sprites Engine enabled  			|xx|GM|SP|TL|BM|GR|OV|TX|
		sta VKY_MSTR_CTRL_0					; Text overlay enabled						| 0| 0| 1| 1| 0| 1| 1| 1|

		lda #%00000110						; Text mode options for the overlay 		|xx|xx|FS|FO|MS|2Y|2X|70|
		sta VKY_MSTR_CTRL_1					; 320 x 240, 60 Hz, dbl X & Y				| 0| 0| 0| 0| 0| 1| 1| 0|
		stz VKY_BRDR_CTRL					; No Border

		lda #$00							; Set the background color
		sta VKY_BKG_COL_R
		lda #$00
		sta VKY_BKG_COL_G
		lda #$00
		sta VKY_BKG_COL_B

		jsr clrScreen

;Set up TinyVicky to display tiles
		lda #%00000000						;no tiles for layer 0 and 1	               |xx|LA YE R1|xx|LA YE R0|
		sta VKY_LAYER_CTRL_0				;						 				   | 0| 0| 0| 0| 0| 0| 0| 0|
		lda #%00000100						;Layer 2 = TileMap 2					   |xx|xx|xx|xx|xx|LA YE R2|	
		sta VKY_LAYER_CTRL_1				;										   | 0| 0| 0| 0| 0| 1| 0| 0|

;Set TileSet 0 for our background
		lda #<tileset
		sta VKY_TS0_AD_L
		lda #>tileset
		sta VKY_TS0_AD_M
		lda #$01							
		sta VKY_TS0_AD_H

;Set Tile Map 0
		lda #%00000001						; 16 x 16 tiles, enable					   |xx|xx|xx|TS|xx|xx|xx|EN|
		sta VKY_TM0_CTRL					;										   | 0| 0| 0| 0| 0| 0| 0| 1|
		lda #20								; Tile Map Size 20 X 
		sta VKY_TM0_SZ_X
		lda #15								; Tile Map Size 15 Y
		sta VKY_TM0_SZ_Y

		lda #<background_map				; Point to the Tile Map Data, LOW BYTE
		sta VKY_TM0_AD_L
		lda #>background_map				; Point to the Tile Map Data, MEDIUM BYTE
		sta VKY_TM0_AD_M
		lda #$01							; Point to the Tile Map Data, HIGH BYTE
		sta VKY_TM0_AD_H

; ************************************************************************************************************************************

; turn on random number generator
		lda #$01
		sta Random_Reg

; ************************************************************************************************************************************

		;POWER indicator sprite
		lda #%01010001 						; 16x16 sprite, layer 2, lut 0, enable on
		sta $d948							; from sprite 0 in Vicky indexed to y
		sta $d950
		lda #$93
		sta $d948+SP_POS_X_L
		lda #$df
		sta $d950+SP_POS_X_L
		stz $d948+SP_POS_X_H
		stz $d950+SP_POS_X_H
		lda #$64
		sta $d948+SP_POS_Y_L
		sta $d950+SP_POS_Y_L
		lda #$00
		sta $d948+SP_POS_Y_H
		sta $d950+SP_POS_Y_H
		lda #<sprite12
		sta $d948+SP_AD_L
		sta $d950+SP_AD_L
		lda #>sprite12
		sta $d948+SP_AD_M
		sta $d950+SP_AD_M
		lda #$01
		sta $d948+SP_AD_H
		sta $d950+SP_AD_H


; ************************************************************************************************************************************
;Load the CLUT into memory
		lda #$01							; Change I/O control to page 1
		sta MMU_IO_CTRL
		lda #<CLUT0							; Set source pointer to CLUT for color information
		sta ptr_src
		lda #>CLUT0
		sta ptr_src+1

		lda #<VKY_GR_CLUT_0					; Set destination pointer to Graphics CLUT 0
		sta ptr_dst
		lda #>VKY_GR_CLUT_0
		sta ptr_dst+1

		ldx #$00							; Y is the number of colors to copy, check for 128
		ldy #128
; ************************************************************************************************************************************
makeClut:
		sty totalColors
color_loop:
		ldy #$00							; Y points to the color component (Blue Red Green Alpha)
comp_loop:
		lda (ptr_src),y						; Read byte from our color table 
		sta (ptr_dst),y						; write byte to the Graphic CLUT
		iny
		cpy #$04							; Do 4 bytes for one color + Alpha
		bne comp_loop

		inx
		cpx totalColors						; Loop for all colors of the CLUT
		beq done_lut

		clc									; Move the source pointer to the next Color
		lda ptr_src
		adc #$04
		sta ptr_src
		lda ptr_src+1
		adc #$00
		sta ptr_src+1

		clc									; Move the destination pointer to the next Color
		lda ptr_dst
		adc #$04
		sta ptr_dst
		lda ptr_dst+1
		adc #$00
		sta ptr_dst+1

		jmp color_loop						; and start copying the next color
done_lut:
		stz MMU_IO_CTRL
; ************************************************************************************************************************************
setFont:									; let's change the font
		lda #<font
		sta $80
		lda #>font
		sta $81
		lda #$c1
		stz $82
		sta $83
		ldy #$00
		ldx #$03
		lda #$01
		sta MMU_IO_CTRL
_sfLoop:
		lda ($80),y
		sta ($82),y 
		iny
		bne _sfLoop
		inc $81
		inc $83
		dex
		bne _sfLoop
		stz MMU_IO_CTRL


; ************************************************************************************************************************************

; ************************************************************************************************************************************
; set timer for SOF

		lda #kernel.args.timer.FRAMES		; set the Timer to Frames
		ora #kernel.args.timer.QUERY		; and query what frame we're on
		sta kernel.args.timer.units			; store in units parameter
		jsr kernel.Clock.SetTimer			; jsr to Kernel Routine
		bcs skipSet							; If Carry set, ignore
		adc #$01							; if not add 1 to Accumulator for next frame
		sta $d0
skipSet:
		jsr SetTimer						; Let's get the kernel set up for the timer

; ************************************************************************************************************************************		
; Game starts here
GameStart:
		lda #$09
		sta ball_in_play
		stz score
		stz score+1
		stz ballState
		stz slpFR
		stz slpLO
		stz ZspeedLO
		stz ZSpeedFR
		jsr setSprites

loop:
		
		lda ballState
		beq nextPlay
		jsr handle_events					; This is my game loop
		bra loop

nextPlay:									; sets up a ball for a new throw
		inc ballState
		dec ball_in_play
		lda ball_in_play
		bmi game_over
		asl
		asl
		asl
		sta ballSprite
		lda startLO
		sta playXLO
		lda startHI
		sta playXHI
		stz playXFR
		lda #248
		sta playYLO
		stz playYHI
		stz playDir
		stz slpFR
		stz slpLO
		stz ZspeedLO
		stz ZSpeedFR
		bra loop	

game_over:
		brk
		rts

handle_events:
		lda kernel.args.events.pending		; Peek at the queue to see if anything is pending
		bpl done_handle_events				; Nothing to do
		jsr kernel.NextEvent				; Get the next event.
		bcs done_handle_events				; If Carry is set, skip the handler
		jsr dispatch						; Handle the event
		jmp handle_events					; go and check for another event
done_handle_events:
		rts 								

dispatch:
		lda event.type						; get the event type from Kernel
		cmp #kernel.event.timer.EXPIRED		; is the event timer.EXPIRED?
		beq UpdateScreenJmp					; run the screen update
		cmp #kernel.event.key.PRESSED		                     
        beq keypress	
		cmp #kernel.event.key.RELEASED
		beq keyRelease
		rts

UpdateScreenJmp
		jmp UpdateScreen					; jmp because conditional is to far

quit:
		stz MMU_IO_CTRL						; reset mmu to zero
		lda #$01							
		sta VKY_MSTR_CTRL_0					; reset the graphics chip
		stz VKY_MSTR_CTRL_1					; reset text size 
		jmp $e020							; jump to kernel reset

keypress:
		lda event.key.flags                                             ; Once a key is pressed, we can get the ascii value by loading the byte from the
		lda event.key.ascii                                             ; event.key.ascii location assigned by the kernel. We then check to see if it's a
		sta ascii
		lda ballState
		cmp #$01
		beq checkSpc
		cmp #$02
		beq checkPWR


		rts
checkSpc:
		lda ascii
		cmp #32
		bne keyDone
		lda #$02
		sta ballState
		lda #$64
		sta pwrLO
		stz pwrDir
		rts

checkPWR:
		lda ascii
		cmp #32
		bne keyDone
		lda #$03
		sta ballState
		jsr getSlope
		jsr getSpeed
		rts




keyDone:
		rts

keyRelease:
		lda #$00
		;bra keyJmp
		rts

getSlope:
		stz playDir
		lda playXHI
		bne keepDir
		lda playXLO
		cmp #196
		bcs keepDir
		inc playDir
keepDir:
		ldx #$00
slopeLoop:
		lda startLO,X
		cmp playXLO 
		bcc nextCheck
		lda startHI,x 
		cmp playXHI
		beq foundSlope
nextCheck:
		inx

		cpx #65
		bcc slopeLoop
foundSlope:
		stx $b0
		lda slopeFR,x 
		sta slpFR
		lda slopeLO,x 
		sta slpLO
		rts

getSpeed:
		stz ZSpeedFR
		sec
		lda #$64
		sbc pwrLO
		sta VspeedLO
		lsr VspeedLO
		ror VSpeedFR
		lsr VspeedLO
		ror VSpeedFR
		lsr VspeedLO
		ror VSpeedFR
		;lsr VspeedLO
		;ror VSpeedFR

		lda #$04
		sta ZspeedLO
		stz ZSpeedFR

		clc
		asl slpFR
		rol slpLO
		asl slpFR
		rol slpLO
		;asl slpFR
		;rol slpLO
		rts

UpdateScreen:
		jsr SetTimer
		lda ballState
		cmp #$01
		beq aimBall
		cmp #$02
		beq addPowerJmp
		cmp #$03
		beq throwBallJmp
		cmp #$04
		beq jumpBallJmp
		cmp #$05
		beq checkBallPosJmp
		cmp #$10
		beq ballBounceJmp
		rts
throwBallJmp:
		jmp throwBall
jumpBallJmp:
		jmp jumpBall
checkBallPosJmp:
		jmp checkBallPos
addPowerJmp:
		jmp addPower
ballBounceJmp:
		jmp ballBounce

aimBall:
		lda playDir
		bne GoLeft
GoRight:		
		clc
		lda playXFR
		adc #$00
		sta playXFR
		lda playXLO
		adc #$05
		sta playXLO
		lda playXHI
		adc #$00
		sta playXHI
		beq placeBall
		lda playXLO
		cmp #$38
		bcc placeBall
		inc playDir
	
GoLeft:
		sec 
		lda playXFR
		sbc #$00
		sta playXFR
		lda playXLO
		sbc #$05
		sta playXLO
		lda playXHI
		sbc #$00
		sta playXHI
		bne placeBall
		lda playXLO
		cmp #$3b
		bcs placeBall
		stz playDir
		bra GoRight

placeBall:
		ldx ballSprite					;placing the ball!
		lda playXLO
		sta VKY_SP0+SP_POS_X_L,x
		lda playXHI
		sta VKY_SP0+SP_POS_X_H,x 
		lda playYLO
		sta VKY_SP0+SP_POS_Y_L,x 
		lda playYHI
		sta VKY_SP0+SP_POS_Y_H,x
		rts

addPower:
		lda pwrDir
		bne lessPWR
morePWR:
		sec 
		lda pwrFR
		sbc #$00
		sta pwrFR
		lda pwrLO
		sbc #$02
		sta pwrLO
		cmp #$28
		bcs setPWRd
		inc pwrDir
lessPWR:
		clc	
		lda pwrFR
		adc #$00
		sta pwrFR
		lda pwrLO
		adc #$02
		sta pwrLO
		cmp #$64
		bcc setPWRd
		stz pwrDir
		bra morePWR

setPWRd:
		lda pwrLO
		sta $d948+SP_POS_Y_L
		sta $d950+SP_POS_Y_L

throwBall:
		ldx ballSprite
		lda playDir
		beq perspLeft
perspRight:
		clc
		lda playXFR
		adc slpFR
		sta playXFR
		lda playXLO
		adc slpLO
		sta playXLO
		sta VKY_SP0+SP_POS_X_L,x
		lda playXHI
		adc #$00
		sta playXHI
		sta VKY_SP0+SP_POS_X_H,x
		bra moveUp
perspLeft:
		sec
		lda playXFR
		sbc slpFR
		sta playXFR
		lda playXLO
		sbc slpLO
		sta playXLO
		sta VKY_SP0+SP_POS_X_L,x
		lda playXHI
		sbc #$00
		sta playXHI
		sta VKY_SP0+SP_POS_X_H,x


moveUp:
		sec
		lda playYFR
		sbc ZSpeedFR
		sta playYFR
		lda playYLO
		sbc ZspeedLO
		sta playYLO
		sta VKY_SP0+SP_POS_Y_L,x
		lda playYHI
		sbc #$00
		sta playYHI
		sta VKY_SP0+SP_POS_Y_H,x
		jsr checkSize
		lda playYLO
		cmp #$7f
		bcc endRunway
		rts
endRunway:
		inc ballState
		lda #$10 
		sta vertSteps
		rts

jumpBall:
		ldx ballSprite
		sec
		lda playYFR
		sbc VSpeedFR
		sta playYFR
		lda playYLO
		sbc VspeedLO
		sta playYLO
		sta VKY_SP0+SP_POS_Y_L,x

		sec
		lda VspeedFR
		sbc #$50
		sta VSpeedFR
		lda VspeedLO
		sbc #$00
		sta VspeedLO

		dec vertSteps
		lda vertSteps
		beq doneJumpBall
		cmp #$09
		bne sameBall
		lda #<sprite10
		sta VKY_SP0+SP_AD_L,x
		lda #>sprite10
		sta VKY_SP0+SP_AD_M,x
sameBall:
		rts
doneJumpBall:
		ldx #$00
		inc ballState
		ldx #$00
checkBallPos:
		lda playXLO
		sta bx
		lda playYLO
		sta by
		lda scoreHoleX,X
		sta tx
		lda scoreHoleY,X
		sta ty
; ************************************************************************************************************************************
collisionCheck:
		jsr checkX
		jsr multiplier
		sta aa 
		sty aa+1
		jsr checkY
		jsr multiplier
		sta bb
		sty bb+1
		clc											;a^2 + b^2 = c^2
		lda aa
		adc bb
		sta cc
		lda aa+1
		adc bb+1
		sta cc+1
		bne noChance
		lda cc
		cmp #$09							;This is the radius of hole - radius of ball squared.
		bcc scored
		cmp #$50							; this is hole radius+ball radius for collision with edge.
		bcc bounce
noChance:
		inx
		cpx #$07
		bcc checkBallPos
		stz ballState ;temp
		rts
scored:
		lda holeValue,X
		sta ballScore
		stz ballScore+1
		asl ballScore
		rol ballScore+1
		asl ballScore
		rol ballScore+1
		asl ballScore
		rol ballScore+1
		asl ballScore
		rol ballScore+1
		jmp updateScore	
bounce:
		; make a bounce noise
		lda Random_L
		sta bounceVX
		lda Random_L+1
		sta bounceVY
		stz Random_Reg
		lda #$01
		sta Random_Reg
		lda Random_L
		and #%00000111
		adc #$05
		sta bounceSteps
		lda Random_L+1
		and #%00000001
		sta bounceDir
		lda #$10
		sta ballState
		rts

ballBounce:
		lda bounceDir
		bne bounceLeft
bounceRight:
		clc
		lda playXFR
		adc bounceVX
		sta playXFR
		lda playXLO
		adc #$00
		sta playXLO
		bra bounceY 
bounceLeft:
		sec
		lda playXFR
		sbc bounceVX
		sta playXFR
		lda playXLO
		sbc #$00
		sta playXLO
bounceY:
		clc
		lda playYFR
		adc bounceVY
		sta playYFR
		lda playYLO
		adc #$00
		sta playYLO
placeBounce:
		ldx ballSprite
		lda playXLO
		sta VKY_SP0+SP_POS_X_L,X
		lda playYLO
		sta VKY_SP0+SP_POS_Y_L,x
		dec bounceSteps
		lda bounceSteps
		bpl bounceDone
		lda #$05
		sta ballState
		ldx #$00
bounceDone:
		rts


checkX:
		lda bx 
		ldy tx 
		jsr subtract
		bpl skipReverse
		eor #$ff 
		clc
		adc #$01
skipReverse
		rts
checkY
		lda by
		ldy ty 
		jsr subtract
		bpl skipReverse
		eor #$ff
		clc
		adc #$01
		rts

subtract:
		sta tmp
		sty tmp+1 
		sec
		lda tmp
		sbc tmp+1
		sta tmp
		
		rts

multiplier:
		sta MULU_A_L
		sta MULU_B_L
		stz MULU_A_H
		stz MULU_B_H
		lda MULU_LL
		ldy MULU_LH
		rts
		; ************************************************************************************************************************************
checkSize:

		lda #<sprite1
		sta ptr_src
		lda #>sprite1
		sta ptr_src+1
		ldx #$00
checkSizeLoop:
		lda sizePers,X
		cmp playYLO
		bcc skipCheck
		clc
		inc ptr_src+1
skipCheck:
		inx
		cpx #08
		bcc checkSizeLoop
		ldx ballSprite
		lda ptr_src
		sta VKY_SP0+SP_AD_L,x
		lda ptr_src+1
		sta VKY_SP0+SP_AD_M,x
		rts


updateScore:
		sed 
		clc
		lda score
		adc ballScore
		sta score
		lda score+1
		adc ballScore+1
		sta score+1
		cld

		lda #$02
		sta MMU_IO_CTRL
		lda score+1
		lsr
		lsr
		lsr 
		lsr 
		tax 
		lda hex,x 
		sta $c000
		lda score+1
		and #$0f
		tax 
		lda hex,x 
		sta $c001
		lda score
		lsr
		lsr
		lsr 
		lsr 
		tax 
		lda hex,x 
		sta $c002
		lda score
		and #$0f
		tax 
		lda hex,x 
		sta $c003		
		stz MMU_IO_CTRL
		stz ballState
		ldx ballSprite
		lda #$00
		sta VKY_SP0,x
		rts
; ************************************************************************************************************************************
SetTimer:	
		inc $d0
		lda $d0
		sta kernel.args.timer.absolute		; store in timer.absolute paramter
		sta kernel.args.timer.cookie		; saved as a cookie to the kernel (same as frame number)
		lda #kernel.args.timer.FRAMES		; set the Timer to Frames
		sta kernel.args.timer.units			; store in units parameter
		jsr kernel.Clock.SetTimer			; jsr to Kernel routine to set timer
		rts
; ************************************************************************************************************************************
; enable 9 sprites on layer 2
setSprites:
		lda #<sprite1						; location of the first sprite
		sta spriteLoc
		lda #>sprite1
		sta spriteLoc+1
		ldx #$00							; set x to zero to start
spriteLoop:
		txa									; transfer to A and multiply by 8
		asl 
		asl 
		asl
		tay									; transfer result to Y, sprite control data is every 8 bytes
		lda #%01010001 						; 16x16 sprite, layer 2, lut 0, enable on
		sta VKY_SP0,y 						; from sprite 0 in Vicky indexed to y
		lda ballXLO,x 
		sta VKY_SP0+SP_POS_X_L,Y
		lda #$00
		sta VKY_SP0+SP_POS_X_H,y
		lda ballYLO,X
		sta VKY_SP0+SP_POS_Y_L,Y
		lda ballYHI,X
		sta VKY_SP0+SP_POS_Y_H,Y
		lda spriteLoc
		sta VKY_SP0+SP_AD_L,y 
		lda spriteLoc+1
		sta VKY_SP0+SP_AD_M,y 
		lda #$01
		sta VKY_SP0+SP_AD_H,y
		inx 
		cpx #9
		bne spriteLoop
		rts
; ************************************************************************************************************************************
clrScreen:
		ldx #$00							; set x for indexing
csLoop:
		lda #$02							; set the output to character matrix
		sta MMU_IO_CTRL
		lda #$20							; set a to a blank character
		sta $c000+$000,x 					; and save every 240 memory locations
		sta $c000+$0f0,x 					;
		sta $c000+$1e0,x 					; We're only going to loop once instead of
		sta $c000+$2d0,x 					; nesting loops
		sta $c000+$3c0,x 
		lda #$03							; set the output to the color matrix
		sta MMU_IO_CTRL
		lda #$f0							; pick white
		sta $c000+$000,x 					; do the same save groups
		sta $c000+$0f0,x 
		sta $c000+$1e0,x 
		sta $c000+$2d0,x 
		sta $c000+$3c0,x 
		inx									; inc x
		cpx #$f1 							; and check if we've hit 241
		bcc csLoop							; if less, continue looping

		stz MMU_IO_CTRL						; reset IO to 0
		rts

; ************************************************************************************************************************************

; ************************************************************************************************************************************

; ************************************************************************************************************************************
; working memory


ballXLO:		.byte 32,32,32,32,32,32,32,32,32
ballXHI:		.byte 0,0,0,0,0,0,0,0,0
ballYLO:		.byte $40,$50,$60,$70,$80,$90,$a0,$b0,$c0
ballYHI:		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00

slopeFR:		.byte $d5,$cf,$c8,$c1,$bb,$b4,$ad,$a7,$a0,$99,$93,$8c,$85,$7f,$78,$71,$6b,$64,$5d,$57,$50,$49,$43,$4c,$35,$2f,$28,$21,$1b,$14,$0d,$07,$00
				.byte $06,$0c,$13,$1a,$20,$27,$2e,$34,$3b,$42,$48,$4f,$56,$5c,$63,$6a,$70,$77,$7e,$84,$8b,$92,$98,$9f,$a6,$ac,$b3,$ba,$ba,$c0,$c7,$ce,$d4
slopeLO:		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

startLO:		.byte $3b,$3f,$43,$47,$4b,$4f,$53,$58,$5c,$60,$64,$68,$6c,$70,$75,$79,$7d,$81,$85,$89,$8e,$92,$96,$9a,$9e,$a2,$a6,$ab,$af,$b3,$b7,$bb,$c0
				.byte $c4,$c8,$cc,$d0,$d4,$d8,$dd,$e1,$e5,$e9,$ed,$f1,$f5,$fa,$fe,$02,$06,$0a,$0e,$13,$17,$1b,$1f,$23,$2b,$30,$34,$38,$3c,$40,$44,$48,$50
startHI:		.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
				.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01

scoreHoleX:		.byte $a3,$d6,$bc,$bc,$bc,$bc,$bc
scoreHoleY:		.byte $2f,$2f,$32,$3f,$49,$53,$62
holeValue:		.byte $10,$10,$05,$04,$03,$02,$01

sizePers:		.byte 220,200,190,180,170,160,150,140,130,120

playXFR:		.byte $00
playXLO:		.byte $00
playXHI:		.byte $00
playYFR:		.byte $00
playYLO:		.byte $00
playYHI:		.byte $00
playDir:		.byte $00
pwrDir:			.byte $00
pwrFR:			.byte $00
pwrLO:			.byte $64
slpFR:			.byte $00
slpLO:			.byte $00
ZSpeedFR		.byte $00
ZspeedLO		.byte $00
VspeedFR:		.byte $00
VspeedLO:		.byte $00
vertSteps:		.byte $00

bounceVX:		.byte $00
bounceVY:		.byte $00
bounceSteps:	.byte $00
bounceDir:		.byte $00

ballState:		.byte $00
ball_in_play:	.byte $08
ballSprite:		.byte $00
newBall:		.byte $00



ascii:			.byte $00
score:			.word $0000
ballScore:		.word $0000
spriteLoc:		.word $0000
hex:			.text "0123456789abcdef"
totalColors:    .byte 128

font:
.binary "atari.bin"
.include "skeester.pal.s"

* = $10000

.include "skeeball_ball.s"
.include "skeester_tileset.s"
background_map:
.word $0000
.binary "tilemap.tlm"