******************************************************************************************
*
* HGR Disk Display
*
******************************************************************************************

*
* HGR/Graphics interface
*

; Init displays HGR and the static text on the page
HgrInit	
		jsr PRINT
		asc "re",_INVERSE,"S",_NORMAL,"can / "
		asc _INVERSE,"R",_NORMAL,"ecalibrate"
		dfb 0
; Lay out a our rudimentary page:
		jsr HGR
		stz TEMPY
:newlin	lda TEMPY
		jsr HPOSN
		ldy #0
:line	lda TEMPY
		and #$8
		bne :lownyb
:hinyb	tya
		lsr
		lsr
		lsr
		lsr
		bra :draw
:lownyb	tya
		and #$0f
:draw	asl	
		asl
		asl
		sta TEMP
		lda TEMPY
		and #$07
		ora TEMP
		tax
		lda HgrHexFont,x
		sta (HBAS),y
		iny
		cpy #35
		bne :line
		inc TEMPY
		lda TEMPY
		cmp #16
		blt :newlin
; return values
		sec				; no scrolloing
		lda #0			; 0 = number of lines (A:Y)
		tay
		rts

HgrDisplay
; Store current track to not interfere with other screens
		lda DSTTRK
		pha
; 144 lines (18 text lines * 8 graphic lines ea)
; buffer = 8192 bytes / 144 lines = 56 bytes ea
; OR NIB suggest 6656 / 144 lines = 46 bytes ea
		stz DSTTRK
:nxttrk	jsr ReadTrack
		lda #16
		sta :ycoord
		jsr SETUPDATA
		stz :count
		lda #46
		sta :bytes
:loop	lda (DATA)
		cmp #$FF
		bne :skip
		inc :count
:skip	dec :bytes
		bne :skip2
		lda :ycoord
		jsr HPOSN
		ldy CURTRK
		ldx #0
		lda :count
		beq :empty
:bits	inx
		lsr
		bne :bits
:empty	lda :plot,x
		sta (HBAS),y
		inc :ycoord
		lda :ycoord
		cmp #160
		bge :botm
		stz :count
		lda #46
		sta :bytes
:skip2	inc DATA
		bne :loop
		inc DATA+1
		dec TEMP
		bne :loop
:botm	inc DSTTRK
		lda DSTTRK
		cmp #35
		blt :nxttrk
; Restore current target track for other screens
		pla
		sta DSTTRK
		rts
:ycoord	dfb	0
:count	dfb 0
:bytes	dfb 0
:plot	hex 000103070f1f3f7fff

HgrKeypress
		cmp #"R"
		beq :recal
		cmp #"S"
		beq :setup
:back	jmp KeyboardWait

:recal	lda #40
		sta CURTRK		; This forces the recalibration
		stz DSTTRK
:setup	jmp SetupPage	; this redraws page and re-initializes, presumably, on a new disk

HgrHexFont		; Stolen from https://github.com/Michaelangel007/apple2_hgr_font_tutorial
		hex 1C22322A26221C00	; 0
		hex 080C080808081C00	; 1
		hex 1C22201804023E00	; 2
		hex 3E20101820221C00	; 3
		hex 101814123E101000	; 4
		hex 3E021E2020221C00	; 5
		hex 3804021E22221C00	; 6
		hex 3E20100804040400	; 7
		hex 1C22221C22221C00	; 8
		hex 1C22223C20100E00	; 9
		hex 081422223E222200	; A
		hex 1E22221E22221E00	; B
		hex 1C22020202221C00	; C
		hex 1E22222222221E00	; D
		hex 3E02021E02023E00	; E
		hex 3E02021E02020200	; F

