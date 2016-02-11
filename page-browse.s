******************************************************************************************
*
* Browse Address Buffer Page
*
******************************************************************************************

*		
* BROWSE interface
*

BrowseInit			; Returns with Acc = max lines
		jsr PRINT
		asc _INVERSE,_MT_ON,_U_ARROW,_D_ARROW,_MT_OFF,_NORMAL," Scroll / "
		asc _INVERSE,_MT_ON,_O_APPLE,_NORMAL,"-",_INVERSE,_U_ARROW,_D_ARROW,_MT_OFF,_NORMAL," Page / "
		asc _MT_ON,_INVERSE,_L_ARROW,_R_ARROW,_NORMAL,_MT_OFF," Track / "
		asc "re",_INVERSE,"S",_NORMAL,"can / "
		asc _INVERSE,"R",_NORMAL,"ecalibrate / "
		asc "goto ",_INVERSE,"T",_NORMAL,"rack"
		dfb 0

* Scan for our prologue mark them by turning off the high bit

		jsr ReadTrack		; position head and fully populate buffer
		jsr SETUPDATA
:scan	ldy #0				; index for comparisons
:loop	lda (DATA),y
		cmp AddressFieldPrologue,Y
		bne :adv
		iny
		cpy #3
		bcc :loop
; We found prologue bytes, strip off high bit
:strip	dey
		bmi :adv
		lda (DATA),y
		and #$7f
		sta (DATA),y
		bne :strip		; should be always
:adv	inc DATA
		bne :scan
		inc DATA+1
		dec TEMP
		bne :scan
; The number of lines is based on how many bytes we show on screen
		lda #1		; A:Y = $100 or 256 lines
		ldy #0
		clc			; we need scrolling
		rts

BrowseDisplay		; Called with Acc = line
; Calculate buffer address
		sta DATA+1
		sty DATA
		ldy #5		; times 32
:mult32	asl DATA
		rol DATA+1
		dey
		bne :mult32
		clc
		lda #>DATASTART
		adc DATA+1
		sta DATA+1
; Display offset
		lda #"+"
		jsr COUT
		lda DATA+1
		and #$3f
		jsr PRHEX
		lda DATA
		jsr PRHEX
		jsr PRINT
		asc "-  ",$00
; Display 'disk' bytes, highlighting the field markers (high bit was stripped off in init routine)
		ldy #0
:nxtbyt	lda (DATA),y
		bmi :prhex
		pha
		lda #_INVERSE
		jsr COUT
		pla
		ora #$80
:prhex	jsr PRHEX
		lda #_NORMAL
		jsr COUT
		tya
		and #$07
		cmp #$07
		bne :testy
		lda #" "
		jsr COUT
:testy	iny
		cpy #32
		bne :nxtbyt
		jmp PRCR

BrowseKeypress		; Called with Acc = key
		jmp TrackNavigationKeys

