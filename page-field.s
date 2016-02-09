******************************************************************************************
*
* Address Field Page
*
******************************************************************************************

*
* FIELD Interface
*

FieldInit		; Returns with A:Y = max lines
		jsr PRINT
		asc _INVERSE,_MT_ON,_U_ARROW,_D_ARROW,_MT_OFF,_NORMAL," Scroll / "
		asc _INVERSE,_MT_ON,_O_APPLE,_NORMAL,"-",_INVERSE,_U_ARROW,_D_ARROW,_MT_OFF,_NORMAL," Page / "
		asc _MT_ON,_INVERSE,_L_ARROW,_R_ARROW,_NORMAL,_MT_OFF," Track / "
		asc "re",_INVERSE,"S",_NORMAL,"can / "
		asc _INVERSE,"R",_NORMAL,"ecalibrate / "
		asc "goto ",_INVERSE,"T",_NORMAL,"rack"
		dfb 0

* Scan for our prologue and save positions in INBUF

		jsr ReadTrack		; position head and fully populate buffer
		ldx #0				; index for INBUF
		jsr SETUPDATA
:scan	ldy #0				; index for comparisons
:loop	lda (DATA),y
		cmp AddressFieldPrologue,Y
		bne :adv
		iny
		cpy #3
		bcc :loop
; We found prologue bytes, save DATA address
		lda DATA
		sta inbuf,x
		inx
		lda DATA+1
		sta inbuf,x
		inx
:adv	inc DATA
		bne :scan
		inc DATA+1
		dec TEMP
		bne :scan
; Calculate # of lines
		txa
		lsr
		tay
		lda #0		; Assumed < 256 lines
		clc			; we need scrolling
		rts

FieldDisplay		; Called with A:Y = line
		tya			; Assuming < 256 lines
		asl
		tay
		lda inbuf,y
		sta DATA
		pha
		lda inbuf+1,y
		sta DATA+1
		pha
; Display offset
		lda #"+"
		jsr COUT
		pla
		and #$3f
		jsr PRHEX
		pla
		jsr PRHEX
		jsr PRINT
		asc "-  ",$00
; Display 'disk' bytes
		ldy #0
		ldx #0
:nextg	lda :groups,x
		sta TEMP
:bytes	lda (DATA),y
		jsr PRHEX
		iny
		dec TEMP
		bne :bytes
		lda #" "
		jsr COUT
		inx
		cpx #6
		bne :nextg
		jsr COUT	; 2nd space
; Display values
		lda #"V"
		ldy #OFFSETV
		jsr PRDATA
		lda #"T"
		ldy #OFFSETT
		jsr PRDATA
		lda #"S"
		ldy #OFFSETS
		jsr PRDATA
		lda #"C"
		ldy #OFFSETC
		jsr PRDATA
		jmp PRCR
; Address Field byte groupings
:groups	dfb 3,2,2,2,2,3

FieldKeypress		; Called with Acc = key
		ldx #-1
		cmp #LARROW
		beq :chgtrk
		ldx #1
		cmp #RARROW
		beq :chgtrk
		cmp #"R"
		beq :recal
		cmp #"S"
		beq :setup
		cmp #"T"
		beq :gotrk
:back	jmp KeyboardWait

:chgtrk	txa
		clc
		adc CURTRK
		cmp #35			; cap at track 35 (0..34)
		bcs :back
:setdst	sta DSTTRK
:setup	jmp SetupPage	; this redraws page and re-initializes on new track
:recal	lda #40
		sta CURTRK		; This forces the recalibration
		lda #0
		beq :setdst

:gotrk	jsr CLRSCRN
		jsr PRINT
		asc "Enter track number: $"00
		jsr READBYTE
		bcs :setup
		bcc :setdst

