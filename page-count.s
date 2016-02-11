******************************************************************************************
*
* Buffer Count Page
*
******************************************************************************************

* Count Settings values (most of these are implicit):

Count_NONE = 0
Count_NONZERO = 1
Count_13SECTOR = 2
Count_16SECTOR = 3

CountInit		; Returns with A:Y = max lines, C = scrolling
		jsr CountStatusBar

; Set inbuf to zero  (only counting $80, so $200-$2ff sufficient)
		ldy #0
		tya
:erase	sta inbuf,y
		iny
		bne :erase
; Start counting
		jsr ReadTrack
		jsr SETUPDATA
		ldy #0
:loop	lda (DATA),y
		asl		; times 2, we don't care about highbit
		tax
		inc inbuf,x
		bne :skip
		inc inbuf+1,x
:skip	iny
		bne :loop
		inc DATA+1
		dec TEMP
		bne :loop
		
		jsr SetCountFlags
		
; Setup framework
		lda #0	; No lines
		tay
		sec		; No scrolling
		rts

CountDisplay	; Called with A:Y = line
		jsr PRINT
		dfb _CLREOL,$8D
		dfb 14	; repeat the next space 14x
		asc " Low   +0   +1   +2   +3   +4   +5   +6   +7   High",$8D
		dfb 19	; repeat the next space 19x
		asc " ==== ==== ==== ==== ==== ==== ==== ====",$8D
		dfb 0

; Y = byte, X = temp, A = temp		
		ldy #$80
:next	ldx #14
		lda #" "
:tab	jsr COUT
		dex
		bne :tab
		tya
		jsr PRHEX
		lda #" "
		jsr COUT
		jsr COUT
:line	lda #" "	; re-loading space due to loop construction
		jsr COUT
		tya
		asl
		tax
		lda inbuf+1,x
		bpl :norm
		pha
		lda #_INVERSE
		jsr COUT
		pla
:norm	and #$7f	; ensure high-bit clear in the count as it is our highlight flag
		jsr PRHEX
		lda inbuf,x
		jsr PRHEX
		lda #_NORMAL
		jsr COUT
		iny
		tya
		and #$07
		bne :line
		lda #" "
		jsr COUT
		jsr COUT
		jsr COUT
		jsr COUT
		tya
		dec
		jsr PRHEX
		jsr PRCR
		cpy #0
		bne :next
		jsr PRCR
		jsr PRCR
		; Fall through to CountStatusBar for redisplay

CountStatusBar
		jsr PRINT
		asc "[-]",$00
		lda #_NORMAL
		ldx CountSettings
		cpx #Count_NONE
		bne :prnone
		lda #_INVERSE
:prnone	jsr COUT
		jsr PRINT
		asc "None",_NORMAL," [0]",$00
		
		lda #_NORMAL
		ldx CountSettings
		cpx #Count_NONZERO
		bne :prnzro
		lda #_INVERSE
:prnzro	jsr COUT
		jsr PRINT
		asc "Non-Zero",_NORMAL," [3]",$00
		
		lda #_NORMAL
		ldx CountSettings
		cpx #Count_13SECTOR
		bne :pr13
		lda #_INVERSE
:pr13	jsr COUT
		jsr PRINT
		asc "13-Sector",_NORMAL," [6]",$00

		lda #_NORMAL
		ldx CountSettings
		cpx #Count_16SECTOR
		bne :pr16
		lda #_INVERSE
:pr16	jsr COUT
		jsr PRINT
		asc "16-Sector",_NORMAL,$00
		rts

CountKeypress	; Called with Acc = key
		ldx #0
:test	cmp :keys,x
		beq :set
		inx
		cpx #4
		bne :test
		jmp TrackNavigationKeys
:keys	asc "-036"
		
:set	stx CountSettings
		jsr SetCountFlags
		jmp DrawPage

SetCountFlags
		lda CountSettings
		asl
		tax
		jmp (:jmp,x)
:jmp	da CountClear
		da CountNonZero
		da Count13Sector
		da Count16Sector

CountClear
		ldy #$80
:loop	tya
		asl
		tax
		lda inbuf+1,x
		and #$7f
		sta inbuf+1,x
		iny
		bne :loop
		rts

CountNonZero
		jsr CountClear
		ldy #$80
:loop	tya
		asl
		tax
		lda inbuf,x
		ora inbuf+1,x
		beq :next
		lda inbuf+1,x
		ora #$80
		sta inbuf+1,x
:next	iny
		bne :loop
		rts

Count13Sector
		jsr CountClear
		ldy #0
:loop	lda Translate13,y
		jsr CountSetBit
		iny
		cpy #32
		bne :loop

; Set the reserved bytes which are identical for 13-sector and 16-sector formats
CountSetReserved
		lda #$D5
		jsr CountSetBit
		lda #$AA
		jsr CountSetBit
		rts

Count16Sector
		jsr CountClear
		ldy #0
:loop	lda Translate16,y
		jsr CountSetBit
		iny
		cpy #64
		bne :loop
		beq CountSetReserved

CountSetBit
		asl
		tax
		lda inbuf+1,x
		ora #$80
		sta inbuf+1,x
		rts

CountSettings
		dfb 0
