******************************************************************************************
*
* Primary page controller.
* ========================================================================================
*
******************************************************************************************

SetupPage
		jsr TEXT		; ensure we are out of graphics modes as application has some!
		jsr PRINT		; position cursor on bottom line
		dfb _HOME
		dfb 23,$8D
		dfb _CLREOL
		dfb $00
		ldx #_init		; vector offset
		jsr _VCALL
		ror noscroll	; set flag based on C
		sty maxlines
		sta maxlines+1
		stz topline
		stz topline+1

DrawPage
		jsr PRINT	; Clear and then draw the content area
		dfb _HOME,$8d,$8d,0
		
; If we aren't scrolling, call _display vector ONCE and then handle keyboard.
		bit noscroll
		bpl :scroll
		lda #0
		tay
		ldx #_display
		jsr _VCALL
		jmp KeyboardWait

; We are scrolling, for each line, redraw it
:scroll	lda #0
:loop	pha
		clc
		adc topline
		sta printline
		lda topline+1
		adc #0
		sta printline+1
		cmp maxlines+1
		blt :drwlin
		lda printline
		cmp maxlines
		bge :erase

:drwlin	lda printline+1
		ldy printline
		ldx #_display
		jsr _VCALL
		bra :incr

:erase 	jsr PRINT
		dfb _CLREOL,$8D,0

:incr	pla
		inc
		cmp #PAGELEN
		blt :loop

; Handle all keyboard interactions
KeyboardWait
		lda KEYBOARD
		bpl KeyboardWait
; Uppercase the if it's A-Z
		cmp #"a"
		blt :goodky
		cmp #"z"+1
		bge :goodky
		and #$df		; uppercase mask
:goodky	sta KEYCLEAR
		bit OpenApple
		bpl :keys
; OpenApple handler
		jsr SetScreen
		bcs :oakeys
		jsr CLRSCRN
		jmp SetupPage

; Standard open-apple keys that are always available
:oakeys	jsr HandleKey
		MenuKey "Q";:Quit
		MenuKey "*";:Mon
		dfb 0

; Open-Apple arrow keys only if scrolling is enabled
		bit noscroll
		bmi :local0
		jsr HandleKey
		MenuKey UpArrow;:PgUp
		MenuKey DownArrow;:PgDn
		dfb 0
:local0	jmp :local

; Normal key handler (only when scrolling)
:keys	bit noscroll
		bmi :local0
		jsr HandleKey
		MenuKey UpArrow;:Up
		MenuKey DownArrow;:Down
		dfb 0
		jmp :local

; OA-Up
:PgUp	ldy #15
:uploop	jsr :up1
		dey
		bne :uploop
		beq :back		; always

; OA-Down
:PgDn	ldy #15
:dnloop	jsr :down1
		dey
		bne :dnloop
		beq :back		; always

; OA-Q
:Quit	jsr PRODOSMLI
		dfb _MLIQUIT
		da QUITPARM
:back	jmp DrawPage	; fall through and common return

; OA-*
:Mon	jsr PRINT
		asc _CLS,"Press CTRL-Y to re-enter AFScanner.",$8D,$00
		jmp MONITOR

; Up
:Up		jsr :up1
		jmp DrawPage

; Down
:Down	jsr :down1
		jmp DrawPage

; "Local" subroutines

; if topline+PAGELEN >= maxlines then return
; topline = topline + 1
:down1	clc
		lda topline
		adc #PAGELEN
		sta printline
		lda printline+1
		adc #0
		sta printline+1
		cmp maxlines+1
		bcc :minus1
		lda printline
		cmp maxlines
		bge :rts
:minus1	inc topline
		bne :rts
		inc topline+1
		rts
; if topline = 0 then return
; topline = topline - 1
:up1	lda topline
		ora topline+1
		beq :rts		; already = 0
		sec
		lda topline
		sbc #1
		sta topline
		lda topline+1
		sbc #0
		sta topline+1
:rts	rts

:local	ldx #_keypress
		; Fall through and JMP to _keypress which takes control for local page keys

* Simple vector call from ZP based on X register

_VCALL	jmp: ($00,x)		; Merlin32 needs to know 16 bit JMP, ":" does that

* Handle screen change - both called by app init and normal keyboard handler

SetScreen
		ldx #0
:next	bit :data,x
		bpl :notf
		cmp :data,x
		beq :setup
		pha
		txa
		clc
		adc #7
		tax
		pla
		bra :next
:notf	sec			; Not found
		rts
:setup	ldy #0
:copy	inx
		lda :data,x
		sta _init,y
		iny
		cpy #6
		bne :copy
		clc
		rts

*
* Data per page: Apple ASCII, Initialization, Display, Keypress (7 bytes)
*
* Definitions:
* - BYTE = Character to match on, case insensitive
* - ADDR = Initialize routine (load track, calculate lookup tables, etc). 
*          Return:
*               A:Y for number of lines, 
*               Carry: Set = no scrolling (graphics, form, etc), Clear = scrolling.
*          Note: Initialization should also display 24th line for the page.  
*                (Cursor is there and line is clear.)
* - ADDR = Display routine.  Called with A:Y for line number.  Should end with $8D and is 
*          responsible for clearing the line.
* - ADDR = Keypress handler.  NOT A SUBROUTINE.  This routine has a number of entry
*          points it might call.  KeyboardWait for another keypress or SetupPage to
*          reinitialize and redraw.
*
:data
		dfb "A"
		da  AboutInit, AboutDisplay, AboutKeypress
		dfb "F"
		da  FieldInit, FieldDisplay, FieldKeypress
		dfb "B"
		da	BrowseInit, BrowseDisplay, BrowseKeypress
		dfb "G"
		da	HgrInit, HgrDisplay, HgrKeypress
		dfb "C"
		da	CountInit, CountDisplay, CountKeypress
		dfb "T"
		da  TestInit, TestDisplay, TestKeypress
		dfb 0	; end

