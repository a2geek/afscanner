******************************************************************************************
*
* Utility routines
* ========================================================================================
*
******************************************************************************************

* Setup the data buffer
SETUPDATA	   LDA #>DATASTART
               STA DATA+1
               LDA #<DATASTART
               STA DATA
               LDA #>DATALEN
               STA TEMP			; number of pages to load
               RTS

* Print identifier and 4 and 4 encoded number
* Acc = character
* Output = " ?=" where ? is char in Acc
PRDATA         PHA
               LDA   #" "
               JSR   COUT
               PLA
               JSR   COUT
               LDA   #"="
               JSR   COUT
               LDA   (DATA),Y
               SEC
               ROL
               INY
               AND   (DATA),Y
               JMP   PRHEX

* Output routine uses simple RLE type encoding
*   High bit set = character;
*   High bit clear =
*       $00 = exit
*       $01 = print byte from address
*       $02-$7F = repeat next character
*
* There are two entry points:
* - PRINT = inline text being printed out; implicit PTR increment at start
* - PRINTP = print address passed in AX; no implicit PTR increment at start

PRINT	PLA
		STA PTR
		PLA
		STA PTR+1
		JSR :MORE
		LDA PTR+1
		PHA
		LDA PTR
		PHA
		RTS

PRINTP	STA PTR+1
		STX PTR
		BRA :START

:MORE	JSR INCPTR
:START	LDX #1			; Assume 1 char to be output
		LDA (PTR)
		BEQ :EXIT
		BMI :SINGLE
		CMP #1
		BNE :REPEAT
		JSR INCPTR
		LDA (PTR)
		STA PTR2
		JSR INCPTR
		LDA (PTR)
		STA PTR2+1
		LDA (PTR2)
		JSR PRHEX
		BRA :MORE
:REPEAT	TAX
		JSR INCPTR
		LDA (PTR)
:SINGLE	JSR COUT
		DEX
		BNE :SINGLE
		BEQ :MORE
:EXIT	RTS

INCPTR	INC PTR
		BNE :EXIT
		INC PTR+1
:EXIT	RTS

* Clear the data portion of the screen
* (Lines 3-22) and reposition to line 3.

CLRSCRN        LDA   #_HOME
               JSR   COUT
               JSR   PRCR
               LDY   #20
:1             JSR   PRCR       ; next line
               LDA   #_CLREOL
               JSR   COUT
               DEY
               BNE   :1
               LDA   #_HOME
               JSR   COUT
               JSR   PRCR       ; move to row 3
               JMP   PRCR

* Read a byte
* If invalid, carry is set (anything NOT 0-9,A-F is invalid)

READBYTE       JSR   READHEX
               BCS   :EXIT
               ASL
               ASL
               ASL
               ASL
               STA   TEMP
               JSR   READHEX    ; carry falls through to exit
               ORA   TEMP
:EXIT          RTS

READHEX        JSR   GETCH
               JSR   COUT
               CMP   #"9"+1
               BCC   :NUMERIC
               CMP   #"F"+1
               BCC   :ALPHA
:BAD           SEC
               RTS
:NUMERIC       SEC
               SBC   #"0"
               BVS   :BAD
:GOOD          CLC
               RTS
:ALPHA         SEC
               SBC   #"A"
               BVS   :BAD
               ADC   #10
:GOOD2         CLC
               RTS

* ProDOS QUIT parameter tables
QUITPARM       DFB   4			; 4 parameters
               DFB   0			; 0 is the only quit type
               DA    0			; reserved
               DFB   0			; reserved
               DA    0			; reserved

; On entry:
;   Acc = keypress
;   Stack = address of table (format: key address key address 0)
; Will not return if key found, a JMP will be performed.
; If not found, return with carry set.
HandleKey
		sta TEMP
		pla
		sta PTR
		pla
		sta PTR+1
		ldy #0
:next
		iny
		lda (PTR),y
		bpl :notFound
		cmp TEMP
		beq :jmp
		iny
		iny
		bra :next
; Jump to keypress handler
:jmp	iny
		lda (PTR),y
		sta PTR2
		iny
		lda (PTR),y
		sta PTR2+1
		jmp (PTR2)
; Not found - return to caller (Y + PTR = return address - 1)
:notFound
		clc
		tya
		adc PTR
		tax
		lda #0
		adc PTR+1
		pha
		phx
		lda TEMP
		sec
		rts



************************* Test Code **************************
; Characters - assumption is a character unless % encountered
;	%(byte < $80) = repeat next character
;   %w = hex word
;   %b = hex byte
;   %% = %
;   %i = inverse
;   %n = normal
;   %mC = C as mouse text (mouse text on, inverse, C, normal, mouse text off)
;   %h# = HTAB, # is 0..79
;   %v# = VTAB, # is 0..23
;   %H = home cursor
;   %C = clear screen and home cursor
;   %E = clear to EOL
;
NewPrint
		pla
		sta PTR
		pla
		sta PTR+1
		jsr IncPTR
		jsr NewPrintX
		jmp (PTR)

NewPrintP
		sta PTR+1
		stx PTR

NewPrintX
:next	jsr FetchPTR
		cmp #0		; Fetch loses the status bits
		beq :exit
		cmp #"%"
		beq :specl
:cout	jsr COUT
		bra :next

:exit	rts

:specl	jsr FetchPTR
		bpl :repeat
		ldx :subcmd
:look	cmp :subcmd,x
		beq :dsptch
		dex
		bne :look
		beq :dsptc0		; X=0 which is %% code as well

:repeat	tax
		jsr FetchPTR
:rep0	jsr COUT
		dex
		bne :rep0
		beq :next

:dsptch	dex		; we're +1 because the lookup table is a STR (length byte at beginning)
:dsptc0	txa
		asl		; times 2
		tax
		jmp (:addrs,x)

:chrout	asl		; requires these to be first in :subcmd
		tax
		lda :lookup,x
		bra :cout

:mtext	jsr FetchPTR
		sta :mt_out+2
		ldy #0
:mtext0	lda :mt_out,y
		jsr COUT
		iny
		cpy #5
		bne :mtext0
		bra :next

:htab	jsr FetchPTR
		sta HTAB
		bra :next
:vtab	jsr FetchPTR
		jsr VTAB
		bra :next
:prbyte	jsr SetPTR2
:lowb	lda (PTR2)
		jsr PRHEX
		bra :next
:prword	jsr SetPTR2
		ldy #1
		lda (PTR2),y
		jsr PRHEX
		bra :lowb

; data tables to make relative branches last longer
:subcmd	str "%inHCEmhvbw"
:addrs	lup 6	; 1st set are a character replacement
		da :lookup
		--^
		da :mtext
		da :htab
		da :vtab
		da :prbyte
		da :prword
:lookup	asc "%",_NORMAL,_INVERSE,_CLS,_HOME,_CLREOL
:mt_out	dfb _MT_ON,_INVERSE,0,_NORMAL,_MT_OFF

SetPTR2	jsr FetchPTR
		sta PTR2
		jsr FetchPTR
		sta PTR2+1
		rts

FetchPTR
		lda (PTR)
IncPTR	inc PTR
		bne :exit
		inc PTR+1
:exit	rts