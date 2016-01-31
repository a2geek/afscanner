********************************
*
* Address Field Scanner
*
* 1/16/2016: Version 1.0
*
* 1/29/2016: Version 2.0
* - Restructuring to support paging and scrollong.
* - Slowly switching from spaces to tabs as an experiment.
* - Pages available:
*   + About (default page)
*   + Headers (updated Address Field Header information)
*
* This is a simple Disk II scanner
* to identify address field contents
*
********************************

	ORG $2000
	TYP SYS

               XC               ; enable 65C02

* Constants:

NUMBYTES       =     29         ; Number of bytes on screen
OFFSETV        =     3          ; Offset to volume
OFFSETT        =     OFFSETV+2  ; Offset to track
OFFSETS        =     OFFSETT+2  ; Offset to sector
OFFSETC        =     OFFSETS+2  ; Offset to checksum
FAILBYTS       =     6656       ; Number of bytes before failure

* 80 column card / print controls:

_PRBYTE        =     1          ; print byte @ addr
_CLS           =     $8C        ; clear screen
_INVERSE       =     $8F
_NORMAL        =     $8E
_MT_OFF        =     $98        ; disable MouseText 
_HOME          =     $99        ; home the cursor, not cls
_MT_ON         =     $9B        ; enable MouseText for uppercase inverse characters
_CLREOL        =     $9D        ; clear to EOL

_C_APPLE       =     "@"		; Closed Apple
_O_APPLE       =     "A"		; Open Apple
_L_ARROW       =     "H"		; Left Arrow
_R_ARROW       =     "U"		; Right Arrow
_U_ARROW       =     "K"		; Up Arrow
_D_ARROW       =     "J"		; Down Arrow
_H_LINE        =     "S"		; Horizontal Line (full width)


* Variable locations:

			DUM	$0		; ZP locs
SAMPLES		DFB	0		; number of sector samples
CURTRK		DFB	0		; current track
DSTTRK		DFB	0		; destination track
PTR			DA	0		; primary print pointer
PTR2		DA	0		; secondary print pointer
SLOT16		DFB	0		; slot# * 16
COUNTER		DA	0		; fail counter
TEMP		DFB	0		; local variable
DATA		DA	0		; data buffer

_init		da	0		; current page init
_display	da	0		; current page line display handler
_keypress	da	0		; current page keypress handler
curline		dfb	0		; current first line
endline		dfb 0		; end of page = min(curline+20,maxlines)
maxlines	dfb 0		; maximum number of lines availalable
			DEND

inbuf		= $200		; reusable buffer

DATASTART	   =	   $4000
DATAEND		   =	   $6000
DATALEN        =       DATAEND-DATASTART

* High ASCII constants

CTRLH		= "H"-$40
LARROW		= CTRLH
CTRLU		= "U"-$40
RARROW		= CTRLU
ESC			= $9B
CTRLK		= "K"-$40
UpArrow		= CTRLK
CTRLJ		= "J"-$40
DownArrow	= CTRLJ

* ProDOS:

PRODOSMLI      =     $BF00
_MLIQUIT       =     $65

* ROM routines:

DELAY          =     $FCA8
GETCH          =     $FD0C
PRCR           =     $FD8E
PRHEX          =     $FDDA
COUT           =     $FDED

* I/O addresses:

KEYBOARD	= $C000
KEYCLEAR	= $C010
OpenApple	= $C061

* Disk II addresses:

PHASEOFF       =     $C080
PHASEON        =     $C081
MOTOROFF       =     $C088
MOTORON        =     $C089
DRV0EN         =     $C08A
Q6L            =     $C08C
Q7L            =     $C08E

* Display screen

MAIN	JSR   $C300      ; Assuming 80 columns
		JSR   PRINT
		DFB   _CLS
		ASC   "AFScanner",$8D
		DFB   _MT_ON,_INVERSE,80,_H_LINE,_NORMAL,_MT_OFF  ; wraps!
		DFB   20,$8D
		DFB   _MT_ON,_INVERSE,80,_H_LINE,_NORMAL,_MT_OFF  ; wraps!
		ASC   _MT_ON,_INVERSE,_L_ARROW,_NORMAL,", ",_INVERSE,_R_ARROW,_NORMAL,_MT_OFF," Track / "
		ASC   "re",_INVERSE,"S",_NORMAL,"can / "
		ASC   _INVERSE,"R",_NORMAL,"ecalibrate / "
		ASC   "goto ",_INVERSE,"T",_NORMAL,"rack / "
		ASC   _INVERSE,"ESC",_NORMAL," quit"
		DFB   _HOME
		HEX   8D8D
		HEX   00

* Setup local variables

Initialize
		ldx #$60		; assumption = slot 6
		stx SLOT16
		lda #40
		sta CURTRK		; force a recalibration
		stz DSTTRK
		lda #"A"		; default screen is "About" page
		jsr SetScreen

SetupPage
		jsr PRINT		; position cursor on bottom line
		dfb _HOME
		dfb 23,$8D
		dfb _CLREOL
		dfb $00
		ldx #_init		; vector offset
		jsr _VCALL
		sta maxlines
		stz curline

DrawPage
		jsr PRINT	; Clear and then draw the content area
		dfb _HOME,$8d,$8d,0

		clc
		lda curline
		adc #20
		sta endline
		cmp maxlines
		bcc :start
		lda maxlines	; maxlines < endline, use maxlines
		sta endline
		
:start	lda curline
:next	pha
		ldx #_display	; vector offset
		jsr _VCALL
		pla
		inc
		cmp endline
		bcc :next 
		
		; Clear rest of screen
:wipe	cmp #20
		bcs KeyboardWait
		pha
		jsr PRINT
		dfb _CLREOL,$8D,0
		pla
		inc
		bne :wipe

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
		bpl :normal
; OpenApple handler
		jsr SetScreen
		bcs :paging
		jsr CLRSCRN
		jmp SetupPage
; OA-Up
:paging cmp #UpArrow
		bne :nPgUp
		ldy #15
:uploop	jsr :up1
		dey
		bne :uploop
		beq :back		; always
; OA-Down
:nPgUp	cmp #DownArrow
		bne :chkOAQ
		ldy #15
:dnloop	jsr :down1
		dey
		bne :dnloop
		beq :back		; always
; OA-Q
:chkOAQ	cmp #"Q"
		bne :back
		jsr PRODOSMLI
		dfb _MLIQUIT
		da QUITPARM
:back	jmp DrawPage	; fall through and common return

; Common keypress handler
; Up
:normal	cmp #UpArrow
		bne :notUp
		jsr :up1
		jmp DrawPage
; Down
:notUp	cmp #DownArrow
		bne :pgKey
		jsr :down1
		jmp DrawPage

; "Local" subroutines
:down1	lda curline
		inc
		sta curline
		clc
		adc #20
		cmp maxlines
		bcc :rts
		; went too far
:up1	lda curline
		dec
		bmi :rts
		sta curline
:rts	rts

:pgKey	ldx #_keypress
		; Fall through and JMP to _keypress which takes control for local page keys

* Simple vector call from ZP based on X register

_VCALL	jmp: ($00,x)		; Merlin32 needs to know 16 bit JMP

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

* Data per page: Apple ASCII, Initialization, Display, Keypress (7 bytes)
* Note: Initialization should also display 24th line for the page.  
*       Cursor is there and line is clear.
:data
		dfb "A"
		da  AboutInit, AboutDisplay, AboutKeypress
		dfb "F"
		da  FieldInit, FieldDisplay, FieldKeypress
		dfb "B"
		da	BrowseInit, BrowseDisplay, BrowseKeypress
		dfb 0	; end

*
* ABOUT page interface
*

* Build array of line pointers in INBUF.
AboutInit		; Returns with Acc = max lines
		jsr PRINT
		asc _INVERSE,_MT_ON,_U_ARROW,_D_ARROW,_MT_OFF,_NORMAL," Scroll / "
		asc _INVERSE,_MT_ON,_O_APPLE,_NORMAL,"-",_INVERSE,_U_ARROW,_D_ARROW,_MT_OFF,_NORMAL," Page"
		dfb 0

		lda #<:lines
		sta PTR
		lda #>:lines
		sta PTR+1
		ldx #0		; max lines
		ldy #0		; buffer offset
		
:save	lda PTR
		sta inbuf,y
		lda PTR+1
		iny
		sta inbuf,y
		iny
		inx

:loop	lda (PTR)
		bne :incr
		txa
		rts
		
:incr	lda (PTR)
		pha
		inc PTR
		bne :skip
		inc PTR+1
:skip	pla
		beq :save
		bne :incr

* Guide     00000000011111111112222222222333333333344444444445555555555666666666677777777778
*           12345678901234567890123456789012345678901234567890123456789012345678901234567890
*          |         +         +         +         +         +         +         +         +|
:lines	asc _INVERSE," Address Field Scanner ",_NORMAL,$00
		asc "Version 2.0beta",$00
		asc " ",$00
		asc "This application is my lame attempt to understand the old Apple Disk II",$00
		asc "interface.",$00
		asc " ",$00
		asc "As of Version 2 of AFSCANNER, multiple pages have been introduced to better",$00
		asc "review the contents of a disk buffer and analyze a disk.",$00
		asc " ",$00
		asc "Use the ",_INVERSE,_MT_ON,_O_APPLE,_MT_OFF,_NORMAL,"-key combinations to toggle pages.",$00
		asc " ",$00
		asc _INVERSE,_MT_ON,_O_APPLE,_MT_OFF,_NORMAL,"A = This about page.",$00
		asc _INVERSE,_MT_ON,_O_APPLE,_MT_OFF,_NORMAL,"F = Address field display.  " ; (cont)
		asc "(Assuming 'good' address fields on disk.)",$00
		asc _INVERSE,_MT_ON,_O_APPLE,_MT_OFF,_NORMAL,"B = Browse track buffer.",$00
		asc " ",$00
		asc "Source available at https://github.com/a2geek/afscanner",$00
		asc " ",$00
		asc "Global Keys",$00
		asc "===========",$00
		asc " ",$00
		asc _INVERSE,_MT_ON,_D_ARROW,_MT_OFF,_NORMAL,"  Scroll down 1 line",$00
		asc _INVERSE,_MT_ON,_U_ARROW,_MT_OFF,_NORMAL,"  Scroll up 1 line",$00
		asc _INVERSE,_MT_ON,_O_APPLE,_D_ARROW,_MT_OFF,_NORMAL," Page down 15 lines",$00
		asc _INVERSE,_MT_ON,_O_APPLE,_U_ARROW,_MT_OFF,_NORMAL," Page up 15 lines",$00
		asc _INVERSE,_MT_ON,_O_APPLE,_MT_OFF,_NORMAL,"Q  Quit to ProDOS",$00
		asc " ",$00
		asc "Address Field Display",$00
		asc "=====================",$00
		asc " ",$00
		asc "Display the list of Address Fields, the disk bytes, as well as the decoded",$00
		asc "values to indicate what sectors are phyiscally present and their order on disk.",$00
		asc "Note that the buffer is $2000 (8192) bytes long and some sectors will be",$00
		asc "repeated.",$00
		asc " ",$00
		asc "Headers are currently set to "
		dfb 1
		da  PROLOGUE
		dfb " ",1
		da  PROLOGUE+1
		dfb " ",1
		da  PROLOGUE+2
		dfb $00
		asc " ",$00
		asc "Browse Track Buffer",$00
		asc "===================",$00
		asc " ",$00
		asc "Browse the raw track buffer.  Header fields are hilighted.  Note that the ",$00
		asc "buffer is $2000 (8192) bytes long and some sectors will be repeated.",$00
		asc " ",$00
		asc "Headers are currently set to "
		dfb 1
		da  PROLOGUE
		dfb " ",1
		da  PROLOGUE+1
		dfb " ",1
		da  PROLOGUE+2
		dfb $00
		asc " ",$00
		asc "-END-",$00
		dfb 0

AboutDisplay	; Called with Acc = line
		pha
		lda #_CLREOL
		jsr COUT
		pla
		asl
		tay
		lda inbuf,y
		tax
		lda inbuf+1,y
		jsr PRINTP
		jmp PRCR

AboutKeypress	; Called with Acc = key
		; Do Nothing, continue loop
		jmp KeyboardWait

*
* FIELD Interface
*

FieldInit		; Returns with Acc = max lines
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
		cmp PROLOGUE,Y
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
		rts

FieldDisplay		; Called with Acc = line
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
		cmp PROLOGUE,Y
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
; HOWEVER, we are limited to 255 lines, so we actually skip the last 32 bytes
		lda #$ff
		rts

BrowseDisplay		; Called with Acc = line
; Calculate buffer address
		sta DATA
		stz DATA+1
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
		jmp FieldKeypress	; identical to Field ... for now at least


*
* DISK II routines
*

ReadTrack
		jsr PRINT
		dfb _HOME,$8D,4,$88
		asc "T="
		dfb _PRBYTE
		da DSTTRK
		dfb 0

		ldx SLOT16		; Slot*16
		lda MOTORON,x	; turn on drive
		lda DRV0EN,x	; Drive #1
		lda Q7L,x		; Q7 = Low and Q6 = Low => READ mode
		lda Q6L,x
		jsr ARMMOVE		; Go to our track (app init sets to 40, so first time this recalibrates)
; Fully read the track into buffer @ DATA
		jsr SETUPDATA
		ldy #0
:loop	lda Q6L,x
		bpl :loop
		sta (DATA),y
		iny
		bne :loop
		inc DATA+1
		dec TEMP
		bne :loop
		lda MOTOROFF,x
		rts


DISPLAY        LDA   #20
               STA   SAMPLES
               JSR   PRINT
               DFB   _HOME
               HEX   8D         ; CR
               DFB   7,$88     ; backspace into 1st line
               ASC   "T="
               DFB   _PRBYTE
               DA    CURTRK
               ASC   ".00"
               HEX   00

               JSR   CLRSCRN

               LDX   SLOT16     ; Restore X
               LDA   MOTORON,X  ; (gets turned off for keypress)

* Fully read the track into buffer @ DATA

               JSR SETUPDATA

               LDY #0
:LOOP          LDA Q6L,X
               BPL :LOOP
               STA (DATA),Y
               INY
               BNE :LOOP
               INC DATA+1
               DEC TEMP
               BNE :LOOP

               LDA   MOTOROFF,X

* Scan for our prologue and save bytes to buffer

               JSR SETUPDATA
               
SCAN           LDY #0
:LOOP          LDA (DATA),Y
               CMP PROLOGUE,Y
               BNE ADVANCE
               INY
               CPY #3
               BCC :LOOP

* Report out the prologue and decode address field

REPORT         LDY   #0
:1             LDA   (DATA),Y
               JSR   PRHEX
               INY
               CPY   #NUMBYTES
               BCC   :1
               LDA   #"V"
               LDY   #OFFSETV
               JSR   PRDATA
               LDA   #"T"
               LDY   #OFFSETT
               JSR   PRDATA
               LDA   #"S"
               LDY   #OFFSETS
               JSR   PRDATA
               LDA   #"C"
               LDY   #OFFSETC
               JSR   PRDATA
               JSR   PRCR

* Fill up the text page... only test when we print something out

TEST           DEC   SAMPLES
               BEQ   KEYPRESS

ADVANCE        INC DATA
               BNE SCAN
               INC DATA+1
               DEC TEMP
               BNE SCAN

* Handle keyboard

KEYPRESS       LDA   KEYBOARD
               BPL   KEYPRESS
               STA   KEYCLEAR
               CMP   #$E1       ; lower-case A
               BCC   :0
               CMP   #$FB       ; lower-case Z + 1
               BCS   :0
               AND   #%11011111 ; force to upper case
:0             CMP   #ESC
               BEQ   QUIT
               CMP   #"T"
               BEQ   GOTOTRK
               CMP   #"S"
               BEQ   RESCAN
               CMP   #"R"
               BNE   :1
               JMP   Recalibrate
:1             LDX   #-1
               CMP   #LARROW
               BEQ   CHGTRACK
               LDX   #1
               CMP   #RARROW
               BNE   KEYPRESS

CHGTRACK       TXA
               CLC
               ADC   CURTRK
               CMP   #35        ; cap at track 34
               BCS   RESCAN
REPOSN         STA   DSTTRK
               JSR   ARMMOVE
RESCAN         JMP   DISPLAY

GOTOTRK        JSR   CLRSCRN
               JSR   PRINT
               ASC   "Enter track number: $"00
               JSR   READBYTE
               BCS   RESCAN
               BCC   REPOSN

QUIT           JSR   PRODOSMLI
               DFB   _MLIQUIT
               DA    QUITPARM
               JMP   MAIN		; should never be executed

READERR        LDA   MOTOROFF,X
               JSR   PRINT
               ASC   "Unable to read track"00
               JMP   KEYPRESS

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

* Move the Disk II arm:

Recalibrate
		LDA #40
		STA CURTRK     ; force a recalibration
		STZ DSTTRK

ARMMOVE        LDA   CURTRK
               CMP   DSTTRK
               BNE   :MOVE
:THERE         LDX   SLOT16     ; restore standard slot
               RTS
:MOVE          PHP
               LDA   #0
               ROL              ; direction from CMP
               TAY
               LDA   CURTRK
               LSR
               TYA
               ROL              ; odd/even from track
               ASL              ; times 2
               TAY
               LDA   ARMTABLE,Y
               JSR   ARMPHASE
               LDA   ARMTABLE+1,Y
               JSR   ARMPHASE
               PLP
               BCS   :SUB1
:ADD1          INC   CURTRK
               BRA   ARMMOVE
:SUB1          DEC   CURTRK
               BRA   ARMMOVE

ARMPHASE       ORA   SLOT16
               TAX
               LDA   PHASEON,X
               LDA   #$56
               JSR   DELAY
               LDA   PHASEOFF,X
               RTS

* Standard 16 sector address prologue
PROLOGUE       HEX   D5AA96

* Phase table for moving arm
* Grouped by inward/outward movement, odd/even track
* To move the arm, two phases need to be triggered
ARMTABLE       HEX   0204
               HEX   0600
               HEX   0604
               HEX   0200

* ProDOS QUIT parameter tables
QUITPARM       DFB   4			; 4 parameters
               DFB   0			; 0 is the only quit type
               DA    0			; reserved
               DFB   0			; reserved
               DA    0			; reserved
