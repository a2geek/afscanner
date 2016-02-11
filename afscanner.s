******************************************************************************************
*
* Address Field Scanner
* ========================================================================================
* This is a simple Disk II scanner
* to identify address field contents
*
* 1/16/2016: Version 1.0
*
* 1/29/2016: Version 2.0
* - Restructuring to support paging and scrolling.
* - Split source into multiple files as one file finally got way too big.
* - Slowly switching from spaces to tabs as an experiment in code formatting.
* - Pages available:
*	+ About (default page)
*	+ Headers (updated Address Field Header information)
*	+ Browse track buffer (hilights Address Field Header bytes)
*	+ Graphical disk display
*	+ Buffer counts 
*
******************************************************************************************

		TYP SYS

		XC				 ; enable 65C02
		
		use macros

* Program Locations

OriginAddress = $2000
ProgramAddress = $6000
ProgramLength = ProgramEnd-ProgramAddress

* Constants:

NUMBYTES	   =	 29			; Number of bytes on screen
OFFSETV		   =	 3			; Offset to volume
OFFSETT		   =	 OFFSETV+2	; Offset to track
OFFSETS		   =	 OFFSETT+2	; Offset to sector
OFFSETC		   =	 OFFSETS+2	; Offset to checksum
FAILBYTS	   =	 6656		; Number of bytes before failure

* 80 column card / print controls:

_PRBYTE		   =	 1			; print byte @ addr
_CLS		   =	 $8C		; clear screen
_INVERSE	   =	 $8F
_NORMAL		   =	 $8E
_MT_OFF		   =	 $98		; disable MouseText 
_HOME		   =	 $99		; home the cursor, not cls
_MT_ON		   =	 $9B		; enable MouseText for uppercase inverse characters
_CLREOL		   =	 $9D		; clear to EOL

_C_APPLE	   =	 "@"		; Closed Apple
_O_APPLE	   =	 "A"		; Open Apple
_L_ARROW	   =	 "H"		; Left Arrow
_R_ARROW	   =	 "U"		; Right Arrow
_U_ARROW	   =	 "K"		; Up Arrow
_D_ARROW	   =	 "J"		; Down Arrow
_H_LINE		   =	 "S"		; Horizontal Line (full width)


* Variable locations:

			DUM $0		; ZP locs
SAMPLES		DFB 0		; number of sector samples
CURTRK		DFB 0		; current track
DSTTRK		DFB 0		; destination track
PTR			DA	0		; primary print pointer
PTR2		DA	0		; secondary print pointer
SLOT16		DFB 0		; slot# * 16
COUNTER		DA	0		; fail counter
TEMP		DFB 0		; local variable
TEMPY		DFB 0		; local Y coordinate
DATA		DA	0		; data buffer

_init		da	0		; current page init
_display	da	0		; current page line display handler
_keypress	da	0		; current page keypress handler
noscroll	dfb 0		; flag for scrolling (in high bit)
topline		dw	0		; current line at top of page
maxlines	dw	0		; maximum number of lines available
printline	dw	0		; line being printed
			DEND

inbuf		= $200		; reusable buffer

DATASTART	= $4000
DATAEND		= $6000
DATALEN		= DATAEND-DATASTART

PAGELEN		= 20		; number of lines displayed in content window

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

PRODOSMLI	   =	 $BF00
_MLIQUIT	   =	 $65

* ROM routines and associated addresses:

TEXT		= $FB2F
HGR			= $F3E2
HPOSN		= $F411
HBAS		= $26

DELAY		= $FCA8
GETCH		= $FD0C
PRCR		= $FD8E
PRHEX		= $FDDA
COUT		= $FDED
MONITOR		= $FF69
USRADR		= $3F8

* I/O addresses:

KEYBOARD	= $C000
KEYCLEAR	= $C010
OpenApple	= $C061

* Relocate application above HGR and HGR2 page

		org OriginAddress

		lda #>OriginEnd
		sta PTR+1
		lda #<OriginEnd
		sta PTR
		lda #>ProgramAddress
		sta PTR2+1
		stz PTR2
		ldy #0
		ldx #>ProgramLength+255 ; account for non-zero low byte
:0		lda (PTR),y
		sta (PTR2),y
		iny
		bne :0
		inc PTR+1
		inc PTR2+1
		dex
		bne :0
		jmp ProgramAddress
OriginEnd		; Marker for code relocation

* Display screen

		org ProgramAddress

MAIN	lda #$4C		; JMP
		sta USRADR
		lda #<ProgramAddress
		sta USRADR+1
		lda #>ProgramAddress
		sta USRADR+2

		JSR	  $C300		 ; Assuming 80 columns
		JSR	  PRINT
		DFB	  _CLS
		ASC	  "AFScanner",$8D
		DFB	  _MT_ON,_INVERSE,80,_H_LINE,_NORMAL,_MT_OFF  ; wraps!
		DFB	  20,$8D
		DFB	  _MT_ON,_INVERSE,80,_H_LINE,_NORMAL,_MT_OFF  ; wraps!
		ASC	  _MT_ON,_INVERSE,_L_ARROW,_NORMAL,", ",_INVERSE,_R_ARROW,_NORMAL,_MT_OFF," Track / "
		ASC	  "re",_INVERSE,"S",_NORMAL,"can / "
		ASC	  _INVERSE,"R",_NORMAL,"ecalibrate / "
		ASC	  "goto ",_INVERSE,"T",_NORMAL,"rack / "
		ASC	  _INVERSE,"ESC",_NORMAL," quit"
		DFB	  _HOME
		HEX	  8D8D
		HEX	  00

* Setup local variables

Initialize
		ldx #$60		; assumption = slot 6
		stx SLOT16
		lda #40
		sta CURTRK		; force a recalibration
		stz DSTTRK
		lda #"A"		; default screen is "About" page
		jsr SetScreen
		
		; Falls through to SetupPage in page.s

		put page
		put page-about
		put page-field
		put page-browse
		put page-graphics
		put page-count
		put page-test
		
		put diskii
		put util

ProgramEnd
