********************************
*
* Address Field Scanner
*
* 1/16/2016: Version 1
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
_HOME          =     $99        ; home the cursor, not cls
_CLREOL        =     $9D        ; clear to EOL

* Variable locations:

               DUM   $0         ; ZP locs
SAMPLES        DFB   0          ; number of sector samples
CURTRK         DFB   0          ; current track
DSTTRK         DFB   0          ; destination track
PTR            DA    0          ; primary print pointer
PTR2           DA    0          ; secondary print pointer
SLOT16         DFB   0          ; slot# * 16
COUNTER        DA    0          ; fail counter
TEMP           DFB   0          ; local variable
               DEND 

BUF            =     $280

* High ASCII constants

CTRLH          =     "H"-$40
LARROW         =     CTRLH
CTRLU          =     "U"-$40
RARROW         =     CTRLU
ESC            =     $9B

* ROM routines:

DELAY          =     $FCA8
GETCH          =     $FD0C
PRCR           =     $FD8E
PRHEX          =     $FDDA 
COUT           =     $FDED

* I/O addresses:

KEYBOARD       =     $C000
KEYCLEAR       =     $C010

* Disk II addresses:

PHASEOFF       =     $C080
PHASEON        =     $C081
MOTOROFF       =     $C088
MOTORON        =     $C089
DRV0EN         =     $C08A
Q6L            =     $C08C
Q7L            =     $C08E

* Display screen

MAIN           JSR   $C300      ; Assuming 80 columns
               JSR   PRINT
               DFB   _CLS
               DFB   _INVERSE
               ASC   " Address Field Scanner 2.0beta "
               DFB   _NORMAL
               DFB   23,$8D     ; repeat 8D 23x
               DFB   _INVERSE
               ASC   "<-"
               DFB   _NORMAL
               ASC   ", "
               DFB   _INVERSE
               ASC   "->"
               DFB   _NORMAL
               ASC   " Track / re"
               DFB   _INVERSE
               ASC   "S"
               DFB   _NORMAL
               ASC   "can / "
               DFB   _INVERSE
               ASC   "R"
               DFB   _NORMAL
               ASC   "ecalibrate / goto "
               DFB   _INVERSE
               ASC   "T"
               DFB   _NORMAL
               ASC   "rack / "
               DFB   _INVERSE
               ASC   "ESC"
               DFB   _NORMAL
               ASC   " quit"
               DFB   _HOME
               HEX   8D8D
               HEX   00

* Turn on drive 1, set read mode, init variables

INIT           LDX   #$60       ; assumption = slot 6 
               STX   SLOT16
               LDA   MOTORON,X
               LDA   DRV0EN,X
               LDA   Q7L,X
               LDA   Q6L,X
RECAL          LDA   #40
               STA   CURTRK     ; force a recalibration
               STZ   DSTTRK
               JSR   ARMMOVE

DISPLAY        LDA   #20
               STA   SAMPLES
               JSR   PRINT
               DFB   _HOME
               HEX   8D         ; CR
               DFB   11,$88     ; backspace into 1st line
               ASC   "Track = "
               DFB   _PRBYTE
               DA    CURTRK
               HEX   00

               JSR   CLRSCRN

               LDX   SLOT16     ; Restore X
               LDA   MOTORON,X  ; (gets turned off for keypress)
               LDA   #>FAILBYTS
               STA   COUNTER+1
               LDA   #<FAILBYTS
               STA   COUNTER

* Scan for our prologue and save bytes to buffer

SCAN           LDY   #0
:LOOP          LDA   Q6L,X      ; match prologue bytes
               BPL   :LOOP
:CONT          CMP   PROLOGUE,Y
               BEQ   :HIT
               DEC   COUNTER
               BNE   SCAN
               DEC   COUNTER+1
               BNE   SCAN
               JMP   READERR
:HIT           STA   BUF,Y
               INY
               CPY   #3
               BCC   :LOOP
:MORE          LDA   Q6L,X      ; scan for rest of data bytes
               BPL   :MORE
               STA   BUF,Y
               INY
               CPY   #NUMBYTES
               BCC   :MORE

* Report out the prologue and decode address field

REPORT         LDY   #0
:1             LDA   BUF,Y 
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

* Fill up the text page...

TEST           DEC   SAMPLES
               BNE   SCAN
               LDA   MOTOROFF,X

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
               JMP   RECAL
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

QUIT           LDA   #_CLREOL
               JMP   COUT

READERR        LDA   MOTOROFF,X
               JSR   PRINT
               ASC   "Unable to read track"00
               JMP   KEYPRESS

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
               LDA   BUF,Y
               SEC
               ROL
               AND   BUF+1,Y
               JMP   PRHEX

* Output routine uses simple RLE type encoding
*   High bit set = character;
*   High bit clear =
*       $00 = exit
*       $01 = print byte from address
*       $02-$7F = repeat next character

PRINT          PLA
               STA   PTR
               PLA
               STA   PTR+1
:MORE          JSR   INCPTR
               LDX   #1         ; Assume 1 char to be output
               LDA   (PTR)
               BEQ   :EXIT
               BMI   :SINGLE
               CMP   #1
               BNE   :REPEAT
               JSR   INCPTR
               LDA   (PTR)
               STA   PTR2
               JSR   INCPTR
               LDA   (PTR)
               STA   PTR2+1 
               LDA   (PTR2)
               JSR   PRHEX
               BRA   :MORE
:REPEAT        TAX
               JSR   INCPTR
               LDA   (PTR)
:SINGLE        JSR   COUT
               DEX
               BNE   :SINGLE
               BEQ   :MORE
:EXIT          LDA   PTR+1
               PHA
               LDA   PTR
               PHA
               RTS

INCPTR         INC   PTR
               BNE   :EXIT
               INC   PTR+1
:EXIT          RTS

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
