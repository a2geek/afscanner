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
