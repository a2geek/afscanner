******************************************************************************************
*
* Disk II routines
* ========================================================================================
*
******************************************************************************************

* Disk II addresses:

PHASEOFF	=     $C080
PHASEON		=     $C081
MOTOROFF	=     $C088
MOTORON		=     $C089
DRV0EN		=     $C08A
DRV1EN		=     $C08B
Q6L			=     $C08C
Q7L			=     $C08E

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

* Phase table for moving arm
* Grouped by inward/outward movement, odd/even track
* To move the arm, two phases need to be triggered
ARMTABLE       HEX   0204
               HEX   0600
               HEX   0604
               HEX   0200

* Standard DOS 13-sector and 16-sector address/data field bytes

AddressFieldPrologue
		hex D5AA96
AddressFieldEpilogue
		hex DEAAEB
DataFieldPrologue
		hex D5AAAD
DataFieldEpilogue
		hex DEAAEB

* DOS 13-sector translate table

Translate13
		hex ABADAEAFB5B6B7BABBBDBEBFD6D7DADB
		hex DDDEDFEAEBEDEEEFF5F6F7FAFBFDFEFF

* DOS 16-sector translate table

Translate16
		hex 96979A9B9D9E9FA6A7ABACADAEAFB2B3
		hex B4B5B6B7B9BABBBCBDBEBFCBCDCECFD3
		hex D6D7D9DADBDCDDDEDFE5E6E7E9EAEBEC
		hex EDEEEFF2F3F4F5F6F7F9FAFBFCFDFEFF
