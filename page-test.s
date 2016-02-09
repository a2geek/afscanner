******************************************************************************************
*
* Test Page
* 
* Used for independent testing various pieces of funcionality.
*
******************************************************************************************

*
* TEST interface
*

TestInit
		lda #>535
		ldy #<535		; Nice odd number of lines > 256
		rts

TestDisplay
		phy
		pha
		lda #_CLREOL
		jsr COUT
		pla
		jsr PRHEX
		pla
		jsr PRHEX
		jmp PRCR

TestKeypress
		jmp KeyboardWait

