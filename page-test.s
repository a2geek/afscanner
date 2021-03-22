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
		jsr NewPrint
		asc "%v",$17,"Bottom Line? %m@ (open apple)",$00

		lda #>535
		ldy #<535		; Nice odd number of lines > 256
		rts

TestDisplay
		sty DATA
		sta DATA+1
		
		jsr NewPrint
		asc "%w"
		da DATA
		dfb $8D,0
		rts

TestKeypress
		jmp KeyboardWait

