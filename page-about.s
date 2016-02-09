******************************************************************************************
*
* About Page / Help Page
*
******************************************************************************************

*
* ABOUT page interface
*

* Build array of line pointers in INBUF.
AboutInit		; Returns with A:Y = max lines, C = scrolling
		jsr PRINT
		asc _INVERSE,_MT_ON,_U_ARROW,_D_ARROW,_MT_OFF,_NORMAL," Scroll / "
		asc _INVERSE,_MT_ON,_O_APPLE,_NORMAL,"-",_INVERSE,_U_ARROW,_D_ARROW,_MT_OFF,_NORMAL," Page"
		dfb 0

		lda #<:lines
		sta PTR
		lda #>:lines
		sta PTR+1
		ldy #0		; max lines
		ldx #0		; buffer offset
		
:save	lda PTR
		sta inbuf,x
		inx
		lda PTR+1
		sta inbuf,x
		inx
		iny

:loop	lda (PTR)
		bne :incr
		dey			; we're one past...
		lda #0		; high byte of max lines; Y = low byte
		clc			; we need scrolling
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
		asc _INVERSE,_MT_ON,_O_APPLE,_MT_OFF,_NORMAL,"B = Browse track buffer.",$00
		asc _INVERSE,_MT_ON,_O_APPLE,_MT_OFF,_NORMAL,"C = Display buffer counts.",$00
		asc _INVERSE,_MT_ON,_O_APPLE,_MT_OFF,_NORMAL,"F = Address field display.  " ; (cont)
		asc "(Assuming 'good' address fields on disk.)",$00
		asc _INVERSE,_MT_ON,_O_APPLE,_MT_OFF,_NORMAL,"G = Graphical disk display.",$00
		asc " ",$00
		asc _INVERSE,_MT_ON,_O_APPLE,_MT_OFF,_NORMAL,"* = Enter Apple monitor.",$00
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
		da  AddressFieldPrologue
		dfb " ",1
		da  AddressFieldPrologue+1
		dfb " ",1
		da  AddressFieldPrologue+2
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
		da  AddressFieldPrologue
		dfb " ",1
		da  AddressFieldPrologue+1
		dfb " ",1
		da  AddressFieldPrologue+2
		dfb $00
		asc " ",$00
		asc "Graphical Disk Display",$00
		asc "======================",$00
		asc " ",$00
		asc "Scans an entire disk and graphically displays where sync ($FF) bytes appear on",$00
		asc "disk.  Note that the length of each bar indicates how many sync bytes were in",$00
		asc "that section of the disk.  Each bar represents approximately 46 bytes.",$00
		asc " ",$00
		asc "Display Buffer Counts",$00
		asc "=====================",$00
		asc " ",$00
		asc "Count the number of disk bytes in the buffer and display totals.  This may be",$00
		asc "helpful when trying to determine address field bytes.",$00
		asc " ",$00
		asc "To assist with identification, non-zero values or 13-sector bytes or 16-sector",$00
		asc "bytes may be highlighted.",$00
		asc " ",$00
		asc "-END-",$00
		dfb 0

AboutDisplay	; Called with A:Y = line
		phy		; assuming < 256 lines
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

