;New - New Project - Set up new dimensions

NewProjectScreen
;      0123456789012345678901234567890123456789
 .byt "   Character Width(fixed):1",13
 .byt "  Character Height(fixed):6",13
 .byt "Character Quantity(fixed):256",13
 .byt "          Character Memory 1536",13
 .byt 13
 .byt "         Block Width(2-8):%1",13
 .byt "        Block Height(2-8):%2",13
 .byt "     Block Quantity(2-99):%3",13
 .byt "              Block Memory %4",13
 .byt 13
 .byt "          Map Width(2-99):%5",13
 .byt "         Map Height(2-99):%6",13
 .byt "       Map Quantity(1-99):%7",13
 .byt "                Map Memory %8",13
 .byt 13
 .byt "                     Total %9 Bytes%A",13
 .byt "                  Possible 19968 Bytes",13
 .byt 13
 .byt "             Screen Width:%B",13
 .byt "                 X Scroll:%C",13
 .byt "            Screen Height:%D",13
 .byt "                 Y Scroll:%E",0

CompileNewProjectScreen
	lda #<NewProjectScreen
	sta source
	lda #>NewProjectScreen
	sta source+1
	
	lda #<$BB80+40
	sta screen
	lda #>$BB80+40
	sta screen+1
	
	ldy #00
	sty ScreenXIndex
	sty SourceIndex

nploop	ldy SourceIndex
	lda (source),y
	
	;Look for attribute codes
	beq npProcessEnd
	cmp #13
	beq npProcessReturn
	
	;Look for markers
	cmp #"%"
	beq npProcessMarker
	
	;Plot
	ldy ScreenXIndex
	sta (screen),y
	
	inc ScreenXIndex
npRent1	inc SourceIndex
	bne nploop
	inc source+1
	jmp nploop


npProcessEnd
	rts
	
npProcessReturn
	lda #00
	sta ScreenXIndex
	lda screen
	clc
	adc #40
	sta screen
	lda screen+1
	adc #00
	sta screen+1
	jmp npRent1

npProcessMarker
	iny
	lda (source),y
	cmp #"A"
.(
	bcc skip1
	sbc #7
skip1	sec
	sbc #"1"
.)
	tax
	lda npMarkerCodeVectorLo,x
.(
	sta vector1+1
	lda npMarkerCodeVectorHi,x
	sta vector1+2
vector1	jsr $dead
.)
	inc SourceIndex
	jmp npRent1

npBlockWidth	.byt 0
npBlockHeight   .byt 0
npBlockQuantity .byt 0
npMapWidth      .byt 0
npMapHeight     .byt 0
npMapQuantity   .byt 0
npBlockTotalLo  .byt 0
npBlockTotalHi  .byt 0
npMapTotalLo    .byt 0
npMapTotalHi    .byt 0
npTotalLo       .byt 0
npTotalHi       .byt 0

npCursorY	.byt 0

npMarkerCodeVectorLo
 .byt <npcBlockWidth
 .byt <npcBlockHeight
 .byt <npcBlockQuantity
 .byt <npcBlockTotal
 .byt <npcMapWidth
 .byt <npcMapHeight
 .byt <npcMapQuantity
 .byt <npcMapTotal
 .byt <npcMemoryTotal
 .byt <npcColourTotal
 .byt <npcScreenWidth
 .byt <npcScrollX
 .byt <npcScreenHeight
 .byt <npcScrollY
npMarkerCodeVectorHi
 .byt >npcBlockWidth
 .byt >npcBlockHeight
 .byt >npcBlockQuantity
 .byt >npcBlockTotal
 .byt >npcMapWidth
 .byt >npcMapHeight
 .byt >npcMapQuantity
 .byt >npcMapTotal
 .byt >npcMemoryTotal
 .byt >npcColourTotal
 .byt >npcScreenWidth
 .byt >npcScrollX
 .byt >npcScreenHeight
 .byt >npcScrollY

npcBlockWidth
	lda npBlockWidth
	sta fecpdInputLo
	lda #00
	sta fecpdInputHi
	jmp fecPlotDecimal

npcBlockHeight
	lda npBlockHeight
	sta fecpdInputLo
	lda #00
	sta fecpdInputHi
	jmp fecPlotDecimal

npcBlockQuantity
	lda npBlockQuantity
	sta fecpdInputLo
	lda #00
	sta fecpdInputHi
	jmp fecPlotDecimal

npcBlockTotal
	lda npBlockWidth
	ldx npBlockHeight
	ldy #0
	jsr Mult16Bit
	lda npBlockQuantity
	jsr Mult16Bit
	stx npBlockTotalLo
	sty npBlockTotalHi
	stx fecpdInputLo
	sty fecpdInputHi
	jmp fecPlotDecimal

npcMapWidth
	lda npMapWidth
	sta fecpdInputLo
	lda #00
	sta fecpdInputHi
	jmp fecPlotDecimal

npcMapHeight
	lda npMapHeight
	sta fecpdInputLo
	lda #00
	sta fecpdInputHi
	jmp fecPlotDecimal

npcMapQuantity
	lda npMapQuantity
	sta fecpdInputLo
	lda #00
	sta fecpdInputHi
	jmp fecPlotDecimal

npcMapTotal
	lda npMapWidth
	ldx npMapHeight
	ldy #0
	jsr Mult16Bit
	lda npMapQuantity
	jsr Mult16Bit
	stx npMapTotalLo
	sty npMapTotalHi
	stx fecpdInputLo
	sty fecpdInputHi
	jmp fecPlotDecimal

npcMemoryTotal
	lda #<1536
	clc
	adc npMapTotalLo
	sta npTotalLo
	lda #>1536
	adc npMapTotalHi
	sta npTotalHi
	
	lda npTotalLo
	adc npBlockTotalLo
	sta npTotalLo
	sta fecpdInputLo
	lda npTotalHi
	adc npBlockTotalHi
	sta npTotalHi
	sta fecpdInputHi
	
	jmp fecPlotDecimal

npcColourTotal
	;Inset Green if total less or equal to Possible else Red
	ldx #2
	lda #<19968
	sec
	sbc npTotalLo
	lda #>19968
	sbc npTotalHi
.(
	bcs skip1
	dex
skip1	stx $BB84+16*40
.)
	stx TotalColourResult
	rts
	
npcScreenWidth
	lda #00
	sta npBlocksWide
	sta fecpdInputHi
.(
loop1	inc npBlocksWide
	tax
	clc
	adc npBlockWidth
	cmp #40
	bcc loop1
skip2	stx fecpdInputLo

	;if (Map Width x Block Width) < Screen Width THEN show former
	lda npMapWidth
	ldx npBlockWidth
	ldy #00
	jsr Mult16Bit
	cpy #01
	bcs skip1
	cpx fecpdInputLo
	bcs skip1
	stx fecpdInputLo
skip1
.)
	jmp fecPlotDecimal
	
npcScreenHeight
	lda #00
	sta npBlocksHigh
	sta fecpdInputHi
.(
loop1	inc npBlocksHigh
	tax
	clc
	adc npBlockHeight
	cmp #33
	bcc loop1
skip2	stx fecpdInputLo

	;if (Map Width x Block Width) < Screen Width THEN show former
	lda npMapHeight
	ldx npBlockHeight
	ldy #00
	jsr Mult16Bit
	cpy #01
	bcs skip1
	cpx fecpdInputLo
	bcs skip1
	stx fecpdInputLo
skip1
.)
	jmp fecPlotDecimal

npcScrollX
	ldx npBlocksWide
	lda #"Y"
	cpx npMapWidth
.(
	bcc skip1
	lda #"N"
skip1	ldy ScreenXIndex
	sta (screen),y
.)
	inc ScreenXIndex
	rts
	
	
npcScrollY
	ldx npBlocksHigh
	lda #"Y"
	cpx npMapHeight
.(
	bcc skip1
	lda #"N"
skip1	ldy ScreenXIndex
	sta (screen),y
.)
	inc ScreenXIndex
	rts

npBlocksWide	.byt 0
npBlocksHigh	.byt 0
	
CalculatenpBlocksWideAndHigh
	lda #00
	sta npBlocksWide
	ldx npMapWidth
.(
loop1	inc npBlocksWide
	dex
	beq skip2
	clc
	adc npBlockWidth
	cmp #39
	bcc loop1
skip2	
	
	;BlocksHigh
	lda #00
	sta npBlocksHigh
	ldx npMapHeight
	
loop2	inc npBlocksHigh
	dex
	beq skip3
	clc
	adc npBlockHeight
	cmp #32
	bcc loop2
skip3	
.)
	rts
	

	
TotalColourResult	.byt 0

NewProject
	;Transfer current dimensions to new project dimensions
	lda BlockWidth
	sta npBlockWidth
	lda BlockHeight
	sta npBlockHeight
	lda BlockQuantity
	sta npBlockQuantity
	lda MapWidth
	sta npMapWidth
	lda MapHeight
	sta npMapHeight
	lda MapQuantity
	sta npMapQuantity
	
	jsr CLS
	jsr CompileNewProjectScreen
npInputDriver
	jsr PlotNPCursor
.(	
loop1	jsr ROM_GTORKB
	bpl loop1
.)	
	sta PressedBlackKey
	
	jsr DeleteNPCursor

	ldy #6
.(
loop1	lda PressedBlackKey
	cmp npBlackKey,y
	beq skip2
	dey
	bpl loop1
	jmp npInputDriver
skip2
.)	
	lda npCommandVectorLo,y
.(
	sta vector1+1
	lda npCommandVectorHi,y
	sta vector1+2
vector1	jsr $dead
.)
	jmp npInputDriver
	
npBlackKey
 .byt 11	;Cursor Up
 .byt 10        ;Cursor Down
 .byt "-"       ;Decrement
 .byt "="       ;Increment
 .byt 27        ;Abort back to Main Menu
 .byt 13        ;Finished!
 .byt "D"       ;Restore entry to default value
npCommandVectorLo
 .byt <npCrsrUp		 ;Cursor Up
 .byt <npCrsrDown        ;Cursor Down
 .byt <npDecrement       ;Decrement
 .byt <npIncrement       ;Increment
 .byt <npAbort        	 ;Abort back to Main Menu
 .byt <npFinished        ;Finished!
 .byt <npRestore       	 ;Restore entry to default value

npCommandVectorHi
 .byt >npCrsrUp		 ;Cursor Up
 .byt >npCrsrDown        ;Cursor Down
 .byt >npDecrement       ;Decrement
 .byt >npIncrement       ;Increment
 .byt >npAbort        	 ;Abort back to Main Menu
 .byt >npFinished        ;Finished!
 .byt >npRestore       	 ;Restore entry to default value

npCrsrUp
	lda npCursorY
.(
	beq skip1
	dec npCursorY
skip1	rts
.)
	
npCrsrDown
	lda npCursorY
	cmp #5
.(
	bcs skip1
	inc npCursorY
skip1	rts
.)

npDecrement
	ldx npCursorY
	lda npBlockWidth,x
	cmp LowerExtreme,x
.(
	beq skip1
	dec npBlockWidth,x
	jsr CLS
	jsr CompileNewProjectScreen
skip1	rts
.)

npIncrement
	ldx npCursorY
	lda npBlockWidth,x
	cmp UpperExtreme,x
.(
	beq skip1
	inc npBlockWidth,x
	jsr CLS
	jsr CompileNewProjectScreen
skip1	rts
.)

npRestore
	ldx npCursorY
	lda BlockWidth,x
	sta npBlockWidth,x
	jsr CLS
	jmp CompileNewProjectScreen
	

npAbort	pla
	pla
	jsr CLS
	jmp CompileFilesScreen

npFinished
	;Check colour confirmation
	lda TotalColourResult
	cmp #2
.(
	beq skip4
skip5	jmp skip1
skip4	;Confirm changes
	jsr CLS
	ldy #01
	lda #TM_AREYOUSURE
	jsr d2pDisplayTextMessage
	jsr WaitOnKey
	cmp #"Y"
	bne skip5
	
	;transfer dimensions to current
	lda npBlockWidth
	sta BlockWidth
	lda npBlockHeight
	sta BlockHeight
	lda npBlockQuantity
	sta BlockQuantity
	lda npMapWidth
	sta MapWidth
	lda npMapHeight
	sta MapHeight
	lda npMapQuantity
	sta MapQuantity
	
	;Calculate...
	;BlockSize
	lda BlockWidth
	ldx BlockHeight
	jsr Mult8Bit
	sta BlockSize
	
	;BlocksWide
	jsr CalculateBlocksWideAndHigh

	;Erase Character Set
	ldx #00
loop3	lda #%01101010
	sta CharacterMemory,x
	sta CharacterMemory+256*1,x
	sta CharacterMemory+256*2,x
	sta CharacterMemory+256*3,x
	sta CharacterMemory+256*4,x
	sta CharacterMemory+256*5,x
	lda #00
	sta CollisionCode,x
	dex
	bne loop3
	
	;
	

	;Since new project reset variables
	lda #00
	sta MapID
	sta MapBaseX
	sta MapBaseY
	sta MapScreenX
	sta MapScreenY
	
	sta CharacterID
	sta GridCursorX
	sta GridCursorY
	sta CharCursorX
	sta CharCursorY
	
	sta BlockID
	sta BlockCursorX
	sta BlockCursorY
	
	sta Grabbed
	jsr RecalcCharCursorLoc
	lda #6
	sta TopColour
	lda #3
	sta BotColour

	lda #TM_NOWSAVEPROJECT
	ldy #4
	jsr d2pDisplayTextMessage
	jsr WaitOnKey

skip1	;return to files editor
.)
	pla
	pla
	jsr CLS
	jmp CompileFilesScreen
	
;skip1	jsr CLS
;.)
;	jmp CompileNewProjectScreen
	
PlotNPCursor
	ldx npCursorY
	lda npCursorScreenLo,x
	sta screen
	lda npCursorScreenHi,x
	sta screen+1
	ldy #00
	lda (screen),y
	ora #128
	sta (screen),y
	rts

	
DeleteNPCursor
	ldx npCursorY
	lda npCursorScreenLo,x
	sta screen
	lda npCursorScreenHi,x
	sta screen+1
	ldy #00
	lda (screen),y
	and #127
	sta (screen),y
	rts

npCursorScreenLo
 .byt <$BB80+26+6*40
 .byt <$BB80+26+7*40
 .byt <$BB80+26+8*40
 .byt <$BB80+26+11*40
 .byt <$BB80+26+12*40
 .byt <$BB80+26+13*40
npCursorScreenHi
 .byt >$BB80+26+6*40
 .byt >$BB80+26+7*40
 .byt >$BB80+26+8*40
 .byt >$BB80+26+11*40
 .byt >$BB80+26+12*40
 .byt >$BB80+26+13*40
UpperExtreme
 .byt 8
 .byt 8
 .byt 99
 .byt 99
 .byt 99
 .byt 99
LowerExtreme
 .byt 2
 .byt 2
 .byt 2
 .byt 2
 .byt 2
 .byt 1
