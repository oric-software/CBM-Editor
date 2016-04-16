;Block Editor

;0123456789012345678901234567890123456789
; BBBBBBBB       BBBBBBBB       BBBBBBBB
; BBBBBBBB       BBBBBBBB       BBBBBBBB
; BBBBBBBB       BBBBBBBB       BBBBBBBB
; BBBBBBBB       BBBBBBBB       BBBBBBBB
; BBBBBBBB       BBBBBBBB       BBBBBBBB
; BBBBBBBB       BBBBBBBB       BBBBBBBB
; BBBBBBBB       BBBBBBBB       BBBBBBBB
; BBBBBBBB       BBBBBBBB       BBBBBBBB
;    FF             00             01

PlotBlocks
	lda BlockID
	sec
	sbc #1
.(
	bcs skip1
	lda BlockQuantity
	sbc #00
skip1	jsr CalculateBlockAddress
.)
	lda #<$A001+40*60
	sta ScreenLocationLo
	lda #>$A001+40*60
	sta ScreenLocationHi
	jsr PlotBlock
	
	lda BlockID
	jsr CalculateBlockAddress
	lda #<$A010+40*60
	sta ScreenLocationLo
	lda #>$A010+40*60
	sta ScreenLocationHi
	jsr PlotBlock

	lda BlockID
	clc
	adc #1
	cmp BlockQuantity
.(
	bcc skip1
	lda #00
skip1	jsr CalculateBlockAddress
.)
	lda #<$A01F+40*60
	sta ScreenLocationLo
	lda #>$A01F+40*60
	sta ScreenLocationHi
	jsr PlotBlock
	rts

PlotBlock
	lda TempL
	sta source
	lda TempH
	sta source+1
	
	ldx #00
.(
loop2	ldy BlockWidth
	dey
	
loop1	lda (source),y
	jsr PlotCharInBlock
	
	dey
	bpl loop1
	
	lda source
	clc
	adc BlockWidth
	sta source
	bcc skip1
	inc source+1 
	
skip1	inx
	cpx BlockHeight
	bcc loop2
.)
	rts
	
PlotCharInBlock
	jsr FetchCharacterAddress
	lda TempL
.(
	sta vector1+1
	lda TempH
	sta vector1+2
	
	tya
	clc
	adc ScreenLocationLo
	sta screen
	lda ScreenLocationHi
	adc #00
	sta screen+1
	
	lda screen
	adc RelativeCharRowLo,x
	sta screen
	lda screen+1
	adc RelativeCharRowHi,x
	sta screen+1
	
	stx TempX
	sty TempY
	
	ldx #5
vector1	lda $dead,x
	ldy Offset1x6,x
	sta (screen),y
	dex
	bpl vector1
.)	
	ldx TempX
	ldy TempY
	
	rts
	
PlotBlockEditorColours
	ldx #00
	ldy #10
	lda #CYANYELLOWCHAR
.(
loop1	jsr PlotEditorChar
	iny
	cpy #18
	bcc loop1
.)
	rts
	
RelativeCharRowLo
 .byt 0
 .byt <240*1
 .byt <240*2
 .byt <240*3
 .byt <240*4
 .byt <240*5
 .byt <240*6
 .byt <240*7
RelativeCharRowHi	
 .byt 0
 .byt >240*1
 .byt >240*2
 .byt >240*3
 .byt >240*4
 .byt >240*5
 .byt >240*6
 .byt >240*7

CalculateBlockAddress
.(
	sta vector1+1
	;Get size
	lda BlockHeight
	ldx BlockWidth
	jsr Mult8Bit
	sta BlockSize
	
vector1	ldx #00
.)
	ldy #00
	jsr Mult16Bit
	
	txa
	clc
	adc #<BlockMemory
	sta TempL
	tya
	adc #>BlockMemory
	sta TempH
	
	rts
	
PlotBlockIndexes
	ldy #00
	lda BlockID
	sec
	sbc #1
.(
	bcs skip1
	lda BlockQuantity
	sbc #00
skip1	jsr Hires2DH
.)	
	ldy #15
	lda BlockID
	jsr Hires2DH
	
	ldy #30
	lda BlockID
	clc
	adc #1
	cmp BlockQuantity
.(
	bcc skip1
	lda #00
skip1	jmp Hires2DH
.)


Hires2DH
	pha
	lsr
	lsr
	lsr
	lsr
	jsr Hires1DH
	pla
	and #15
Hires1DH
	cmp #10
.(
	bcc skip1
	adc #6
skip1	asl
.)
	asl
	asl
	tax
	lda $9980,x
	ora #64
	sta $B0E4,y

	lda $9981,x
	ora #64
	sta $B0E4+40*1,y

	lda $9982,x
	ora #64
	sta $B0E4+40*2,y
	
	lda $9983,x
	ora #64
	sta $B0E4+40*3,y

	lda $9984,x
	ora #64
	sta $B0E4+40*4,y

	lda $9985,x
	ora #64
	sta $B0E4+40*5,y

	lda $9986,x
	ora #64
	sta $B0E4+40*6,y

	iny
	rts

beInputDriver
	lda #ED_BLOCK
	sta CurrentCBMEditor
	jsr UpdateLegend
.(
loop1	jsr ROM_GTORKB
	bpl loop1
.)
	sta PressedBlackKey
	lda $0209
	sta PressedRedKey
	
	ldy #19
.(
loop1	lda PressedBlackKey
	cmp beBlackKey,y
	bne skip1
	lda PressedRedKey
	cmp beRedKey,y
	beq skip2
skip1	dey
	bpl loop1
	jmp beInputDriver
skip2
.)	
	lda beCommandVectorLo,y
.(
	sta vector1+1
	lda beCommandVectorHi,y
	sta vector1+2
vector1	jsr $dead
.)
	jmp beInputDriver

beCommandVectorLo
 .byt <beMoveCursorLeftWithinBlock
 .byt <beMoveCursorRightWithinBlock
 .byt <beMoveCursorDownWithinBlock
 .byt <beMoveCursorUpWithinBlock
 
 .byt <ceDecrementCharacterID
 .byt <ceIncrementCharacterID
 
 .byt <beJumpToCharacterEditor
 .byt <beJumpToMapEditor
 .byt <beJumpToFiles
 
 .byt <bePlotCharacter
 .byt <beDeleteCharacterBeneathCursor
 .byt <beDecrementBlock
 .byt <beIncrementBlock
 
 .byt <beGrabBlock
 .byt <beDropBlock
 .byt <beGrabCharacterUnderCursor
 .byt <beClearBlock

 .byt <beToggleUsedFlag
 .byt <beSelectNextFree
 .byt <beKeyHelp
beCommandVectorHi
 .byt >beMoveCursorLeftWithinBlock
 .byt >beMoveCursorRightWithinBlock
 .byt >beMoveCursorDownWithinBlock
 .byt >beMoveCursorUpWithinBlock
 
 .byt >ceDecrementCharacterID
 .byt >ceIncrementCharacterID
 
 .byt >beJumpToCharacterEditor
 .byt >beJumpToMapEditor
 .byt >beJumpToFiles
 
 .byt >bePlotCharacter
 .byt >beDeleteCharacterBeneathCursor
 .byt >beDecrementBlock
 .byt >beIncrementBlock
 
 .byt >beGrabBlock
 .byt >beDropBlock
 .byt >beGrabCharacterUnderCursor
 .byt >beClearBlock
 
 .byt >beToggleUsedFlag
 .byt >beSelectNextFree
 .byt >beKeyHelp

beMoveCursorLeftWithinBlock
	lda BlockCursorX
.(
	beq skip1
	jsr StopBlockCursor
	dec BlockCursorX
	jsr StartBlockCursor
skip1	rts
.)

beMoveCursorRightWithinBlock
	lda BlockCursorX
.(
	clc
	adc #1
	cmp BlockWidth
	beq skip1
	jsr StopBlockCursor
	inc BlockCursorX
	jsr StartBlockCursor
skip1	rts
.)

beMoveCursorDownWithinBlock
	lda BlockCursorY
.(
	clc
	adc #1
	cmp BlockHeight
	beq skip1
	jsr StopBlockCursor
	inc BlockCursorY
	jsr StartBlockCursor
skip1	rts
.)

beMoveCursorUpWithinBlock
	lda BlockCursorY
.(
	beq skip1
	jsr StopBlockCursor
	dec BlockCursorY
	jsr StartBlockCursor
skip1	rts
.)

beJumpToCharacterEditor
	lda #INACTIVE
	sta CharAnimActive+2
	sta CharAnimActive+3
	lda #ACTIVE
	sta CharAnimActive
	sta CharAnimActive+1
	
	jsr PlotBlocks
	jmp ceInputDriver

beJumpToMapEditor
	;Turn off cursors
	lda #INACTIVE
	sta CharAnimActive
	sta CharAnimActive+1
	sta CharAnimActive+2
	
	;Clear HIRES
	jsr ROM_HIRES
	
	;Turn on Map Cursor
	lda #ACTIVE
	sta CharAnimActive+3
	
	;Plot Map screen
	jsr DisplayMapColours
	jsr PlotMap
	
	;Refresh Legend
	jsr DisplayLegend
	jsr UpdateLegend
	
	;Jump to Map Editor
	jmp meInputDriver

beJumpToFiles
	;Turn off Cursors
	lda #INACTIVE
	sta CharAnimActive
	sta CharAnimActive+1
	sta CharAnimActive+2
	sta CharAnimActive+3

	;Return to Text
	jsr ROM_TEXT
	
	;Refresh Files screen
	jsr CompileFilesScreen
	
	;Jump to Files editor
	jmp FilesEditor


bePlotCharacter
	jsr CalculateCursorLocationInBlock
	lda CharacterID
	ldy #00
	sta (TempL),y
	jmp PlotBlocks
	
beDeleteCharacterBeneathCursor
	jsr CalculateCursorLocationInBlock
	lda #00
	tay
	sta (TempL),y
	jmp PlotBlocks

beDecrementBlock
	dec BlockID
.(
	bpl skip1
	lda BlockQuantity
	sta BlockID
	dec BlockID
skip1	jsr PlotBlockIndexes
	jmp PlotBlocks
.)
	
beIncrementBlock
	inc BlockID
	lda BlockID
	cmp BlockQuantity
.(
	bcc skip1
	lda #00
	sta BlockID
skip1	jsr PlotBlockIndexes
	jmp PlotBlocks
.)

beGrabBlock
	lda BlockID
	sta Grabbed
	jmp UpdateLegend
beDropBlock
	rts
	
RecalcCharCursorXY
	lda CharacterID
	and #31
	sta CharCursorX
	lda CharacterID
	lsr
	lsr
	lsr
	lsr
	lsr
	sta CharCursorY
	rts


beGrabCharacterUnderCursor
	jsr StopCharCursor
	jsr CalculateCursorLocationInBlock
	ldy #00
	lda (TempL),y
	sta CharacterID
	jsr RecalcCharCursorXY
	jsr PlotBlocks
	jsr StartCharCursor
	
	jmp PlotCharacterInGrid

beClearBlock
	lda BlockID
	jsr CalculateBlockAddress
	ldy BlockSize
	dey
	lda #00
.(
loop1	sta (TempL),y
	dey
	bpl loop1
.)
	jmp PlotBlocks

beToggleUsedFlag
	ldx BlockID
	lda CollisionCode,x
	eor #64
	sta CollisionCode,x
	jmp UpdateLegend

beSelectNextFree
	ldx #00
.(
loop1	lda CollisionCode,x
	and #64
	beq skip1
	inx
	bne loop1
	rts
skip1	txa
.)
	pha
	jsr StopBlockCursor
	pla
	sta BlockID
	jsr PlotBlocks
	
	jsr StartBlockCursor
	jmp UpdateLegend

beKeyHelp
	lda #00
	sta CharAnimActive
	sta CharAnimActive+1
	sta CharAnimActive+2
	sta CharAnimActive+3
	
	jsr ROM_TEXT
	jsr khDisplayKeyLegend1
	lda #00
	sta KeyListOffset
	ldx #01
	jsr khDisplayScreen
	jsr khInputDriver

	;Return to Block Editor
	jsr ROM_HIRES
	jsr DisplayLegend
	jsr UpdateLegend
	lda #ACTIVE
	sta CharAnimActive+2	;Turn on Block Cursor
	sta CharAnimActive+1	;Turn on Char Cursor
	lda #INACTIVE
	sta CharAnimActive	;Turn off Grid Cursor
	sta CharAnimActive+3	;Turn off Map Cursor
	jsr PlotCharacters
	jsr PlotCharacterInGrid
	jsr PlotCharacterEditorColours
	jsr PlotBlockEditorColours
	jsr PlotBlockIndexes
	jmp PlotBlocks
	
CalculateCursorLocationInBlock
	lda BlockID
	jsr CalculateBlockAddress
	
	;Now calculate offset within block
	lda BlockWidth
	ldx BlockCursorY
	jsr Mult8Bit
	adc BlockCursorX

	adc TempL
	sta TempL
	lda TempH
	adc #00
	sta TempH
	rts

StopBlockCursor
	;1 Stop Block cursor (running under interrupt)
	;2 Restore character where cursor was
	
	lda #00
	sta CharAnimActive+2
	
	lda #<$A010+40*60
	sta ScreenLocationLo
	lda #>$A010+40*60
	sta ScreenLocationHi
	jsr CalculateCursorLocationInBlock
	ldy #00
	lda (TempL),y
	ldx BlockCursorY
	ldy BlockCursorX
	jmp PlotCharInBlock
	
	
StartBlockCursor
	;1 Recalculate new Block Cursor Screen Location
	;2 Start Block Cursor (running under interrupt)
	
	lda BlockCursorY
	clc
	adc #10
	tay
	lda BlockCursorX
	clc
	adc #16
	adc CharacterYLOCL,y
	sta CharAnimScreenLocL+2
	lda CharacterYLOCH,y
	adc #00
	sta CharAnimScreenLocH+2
	
	lda #01
	sta CharAnimActive+2
	rts
