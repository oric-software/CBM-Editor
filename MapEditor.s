;Map Editor
DisplayMapColours
	ldy #32
	ldx #00
	lda #CYANYELLOWCHAR
.(
loop1	jsr PlotEditorChar
	dey
	bpl loop1
.)
	rts

CalcMapAddress
	ldx MapSizeLo
	ldy MapSizeHi
	lda MapID
.(
	bne skip1
	tax
	tay
	jmp skip2

skip1	jsr Mult16Bit
skip2	txa
.)
	clc
	adc #<MapMemory
	sta map
	tya
	adc #>MapMemory
	sta map+1
	
	;Now add MapBaseX and MapBaseY
	lda MapBaseY
.(
	beq skip2
	ldx MapWidth
	ldy #00
	jsr Mult16Bit
	txa
	adc map
	sta map
	lda map+1
	adc #00
	sta map+1
skip2	lda MapBaseX
.)
	adc map
	sta map
	lda map+1
	adc #00
	sta map+1
	rts

PlotMap
	jsr CalcMapAddress

	;Set up main loop counters
	lda #00
	sta BlockY


.(	
loop2	lda #00
	sta BlockX
	
loop1	;Calculate screen location
	
	;BlockY x BlockHeight
	lda BlockY
	ldx BlockHeight
	cmp #00
	beq skip1
	jsr Mult8Bit
skip1	tay
	
	lda BlockX
	ldx BlockWidth
	cmp #00
	beq skip2
	jsr Mult8Bit
	
skip2	sec
	adc Screen6x6YLOCL,y
	sta ScreenLocationLo
	lda Screen6x6YLOCH,y
	adc #00
	sta ScreenLocationHi
	
	ldy BlockX
	
	lda (map),y
	
	cmp BlockQuantity
	bcc skip3
	
	lda #00
	jsr CalculateBlockAddress
	jsr PlotBlock
	ldy BlockX
	lda (map),y
	jsr PlotBlockNumber
	jmp skip4
	
skip3	jsr CalculateBlockAddress
	
	jsr PlotBlock
	
skip4	inc BlockX
	lda BlockX
	cmp BlocksWide
	bcc loop1
	
	lda map
	clc
	adc MapWidth
	sta map
	lda map+1
	adc #00
	sta map+1
	
	inc BlockY
	lda BlockY
	cmp BlocksHigh
	bcc loop2
.)	
	rts

PlotBlock00AndNumber
	pha
	lda #00
	jsr CalculateBlockAddress
	jsr PlotBlock
	pla
	jmp PlotBlockNumber


PlotBlockNumber
	;Convert block number to displayable data
	pha
	
	;Calculate screen location of top left character in block
	lda ScreenLocationLo
	sta screen
	lda ScreenLocationHi
	sta screen+1
	
	pla
	pha
	lsr
	lsr
	lsr
	lsr
	jsr Hires1DH2
	pla
	and #15
Hires1DH2
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
	ldy #00
	sta (screen),y	;$B0E4,y

	lda $9981,x
	ora #64
	ldy #40
	sta (screen),y	;$B0E4+40*1,y

	lda $9982,x
	ora #64
	ldy #40*2
	sta (screen),y	;$B0E4+40*2,y
	
	lda $9983,x
	ora #64
	ldy #40*3
	sta (screen),y	;$B0E4+40*3,y

	lda $9984,x
	ora #64
	ldy #40*4
	sta (screen),y	;$B0E4+40*4,y

	lda $9985,x
	ora #64
	ldy #40*5
	sta (screen),y	;$B0E4+40*5,y

	lda $9986,x
	ora #64
	ldy #40*6
	sta (screen),y	;$B0E4+40*6,y
.(
	inc screen
	bne skip1
	inc screen+1
skip1	rts
.)	

	

meInputDriver
	lda #ED_MAP
	sta CurrentCBMEditor
	jsr UpdateLegend
	
.(
loop1	jsr ROM_GTORKB
	bpl loop1
.)
	sta PressedBlackKey
	lda $0209
	sta PressedRedKey
	
	ldy #16
.(
loop1	lda PressedBlackKey
	cmp meBlackKey,y
	bne skip1
	lda PressedRedKey
	cmp meRedKey,y
	beq skip2
skip1	dey
	bpl loop1
	jmp meInputDriver
skip2
.)	
	lda meCommandVectorLo,y
.(
	sta vector1+1
	lda meCommandVectorHi,y
	sta vector1+2
vector1	jsr $dead
.)
	jmp meInputDriver


meCommandVectorLo
 .byt <meMoveCursorleft
 .byt <meMoveCursorRight
 .byt <meMoveCursorDown
 .byt <meMoveCursorUp
 
 .byt <meJumpToCharacterEditor
 .byt <meJumpToBlockEditor
 .byt <meJumpToFiles

 .byt <meDecrementBlock
 .byt <meIncrementBlock
 .byt <mePlotBlock
 .byt <meGrabBlockBeneathCursor
 .byt <meDelete
 .byt <meClearMap
 
 .byt <mePreviousMap
 .byt <meNextMap
 .byt <meKeyHelp
 .byt <meDumpMap
 
meCommandVectorHi
 .byt >meMoveCursorleft
 .byt >meMoveCursorRight
 .byt >meMoveCursorDown
 .byt >meMoveCursorUp
 
 .byt >meJumpToCharacterEditor
 .byt >meJumpToBlockEditor
 .byt >meJumpToFiles

 .byt >meDecrementBlock
 .byt >meIncrementBlock
 .byt >mePlotBlock
 .byt >meGrabBlockBeneathCursor
 .byt >meDelete
 .byt >meClearMap
 
 .byt >mePreviousMap
 .byt >meNextMap
 .byt >meKeyHelp
 .byt >meDumpMap

meMoveCursorleft
	;The control is potentially a push scroll
	lda MapScreenX
	beq AttemptPushScrollLeft
	jsr StopMapCursor
	dec MapScreenX
	jmp StartMapCursor
AttemptPushScrollLeft
	lda MapBaseX
	beq CantMoveLeft
	jsr StopMapCursor
	dec MapBaseX
	jsr PlotMap
	jmp StartMapCursor
CantMoveLeft
	rts

meMoveCursorRight
	lda MapScreenX
	clc
	adc #1
	cmp BlocksWide
	bcs AttemptPushScrollRight
	jsr StopMapCursor
	inc MapScreenX
	jmp StartMapCursor
AttemptPushScrollRight
	clc
	adc MapBaseX
	cmp MapWidth
	bcs CantMoveRight
	jsr StopMapCursor
	inc MapBaseX
	jsr PlotMap
	jmp StartMapCursor
CantMoveRight
	rts

meMoveCursorDown
	lda MapScreenY
	clc
	adc #1
	cmp BlocksHigh
	bcs AttemptPushScrollDown
	jsr StopMapCursor
	inc MapScreenY
	jmp StartMapCursor
AttemptPushScrollDown
	clc
	adc MapBaseY
	cmp MapHeight
	bcs CantMoveDown
	jsr StopMapCursor
	inc MapBaseY
	jsr PlotMap
	jmp StartMapCursor
CantMoveDown
	rts

meMoveCursorUp
	lda MapScreenY
	beq AttemptPushScrollUp
	jsr StopMapCursor
	dec MapScreenY
	jmp StartMapCursor
AttemptPushScrollUp
	lda MapBaseY
	beq CantMoveUp
	jsr StopMapCursor
	dec MapBaseY
	jsr PlotMap
	jmp StartMapCursor
CantMoveUp
	rts

meJumpToCharacterEditor
	;Turn off Map Cursor
	lda #INACTIVE
	sta CharAnimActive+2
	sta CharAnimActive+3
	
	;Clear HIRES
	jsr ROM_HIRES
	
	;Turn on Character and block cursors
	lda #ACTIVE
	sta CharAnimActive
	sta CharAnimActive+1
	
	;Display Character and block editors
	jsr PlotCharacters
	jsr PlotCharacterInGrid
	jsr PlotCharacterEditorColours
	jsr PlotBlockEditorColours
	jsr PlotBlockIndexes
	jsr PlotBlocks

	;Refresh Legend
	jsr DisplayLegend
	jsr UpdateLegend
	
	;Jump to Character Editor
	jmp ceInputDriver
	
meJumpToBlockEditor
	;Turn off Map Cursor
	lda #INACTIVE
	sta CharAnimActive
	sta CharAnimActive+3
	
	;Clear HIRES
	jsr ROM_HIRES
	
	;Turn on Character and block cursors
	lda #ACTIVE
	sta CharAnimActive+1
	sta CharAnimActive+2
	
	;Display Character and block editors
	jsr PlotCharacters
	jsr PlotCharacterInGrid
	jsr PlotCharacterEditorColours
	jsr PlotBlockEditorColours
	jsr PlotBlockIndexes
	jsr PlotBlocks

	;Refresh Legend
	jsr DisplayLegend
	jsr UpdateLegend
	
	;Jump to Character Editor
	jmp beInputDriver

meJumpToFiles
	;Turn off Map Cursor
	lda #INACTIVE
	sta CharAnimActive
	sta CharAnimActive+1
	sta CharAnimActive+2
	sta CharAnimActive+3

	;Switch to text
	jsr ROM_TEXT
	
	;Display files screen
	jsr CompileFilesScreen

	;Jump to Files
	jmp FilesEditor


meDecrementBlock
	dec BlockID
;As part of adding placeholders for blocks outside range we need to
;allow any of the 255 blocks to be selected and plotted.
;.(
;	bpl skip1
;	lda BlockQuantity
;	sta BlockID
;	dec BlockID
;skip1
	jsr UpdateBlock
	jmp UpdateLegend
;.)

UpdateBlock
	jsr CalcScreenLocInMapScreen
	lda BlockID
	cmp BlockQuantity
.(
	bcc skip1
	lda #00
	jsr CalculateBlockAddress
	lda BlockID
	jmp PlotBlock00AndNumber
skip1	jsr CalculateBlockAddress
.)
	jmp PlotBlock

	
meIncrementBlock
	inc BlockID
;As part of adding placeholders for blocks outside range we need to
;allow any of the 255 blocks to be selected and plotted.
;	lda BlockID
;	cmp BlockQuantity
;.(
;	bcc skip1
;	lda #00
;	sta BlockID
;skip1
	jsr UpdateBlock
	jmp UpdateLegend
;.)

mePlotBlock
	;Store BlockID in Map
	jsr CalculateBlockLocationInMap
	lda BlockID
	ldy #00
	sta (map),y
	
	jmp ShowNewBlockInMap
	
ShowNewBlockInMap
	;Update screen with stored Block
	jsr CalcScreenLocInMapScreen
	lda BlockID
	cmp BlockQuantity
.(
	bcc skip1
	lda #00
	jsr CalculateBlockAddress
	lda BlockID
	jmp PlotBlock00AndNumber
skip1	jsr CalculateBlockAddress
.)
	jmp PlotBlock

meGrabBlockBeneathCursor
	jsr CalculateBlockLocationInMap
	ldy #00
	lda (map),y
	sta BlockID
	rts
meDelete
	jsr CalculateBlockLocationInMap
	lda #00
	tay
	sta (map),y
	jmp ShowNewBlockInMap

meClearMap	;FUNC C so don't bother with are you sure
	jsr CalculateMapIDLocation
	lda MapSizeLo
	sta TempL
	lda MapSizeHi
	sta TempH
	ldy #00
	lda #00
.(
loop2	sta (map),y
	inc map
	bne skip3
	inc map+1
skip3
	lda TempL
	sec
	sbc #1
	sta TempL
	lda TempH
	sbc #00
	sta TempH
	bcs loop2

skip1	jmp PlotMap
.)
	

mePreviousMap
	lda MapQuantity
	cmp #2
.(
	bcc skip1
	lda MapID
	beq skip1
	dec MapID
	jmp PlotMap
skip1	rts
.)

meNextMap
	lda MapQuantity
	cmp #2
.(
	bcc skip1
	lda MapID
	clc
	adc #01
	cmp MapQuantity
	bcs skip1
	inc MapID
	jmp PlotMap
skip1	rts
.)

meKeyHelp
	lda #00
	sta CharAnimActive
	sta CharAnimActive+1
	sta CharAnimActive+2
	sta CharAnimActive+3
	
	jsr ROM_TEXT
	jsr khDisplayKeyLegend1
	lda #00
	sta KeyListOffset
	ldx #02
	jsr khDisplayScreen
	jsr khInputDriver

	;Return to Map Editor
	jsr ROM_HIRES
	jsr DisplayLegend
	jsr UpdateLegend
	lda #ACTIVE
	sta CharAnimActive+3
	
	jsr DisplayMapColours
	jsr PlotMap
	jmp meInputDriver


CalculateMapIDLocation
	ldx MapSizeLo
	ldy MapSizeHi
	lda MapID
.(
	bne skip1
	ldx #00
	ldy #00
	jmp skip2
	
skip1	jsr Mult16Bit
skip2	txa
.)
	clc
	adc #<MapMemory
	sta map
	tya
	adc #>MapMemory
	sta map+1
	rts


CalculateBlockLocationInMap
	jsr CalculateMapIDLocation

	lda MapBaseY
	clc
	adc MapScreenY
	tax
	lda MapWidth
	ldy #00
	jsr Mult16Bit
	txa
	clc
	adc map
	sta map
	tya
	adc map+1
	sta map+1
	
	lda MapBaseX
	adc MapScreenX
	adc map
	sta map
	lda map+1
	adc #00
	sta map+1
	rts

CalcScreenLocInMapScreen
	;BlockY x BlockHeight
	lda MapScreenX
	ldx BlockWidth
	jsr Mult8Bit
	pha
	
	lda MapScreenY
	ldx BlockHeight
	jsr Mult8Bit
	tax
	
	pla
	sec
	adc Screen6x6YLOCL,x
	sta ScreenLocationLo
	lda Screen6x6YLOCH,x
	adc #00
	sta ScreenLocationHi
	rts

MapWarnScreen
 .byt 1,"Ensure Printer is enabled before       "
 .byt 1,"proceeding!                            "
 .byt 5,"Press 'Y' to proceed                   "
 


meDumpMap
	;Warn about printer must be on and confirm...
	ldx #119
.(
loop1	lda MapWarnScreen,x
	sta $BFE0-120,x
	dex
	bpl loop1
	jsr WaitOnKey
	cmp #"Y"
	bne skip1
	
	;Dump current map to Printer
	lda #DT_IMAPDATA
	sta d2pDumpType
	jsr CalcMapAddress
	lda map
	sta d2pStartLo
	lda map+1
	sta d2pStartHi
	lda MapSizeLo
	sta d2pLengthLo
	lda MapSizeHi
	sta d2pLengthHi
	lda MapWidth
	sta d2pRowWidth
	lda #00
	sta d2pLabelHeight
	lda #<MapDataText
	sta d2pLabelLo
	lda #>MapDataText
	sta d2pLabelHi
	lda MapID
	jsr DumpArea
	jsr DisplayLegend
	jmp UpdateLegend
skip1	rts
.)

