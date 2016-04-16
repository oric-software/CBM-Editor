;CBM Editor   Character - Block - Map - Editor

;0123456789012345678901234567890123456789
; LLLLLL 01234567890123456789012345678901
; LLLLLL 01234567890123456789012345678901
; LLLLLL 01234567890123456789012345678901
; LLLLLL 01234567890123456789012345678901
; LLLLLL 01234567890123456789012345678901
; LLLLLL 01234567890123456789012345678901
;        01234567890123456789012345678901
;        01234567890123456789012345678901

PlotEditorChar
	sta TempA
	asl
	sta TempL
	asl
	adc TempL
	clc
	adc #<EditorCharacterMemory
.(
	sta vector2+1
	lda #>EditorCharacterMemory
	adc #00
	sta vector2+2
	
	txa
	clc
	adc CharacterYLOCL,y
	sta screen
	lda CharacterYLOCH,y
	sta screen+1
	
	stx TempX
	sty TempY
	
	ldx #5

loop1	ldy Offset1x6,x
vector2	lda $dead,x
	sta (screen),y
	dex
	bpl loop1
.)
	lda TempA
	ldx TempX
	ldy TempY
	
	rts
	

PlotCharacters
	lda #00
	tay
.(
loop2	ldx #00
	
loop1	jsr FetchCharacterAddress
	jsr DisplayCharacter
	
	clc
	adc #1
	
	inx
	cpx #32
	bcc loop1
	
	ldx #00
	iny
	cpy #8
	bcc loop2
.)	
	rts

FetchCharacterAddress
	stx TempX
	ldx #00
	stx TempC
	sta TempA
	asl
	rol TempC
	
	sta TempL
	lda TempC
	sta TempH
	lda TempL
	
	asl
	rol TempC
	
	adc TempL
	sta TempL
	lda TempC
	adc TempH
	sta TempH
	
	lda TempL
	adc #<CharacterMemory
	sta TempL
	lda TempH
	adc #>CharacterMemory
	sta TempH
	
	ldx TempX
	lda TempA

	rts

DisplayCharacter
	sta TempA
	sty TempY
	stx TempX
	
	txa
	
	;Offset X by 8
	clc
	adc #08
	
	;Add Y row address
	adc CharacterYLOCL,y
	sta screen
	lda CharacterYLOCH,y
	adc #00
	sta screen+1
	
	;Fetch Character Address
	lda TempL
.(
	sta vector1+1
	lda TempH
	sta vector1+2
	
	ldx #5

loop1	ldy Offset1x6,x
vector1	lda $dead,x
	sta (screen),y
	dex
	bpl loop1
.)
	lda TempA
	ldy TempY
	ldx TempX
	
	rts
	
PlotCharacterInGrid
	lda CharacterID
	jsr FetchCharacterAddress
	
	lda TempL
.(
	sta vector1+1
	lda TempH
	sta vector1+2
	
	ldy #00
loop3	ldx #00
	
loop2	txa
	clc
	adc #1
	adc CharacterYLOCL,y
	sta screen
	lda CharacterYLOCH,y
	adc #00
	sta screen+1
	
	stx TempX
	sty TempY
	
vector1	lda $dead,y
	pha
	and #128
	sta GridInverseFlag
	pla
	and Bitpos,x
	beq skip1
	lda #6
skip1	clc
	adc #<EditorCharacterMemory
	sta vector2+1
	lda #>EditorCharacterMemory
	adc #00
	sta vector2+2
	
	ldx #5

loop1	ldy Offset1x6,x
vector2	lda $dead,x
	ora GridInverseFlag
	sta (screen),y
	dex
	bpl loop1

	ldy TempY
	ldx TempX
	
	inx
	cpx #6
	bcc loop2
	
	iny
	cpy #6
	bcc loop3
.)
	rts
	
PlotCharacterEditorColours
	;Plot colour mix down column 7
	ldx #7
	ldy #0
	lda #CYANYELLOWCHAR
.(
loop1	jsr PlotEditorChar
	iny
	cpy #8
	bcc loop1
.)
	;Plot cyan/yellow down column 0 for character grid
	ldx #0
	ldy #0
.(
loop1	lda #CYANCHAR
	jsr PlotEditorChar
	iny
	lda #YELLOWCHAR
	jsr PlotEditorChar
	iny
	cpy #6
	bcc loop1
.)
	rts

Bitpos
 .byt 32,16,8,4,2,1
Offset1x6
 .byt 0,40,80,120,160,200
Screen6x6YLOCL
CharacterYLOCL
 .byt <$A000
 .byt <$A000+240*1
 .byt <$A000+240*2
 .byt <$A000+240*3
 .byt <$A000+240*4
 .byt <$A000+240*5
 .byt <$A000+240*6
 .byt <$A000+240*7
 
 .byt <$A000+240*8
 .byt <$A000+240*9
 
 .byt <$A000+240*10
 .byt <$A000+240*11
 .byt <$A000+240*12
 .byt <$A000+240*13
 .byt <$A000+240*14
 .byt <$A000+240*15
 .byt <$A000+240*16
 .byt <$A000+240*17
 .byt <$A000+240*18
 .byt <$A000+240*19
;For Screen6x6YLOCL continue thru to 32
 .byt <$A000+240*20
 .byt <$A000+240*21
 .byt <$A000+240*22
 .byt <$A000+240*23
 .byt <$A000+240*24
 .byt <$A000+240*25
 .byt <$A000+240*26
 .byt <$A000+240*27
 .byt <$A000+240*28
 .byt <$A000+240*29
 .byt <$A000+240*30
 .byt <$A000+240*31
 .byt <$A000+240*32
Screen6x6YLOCH
CharacterYLOCH
 .byt >$A000
 .byt >$A000+240*1
 .byt >$A000+240*2
 .byt >$A000+240*3
 .byt >$A000+240*4
 .byt >$A000+240*5
 .byt >$A000+240*6
 .byt >$A000+240*7

 .byt >$A000+240*8
 .byt >$A000+240*9
 
 .byt >$A000+240*10
 .byt >$A000+240*11
 .byt >$A000+240*12
 .byt >$A000+240*13
 .byt >$A000+240*14
 .byt >$A000+240*15
 .byt >$A000+240*16
 .byt >$A000+240*17
 .byt >$A000+240*18
 .byt >$A000+240*19

 .byt >$A000+240*20
 .byt >$A000+240*21
 .byt >$A000+240*22
 .byt >$A000+240*23
 .byt >$A000+240*24
 .byt >$A000+240*25
 .byt >$A000+240*26
 .byt >$A000+240*27
 .byt >$A000+240*28
 .byt >$A000+240*29
 .byt >$A000+240*30
 .byt >$A000+240*31
 .byt >$A000+240*32

EditorCharacterMemory
 .byt %01111111
 .byt %01100000
 .byt %01100000
 .byt %01100000
 .byt %01100000
 .byt %01100000

 .byt %01111111
 .byt %01100000
 .byt %01101110
 .byt %01101110
 .byt %01101110
 .byt %01100000
;Cyan (2)
 .byt 6,6,6,6,6,6
;Yellow (3)
 .byt 3,3,3,3,3,3
;Mixed.. (4)
 .byt 6,3,6,3,6,3
;Character Char Animation frames
CharAnimFrame00	;5
 .byt %00101010
 .byt %00000001
 .byt %00100000
 .byt %00000001
 .byt %00100000
 .byt %00010101
 
 .byt %00010101	;6
 .byt %00100000
 .byt %00000001
 .byt %00100000
 .byt %00000001
 .byt %00101010
 
;Editor character 7 (Ctrl)
 .byt %00000000
 .byt %00000000
 .byt %00000000
 .byt %00000000
 .byt %00000000
 .byt %00000000

;Editor character 
ceInputDriver
	lda #ED_CHARACTER
	sta CurrentCBMEditor
	jsr UpdateLegend
.(
loop1	jsr ROM_GTORKB
	bpl loop1
.)
	sta PressedBlackKey
	lda $0209
	sta PressedRedKey
	
	ldy #31
.(
loop1	lda PressedBlackKey
	cmp ceBlackKey,y
	bne skip1
	lda PressedRedKey
	cmp ceRedKey,y
	beq skip2
skip1	dey
	bpl loop1
	jmp ceInputDriver
skip2
.)	
	lda ceCommandVectorLo,y
.(
	sta vector1+1
	lda ceCommandVectorHi,y
	sta vector1+2
vector1	jsr $dead
.)
	jmp ceInputDriver
	


ceGridCrsrLeft
	lda GridCursorX
.(
	beq skip1
	jsr StopGridCursor
	dec GridCursorX
	jsr StartGridCursor
skip1	rts
.)

ceGridCrsrRight
	lda GridCursorX
	cmp #5
.(
	bcs skip1
	jsr StopGridCursor
	inc GridCursorX
	jsr StartGridCursor
skip1	rts
.)
ceGridCrsrUp
	lda GridCursorY
.(
	beq skip1
	jsr StopGridCursor
	dec GridCursorY
	jsr StartGridCursor
skip1	rts
.)
ceGridCrsrDown
	lda GridCursorY
	cmp #5
.(
	bcs skip1
	jsr StopGridCursor
	inc GridCursorY
	jsr StartGridCursor
skip1	rts
.)

ceDecrementCharacterID
.(
	lda CharCursorX
	beq skip1
	jsr StopCharCursor
	dec CharCursorX
rent1	jsr StartCharCursor
	jsr RecalcCharacterID
	jsr PlotCharacterInGrid
	jmp UpdateLegend
skip1	lda CharCursorY
	beq skip2
	jsr StopCharCursor
	dec CharCursorY
	lda #31
	sta CharCursorX
	jmp rent1
skip2	rts
.)

ceIncrementCharacterID
.(
	lda CharCursorX
	cmp #31
	beq skip1
	jsr StopCharCursor
	inc CharCursorX
rent1	jsr StartCharCursor
	jsr RecalcCharacterID
	jsr PlotCharacterInGrid
	jmp UpdateLegend
skip1	lda CharCursorY
	cmp #7
	beq skip2
	jsr StopCharCursor
	inc CharCursorY
	lda #00
	sta CharCursorX
	jmp rent1
skip2	rts
.)


RecalcCharacterID
	lda CharCursorY
	asl
	asl
	asl
	asl
	asl
	ora CharCursorX
	sta CharacterID
	rts


StopGridCursor
	;1 Stop Grid cursor (running under interrupt)
	;2 Restore editor character where cursor was
	
	lda #00
	sta CharAnimActive
	
	jmp PlotCharacterInGrid
	
	
StartGridCursor
	;1 Recalculate new Grid Cursor Screen Location
	;2 Start Grid Cursor (running under interrupt)
	
	ldx GridCursorX
	inx
	ldy GridCursorY
	txa
	clc
	adc CharacterYLOCL,y
	sta CharAnimScreenLocL
	lda CharacterYLOCH,y
	adc #00
	sta CharAnimScreenLocH
	
	lda #01
	sta CharAnimActive
	rts

StopCharCursor
	;1 Stop Char cursor (running under interrupt)
	;2 Restore character where cursor was
	
	lda #00
	sta CharAnimActive+1
	
RestoreCharCharacter
	;Also used when modifying character def
	
	;Get screen address
	lda CharAnimScreenLocL+1
.(
	sta vector2+1
	lda CharAnimScreenLocH+1
	sta vector2+2
	
	;Calc Char Address
	lda CharacterID
	jsr FetchCharacterAddress
	lda TempL
	sta vector1+1
	lda TempH
	sta vector1+2
	
	ldx #5

loop1	ldy Offset1x6,x
vector1	lda $dead,x
vector2	sta $dead,y

	dex
	bpl loop1
.)
	rts
	

StartCharCursor
	;1 Recalculate new Char Cursor Screen Location
	;2 Start Char Cursor (running under interrupt)

	jsr RecalcCharCursorLoc
	
	lda #01
	sta CharAnimActive+1
	rts

RecalcCharCursorLoc
	lda CharCursorX
	clc
	adc #8
	ldy CharCursorY
	adc CharacterYLOCL,y
	sta CharAnimScreenLocL+1
	lda CharacterYLOCH,y
	adc #00
	sta CharAnimScreenLocH+1
	rts

ceCopyAsLast
	lda CharacterID
	jsr FetchCharacterAddress
	lda TempL
.(
	sta vector1+1
	lda TempH
	sta vector1+2
	lda CharacterID
	sec
	sbc #01
	jsr FetchCharacterAddress
	
	ldy #05
loop1	lda (TempL),y
vector1	sta $dead,y
	dey
	bpl loop1
.)
	jmp ceUpdateAfterCharacterMod

ceCopyAsNext
	lda CharacterID
	jsr FetchCharacterAddress
	lda TempL
.(
	sta vector1+1
	lda TempH
	sta vector1+2
	lda CharacterID
	clc
	adc #1
	jsr FetchCharacterAddress
	
	ldy #05
loop1	lda (TempL),y
vector1	sta $dead,y
	dey
	bpl loop1
.)
	jmp ceUpdateAfterCharacterMod

ceCharGrab
	lda CharacterID
	sta Grabbed
	jmp UpdateLegend

ceCharDrop
	lda Grabbed
	jsr FetchCharacterAddress
	lda TempL
.(
	sta vector1+1
	lda TempH
	sta vector1+2
	lda CharacterID
	jsr FetchCharacterAddress
	ldy #5
loop1
vector1	lda $dead,y
	sta (TempL),y
	dey
	bpl loop1
.)
	jmp ceUpdateAfterCharacterMod

	

ceCharToggleBit
	;Invert character bit
	lda CharacterID
	jsr FetchCharacterAddress
	ldy GridCursorY
	lda (TempL),y
	ldx GridCursorX
	eor Bitpos,x
	sta (TempL),y
ceUpdateAfterCharacterMod	
	;Refresh Grid
	jsr PlotCharacterInGrid

	;Refresh screen
	lda CharacterID
	jmp RefreshScreenCharacter

ceCharScrollWest
	lda CharacterID
	jsr FetchCharacterAddress
	ldy #05
.(
loop1	lda (TempL),y
	and #%11000000
	sta TempA
	lda (TempL),y
	and #%00111111
	asl
	cmp #%01000000
	bcc skip1
	ora #%00000001
skip1	and #%00111111
	ora TempA
	sta (TempL),y
	dey
	bpl loop1
.)
	jmp ceUpdateAfterCharacterMod
	
ceCharScrollEast
	lda CharacterID
	jsr FetchCharacterAddress
	ldy #05
.(
loop1	lda (TempL),y
	and #%11000000
	sta TempA
	lda (TempL),y
	and #%00111111
	lsr
	bcc skip1
	ora #%00100000
skip1	ora TempA
	sta (TempL),y
	dey
	bpl loop1
.)
	jmp ceUpdateAfterCharacterMod

ceCharHLine
	lda CharacterID
	jsr FetchCharacterAddress
	ldy GridCursorY
	lda #%01111111
	sta (TempL),y
	jmp ceUpdateAfterCharacterMod

ceCharVLine
	lda CharacterID
	jsr FetchCharacterAddress
	ldy #05
.(
loop1	lda (TempL),y
	ldx GridCursorX
	ora Bitpos,x
	sta (TempL),y
	dey
	bpl loop1
.)
	jmp ceUpdateAfterCharacterMod

ceCharScrollNorth
	lda CharacterID
	jsr FetchCharacterAddress
	ldy #00
	lda (TempL),y
	pha
	iny
.(	
loop1	lda (TempL),y
	dey
	sta (TempL),y
	iny
	iny
	cpy #6
	bcc loop1
.)
	pla
	dey
	sta (TempL),y
	jmp ceUpdateAfterCharacterMod

ceCharScrollSouth
	lda CharacterID
	jsr FetchCharacterAddress
	ldy #5
	lda (TempL),y
	pha
	dey
.(
loop1	lda (TempL),y
	iny
	sta (TempL),y
	dey
	dey
	bpl loop1
.)	
	iny
	pla
	sta (TempL),y
	jmp ceUpdateAfterCharacterMod
	
ceCharFlip
	;Vertical Mirror
	lda CharacterID
	jsr FetchCharacterAddress
	ldy #05
.(
loop1	lda (TempL),y
	pha
	dey
	bpl loop1
.)
	;Now unroll
	ldy #05
.(
loop1	pla
	sta (TempL),y
	dey
	bpl loop1
.)
	jmp ceUpdateAfterCharacterMod
	

ceCharMirror
	;Horizontal mirror
	lda CharacterID
	jsr FetchCharacterAddress
	ldy #05
.(	
loop1	lda (TempL),y
	and #%11000000
	sta TempC
	
	lda (TempL),y
	and #%00000111
	tax
	lda Lower2UpperMirror,x
	sta TempA
	
	lda (TempL),y
	and #%00111000
	lsr
	lsr
	lsr
	tax
	lda Lower2UpperMirror,x
	lsr
	lsr
	lsr
	ora TempA
	ora TempC
	sta (TempL),y
	dey
	bpl loop1
.)
	jmp ceUpdateAfterCharacterMod
	
Lower2UpperMirror
 .byt %000000	;%000 >> %000
 .byt %100000	;%001 >> %100
 .byt %010000	;%010 >> %010
 .byt %110000	;%011 >> %110
 .byt %001000	;%100 >> %001
 .byt %101000	;%101 >> %101
 .byt %011000	;%110 >> %011
 .byt %111000	;%111 >> %111

ceCharRotate
	lda CharacterID
	jsr FetchCharacterAddress
	lda TempL
.(
	sta vector1+1
	lda TempH
	sta vector1+2
	
	;Clear out buffer
	lda #00
	ldy #5
loop3	sta cecrBuffer,y
	dey
	bpl loop3
	
	;Scan each pixel Horizontally, store vertically
	;base on 6x6 matrix
	ldy #0
loop2	ldx #0
	
loop1	;extract bit
vector1	lda $dead,y
	and Bitpos,x
	
	;Convert bit to carry bit
	cmp Bitpos,x
	
	bcc skip1
	lda cecrBuffer,x
	ora Bitpos,y
	sta cecrBuffer,x

skip1	inx
	cpx #6
	bcc loop1
	iny
	cpy #6
	bcc loop2
.)	
	;Now transfer buffer to character
	ldy #5
.(
loop1	lda cecrBuffer,y
	;Ensure no bad bytes
	and #%10111111
	ora #%01000000
	sta (TempL),y
	dey
	bpl loop1
.)
	jmp ceUpdateAfterCharacterMod

cecrBuffer
 .dsb 6,0

	
	
	
	rts
ceCharWipe
	lda CharacterID
	jsr FetchCharacterAddress
	lda #64
	ldy #05
.(
loop1	sta (TempL),y
	dey
	bpl loop1
.)
	jmp ceUpdateAfterCharacterMod

ceCharInverseLine
	lda CharacterID
	jsr FetchCharacterAddress
	ldy GridCursorY
	lda (TempL),y
	eor #%10111111
	sta (TempL),y
	jmp ceUpdateAfterCharacterMod

ceDecrementCollisionID
	ldx CharacterID
	lda CollisionCode,x
	and #128+64
.(
	sta vector1+1
	lda CollisionCode,x
	sec
	sbc #1
	and #63
vector1	ora #00
.)
	sta CollisionCode,x
	jmp UpdateLegend

ceIncrementCollisionID
	ldx CharacterID
	lda CollisionCode,x
	and #128+64
.(
	sta vector1+1
	lda CollisionCode,x
	clc
	adc #1
	and #63
vector1	ora #00
.)
	sta CollisionCode,x
	jmp UpdateLegend

ceAlterTopColour
	dec TopColour
.(
	bpl skip1
	lda #7
	sta TopColour
skip1	jsr UpdateLegend
	lda TopColour
	ldy #5
loop1	sta EditorCharacterMemory+12,y
	dey
	bpl loop1
.)
	sta EditorCharacterMemory+24
	sta EditorCharacterMemory+26
	sta EditorCharacterMemory+28
	jsr PlotCharacterEditorColours
	jmp PlotBlockEditorColours

ceAlterBotColour
	dec BotColour
.(
	bpl skip1
	lda #7
	sta BotColour
skip1	jsr UpdateLegend
	lda BotColour
	ldy #5
loop1	sta EditorCharacterMemory+18,y
	dey
	bpl loop1
.)
	sta EditorCharacterMemory+25
	sta EditorCharacterMemory+27
	sta EditorCharacterMemory+29
	jsr PlotCharacterEditorColours
	jmp PlotBlockEditorColours

JumpBlockEditor
	jsr StopGridCursor
	lda #ACTIVE
	sta CharAnimActive+1	;Turn on Char Cursor
	sta CharAnimActive+2	;Turn on Block Cursor
	lda #INACTIVE
	sta CharAnimActive+3	;Turn off Map Cursor

	;Jump Block Editor
	jmp beInputDriver

JumpMapEditor
	;Turn off Cursors
	lda #INACTIVE
	sta CharAnimActive
	sta CharAnimActive+1
	sta CharAnimActive+2
	
	;Clear HIRES
	jsr ROM_HIRES
	
	;Turn on Map Cursor
	lda #ACTIVE
	sta CharAnimActive+3
	
	;Refresh Map screen
	jsr DisplayMapColours
	jsr PlotMap
	jsr DisplayLegend
	jsr UpdateLegend

	;Jump Map Editor
	jmp meInputDriver
	
JumpFileEditor
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

ceToggleUsedFlag
	ldx CharacterID
	lda CollisionCode,x
	eor #128
	sta CollisionCode,x
	jmp UpdateLegend

ceSelectNextFree
	ldx #00
.(
loop1	lda CollisionCode,x
	bpl skip1
	inx
	bne loop1
	rts
skip1	txa
.)
	pha
	jsr StopCharCursor
	pla
	sta CharacterID
	jsr RecalcCharCursorXY
	
	jsr StartCharCursor
	jmp UpdateLegend

ceKeyHelp
	lda #00
	sta CharAnimActive
	sta CharAnimActive+1
	sta CharAnimActive+2
	sta CharAnimActive+3
	
	jsr ROM_TEXT
	jsr khDisplayKeyLegend1
	lda #00
	sta KeyListOffset
	ldx #00
	jsr khDisplayScreen
	jsr khInputDriver
	
	;Return to Character Editor
	jsr ROM_HIRES
	jsr DisplayLegend
	jsr UpdateLegend
	lda #ACTIVE
	sta CharAnimActive	;Turn on Grid Cursor
	sta CharAnimActive+1	;Turn on Char Cursor
	lda #INACTIVE
	sta CharAnimActive+2	;Turn off Block Cursor
	sta CharAnimActive+3	;Turn off Map Cursor
	jsr PlotCharacters
	jsr PlotCharacterInGrid
	jsr PlotCharacterEditorColours
	jsr PlotBlockEditorColours
	jsr PlotBlockIndexes
	jmp PlotBlocks

	

 
ceCommandVectorLo
 .byt <ceGridCrsrLeft
 .byt <ceGridCrsrRight
 .byt <ceGridCrsrUp
 .byt <ceGridCrsrDown

 .byt <ceDecrementCharacterID
 .byt <ceIncrementCharacterID

 .byt <JumpBlockEditor
 .byt <JumpMapEditor
 .byt <JumpFileEditor

 .byt <ceCopyAsLast
 .byt <ceCopyAsNext
 .byt <ceCharGrab
 .byt <ceCharDrop
 .byt <ceCharScrollWest
 .byt <ceCharScrollEast
 .byt <ceCharHLine
 .byt <ceCharVLine
 .byt <ceCharScrollNorth
 .byt <ceCharScrollSouth
 .byt <ceCharFlip
 .byt <ceCharMirror
 .byt <ceCharRotate
 .byt <ceCharWipe
 .byt <ceCharInverseLine
 .byt <ceDecrementCollisionID
 .byt <ceIncrementCollisionID
 .byt <ceCharToggleBit
 .byt <ceAlterTopColour
 .byt <ceAlterBotColour
 .byt <ceToggleUsedFlag
 .byt <ceSelectNextFree
 .byt <ceKeyHelp
ceCommandVectorHi
 .byt >ceGridCrsrLeft
 .byt >ceGridCrsrRight
 .byt >ceGridCrsrUp
 .byt >ceGridCrsrDown

 .byt >ceDecrementCharacterID
 .byt >ceIncrementCharacterID

 .byt >JumpBlockEditor
 .byt >JumpMapEditor
 .byt >JumpFileEditor

 .byt >ceCopyAsLast
 .byt >ceCopyAsNext
 .byt >ceCharGrab
 .byt >ceCharDrop
 .byt >ceCharScrollWest
 .byt >ceCharScrollEast
 .byt >ceCharHLine
 .byt >ceCharVLine
 .byt >ceCharScrollNorth
 .byt >ceCharScrollSouth
 .byt >ceCharFlip
 .byt >ceCharMirror
 .byt >ceCharRotate
 .byt >ceCharWipe
 .byt >ceCharInverseLine
 .byt >ceDecrementCollisionID
 .byt >ceIncrementCollisionID
 .byt >ceCharToggleBit
 .byt >ceAlterTopColour
 .byt >ceAlterBotColour
 .byt >ceToggleUsedFlag
 .byt >ceSelectNextFree
 .byt >ceKeyHelp
 
