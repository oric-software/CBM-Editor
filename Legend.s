;Legend

;0123456789012345678901234567890123456789
; CharID;00 ColID;00 Colours;6,3  Grab;00
;BlockID;00(8x8x64)    MapID;00(08x08x64)
;[                                      ]

LegendText
 .byt " CharID:00U ColID:00 Colours:6,3 Grab:00"
 .byt "BlockID:00U (8x8x64)  MapID:00(08x08x64)"
 .byt "                           ",2,"CHAR EDITOR "

DisplayLegend
	ldx #119
.(
loop1	lda LegendText,x
	sta $BFE0-120,x
	dex
	bpl loop1
.)
	rts

UpdateLegend
	ldy #8
	lda CharacterID
	jsr Plot2DH
	
	ldx CharacterID
	lda #" "
	ldy CollisionCode,x
.(
	bpl skip1
	lda #"u"
skip1	sta $BFE0-110
.)
	ldy #18
	ldx CharacterID
	lda CollisionCode,x
	and #63
	jsr Plot2DH
	
	ldy #29
	lda TopColour
	jsr Plot1DH
	
	ldy #31
	lda BotColour
	jsr Plot1DH
	
	ldy #38
	lda Grabbed
	jsr Plot2DH
	
	ldy #8+40
	lda BlockID
	jsr Plot2DH
	
	ldx BlockID
	ldy #" "
	lda CollisionCode,x
.(
	and #64
	beq skip1
	ldy #"u"
skip1	sty $BFE0-70
.)
	ldy #40+13
	lda BlockWidth
	jsr Plot1DD
	
	ldy #40+15
	lda BlockHeight
	jsr Plot1DD

	ldy #40+17
	lda BlockQuantity
	jsr Plot2DD
	
	ldy #40+28
	lda MapID
	jsr Plot2DD

	ldy #40+31
	lda MapWidth
	jsr Plot2DD

	ldy #40+34
	lda MapHeight
	jsr Plot2DD
	
	ldy #40+37
	lda MapQuantity
	jsr Plot2DD
	
	;Plot Editor name
	ldx CurrentCBMEditor
	lda EditorNameTextLo-1,x
	sta source
	lda EditorNameTextHi-1,x
	sta source+1
	ldy #11
.(
loop1	lda (source),y
	sta $bfb8+28,y
	dey
	bpl loop1
.)
	rts
	
EditorNameTextLo
 .byt <CharacterEditorNameText
 .byt <BlockEditorNameText
 .byt <MapEditorNameText
EditorNameTextHi
 .byt >CharacterEditorNameText
 .byt >BlockEditorNameText
 .byt >MapEditorNameText
 
CharacterEditorNameText
 .byt "CHAR EDITOR "
BlockEditorNameText
 .byt "BLOCK EDITOR"
MapEditorNameText
 .byt "MAP EDITOR  "

