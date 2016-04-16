;Generic and math Routines

fecpdInputLo	.byt 0
fecpdInputHi	.byt 0
Digits
 .dsb 5,0
;Input
;A value
;X multiplier
;Output
;A Value
;Corrupts
;AX
Mult8Bit
.(
	sta vector1+1
	txa

	beq skip1
	
	lda #00
	clc
vector1	adc #00
	dex
	bne vector1
skip1
.)
	rts


;Input
;A multiplier
;X Value Low
;Y Value High
;Output
;X Value Low
;Y Value High
;Corrupts
;AXY
Mult16Bit
.(
	stx vector1+1
	sty vector2+1
	ldx #00
	stx vector3+1
	stx vector4+1
	
	tax
	beq skip1
	clc
	
vector3	lda #00
vector1	adc #00
	sta vector3+1
vector4	lda #00
vector2	adc #00
	sta vector4+1
	
	dex
	bne vector3
	
skip1	ldx vector3+1
	ldy vector4+1
.)	
	rts

;Input
;fecpdInputLo
;fecpdInputHi
;Output (Text)
;Examples
;"65535"
;"226"
fecPlotDecimal
;joop2	nop
;	jmp joop2

	ldy #3
.(
loop2	ldx #47

loop1	inx
	
	lda fecpdInputLo
	sec
	sbc NoughtsTableLo,y
	sta fecpdInputLo
	lda fecpdInputHi
	sbc NoughtsTableHi,y
	sta fecpdInputHi
	
	bcs loop1

	lda fecpdInputLo
	adc NoughtsTableLo,y
	sta fecpdInputLo
	lda fecpdInputHi
	adc NoughtsTableHi,y
	sta fecpdInputHi
	
	txa
	sta Digits+1,y
	
	dey
	bpl loop2
.)	
	lda fecpdInputLo
	ora #48
	sta Digits
	
	;Look from left for 0
	ldx #04
.(
loop1	lda Digits,x
	cmp #48
	bne skip1
	dex
	bpl loop1
	inx
loop2
skip1
	;Display the digits starting at X
	lda Digits,x
	ldy ScreenXIndex
	sta (screen),y
	inc ScreenXIndex
	iny
	dex
	bpl loop2
.)
	rts


NoughtsTableLo
 .byt <10
 .byt <100
 .byt <1000
 .byt <10000
NoughtsTableHi
 .byt >10
 .byt >100
 .byt >1000
 .byt >10000

CLS
	ldx #00
	lda #32
.(
loop1	sta $BB80,x
	sta $BC80,x
	sta $BD80,x
	sta $BE80,x
	sta $BEE0,x
	dex
	bne loop1
.)
	rts

FlushInput
	jsr ROM_GTORKB
	bmi FlushInput
	rts
	
WaitOnKey
	;Flush keys
	jsr FlushInput
	
	;Wait on key
.(	
loop1	jsr ROM_GTORKB
	bpl loop1
.)
	rts

rscCharacter	.byt 0
;A The character to refresh
;will refresh the character in the character block and
;refresh any occurrence of the character in -1,0,+1 blocks
RefreshScreenCharacter
	sta rscCharacter
	
	;Breakdown into character block column and row
	and #31
	tax
	lda rscCharacter
	lsr
	lsr
	lsr
	lsr
	lsr
	
	;Calculate screen location
	tay
	lda rscCharacter
	jsr FetchCharacterAddress
	jsr DisplayCharacter
	
	;Now turn to blocks
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
	jsr rscSearchBlock
	
	lda BlockID
	jsr CalculateBlockAddress
	lda #<$A010+40*60
	sta ScreenLocationLo
	lda #>$A010+40*60
	sta ScreenLocationHi
	jsr rscSearchBlock

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
	jmp rscSearchBlock

rscSearchBlock
	lda TempL
	sta source
	lda TempH
	sta source+1
	
	ldx #00
.(
loop2	ldy BlockWidth
	dey
	
loop1	lda (source),y
	cmp rscCharacter
	bne skip1
	jsr PlotCharInBlock
skip1
	dey
	bpl loop1
	
	lda source
	clc
	adc BlockWidth
	sta source
	bcc skip2
	inc source+1 
skip2	
	inx
	cpx BlockHeight
	bcc loop2
.)
	rts
	
StopMapCursor
	;1 Stop Map cursor (running under interrupt)
	;2 Restore block behind cursor (to wipe cursor)
	
	lda #00
	sta CharAnimActive+3
	
	jsr CalculateBlockLocationInMap
	ldy #00
	lda (map),y
	pha
	jsr CalculateBlockAddress
	lda CharAnimScreenLocL+3
	sta ScreenLocationLo
	lda CharAnimScreenLocH+3
	sta ScreenLocationHi
	
	pla
	cmp BlockQuantity
.(
	bcc skip1
	pha
	lda #00
	jsr CalculateBlockAddress
	pla
	jmp PlotBlock00AndNumber
skip1	
.)	
	jmp PlotBlock

	
StartMapCursor
	;1 Recalculate new Block Cursor Screen Location
	;2 Start Block Cursor (running under interrupt)
	
	jsr CalcScreenLocInMapScreen
	lda ScreenLocationLo
	sta CharAnimScreenLocL+3
	lda ScreenLocationHi
	sta CharAnimScreenLocH+3
	
	lda BlockID
	jsr CalculateBlockAddress
	jsr PlotBlock

	lda #01
	sta CharAnimActive+3
	
	rts

CalculateBlocksWideAndHigh
	lda #00
	sta BlocksWide
	ldx MapWidth
.(
loop1	inc BlocksWide
	dex
	beq skip2
	clc
	adc BlockWidth
	cmp #39
	bcc loop1
skip2	;dec BlocksWide
	
	;BlocksHigh
	lda #00
	sta BlocksHigh
	ldx MapHeight
	
loop2	inc BlocksHigh
	dex
	beq skip3
	clc
	adc BlockHeight
	cmp #32
	bcc loop2
skip3	;dec BlocksHigh
.)
	rts
