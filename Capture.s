;Capture - 
;The highlight is simply an inverse of each corner byte thereby permitting it to work on any
;HIRES screen with any amount of exotic codes.
;The highlight will not look spectacular but it will suffice.

CaptureUnits	.byt CU_CHARACTER
CaptureX	.byt 0
CaptureY        .byt 0
CaptureW        .byt 1
CaptureH        .byt 1


Capture	
	jsr DisplayCaptureLegend
	jmp caInputDriver
;0123456789012345678901234567890123456789
;X;00 Y;00 Width;00 Height;00 
; Units;Blocks(4x2)

CaptureLegend
 .byt "X:00 Y:00 Width:00 Height:00            "
 .byt " Units: Blocks (4x2) Free:000           "
 .byt "                                        "

DisplayCaptureLegend
	ldx #119
.(
loop1	lda CaptureLegend,x
	sta $bfe0-120,x
	dex
	bpl loop1
.)
	ldy #2
	lda CaptureX
	jsr Plot2DD
	
	ldy #7
	lda CaptureY
	jsr Plot2DD
	
	ldy #16
	lda CaptureW
	jsr Plot2DD

	ldy #26
	lda CaptureH
	jsr Plot2DD
	
	ldx CaptureUnits
.(
	cpx #CU_BLOCKS
	beq DisplayBlockUnits
DisplayCharacterUnits
	ldx #11
loop1	lda UnitCharacterText,x
	sta $BFE0-72,x
	dex
	bpl loop1
	
	;Now search and display number of unused Characters
	ldy #00
	ldx #00
	clc
loop3	lda CollisionCode,x
	and #128
	bne skip3
	iny
	sec
skip3	inx
	bne loop3
	tya
	bne skip4
	bcs skip5
skip4	ldy #66
	jsr Plot3DD

	jmp skip1

skip5	;Display 256
	lda #"2"
	sta $BFE0-120+66
	lda #"5"
	sta $BFE0-120+67
	lda #"6"
	sta $BFE0-120+68
	jmp skip1

DisplayBlockUnits
	ldx #11
loop2	lda UnitBlockText,x
	sta $BFE0-72,x
	dex
	bpl loop2
	lda BlockWidth
	ora #48
	sta $BFE0-64
	lda BlockHeight
	ora #48
	sta $BFE0-62
	
	ldy #00
	ldx #00
loop4	lda CollisionCode,x
	and #64
	bne skip2
	iny
skip2	inx
	cpx BlockQuantity
	bcc loop4
	tya
	ldy #66
	jsr Plot3DD

skip1	rts
.)

UnitCharacterText
 .byt "Character   "
UnitBlockText
 .byt "Blocks (4x2)"

	
;In Units as Blocks the selected area is broken down into individual blocks then broken further
;into individual characters. These patterns are then searched for in the Character Data and
;replaced if they exist or new characters used if they don't.

;In units as characters the selected area is broken down into individual characters. These patterns
;are then searched for in the Character Data and replaced if they exist or new characters used if
;they don't.

;In both modes a Use Table will be used to identify used(1) or unused(0) characters.
;Note the option is given at the start to erase the Use table. By not erasing it (it is saved as
;part of the project) it will be possible to capture from multiple HIRES screens.
;The Use Table is also used to locate the next free character in the Character Editors 

caInputDriver

caInputDriverLoop
	jsr InverseSelectedArea	;Turn on
.(	
loop1	jsr ROM_GTORKB
	bpl loop1
.)	
	sta PressedBlackKey
	
	jsr InverseSelectedArea	;Turn off
	
	ldx #10
.(
loop1	lda PressedBlackKey
	cmp caBlackKey,x
	beq skip1
	dex
	bpl loop1
	jmp caInputDriverLoop
skip1
.)
	lda caCommandVectorLo,x
.(
	sta vector1+1
	lda caCommandVectorHi,x
	sta vector1+2
vector1	jsr $dead
.)
	jmp caInputDriverLoop
	
caBlackKey
 .byt 8		;Left
 .byt 9		;Right
 .byt 10	;Down
 .byt 11	;Up
 
 .byt "Z"	;Decrease Width
 .byt "X"	;Increase Width
 .byt "'"	;Decrease Height
 .byt "/"	;Increase Height
 
 .byt "U"       ;Select Units (Blocks or Characters)
 .byt 13	;Capture
 .byt 27	;Quit to Files

caCommandVectorLo
 .byt <capLeft
 .byt <capRight
 .byt <capDown
 .byt <capUp
 
 .byt <capDecreaseWidth
 .byt <capIncreaseWidth
 .byt <capDecreaseHeight
 .byt <capIncreaseHeight
 
 .byt <capSelectUnits
 .byt <capCapture
 .byt <capJump2Files

caCommandVectorHi
 .byt >capLeft
 .byt >capRight
 .byt >capDown
 .byt >capUp
 
 .byt >capDecreaseWidth
 .byt >capIncreaseWidth
 .byt >capDecreaseHeight
 .byt >capIncreaseHeight
 
 .byt >capSelectUnits
 .byt >capCapture
 .byt >capJump2Files

	
capLeft
	lda CaptureX
.(
	beq skip1
	dec CaptureX
	jmp DisplayCaptureLegend
skip1	rts
.)
capRight
	lda CaptureX
	clc
	adc CaptureW
	cmp #40
.(
	bcs skip1
	inc CaptureX
	jmp DisplayCaptureLegend
skip1	rts
.)

capUp
	lda CaptureY
.(
	beq skip1
	dec CaptureY
	jmp DisplayCaptureLegend
skip1	rts
.)

capDown
	lda CaptureY
	clc
	adc CaptureH
	cmp #33
.(
	bcs skip1
	inc CaptureY
	jmp DisplayCaptureLegend
skip1	rts
.)

capIncreaseWidth
	lda CaptureUnits
	cmp #CU_BLOCKS
.(
	beq skip1
 
	lda CaptureX
	clc
	adc CaptureW
	cmp #40
	bcs skip1
	inc CaptureW
	jmp DisplayCaptureLegend
skip1	rts
.)
capDecreaseWidth
	lda CaptureUnits
	cmp #CU_BLOCKS
.(
	beq skip1
	lda CaptureW
	cmp #1
	beq skip1
	dec CaptureW
	jmp DisplayCaptureLegend
skip1	rts
.)

capIncreaseHeight
	lda CaptureUnits
	cmp #CU_BLOCKS
.(
	beq skip1
	lda CaptureY
	clc
	adc CaptureH
	cmp #33
	bcs skip1
	inc CaptureH
	jmp DisplayCaptureLegend
skip1	rts
.)

capDecreaseHeight
	lda CaptureUnits
	cmp #CU_BLOCKS
.(
	beq skip1
	lda CaptureH
	cmp #1
	beq skip1
	dec CaptureH
	jmp DisplayCaptureLegend
skip1	rts
.)

capSelectUnits
	lda CaptureUnits
	eor #1
	sta CaptureUnits
	;Allign Capture Width and Height to new unit
	cmp #CU_CHARACTER
.(
	beq skip1
	
	lda #00
	sta CaptureX
	sta CaptureY
	lda BlockWidth
	sta CaptureW
	lda BlockHeight
	sta CaptureH
	jmp skip2
	
skip1	lda #00
	sta CaptureX
	sta CaptureY
	lda #1
	sta CaptureW
	sta CaptureH

skip2	jmp DisplayCaptureLegend
.)

capJump2Files
	ldx OriginalStackPointer
	txs
	lda #CMD_BOOT
	lda TRX_COMMAND4BASIC
	jsr ROM_TEXT
	jmp NormalTransfer

InverseSelectedArea
	;If the capture width is 1 then the left and right inverses will be at the same
	;location, so avoid cancelling inverse
	
	;Display Top Left...
	ldx CaptureY
	lda CaptureX
	clc
	adc Screen6x6YLOCL,x
	sta screen
	lda Screen6x6YLOCH,x
	adc #00
	sta screen+1
	
	ldy #00
	
	lda (screen),y
	eor #128
	sta (screen),y
	
	;Display Bottom Left...
	lda CaptureH
	adc CaptureY
	tax
	dex
	lda CaptureX
	clc
	adc Screen6x6YLOCL,x
	sta screen
	lda Screen6x6YLOCH,x
	adc #00
	sta screen+1
	
	ldy #5*40
	lda (screen),y
	eor #128
	sta (screen),y
		
	lda CaptureW
	cmp #1
.(
	beq skip1

	;Display Top Right...
	ldx CaptureY
	lda CaptureX
	clc
	adc CaptureW
	sec
	sbc #01
	clc
	adc Screen6x6YLOCL,x
	sta screen
	lda Screen6x6YLOCH,x
	adc #00
	sta screen+1
	
	ldy #00
	lda (screen),y
	eor #128
	sta (screen),y
	
	;Display Bottom Right...
	lda CaptureH
	adc CaptureY
	tax
	dex
	lda CaptureX
	adc CaptureW
	sec
	sbc #01
	clc
	adc Screen6x6YLOCL,x
	sta screen
	lda Screen6x6YLOCH,x
	sta screen+1
	
	ldy #5*40
	lda (screen),y
	eor #128
	sta (screen),y
	
skip1
.)
	rts

capCapture
	lda CaptureUnits
	cmp #CU_BLOCKS
	beq CaptureBlock
CaptureCharacters
	ldy CaptureY
	lda CaptureH
	sta CountH
.(
loop2	ldx CaptureX
	lda CaptureW
	sta CountW
	
loop1	jsr capGetScreenAddress
	jsr capLookForDuplicate
	bcs skip1
	jsr capFindFreeCharacter
	bcc skip2
	sta capFreeCharacter
	jsr capStoreCapturedCharacter
	jsr capMarkCharacterAsUsed
skip1
	inx
	dec CountW
	bne loop1
	
	iny
	dec CountH
	bne loop2

	jmp DisplayCaptureLegend
	
skip2	jsr WarnNoMoreChars
.)
	jmp DisplayCaptureLegend

	

CaptureBlock
	;Can only capture the one block atm
	jsr capFindFreeBlock
	bcs WarnNoMoreBlocks
	sta capFreeBlock
	jsr capMarkBlockAsUsed
	
	ldy CaptureY
	lda CaptureH
	sta CountH
	lda #00
	sta BlockY
.(
loop2	ldx CaptureX
	lda CaptureW
	sta CountW
	lda #00
	sta BlockX
	
loop1	jsr capGetScreenAddress
	jsr capLookForDuplicate
	bcs skip1
;	sta capDuplicateCharacter
	jsr capFindFreeCharacter
	bcc WarnNoMoreChars
	sta capFreeCharacter
	jsr capStoreCapturedCharacter
	lda capFreeCharacter
skip1	jsr capStoreCharacterInBlock
	jsr capMarkCharacterAsUsed

	inx
	inc BlockX
	dec CountW
	bne loop1
	
	iny
	inc BlockY
	dec CountH
	bne loop2
.)	
	jmp WarnBlockUsed

WarnNoMoreChars
	ldx #00
.(
loop3	lda NoMoreFreeCharacters,x
	pha
	and #127
	sta $BFE0-40,x
	inx
	pla
	bpl loop3
.)	
	rts

WarnNoMoreBlocks
	ldx #00
.(
loop3	lda NoMoreFreeBlocks,x
	pha
	and #127
	sta $BFE0-40,x
	inx
	pla
	bpl loop3
.)	
	rts
	
WarnBlockUsed
	ldx #00
.(
loop3	lda BlockUsedText,x
	pha
	and #127
	sta $BFE0-40,x
	inx
	pla
	bpl loop3
.)
	ldy #86
	lda capFreeBlock
	jmp Plot3DD



NoMoreFreeCharacters
 .byt "No more free Character","s"+128
NoMoreFreeBlocks
 .byt "No more free Block","s"+128
BlockUsedText
 .byt "Block xxx use","d"+128

;Find free block by scanning CollisionCode for first Block Bit clear (B6)
;Return Block in A with CLC else SEC
capFindFreeBlock
.(
	stx vector1+1
	ldx #00
	clc
loop1	lda CollisionCode,x
	and #64
	beq FreeBlockInX
	inx
	cpx BlockQuantity
	bcc loop1
FreeBlockInX
	txa
vector1	ldx #00
.)
	rts

;Mark the block as used in collisioncode at index A
capMarkBlockAsUsed
.(
	stx vector1+1
	tax
	lda CollisionCode,x
	ora #64
	sta CollisionCode,x
vector1	ldx #00
.)
	rts

;Get screen address at character based X,Y and place in screen/+1
capGetScreenAddress
	txa
	clc
	adc Screen6x6YLOCL,y
	sta screen
	lda Screen6x6YLOCH,y
	adc #00
	sta screen+1
	rts
 
;Match used character definitions with screen and SEC if found returning the characterID in A
;else CLC
capLookForDuplicate
.(
	stx Temp02
	sty TempY
	ldx #00
loop2
	lda CollisionCode,x
	bpl skip2

	txa
	jsr FetchCharacterAddress
	lda TempL
	sta vector3+1
	lda TempH
	sta vector3+2
	
	;Compare Character
	stx Temp01
	ldx #05
loop1	ldy Offset1x6,x
	lda (screen),y
	jsr FilterOutAttributes
vector3	cmp $dead,x
	bne skip1
	dex
	bpl loop1
	;Duplicate Found
	lda Temp01
	ldx Temp02
	ldy TempY
	sec
	rts
skip1	;Not a Duplicate
	ldx Temp01
	
skip2	inx
	bne loop2
.)	
	;No duplicates found
	ldx Temp02
	ldy TempY
	clc
	rts
	
;SEC if found and return Free character in A else CLC	
capFindFreeCharacter
.(
	stx TempX
	ldx #00
loop1	lda CollisionCode,x
	bpl skip1
	inx
	bne loop1
	;No free character found
	clc
	ldx TempX
	rts
skip1	;Free character found
.)
	sec
	txa
	ldx TempX
	rts

;Store the screen character in the free character
capStoreCapturedCharacter
	stx TempX
	sty TempY
.(	
	lda capFreeCharacter
	jsr FetchCharacterAddress
	lda TempL
	sta vector3+1
	lda TempH
	sta vector3+2

	ldx #5
loop1	ldy Offset1x6,x
	lda (screen),y
	
	jsr FilterOutAttributes
	
vector3	sta $dead,x
	dex
	bpl loop1
.)
	ldx TempX
	ldy TempY
	rts

FilterOutAttributes
	;capture Inverse Flag
	and #%10000000
	sta TempA
	
	;If not attribute then ok
	lda (screen),y
	and #%01111111
	cmp #%01000000
.(
	bcs recombine
	
	;Set to 64
	lda #64
	
recombine
.)
	ora TempA
	rts

;Store Character in A to BlockX,BlockY of capFreeBlock
capStoreCharacterInBlock
	sta TempA
	stx TempX
	sty TempY
	
	lda capFreeBlock
	jsr CalculateBlockAddress
	
	;Calculate offset in block
	lda BlockY
	ldx BlockWidth
	jsr Mult8Bit
	clc
	adc BlockX
	tay
	
	lda TempA
	sta (TempL),y
	
	lda TempA
	ldx TempX
	ldy TempY
	rts
	
capMarkCharacterAsUsed
	stx TempX
	ldx capFreeCharacter
	lda CollisionCode,x
	ora #128
	sta CollisionCode,x
	ldx TempX
	rts

