;Keyhelp.s
;      0123456789012345678901234567890123456789
KeyLegend1
 .byt "$/# Navigate up/down    R   Redefine Key"
 .byt "Q/A Previous/Next Page  Esc Back        "

khDisplayKeyLegend1
	ldx #79
.(
loop1	lda KeyLegend1,x
	sta $BFE0-80,x
	dex
	bpl loop1
.)
	rts
	


KeyHelpY		.byt 0
KeyListOffset		.byt 0
CountY			.byt 0
khEditorKeyCount	.byt 0
khLastCBMEditor		.byt 0

khInputDriver
	jsr InvertRowHighlight
.(	
loop1	jsr ROM_GTORKB
	bpl loop1
.)
	pha
	jsr InvertRowHighlight
	pla
	
	ldx #5
.(
loop1	cmp khBlackKey,x
	beq skip1
	dex
	bpl loop1
	jmp khInputDriver
skip1	lda khKeyVectorLo,x
	sta vector1+1
	lda khKeyVectorHi,x
	sta vector1+2
vector1	jsr $dead
.)
	jmp khInputDriver
	
khBlackKey
 .byt 10
 .byt 11
 .byt "Q"
 .byt "A"
 .byt "R"
 .byt 27

khKeyVectorLo
 .byt <khMoveDown
 .byt <khMoveUp
 .byt <khPageUp
 .byt <khPageDown
 .byt <khRedefine
 .byt <khEscape
khKeyVectorHi
 .byt >khMoveDown
 .byt >khMoveUp
 .byt >khPageUp
 .byt >khPageDown
 .byt >khRedefine
 .byt >khEscape

khMoveDown
	lda KeyHelpY
	sec
	adc KeyListOffset
	cmp khEditorKeyCount
.(
	bcs skip1
	lda KeyHelpY
	cmp #24
	bcs skip1
	inc KeyHelpY
skip1	rts
.)
	
khMoveUp
	lda KeyHelpY
.(
	beq skip1
	dec KeyHelpY
skip1	rts
.)

khPageUp
	lda KeyListOffset
.(
	beq skip1
	sec
	sbc #25
	sta KeyListOffset
	lda #00
	sta KeyHelpY
	jsr CLS
	jsr khDisplayScreen
	jsr khDisplayKeyLegend1
skip1	rts
.)

khPageDown
	lda KeyListOffset
	clc
	adc #25
	cmp khEditorKeyCount
.(
	bcs skip1
	sta KeyListOffset
	lda #00
	sta KeyHelpY
	jsr CLS
	jsr khDisplayScreen
	jsr khDisplayKeyLegend1
skip1	rts
.)

khRedefine
	;Clear last two lines
	jsr ClearLast2TextLines

	;0123456789012345678901234567890123456789
	;Press preferred key combination
	lda #TM_PRESSPREFERRED
	ldy #26
	jsr d2pDisplayTextMessage
	
	jsr WaitOnKey
	
	sta PressedBlackKey
	lda $0209
	sta PressedRedKey

	jsr ClearLast2TextLines
	
	;Validate Key
	lda PressedRedKey
	cmp #SOFTKEY_NONE
	beq khCheckBlackKey
	cmp #SOFTKEY_CTRL
	beq khCheckCtrl
	cmp #SOFTKEY_FUNC
	bne khKeyBad
khCheckBlackKey
	ldx #53
	lda PressedBlackKey
.(
loop1	cmp khValidKey,x
	beq khKeyOK
	dex
	bpl loop1
.)
	jmp khKeyBad
khCheckCtrl
	lda PressedBlackKey
	cmp #1
	bcc khKeyBad
	cmp #27
	bcs khKeyBad
	jmp khKeyOK
khKeyBad
	;Key or Combination not possible
	lda #TM_KEYNOTPOSS
	ldy #26
	jsr d2pDisplayTextMessage
	lda #TM_PRESSSPACE
	ldy #27
	jsr d2pDisplayTextMessage
	jsr WaitOnKey
	jsr CLS
	jsr khDisplayScreen
	jmp khDisplayKeyLegend1
khKeyOK
	ldx khLastCBMEditor
	lda HardKeyListLo,x
.(
	sta vector1+1
	lda HardKeyListHi,x
	sta vector1+2

	lda SoftKeyListLo,x
	sta vector2+1
	lda SoftKeyListHi,x
	sta vector2+2
	
	lda KeyHelpY
	clc
	adc KeyListOffset
	tay
	lda PressedRedKey
vector2	sta $dead,y
	lda PressedBlackKey
vector1	sta $dead,y
.)		
	jsr CLS
	jsr khDisplayScreen
	jmp khDisplayKeyLegend1

	
	
	
	

ClearLast2TextLines
	ldy #79
	lda #32
.(
loop1	sta $bfe0-80,y
	dey
	bpl loop1
.)
	rts
	
khEscape
	pla
	pla
	rts

InvertRowHighlight
	ldx KeyHelpY
	jsr khCalculateScreenRowLoc
	ldy #39
.(
loop1	lda (screen),y
	eor #128
	sta (screen),y
	dey
	bpl loop1
.)
	rts

;X holds 0 for Character, 1 for Block and 2 for Map Editor
khDisplayScreen
.(
	stx khLastCBMEditor
	lda HardKeyListLo,x
	sta vector1+1
	lda HardKeyListHi,x
	sta vector1+2

	lda SoftKeyListLo,x
	sta vector2+1
	lda SoftKeyListHi,x
	sta vector2+2

	lda KeyHelpListLoVectorLo,x
	sta text
	lda KeyHelpListHiVectorLo,x
	sta text+1
	lda KeyHelpListLoVectorHi,x
	sta text2
	lda KeyHelpListHiVectorHi,x
	sta text2+1

	lda EditorKeyCount,x
	sta khEditorKeyCount
	sta TempA

	ldy KeyListOffset
	
	lda #00
	sta CountY

loop1	jsr khCalculateScreenStart
vector2	ldx $dead,y
vector1	lda $dead,y
	
	sty TempY
	jsr DisplayKey
	jsr DisplayKeyDescription
	ldy TempY
	
	inc CountY
	lda CountY
	cmp #25
	beq skip1
	
	iny
	cpy TempA
	bcc loop1

skip1	rts
.)

khCalculateScreenStart
	ldx CountY
khCalculateScreenRowLoc
	lda TextYLOCL,x
	sta screen
	lda TextYLOCH,x
	sta screen+1
	rts
	
DisplayKey
	;Codes are complicated by the range 0-31 and the use of the CTRL key in Oric Basic.
	;For example you cannot use "CTRL Cursor Left" aswell as "CTRL H" simply because they
	;share the same ascii code 8. This is not a problem under asm but is alot more work and
	;beyond the scope of this editor.
	
	;No Soft key..
	;8  Cursor Left		 Symbol
	;9  Cursor Right         Symbol
	;10 Cursor Down          Symbol
	;11 Cursor Up            Symbol
	;13 Return               Ret
	;27 Esc                  Esc
	;32 Space                Space
	;39 Apostraphe           '
	;44 Comma                ,
	;45 Minus                -
	;46 Full Stop            .
	;47 Forward Slash        /
	;48-57 Number            0
	;59 Semicolon            ;
	;61 Equals               =
	;65-90 Letters           A
	;91 Open square bracket  [
	;92 Back Slash           \
	;93 Close square bracket ]
	;127 Del                 Del
	;
	;With Ctrl...
	;1-26 Ctrl Letter        Symbol A
	
	cpx #SOFTKEY_NONE
.(
	bne skip1
rent1	ldy #53
loop1	cmp khValidKey,y
	beq skip2
	dey
	bpl loop1
	jmp DisplayKeyUnknown
skip2	ldx khKeyTextHi,y
	beq DisplayCharacterOnly
	stx vector1+2
	ldx khKeyTextLo,y
	stx vector1+1
	ldy #02
	ldx #00
vector1	lda $dead,x
	pha
	and #127
	sta (screen),y
	inx
	iny
	pla
	bpl vector1
	rts
DisplayCharacterOnly
	ldy #02
	sta (screen),y
	rts
	
skip1	cpx #SOFTKEY_FUNC
	bne skip3
	
	pha
	lda #SYMBOL_FUNC
rent2	ldy #00
	sta (screen),y
	pla
	jmp rent1
	
skip3	cpx #SOFTKEY_CTRL
	bne DisplayKeyUnknown
	clc
	adc #64
	pha
	lda #SYMBOL_CTRL
	jmp rent2
.)
DisplayKeyUnknown
	ldy #2
	lda #"?"
	sta (screen),y
	iny
	sta (screen),y
	iny
	sta (screen),y
	rts

	
	
khValidKey
 .byt 8,9,10,11
 .byt 13,27
 .byt 32,39,44,45,46,47,48,49,50,51,52,53,54,55,56,57,59,61
 .byt 65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90
 .byt 91,92,93,127
khKeyTextLo
 .byt <khCrsrLeftText,<khCrsrRightText,<khCrsrDownText,<khCrsrUpText
 .byt <khReturnText,<khEscapeText
 .byt <khSpaceText,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 .byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 .byt 0,0,0,<khDelText
khKeyTextHi
 .byt >khCrsrLeftText,>khCrsrRightText,>khCrsrDownText,>khCrsrUpText
 .byt >khReturnText,>khEscapeText
 .byt >khSpaceText,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 .byt 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
 .byt 0,0,0,>khDelText

khCrsrLeftText
 .byt SYMBOL_CURSORLEFT+128
khCrsrRightText
 .byt SYMBOL_CURSORRIGHT+128
khCrsrDownText
 .byt SYMBOL_CURSORDOWN+128
khCrsrUpText
 .byt SYMBOL_CURSORUP+128
khReturnText
 .byt "Re","t"+128
khEscapeText
 .byt "Es","c"+128
khSpaceText
 .byt "Sp","c"+128
khDelText
 .byt "De","l"+128


DisplayKeyDescription
	ldy TempY
	lda (text1),y
.(
	sta vector1+1
	lda (text2),y
	sta vector1+2
	ldx #00
	ldy #7
vector1	lda $dead,x
	pha
	and #127
	sta (screen),y
	iny
	inx
	pla
	bpl vector1
.)
	rts

HardKeyListLo
 .byt <ceBlackKey
 .byt <beBlackKey
 .byt <meBlackKey
HardKeyListHi
 .byt >ceBlackKey
 .byt >beBlackKey
 .byt >meBlackKey
SoftKeyListLo
 .byt <ceRedKey
 .byt <beRedKey
 .byt <meRedKey
SoftKeyListHi
 .byt >ceRedKey
 .byt >beRedKey
 .byt >meRedKey

KeyHelpListLoVectorLo
 .byt <ceDescriptionLo
 .byt <beDescriptionLo
 .byt <meDescriptionLo
KeyHelpListHiVectorLo
 .byt >ceDescriptionLo
 .byt >beDescriptionLo
 .byt >meDescriptionLo
KeyHelpListLoVectorHi
 .byt <ceDescriptionHi
 .byt <beDescriptionHi
 .byt <meDescriptionHi
KeyHelpListHiVectorHi
 .byt >ceDescriptionHi
 .byt >beDescriptionHi
 .byt >meDescriptionHi

ceDescriptionLo
 .byt <Description_CursorLeft
 .byt <Description_CursorRight
 .byt <Description_CursorUp
 .byt <Description_CursorDown
                  
 .byt <Description_DecrementCharacterID
 .byt <Description_IncrementCharacterID
                  
 .byt <Description_Jump2BlockEditor
 .byt <Description_Jump2MapEditor
 .byt <Description_Jump2Files
                  
 .byt <Description_CopyAsLast
 .byt <Description_CopyAsNext
 .byt <Description_Grab
 .byt <Description_Drop
 .byt <ceDescription_ScrollWest
 .byt <ceDescription_ScrollEast
 .byt <ceDescription_HLine
 .byt <ceDescription_VLine
 .byt <ceDescription_ScrollNorth
 .byt <ceDescription_ScrollSouth
 .byt <ceDescription_Flip
 .byt <ceDescription_Mirror
 .byt <ceDescription_Rotate
 .byt <Description_Clear
 .byt <ceDescription_InverseLine
 .byt <ceDescription_DecrementCollisionID
 .byt <ceDescription_IncrementCollisionID
 .byt <ceDescription_ToggleBit 
 .byt <ceDescription_TopColour
 .byt <ceDescription_BotColour
 .byt <ceDescription_ToggleUsedFlagForCharacter
 .byt <ceDescription_SelectNextFreeCharacter
 .byt <Description_KeyHelp

ceDescriptionHi
 .byt >Description_CursorLeft
 .byt >Description_CursorRight
 .byt >Description_CursorUp
 .byt >Description_CursorDown
                  
 .byt >Description_DecrementCharacterID
 .byt >Description_IncrementCharacterID
                  
 .byt >Description_Jump2BlockEditor
 .byt >Description_Jump2MapEditor
 .byt >Description_Jump2Files
                  
 .byt >Description_CopyAsLast
 .byt >Description_CopyAsNext
 .byt >Description_Grab
 .byt >Description_Drop
 .byt >ceDescription_ScrollWest
 .byt >ceDescription_ScrollEast
 .byt >ceDescription_HLine
 .byt >ceDescription_VLine
 .byt >ceDescription_ScrollNorth
 .byt >ceDescription_ScrollSouth
 .byt >ceDescription_Flip
 .byt >ceDescription_Mirror
 .byt >ceDescription_Rotate
 .byt >Description_Clear
 .byt >ceDescription_InverseLine
 .byt >ceDescription_DecrementCollisionID
 .byt >ceDescription_IncrementCollisionID
 .byt >ceDescription_ToggleBit 
 .byt >ceDescription_TopColour
 .byt >ceDescription_BotColour
 .byt >ceDescription_ToggleUsedFlagForCharacter
 .byt >ceDescription_SelectNextFreeCharacter
 .byt >Description_KeyHelp
beDescriptionLo
 .byt <Description_CursorLeft
 .byt <Description_CursorRight
 .byt <Description_CursorUp
 .byt <Description_CursorDown
                  
 .byt <Description_DecrementCharacterID
 .byt <Description_IncrementCharacterID
 
 .byt <Description_Jump2CharacterEditor
 .byt <Description_Jump2MapEditor
 .byt <Description_Jump2Files
 
 .byt <beDescription_PlotCharacter
 .byt <beDescription_Delete
 .byt <Description_DecrementBlock
 .byt <Description_IncrementBlock
 
 .byt <Description_Grab
 .byt <Description_Drop
 .byt <beDescription_GrabCharacter
 .byt <Description_Clear
 
 .byt <beDescription_ToggleUsedFlagForBlock
 .byt <beDescription_SelectNextFreeBlock
 .byt <Description_KeyHelp

beDescriptionHi
 .byt >Description_CursorLeft
 .byt >Description_CursorRight
 .byt >Description_CursorUp
 .byt >Description_CursorDown
                  
 .byt >Description_DecrementCharacterID
 .byt >Description_IncrementCharacterID
 
 .byt >Description_Jump2CharacterEditor
 .byt >Description_Jump2MapEditor
 .byt >Description_Jump2Files
 
 .byt >beDescription_PlotCharacter
 .byt >beDescription_Delete
 .byt >Description_DecrementBlock
 .byt >Description_IncrementBlock
 
 .byt >Description_Grab
 .byt >Description_Drop
 .byt >beDescription_GrabCharacter
 .byt >Description_Clear
 
 .byt >beDescription_ToggleUsedFlagForBlock
 .byt >beDescription_SelectNextFreeBlock
 .byt >Description_KeyHelp
meDescriptionLo
 .byt <Description_CursorLeft
 .byt <Description_CursorRight
 .byt <Description_CursorUp
 .byt <Description_CursorDown
 
 .byt <Description_Jump2CharacterEditor
 .byt <Description_Jump2BlockEditor
 .byt <Description_Jump2Files

 .byt <Description_DecrementBlock
 .byt <Description_IncrementBlock
 .byt <meDescription_PlotBlock
 .byt <meDescription_GrabBlockBeneathCursor
 .byt <meDescription_Delete
 .byt <Description_Clear
 
 .byt <meDescription_PreviousMap
 .byt <meDescription_NextMap
 .byt <Description_KeyHelp
 .byt <Description_Dump

meDescriptionHi
 .byt >Description_CursorLeft
 .byt >Description_CursorRight
 .byt >Description_CursorUp
 .byt >Description_CursorDown
 
 .byt >Description_Jump2CharacterEditor
 .byt >Description_Jump2BlockEditor
 .byt >Description_Jump2Files

 .byt >Description_DecrementBlock
 .byt >Description_IncrementBlock
 .byt >meDescription_PlotBlock
 .byt >meDescription_GrabBlockBeneathCursor
 .byt >meDescription_Delete
 .byt >Description_Clear
 
 .byt >meDescription_PreviousMap
 .byt >meDescription_NextMap
 .byt >Description_KeyHelp
 .byt >Description_Dump

Description_Dump
 .byt "Dump Map to Printe","r"+128
Description_CursorLeft
 .byt "Cursor Lef","t"+128
Description_CursorRight
 .byt "Cursor Righ","t"+128
Description_CursorUp
 .byt "Cursor U","p"+128
Description_CursorDown
 .byt "Cursor Dow","n"+128
Description_DecrementCharacterID
 .byt "Decrement CharacterI","D"+128
Description_IncrementCharacterID
 .byt "Increment CharacterI","D"+128

Description_Jump2CharacterEditor
 .byt "Jump to Character Edito","r"+128
Description_Jump2BlockEditor
 .byt "Jump to Block Edito","r"+128
Description_Jump2MapEditor
 .byt "Jump to Map Edito","r"+128
Description_Jump2Files
 .byt "Jump to File","s"+128
           
Description_CopyAsLast
 .byt "Copy as las","t"+128
Description_CopyAsNext
 .byt "Copy as nex","t"+128
Description_Grab
 .byt "Gra","b"+128
Description_Drop
 .byt "Dro","p"+128
Description_Clear
 .byt "Clea","r"+128
Description_DecrementBlock
 .byt "Decrement Bloc","k"+128
Description_IncrementBlock
 .byt "Increment Bloc","k"+128
Description_KeyHelp
 .byt "Key Hel","p"+128

ceDescription_ScrollWest
 .byt "Scroll West/Lef","t"+128
ceDescription_ScrollEast
 .byt "Scroll East/Righ","t"+128
ceDescription_HLine
 .byt "Horizontal Line(127",")"+128
ceDescription_VLine
 .byt "Vertical Lin","e"+128
ceDescription_ScrollNorth
 .byt "Scroll North/U","p"+128
ceDescription_ScrollSouth
 .byt "Scroll South/Dow","n"+128
ceDescription_Flip
 .byt "Fli","p"+128
ceDescription_Mirror
 .byt "Mirro","r"+128
ceDescription_Rotate
 .byt "Rotat","e"+128
ceDescription_InverseLine
 .byt "Inverse Lin","e"+128
ceDescription_DecrementCollisionID
 .byt "Decrement CollisionI","D"+128
ceDescription_IncrementCollisionID
 .byt "Increment CollisionI","D"+128
ceDescription_ToggleBit
 .byt "Toggle Bit/Pixe","l"+128
ceDescription_TopColour
 .byt "First Colou","r"+128
ceDescription_BotColour
 .byt "Second Colou","r"+128
ceDescription_ToggleUsedFlagForCharacter
 .byt "Toggle Used Flag for Characte","r"+128
ceDescription_SelectNextFreeCharacter
 .byt "Select next free characte","r"+128
beDescription_PlotCharacter
 .byt "Plot Characte","r"+128
beDescription_Delete
 .byt "Delete Character under curso","r"+128
beDescription_GrabCharacter
 .byt "Grab Characte","r"+128
beDescription_ToggleUsedFlagForBlock
 .byt "Toggle Used flag for Bloc","k"+128
beDescription_SelectNextFreeBlock
 .byt "Select next free bloc","k"+128
meDescription_PlotBlock
 .byt "Plot Bloc","k"+128
meDescription_GrabBlockBeneathCursor
 .byt "Grab Block beneath curso","r"+128
meDescription_PreviousMap
 .byt "Previous Ma","p"+128
meDescription_NextMap
 .byt "Next Ma","p"+128
meDescription_Delete
 .byt "Delete Block beneath curso","r"+128



