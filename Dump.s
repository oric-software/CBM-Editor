;Dump - Dump to Printer
;Dump Characters, Blocks and Maps to the Printer as .byt xa
;statements.
;Dump Characters as 16 bytes per row.
;Dump Blocks as the block width per row. Only the number of
;blocks defined will be dumped.
;Dump Maps as 16 bytes per row. Only the number of Maps defined
;will be dumped.
;Each Area is given the label CharacterDataxx,BlockDataxx and
;MapDatayy where xx is index in hex and yy is index in decimal



CharacterDataText
 .byt "CharacterDat","a"+128
BlockDataText
 .byt "BlockDat","a"+128
MapDataText
 .byt "MapDat","a"+128
CollisionDataText
 .byt "CollisionTabl","e"+128

d2pDumpType	.byt 0
d2pStartLo	.byt 0
d2pStartHi      .byt 0
d2pLengthLo     .byt 0
d2pLengthHi     .byt 0
d2pRowWidth     .byt 0
d2pLabelLo      .byt 0
d2pLabelHi      .byt 0
d2pLabelHeight	.byt 0
LabelIndex	.byt 0



Dump2Printer
	jsr CLS
	lda #TM_WARNPRINTER
	ldy #1
	jsr d2pDisplayTextMessage
	jsr d2pGetConfirmation
.(
	bcs skip1
	jsr CLS
	jmp CompileFilesScreen
skip1
.)
	lda #TM_DUMPINGCHARS
	ldy #3
	jsr d2pDisplayTextMessage

	;Dump Characters First
	;Except must be dumped as 6x1 bytes per label
	lda #DT_CHARACTERDATA
	sta d2pDumpType
	lda #<CharacterMemory
	sta d2pStartLo
	lda #>CharacterMemory
	sta d2pStartHi
	lda #<1536
	sta d2pLengthLo
	lda #>1536
	sta d2pLengthHi
	lda #6
	sta d2pRowWidth
	lda #1
	sta d2pLabelHeight
	lda #<CharacterDataText
	sta d2pLabelLo
	lda #>CharacterDataText
	sta d2pLabelHi
	jsr DumpArea
	
	lda #TM_DUMPINGBLOCKS
	ldy #4
	jsr d2pDisplayTextMessage

	;Dump Block
	lda #DT_BLOCKDATA
	sta d2pDumpType
	lda #<BlockMemory
	sta d2pStartLo
	lda #>BlockMemory
	sta d2pStartHi
	lda BlockTotalLo
	sta d2pLengthLo
	lda BlockTotalHi
	sta d2pLengthHi
	lda BlockWidth
	sta d2pRowWidth
	lda BlockHeight
	sta d2pLabelHeight
	lda #<BlockDataText
	sta d2pLabelLo
	lda #>BlockDataText
	sta d2pLabelHi
	jsr DumpArea

	lda #TM_DUMPINGMAPS
	ldy #5
	jsr d2pDisplayTextMessage

	;Dump Map
	lda #DT_MAPDATA
	sta d2pDumpType
	lda #<MapMemory
	sta d2pStartLo
	lda #>MapMemory
	sta d2pStartHi
	lda MapTotalLo
	sta d2pLengthLo
	lda MapTotalHi
	sta d2pLengthHi
	lda #16
	sta d2pRowWidth
	lda #0
	sta d2pLabelHeight
	lda #<MapDataText
	sta d2pLabelLo
	lda #>MapDataText
	sta d2pLabelHi
	jsr DumpArea
	
	lda #TM_DUMPINGCOLLISIONDATA
	ldy #6
	jsr d2pDisplayTextMessage
	
	;Dump Collision Table
	lda #DT_COLLISIONDATA
	sta d2pDumpType
	lda #<CollisionCode
	sta d2pStartLo
	lda #>CollisionCode
	sta d2pStartHi
	lda #<256
	sta d2pLengthLo
	lda #>256
	sta d2pLengthHi
	lda #8
	sta d2pRowWidth
	lda #0
	sta d2pLabelHeight
	lda #<CollisionDataText
	sta d2pLabelLo
	lda #>CollisionDataText
	sta d2pLabelHi
.(
skip1	nop
	jmp skip1
.)
	jsr DumpArea
	
	
	lda #TM_ALLDONE
	ldy #7
	jsr d2pDisplayTextMessage
	
	
	lda #TM_PRESSSPACE
	ldy #8
	jsr d2pDisplayTextMessage
	
	jsr WaitOnKey
	jsr CLS
	jmp CompileFilesScreen


d2pGetConfirmation
	ldy #3
	lda #TM_AREYOUSURE
	jsr d2pDisplayTextMessage
.(
loop1	jsr ROM_GTORKB
	bmi loop1
loop2	jsr ROM_GTORKB
	bpl loop2
.)
	cmp #"Y"
.(
	beq skip1
	clc
skip1	rts
.)
	
DumpArea
	;
	ldx d2pDumpType
	cpx #DT_IMAPDATA
.(
	beq skip1
	lda #00
skip1	sta LabelIndex
.)	
	lda d2pStartLo
	sta source
	lda d2pStartHi
	sta source+1
	lda #01
	sta TempH
.(	
loop2
	dec TempH
	bne skip1
	lda d2pLabelHeight
	sta TempH

	;Print Label with index in hex
	lda d2pLabelLo

	sta vector1+1
	lda d2pLabelHi
	sta vector1+2

	ldy #00
vector1	lda $dead,y
	iny
	pha
	and #127
	jsr ROM_PRTCHAR
	pla
	bpl vector1
	
	lda LabelIndex
	jsr Convert2DisplayableText
	jsr ROM_PRTCHAR
	txa
	jsr ROM_PRTCHAR
	
	inc LabelIndex
	
	;Send Carriage Return and Line Feed
	jsr NextLine

skip1	lda #" "
	jsr ROM_PRTCHAR
	lda #"."
	jsr ROM_PRTCHAR
	lda #"b"
	jsr ROM_PRTCHAR
	lda #"y"
	jsr ROM_PRTCHAR
	lda #"t"
	jsr ROM_PRTCHAR
	lda #" "
	jsr ROM_PRTCHAR
	
	lda d2pRowWidth
	sta TempC
	
loop1	lda #"$"
	jsr ROM_PRTCHAR
	ldy #00
	lda (source),y
	
	;If Collision display only B0-5
	ldy d2pDumpType
	cpy #DT_COLLISIONDATA
	bne skip2
	and #63
	
skip2	;Convert to displayable hex
	jsr Convert2DisplayableText

	jsr ROM_PRTCHAR
	txa
	jsr ROM_PRTCHAR
	
	inc source
	bne skip3
	inc source+1
skip3
	lda d2pLengthLo
	sec
	sbc #1
	sta d2pLengthLo
	lda d2pLengthHi
	sbc #00
	sta d2pLengthHi
	ora d2pLengthLo
	beq skip5
	bcc skip5
	
	dec TempC
	beq skip4
	
	lda #","
	jsr ROM_PRTCHAR
	
	jmp loop1
skip4
	;Proceed to next .byt row statement
	jsr NextLine
	
	jmp loop2
skip5
	jsr NextLine
	jsr NextLine
	rts
.)
	
Convert2DisplayableText
	pha
	and #15
	cmp #10
.(
	bcc skip1
	adc #6
skip1	adc #48
	tax
	pla
	lsr
	lsr
	lsr
	lsr
	cmp #10
	bcc skip2
	adc #6
skip2	adc #48
.)
	rts

NextLine
	lda #13
	jsr ROM_PRTCHAR
	lda #10
	jmp ROM_PRTCHAR

d2pDisplayTextMessage
	;Display text via pointer in A
	;Display starting on row specified in Y
	tax
	lda d2pTextAddressLo,x
	sta source
	lda d2pTextAddressHi,x
	sta source+1
	
	lda TextYLOCL,y
	sta screen
	lda TextYLOCH,y
	sta screen+1
	
	ldy #00
.(
loop1	lda (source),y
	iny
	pha
	and #127
	sta (screen),y
	pla
	bpl loop1
.)
	rts

TextYLOCL
 .byt <$BB80	 ;0
 .byt <$BB80+40*1    ;1
 .byt <$BB80+40*2    ;2
 .byt <$BB80+40*3    ;3
 .byt <$BB80+40*4    ;4
 .byt <$BB80+40*5    ;5
 .byt <$BB80+40*6    ;6
 .byt <$BB80+40*7    ;7
 .byt <$BB80+40*8    ;8
 .byt <$BB80+40*9    ;9
 .byt <$BB80+40*10   ;10
 .byt <$BB80+40*11   ;11
 .byt <$BB80+40*12   ;12
 .byt <$BB80+40*13   ;13
 .byt <$BB80+40*14   ;14
 .byt <$BB80+40*15   ;15
 
 .byt <$BB80+40*16   ;16
 .byt <$BB80+40*17   ;17
 .byt <$BB80+40*18   ;18
 .byt <$BB80+40*19   ;19
 .byt <$BB80+40*20   ;20
 .byt <$BB80+40*21   ;21
 .byt <$BB80+40*22   ;22
 .byt <$BB80+40*23   ;23
 .byt <$BB80+40*24   ;24
 .byt <$BB80+40*25   ;25
 .byt <$BB80+40*26   ;26
 .byt <$BB80+40*27   ;27

TextYLOCH
 .byt >$BB80	 ;0
 .byt >$BB80+40*1    ;1
 .byt >$BB80+40*2    ;2
 .byt >$BB80+40*3    ;3
 .byt >$BB80+40*4    ;4
 .byt >$BB80+40*5    ;5
 .byt >$BB80+40*6    ;6
 .byt >$BB80+40*7    ;7
 .byt >$BB80+40*8    ;8
 .byt >$BB80+40*9    ;9
 .byt >$BB80+40*10   ;10
 .byt >$BB80+40*11   ;11
 .byt >$BB80+40*12   ;12
 .byt >$BB80+40*13   ;13
 .byt >$BB80+40*14   ;14
 .byt >$BB80+40*15   ;15

 .byt >$BB80+40*16   ;16
 .byt >$BB80+40*17   ;17
 .byt >$BB80+40*18   ;18
 .byt >$BB80+40*19   ;19
 .byt >$BB80+40*20   ;20
 .byt >$BB80+40*21   ;21
 .byt >$BB80+40*22   ;22
 .byt >$BB80+40*23   ;23
 .byt >$BB80+40*24   ;24
 .byt >$BB80+40*25   ;25
 .byt >$BB80+40*26   ;26
 .byt >$BB80+40*27   ;27

 		

d2pTextAddressLo
 .byt <tm_warnprinter
 .byt <tm_alldone
 .byt <tm_dumpingchars
 .byt <tm_dumpingblocks
 .byt <tm_dumpingmaps
 .byt <tm_areyousure
 .byt <tm_NowSaveProject
 .byt <tm_dumpingcollisiondata
 .byt <tm_PressSpace
 .byt <tm_presspreferred
 .byt <tm_keynotposs
d2pTextAddressHi
 .byt >tm_warnprinter
 .byt >tm_alldone
 .byt >tm_dumpingchars
 .byt >tm_dumpingblocks
 .byt >tm_dumpingmaps
 .byt >tm_areyousure
 .byt >tm_NowSaveProject
 .byt >tm_dumpingcollisiondata
 .byt >tm_PressSpace
 .byt >tm_presspreferred
 .byt >tm_keynotposs

;Messages may be over 6 rows long but must align to 40 columns
tm_warnprinter
;      0123456789012345678901234567890123456789
 .byt 1,"Ensure Printer is enabled before       "
 .byt 1,"proceeding!","!"+128
 
tm_alldone
 .byt 2,"All Done!","!"+128

tm_dumpingchars
 .byt 2,"Dumping Character Data..","."+128
tm_dumpingblocks
 .byt 2,"Dumping Block Data..","."+128
tm_dumpingmaps
 .byt 2,"Dumping Map Data..","."+128
tm_areyousure
 .byt 5,"Are you sure you wish to continue Y/N","?"+128
tm_NowSaveProject
 .byt 12,"Now Save your project","!"+128
tm_dumpingcollisiondata
 .byt 2,"Dumping Collision Data..","."+128
tm_PressSpace
 .byt 3,"Press Spac","e"+128
tm_presspreferred
 .byt 3,"Press preferred key combinatio","n"+128
tm_keynotposs
 .byt 5,"Key or combination not possibl","e"+128