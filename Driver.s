;Driver.s

;BFE0 trxCommand4Basic
;       CMD_QUIT
;	CMD_SAVE
;	CMD_LOAD
;	CMD_DIR
;	CMD_CAPTURE
;	CMD_UPDATE
;BFE1 Project Start Address Lo
;BFE2 Project Start Address Hi
;BFE3 Project End Address Lo
;BFE4 Project End Address Hi
;BFE5 Device Type
;BFE6 Drive Number
;BFE7 Filename Length
;BFE8 Filename(Maximum 9)

#define	TRX_COMMAND4BASIC 	$BFE0
#define	TRX_PROJECTSTARTLO	$BFE1
#define	TRX_PROJECTSTARTHI      $BFE2
#define	TRX_PROJECTENDLO        $BFE3
#define	TRX_PROJECTENDHI        $BFE4
#define	TRX_DEVICEID            $BFE5
#define	TRX_DRIVENUMBER         $BFE6
#define	TRX_FILENAMELENGTH      $BFE7
#define	TRX_FILENAME            $BFE8


#define	CMD_QUIT	0
#define	CMD_SAVE        1
#define	CMD_LOAD        2
#define	CMD_DIR         3
#define	CMD_CAPTURE     4
#define	CMD_UPDATE      5
#define	CMD_BOOT	9

#define	ROM_HIRES	$EC33
#define	ROM_TEXT	$EC21
#define	ROM_GTORKB	$EB78
#define	ROM_PRTCHAR	$F5C1

#define	BITOFFCHAR	0
#define	BITONCHAR	1
#define	CYANCHAR	2
#define	YELLOWCHAR	3
#define	CYANYELLOWCHAR	4
#define	CHARANIM00	5
#define	CHARANIM01	6

#define	SOFTKEY_NONE	56
#define	SOFTKEY_CTRL	162
#define	SOFTKEY_FUNC	165
#define	SOFTKEY_SHFT	164

#define	ACTIVE		1
#define	INACTIVE	0

#define	TEXT_TAPE		0
#define	TEXT_DISK		1
#define	TEXT_CHARACTEREDITOR	2
#define	TEXT_BLOCKEDITOR	3
#define	TEXT_MAPEDITOR		4

#define	DEVICE_DISK		0
#define	DEVICE_TAPE		1

#define	TM_WARNPRINTER		0
#define	TM_ALLDONE		1
#define	TM_DUMPINGCHARS		2
#define TM_DUMPINGBLOCKS	3
#define	TM_DUMPINGMAPS		4
#define	TM_AREYOUSURE		5
#define	TM_NOWSAVEPROJECT	6
#define	TM_DUMPINGCOLLISIONDATA	7
#define	TM_PRESSSPACE		8
#define	TM_PRESSPREFERRED	9
#define	TM_KEYNOTPOSS		10

#define	ED_FILES		0
#define	ED_CHARACTER		1
#define	ED_BLOCK		2
#define	ED_MAP			3

;Symbol characters in Standard Text charset
#define SYMBOL_CURSORLEFT	33
#define SYMBOL_CURSORRIGHT	34
#define SYMBOL_CURSORDOWN	35
#define SYMBOL_CURSORUP		36
#define SYMBOL_CTRL		37
#define SYMBOL_FUNC		38

#define	CU_BLOCKS		0
#define	CU_CHARACTER		1

#define	DT_COLLISIONDATA	1
#define	DT_CHARACTERDATA	2
#define	DT_BLOCKDATA		3
#define	DT_MAPDATA		4
#define	DT_IMAPDATA		5

 .zero
*=$00

TempC		.dsb 1
TempA           .dsb 1
TempX           .dsb 1
TempY           .dsb 1
TempL           .dsb 1
TempH           .dsb 1
screen		.dsb 2
irqTemp01	.dsb 1
text2
source		.dsb 2

*=$BB
map		.dsb 2

*=$F3
irqscreen	.dsb 2
text1
text		.dsb 2
irqTemp02	.dsb 1
irqTemp03	.dsb 1

 .text

*=$1800

Driver	jmp Driver2

#include "ProjectDataFile.s"

Driver2	tsx
	stx OriginalStackPointer
	jsr irqSetup
	jsr ModifyStandardCharset
	
	;Branch on TRX_COMMAND4BASIC
	lda TRX_COMMAND4BASIC
	cmp #CMD_SAVE
	beq TransferFilename
	cmp #CMD_LOAD
	beq TransferFilename
	cmp #CMD_CAPTURE
	beq Jump2Capture
NormalTransfer
	jsr CLS
	jsr CompileFilesScreen
	jmp FilesEditor

TransferFilename
	ldx $BFE7
	stx FilenameLength
	dex
.(
loop1	lda $BFE8,x
	sta Filename,x
	dex
	bpl loop1
.)	
	jsr CalculateBlocksWideAndHigh
	jmp NormalTransfer

Jump2Capture
	;We'll be in HIRES at this point
	lda #INACTIVE
	sta CharAnimActive
	sta CharAnimActive+1
	sta CharAnimActive+2
	jmp Capture

ModifyStandardCharset
	lda #$99
.(
	sta vector1+2
	lda $021F
	cmp #1
	beq skip1
	lda #$B5
	sta vector1+2
skip1	ldx #47
loop1	lda StandardCharacterMod,x
vector1	sta $B508,x
	dex
	bpl loop1
.)
	rts

StandardCharacterMod
 .byt %00000000
 .byt %00001000
 .byt %00011000
 .byt %00111110
 .byt %00011000
 .byt %00001000
 .byt %00000000
 .byt %00000000

 .byt %00000000
 .byt %00001000
 .byt %00001100
 .byt %00111110
 .byt %00001100
 .byt %00001000
 .byt %00000000
 .byt %00000000

 .byt %00000000
 .byt %00001000
 .byt %00001000
 .byt %00001000
 .byt %00111110
 .byt %00011100
 .byt %00001000
 .byt %00000000

 .byt %00000000
 .byt %00001000
 .byt %00011100
 .byt %00111110
 .byt %00001000
 .byt %00001000
 .byt %00001000
 .byt %00000000

 .byt %00000000
 .byt %00110111
 .byt %00100010
 .byt %00100010
 .byt %00100010
 .byt %00100010
 .byt %00110010
 .byt %00000000

 .byt %00000000
 .byt %00110110
 .byt %00100101
 .byt %00100101
 .byt %00110101
 .byt %00100101
 .byt %00100101
 .byt %00000000

OriginalStackPointer	.byt 0

PressedBlackKey	.byt 0
PressedRedKey	.byt 0

Grabbed		.byt 0	;Can be char or block
CharAnimFrac	.byt 0

;Character Editor Grid
GridCursorX	.byt 0
GridCursorY	.byt 0
GridInverseFlag	.byt 0

;Character Editor Char
CharCursorX	.byt 0
CharCursorY	.byt 0

;Block Editor
BlockCursorX		.byt 0
BlockCursorY		.byt 0
BlockSize		.byt 8
ScreenLocationLo	.byt 0
ScreenLocationHi	.byt 0

;Map Editor (Using example of Stormlord
MapSizeLo	.byt <9*8
MapSizeHi       .byt >9*8
BlocksWide      .byt 9
BlocksHigh      .byt 8
BlockX		.byt 0
BlockY		.byt 0
MapBaseX	.byt 0
MapBaseY	.byt 0
MapScreenX	.byt 0
MapScreenY	.byt 0

;IRQ Routine...
irqTempL	.byt 0
irqTempA        .byt 0
irqTempX        .byt 0
irqTempY        .byt 0

;Capture...
Temp01			.byt 0
Temp02          	.byt 0
CountH                  .byt 0
CountW                  .byt 0
capFreeCharacter        .byt 0
capFreeBlock            .byt 0


#include "CharacterEditor.s"
#include "AnimateCharacterCursor.s"
#include "Legend.s"
#include "BlockEditor.s"
#include "MapEditor.s"
#include "Generic.s"
#include "files.s"
#include "Dump.s"
#include "New.s"
#include "Capture.s"
#include "KeyHelp.s"


 
irqSetup
	sei
	lda #<irqDriver
	sta $024B
	lda #>irqDriver
	sta $024C
	lda #$4C
	sta $024a
	cli
	rts

irqDriver
	sta irqTempA
	stx irqTempX
	sty irqTempY
	
	lda CharAnimFrac
	adc #32
	sta CharAnimFrac
.(
	bcc skip1
	jsr AnimateCharacterID
skip1
.)	

	lda irqTempA
	ldx irqTempX
	ldy irqTempY
	rti

Plot3DD	ldx #47
	sec
.(
loop1	inx
	sbc #100
	bcs loop1
.)
	adc #100
	pha
	txa
	sta $BFE0-120,y
	iny
	pla


Plot2DD	ldx #47
	sec
.(
loop1	inx
	sbc #10
	bcs loop1
.)
	adc #10
	pha
	txa
	sta $BFE0-120,y
	iny
	pla
Plot1DD
	clc
	adc #48
	sta $BFE0-120,y
	iny
	rts

Plot2DH
	pha
	lsr
	lsr
	lsr
	lsr
	jsr Plot1DH
	pla
	and #15
Plot1DH
	cmp #10
.(
	bcc skip1
	adc #6
skip1	adc #48
.)
	sta $BFE0-120,y
	iny
	rts

EndOfMem
 .byt 0
