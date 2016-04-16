;Files

SourceIndex		.byt 0
ScreenXIndex		.byt 0
BlockTotalLo		.byt 0
BlockTotalHi            .byt 0
MapTotalLo              .byt 0
MapTotalHi              .byt 0
AccumulatedValueLo      .byt 0
AccumulatedValueHi      .byt 0

;Settings
setDevice	.byt 0
setDrive	.byt 0


;0123456789012345678901234567890123456789
;CBM(AIC) Editor by Twilighte 2013 V0.01
;
;Characters;1x6x256  == 1536
;    Blocks;4x2x60   == 480
;      Maps;9x8x64   == 4608
;
;              Total == 6624 Bytes
;
;1 Save Project to Disk Drive A
;2 Load Project from Disk Drive A
;3 List Directory of Disk Drive A
;4 Capture graphics from Disk Drive A
;5 Update "Default" to Disk Drive A
;6 Dump Project to Printer
;7 New Project...
;8 Change to Tape operation
;9 Change Drive to B
;0 Quit!!
;
;ESC Return to Character Editor

;New Project...
;New Block Width (2-8) ;
;New Block Height (2-8)
;New Block Quantity (2-99)
;
;New Map Width (2-99)
;New Map Height (2-99)
;New Map Quantity (2-99)
;
;New Total Memory used (Max ????)
CompileFilesScreen
	lda #<FileEditorScreen
	sta source
	lda #>FileEditorScreen
	sta source+1
	
	lda #<$BB80+40
	sta screen
	lda #>$BB80+40
	sta screen+1
	
	ldy #00
	sty ScreenXIndex
	sty SourceIndex

feloop	ldy SourceIndex
	lda (source),y
	
	;Look for attribute codes
	beq feProcessEnd
	cmp #13
	beq feProcessReturn
	cmp #9
	beq feProcessTab
	
	;Look for markers
	cmp #"%"
	beq feProcessMarker
	
	;Plot
	ldy ScreenXIndex
	sta (screen),y
	
	inc ScreenXIndex
feRent1	inc SourceIndex
	bne feloop
	inc source+1
	jmp feloop


feProcessEnd
	rts
	
feProcessReturn
	lda #00
	sta ScreenXIndex
	lda screen
	clc
	adc #40
	sta screen
	lda screen+1
	adc #00
	sta screen+1
	jmp feRent1

feProcessTab
	lda #21
	sta ScreenXIndex
	jmp feRent1

feProcessMarker
;joop1	nop
;	jmp joop1

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
	lda MarkerCodeVectorLo,x
.(
	sta vector1+1
	lda MarkerCodeVectorHi,x
	sta vector1+2
vector1	jsr $dead
.)
	inc SourceIndex
	jmp feRent1
	
MarkerCodeVectorLo
 .byt <fecDeviceName	;%1 "Disk Drive" or "Tape"
 .byt <fecDriveLetter	;%2 "A", "B", "C" or "D"
 .byt <fecDeviceChange	;%3 "Tape" or "Disk Drive"
 .byt <fecFilename	;%4 Filename last used except capture
 .byt <fecDriveChange	;%5 "B", "C", "D" or "A"
 .byt <fecLastEditor	;%6 "Character Editor", "Block Editor" or "Map Editor"
 .byt <fecBlockWidth	;%7 Block Width
 .byt <fecBlockHeight	;%8 Block Height
 .byt <fecBlockQuantity	;%9 Number of Blocks
 .byt <fecBlockMemory	;%A Block Memory Total
 .byt <fecMapWidth	;%B Map Width
 .byt <fecMapHeight	;%C Map Height
 .byt <fecMapQuantity	;%D Number of Maps
 .byt <fecMapMemory	;%E Map Memory Total
 .byt <fecTotalMemory	;%F Total Memory Used
MarkerCodeVectorHi
 .byt >fecDeviceName	;%1 "Disk Drive" or "Tape"
 .byt >fecDriveLetter	;%2 "A", "B", "C" or "D"
 .byt >fecDeviceChange	;%3 "Tape" or "Disk Drive"
 .byt >fecFilename	;%4 Filename last used except capture
 .byt >fecDriveChange	;%5 "B", "C", "D" or "A"
 .byt >fecLastEditor	;%6 "Character Editor", "Block Editor" or "Map Editor"
 .byt >fecBlockWidth	;%7 Block Width
 .byt >fecBlockHeight	;%8 Block Height
 .byt >fecBlockQuantity	;%9 Number of Blocks
 .byt >fecBlockMemory	;%A Block Memory Total
 .byt >fecMapWidth	;%B Map Width
 .byt >fecMapHeight	;%C Map Height
 .byt >fecMapQuantity	;%D Number of Maps
 .byt >fecMapMemory	;%E Map Memory Total
 .byt >fecTotalMemory	;%F Total Memory Used

Text4InsertionLo
 .byt <txt_tape
 .byt <txt_disk
 .byt <txt_charactereditor
 .byt <txt_blockeditor
 .byt <txt_mapeditor
Text4InsertionHi
 .byt >txt_tape
 .byt >txt_disk
 .byt >txt_charactereditor
 .byt >txt_blockeditor
 .byt >txt_mapeditor

txt_tape
 .byt "Tap","e"+128
txt_disk
 .byt "Dis","k"+128
txt_charactereditor
 .byt "Character Edito","r"+128
txt_blockeditor
 .byt "Block Edito","r"+128
txt_mapeditor
 .byt "Map Edito","r"+128

 
;Unknowns...
Filename
 .byt "DEFAULT  "
FilenameLength	.byt 7
LastEditor	.byt 0

fecDeviceName	;%1 "Disk Drive"(0) or "Tape"(1)
	lda setDevice
	cmp #DEVICE_TAPE

	bne fecDisk
fecTape	lda #TEXT_TAPE
	jmp fecPlotText
fecDisk	lda #TEXT_DISK

fecPlotText
	tax
	lda Text4InsertionLo,x
.(
	sta vector1+1
	lda Text4InsertionHi,x
	sta vector1+2
	
	ldx #00

loop1	ldy ScreenXIndex
vector1	lda $dead,x
	pha
	and #127
	sta (screen),y
	inc ScreenXIndex
	iny
	inx
	pla
	bpl loop1
.)
	rts
	
fecDriveLetter	;%2 "A", "B", "C" or "D"
	;If Device Tape then mustn't plot letter
	lda setDevice
	cmp #DEVICE_TAPE
	beq fecdlSkip1

	lda setDrive
	;0-3
	clc
	adc #"A"
fecPlotAcc
	ldy ScreenXIndex
	sta (screen),y
	inc ScreenXIndex
fecdlSkip1
	rts

fecDeviceChange	;%3 "Tape" or "Disk Drive"
	lda setDevice
	cmp #DEVICE_TAPE
	beq fecDisk
	jmp fecTape

fecFilename	;%4 Filename last used except capture
	ldy ScreenXIndex
	lda FilenameLength
	sta TempC
	ldx #00
.(
loop1	lda Filename,x
	sta (screen),y
	inc ScreenXIndex
	inx
	iny
	dec TempC
	bne loop1
.)
	rts
	

fecDriveChange	;%5 "B", "C", "D" or "A"
	lda setDrive
	clc
	adc #"B"
	cmp #"E"

	bcc fecPlotAcc
	lda #"A"
	jmp fecPlotAcc
	
fecLastEditor	;%6 "Character Editor", "Block Editor" or "Map Editor"
	lda LastEditor
	clc
	adc #TEXT_CHARACTEREDITOR
	jmp fecPlotText	

fecBlockWidth	;%7 Block Width
	lda BlockWidth
	;2-8
	sta fecpdInputLo
	lda #00
	sta fecpdInputHi
	jmp fecPlotDecimal
	
fecBlockHeight	;%8 Block Height
	lda BlockHeight
	;2-8
	sta fecpdInputLo
	lda #00
	sta fecpdInputHi
	jmp fecPlotDecimal
	
fecBlockQuantity	;%9 Number of Blocks
	lda BlockQuantity
	;2-99
	sta fecpdInputLo
	lda #00
	sta fecpdInputHi
	jmp fecPlotDecimal

fecBlockMemory	;%A Block Memory Total

	lda BlockSize
	;4-64
	ldx BlockQuantity
	;2-99
	ldy #0
	jsr Mult16Bit
	stx BlockTotalLo
	sty BlockTotalHi
	stx fecpdInputLo
	sty fecpdInputHi
	jmp fecPlotDecimal 

fecMapWidth	;%B Map Width
	lda MapWidth
	;2-99
	sta fecpdInputLo
	lda #00
	sta fecpdInputHi
	jmp fecPlotDecimal
fecMapHeight	;%C Map Height
	lda MapHeight
	;2-99
	sta fecpdInputLo
	lda #00
	sta fecpdInputHi
	jmp fecPlotDecimal

fecMapQuantity	;%D Number of Maps
	lda MapQuantity
	sta fecpdInputLo
	lda #00
	sta fecpdInputHi
	jmp fecPlotDecimal

fecMapMemory	;%E Map Memory Total
	lda MapWidth
	ldx MapHeight
	ldy #00
	jsr Mult16Bit
	stx MapSizeLo
	sty MapSizeHi
	lda MapQuantity
	ldy #00
	jsr Mult16Bit
	stx MapTotalLo
	sty MapTotalHi
	stx fecpdInputLo
	sty fecpdInputHi
	jmp fecPlotDecimal

fecTotalMemory	;%F Total Memory Used
	;Character memory
	lda #<1536
	sta AccumulatedValueLo
	lda #>1536
	sta AccumulatedValueHi
	
	lda AccumulatedValueLo
	clc
	adc BlockTotalLo
	sta AccumulatedValueLo
	lda BlockTotalHi
	adc AccumulatedValueHi
	sta AccumulatedValueHi
	
	lda AccumulatedValueLo
	adc MapTotalLo
	sta AccumulatedValueLo
	lda AccumulatedValueHi
	adc MapTotalHi
	sta AccumulatedValueHi
	
	lda AccumulatedValueLo
	sta fecpdInputLo
	lda AccumulatedValueHi
	sta fecpdInputHi
	
;joop2	nop
;	jmp joop2
	
	jmp fecPlotDecimal 
		

FileEditorScreen
 .byt "CBM(AIC) Editor by Twilighte 2013 V0.02",13
 .byt 13
 .byt "Characters:1x6x256   == 1536",13
 .byt "    Blocks:%7x%8x%9",9,"== %A",13
 .byt "      Maps:%Bx%Cx%D",9,"== %E",13
 .byt 13
 .byt "               Total == %F Bytes",13
 .byt 13
 .byt "1 Save Project to %1 %2",13
 .byt "2 Load Project from %1 %2",13
 .byt "3 List Directory of %1 %2",13
 .byt "4 Capture graphics from %1 %2",13
 .byt "5 Update '%4' to %1 %2",13
 .byt "6 Dump Project to Printer",13
 .byt "7 New Project...",13
 .byt "8 Change to %3 operation",13
 .byt "9 Change Drive to %5",13
 .byt "0 Quit",13
 .byt 13
 .byt "ESC Return to %6",0

FilesEditor
FilesLoop
.(
loop1	jsr ROM_GTORKB
	bpl loop1
.)
	cmp #27
.(
	bne skip1
	jmp FilesEscape
skip1
.)
	cmp #"9"+1
	bcs FilesLoop
	cmp #"0"
	bcc FilesLoop
NumberSelected
	sbc #"0"
	tax
	lda FileNumberCodeVectorLo,x
.(
	sta vector1+1
	lda FileNumberCodeVectorHi,x
	sta vector1+2

vector1	jsr $dead
.)
	jmp FilesLoop
		
FileNumberCodeVectorLo
 .byt <flsQuit		;0
 .byt <flsSave          ;1
 .byt <flsLoad          ;2
 .byt <flsDir           ;3
 .byt <flsCapture       ;4
 .byt <flsUpdate        ;5
 .byt <flsDump          ;6
 .byt <flsNew           ;7
 .byt <flsDevice        ;8
 .byt <flsDrive         ;9
FileNumberCodeVectorHi
 .byt >flsQuit
 .byt >flsSave
 .byt >flsLoad
 .byt >flsDir
 .byt >flsCapture
 .byt >flsUpdate
 .byt >flsDump
 .byt >flsNew
 .byt >flsDevice
 .byt >flsDrive

flsQuit
	;Display Are you sure Y/N?
	
	;Carry Clear for not sure
.(
	bcc skip1
	
	lda #CMD_QUIT
	sta TRX_COMMAND4BASIC
	jmp ReturnToBasic
skip1	rts
.)

flsSave
	lda #CMD_SAVE
	sta TRX_COMMAND4BASIC
flsRent2
	lda #<ProjectStart
	sta TRX_PROJECTSTARTLO
	lda #>ProjectStart
	sta TRX_PROJECTSTARTHI
	lda #<MapMemory-1
	clc
	adc MapTotalLo
	sta TRX_PROJECTENDLO
	lda #>MapMemory-1
	adc MapTotalHi
	sta TRX_PROJECTENDHI
flsRent1
	lda setDevice
	sta TRX_DEVICEID
	lda setDrive
	sta TRX_DRIVENUMBER
	jmp ReturnToBasic

flsLoad
	lda #CMD_LOAD
	sta TRX_COMMAND4BASIC
	jmp flsRent1
	
flsDir
	lda #CMD_DIR
	sta TRX_COMMAND4BASIC
	jmp flsRent1

flsCapture
	;Plot for filename then load file from tape or disk
	;then return to highlight editor and capturing routines
	lda #CMD_CAPTURE
	sta TRX_COMMAND4BASIC
	jmp flsRent1

flsUpdate
	;Must be sure the project has been saved before
	;So check filename length
	lda FilenameLength
	;Also check Project Start and End parameters
	ora TRX_PROJECTSTARTHI
	ora TRX_PROJECTENDHI  
.(
	beq skip1
	lda #CMD_UPDATE
	sta TRX_COMMAND4BASIC
	jmp flsRent2
skip1	rts
.)

flsDump
	jmp Dump2Printer

flsNew
	jmp NewProject

flsDevice
	lda setDevice
	eor #1
	sta setDevice
	jmp CompileFilesScreen

flsDrive
	lda setDrive
	clc
	adc #1
	and #3
	sta setDrive
	jmp CompileFilesScreen

FilesEscape
	;Return to HIRES
	jsr ROM_HIRES
	jsr DisplayLegend
	jsr UpdateLegend
	
	;Return to previous CBM editor
	lda CurrentCBMEditor
	cmp #ED_CHARACTER
.(
	beq skip2
	cmp #ED_BLOCK
	bne PrepareForMapEditor
skip2	;These routines apply to both char and block editors
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
	jsr PlotBlocks
	
	lda CurrentCBMEditor
	cmp #ED_CHARACTER
	bne skip1
	jmp ceInputDriver
skip1	jmp beInputDriver

PrepareForMapEditor
.)
	lda #ACTIVE
	sta CharAnimActive+3
	lda #INACTIVE
	sta CharAnimActive	;Turn off Grid Cursor
	sta CharAnimActive+1	;Turn off Char Cursor
	sta CharAnimActive+2	;Turn off Block Cursor

	jsr DisplayMapColours
	jsr PlotMap
	jmp meInputDriver

ReturnToBasic
	;Disable irq routine
	sei
	lda #$40
	sta $024a
	cli
	ldx OriginalStackPointer
	txs
	rts
