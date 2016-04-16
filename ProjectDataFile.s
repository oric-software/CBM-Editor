;Project data file


ProjectStart

Version
 .byt "Ver"	;New to V0.02
 .byt "0.02"	;New to V0.02
;Environment variables and tables
CharacterID		.byt 0
BlockID			.byt 0
MapID			.byt 0

CurrentCBMEditor	.byt 1

;Project Dimensions
BlockWidth	.byt 4
BlockHeight	.byt 2
BlockQuantity	.byt 60
MapWidth	.byt 9
MapHeight	.byt 8
MapQuantity	.byt 32

;Project properties
TopColour       .byt 6
BotColour       .byt 3

EditorKeyCount
 .byt 32
 .byt 20
 .byt 17

;Character Editor Keys
ceBlackKey
 .byt 8		;00 Grid Cursor Left
 .byt 9         ;01 Grid Cursor Right
 .byt 11        ;02 Grid Cursor Up
 .byt 10        ;03 Grid Cursor Down
                  
 .byt "-"	;04 Decrement CharacterID
 .byt "="	;05 Increment CharacterID
                  
 .byt 2       	;08 (CTRL B) Jump to Block Editor
 .byt 13       	;09 (CTRL M) Jump to Map Editor
 .byt 27       	;10 Jump to Files
                  
 .byt ","	;11 Copy As Last
 .byt "."	;12 Copy As Next
 .byt "J"	;13 Char Grab
 .byt "K"	;14 Char Drop
 .byt "W"	;15 Char Scroll West
 .byt "E"	;16 Char Scroll East
 .byt "H"	;17 Char H Line
 .byt "V"	;18 Char V Line
 .byt "N"	;19 Char Scroll North
 .byt "S"	;20 Char Scroll South
 .byt "Y"	;21 Char Flip
 .byt "X"	;22 Char Mirror
 .byt "R"	;23 Char Rotate
 .byt "C"	;24 Char Clear
 .byt "I"	;25 Char Inverse Line
 .byt "-"	;26 Decrement Collision ID
 .byt "="	;27 Increment Collision ID
 .byt " "	;28 Toggle bit 
 .byt "["	;29 Top Colour
 .byt "]"	;30 Bot Colour
 .byt "U"	;31 Toggle Used Flag for character
 .byt "F"	;32 Select next Free Character
 .byt 8		;33 Key Help
 .byt 0,0,0,0,0	;New to V0.02
ceRedKey
 .byt SOFTKEY_NONE
 .byt SOFTKEY_NONE
 .byt SOFTKEY_NONE
 .byt SOFTKEY_NONE

 .byt SOFTKEY_NONE
 .byt SOFTKEY_NONE

 .byt SOFTKEY_CTRL
 .byt SOFTKEY_CTRL
 .byt SOFTKEY_NONE
 
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_NONE
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_CTRL
 .byt 0,0,0,0,0	;New to V0.02
;Block Editor Keys
beBlackKey
 .byt 8		;Move cursor left within block
 .byt 9		;Move cursor right within block
 .byt 10	;Move cursor down within block
 .byt 11	;Move cursor up within block
 
 .byt "-"	;04 Decrement CharacterID
 .byt "="	;05 Increment CharacterID
 
 .byt 3		;CTRL C Jump to Character Editor
 .byt 13	;CTRL M Jump to Map Editor
 .byt 27	;Jump to Files
 
 .byt " "	;Plot Character
 .byt 127	;Delete (plot 00) Character beneath cursor
 .byt ","	;decrement block
 .byt "."	;increment block
 
 .byt "J"	;Grab Block
 .byt "K"	;Drop Block
 .byt "G"	;Grab Character under cursor
 .byt "C"	;Clear Block
 
 .byt "U"	;Toggle Used Flag for Block
 .byt "F"	;Select next free block
 .byt 8		;Key Help
 .byt 0,0,0,0,0	;New to V0.02
 
 
beRedKey
 .byt SOFTKEY_NONE
 .byt SOFTKEY_NONE
 .byt SOFTKEY_NONE
 .byt SOFTKEY_NONE

 .byt SOFTKEY_NONE
 .byt SOFTKEY_NONE

 .byt SOFTKEY_CTRL
 .byt SOFTKEY_CTRL
 .byt SOFTKEY_NONE

 .byt SOFTKEY_NONE
 .byt SOFTKEY_NONE
 .byt SOFTKEY_NONE
 .byt SOFTKEY_NONE

 .byt SOFTKEY_NONE
 .byt SOFTKEY_NONE
 .byt SOFTKEY_NONE
 .byt SOFTKEY_FUNC
 
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_FUNC
 .byt SOFTKEY_CTRL
 .byt 0,0,0,0,0	;New to V0.02
;Map Editor Keys
meBlackKey
 .byt 8		;Move cursor left with possible push scroll
 .byt 9		;Move Cursor Right with possible push scroll
 .byt 10	;Move Cursor Down with possible push scroll
 .byt 11	;Move Cursor Up with possible push scroll
 
 .byt 3		;CTRL C Jump to Character Editor
 .byt 2		;CTRL B Jump to Block Editor
 .byt 27	;Jump to Files

 .byt "-"	;Decrement Block
 .byt "="	;Increment Block
 .byt " "	;Plot Block
 .byt "G"	;Grab block beneath cursor
 .byt 127	;Delete (Plot block 00)
 .byt "C"	;Clear Map
 
 .byt "["	;Previous Map
 .byt "]"	;Next Map
 .byt 8		;Key Help
 .byt 4		;Dump(D) map to printer (New to V0.02)
 .byt 0,0,0,0,0	;New to V0.02
 
 
meRedKey
 .byt SOFTKEY_NONE	;Move cursor left with possible push scroll
 .byt SOFTKEY_NONE	;Move Cursor Right with possible push scroll
 .byt SOFTKEY_NONE	;Move Cursor Down with possible push scroll
 .byt SOFTKEY_NONE	;Move Cursor Up with possible push scroll
 
 .byt SOFTKEY_CTRL	;Jump to Character Editor
 .byt SOFTKEY_CTRL	;Jump to Block Editor
 .byt SOFTKEY_NONE	;Jump to Files

 .byt SOFTKEY_NONE	;Decrement Block
 .byt SOFTKEY_NONE	;Increment Block
 .byt SOFTKEY_NONE	;Plot Block
 .byt SOFTKEY_NONE	;Grab block beneath cursor
 .byt SOFTKEY_NONE	;Delete (Plot block 00)
 .byt SOFTKEY_FUNC	;Clear Map
 
 .byt SOFTKEY_NONE	;Previous Map
 .byt SOFTKEY_NONE	;Next Map
 .byt SOFTKEY_CTRL	;Key Help
 .byt SOFTKEY_CTRL	;Dump map to printer (New to V0.02)
 .byt 0,0,0,0,0	;New to V0.02


CharacterMemory
 .dsb 256*6,%01101010

;CollisionCode
;B0-5 CollisionID
;B6   Block Used Flag
;B7   Character Used Flag

CollisionCode
 .dsb 256,0

BlockMemory
 .dsb 64*32

MapMemory
 .dsb 16384,0


ProjectEnd

