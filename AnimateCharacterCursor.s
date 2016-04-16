;Animate Character Cursor

AnimateCharacterID
	;Animate in 3 places
	;0 Character Grid (pixel position)
	;1 Character Block (Character ID)
	;2 Block (Character position)
	ldx #3
.(
loop2	lda CharAnimActive,x
	beq skip1
	
	lda CharAnimFrame,x
	clc
	adc #6
	cmp #12
	bcc skip2
	lda #00
	clc
skip2	sta CharAnimFrame,x

	adc #<CharAnimFrame00
	sta vector1+1
	lda #>CharAnimFrame00
	adc #00
	sta vector1+2

	lda CharAnimScreenLocL,x
	sta irqscreen
	lda CharAnimScreenLocH,x
	sta irqscreen+1
	
	stx irqTemp01
	
	cpx #3
	bcc skip3
	jsr ProcessMapCursor
	jmp pmcRent1

skip3	ldx #05
loop1	ldy Offset1x6,x
	lda (irqscreen),y
vector1	eor $dead,x
	sta (irqscreen),y
	dex
	bpl loop1
pmcRent1
	ldx irqTemp01
skip1	dex
	bpl loop2
.)
	rts



;Unlike character and block cursor, the map cursor must
;envelope the entire block.
;To speed up the process, use pattern bytes for horizontals
ProcessMapCursor
	ldx CharAnimFrame+3
	
	;Draw top
	ldy BlockWidth
	dey
.(
loop1	lda CharAnimFrame00,x
	and #%00111111
	eor (irqscreen),y
	sta (irqscreen),y
	dey
	bpl loop1
.)
	
	lda BlockHeight
	asl
	sta irqTemp02
	asl
	adc irqTemp02
	sbc #1
	sta irqTemp03
	
	lda #6
	sta irqTemp02
.(
loop1	lda irqscreen
	clc
	adc #40
	sta irqscreen
	lda irqscreen+1
	adc #00
	sta irqscreen+1
	
	;Draw left side
	lda CharAnimFrame00,x
	and #%00100000
	ldy #00
	eor (irqscreen),y
	sta (irqscreen),y
	
	;Draw right side
	lda CharAnimFrame00,x
	and #%00000001
	ldy BlockWidth
	dey
	eor (irqscreen),y
	sta (irqscreen),y
	
	inx
	dec irqTemp02
	bne skip1
	ldx CharAnimFrame+3
	lda #6
	sta irqTemp02
	
skip1	dec irqTemp03
	bne loop1
.)	
	lda irqscreen
	clc
	adc #40
	sta irqscreen
	lda irqscreen+1
	adc #00
	sta irqscreen+1

	;Draw Bottom
	ldy BlockWidth
	dey
	ldx CharAnimFrame+3
.(
loop1	lda CharAnimFrame00+5,x
	and #%00111111
	eor (irqscreen),y
	sta (irqscreen),y
	dey
	bpl loop1
.)
	rts

CharAnimActive
 .byt ACTIVE	;Grid Cursor
 .byt ACTIVE	;Char Cursor
 .byt ACTIVE	;Block Cursor
 .byt INACTIVE	;Map Cursor
CharAnimFrame
 .dsb 4,0
CharAnimScreenLocL
 .byt <$A001		;Character Grid Cursor
 .byt <$A008		;Character Char Cursor
 .byt <$A010+40*60      ;Block Character Cursor
 .byt <$A001		;Map Cursor
CharAnimScreenLocH
 .byt >$A001
 .byt >$A008
 .byt >$A010+40*60
 .byt >$A001

;0----1
;------
;------
;------
;------
;3----2
