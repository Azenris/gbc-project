
INCLUDE "include/gameboy.inc"

; ------------------------------------------------------------------------
; memory
; ------------------------------------------------------------------------
SECTION "MEMORY FUNCTIONS", ROM0

; @function		MemCopy16
; @description	Copy memory from one area to another in 16byte chunks
; @param [de]	destination address
; @param [hl]	source address
; @param [b]	chunks of 16bytes ( must be > 0 and < 256 )
; @registers	[ a, b, d, e, h, l, f ] : NOT_PRESERVED
MemCopy16::
.memcopy16_loop
REPT 16
	ldi a, [hl]										; load a byte from the source address and increment
	ld [de], a										; copy the byte to the destination
	inc de											; increment the destination address to next byte
ENDR
	dec b											; decrease the chunk counter
	jr nz, .memcopy16_loop
	ret

; @function		MemCopySmall
; @description	Copy memory from one area to another, max 255 bytes
; @param [de]	destination address
; @param [hl]	source address
; @param [b]	bytes ( must be > 0 and < 256 )
; @registers	[ a, b, d, e, h, l, f ] : NOT_PRESERVED
MemCopySmall::
.memcopysmall_loop
	ldi a, [hl]										; load a byte from the source address and increment
	ld [de], a										; copy the byte to the destination
	inc de											; increment the destination address to next byte
	dec b											; decrease the byte counter
	jr nz, .memcopysmall_loop
	ret

; @function		MemCopy
; @description	Copy memory from one area to another
; @param [de]	destination address
; @param [hl]	source address
; @param [bc]	bytes ( must be > 0 )
; @registers	[ a, b, c, d, e, h, l, f ] : NOT_PRESERVED
MemCopy::
.memcopy_loop
	ldi a, [hl]										; load a byte from the source address and increment
	ld [de], a										; copy the byte to the destination
	inc de											; increment the destination address to next byte
	dec bc											; decrease the byte counter (dec r16 won't affect the flags')
	ld a, b											; because flags wont be set by dec r16, load b into a
	or a, c											; and or it (checking if any bits are present in b or c)
	jr nz, .memcopy_loop
	ret

; @function		MemSetSmall
; @description	Set a memory range to a value
; @param [hl]	destination address
; @param [b]	bytes ( must be > 0 and < 256 )
; @param [a]	value
; @registers	[ b, c, h, l, f ] : NOT_PRESERVED, [ a ] : PRESERVED
MemSetSmall::
.memset_loop
	ldi [hl], a										; copy the byte to the dest addr and increment hl
	dec b											; decrease the byte counter (dec r16 won't affect the flags')
	jr nz, .memset_loop
	ret

; @function		MemSetWideSmall
; @description	Set a memory range to a 16bit value
;				Expects an even number of bytes to write to
; @param [hl]	destination address
; @param [b]	bytes ( must be > 0 and < 256 and % 2 == 0 )
; @param [d]	high byte value
; @param [e]	low byte value value
; @registers	[ a, b, c, h, l, f ] : NOT_PRESERVED, [ d, e ] : PRESERVED
MemSetWideSmall::
.memset_loop
	ld a, d											; load register a with high byte
	ldi [hl], a										; copy the byte to the dest addr and increment hl
	ld a, e											; load register a with low byte
	ldi [hl], a										; copy the byte to the dest addr and increment hl
	dec b											; decrease the byte counter (dec r16 won't affect the flags')
	jr nz, .memset_loop
	ret

; @function		MemClearSmall
; @description	Clear a memory range to 0
; @param [hl]	destination address
; @param [b]	bytes ( must be > 0 and < 256 )
; @registers	[ a, b, h, l, f ] : NOT_PRESERVED
MemClearSmall::
	xor a											; set register a to 0
.memclear_loop
	ldi [hl], a										; set the dest addr byte to 0 and increment hl
	dec b											; decrease the byte counter (dec r16 won't affect the flags')
	jr nz, .memclear_loop
	ret

; @function		MemClear
; @description	Clear a memory range to 0
; @param [hl]	destination address
; @param [bc]	bytes ( must be > 0 )
; @registers	[ a, b, c, h, l, f ] : NOT_PRESERVED
MemClear::
.memclear_loop
	xor a											; set register a to 0
	ldi [hl], a										; set the dest addr byte to 0 and increment hl
	dec bc											; decrease the byte counter (dec r16 won't affect the flags')
	ld a, b											; because flags wont be set by dec r16, load b into a
	or a, c											; and or it (checking if any bits are present in b or c)
	jr nz, .memclear_loop
	ret

; ------------------------------------------------------------------------
; utility
; ------------------------------------------------------------------------
SECTION "UTILITY FUNCTIONS", ROM0

; @function		CallHL
; @description	call a hl
;				Can be used to jump to hl
;				add push pc to stack,
;				expects whatever is at hl to ret
;				To be used with MACRO : call_hl 
CallHL::
	jp hl											; jump to hl

; @function		CallHL
; @description	call a de
;				Can be used to jump to de
;				add push pc to stack,
;				expects whatever is at de to ret
;				To be used with MACRO : call_de 
CallDe::
	push de											; pushes on stack
	ret												; pops stack and jumps to it

; @function		EnableDoubleCPU
; @description	Enable the double cpu speed for GBC
; @registers	[ a, f ] : NOT_PRESERVED
EnableDoubleCPU::
	ld a, [g_bootup_type]							; get the boottype
	cp a, BOOTUP_A_CGB								; check its a gbc
	ret nz											; exit if it can't go double cpu mode
	xor a											; set register a to 0
	ldh [rIF], a									; set Interrupt Flag to 0 and clear current flags
	ld a, %00110000									; joypad bits 4 & 5
	ld [rP1], a										; set into FF00
	ld a, KEY1F_PREPARE								; set register a to 1
	ld [rSPD], a									; set into FF4D
	stop											; stop has to be called for the speed change to take effect
	ret

; @function		InputUpdate
; @description	Updates the joypad keys
; @registers	[ a, c, d, h, l, f ] : NOT_PRESERVED
InputUpdate::
	ldh a, [g_key_down]								; load the current keys into register a
	ldh [g_key_down_previous], a					; save them into the previous keys
	ld c, LOW( rP1 )								; low byte of address rP1

	ld a, %00100000									; load a with the get dpad bit
	ldh [c], a										; set that into $FF00
	ldh a, [c]										; read the results of the dpad
	ldh a, [c]										; the result is read multiple times to make sure its updated properly
	ldh a, [c]										; its called debouncing
	and a, $0f										; preserve only the first 4 bits [00001111]
	swap a											; swap the nibbles so [AAAABBBB] -> [BBBBAAAA]
	ld d, a											; store into register d

	ld a, %00010000									; load a with the get buttons bit
	ldh [c], a										; set that into $FF00
	ldh a, [c]										; read the results of the buttons
	ldh a, [c]										; once again read multiple times
	ldh a, [c]										; 
	ldh a, [c]										; 
	ldh a, [c]										; 
	ldh a, [c]										; 
	and a, $0f										; preserve only the first 4 bits [00001111]

	or a, d											; or a with d, combining their results
	cpl												; invert a so 0=not pressed and 1=pressed instead of the reverse

	ldh [g_key_down], a								; store all the key results into g_key_down

	; keys pressed
	ld d, a											; store current into register d
	ld hl, g_key_down_previous						; point to the previous down keys
	ld a, [hl]										; store previous keys into register a
	cpl												; flip the bits of the previous keys
	and a, d										; and them together to get keys previously not down, but now are [pressed keys]
	ldh [g_key_pressed], a							; store pressed keys into g_key_pressed

	; keys released
	ld a, d											; store the current keys into register a
	cpl												; flip the bits of current down keys
	ld d, [hl]										; store previous keys into register d
	and a, d										; and them together to get keys previously down, but now arn't [released keys]
	ldh [g_key_released], a							; store released keys into g_key_released

	ret

; ------------------------------------------------------------------------
; tile
; ------------------------------------------------------------------------
SECTION "TILE FUNCTIONS", ROM0

; @function		GBC_DMA / SetBGData
; @description	Start a DMA transfer of upto 127 blocks
;				Source & destination must be 16-byte aligned ($XXX0)
;				SetBGData will decrement the length by 1 automatically
; @param [de]	destination address											[range $8000-$9FF0]
; @param [hl]	source address [ROM, SRAM or WRAM]							[range $0000-$7FF0 or $A000-$DFF0]
; @param [b]	length ([chunks of 16 bytes] - 1). eg 6 tiles = send in 5	[range $00-$7F] (aka. 0-127)
; @registers	[ a ] : NOT_PRESERVED, [ b, d, e, h, l ] : PRESERVED
SetBGData::
	dec b											; dec the length by 1
GBC_DMA::
	ld a, h											; 
	ldh [rHDMA1], a									; load the source address high byte
	ld a, l											; 
	ldh [rHDMA2], a									; load the source address low byte
	ld a, d											; 
	ldh [rHDMA3], a									; load the destination address high byte
	ld a, e											; 
	ldh [rHDMA4], a									; load the destination address low byte
	ld a, b											; 
	ldh [rHDMA5], a									; load the number of chunks -1 to transfer
	ret

; @function		ClearScreenTile
; @description	Set the screen tilemap to a single tile
; @param [hl]	address base
; @param [d]	tile_id
; @param [e]	tile_attr
; @registers	[ a, b, c, d, e, h, l, f ] : NOT_PRESERVED
ClearScreenTile::
	ld c, SCREEN_TILE_HEIGHT						; set the height
	jr .no_carry									; skip the loop_height first time
.loop_height
	ld a, l											; load register a with low byte
	add a, 32 - SCREEN_TILE_WIDTH					; move to start of next row ( 32 bytes per row of the whole tilemap )
	ld l, a											; load result back into l
	jr nc, .no_carry								; check if there was a carry or not
	inc h											; increase high byte
.no_carry
	; reset width again
	ld a, d											; load register a with tile_id
	ld b, SCREEN_TILE_WIDTH / 4						; reset the width for the next loop
	push hl											; push hl to stack
	rst WaitVblank									; wait for vblamk
.loop_width
	; tile_id
	ldi [hl], a										; store this tile_id into the destination address
	ldi [hl], a										; store this tile_id into the destination address
	ldi [hl], a										; store this tile_id into the destination address
	ldi [hl], a										; store this tile_id into the destination address
	; loop
	dec b											; decrease width
	jr nz, .loop_width								; check if all the width has done
	; tile_attr
	ld a, 1											; set register a to 1
	ldh [rVBK], a									; set rVBK to bank 1
	ld a, e											; tile_attr
	ld b, SCREEN_TILE_WIDTH / 4						; reset the width for the next loop
	pop hl											; restore hl from stack to do a row of attributes
	rst WaitVblank									; wait for vblamk
.loop_tile_attr_width
	ldi [hl], a										; set the tile_attr at the destination address
	ldi [hl], a										; set the tile_attr at the destination address
	ldi [hl], a										; set the tile_attr at the destination address
	ldi [hl], a										; set the tile_attr at the destination address
	; loop
	dec b											; decrease width
	jr nz, .loop_tile_attr_width					; check if all the width has done
	xor a											; set register a to 0
	ldh [rVBK], a									; set rVBK back to bank 0
	; y loop
	dec c											; decrease height
	jr nz, .loop_height								; check if all the height has done
	ret

; @function		SetTile
; @description	Set a single tile_id & tile_attr
; @param [bc]	address base
; @param [d]	yoffset
; @param [e]	xoffset
; @param [h]	tile_id
; @param [l]	tile_attr
; @registers	[ a, b, c, d, e, h, l, f ] : NOT_PRESERVED
SetTile::
	xor a											; clear register a
	rr d											; shift the yoffset 3 times
	rra												; y * 32
	rr d											; 
	rra												; 
	rr d											; 
	rra												; 
	or e											; combine xoffset to a register
	ld e, a											; and place it in register e

	ld a, d											; load register d into a
	add a, b										; add the base address (the second half is always 00 so just do the upper byte)
	ld d, a											; update the destination

	ld a, h											; load the tile_id into register a
	ld [de], a										; set the tile_id at the destination address

	ld a, 1											; set register a to 1
	ldh [rVBK], a									; set rVBK to bank 1

	ld a, l											; load the tile_attr into register a
	ld [de], a										; set the tile_attr at the destination address

	xor a											; set register a to 0
	ldh [rVBK], a									; set rVBK back to bank 0

	ret

; @function		SetBGTilesSubmap
; @description	Setup bg tiles in a sub area
; @param [a]	destination base address : high byte
; @param [d]	yoffset
; @param [e]	xoffset
; @param [hl]	source data address
; @param [b]	width
; @param [c]	height
; @registers	[ a, b, c, d, e, h, l, f, reg_x, reg_r, reg_t ] : NOT_PRESERVED
SetBGTilesSubmap::
	; calculate the destination address
	push bc											; save bc
		ld b, a										; store the offset into register b
		xor a										; clear register a
		rr d										; shift the yoffset 3 times
		rra											; y * 32
		rr d										; 
		rra											; 
		rr d										; 
		rra											; 
		or a, e										; combine xoffset to a register
		ld e, a										; and place it in register e
		ld a, d										; load register d into a
		add a, b									; add the base address
		ld d, a										; update the destination
	pop bc											; restore bc
	ld a, b											; store width into register a
	ldh [reg_x], a									; store it into reg_x for later use
	jr .loop_width									; skip the loop_height first time
.loop_height
	ld a, e											; load register a with low byte
	add a, 32 - SCREEN_TILE_WIDTH					; move to start of next row ( 32 bytes per row of the whole tilemap )
	ld e, a											; load result back into e
	jr nc, .no_carry								; check if there was a carry or not
	inc d											; increase high byte
.no_carry
	; reset width again
	ldh a, [reg_x]									; load reg_x into register a
	ld b, a											; reset the width for the next loop
.loop_width
	ldi a, [hl]										; load tile_id and increase hl address
	ld [de], a										; store this tile_id into the destination address
	ld a, 1											; set register a to 1
	ldh [rVBK], a									; set rVBK to bank 1
	ldi a, [hl]										; load the tile_attr into register a and increase hl address
	ld [de], a										; set the tile_attr at the destination address
	xor a											; set register a to 0
	ldh [rVBK], a									; set rVBK back to bank 0
	inc de											; move to next destination
	dec b											; decrease width
	jr nz, .loop_width								; check if all the width has done
	dec c											; decrease height
	jr nz, .loop_height								; check if all the height has done
	ret

; @function		SpriteTileDataDestination
; @description	Converts an offset to a VRAM address
; @param [a]	tile_id
; @return [de]	destination
; @registers	[ a, d, e, h, l, f ] : NOT_PRESERVED
SpriteTileDataDestination::							;														| 25, 21
	ld hl, TILE_VRAM_DATA_8000						; tilemap												|  3,  3
	ld e, a											; load register e with a								|  1.  1
	rl e											; multiply e by 16										|  2,  2
	rla												; ( 16 bytes per tile )									|  1,  1
	rl e											; 														|  2,  2
	rla												; 														|  1,  1
	rl e											; 														|  2,  2
	rla												; 														|  1,  1
	rl e											; 														|  2,  2
	rla												; 														|  1,  1
	ld d, a											; load register d with a ( de = offset )				|  1,  1
	add hl, de										; offset hl by de										|  2,  1
	ld d, h											; store hl into de for output							|  1,  1
	ld e, l											; ( de = vram tiledata destination )					|  1,  1
	ret												;														|  4,  1

; @function		TileDataDestination
; @description	Converts an offset to a VRAM address
; @param [a]	tile_id
; @return [de]	destination
; @registers	[ a, d, e, h, l, f ] : NOT_PRESERVED
TileDataDestination::								;														| 25, 21
	ld hl, TILE_VRAM_DATA_8800						; tilemap												|  3,  3
	ld e, a											; load register e with a								|  1.  1
	rl e											; multiply e by 16										|  2,  2
	rla												; ( 16 bytes per tile )									|  1,  1
	rl e											; 														|  2,  2
	rla												; 														|  1,  1
	rl e											; 														|  2,  2
	rla												; 														|  1,  1
	rl e											; 														|  2,  2
	rla												; 														|  1,  1
	ld d, a											; load register d with a ( de = offset )				|  1,  1
	add hl, de										; offset hl by de										|  2,  1
	ld d, h											; store hl into de for output							|  1,  1
	ld e, l											; ( de = vram tiledata destination )					|  1,  1
	ret												;														|  4,  1

; @function		LoadScreenTiledata
; @description	Load screen tile data
; @param [hl]	screen_data
; @param [bc]	offset in screen_data for block count
; @registers	[ a, b, c, d, e, h, l, f ] : NOT_PRESERVED
LoadScreenTiledata::
	; load VRAM bank
	add hl, bc										; offset hl to SCREEN_DATA_TILES_VRAM1_BLOCKS
	ldd a, [hl]										; load register a with tile_data_size
	or a											; check if the size is 0
	ret z											; skip loading vram bank if its empty
	; vram bank has data !
	ld b, a											; load register b with chunk count
	ld c, 0											; default to 0 (which means no second batch)
	sub a, 128										; check if more than 1 batch needs loading
	jr c, .vram_check_done
	; more than 1 batch will need drawing
	ld c, a											; save the remaining chunks for a second batch
	ld b, 127										; load the max chunks capable
.vram_check_done

	ldd a, [hl]										; high byte
	ld l, [hl]										; low byte
	ld h, a											; hl = screen_data_tile_data

	; dma can only update 128 blocks at a time
	ld de, TILE_VRAM_DATA_8800_BOTTOM				; destination
	rst WaitVblank									; wait for vblamk
	call GBC_DMA									; transfer the data

	; check if a second batch is required
	ld a, c											; load register a with chunk count
	or a											; check if chunk count is 0
	ret z											; if there are no more tiles, return
	; more to draw
	ld bc, 128 * 16									; offset the next chunk
	add hl, bc										; update the hl to the new data
	ld b, a											; load register b with the remaining chunks
	ld de, TILE_VRAM_DATA_8800_TOP					; destination
	rst WaitVblank									; wait for vblamk
	jp GBC_DMA										; transfer the data

; @function		LoadScreenTilemap
; @description	Load screen tile data
; @param [hl]	screen_data
; @param [bc]	offset in screen_data for data
; @registers	[ a, b, c, d, e, h, l, f ] : NOT_PRESERVED
LoadScreenTilemap::
	add hl, bc										; offset hl
	ldi a, [hl]										; low byte
	ld h, [hl]										; high byte
	ld l, a											; hl = tilemap data

	ld b, SCREEN_TILE_HEIGHT						; screen height
	ld c, SCREEN_TILE_WIDTH							; screen width
	ld de, TILEMAP_DATA_9800						; destination in tilemap
	jr .no_carry									; start in the loop

.loop_y
	ld c, SCREEN_TILE_WIDTH							; screen width
	ld a, e											; load register a with e
	add a, 32 - SCREEN_TILE_WIDTH					; move to start of next row ( 32 bytes per row of the whole tilemap )
	ld e, a											; load it back into register e
	jr nc, .no_carry								; check if a carry is needed
	inc d											; increment the high byte if a carry was needed
.no_carry
	rst WaitVblank									; wait for vblamk
.loop_x
	ldi a, [hl]										; load register a with a source byte and incrememt hl
	ld [de], a										; set the destination with the new byte
	inc de											; increment the destination address to next byte
	dec c											; decrease the width by 1
	jr nz, .loop_x									; keep looping if there is more width to go
	dec b											; decrease the height by 1
	jr nz, .loop_y									; keep looping if there is more height to go
	ret

; @function		LoadScreen
; @description	Load screen data
; @param [a]	bank for screen_data
; @param [hl]	screen_data
; @registers	[ a, b, c, d, e, h, l, f ] : NOT_PRESERVED
LoadScreen::
	set_bank										; set bank to access screen data

	; load tiles into vram bank 1
	push hl											; store hl on the stack
		ld a, 1										; vram bank 1
		ldh [rVBK], a								; set the vram bank
		ld bc, SCREEN_DATA_TILES_VRAM1_BLOCKS		; offset SCREEN_DATA_TILES_VRAM1_BLOCKS
		call LoadScreenTiledata						; load tile data
		xor a										; vram bank 0
		ldh [rVBK], a								; set the vram bank
	pop hl											; restore hl from the stack

	; load tiles into vram bank 0
	push hl											; keep hl on the stack
		ld bc, SCREEN_DATA_TILES_VRAM0_BLOCKS		; offset SCREEN_DATA_TILES_VRAM1_BLOCKS
		call LoadScreenTiledata						; load tile data
	pop hl											; restore hl from the stack

	; load tilemap data
	push hl											; keep hl on the stack
		ld bc, SCREEN_DATA_TILEMAP					; offset SCREEN_DATA_TILEMAP
		call LoadScreenTilemap						; load tilemap
	pop hl											; restore hl from the stack

	; load tilemap attributes
	push hl											; keep hl on the stack
		ld a, 1										; vram bank 1
		ldh [rVBK], a								; set the vram bank
		ld bc, SCREEN_DATA_TILEMAP_ATTRIBUTES		; offset SCREEN_DATA_TILEMAP_ATTRIBUTES
		call LoadScreenTilemap						; load tilemap
		xor a										; vram bank 0
		ldh [rVBK], a								; set the vram bank
	pop hl											; restore hl from the stack

	; load palette data
	ld bc, SCREEN_DATA_PALETTE_SIZE					; offset SCREEN_DATA_TILEMAP_ATTRIBUTES
	add hl, bc										; offset hl to SCREEN_DATA_TILEMAP_ATTRIBUTES
	ldd a, [hl]										; palette count
	ld b, a											; load register b with palette count
	ldd a, [hl]										; high byte
	ld l, [hl]										; low byte
	ld h, a											; hl = palette_data
	xor a											; palette index
	jp SetBGPaletteShadow							; set the background palette data

; ------------------------------------------------------------------------
; rsts
; ------------------------------------------------------------------------

; @function		WaitVblank
; @description	Wait for a vblank interrupt to fire
; @registers	[ f ] : NOT_PRESERVED, [ h, l ] : PRESERVED
SECTION "RST_00", ROM0[$00]							; 8 bytes per rst slot, so this uses 2						|
WaitVblank::										; Wait for vblank before turning the LCD off				| 28, 15
	push hl											; push hl to the stack										|  4,  1
		ld hl, g_render_flag						; hl point to g_render_flag									|  3,  3
		res bRENDER_FLAG_V_BLANK, [hl]				; remove the vblank bit from g_render_flag					|  4,  2
.wait
		halt										; halt cpu until an interrupt triggers						|  -,  1
		bit bRENDER_FLAG_V_BLANK, [hl]				; check for bit bRENDER_FLAG_V_BLANK at hl address			|  3,  2
		jr z, .wait									; if its still 0, it was the wrong interrupt				|  3,  2
		res bRENDER_FLAG_V_BLANK, [hl]				; remove the vblank bit from g_render_flag					|  4,  2
	pop hl											; restore hl from the stack									|  3,  1
	ret												;															|  4,  1

;SECTION "RST_08", ROM0[$08]
;overflow from RST_00

; @function		JumpTable
; @description	Jumps to an address in a call table
; @param [a]	index
; @registers	[ a, h, l, f ] : NOT_PRESERVED
SECTION "RST_10", ROM0[$10]							; 8 bytes per rst slot, so this uses 2						| 16, 11
JumpTable::											;															|  c,  b
	pop	hl											; get the return address, which is the base of the table	|  3,  1
	rlca											; each entry is 2 bytes										|  1,  1
	add a, l										; add a to the table base address							|  1,  1
	jr nc, .no_carry								; check if there is a carry or not							|  3,  2
	inc	h											; add carry if needed										|  1,  1
.no_carry
	ld l, a											;															|  1,  1
	ldi a, [hl]										; low byte													|  2,  1
	ld h, [hl]										; high byte of target address								|  2,  1
	ld l, a											; low byte of target address								|  1,  1
	jp hl											; jump to target											|  1,  1

;SECTION "RST_18", ROM0[$18]
;overflow from RST_10

SECTION "RST_20", ROM0[$20]
	ret

SECTION "RST_28", ROM0[$28]
	ret

SECTION "RST_30", ROM0[$30]
	ret

SECTION "RST_38", ROM0[$38]
	ret

; ------------------------------------------------------------------------
; HRAM SHADOW REGISTERS - use these instead of the direct registers, they will be copied over in vblank
; 6 bytes
; ------------------------------------------------------------------------
SECTION "SHADOW REGISTERS", HRAM

VBLANK_ENABLED:: db									; bool
LCDC:: db											; rLCDC
SCREEN_X_POS:: db									; rSCX
SCREEN_Y_POS:: db									; rSCY
WINDOW_X_POS:: db									; rWX
WINDOW_Y_POS:: db									; rWY

; ------------------------------------------------------------------------
; HRAM General
; 14 bytes
; ------------------------------------------------------------------------
SECTION "HRAM GENERAL", HRAM

reg_x:: db											; used similar to a register
reg_y:: db											; used similar to a register
reg_r:: db											; used similar to a register
reg_t:: db											; used similar to a register
reg_u:: db											; used similar to a register
reg_v:: db											; used similar to a register
xa:: db												; used similar to a register
xb:: db												; used similar to a register
xc:: db												; used similar to a register
xd:: db												; used similar to a register
xe:: db												; used similar to a register
xf:: db												; used similar to a register
xh:: db												; used similar to a register
xl:: db												; used similar to a register

; ------------------------------------------------------------------------
; CODE ENTRY
; ------------------------------------------------------------------------
; gameboy starts a $100, the header is at $104
; this leaves 4 bytes of room for some entry code
SECTION "CODE ENTRY", ROM0[$0100]
	di												; disable interupts											|  1,  1
	jp Main											; jump to start of code										|  4,  3

; ------------------------------------------------------------------------
; HEADER
; ------------------------------------------------------------------------
SECTION "HEADER", ROM0[$0104]
REPT $150 - $104									; just 0 out the header, rgbfix will fix it
	db 0
ENDR