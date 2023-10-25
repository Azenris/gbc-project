
INCLUDE "include/gameboy.inc"

; ------------------------------------------------------------------------
; palette data
; ------------------------------------------------------------------------
SECTION "PALETTE DATA", WRAM0

shadow_bg_palette:: ds 4 * 8 * 2					; bg palette memory 64 bytes
shadow_obj_palette:: ds 4 * 8 * 2					; obj palette memory 64 bytes

; ------------------------------------------------------------------------
; palette functions
; ------------------------------------------------------------------------
SECTION "PALETTE FUNCTIONS", ROM0

; @function		PaletteInitialise
; @description	Initialise all palette data to default colour, only use in VBlank
; @registers	[ a, b, c, d, e, f ] : NOT_PRESERVED
PaletteInitialise::
	ld de, %0111111111111111						; default colour
	ld hl, shadow_bg_palette						; destination
	ld b, 4 * 8 * 2									; 4 colours x 8 palettes x 2 bytes
	call MemSetWideSmall							; call MemSetWideSmall
	ld hl, shadow_obj_palette						; destination
	ld b, 4 * 8 * 2									; 4 colours x 8 palettes x 2 bytes
	call MemSetWideSmall							; call MemSetWideSmall
	; set the gameboy colour palettes
	ld a, %10000000									; auto incrementing, palette address 0
	ldh [rBCPS], a									; set this to the background colour palette specification
	ldh [rOCPS], a									; set this to the object colour palette specification
	ld b, 32										; set register b to 32 colours
	ld c, LOW( rBCPD )								; might aswell set one of them faster
.palette_loop
	ld a, e											; GGGRRRRR
	ldh [c], a										; place it in bg palette memory
	ldh [rOCPD], a									; place it in obj palette memory
	ld a, d											; xBBBBBGG
	ldh [c], a										; place it in bg palette memory
	ldh [rOCPD], a									; place it in obj palette memory
	dec b											; decrease by 1
	jr nz, .palette_loop							; check if all palettes are done
	ret

; ------------------------------------------------------------------------
; palette functions [shadow]
; ------------------------------------------------------------------------
SECTION "PALETTE FUNCTIONS SHADOW", ROM0

; @function		SetBGPaletteToColourShadow
; @description	Set background palette to a colour
; @param [a]	palette index to start at	[0-7]
; @param [b]	colour count				[1-32]
; @param [de]	colour						[xBBBBBGGGGGRRRRR]
; @registers	[ a, b, d, e, h, l, f ] : NOT_PRESERVED
SetBGPaletteToColourShadow::
	rlca											; palette index * 2
	rlca											; palette index * 2
	rlca											; palette index * 2 again for * 8 (4 colours per palette and 2 bytes per colour)
	hla_add shadow_bg_palette						; offset to palette
.palette_loop
	ld a, d											; load register a with GGGRRRRR
	ldi [hl], a										; place it in palette shadow memory [ GGGRRRRR ]
	ld a, e											; load register a with xBBBBBGG
	ldi [hl], a										; place it in palette shadow memory [ xBBBBBGG ]
	dec b											; decrease by 1
	jr nz, .palette_loop							; check if all palettes are done
	ret

; @function		SetBGPaletteShadow
; @description	Set background palette data
; @param [a]	palette index to start at	[0-7]
; @param [b]	colour count				[1-32]
; @param [hl]	palette data
; @registers	[ a, b, d, e, h, l, f ] : NOT_PRESERVED
SetBGPaletteShadow::
	rlca											; palette index * 2
	rlca											; palette index * 2
	rlca											; palette index * 2 again for * 8 (4 colours per palette and 2 bytes per colour)
	dea_add shadow_bg_palette						; offset to palette
.palette_loop
	ldi a, [hl]										; load byte from palette data into register a [ GGGRRRRR ]
	ld [de], a										; place it in palette shadow memory
	inc de											; move next byte
	ldi a, [hl]										; load byte from palette data into register a [ xBBBBBGG ]
	ld [de], a										; place it in palette shadow memory
	inc de											; move next byte
	dec b											; decrease by 1
	jr nz, .palette_loop							; check if all palettes are done
	ret

; @function		SetBGPaletteColourShadow
; @description	Set a single background palette colour
; @param [a]	palette index to start at	[0-7]
; @param [b]	colour offset				[0-3]
; @param [hl]	colour
; @registers	[ a, b, d, e, h, l, f ] : NOT_PRESERVED
SetBGPaletteColourShadow::
	rlca											; palette index * 2
	rlca											; palette index * 2
	rlca											; palette index * 2 again for * 8 (4 colours per palette and 2 bytes per colour)
	add a, b										; add b to a twice
	add a, b										; so the offset is doubled ( 2 bytes per colour )
	dea_add shadow_bg_palette						; offset to palette
	ldi a, [hl]										; load byte from palette data into register a [ GGGRRRRR ]
	ld [de], a										; place it in palette shadow memory
	inc de											; move next byte
	ldi a, [hl]										; load byte from palette data into register a [ xBBBBBGG ]
	ld [de], a										; place it in palette shadow memory
	ret

; @function		SetObjPaletteShadow
; @description	Set object palette data
; @param [a]	palette index to start at	[0-7]
; @param [b]	colour count				[1-32]
; @param [hl]	palette data
; @registers	[ a, b, d, e, h, l, f ] : NOT_PRESERVED
SetObjPaletteShadow::
	rlca											; palette index * 2
	rlca											; palette index * 2
	rlca											; palette index * 2 again for * 8 (4 colours per palette and 2 bytes per colour)
	dea_add shadow_obj_palette						; offset to palette
.palette_loop
	ldi a, [hl]										; load byte from palette data into register a [ GGGRRRRR ]
	ld [de], a										; place it in palette shadow memory
	inc de											; move next byte
	ldi a, [hl]										; load byte from palette data into register a [ xBBBBBGG ]
	ld [de], a										; place it in palette shadow memory
	inc de											; move next byte
	dec b											; decrease by 1
	jr nz, .palette_loop							; check if all palettes are done
	ret

; @function		SetObjPaletteColourShadow
; @description	Set a single object palette colour
; @param [a]	palette index to start at	[0-7]
; @param [b]	colour offset				[0-3]
; @param [hl]	palette data
; @registers	[ a, b, d, e, h, l, f ] : NOT_PRESERVED
SetObjPaletteColourShadow::
	rlca											; palette index * 2
	rlca											; palette index * 2
	rlca											; palette index * 2 again for * 8 (4 colours per palette and 2 bytes per colour)
	add a, b										; add b to a twice
	add a, b										; so the offset is doubled ( 2 bytes per colour )
	dea_add shadow_obj_palette						; offset to palette
	ldi a, [hl]										; load byte from palette data into register a [ GGGRRRRR ]
	ld [de], a										; place it in palette shadow memory
	inc de											; move next byte
	ldi a, [hl]										; load byte from palette data into register a [ xBBBBBGG ]
	ld [de], a										; place it in palette shadow memory
	ret

; @function		CopyCRAMToShadowPalette
; @description	Copy CRAM to shadow palette
; @param [a]	specification, eg. BCPSF_AUTOINC or OCPSF_AUTOINC
; @param [c]	low byte of ( rBCPD or rOCPD )
; @param [hl]	target_shadow_palette
; @registers	[ a, b, h, l, f ] : NOT_PRESERVED
CopyCRAMToShadowPalette::
	ldh [rBCPS], a									; specificatiom
	ld b, 32										; 32 colours
.loop
	ldh a, [c]										; read GGGRRRRR
	ldh [c], a										; write back to advance it
	ldi [hl], a										; save into shadow palette
	ldh a, [c]										; read xBBBBBGG
	ldh [c], a										; write back to advance it
	ldi [hl], a										; save into shadow palette
	dec b											; decrease colours left
	jr nz, .loop									; check if its finished
	ret

; @function		CopyBGCRAMToShadowPalette
; @description	Copy background CRAM to shadow palette
; @registers	[ a, b, h, l, f ] : NOT_PRESERVED
CopyBGCRAMToShadowPalette::
	ld a, BCPSF_AUTOINC								; auto increment
	ld c, LOW( rBCPD )								; low byte of rOCPD
	ld hl, shadow_bg_palette						; point to shadow_obj_palette
	jr CopyCRAMToShadowPalette						; jump to CopyCRAMToShadowPalette

; @function		CopyOBJCRAMToShadowPalette
; @description	Copy object CRAM to shadow palette
; @registers	[ a, b, h, l, f ] : NOT_PRESERVED
CopyOBJCRAMToShadowPalette::
	ld a, OCPSF_AUTOINC								; auto increment
	ld c, LOW( rOCPD )								; low byte of rOCPD
	ld hl, shadow_obj_palette						; point to shadow_obj_palette
	jr CopyCRAMToShadowPalette						; jump to CopyCRAMToShadowPalette

; ------------------------------------------------------------------------
; palette functions direct
; ------------------------------------------------------------------------
SECTION "PALETTE FUNCTIONS DIRECT", ROM0			; modify the palette directly

; @function		SetBGPaletteToColourDirect
; @description	Set background palette to a colour
;				Direct version, only use in VBlank
; @param [a]	palette index to start at	[0-7]
; @param [b]	colour count				[1-32]
; @param [de]	colour						[xBBBBBGGGGGRRRRR]
; @registers	[ a, b, c, d, e, f ] : NOT_PRESERVED
SetBGPaletteToColourDirect::
	rlca											; palette index * 2
	rlca											; palette index * 2
	rlca											; palette index * 2 again for * 8 (4 colours per palette and 2 bytes per colour)
	or a, BCPSF_AUTOINC								; add auto incrementing to the palette address
	ldh [rBCPS], a									; set this to the address, after that rBCPD can be used to write to
	ld c, LOW( rBCPD )								; low byte of address rBCPD
.palette_loop
	ld a, d											; load register a with GGGRRRRR
	ldh [c], a										; place it in palette memory [ GGGRRRRR ]
	ld a, e											; load register a with xBBBBBGG
	ldh [c], a										; place it in palette memory [ xBBBBBGG ]
	dec b											; decrease by 1
	jr nz, .palette_loop							; check if all palettes are done
	ret

; @function		SetBGPaletteDirect
; @description	Set background palette data
;				Direct version, only use in VBlank
; @param [a]	palette index to start at	[0-7]
; @param [b]	colour count				[1-32]
; @param [hl]	palette data
; @registers	[ a, b, c, h, l, f ] : NOT_PRESERVED
SetBGPaletteDirect::
	rlca											; palette index * 2
	rlca											; palette index * 2
	rlca											; palette index * 2 again for * 8 (4 colours per palette and 2 bytes per colour)
	or a, BCPSF_AUTOINC								; add auto incrementing to the palette address
	ldh [rBCPS], a									; set this to the address, after that rBCPD can be used to write to
	ld c, LOW( rBCPD )								; low byte of address rBCPD
.palette_loop
	ldi a, [hl]										; load byte from palette data into register a [ GGGRRRRR ]
	ldh [c], a										; place it in palette memory
	ldi a, [hl]										; load byte from palette data into register a [ xBBBBBGG ]
	ldh [c], a										; place it in palette memory
	dec b											; decrease by 1
	jr nz, .palette_loop							; check if all palettes are done
	ret

; @function		SetBGPaletteColourDirect
; @description	Set a single background palette colour
;				Direct version, only use in VBlank
; @param [a]	palette index to start at	[0-7]
; @param [b]	colour offset				[0-3]
; @param [hl]	palette data
; @registers	[ a, b, h, l, f ] : NOT_PRESERVED
SetBGPaletteColourDirect::
	rlca											; palette index * 2
	rlca											; palette index * 2
	rlca											; palette index * 2 again for * 8 (4 colours per palette and 2 bytes per colour)
	add a, b										; add b to a twice
	add a, b										; so the offset is doubled ( 2 bytes per colour )
	or a, BCPSF_AUTOINC								; add auto incrementing to the palette address
	ldh [rBCPS], a									; set this to the address, after that rBCPD can be used to write to
	ldi a, [hl]										; load byte from palette data into register a [ GGGRRRRR ]
	ldh [rBCPD], a									; place it in palette memory
	ldi a, [hl]										; load byte from palette data into register a [ xBBBBBGG ]
	ldh [rBCPD], a									; place it in palette memory
	ret

; @function		SetObjPaletteDirect
; @description	Set object palette data
;				Direct version, only use in VBlank
; @param [a]	palette index to start at	[0-7]
; @param [b]	colour count				[1-32]
; @param [hl]	palette data
; @registers	[ a, b, c, h, l, f ] : NOT_PRESERVED
SetObjPaletteDirect::
	rlca											; palette index * 2
	rlca											; palette index * 2
	rlca											; palette index * 2 again for * 8 (4 colours per palette and 2 bytes per colour)
	or a, OCPSF_AUTOINC								; add auto incrementing to the palette address
	ldh [rOCPS], a									; set this to the address, after that rOCPD can be used to write to
	ld c, LOW( rOCPD )								; low byte of address rOCPD
.palette_loop
	ldi a, [hl]										; load byte from palette data into register a [ GGGRRRRR ]
	ldh [c], a										; place it in palette memory
	ldi a, [hl]										; load byte from palette data into register a [ xBBBBBGG ]
	ldh [c], a										; place it in palette memory
	dec b											; decrease by 1
	jr nz, .palette_loop							; check if all palettes are done
	ret

; @function		SetObjPaletteColourDirect
; @description	Set a single object palette colour
;				Direct version, only use in VBlank
; @param [a]	palette index to start at	[0-7]
; @param [b]	colour offset				[0-3]
; @param [hl]	palette data
; @registers	[ a, b, h, l, f ] : NOT_PRESERVED
SetObjPaletteColourDirect::
	rlca											; palette index * 2
	rlca											; palette index * 2
	rlca											; palette index * 2 again for * 8 (4 colours per palette and 2 bytes per colour)
	add a, b										; add b to a twice
	add a, b										; so the offset is doubled ( 2 bytes per colour )
	or a, OCPSF_AUTOINC								; add auto incrementing to the palette address
	ldh [rOCPS], a									; set this to the address, after that rOCPD can be used to write to
	ldi a, [hl]										; load byte from palette data into register a [ GGGRRRRR ]
	ldh [rOCPD], a									; place it in palette memory
	ldi a, [hl]										; load byte from palette data into register a [ xBBBBBGG ]
	ldh [rOCPD], a									; place it in palette memory
	ret