
INCLUDE "include/gameboy.inc"

; ------------------------------------------------------------------------
; sprite
; ------------------------------------------------------------------------
SECTION "SPRITE", ROM0

; @function		NewSpriteID
; @description	Get a free sprite id
;				Will remove it from free_sprite_ids
; @return [a]	spriteID
; @registers	[ a, h, l, f ] : NOT_PRESERVED
NewSpriteID::
	ld hl, g_sprites_assigned						; point to free_sprite_ids
	ld a, [hl]										; get the current id value
	cp a, OAM_COUNT - 1								; check if against OAM_COUNT
	jr nc, .no_ids_left								; check there are still ids left
	inc [hl]										; increase the value for next time
	ret
.no_ids_left
	ld a, OAM_COUNT - 1								; if there are no ids avilable, it returns the last position
	ret

; ------------------------------------------------------------------------
; sprite
; ------------------------------------------------------------------------
SECTION "SPRITE WRAM", WRAM0

g_sprites_assigned:: db								; current sprite assigned this frame

; ------------------------------------------------------------------------
; OAM
; ------------------------------------------------------------------------
SECTION "OAM", ROM0

; @function		OAMInitialise
; @description	Setup OAM data, and copy OAM_DMA to HRAM
; @registers	[ a, b, h, l, f ] : NOT_PRESERVED
OAMInitialise::
	ld  hl, OAM_DMA_ROM								; point to OAM_DMA_ROM
	ld  b, OAM_DMA_ROM.end - OAM_DMA_ROM			; total bytes of the routine
	ld  c, LOW( HRAM_OAM_DMA )						; low byte of the destination address
.oam_dma_routine_copy
	ldi  a, [hl]									; load a with a byte and increment hl
	ldh [c], a										; ldh [c], a = will use c as low byte and assume $FF for the high byte, write a to that addr
	inc c											; increase c by 1 to move to next byte
	dec b											; decrease bytes remaining to copy
	jr nz, .oam_dma_routine_copy					; check if there are more bytes left

	ld hl, g_shadow_oam								; point to g_shadow_oam
	ld b, OAM_COUNT									; size of OAM
.oam_clear_loop
	ld a, $f4										; set register a to an offscreen value
	ldi [hl], a										; set the y-value to offscreen
	xor a											; set register a to 0
	ldi [hl], a										; set x-value to 0
	ldi [hl], a										; set tile-id to 0
	ldi [hl], a										; set flags to 0
	dec b											; decrease the byte counter (dec r16 won't affect the flags')
	jr nz, .oam_clear_loop

	ld a, HIGH( g_shadow_oam )						; only high byte required, $00 assumed for low byte, we align g_shadow_oam so its ok :]
	call HRAM_OAM_DMA								; copy shadow OAM memory to real location, HRAM_OAM_DMA uses register a

	xor a											; set register a with 0
	ld [g_sprites_assigned], a						; set g_sprites_assigned value to 0
	ret

; ------------------------------------------------------------------------
; OAM_DMA ROM VERSION : Don't use this version, this is just to copy to HRAM
; ------------------------------------------------------------------------
SECTION "OAM DMA ROM", ROM0

; @function		OAM_DMA_ROM
; @description	Starts a transfer and waits for completion
;				This function is copied into HRAM
;				DMA Copies data to $FE00-$FE9F
; @param [a]	start address (high byte only, second byte assumed to be $00, which is why g_shadow_oam is ALIGN[8] )	range $00-$F1
; @registers	[ a, f ] : NOT_PRESERVED
OAM_DMA_ROM:
	ldh [rDMA], a									; starts the transfer with address a
	ld a, 40										; set register a to 40
.oam_dma_wait										; loop for 160 microseconds, the exact time needed to transfer the OAM
	dec a											; decrease register a		[1 cycle]
	jr nz, .oam_dma_wait							; if not 0, keep looping	[3 cycles]
	ret												; 4x40 cycles, approx 160 microseconds
.end												; used to calculate the HRAM space needed

; ------------------------------------------------------------------------
; HRAM OAM FUNCTION	: Use this version to copy data to OAM
; 8 bytes of HRAM
; ------------------------------------------------------------------------
SECTION "OAM DMA HRAM", HRAM

HRAM_OAM_DMA::
	ds OAM_DMA_ROM.end - OAM_DMA_ROM				; reserve enough space in HRAM to fit the OAM_DMA_ROM function

; ------------------------------------------------------------------------
; SHADOW OAM - use this instead of the direct OAM, this will be copied over in vblank
; ------------------------------------------------------------------------
SECTION "SHADOW OAM", WRAM0, ALIGN[ 8 ]

; this is a copy of the OAM but in RAM, this should be modified
; instead of doing it directly, this will get copied over during vblank
; each sprite entry is 4 bytes
g_shadow_oam:: ds OAM_COUNT * 4