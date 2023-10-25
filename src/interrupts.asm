
INCLUDE "include/gameboy.inc"

; ------------------------------------------------------------------------
; interrupts
; ------------------------------------------------------------------------
SECTION "Interrupt_Vblank", ROM0[$0040]
	push af
	jp VBlank_Handler

SECTION "Interrupt_LCDC", ROM0[$0048]
	reti

SECTION "Interrupt_Timer", ROM0[$0050]
	reti

SECTION "Interrupt_Serial", ROM0[$0058]
	reti

SECTION "Interrupt_Joypad", ROM0[$0060]
	reti

SECTION "Interupt_Handlers", ROM0

; @function		VBlank_Handler
; @description	Called when the VBlank triggers
; @registers	[ a, f ] : PRESERVED
VBlank_Handler:
	ld a, [g_render_flag]				; get current render flags
	set bRENDER_FLAG_V_BLANK, a			; set bRENDER_FLAG_V_BLANK bit
	ld [g_render_flag], a				; set the render flag

	ldh a, [VBLANK_ENABLED]				; load register a with VBLANK_ENABLED
	rrca								; rotate it to the right to get the first bit in the carry
	jr nc, .skip_vblank					; if no-carry vblank is skipped

	ldh a, [LCDC]						; load the shadow register LCDC into register a
	or a, LCDCF_ON						; screen is always kept on
	ldh [rLCDC], a						; load a into the real register rLCDC
	ldh a, [SCREEN_X_POS]				; load the shadow register SCREEN_X_POS into register a
	ldh [rSCX], a						; load a into the real register rSCX
	ldh a, [SCREEN_Y_POS]				; load the shadow register SCREEN_Y_POS into register a
	ldh [rSCY], a						; load a into the real register rSCY
	ldh a, [WINDOW_X_POS]				; load the shadow register WINDOW_X_POS into register a
	ldh [rWX], a						; load a into the real register rSCX
	ldh a, [WINDOW_Y_POS]				; load the shadow register WINDOW_Y_POS into register a
	ldh [rWY], a						; load a into the real register rSCY

	ld a, HIGH( g_shadow_oam )			; only high byte required, $00 assumed for low byte, we align g_shadow_oam so its ok :]
	call HRAM_OAM_DMA					; copy shadow OAM memory to real location, HRAM_OAM_DMA uses register a

.skip_vblank
	pop af
	reti