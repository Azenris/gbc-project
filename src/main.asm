
INCLUDE "include/gameboy.inc"

; ------------------------------------------------------------------------
; MAIN
; ------------------------------------------------------------------------
SECTION "MAIN", ROM0

Main::
	ld [g_bootup_type], a							; save some bootup information a = type
	ld a, b											; load b register into a
	ld [g_bootup_qualifier], a						; save qualifier
	ld sp, STACK_USING_WRAM_BANKS					; set stack pointer

	; we have to wait for vblank to turn the screen off
	; doing it outside vblank can cause hardware damage
.turnOffDisplay										; Wait for vblank before turning the LCD off
	ld a, [rLY]										; LCDC Y-Coordinate
	cp a, 144										; check its in vlbank
	jr c, .turnOffDisplay							; if it isn't keep checking
	xor a											; set register a to 0
	ldh [rLCDC], a									; clear all LCD Control bits, turning the screen off

	ldh [g_frame_counter], a						; default the global frame counter to 0
	ldh [rSCX], a									; default the scroll position x to 0
	ldh [rSCY], a									; default the scroll position y to 0
	ldh [SCREEN_X_POS], a							; default the scroll position x to 0
	ldh [SCREEN_Y_POS], a							; default the scroll position y to 0
	ldh [rWX], a									; default the window scroll position y to 0
	ldh [WINDOW_Y_POS], a							; default the window scroll position y to 0
	inc a											; set a 1
	ldh [VBLANK_ENABLED], a							; default VBLANK_ENABLED is true
	ld a, 7											; window x to be at the left is 7, 0-6 are unstable
	ldh [rWX], a									; default the window scroll position x to 7
	ldh [WINDOW_X_POS], a							; default the window scroll position x to 7

	call OAMInitialise								; initialise the OAM
	call PaletteInitialise							; initialise all the palettes
	call SoundInitialise							; initialise sound

	;
	; INIT_CODE_HERE <---------------
	;

; ------------------------------------------------------------------------
; MAIN_LOOP
; ------------------------------------------------------------------------
.mainloop
	call InputUpdate								; update inputs

	;
	; MAIN_LOOP_HERE <---------------
	;

	ld hl, g_frame_counter							; point to g_frame_counter
	inc [hl]										; increase it by 1
	xor a											; set register a to 0
	ld [g_sprites_assigned], a						; set sprites_assigned value to 0
	jr .mainloop									; start the whole loop again
; ------------------------------------------------------------------------
; MAIN_LOOP_END
; ------------------------------------------------------------------------

; ------------------------------------------------------------------------
; GLOBAL WORK RAM
; 2 bytes
; ------------------------------------------------------------------------
SECTION "GLOBAL WRAM", WRAM0
g_bootup_type:: db									; type BOOTUP_A_DMG or BOOTUP_A_CGB or BOOTUP_A_MGB
g_bootup_qualifier:: db								; qualifier BOOTUP_B_CGB or BOOTUP_B_AGB

; ------------------------------------------------------------------------
; GLOBAL HRAM
; 7 bytes
; ------------------------------------------------------------------------
SECTION "GLOBAL HRAM", HRAM

g_frame_counter:: db					; Global counter increments each frame from 0-255 and loops over, can be used to sync animations etc
g_key_down_previous:: db				; previous state of joypad
g_key_down:: db							; current state of joypad
g_key_pressed:: db						; state of new presses of the joypad
g_key_released:: db						; state of new releases of the joypad
g_current_bank:: db						; current bank set
g_current_wram_bank:: db				; current work ram bank set