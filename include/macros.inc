
IF !DEF( MACROS_INC )
DEF MACROS_INC EQU 1

INCLUDE "include/hardware.inc"

; ------------------------------------------------------------------------
; BUILD OPTIONS
; ------------------------------------------------------------------------
DEF BUILD_VERSION_MAJOR						EQU		0					; build version major
DEF BUILD_VERSION_MINOR						EQU		1					; build version minor
DEF BUILD_GBC_ONLY							EQU		1					; set is gbc only build
DEF BUILD_DEBUG								EQU		1					; debugging is enabled
;DEF BUILD_NORMALISE_PLAYER_MOVEMENT		EQU		1					; move slower diagonally

; ------------------------------------------------------------------------
; MACROS
; ------------------------------------------------------------------------

; @description	Load de to an n16 using n8
; @param [1]	high
; @param [2]	low
MACRO de_n16
	ASSERT _NARG == 2, "high, low"
	ASSERT ( ( \1 ) >= 0 && ( \1 ) <= 255 ), "High is not between 0 and 255"
	ASSERT ( ( \2 ) >= 0 && ( \2 ) <= 255 ), "Low is not between 0 and 255"
	ld de, ( ( ( \1 ) << 8 ) | ( \2 ) )
ENDM

; @description	Load hl to an n16 using n8
; @param [1]	high
; @param [2]	low
MACRO hl_n16
	ASSERT _NARG == 2, "high, low"
	ASSERT ( ( \1 ) >= 0 && ( \1 ) <= 255 ), "High is not between 0 and 255"
	ASSERT ( ( \2 ) >= 0 && ( \2 ) <= 255 ), "Low is not between 0 and 255"
	ld hl, ( ( ( \1 ) << 8 ) | ( \2 ) )
ENDM

; @description	Create a dw colour
; @param [1]	red				[0-31]
; @param [2]	green			[0-31]
; @param [3]	blue			[0-31]
MACRO RGB
	ASSERT _NARG == 3, "red, green, blue"
	ASSERT ( ( \1 ) >= 0 && ( \1 ) <= 31 ), "Red is not between 0 and 31"
	ASSERT ( ( \2 ) >= 0 && ( \2 ) <= 31 ), "Green is not between 0 and 31"
	ASSERT ( ( \3 ) >= 0 && ( \3 ) <= 31 ), "Blue is not between 0 and 31"

	dw ( ( \3 ) << 10 ) | ( ( \2 ) << 5 ) | ( \1 )
ENDM

; @description	Create 4 dw's for a palette
MACRO PAL
	ASSERT _NARG == 12, "( red, green, blue ) x4"
	REPT 4
		RGB \1, \2, \3
		SHIFT 3
	ENDR
ENDM

; @description	Create a dw colour from rgb8bits
; @param [1]	red				[0-255]
; @param [2]	green			[0-255]
; @param [3]	blue			[0-255]
MACRO RGB255
	ASSERT _NARG == 3, "red, green, blue"
	ASSERT ( ( \1 ) >= 0 && ( \1 ) <= 255 ), "Red is not between 0 and 255"
	ASSERT ( ( \2 ) >= 0 && ( \2 ) <= 255 ), "Green is not between 0 and 255"
	ASSERT ( ( \3 ) >= 0 && ( \3 ) <= 255 ), "Blue is not between 0 and 255"

	dw ( ( ( ( ( \3 ) / 255 ) * 31 ) << 10 ) | ( ( ( ( \2 ) / 255 ) * 31 ) << 5 ) | ( ( ( \1 ) / 255 ) * 31 )
ENDM

; @description	Create 4 dw's for a palette
MACRO PAL225
	ASSERT _NARG == 12, "( red, green, blue ) x4"
	REPT 4
		RGB255 \1, \2, \3
		SHIFT 3
	ENDR
ENDM

; @description	Push the current rom bank to stack
; @registers	[ a ] : NOT_PRESERVED
MACRO push_current_bank
	ldh a, [g_current_bank]												; load register a with current bank
	push af																; push af to stack
ENDM

; @description	Pop the current rom bank from stack and set it
; @registers	[ a ] : NOT_PRESERVED
MACRO pop_current_bank
	pop af																; pop af from stack
	set_bank															; set the bank
ENDM

; @description	Switch to a different rom bank
; @param [a]	bank			[1-255]
; @registers	[ a ] : PRESERVED
MACRO set_bank
	ldh [g_current_bank], a
	ld [rROMB0], a
ENDM

; @description	Set the rom banks to using 1-255
; @registers	[ a ] : NOT_PRESERVED
MACRO set_low_bank_mode
	xor a																; 0 here for the 9th bit
	ld [rROMB1], a														; rROM1 contains the 9th bit of the bank
ENDM

; @description	Set the rom banks to using 256-511
; @registers	[ a ] : NOT_PRESERVED
MACRO set_high_bank_mode
	ld a, 1																; 1 here for the 9th bit
	ld [rROMB1], a														; rROM1 contains the 9th bit of the bank
ENDM

; @description	Switch to a different work RAM bank
; @param [a]	wram_bank		[1-7]
; @registers	[ a ] : PRESERVED
MACRO set_wram_bank
	ldh [g_current_wram_bank], a
	ldh [rSVBK], a
ENDM

; @description	Enable external cart ram reading/writing
;				Keep battery banked external ram closed as much as
;				possible. Losing power while open can corrupt data.
; @registers	[ a ] : NOT_PRESERVED
MACRO enable_ext_ram
	ld a, $a															; enable number
	ld [rRAMG], a														; set to enable
ENDM

; @description	Disable external cart ram reading/writing
;				Keep battery banked external ram closed as much as
;				possible. Losing power while open can corrupt data.
; @registers	[ a ] : NOT_PRESERVED
MACRO disable_ext_ram
	xor a																; disable number
	ld [rRAMG], a														; set to enable
ENDM

; @description	Set external cart ram bank
;				Maps ram into A000-BFFF range depending on size
;				Banks 0-15 (0-F)
; @registers	[ a ] : PRESERVED
MACRO set_ext_ram_bank
	ld [rRAMB], a														; set external cart ram bank
ENDM

; @description	Used for calling an address in register pair hl
; @param [hl]	16bit address
MACRO call_hl
	call CallHL
ENDM

; @description	Used for calling an address in register pair de
; @param [de]	16bit address
MACRO call_de
	call CallDE															; call CallDE
ENDM

; @description	Dereference hl ( hl = *hl )
MACRO hla_deref
	ldi a, [hl]															; hl = *hl
	ld h, [hl]															; 
	ld l, a																; 
ENDM

; @description	Copy hl into bc
MACRO hl_to_bc
	ld b, h																; copy h into b
	ld c, l																; copy l into c
ENDM

; @description	Copy bc into hl
MACRO bc_to_hl
	ld h, b																; copy b into h
	ld l, c																; copy c into l
ENDM

; @description	Copy hl into de
MACRO hl_to_de
	ld d, h																; copy h into d
	ld e, l																; copy l into e
ENDM

; @description	Copy de into hl
MACRO de_to_hl
	ld h, d																; copy d into h
	ld l, e																; copy e into l
ENDM

; @description	Add bc to de using register a
MACRO add_de_bc
	ld a, e																; load register a with low byte
	add a, c															; add c to it
	ld e, a																; put it back in e
	ld a, d																; now load high byte
	adc a, b															; add b + carry to it
	ld d, a																; put it back in d
ENDM

; @description	Add bc to de using register a
MACRO add_bc_de
	ld a, c																; load register a with low byte
	add a, e															; add e to it
	ld c, a																; put it back in c
	ld a, b																; now load high byte
	adc a, d															; add d + carry to it
	ld b, a																; put it back in b
ENDM

MACRO hl_ptr															;												| 11,  8
	ASSERT _NARG == 2, "target,  address"
	ld hl, \1															; set current font selected to main font		|  3,  3
	ld [hl], LOW( \2 )													; set lower 8 bits								|  3,  2
	inc hl																; inc hl										|  2,  1
	ld [hl], HIGH( \2 )													; set higher 8 bits								|  3,  2
ENDM

MACRO hla_add															;												|  9,  8
	ASSERT _NARG == 1, "address"
	ld hl, \1															; assign hl										|  3,  3
	add a, l															; offset										|  1,  1
	ld l, a																; load it into l								|  1,  1
	jr nc, .no_carry\@													; check if there was a carry					|  3,  2
	inc h																; if there was, inc h							|  1,  1
.no_carry\@
ENDM

MACRO hla_offset														;												|  7,  6
	ASSERT _NARG == 1, "offset"
	ld a, \1															; load register a with offset					|  1,  1
	add a, l															; offset										|  1,  1
	ld l, a																; load it into l								|  1,  1
	jr nc, .no_carry\@													; check if there was a carry					|  3,  2
	inc h																; if there was, inc h							|  1,  1
.no_carry\@
ENDM

MACRO hla_sprite														;												| 12, 11
	ASSERT _NARG == 1, "offset"
	ld a, \1															; load register a with sprite id				|  1,  1
	rlca																; *2											|  1,  1
	rlca																; *4  ( 4 bytes per OAM entry )					|  1,  1
	hla_add g_shadow_oam												; assign g_shadow_oam to hl and offset by a		|  9,  8
ENDM

MACRO bca_add															;												|  9,  8
	ASSERT _NARG == 1, "offset"
	ld bc, \1															; assign bc										|  3,  3
	add a, c															; offset										|  1,  1
	ld c, a																; load it into c								|  1,  1
	jr nc, .no_carry\@													; check if there was a carry					|  3,  2
	inc b																; if there was, inc b							|  1,  1
.no_carry\@
ENDM

MACRO bca_offset														;												|  7,  6
	ASSERT _NARG == 1, "offset"
	ld a, \1															; load register a with offset					|  1,  1
	add a, c															; offset										|  1,  1
	ld c, a																; load it into c								|  1,  1
	jr nc, .no_carry\@													; check if there was a carry					|  3,  2
	inc b																; if there was, inc b							|  1,  1
.no_carry\@
ENDM

MACRO dea_add															;												|  9,  8
	ASSERT _NARG == 1, "offset"
	ld de, \1															; assign de										|  3,  3
	add a, e															; offset										|  1,  1
	ld e, a																; load it into e								|  1,  1
	jr nc, .no_carry\@													; check if there was a carry					|  3,  2
	inc d																; if there was, inc d							|  1,  1
.no_carry\@
ENDM

MACRO dea_offset														;												|  7,  6
	ASSERT _NARG == 1, "offset"
	ld a, \1															; load register a with offset					|  1,  1
	add a, e															; offset										|  1,  1
	ld e, a																; load it into e								|  1,  1
	jr nc, .no_carry\@													; check if there was a carry					|  3,  2
	inc d																; if there was, inc d							|  1,  1
.no_carry\@
ENDM

MACRO dea_sprite														;												| 12, 11
	ASSERT _NARG == 1, "offset"
	ld a, \1															; load register a with sprite id				|  1,  1
	rlca																; *2											|  1,  1
	rlca																; *4  ( 4 bytes per OAM entry )					|  1,  1
	dea_add g_shadow_oam												; assign g_shadow_oam to de and offset by a		|  9,  8
ENDM

MACRO a_vblank_on
	ld a, 1																; load register a with 1
	ldh [VBLANK_ENABLED], a												; set it to VBLANK_ENABLED
ENDM

MACRO a_vblank_off
	xor a																; load register a with 0
	ldh [VBLANK_ENABLED], a												; set it to VBLANK_ENABLED
ENDM

; ------------------------------------------------------------------------
; DIALOGUE MACROS
; ------------------------------------------------------------------------

; @description	Create a line of dialogue
;				Text should be no longer than DIALOGUE_MAX_STRING [16].
;				Should be no longer than DIALOGUE_MAX_BUFFER [24]
;				when including commands,
; @param [\1]	message			[text not longer than]
; @registers	[ a ] : PRESERVED
MACRO dialogue_line
	db \1
	STATIC_ASSERT STRLEN( "\1" ) <= DIALOGUE_MAX_BUFFER, "Text \1 is too long."
ENDM

; @description	Null terminate a dialogue
; @registers	[ a ] : PRESERVED
MACRO dialogue_end
	db 0
ENDM

ENDC; MACROS_INC