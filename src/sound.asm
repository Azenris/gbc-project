
INCLUDE "include/gameboy.inc"

; ------------------------------------------------------------------------
; sound
; ------------------------------------------------------------------------
SECTION "SOUND ROM0", ROM0

; @function		SoundInitialise
; @description	Initialise sound system
; @registers	[ a ] : NOT_PRESERVED
SoundInitialise::
	jp SoundEnable									; enable sound

; @function		SoundEnable
; @description	Turn sound on
; @registers	[ a ] : NOT_PRESERVED
SoundEnable::
	ld a, $80
	ldh [rNR52], a									; enable sound
	ld a, $77
	ldh [rNR50], a									; set volume of both l/r channel to max
	ld a, $FF
	ldh [rNR51], a									; set which channels to use
	ret

; @function		SoundDisable
; @description	Turn sound off
; @registers	[ a ] : NOT_PRESERVED
SoundDisable::
	xor a											; set register a to 0
	ldh [rNR52], a									; disable sound
	ret

; @function		SoundPlay
; @description	Play a basic sound
;				Bank Swaps : PRESERVED
; @param [hl]	sound_id
; @registers	[ h, l ] : NOT_PRESERVED, [ a, f ] : PRESERVED
SoundPlay::
	push af											; save af
	push_current_bank								; push current bank to stack
	ld a, SOUND_BANK								; load sound bank into register a
	set_bank										; change the bank
	call SoundPlay_Basic							; load the basic sound
	pop_current_bank								; pop current bank from stack
	pop af											; pop af
	ret

; ------------------------------------------------------------------------
; sound banked
; ------------------------------------------------------------------------
SECTION "SOUND ROMX", ROMX, BANK[ SOUND_BANK ]

; @function		SoundPlay_Basic
; @description	Play a basic sound
; @param [hl]	sound_data
; @registers	[ a, h, l ] : NOT_PRESERVED
SoundPlay_Basic:
	ldi a, [hl]
	ldh [rNR10], a
	ldi a, [hl]
	ldh [rNR11], a
	ldi a, [hl]
	ldh [rNR12], a
	ldi a, [hl]
	ldh [rNR13], a
	ldi a, [hl]
	ldh [rNR14], a
	ret

; ------------------------------------------------------------------------
; sound data
; ------------------------------------------------------------------------
SECTION "SOUND DATA ROMX", ROMX, BANK[ SOUND_BANK ]

sound_table::
	db $1b, $88, $43, $73, $86						; SOUND_TYPEWRITER
	db $1b, $88, $43, $73, $86						; SOUND_TYPEWRITER_LINE_UP
	db $79, $8d, $63, $c8, $80						; SOUND_TYPEWRITER_CONTINUE
	db $15, $96, $73, $bb, $85						; SOUND_TYPEWRITER_CLOSE
.end