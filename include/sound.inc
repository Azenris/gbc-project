
IF !DEF( SOUND_INC )
DEF SOUND_INC EQU 1

; ------------------------------------------------------------------------
; MACROS
; ------------------------------------------------------------------------
MACRO hl_snd													; plays a sound, uses hl (not preserved)
	ld hl, sound_table + ( \1 * 5 )								; get the sound address
	call SoundPlay												; call the sound
ENDM

; ------------------------------------------------------------------------
; SOUNDS
; ------------------------------------------------------------------------
DEF SOUND_TYPEWRITER				EQU		( 0 )				; 
DEF SOUND_TYPEWRITER_LINE_UP		EQU		( 1 )				; 
DEF SOUND_TYPEWRITER_CONTINUE		EQU		( 2 )				; 
DEF SOUND_TYPEWRITER_CLOSE			EQU		( 3 )				; 

ENDC; SOUND_INC