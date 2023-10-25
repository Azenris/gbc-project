
IF !DEF( RENDER_INC )
DEF RENDER_INC EQU 1

INCLUDE "include/hardware.inc"

; ------------------------------------------------------------------------
; RENDER
; ------------------------------------------------------------------------

DEF DEFAULT_RENDER_FLAGS					EQU		0						; default rendering flags

DEF RENDER_FLAG_V_BLANK						EQU		%00000001				; a vblank was recently fired
DEF RENDER_FLAG_REDRAW_ALL					EQU		%00000010				; force a full redraw
DEF RENDER_FLAG_REDRAW_LEFT					EQU		%00000100				; redraw the left-side of map
DEF RENDER_FLAG_REDRAW_RIGHT				EQU		%00001000				; redraw the right-side of map
DEF RENDER_FLAG_REDRAW_TOP					EQU		%00010000				; redraw top-side of map
DEF RENDER_FLAG_REDRAW_BOTTOM				EQU		%00100000				; redraw the bottom-side of map
DEF RENDER_FLAG_LOAD_OVERWORLD_UI			EQU		%01000000				; load the overworld ui tileset
DEF RENDER_FLAG_UPDATE_OVERWORLD_UI			EQU		%10000000				; draw the overworld ui

DEF bRENDER_FLAG_V_BLANK					EQU		0						; bit for RENDER_FLAG_V_BLANK
DEF bRENDER_FLAG_REDRAW_ALL					EQU		1						; bit for RENDER_FLAG_REDRAW_ALL
DEF bRENDER_FLAG_REDRAW_RIGHT				EQU		2						; bit for RENDER_FLAG_REDRAW_RIGHT
DEF bRENDER_FLAG_REDRAW_TOP					EQU		3						; bit for RENDER_FLAG_REDRAW_TOP
DEF bRENDER_FLAG_REDRAW_LEFT				EQU		4						; bit for RENDER_FLAG_REDRAW_LEFT
DEF bRENDER_FLAG_REDRAW_BOTTOM				EQU		5						; bit for RENDER_FLAG_REDRAW_BOTTOM
DEF bRENDER_FLAG_LOAD_OVERWORLD_UI			EQU		6						; bit for RENDER_FLAG_LOAD_OVERWORLD_UI
DEF bRENDER_FLAG_UPDATE_OVERWORLD_UI		EQU		7						; bit for RENDER_FLAG_UPDATE_OVERWORLD_UI

DEF RENDER_FLAG_REDRAW_EDGES				EQU (RENDER_FLAG_REDRAW_LEFT|RENDER_FLAG_REDRAW_RIGHT|RENDER_FLAG_REDRAW_TOP|RENDER_FLAG_REDRAW_BOTTOM)

ENDC; RENDER_INC