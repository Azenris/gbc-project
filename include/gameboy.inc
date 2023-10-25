
IF !DEF( GAMEBOY_INC )
DEF GAMEBOY_INC EQU 1

INCLUDE "include/hardware.inc"
INCLUDE "include/macros.inc"
INCLUDE "include/render.inc"
INCLUDE "include/sound.inc"

; ------------------------------------------------------------------------
; BANKS																		; Fixed bank locations
; ------------------------------------------------------------------------
DEF DIALOGUE_BANK							EQU		1						; dialogue bank
DEF UI_BANK									EQU		2						; ui bank
DEF SOUND_BANK								EQU		3						; sound bank

; ------------------------------------------------------------------------
; INPUTS
; ------------------------------------------------------------------------
DEF BUTTON_A								EQU		%00000001				; button a
DEF BUTTON_B								EQU		%00000010				; button b
DEF BUTTON_SELECT							EQU		%00000100				; button select
DEF BUTTON_START							EQU		%00001000				; button start
DEF BUTTON_RIGHT							EQU		%00010000				; d-pad button right
DEF BUTTON_LEFT								EQU		%00100000				; d-pad button left
DEF BUTTON_UP								EQU		%01000000				; d-pad button up
DEF BUTTON_DOWN								EQU		%10000000				; d-pad button down

DEF BUTTON_SKIP								EQU		( BUTTON_A | BUTTON_B | BUTTON_START | BUTTON_SELECT )

; ------------------------------------------------------------------------
; GRAPHICS
; ------------------------------------------------------------------------
DEF TILEMAP_DATA_9800						EQU		$9800					; tilemap
DEF TILEMAP_DATA_9C00						EQU		$9C00					; window

DEF TILE_VRAM_DATA_8000						EQU		$8000					; $8000-$8FFF
DEF TILE_VRAM_DATA_8800						EQU		$8800					; $8800-97FFF
DEF TILE_VRAM_DATA_8800_TOP					EQU		$8800					; $8800-97FFF
DEF TILE_VRAM_DATA_8800_BOTTOM				EQU		$8800 + 128 * 16		; $8800-97FFF

DEF DEFAULT_INTERRUPT_FLAGS					EQU		( IEF_STAT | IEF_VBLANK )
DEF DEFAULT_LCD_FLAGS						EQU		( LCDCF_ON | LCDCF_WIN9C00 | LCDCF_BG8800 | LCDCF_BG9800 | LCDCF_OBJ16 | LCDCF_OBJON | LCDCF_BGON )

DEF STACK_USING_WRAM_BANKS					EQU		$D000					; WRAM bank used
DEF STACK_NOT_USING_WRAM_BANKS				EQU		$E000					; WRAM bank not used

; ------------------------------------------------------------------------
; SCREEN
; ------------------------------------------------------------------------
DEF SCREEN_METATILE_WIDTH					EQU		10						; width of screen in metatiles
DEF SCREEN_METATILE_HEIGHT					EQU		8						; height of screen in metatiles, -1 because ui is 16 pixels
DEF SCREEN_TILE_WIDTH						EQU		20						; width of screen in tiles
DEF SCREEN_TILE_HEIGHT						EQU		18						; height of screen in tiles
DEF SCREEN_WIDTH							EQU		160						; width of screen in pixels
DEF SCREEN_HEIGHT							EQU		144						; height of screen in pixels
DEF SCREEN_DIMENSIONS						EQU		( ( 20 << 8 ) | 18 )	; can be used like ld bc, SCREEN_DIMENSIONS

RSRESET
DEF SCREEN_DATA_PALETTE						RB		2						; palette data
DEF SCREEN_DATA_PALETTE_SIZE				RB		1						; palette data bytes
DEF SCREEN_DATA_TILEMAP						RB		2						; tilemap data
DEF SCREEN_DATA_TILEMAP_ATTRIBUTES			RB		2						; tilemap attributes data
DEF SCREEN_DATA_TILES_VRAM0					RB		2						; tiles data for vram bank 0
DEF SCREEN_DATA_TILES_VRAM0_BLOCKS			RB		1						; tiles data 16 byte blocks - 1
DEF SCREEN_DATA_TILES_VRAM1					RB		2						; tiles data for vram bank 1
DEF SCREEN_DATA_TILES_VRAM1_BLOCKS			RB		1						; tiles data 16 byte blocks - 1

; ------------------------------------------------------------------------
; UTILITY
; ------------------------------------------------------------------------
DEF NO_CALL									EQU		$E0						; Invalid Call, Echo memory, can just check high byte
DEF NoCall									EQU		$E000					; Used to setup dw address calls

ENDC; GAMEBOY_INC