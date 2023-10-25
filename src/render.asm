
INCLUDE "include/gameboy.inc"

; ------------------------------------------------------------------------
; WRAM0
; 1 bytes
; ------------------------------------------------------------------------
SECTION "RENDER WRAM0", WRAM0

g_render_flag:: db										; various flags RENDER_FLAG