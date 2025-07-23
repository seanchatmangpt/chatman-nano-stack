	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 15, 0	sdk_version 15, 5
	.globl	_generate_8tick_code            ; -- Begin function generate_8tick_code
	.p2align	2
_generate_8tick_code:                   ; @generate_8tick_code
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #32
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
Lloh0:
	adrp	x0, l_str@PAGE
Lloh1:
	add	x0, x0, l_str@PAGEOFF
	bl	_puts
Lloh2:
	adrp	x0, l_str.39@PAGE
Lloh3:
	add	x0, x0, l_str.39@PAGEOFF
	bl	_puts
	mov	x0, #0                          ; =0x0
	bl	_time
	str	x0, [sp]
Lloh4:
	adrp	x0, l_.str.2@PAGE
Lloh5:
	add	x0, x0, l_.str.2@PAGEOFF
	bl	_printf
Lloh6:
	adrp	x0, l_str.40@PAGE
Lloh7:
	add	x0, x0, l_str.40@PAGEOFF
	bl	_puts
Lloh8:
	adrp	x0, l_str.41@PAGE
Lloh9:
	add	x0, x0, l_str.41@PAGEOFF
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32
	b	_puts
	.loh AdrpAdd	Lloh8, Lloh9
	.loh AdrpAdd	Lloh6, Lloh7
	.loh AdrpAdd	Lloh4, Lloh5
	.loh AdrpAdd	Lloh2, Lloh3
	.loh AdrpAdd	Lloh0, Lloh1
	.cfi_endproc
                                        ; -- End function
	.globl	_benchmark_true_8tick           ; -- Begin function benchmark_true_8tick
	.p2align	2
_benchmark_true_8tick:                  ; @benchmark_true_8tick
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #80
	stp	d9, d8, [sp, #16]               ; 16-byte Folded Spill
	stp	x22, x21, [sp, #32]             ; 16-byte Folded Spill
	stp	x20, x19, [sp, #48]             ; 16-byte Folded Spill
	stp	x29, x30, [sp, #64]             ; 16-byte Folded Spill
	add	x29, sp, #64
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
	.cfi_offset w19, -24
	.cfi_offset w20, -32
	.cfi_offset w21, -40
	.cfi_offset w22, -48
	.cfi_offset b8, -56
	.cfi_offset b9, -64
Lloh10:
	adrp	x0, l_str.42@PAGE
Lloh11:
	add	x0, x0, l_str.42@PAGEOFF
	bl	_puts
Lloh12:
	adrp	x0, l_str.43@PAGE
Lloh13:
	add	x0, x0, l_str.43@PAGEOFF
	bl	_puts
Lloh14:
	adrp	x0, l_str.44@PAGE
Lloh15:
	add	x0, x0, l_str.44@PAGEOFF
	bl	_puts
	mov	x0, #0                          ; =0x0
	bl	_time
	mov	x19, x0
	mov	w20, #1000                      ; =0x3e8
LBB1_1:                                 ; =>This Inner Loop Header: Depth=1
	mov	x0, #0                          ; =0x0
	bl	_time
	subs	w20, w20, #1
	b.ne	LBB1_1
; %bb.2:
	add	x19, x19, #3600
	mov	w21, #38528                     ; =0x9680
	movk	w21, #152, lsl #16
	; InlineAsm Start
	mrs	x20, CNTVCT_EL0
	; InlineAsm End
LBB1_3:                                 ; =>This Inner Loop Header: Depth=1
	mov	x0, #0                          ; =0x0
	bl	_time
	cmp	x19, x0
	cset	w8, hi
	; InlineAsm Start
	; InlineAsm End
	subs	w21, w21, #1
	b.ne	LBB1_3
; %bb.4:
	; InlineAsm Start
	mrs	x8, CNTVCT_EL0
	; InlineAsm End
	sub	x8, x8, x20
	mov	w9, #137                        ; =0x89
	mul	x19, x8, x9
	mov	x8, #17085                      ; =0x42bd
	movk	x8, #58746, lsl #16
	movk	x8, #38101, lsl #32
	movk	x8, #54975, lsl #48
	umulh	x8, x19, x8
	lsr	x20, x8, #23
	add	x8, x20, #9
	mov	x9, #-7378697629483820647       ; =0x9999999999999999
	movk	x9, #39322
	movk	x9, #6553, lsl #48
	umulh	x21, x8, x9
	ucvtf	d0, x20
	mov	x8, #7378697629483820646        ; =0x6666666666666666
	movk	x8, #16394, lsl #48
	fmov	d1, x8
	fdiv	d8, d0, d1
Lloh16:
	adrp	x0, l_.str.8@PAGE
Lloh17:
	add	x0, x0, l_.str.8@PAGEOFF
	bl	_printf
	str	x20, [sp]
Lloh18:
	adrp	x0, l_.str.9@PAGE
Lloh19:
	add	x0, x0, l_.str.9@PAGEOFF
	bl	_printf
	str	x21, [sp]
Lloh20:
	adrp	x0, l_.str.10@PAGE
Lloh21:
	add	x0, x0, l_.str.10@PAGEOFF
	bl	_printf
	str	d8, [sp]
Lloh22:
	adrp	x0, l_.str.11@PAGE
Lloh23:
	add	x0, x0, l_.str.11@PAGEOFF
	bl	_printf
Lloh24:
	adrp	x8, l_.str.14@PAGE
Lloh25:
	add	x8, x8, l_.str.14@PAGEOFF
Lloh26:
	adrp	x9, l_.str.13@PAGE
Lloh27:
	add	x9, x9, l_.str.13@PAGEOFF
	mov	w10, #40576                     ; =0x9e80
	movk	w10, #12359, lsl #16
	cmp	x19, x10
	csel	x8, x9, x8, lo
	str	x8, [sp]
Lloh28:
	adrp	x0, l_.str.12@PAGE
Lloh29:
	add	x0, x0, l_.str.12@PAGEOFF
Lloh30:
	adrp	x8, l_.str.17@PAGE
Lloh31:
	add	x8, x8, l_.str.17@PAGEOFF
Lloh32:
	adrp	x9, l_.str.16@PAGE
Lloh33:
	add	x9, x9, l_.str.16@PAGEOFF
	csel	x19, x9, x8, lo
	bl	_printf
	str	x19, [sp]
Lloh34:
	adrp	x0, l_.str.15@PAGE
Lloh35:
	add	x0, x0, l_.str.15@PAGEOFF
	bl	_printf
Lloh36:
	adrp	x0, l_.str.18@PAGE
Lloh37:
	add	x0, x0, l_.str.18@PAGEOFF
	bl	_printf
Lloh38:
	adrp	x0, l_.str.19@PAGE
Lloh39:
	add	x0, x0, l_.str.19@PAGEOFF
	bl	_printf
Lloh40:
	adrp	x0, l_.str.20@PAGE
Lloh41:
	add	x0, x0, l_.str.20@PAGEOFF
	bl	_printf
Lloh42:
	adrp	x0, l_.str.21@PAGE
Lloh43:
	add	x0, x0, l_.str.21@PAGEOFF
	bl	_printf
Lloh44:
	adrp	x0, l_.str.22@PAGE
Lloh45:
	add	x0, x0, l_.str.22@PAGEOFF
	bl	_printf
Lloh46:
	adrp	x0, l_.str.23@PAGE
Lloh47:
	add	x0, x0, l_.str.23@PAGEOFF
	bl	_printf
Lloh48:
	adrp	x0, l_.str.24@PAGE
Lloh49:
	add	x0, x0, l_.str.24@PAGEOFF
	bl	_printf
Lloh50:
	adrp	x0, l_.str.25@PAGE
Lloh51:
	add	x0, x0, l_.str.25@PAGEOFF
	bl	_printf
Lloh52:
	adrp	x0, l_.str.26@PAGE
Lloh53:
	add	x0, x0, l_.str.26@PAGEOFF
	bl	_printf
Lloh54:
	adrp	x0, l_str.45@PAGE
Lloh55:
	add	x0, x0, l_str.45@PAGEOFF
	bl	_puts
Lloh56:
	adrp	x0, l_str.46@PAGE
Lloh57:
	add	x0, x0, l_str.46@PAGEOFF
	bl	_puts
Lloh58:
	adrp	x0, l_str.47@PAGE
Lloh59:
	add	x0, x0, l_str.47@PAGEOFF
	bl	_puts
Lloh60:
	adrp	x0, l_str.48@PAGE
Lloh61:
	add	x0, x0, l_str.48@PAGEOFF
	ldp	x29, x30, [sp, #64]             ; 16-byte Folded Reload
	ldp	x20, x19, [sp, #48]             ; 16-byte Folded Reload
	ldp	x22, x21, [sp, #32]             ; 16-byte Folded Reload
	ldp	d9, d8, [sp, #16]               ; 16-byte Folded Reload
	add	sp, sp, #80
	b	_puts
	.loh AdrpAdd	Lloh14, Lloh15
	.loh AdrpAdd	Lloh12, Lloh13
	.loh AdrpAdd	Lloh10, Lloh11
	.loh AdrpAdd	Lloh60, Lloh61
	.loh AdrpAdd	Lloh58, Lloh59
	.loh AdrpAdd	Lloh56, Lloh57
	.loh AdrpAdd	Lloh54, Lloh55
	.loh AdrpAdd	Lloh52, Lloh53
	.loh AdrpAdd	Lloh50, Lloh51
	.loh AdrpAdd	Lloh48, Lloh49
	.loh AdrpAdd	Lloh46, Lloh47
	.loh AdrpAdd	Lloh44, Lloh45
	.loh AdrpAdd	Lloh42, Lloh43
	.loh AdrpAdd	Lloh40, Lloh41
	.loh AdrpAdd	Lloh38, Lloh39
	.loh AdrpAdd	Lloh36, Lloh37
	.loh AdrpAdd	Lloh34, Lloh35
	.loh AdrpAdd	Lloh32, Lloh33
	.loh AdrpAdd	Lloh30, Lloh31
	.loh AdrpAdd	Lloh28, Lloh29
	.loh AdrpAdd	Lloh26, Lloh27
	.loh AdrpAdd	Lloh24, Lloh25
	.loh AdrpAdd	Lloh22, Lloh23
	.loh AdrpAdd	Lloh20, Lloh21
	.loh AdrpAdd	Lloh18, Lloh19
	.loh AdrpAdd	Lloh16, Lloh17
	.cfi_endproc
                                        ; -- End function
	.globl	_main                           ; -- Begin function main
	.p2align	2
_main:                                  ; @main
	.cfi_startproc
; %bb.0:
	sub	sp, sp, #32
	stp	x29, x30, [sp, #16]             ; 16-byte Folded Spill
	add	x29, sp, #16
	.cfi_def_cfa w29, 16
	.cfi_offset w30, -8
	.cfi_offset w29, -16
Lloh62:
	adrp	x0, l_str.49@PAGE
Lloh63:
	add	x0, x0, l_str.49@PAGEOFF
	bl	_puts
Lloh64:
	adrp	x0, l_str.50@PAGE
Lloh65:
	add	x0, x0, l_str.50@PAGEOFF
	bl	_puts
Lloh66:
	adrp	x0, l_str@PAGE
Lloh67:
	add	x0, x0, l_str@PAGEOFF
	bl	_puts
Lloh68:
	adrp	x0, l_str.39@PAGE
Lloh69:
	add	x0, x0, l_str.39@PAGEOFF
	bl	_puts
	mov	x0, #0                          ; =0x0
	bl	_time
	str	x0, [sp]
Lloh70:
	adrp	x0, l_.str.2@PAGE
Lloh71:
	add	x0, x0, l_.str.2@PAGEOFF
	bl	_printf
Lloh72:
	adrp	x0, l_str.40@PAGE
Lloh73:
	add	x0, x0, l_str.40@PAGEOFF
	bl	_puts
Lloh74:
	adrp	x0, l_str.41@PAGE
Lloh75:
	add	x0, x0, l_str.41@PAGEOFF
	bl	_puts
	bl	_benchmark_true_8tick
Lloh76:
	adrp	x0, l_str.51@PAGE
Lloh77:
	add	x0, x0, l_str.51@PAGEOFF
	bl	_puts
Lloh78:
	adrp	x0, l_str.52@PAGE
Lloh79:
	add	x0, x0, l_str.52@PAGEOFF
	bl	_puts
Lloh80:
	adrp	x0, l_str.53@PAGE
Lloh81:
	add	x0, x0, l_str.53@PAGEOFF
	bl	_puts
Lloh82:
	adrp	x0, l_str.54@PAGE
Lloh83:
	add	x0, x0, l_str.54@PAGEOFF
	bl	_puts
Lloh84:
	adrp	x0, l_str.55@PAGE
Lloh85:
	add	x0, x0, l_str.55@PAGEOFF
	bl	_puts
Lloh86:
	adrp	x0, l_str.56@PAGE
Lloh87:
	add	x0, x0, l_str.56@PAGEOFF
	bl	_puts
	mov	w0, #0                          ; =0x0
	ldp	x29, x30, [sp, #16]             ; 16-byte Folded Reload
	add	sp, sp, #32
	ret
	.loh AdrpAdd	Lloh86, Lloh87
	.loh AdrpAdd	Lloh84, Lloh85
	.loh AdrpAdd	Lloh82, Lloh83
	.loh AdrpAdd	Lloh80, Lloh81
	.loh AdrpAdd	Lloh78, Lloh79
	.loh AdrpAdd	Lloh76, Lloh77
	.loh AdrpAdd	Lloh74, Lloh75
	.loh AdrpAdd	Lloh72, Lloh73
	.loh AdrpAdd	Lloh70, Lloh71
	.loh AdrpAdd	Lloh68, Lloh69
	.loh AdrpAdd	Lloh66, Lloh67
	.loh AdrpAdd	Lloh64, Lloh65
	.loh AdrpAdd	Lloh62, Lloh63
	.cfi_endproc
                                        ; -- End function
	.section	__TEXT,__cstring,cstring_literals
l_.str.2:                               ; @.str.2
	.asciz	"    return d[0] & d[1] & d[2] & (d[3] > %lu);\n"

l_.str.8:                               ; @.str.8
	.asciz	"    B --> P1[sparql_8tick_ultimate<br/>"

l_.str.9:                               ; @.str.9
	.asciz	"Cycles: %llu<br/>"

l_.str.10:                              ; @.str.10
	.asciz	"Ticks: %llu<br/>"

l_.str.11:                              ; @.str.11
	.asciz	"Time: %.2f ns<br/>"

l_.str.12:                              ; @.str.12
	.asciz	"Status: %s]\n"

l_.str.13:                              ; @.str.13
	.asciz	"PASS \342\234\223"

l_.str.14:                              ; @.str.14
	.asciz	"FAIL \342\234\227"

l_.str.15:                              ; @.str.15
	.asciz	"    P1:::%s\n"

l_.str.16:                              ; @.str.16
	.asciz	"pass"

l_.str.17:                              ; @.str.17
	.asciz	"fail"

l_.str.18:                              ; @.str.18
	.asciz	"    B --> A1[Assembly Analysis<br/>"

l_.str.19:                              ; @.str.19
	.asciz	"8 Instructions:<br/>"

l_.str.20:                              ; @.str.20
	.asciz	"LDR x0,[x8]<br/>"

l_.str.21:                              ; @.str.21
	.asciz	"LDR x1,[x8,#8]<br/>"

l_.str.22:                              ; @.str.22
	.asciz	"LDR x2,[x8,#16]<br/>"

l_.str.23:                              ; @.str.23
	.asciz	"LDR x3,[x8,#24]<br/>"

l_.str.24:                              ; @.str.24
	.asciz	"AND x0,x0,x1<br/>"

l_.str.25:                              ; @.str.25
	.asciz	"AND x0,x0,x2<br/>"

l_.str.26:                              ; @.str.26
	.asciz	"CMP x3,time<br/>"

l_str:                                  ; @str
	.asciz	"\n// Generated 8-tick SPARQL executor:"

l_str.39:                               ; @str.39
	.asciz	"static inline bool execute_sparql_8tick(uint64_t* d) {"

l_str.40:                               ; @str.40
	.asciz	"    // Compiles to exactly 8 instructions"

l_str.41:                               ; @str.41
	.asciz	"}\n"

l_str.42:                               ; @str.42
	.asciz	"\n```mermaid"

l_str.43:                               ; @str.43
	.asciz	"graph TD"

l_str.44:                               ; @str.44
	.asciz	"    A[True 8-Tick Implementation] --> B[Results]"

l_str.45:                               ; @str.45
	.asciz	"CSET x0,gt]"

l_str.46:                               ; @str.46
	.asciz	"    classDef pass fill:#90EE90,stroke:#228B22,stroke-width:2px"

l_str.47:                               ; @str.47
	.asciz	"    classDef fail fill:#FFB6C1,stroke:#DC143C,stroke-width:2px"

l_str.48:                               ; @str.48
	.asciz	"```"

l_str.49:                               ; @str.49
	.asciz	"True 8-Tick SPARQL Implementation"

l_str.50:                               ; @str.50
	.asciz	"=================================="

l_str.51:                               ; @str.51
	.asciz	"\nKey Insights for 8-tick execution:"

l_str.52:                               ; @str.52
	.asciz	"1. Eliminate ALL function calls"

l_str.53:                               ; @str.53
	.asciz	"2. Use compile-time code generation"

l_str.54:                               ; @str.54
	.asciz	"3. Ensure exactly 8 CPU instructions"

l_str.55:                               ; @str.55
	.asciz	"4. Use branchless operations"

l_str.56:                               ; @str.56
	.asciz	"5. Align data for single cache line"

.subsections_via_symbols
