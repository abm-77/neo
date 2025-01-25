.section .text
	.global add

add:
add_entry_0:
stp fp, lr, [sp, -#16]!
mov fp, sp
b add_loop_preheader_0

add_loop_preheader_0:
mov x2, x0
mov x0, #0
b add_loop_header_0

add_loop_header_0:
cmp x0, #10
cset x5, LT
cbz x5, add_loop_end_0

add_loop_0:
add x3, x2, #1
add x4, x0, #1
mov x2, x3
mov x0, x4
b add_loop_header_0

add_loop_end_0:
add x1, x2, x1
mov x0, x1

ldp fp, lr, [sp], #16
ret
