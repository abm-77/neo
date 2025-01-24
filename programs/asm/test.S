.section .text
	.global add

add:
add_entry_0:
stp fp, lr, [sp, -#28]!
mov fp, sp
str x0, [sp, 16]
str x0, [sp, 20]
str x0, [sp, 24]
b add_loop_header_0

add_loop_header_0:
ldr x3, [sp, 24]
cmp x3, x1
cset x0, LT
cbz x0, add_loop_end_0

add_loop_body_0:
ldr x7, [sp, 16]
add x4, x7, x1
str x4, [sp, 16]
b add_loop_header_0

add_loop_end_0:
ldr x1, [sp, 16]
ldr x2, [sp, 20]
add x1, x1, x2
mov x0, x1

ldp fp, lr, [sp], #28
ret
