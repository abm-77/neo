.section .text
	.global add
	.global foo
	.global sub

add:
add_entry_0:
stp fp, lr, [sp, -#16]!
mov fp, sp
add x0, x0, x1
ldp fp, lr, [sp], #16
ret 

foo:
foo_entry_0:
stp fp, lr, [sp, -#16]!
mov fp, sp
mov x0, x0
mov x1, #1
bl add
mov x0, #1
mov x1, x1
bl sub
mov x1, x0
mul x0, x0, x1
ldp fp, lr, [sp], #16
ret 

sub:
sub_entry_0:
stp fp, lr, [sp, -#16]!
mov fp, sp
sub x0, x0, x1
ldp fp, lr, [sp], #16
ret 

