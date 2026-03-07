.globl	add
add:
.LC0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	16(%rbp), %eax
	addl	24(%rbp), %eax
	movl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	jmp	.LC1
.LC1:
	leave
	ret
.globl	main
main:
.LC2:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	jmp	.LC3
.LC3:
	leave
	ret
