.globl	main
main:
.LC0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	$0, -4(%rbp)
	movl	$1, -8(%rbp)
	movl	-4(%rbp), %eax
	cmpl	-8(%rbp), %eax
	je	.LC1
	movl	$1, %eax
	jmp	.LC2
.LC1:
	movl	$0, %eax
	jmp	.LC2
.LC2:
	leave
	ret
