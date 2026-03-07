.globl	main
main:
.LC0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	$.LK0, -8(%rbp)
	movq %rax, %rdi	/* inline */
	call puts	/* inline */
	movl	$0, %eax
	jmp	.LC1
.LC1:
	leave
	ret
.LK0:
	.asciz	"Hello, world"
