.globl	test
test:
.LC0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movq	$.LK0, -8(%rbp)
	movq %rax, %rdi	/* inline */
	call puts	/* inline */
.LC1:
	leave
	ret
.globl	main
main:
.LC2:
	pushq	%rbp
	movq	%rsp, %rbp
	call	test
	movl	$0, %eax
	jmp	.LC3
.LC3:
	leave
	ret
.LK0:
	.asciz	"Hello, world!\n"
