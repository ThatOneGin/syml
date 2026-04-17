.text
.globl	main
.type main, @function
main:
.LC0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	leaq .LK0(%rip), %rax
/* alloca %0 string */
	movq	%rax, -8(%rbp)
	movq %rax, %rdi	/* inline */
	call puts	/* inline */
	movl	$0, %eax
	jmp	.LC1
.LC1:
	leave
	ret
.size main, .-main
.section .rodata
.LK0:
	.asciz	"Hello, world"
