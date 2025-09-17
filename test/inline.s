	.globl main
main:
	/* label constant 0 */
.LC0:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	$.LK0, %rax
	movq %rax, %rdi	/* inline */
	call puts	/* inline */
	movl	$0, %eax
	jmp .LC1
	/* label constant 1 */
.LC1:
	popq	%rbp
	ret
.LK0:
	.asciz "Hello, world"
