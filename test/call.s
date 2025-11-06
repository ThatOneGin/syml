	.globl test
test:
	/* label constant 0 */
.LC0:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	$.LK0,	-8(%rbp)
	movq %rax, %rdi	/* inline */
	call puts	/* inline */
	/* label constant 1 */
.LC1:
	popq	%rbp
	ret
	.globl main
main:
	/* label constant 2 */
.LC2:
	pushq	%rbp
	movq	%rsp, %rbp
	call	test
	movl	$0,	%eax
	jmp	.LC3
	/* label constant 3 */
.LC3:
	popq	%rbp
	ret
.LK0:
	.asciz "Hello, world!\n"
