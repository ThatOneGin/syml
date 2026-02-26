	.globl test
test:
	/* label constant 0 */
.LC0:
	pushq	%rbp
	movq %rsp, %rbp
	subq $16, %rbp
	movq	$.LK0,	-8(%rbp)
	movq %rax, %rdi	/* inline */
	call puts	/* inline */
	/* label constant 1 */
.LC1:
	leave
	ret
	.globl main
main:
	/* label constant 2 */
.LC2:
	pushq	%rbp
	movq %rsp, %rbp
	/* call start (test) */
	call	test
	/* call end (test) */
	movl	$0,	%eax
	jmp	.LC3
	/* label constant 3 */
.LC3:
	leave
	ret
.LK0:
	.asciz "Hello, world!\n"
