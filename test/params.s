	.globl add
add:
	/* label constant 0 */
.LC0:
	pushq	%rbp
	movq %rsp, %rbp
	subq $16, %rbp
	movl	16(%rbp),	%eax
	movl	24(%rbp),	%ebx
	addl	%ebx,	%eax
	movl	%eax,	-4(%rbp)
	movl	-4(%rbp),	%eax
	jmp	.LC1
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
	movl	$0,	%eax
	jmp	.LC3
	/* label constant 3 */
.LC3:
	leave
	ret
