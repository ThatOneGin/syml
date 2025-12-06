	.globl main
main:
	/* label constant 0 */
.LC0:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0,	-4(%rbp)
	movl	$1,	-8(%rbp)
	movl	-4(%rbp),	%eax
	movl	-8(%rbp),	%ebx
	cmpl	%ebx,	%eax
	je	.LC1
	movl	$1,	%eax
	jmp	.LC2
	/* label constant 1 */
.LC1:
	movl	$0,	%eax
	jmp	.LC2
	/* label constant 2 */
.LC2:
	popq	%rbp
	ret
