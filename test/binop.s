	.globl main
main:
	/* label constant 0 */
.LC0:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$2,	%eax
	movl	$4,	%ebx
	addl	%ebx,	%eax
	movl	%eax,	%ebx
	movl	$3,	%eax
	imull	%eax,	%ebx
	movl	%ebx,	-4(%rbp)
	movl	$2,	%eax
	movl	$4,	%ebx
	movl	$3,	%r8d
	imull	%r8d,	%ebx
	movl	%ebx,	%r8d
	addl	%r8d,	%eax
	movl	%eax,	-8(%rbp)
	movl	-4(%rbp),	%eax
	jmp	.LC1
	/* label constant 1 */
.LC1:
	popq	%rbp
	ret
