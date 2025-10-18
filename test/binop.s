	.globl main
main:
	/* label constant 0 */
.LC0:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$2,	%eax
	movl	$4,	%ecx
	movl	$3,	%edx
	imull	%edx,	%ecx
	movl	%ecx,	%ebx
	addl	%ebx,	%eax
	movl	%eax,	-4(%rbp)
	movl	-4(%rbp),	%eax
	jmp	.LC1
	/* label constant 1 */
.LC1:
	popq	%rbp
	ret
