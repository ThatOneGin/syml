	.globl main
main:
	/* label constant 0 */
.LC0:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$2,	%ecx
	movl	$4,	%edx
	addl	%edx,	%ecx
	movl	%ecx,	%eax
	movl	$3,	%ebx
	imull	%ebx,	%eax
	movl	%eax,	-4(%rbp)
	movl	-4(%rbp),	%eax
	jmp	.LC1
	/* label constant 1 */
.LC1:
	popq	%rbp
	ret
