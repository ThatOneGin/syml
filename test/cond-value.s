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
	sete	%al
	movzbl	%al,	%eax
	movl	%eax,	-12(%rbp)
	movl	-12(%rbp),	%eax
	jmp	.LC1
	/* label constant 1 */
.LC1:
	popq	%rbp
	ret
