	.globl add
add:
	/* label constant 0 */
.LC0:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	%edi,	-4(%rbp)
	movl	%esi,	-8(%rbp)
	movl	-4(%rbp),	%eax
	movl	-8(%rbp),	%ebx
	addl	%ebx,	%eax
	movl	%eax,	-12(%rbp)
	movl	-12(%rbp),	%eax
	jmp	.LC1
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
	movl	$0,	%eax
	jmp	.LC3
	/* label constant 3 */
.LC3:
	popq	%rbp
	ret
