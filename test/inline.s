.text
.globl	add
.type	add,	@function
add:
.LC0:
	pushq	%rbp
	movq	%rsp,	%rbp
	subq	$16,	%rsp
	movl	%edi,	%eax
	addl	%esi,	%eax
	nop
	jmp	.LC1
.LC1:
	leave
	ret
.size	add,	.-add
.text
.globl	main
.type	main,	@function
main:
.LC2:
	pushq	%rbp
	movq	%rsp,	%rbp
	subq	$16,	%rsp
	movl	$32,	-4(%rbp)
	movl	$43,	-8(%rbp)
	movl	-4(%rbp),	%edi
	movl	-8(%rbp),	%esi
	call	add
	movl	%eax,	-12(%rbp)
	movl -12(%rbp), -8(%rbp)
.LC3:
	leave
	ret
.size	main,	.-main
