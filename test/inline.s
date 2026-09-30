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
	movq	-4(%rbp),	%rdi
	movq	-8(%rbp),	%rsi
	call	add
	movl	%eax,	-12(%rbp)
	movl -12(%rbp), -8(%rbp)
.LC3:
	leave
	ret
.size	main,	.-main
