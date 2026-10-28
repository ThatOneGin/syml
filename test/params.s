.text
.globl	syml.add
.type	syml.add,	@function
syml.add:
.LC0:
	pushq	%rbp
	movq	%rsp,	%rbp
	subq	$16,	%rsp
	movl	%edi,	%eax
	addl	%esi,	%eax
	movl	%eax,	-4(%rbp)
	movl	-4(%rbp),	%eax
	jmp	.LC1
.LC1:
	leave
	ret
.size	syml.add,	.-syml.add
.text
.globl	syml.main
.type	syml.main,	@function
syml.main:
.LC2:
	pushq	%rbp
	movq	%rsp,	%rbp
	movl	$0,	%eax
	jmp	.LC3
.LC3:
	leave
	ret
.size	syml.main,	.-syml.main
