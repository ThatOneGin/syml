.text
.globl	syml.test
.type	syml.test,	@function
syml.test:
.LC0:
	pushq	%rbp
	movq	%rsp,	%rbp
	subq	$16,	%rsp
	leaq	.LK0(%rip),	%rax
	movq	%rax,	-8(%rbp)
	movq	-8(%rbp),	%rdi
	call	puts
.LC1:
	leave
	ret
.size	syml.test,	.-syml.test
.text
.globl	syml.main
.type	syml.main,	@function
syml.main:
.LC2:
	pushq	%rbp
	movq	%rsp,	%rbp
	call	syml.test
	movl	$0,	%eax
	jmp	.LC3
.LC3:
	leave
	ret
.size	syml.main,	.-syml.main
.section	.rodata
.LK0:
	.asciz	"Hello, world!"
