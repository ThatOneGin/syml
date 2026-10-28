.text
.globl	syml.main
.type	syml.main,	@function
syml.main:
.LC0:
	pushq	%rbp
	movq	%rsp,	%rbp
	subq	$16,	%rsp
	leaq	.LK0(%rip),	%rax
	movq	%rax,	-8(%rbp)
	movl	$0,	%eax
	jmp	.LC1
.LC1:
	leave
	ret
.size	syml.main,	.-syml.main
.section	.rodata
.LK0:
	.asciz	"constant string"
.section	.rodata
syml.x:
	.long	46
syml.y:
	.long	65535
syml.z:
	.asciz	"Hello, world!"
