.text
.globl	main
.type	main,	@function
main:
.LC0:
	pushq	%rbp
	movq	%rsp,	%rbp
	subq	$32,	%rsp
	leaq	.LK0(%rip),	%rax
	movq	%rax,	-8(%rbp)
	movl	$0,	%eax
	jmp	.LC1
.LC1:
	leave
	ret
.size	main,	.-main
.section	.rodata
.LK0:
	.asciz	"constant string"
.section	.rodata
x:
	.long	46
y:
	.long	65535
z:
	.asciz	"Hello, world!"
