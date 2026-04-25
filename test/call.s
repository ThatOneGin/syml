.text
.globl	test
.type	test,	@function
test:
.LC0:
	pushq	%rbp
	movq	%rsp,	%rbp
	subq	$16,	%rsp
	leaq	.LK0(%rip),	%rax
/* alloca %0 string */
	movq	%rax,	-8(%rbp)
	movq -8(%rbp), %rdi
	call puts
.LC1:
	leave
	ret
.size	test,	.-test
.text
.globl	main
.type	main,	@function
main:
.LC2:
	pushq	%rbp
	movq	%rsp,	%rbp
	call	test
	movl	$0,	%eax
	jmp	.LC3
.LC3:
	leave
	ret
.size	main,	.-main
.section .rodata
.LK0:
	.asciz	"Hello, world!\n"
