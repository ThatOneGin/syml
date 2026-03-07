.globl	main
main:
.LC0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	$0, -4(%rbp)
	movl	$1, -8(%rbp)
	movl	-4(%rbp), %eax
	cmpl	-8(%rbp), %eax
	sete	%al
	movzbl	%al, %eax
	movl	%eax, -12(%rbp)
	movl	-12(%rbp), %eax
	jmp	.LC1
.LC1:
	leave
	ret
