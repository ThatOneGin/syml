.globl	main
main:
.LC0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	$2, %eax
	addl	$4, %eax
	movl	%eax, %ebx
	imull	$3, %ebx
	movl	%ebx, -4(%rbp)
	movl	$2, %eax
	movl	$4, %ebx
	imull	$3, %ebx
	addl	%ebx, %eax
	movl	%eax, -8(%rbp)
	movl	-4(%rbp), %eax
	jmp	.LC1
.LC1:
	leave
	ret
