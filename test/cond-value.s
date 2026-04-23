.text
.globl	main
.type	main,	@function
main:
.LC0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
/* alloca %0 i32 */
	movl	$0, -4(%rbp)
/* alloca %1 i32 */
	movl	$1, -8(%rbp)
	movl	-4(%rbp), %eax
	cmpl	-8(%rbp), %eax
	sete	%al
	movzbl	%al, %eax
/* alloca %2 i32 */
	movl	%eax, -12(%rbp)
	movl	-12(%rbp), %eax
	jmp	.LC1
.LC1:
	leave
	ret
.size main, .-main
