.text
.globl	main
.type	main,	@function
main:
.LC0:
	pushq	%rbp
	movq	%rsp,	%rbp
	subq	$16,	%rsp
/* alloca %0 i32 */
	movl	$0,	-4(%rbp)
/* alloca %1 i32 */
	movl	$1,	-8(%rbp)
	movl	-4(%rbp),	%eax
	cmpl	-8(%rbp), %eax
	je	.LC1
	movl	$1,	%eax
	jmp	.LC2
.LC1:
	movl	$0,	%eax
	jmp	.LC2
.LC2:
	leave
	ret
.size	main,	.-main
