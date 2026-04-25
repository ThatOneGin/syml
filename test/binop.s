.text
.globl	main
.type	main,	@function
main:
.LC0:
	pushq	%rbp
	movq	%rsp,	%rbp
	subq	$16,	%rsp
	movl	$2,	%eax
	addl	$4, %eax
	movl	%eax,	%ebx
	imull	$3, %ebx
/* alloca %0 i32 */
	movl	%ebx,	-4(%rbp)
	movl	$2,	%ebx
	movl	$4,	%eax
	imull	$3, %eax
	addl	%eax, %ebx
/* alloca %3 i32 */
	movl	%ebx,	-8(%rbp)
	movl	-4(%rbp),	%eax
	jmp	.LC1
.LC1:
	leave
	ret
.size	main,	.-main
