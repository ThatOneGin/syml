.text
.globl	add
.type	add,	@function
add:
.LC0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	movl	16(%rbp), %eax
	addl	24(%rbp), %eax
/* alloca %0 i32 */
	movl	%eax, -4(%rbp)
	movl	-4(%rbp), %eax
	jmp	.LC1
.LC1:
	leave
	ret
.size add, .-add
.text
.globl	main
.type	main,	@function
main:
.LC2:
	pushq	%rbp
	movq	%rsp, %rbp
	movl	$0, %eax
	jmp	.LC3
.LC3:
	leave
	ret
.size main, .-main
