.text
.globl	add
.type	add,	@function
add:
.LC0:
	pushq	%rbp
	movq	%rsp,	%rbp
	subq	$16,	%rsp
	movl	16(%rbp),	%eax
	addl	24(%rbp), %eax
	nop
	jmp	.LC1
.LC1:
	leave
	ret
.size	add,	.-add
.text
.globl	main
.type	main,	@function
main:
.LC2:
	pushq	%rbp
	movq	%rsp,	%rbp
	subq	$16,	%rsp
/* alloca %0 i32 */
	movl	$32,	-4(%rbp)
/* alloca %1 i32 */
	movl	$43,	-8(%rbp)
	pushq	-8(%rbp)
	pushq	-4(%rbp)
	call	add
/* alloca %2 i32 */
	movl	%eax,	-12(%rbp)
	movl -12(%rbp), -8(%rbp)
.LC3:
	leave
	ret
.size	main,	.-main
