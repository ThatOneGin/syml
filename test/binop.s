.file	"test/binop.syml"
.text
.globl	_S4mainF
.type	_S4mainF,	@function
_S4mainF:
.LC0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp,	%rbp
	.cfi_def_cfa_register %rbp
	subq	$16,	%rsp
	movl	$2,	%eax
	addl	$4,	%eax
	movl	%eax,	%ebx
	imull	$3,	%ebx
	movl	%ebx,	-4(%rbp)
	movl	$2,	%ebx
	movl	$4,	%eax
	imull	$3,	%eax
	addl	%eax,	%ebx
	movl	%ebx,	-8(%rbp)
	movl	-4(%rbp),	%eax
	jmp	.LC1
.LC1:
	leave
	.cfi_def_cfa %rbp, 8
	ret
	.cfi_endproc
.size	_S4mainF,	.-_S4mainF
