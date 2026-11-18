.file	"test/cond-value.syml"
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
	movl	$0,	-4(%rbp)
	movl	$1,	-8(%rbp)
	movl	-4(%rbp),	%eax
	cmpl	-8(%rbp),	%eax
	sete	%al
	movzbl	%al, %eax
	movl	%eax,	-12(%rbp)
	movl	-12(%rbp),	%eax
	jmp	.LC1
.LC1:
	leave
	.cfi_def_cfa %rbp, 8
	ret
	.cfi_endproc
.size	_S4mainF,	.-_S4mainF
