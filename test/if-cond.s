.file	"test/if-cond.syml"
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
	je	.LC1
	movl	$1,	%eax
	jmp	.LC2
.LC1:
	movl	$0,	%eax
	jmp	.LC2
.LC2:
	leave
	.cfi_def_cfa %rbp, 8
	ret
	.cfi_endproc
.size	_S4mainF,	.-_S4mainF
