.file	"test/params.syml"
.text
.globl	_S3addFll
.type	_S3addFll,	@function
_S3addFll:
.LC0:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp,	%rbp
	.cfi_def_cfa_register %rbp
	subq	$16,	%rsp
	movl	%edi,	-4(%rbp)
	movl	%esi,	-8(%rbp)
	movl	-4(%rbp),	%eax
	addl	-8(%rbp),	%eax
	movl	%eax,	-12(%rbp)
	movl	-12(%rbp),	%eax
	jmp	.LC1
.LC1:
	leave
	.cfi_def_cfa %rbp, 8
	ret
	.cfi_endproc
.size	_S3addFll,	.-_S3addFll
.text
.globl	_S4mainF
.type	_S4mainF,	@function
_S4mainF:
.LC2:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp,	%rbp
	.cfi_def_cfa_register %rbp
	movl	$0,	%eax
	jmp	.LC3
.LC3:
	leave
	.cfi_def_cfa %rbp, 8
	ret
	.cfi_endproc
.size	_S4mainF,	.-_S4mainF
