.file	"test/global.syml"
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
	leaq	.LK0(%rip),	%rax
	movq	%rax,	-8(%rbp)
	movl	$0,	%eax
	jmp	.LC1
.LC1:
	leave
	.cfi_def_cfa %rbp, 8
	ret
	.cfi_endproc
.size	_S4mainF,	.-_S4mainF
.section	.rodata
.LK0:
	.asciz	"constant string"
.section	.rodata
_S1xVl:
	.long	46
_S1yVd:
	.long	65535
_S1zVs:
	.asciz	"Hello, world!"
