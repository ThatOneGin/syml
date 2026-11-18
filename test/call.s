.file	"test/call.syml"
.text
.globl	_S4testF
.type	_S4testF,	@function
_S4testF:
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
	movq	-8(%rbp),	%rdi
	call	puts@PLT
.LC1:
	leave
	.cfi_def_cfa %rbp, 8
	ret
	.cfi_endproc
.size	_S4testF,	.-_S4testF
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
	call	_S4testF
	movl	$0,	%eax
	jmp	.LC3
.LC3:
	leave
	.cfi_def_cfa %rbp, 8
	ret
	.cfi_endproc
.size	_S4mainF,	.-_S4mainF
.section	.rodata
.LK0:
	.asciz	"Hello, world!"
