/*
 * SCHEME->C
 *
 * FREEBSD assembly code.
 *
 */

	.align 2
.globl	_sc_geti386regs

_sc_geti386regs:
	pushl	%ebp
	movl %esp,%ebp
	movl 8(%ebp),%eax
	movl %edx,(%eax)
	movl %ecx,4(%eax)
	movl %ebx,8(%eax)
	movl %esi,12(%eax)
	movl %edi,16(%eax)
	leave
	ret

