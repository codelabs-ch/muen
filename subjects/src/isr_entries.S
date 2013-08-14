/*
 *  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
 *  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

.data
.global isrlist
isrlist:

.text

.macro save_regs
	push %r15
	push %r14
	push %r13
	push %r12
	push %r11
	push %r10
	push %r9
	push %r8
	push %rsi
	push %rdi
	push %rdx
	push %rcx
	push %rbx
	push %rax
.endm

.macro restore_regs
	pop %rax
	pop %rbx
	pop %rcx
	pop %rdx
	pop %rdi
	pop %rsi
	pop %r8
	pop %r9
	pop %r10
	pop %r11
	pop %r12
	pop %r13
	pop %r14
	pop %r15
.endm

.macro reserved_entry
	.data
	.quad 0
	.previous
.endm

.macro isr_entry
	.align 4, 0x90
1:	.data
	.quad 1b
	.previous
.endm

.macro interrupt vector
	isr_entry
	push $\vector
	jmp handle_all
.endm

handle_all:
	save_regs
	mov 0x70(%rsp), %rdi
	call dispatch_interrupt
	restore_regs
	add $8, %rsp
	iretq

.rept 32
	reserved_entry
.endr

.set cur_vector, 32
.rept 224
	interrupt cur_vector
.set cur_vector, cur_vector + 1
.endr
