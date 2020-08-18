/*
 *  Copyright (C) 2020  Reto Buerki <reet@codelabs.ch>
 *  Copyright (C) 2020  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

.section .text
.global _main

_main:
	// Save initial register values on stack to restore after
	// initialization.
	// Note: Registers r10-r15 are used by the init code and do not
	//       retain their initial value.
	push %r9
	push %r8
	push %rbp
	push %rsi
	push %rdi
	push %rdx
	push %rcx
	push %rbx
	push %rax
	// Space for return values: 5 x 8 bytes (see Run_Info_Type).
	sub $0x28, %rsp
	mov %rsp, %rdi
	call init_main
	pop %r15       // Entry point
	pop %r14       // Status address
	pop %r13       // Status value
	pop %r12       // Stack address
	pop %r11       // Stack size
	pop %rax
	pop %rbx
	pop %rcx
	pop %rdx
	pop %rdi
	pop %rsi
	pop %rbp
	pop %r8
	pop %r9

	// Clear stack region
	xchg %rdi, %r12
	xchg %rcx, %r11
	xor %r10, %r10
	xchg %rax, %r10
	rep stosb

	// Restore registers
	xchg %rdi, %r12
	xchg %rcx, %r11
	xchg %rax, %r10

	// Set STATE_RUNNING
	movq %r13, (%r14)

	// Clear used registers.
	xor %r11, %r11
	xor %r12, %r12
	xor %r13, %r13
	xor %r14, %r14

	// Jump to component code
	jmpq *%r15
	// Should never return
	ud2
