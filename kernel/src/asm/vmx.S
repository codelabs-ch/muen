/*
 *  Copyright (C) 2013, 2015  Reto Buerki <reet@codelabs.ch>
 *  Copyright (C) 2013, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

.global vmx_exit_handler_ptr
.global kernel_init

.data
vmx_exit_handler_ptr:
	.quad vmx_exit_handler

#define CPU_REGS_SIZE 8 * 16

.text
vmx_exit_handler:
	push %r15
	push %r14
	push %r13
	push %r12
	push %r11
	push %r10
	push %r9
	push %r8
	push %rbp
	push %rsi
	push %rdi
	push %rdx
	push %rcx
	push %rbx
	push %rax
	mov %cr2, %rax
	push %rax
	mov %rsp, %rdi
	call handle_vmx_exit

launch_subject:
	pop %rax
	mov %rax, %cr2
	pop %rax
	pop %rbx
	pop %rcx
	pop %rdx
	pop %rdi
	pop %rsi
	pop %rbp
	pop %r8
	pop %r9
	pop %r10
	pop %r11
	pop %r12
	pop %r13
	pop %r14
	pop %r15
	vmresume
	vmlaunch
	call vmx_error_from_asm

1:	hlt
	jmp 1b

kernel_init:
	sub $CPU_REGS_SIZE, %rsp
	mov %rsp, %rdi
	call sk_initialize
	jmp launch_subject
