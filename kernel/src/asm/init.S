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

#include "policy.h"

.global _start
.global ap_entry_point
.global cpu_counter
.global cpu_id

#define IA32_VMX_BASIC 0x480
#define IA32_PAT       0x277
#define IA32_EFER      0xc0000080

#define CR0_NE_BIT    5
#define CR0_WP_BIT    16
#define CR0_NW_BIT    29
#define CR0_CD_BIT    30
#define CR0_PG_BIT    31
#define CR4_PAE_BIT   5
#define EFER_LME_BIT  8
#define EFER_NXE_BIT  11

// clear pagecount number of 4k memory starting at given address
.macro clear address pagecount
	movl $\address, %ebx
	movl $\pagecount, %eax
	movl $0x400, %ecx
	mull %ecx
	movl %eax, %ecx
1:
	movl $0, (%ebx)
	add $4, %ebx
	loop 1b
.endm

.text
.code32

// see "Initializing IA-32e Mode" in the Intel SDM, Volume 3A, Chapter 9.8.5

_start:

	// copy AP trampoline code to low-memory

	cld
	mov $trampoline_src, %esi
	mov $trampoline_start, %edi
	mov $trampoline_end, %ecx
	sub $trampoline_start, %ecx
	rep movsb

	// initialize VMX regions

	clear VMXON_ADDRESS CPU_COUNT
	clear VMCS_ADDRESS SUBJECT_COUNT

	movl $IA32_VMX_BASIC, %ecx
	rdmsr

	// VMXON

	movl $CPU_COUNT, %ecx
	movl $VMXON_ADDRESS, %edx
1:
	movl %eax, (%edx)
	add $0x1000, %edx
	loop 1b

	// VMCS

	movl $SUBJECT_COUNT, %ecx
	movl $VMCS_ADDRESS, %edx
1:
	movl %eax, (%edx)
	add $0x1000, %edx
	loop 1b

ap_entry_point:

	// enable PAE

	movl %cr4, %eax
	btsl $CR4_PAE_BIT, %eax
	movl %eax, %cr4

	// get CPU ID and increment counter

	mov $1, %eax
	lock xadd %eax, cpu_counter

	// deactivate excess CPUs

	cmp $CPU_COUNT, %eax
	jae deactivate

	// store local CPU ID in esi register until it can be saved to per-cpu area

	mov %eax, %esi

	// set up per-CPU paging

	mov $0x4, %edx
	mul %edx
	add $kernel_pml4_start, %eax
	mov (%eax), %edx
	mov %edx, %cr3

	// enable IA-32e mode and execute-disable

	movl $IA32_EFER, %ecx
	rdmsr
	btsl $EFER_LME_BIT, %eax
	btsl $EFER_NXE_BIT, %eax
	wrmsr

	// enable paging, write protection, caching and native FPU error reporting

	movl %cr0, %eax
	btsl $CR0_PG_BIT, %eax
	btsl $CR0_WP_BIT, %eax
	btsl $CR0_NE_BIT, %eax
	btrl $CR0_NW_BIT, %eax
	btrl $CR0_CD_BIT, %eax
	movl %eax, %cr0

	// set up GDT

	lgdt gdt_ptr

	// indirect long jump to 64-bit code

	ljmp $8, $start64

deactivate:
	cli
	hlt
	jmp deactivate

.code64
start64:

	// set up Page-Attribute Table (PAT)

	movl $IA32_PAT, %ecx
	movl $PAT_LOW,  %eax
	movl $PAT_HIGH, %edx
	wrmsr

	// set up kernel stack

	mov $KERNEL_STACK, %rsp

	// set CPU ID variable located at start of per-CPU store

	mov %rsi, %rax
	movb %al, cpu_id

	// set up kernel segment selectors

	mov $0x10, %ecx
	mov %ecx, %ds
	mov %ecx, %es
	mov %ecx, %fs
	mov %ecx, %gs
	mov %ecx, %ss

	// initialize Ada runtime (BSP only)

	cmp $0x0, %rsi
	jne kernel_init
	call adainit
	jmp kernel_init

.data
	.align 4
cpu_counter:
	.long 0

.set cpu_id, PERCPU_STORE
