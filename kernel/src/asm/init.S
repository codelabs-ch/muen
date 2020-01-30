/*
 *  Copyright (C) 2013, 2016  Reto Buerki <reet@codelabs.ch>
 *  Copyright (C) 2013, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

#include "gdt.h"
#include "policy.h"

.global kernel_entry_point
.global ap_entry_point
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

// see "Initializing IA-32e Mode" in the Intel SDM Vol. 3A,
// "9.8.5 Initializing IA-32e Mode"

kernel_entry_point:

	// copy AP trampoline code to low-memory

	cld
	mov $trampoline_src, %esi
	mov $trampoline_start, %edi
	mov $trampoline_end, %ecx
	sub $trampoline_start, %ecx
	rep movsb

	// initialize VMX regions

	clear VMXON_ADDRESS CPU_COUNT

	movl $IA32_VMX_BASIC, %ecx
	rdmsr

	// Set VMCS revision identifier in VMXON regions, see Intel SDM Vol. 3C,
	// "24.11.5 VMXON Region".

	movl $CPU_COUNT, %ecx
	movl $VMXON_ADDRESS, %edx
1:
	movl %eax, (%edx)
	add $0x1000, %edx
	loop 1b

ap_entry_point:

	// explicitly disable interrupts, do not assume bootloader has done it.

	cli

	// assure cache consistency.

	wbinvd

	// enable PAE

	movl %cr4, %eax
	btsl $CR4_PAE_BIT, %eax
	movl %eax, %cr4

	// derive CPU ID from initial APIC ID
	// divide APIC ID by two to get idx into CPU ID mapping table

	movl $1, %eax
	cpuid
	shrl $25, %ebx

	movl $kernel_cpu_ids, %edx
	addl %ebx, %edx
	xorl %eax, %eax
	movb (%edx), %al

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

	// set up initial GDT for jump to 64-bit code

	lgdt boot_gdt64_descr

	// indirect long jump to 64-bit code

	ljmp $8, $start64

.code64
start64:

	// set up Page-Attribute Table (PAT)

	movl $IA32_PAT, %ecx
	movl $PAT_LOW,  %eax
	movl $PAT_HIGH, %edx
	wrmsr

	// set up kernel stack

	mov $KERNEL_STACK, %rsp

	// set CPU ID variable

	mov %rsi, %rax
	movb %al, cpu_id

	// set up kernel segment selectors

	mov $0x10, %ecx
	mov %ecx, %ds
	mov %ecx, %es
	mov %ecx, %fs
	mov %ecx, %gs
	mov %ecx, %ss

	// initialize Ada runtime

	call adainit
	jmp kernel_init

.data
cpu_id: .byte 0

// Global Descriptor Table: See Intel SDM Vol. 3A,
// "3.5.1 Segment Descriptor Tables"
	.align 4
	.space 2
boot_gdt64_descr:
	.word boot_gdt64_end - boot_gdt64_start - 1 // limit
	.long boot_gdt64_start                      // base address

	.align 8
boot_gdt64_start:
	// null descriptor
	.quad 0
	// 64-bit code segment descriptor, index 1
	.long 0
	.long (GDTE_LONG | GDTE_PRESENT | GDTE_CODE | GDTE_NON_SYSTEM)
	// 64-bit data segment descriptor, index 2
	.long 0
	.long (GDTE_LONG | GDTE_PRESENT | GDTE_TYPE_DATA_W | GDTE_NON_SYSTEM)
boot_gdt64_end:
