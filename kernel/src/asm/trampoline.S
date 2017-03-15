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

#include "gdt.h"

#define CR0_PE_BIT 1

.section .trampoline, "a"
.code16

ap_start:
	lgdt ap_gdt_descr

	// enable protected mode

	movl %cr0, %eax
	or $CR0_PE_BIT, %eax
	movl %eax, %cr0

	ljmp $8, $ap_start32

.code32
ap_start32:
	movl $AP_KERN_DS, %eax
	mov %eax, %ds
	mov %eax, %ss

	ljmp $AP_KERN_CS, $ap_entry_point

	.align 4
	.space 2

ap_gdt_descr:
	.word ap_gdt_end - ap_gdt_start - 1 // limit
	.long ap_gdt_start                  // base address

	.align 4
ap_gdt_start:
	// null descriptor
	.quad 0
	// 32-bit code segment descriptor, index 1
	.long 0xffff
	.long (GDTE_32BIT | GDTE_PRESENT | GDTE_CODE | GDTE_NON_SYSTEM | GDTE_GRANULAR_4K)
	// 32-bit data segment descriptor, index 2
	.long 0xffff
	.long (GDTE_32BIT | GDTE_PRESENT | GDTE_TYPE_DATA_W | GDTE_NON_SYSTEM | GDTE_GRANULAR_4K)
ap_gdt_end:
