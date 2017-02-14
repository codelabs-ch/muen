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

.global gdt_ptr

.section .gdt

	.align 4
	.space 2

// Global Descriptor Table: See Intel SDM Vol. 3A, section 3.5.1
gdt_ptr:
	.word gdt_end - gdt_start - 1 // limit
	.long gdt_start               // base address

	.align 8
gdt_start:
	// null descriptor
	.quad 0
	// 64-bit code segment descriptor, index 1
	.long 0
	.long (GDTE_LONG | GDTE_PRESENT | GDTE_CODE | GDTE_NON_SYSTEM)
	// 64-bit data segment descriptor, index 2
	.long 0
	.long (GDTE_LONG | GDTE_PRESENT | GDTE_TYPE_DATA_W | GDTE_NON_SYSTEM)
	// task segment descriptor, index 3
	.long 0
	.long (GDTE_PRESENT | GDTE_SYS_TSS)
gdt_end:
