/*
 *  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
 *  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

#include "defines.h"

.global deactivate_ap
.global deactivate_hlt
.global deactivate_idt32_start

deactivate_ap:

	lidt deactivate_idt32_descr

deactivate_hlt:

	// reset stack first

	mov $deactivate_stack_top, %esp

	hlt
	jmp deactivate_hlt

.data

	// stack used for deactivated APs, only big enough to hold hardware-pushed
	// values, see Intel SDM Vol. 3A, section 6.12.1.
	//
	// NOTE: the AP deactivate_hlt handler must not rely on values pushed onto
	//       the stack as multiple APs might fault simultaneously.
	.align 4
	.space 4 * 4
deactivate_stack_top:

.section .rodata
	.align 4

deactivate_idt32_descr:
	.word deactivate_idt32_end - deactivate_idt32_start - 1 // limit
	.long deactivate_idt32_start                            // base address

	.align 8

deactivate_idt32_start:
	.rept NUM_EXCEPTION_VECTORS
	.long 0
	.long 0
	.endr
deactivate_idt32_end:
