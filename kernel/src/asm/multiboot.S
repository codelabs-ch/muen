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

#define MBOOT_PAGEALIGN  1 <<  0
#define MBOOT_MEMINFO    1 <<  1
#define MBOOT_AOUTKLUDGE 1 << 16
#define MBOOT_FLAGS      MBOOT_PAGEALIGN | MBOOT_MEMINFO | MBOOT_AOUTKLUDGE
#define MBOOT_MAGIC      0x1BADB002
#define MBOOT_CHECKSUM   -(MBOOT_MAGIC + MBOOT_FLAGS)

.section .multiboot
	.align 4
multiboot_header:
	.long MBOOT_MAGIC
	.long MBOOT_FLAGS
	.long MBOOT_CHECKSUM
	.long multiboot_header
	.long image_start
	.long 0
	.long 0
	.long kernel_entry_point
