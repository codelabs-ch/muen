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

ENTRY(kernel_entry_point)

SECTIONS
{
	. = 0x00100000;

	.text :
	{
		image_start = .;
		KEEP (multiboot.o(.multiboot))
		*(.text)
		trampoline_src = .;
	}

	.trampoline 0x00001000 : AT (ADDR (.text) + SIZEOF (.text))
	{
		trampoline_start = .;
		KEEP (trampoline.o(.trampoline))
		trampoline_end = .;
	}

	.data 0x00120000 : AT (0x00120000)
	{
		*(.data)
	}

	.bss 0x00122000 :
	{
		*(.bss)
	}

	.globaldata 0x0012b000 :
	{
		*(.globaldata)
	}

	.rodata 0x0012f000 :
	{
		*(.rodata)
	}

	/DISCARD/ :
	{
		*(.comment)
		*(.eh_frame)
		*(.eh_frame_hdr)
	}
}
