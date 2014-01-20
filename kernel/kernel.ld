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

SECTIONS
{
	. = 0x00100000;

	.text :
	{
		image_start = .;
		multiboot.o(.multiboot)
		*(.text)

		trampoline_src = .;
	}

	.trampoline 0x00000000 : AT (ADDR (.text) + SIZEOF (.text))
	{
		trampoline_start = .;
		trampoline.o(.trampoline)
		trampoline_end = .;
	}

	.data 0x00110000 : AT (0x00110000)
	{
		*(.gdt)
		*(.data)
	}

	.bss 0x00111000 :
	{
		*(.bss)
	}

	.stack 0x00113000 :
	{
		. += 0x4000;
	}

	.cpu_local 0x00117000 :
	{
		cpu_id = .;
		. += 0x4000;
	}

	.rodata :
	{
		*(.rodata)
	}

	.interfaces 0x001ff000 :
	{
		*(.interfaces)
	}

	. = 0x00400000;
	image_end = .;

	/DISCARD/ :
	{
		*(.comment)
		*(.eh_frame)
		*(.eh_frame_hdr)
	}
}
