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

ENTRY(_main)

SECTIONS
{
	/* first page is used as stack guard page */

	.stack 0x1000 :
	{
		. += 0x4000;
	}

	.text ALIGN (4096) :
	{
		KEEP (stub.o(.stub))
		*(.text .text.*)
	}

	.bss ALIGN (4096) :
	{
		*(.bss .bss.*)
	}

	.rodata ALIGN (4096) :
	{
		*(.rodata .rodata.*)
	}

	.data ALIGN (4096) :
	{
		*(.data .data.*)
	}

	/DISCARD/ :
	{
		*(.comment)
		*(.eh_frame)
		*(.eh_frame_hdr)
	}
}
