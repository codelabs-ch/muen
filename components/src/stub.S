/*
 *  Copyright (C) 2013-2024  Reto Buerki <reet@codelabs.ch>
 *  Copyright (C) 2013-2024  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

.section .text.init
.global _main

_main:
	// Set RBP to null for backtracing
	xor %rbp, %rbp
	call main
	// Should never return. Raise exception if ever reached.
	ud2
