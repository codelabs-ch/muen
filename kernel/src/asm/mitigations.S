/*
 *  Copyright (C) 2026  Reto Buerki <reet@codelabs.ch>
 *  Copyright (C) 2026  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

#define RSB_ENTRIES	32

/*
 * Full 32-entry RSB fill using r15 as the loop counter. Mirrors the Linux
 * __FILL_RETURN_BUFFER (RSB_CLEAR_LOOPS = 32) with its int3 slot trap -
 * arch/x86/include/asm/nospec-branch.h, Linux master f47d223627f8
 * (2026-10-19, v7.4-rc1 development). Each iteration stuffs two slots whose
 * return addresses point at an int3 (the speculation trap) and pops them;
 * the trailing lfence is the jnz-misprediction barrier.
 */
.macro fill_return_buffer
	mov $(RSB_ENTRIES/2), %r15
1:
	call 3f
	int3
3:
	call 5f
	int3
5:
	add $16, %rsp
	dec %r15
	jnz 1b
	lfence
.endm

/*
 * Mitigation preamble of the VM-exit path, invoked once at the top of
 * vmx_exit_handler before any RET of the exit path.
 */
.macro vmx_exit_mitigations_preamble
	fill_return_buffer
.endm
