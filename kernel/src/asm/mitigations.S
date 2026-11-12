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

#include "sk-mitigations.h"

#define RSB_ENTRIES	32

/*
 * Selection validation: fail the build when a mitigation selected by the
 * mugenspec-generated sk-mitigations.h is not implemented below. Drop the
 * #error of a mitigation when its primitive is implemented.
 */
#if defined (SK_MIT_EIBRS)
#error "eibrs selected but not implemented in the kernel"
#endif
#if defined (SK_MIT_BHI_DIS_S)
#error "bhiDisS selected but not implemented in the kernel"
#endif
#if defined (SK_MIT_RSB_STUFFING_SINGLECALL)
#error "rsbStuffing singleCall selected but not implemented in the kernel"
#endif

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
 * BHB clearing loop. Mirrors the Linux clear_bhb_loop
 * (arch/x86/entry/entry_64.S, Linux master f47d223627f8, 2026-10-19):
 * 5 x 5 rounds of nested call/return that repopulate the branch history
 * buffer with a trusted pattern before the SK's first indirect branch of
 * the exit path, plus a trailing lfence. Adapted to the preamble register
 * contract: only r15 (outer counter, saved across the inner rounds on the
 * stack) is used. The Linux cacheline alignment (RET in the upper half of a
 * cacheline) is an ITS mitigation; Muen omits it because ITS does not cross
 * the VMX boundary on the supported platform and subject-side aligned
 * thunks handle intra-mode ITS.
 */
.macro clear_branch_history
	mov  $5, %r15
	call 11f
	jmp  15f
11:	call 12f
	RET
12:	push %r15
	mov  $5, %r15
13:	jmp  14f
	nop
14:	sub  $1, %r15
	jnz  13b
	pop  %r15
	sub  $1, %r15
	jnz  11b
	RET
15:	lfence
.endm

/*
 * Legacy IBRS re-assert. Legacy IBRS is not sticky across the guest's
 * execution, so IA32_SPEC_CTRL.IBRS must be set again before the first
 * indirect branch of the exit path. Mirrors RESTORE_HOST_SPEC_CTRL_BODY /
 * vmx_spec_ctrl_restore_host(), arch/x86/kvm/vmx/{vmenter.S,vmx.c}, Linux
 * master f47d223627f8 (2026-10-19): on legacy-IBRS CPUs the host value is
 * written unconditionally, regardless of the guest's value. The host value
 * is IBRS = 1, SSBD = 0 (IA32_SPEC_CTRL = 1), see SDM Vol. 4, Table 2-2,
 * MSR 0x48.
 *
 * The preamble may only use r15 (saved by the caller) and rsp; the other
 * GPRs still hold guest state, hence the save/restore of rax/rdx/rcx
 * around the WRMSR.
 */
.macro set_ibrs
	push %rax
	push %rdx
	push %rcx
	mov $0x48, %ecx
	mov $1, %eax
	xor %edx, %edx
	wrmsr
	pop %rcx
	pop %rdx
	pop %rax
.endm

/*
 * Mitigation preamble of the VM-exit path, invoked once at the top of
 * vmx_exit_handler before any RET of the exit path. A primitive is compiled
 * in iff its selection define is present.
 */
.macro vmx_exit_mitigations_preamble
#if defined (SK_MIT_IBRS_LEGACY)
	set_ibrs
#endif
#if defined (SK_MIT_RSB_STUFFING_FULLFILL32)
	fill_return_buffer
#endif
#if defined (SK_MIT_BHB_LOOP)
	clear_branch_history
#endif
.endm
