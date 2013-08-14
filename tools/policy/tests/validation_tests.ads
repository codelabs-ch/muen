--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Ahven.Framework;

package Validation_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Verify invalid memory region size check.
   procedure Invalid_Memregion_Size;

   --  Verify invalid memory region addresses.
   procedure Invalid_Memregion_Addrs;

   --  Verify invalid VMXON addresses.
   procedure Invalid_Vmxon_Addrs;

   --  Verify invalid VMCS addresses.
   procedure Invalid_Vmcs_Addrs;

   --  Verify kernel with invalid PML4 address.
   procedure Invalid_Knl_Pml4_Addr;

   --  Verify kernel with invalid stack address.
   procedure Invalid_Knl_Stack_Addr;

   --  Verify kernel with invalid per-CPU page address.
   procedure Invalid_Knl_CPU_Page_Addr;

   --  Verify scheduling CPU elements with different total tick count.
   procedure Invalid_Sched_CPU_Ticks;

   --  Verify scheduling major frame with incorrect CPU element count.
   procedure Invalid_Sched_CPU_Count;

   --  Verify scheduling plan with invalid subject.
   procedure Invalid_Sched_Subject;

   --  Verify scheduling plan with subject on wrong CPU.
   procedure Invalid_Sched_Subject_CPU;

   --  Verify subject with invalid PML4 address.
   procedure Invalid_Subj_Pml4_Addr;

   --  Verify subject with invalid I/O bitmap address.
   procedure Invalid_Subj_IO_Bitmap_Addr;

   --  Verify subject with invalid CPU number.
   procedure Invalid_Subj_CPU;

   --  Verify subject with invalid binary reference.
   procedure Invalid_Subj_Binary;

   --  Verify subject with trap table entry self-reference.
   procedure Invalid_Subj_Trap_Self_Ref;

   --  Verify subject trap table entry with invalid destination subject.
   procedure Invalid_Subj_Trap_Dst;

   --  Verify subject trap table entry with invalid destination subject CPU.
   procedure Invalid_Subj_Trap_Dst_CPU;

   --  Verify subject with event table entry self-reference.
   procedure Invalid_Subj_Event_Self_Ref;

   --  Verify subject event table entry with invalid destination subject.
   procedure Invalid_Subj_Event_Dst;

   --  Verify subject event table entry with invalid destination subject CPU.
   procedure Invalid_Subj_Event_Dst_CPU;

   --  Verify subject IPI event table entry with invalid destination CPU.
   procedure Invalid_Subj_Event_IPI_Dst_CPU;

   --  Verify subject with invalid MSR.
   procedure Invalid_Subj_MSR;

   --  Verify subject with invalid MSR bitmap address.
   procedure Invalid_Subj_MSR_Bitmap_Addr;

   --  Verify device with non-unique IRQ.
   procedure Invalid_Device_IRQ;

   --  Validate policy.
   procedure Policy_Validation;

end Validation_Tests;
