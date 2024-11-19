--
--  Copyright (C) 2023  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2023  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Skp.Arch;
with Skp.Subjects;

with SK.Constants;
with SK.CPU;
with SK.VMX;

package body SK.Arch
is

   -------------------------------------------------------------------------

   function Get_Current_Timestamp return Word64 renames SK.CPU.RDTSC;

   -------------------------------------------------------------------------

   procedure Load_Subject (ID : Skp.Global_Subject_ID_Type)
   is
      Current_VMCS_Addr : constant Word64
        := Skp.Subjects.Get_VMCS_Address (Subject_ID => ID);
   begin
      --D @Item List => impl_kernel_init_sched_steps
      --D Load VMCS of initial subject.
      VMX.Load (VMCS_Address => Current_VMCS_Addr);
   end Load_Subject;

   -------------------------------------------------------------------------

   procedure Set_Timer (Deadline : Word64)
   is
      Now    : constant Word64 := Get_Current_Timestamp;
      Cycles : Word64;
   begin
      if Deadline > Now then
         Cycles := Deadline - Now;
      else
         Cycles := 0;
      end if;
      VMX.VMCS_Write (Field => Constants.GUEST_VMX_PREEMPT_TIMER,
                      Value => Cycles / 2 ** Skp.Arch.VMX_Timer_Rate);
   end Set_Timer;

end SK.Arch;
