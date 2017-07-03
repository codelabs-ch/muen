--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with X86_64;

with SK.CPU_Info;
with SK.Crash_Audit_Types;

package SK.Crash_Audit
with
   Abstract_State => (State with External => (Async_Writers, Async_Readers)),
   Initializes    => State
is

   --  Initialize crash audit facility.
   procedure Init
   with
      Pre => CPU_Info.Is_BSP;

   --  Crash audit entry.
   type Entry_Type (<>) is private;

   Null_Entry : constant Entry_Type;

   --  Allocate new crash audit entry. If this operation fails because no crash
   --  audit entries are available, the calling CPU will be halted.
   procedure Allocate (Audit : out Entry_Type)
   with
      Global => (Input  => CPU_Info.APIC_ID,
                 In_Out => (State, X86_64.State)),
      Post   => Audit /= Null_Entry;

   --  Set crash reason for given entry.
   procedure Set_Reason
     (Audit  : Entry_Type;
      Reason : Crash_Audit_Types.Reason_Type)
   with
      Global => (In_Out => State),
      Pre    => Audit /= Null_Entry;

   --  Set exception context information for given entry and mark it as valid.
   procedure Set_Exception_Context
     (Audit   : Entry_Type;
      Context : Crash_Audit_Types.Exception_Context_Type)
   with
      Global => (In_Out => State),
      Pre    => Audit /= Null_Entry;

   --  Set MCE context information for given entry and mark it as valid.
   procedure Set_MCE_Context
     (Audit   : Entry_Type;
      Context : Crash_Audit_Types.MCE_Context_Type)
   with
      Global => (In_Out => State),
      Pre    => Audit /= Null_Entry;

   --  Set subject context information for given entry and mark it as valid.
   procedure Set_Subject_Context
     (Audit   : Entry_Type;
      Context : Crash_Audit_Types.Subj_Context_Type)
   with
      Global => (In_Out => State),
      Pre    => Audit /= Null_Entry;

   --  Set init context information for given entry and mark it as valid.
   procedure Set_Init_Context
     (Audit   : Entry_Type;
      Context : Crash_Audit_Types.Init_Context_Type)
   with
      Global => (In_Out => State),
      Pre    => Audit /= Null_Entry;

   --  Set VT-x context information for given entry and mark it as valid.
   procedure Set_VTx_Context
     (Audit   : Entry_Type;
      Context : Crash_Audit_Types.VTx_Context_Type)
   with
      Global => (In_Out => State),
      Pre    => Audit /= Null_Entry;

   --  Finalize crash audit by performing a warm system restart. By setting the
   --  generation counter to boot counter + 1, the crash dump will be active on
   --  the next reboot.
   --
   --  The procedure spins 100 ms before hitting reset to give other cores
   --  time to write their dumps. In debug mode, it will spin an additional 10
   --  seconds before resetting the system.
   procedure Finalize (Audit : Entry_Type)
   with
      Global => (In_Out => (State, X86_64.State)),
      Pre    => Audit /= Null_Entry,
      No_Return;

private

   type Entry_Type is record
      Slot : Crash_Audit_Types.Dumpdata_Length;
   end record;

   Null_Entry : constant Entry_Type
     := (Slot => Crash_Audit_Types.Dumpdata_Length'First);

end SK.Crash_Audit;
