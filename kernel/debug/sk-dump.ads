--
--  Copyright (C) 2013, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Skp.Interrupts;

with SK.Crash_Audit_Types;

package SK.Dump
with
   SPARK_Mode => Off
is

   Invalid_IRT_Idx : constant := 256;

   type IRT_Idx_Type is range Byte'First .. Invalid_IRT_Idx;

   --  Print IRQ Routing.
   procedure Print_IRQ_Routing
     (RTE_Idx     : Skp.Interrupts.RTE_Index_Type;
      IRQ         : Byte;
      Vector      : Byte;
      APIC_ID     : Byte;
      VTd_IRT_Idx : IRT_Idx_Type := Invalid_IRT_Idx);

   --  Print ISR execution environment state.
   procedure Print_ISR_State
     (Context : Crash_Audit_Types.Exception_Context_Type);

   --  Print message followed by a newline.
   procedure Print_Message (Msg : String);

   --  Print invalid event from userspace.
   procedure Print_Spurious_Event
     (Current_Subject : Skp.Global_Subject_ID_Type;
      Event_Nr        : Word64);

   --  Print CPU registers.
   procedure Print_Registers
     (Regs : CPU_Registers_Type;
      RIP, CS, RFL, RSP, SS, CR0, CR3, CR4 : Word64);

   --  Print CPU segment with given name.
   procedure Print_Segment
     (Name : String;
      Seg  : Segment_Type);

   --  Print VMX error information.
   procedure Print_VMX_Error
     (Reason  : Crash_Audit_Types.VTx_Reason_Range;
      Context : Crash_Audit_Types.VTx_Context_Type);

end SK.Dump;
