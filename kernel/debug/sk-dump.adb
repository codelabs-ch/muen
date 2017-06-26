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

with SK.KC;
with SK.CPU.VMX;
with SK.Constants;
with SK.Locks;
with SK.CPU_Info;
with SK.Scheduler;
with SK.Strings;
with SK.Dumper;

package body SK.Dump
with
   SPARK_Mode => Off
is

   use SK.Strings;

   package D is new Dumper
     (Output_New_Line   => KC.New_Line,
      Output_Put_Line   => KC.Put_Line,
      Output_Put_String => KC.Put_String);

   -------------------------------------------------------------------------

   procedure Print_Segment
     (Name : String;
      Seg  : Segment_Type)
      renames D.Output_Segment;

   -------------------------------------------------------------------------

   procedure Print_IRQ_Routing
     (RTE_Idx     : Skp.Interrupts.RTE_Index_Type;
      IRQ         : Byte;
      Vector      : Byte;
      APIC_ID     : Byte;
      VTd_IRT_Idx : IRT_Idx_Type := Invalid_IRT_Idx)
   is
   begin
      Locks.Acquire;
      KC.Put_String
        (Item => "I/O APIC RTE " & Img (Byte (RTE_Idx)) & ": Routing IRQ "
         & Img (IRQ) & " as vector " & Img (Vector) & " to CPU with APIC ID "
         & Img (APIC_ID));

      if VTd_IRT_Idx /= Invalid_IRT_Idx then
         KC.Put_String
           (Item => ", VT-d IRT index " & Img (Byte (VTd_IRT_Idx)));
      end if;

      KC.New_Line;
      Locks.Release;
   end Print_IRQ_Routing;

   -------------------------------------------------------------------------

   procedure Print_Registers
     (Regs : CPU_Registers_Type;
      RIP, CS, RFL, RSP, SS, CR0, CR3, CR4 : Word64)
      renames D.Output_Registers;

   -------------------------------------------------------------------------

   procedure Print_ISR_State
     (Context : Crash_Audit_Types.Exception_Context_Type)
   is
   begin
      Locks.Acquire;
      D.Output_ISR_State
        (Context => Context,
         APIC_ID => Byte (CPU_Info.APIC_ID));
      Locks.Release;
   end Print_ISR_State;

   -------------------------------------------------------------------------

   procedure Print_Message (Msg : String)
   is
   begin
      Locks.Acquire;
      KC.Put_Line (Item => Msg);
      Locks.Release;
   end Print_Message;

   -------------------------------------------------------------------------

   procedure Print_Spurious_Event
     (Current_Subject : Skp.Global_Subject_ID_Type;
      Event_Nr        : Word64)
   is
   begin
      Locks.Acquire;
      KC.Put_Line (Item => "Ignoring spurious event " & Img (Event_Nr)
                   & " from subject 0x" & Img (Byte (Current_Subject)));
      Locks.Release;
   end Print_Spurious_Event;

   -------------------------------------------------------------------------

   procedure Print_VMX_Error
   is
      Error     : Word64;
      Success   : Boolean;
      Subj_ID   : constant Skp.Global_Subject_ID_Type
        := Scheduler.Get_Current_Subject_ID;
      VMCS_Addr : Word64;
   begin
      Locks.Acquire;
      KC.Put_Line (Item => "VMX error details");
      KC.Put_Line (Item => "Active subject: " & Img (Byte (Subj_ID)));

      CPU.VMX.VMREAD
        (Field   => Constants.VMX_INST_ERROR,
         Value   => Error,
         Success => Success);
      if Success then
         KC.Put_Line
           (Item => "VM instruction error: " & Img (Byte (Error)));
      else
         KC.Put_Line (Item => "VMX instruction error not available");
      end if;

      CPU.VMX.VMPTRST
        (Region  => VMCS_Addr,
         Success => Success);
      if Success then
         KC.Put_Line
           (Item => "Current-VMCS pointer: " & Img (VMCS_Addr));
      else
         KC.Put_Line (Item => "Unable to read current-VMCS pointer");
      end if;
      Locks.Release;
   end Print_VMX_Error;

end SK.Dump;
