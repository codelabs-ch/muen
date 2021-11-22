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

with System;

with SK.KC;
with SK.Debug_Lock;
with SK.CPU_Info;
with SK.Strings;
with SK.Dumper;

package body SK.Dump
with
   SPARK_Mode => Off
is

   use SK.Strings;

   package D is new Dumper
     (New_Line   => KC.New_Line,
      Put_Line   => KC.Put_Line,
      Put_String => KC.Put_String);

   -------------------------------------------------------------------------

   procedure Dump_Page (Address : SK.Word64)
   is
      Mem_Page : array (0 .. SK.Page_Size - 1) of SK.Byte
      with
         Address => System'To_Address (Address);
   begin
      for I in Mem_Page'Range loop
         if I mod 16 = 0 then
            KC.New_Line;
            KC.Put_String (Item => SK.Strings.Img (SK.Word16 (I)) & ": ");
         end if;
         KC.Put_String
           (Item => SK.Strings.Img_Nobase (Item => Mem_Page (I)) & " ");
      end loop;
      KC.New_Line;
      KC.New_Line;
   end Dump_Page;

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
      Debug_Lock.Acquire;
      KC.Put_String
        (Item => "I/O APIC RTE " & Img (Byte (RTE_Idx)) & ": Routing IRQ "
         & Img (IRQ) & " as vector " & Img (Vector) & " to CPU with APIC ID "
         & Img (APIC_ID));

      if VTd_IRT_Idx /= Invalid_IRT_Idx then
         KC.Put_String
           (Item => ", VT-d IRT index " & Img (Byte (VTd_IRT_Idx)));
      end if;

      KC.New_Line;
      Debug_Lock.Release;
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
      Debug_Lock.Acquire;
      D.Output_ISR_State
        (Context => Context,
         APIC_ID => Byte (CPU_Info.APIC_ID));
      Debug_Lock.Release;
   end Print_ISR_State;

   -------------------------------------------------------------------------

   procedure Print_MCE_State (Context : Crash_Audit_Types.MCE_Context_Type)
   is
   begin
      Debug_Lock.Acquire;
      D.Output_MCE_State (Context => Context);
      Debug_Lock.Release;
   end Print_MCE_State;

   -------------------------------------------------------------------------

   procedure Print_MCU_Header (Hdr : MCU.Header_Type)
   is
   begin
      Debug_Lock.Acquire;
      KC.Put_Line (Item => "MCU: Header version      : "
                   & Strings.Img (Hdr.Header_Version));
      KC.Put_Line (Item => "MCU: Update revision     : "
                   & Strings.Img (Hdr.Update_Revision));
      KC.Put_Line (Item => "MCU: Update date         : "
                   & Strings.Img (Hdr.Date));
      KC.Put_Line (Item => "MCU: Processor signature : "
                   & Strings.Img (Hdr.Processor_Signature));
      KC.Put_Line (Item => "MCU: Loader revision     : "
                   & Strings.Img (Hdr.Loader_Revision));
      KC.Put_Line (Item => "MCU: Processor flags     : "
                   & Strings.Img (Hdr.Processor_Flags));
      Debug_Lock.Release;
   end Print_MCU_Header;

   -------------------------------------------------------------------------

   procedure Print_Message (Msg : String)
   is
   begin
      Debug_Lock.Acquire;
      KC.Put_Line (Item => Msg);
      Debug_Lock.Release;
   end Print_Message;

   -------------------------------------------------------------------------

   procedure Print_Spurious_Event
     (Current_Subject : Skp.Global_Subject_ID_Type;
      Event_Nr        : Word64)
   is
   begin
      Debug_Lock.Acquire;
      KC.Put_Line (Item => "Ignoring spurious event " & Img (Event_Nr)
                   & " from subject 0x" & Img (Byte (Current_Subject)));
      Debug_Lock.Release;
   end Print_Spurious_Event;

   -------------------------------------------------------------------------

   procedure Print_VMX_Error
     (Reason  : Crash_Audit_Types.VTx_Reason_Range;
      Context : Crash_Audit_Types.VTx_Context_Type)
   is
   begin
      Debug_Lock.Acquire;
      D.Output_VMX_Error (Reason  => Reason,
                          Context => Context);
      Debug_Lock.Release;
   end Print_VMX_Error;

end SK.Dump;
