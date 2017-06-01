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

package body SK.Dump
with
   SPARK_Mode => Off
is

   use SK.Strings;

   -------------------------------------------------------------------------

   procedure Print_Segment
     (Name : String;
      Seg  : Segment_Type)
   is
   begin
      KC.Put_String (Item => Name);
      KC.Put_Line
        (Item => ": " & Img (Word16 (Seg.Selector))
         & ":" & Img (Seg.Base)
         & ":" & Img (Seg.Limit)
         & ":" & Img (Seg.Access_Rights));
   end Print_Segment;

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
   is
   begin
      KC.Put_Line (Item => "RIP: " & Img (RIP) & " CS : " & Img (Word16 (CS)));
      KC.Put_Line (Item => "RSP: " & Img (RSP) & " SS : " & Img (Word16 (SS)));

      KC.Put_Line (Item => "RAX: " & Img (Regs.RAX)
                   & " RBX: " & Img (Regs.RBX)
                   & " RCX: " & Img (Regs.RCX));

      KC.Put_Line (Item => "RDX: " & Img (Regs.RDX)
                   & " RSI: " & Img (Regs.RSI)
                   & " RDI: " & Img (Regs.RDI));

      KC.Put_Line (Item => "RBP: " & Img (Regs.RBP)
                   & " R08: " & Img (Regs.R08)
                   & " R09: " & Img (Regs.R09));

      KC.Put_Line (Item => "R10: " & Img (Regs.R10) & " R11: " & Img (Regs.R11)
                   & " R12: " & Img (Regs.R12));
      KC.Put_Line (Item => "R13: " & Img (Regs.R13) & " R14: " & Img (Regs.R14)
                   & " R15: " & Img (Regs.R15));

      KC.Put_Line (Item => "CR0: " & Img (CR0) & " CR2: " & Img (Regs.CR2)
                   & " CR3: " & Img (CR3));
      KC.Put_Line (Item => "CR4: " & Img (CR4) & " EFL: "
                   & Img (Word32 (RFL)));
   end Print_Registers;

   -------------------------------------------------------------------------

   procedure Print_ISR_State (Context : Isr_Context_Type)
   is
   begin
      Locks.Acquire;
      KC.New_Line;
      KC.Put_Line (Item => "[CPU " & Img (Byte (CPU_Info.CPU_ID))
                   & " KERNEL PANIC]");

      KC.Put_Line (Item => "Vector: " & Img (Byte (Context.Vector))
                   & ", Error: " & Img (Context.Error_Code));
      KC.New_Line;
      Print_Registers (Regs => Context.Regs,
                       RIP  => Context.RIP,
                       CS   => Context.CS,
                       RFL  => Context.RFLAGS,
                       RSP  => Context.RSP,
                       SS   => Context.SS,
                       CR0  => CPU.Get_CR0,
                       CR3  => CPU.Get_CR3,
                       CR4  => CPU.Get_CR4);
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

   procedure Print_Message_8 (Msg : String; Item : Byte)
   is
   begin
      Locks.Acquire;
      KC.Put_String (Item => Msg);
      KC.Put_String (Item => " ");
      KC.Put_Byte   (Item => Item);
      KC.New_Line;
      Locks.Release;
   end Print_Message_8;

   -------------------------------------------------------------------------

   procedure Print_Message_16 (Msg : String; Item : Word16)
   is
   begin
      Locks.Acquire;
      KC.Put_String (Item => Msg);
      KC.Put_String (Item => " ");
      KC.Put_Word16 (Item => Item);
      KC.New_Line;
      Locks.Release;
   end Print_Message_16;

   -------------------------------------------------------------------------

   procedure Print_Message_32 (Msg : String; Item : Word32)
   is
   begin
      Locks.Acquire;
      KC.Put_String (Item => Msg);
      KC.Put_String (Item => " ");
      KC.Put_Word32 (Item => Item);
      KC.New_Line;
      Locks.Release;
   end Print_Message_32;

   -------------------------------------------------------------------------

   procedure Print_Message_64 (Msg : String; Item : Word64)
   is
   begin
      Locks.Acquire;
      KC.Put_String (Item => Msg);
      KC.Put_String (Item => " ");
      KC.Put_Word64 (Item => Item);
      KC.New_Line;
      Locks.Release;
   end Print_Message_64;

   -------------------------------------------------------------------------

   procedure Print_Spurious_Event
     (Current_Subject : Skp.Subject_Id_Type;
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
      Subj_ID   : constant Skp.Subject_Id_Type
        := Scheduler.Get_Current_Subject_ID;
      VMCS_Addr : Word64;
   begin
      Locks.Acquire;
      KC.Put_Line   (Item => "VMX error details");
      KC.Put_String (Item => "Active subject: 16#");
      KC.Put_Byte   (Item => SK.Byte (Subj_ID));
      KC.Put_Line   (Item => "#");

      CPU.VMX.VMREAD
        (Field   => Constants.VMX_INST_ERROR,
         Value   => Error,
         Success => Success);
      if Success then
         KC.Put_String (Item => "VM instruction error: 16#");
         KC.Put_Byte   (Item => Byte (Error));
         KC.Put_Line   (Item => "#");
      else
         KC.Put_Line (Item => "VMX instruction error not available");
      end if;

      CPU.VMX.VMPTRST
        (Region  => VMCS_Addr,
         Success => Success);
      if Success then
         KC.Put_String (Item => "Current-VMCS pointer: 16#");
         KC.Put_Word64 (Item => VMCS_Addr);
         KC.Put_Line   (Item => "#");
      else
         KC.Put_Line (Item => "Unable to read current-VMCS pointer");
      end if;
      Locks.Release;
   end Print_VMX_Error;

end SK.Dump;
