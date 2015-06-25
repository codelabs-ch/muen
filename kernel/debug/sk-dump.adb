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
with SK.CPU;
with SK.Apic;
with SK.Constants;
with SK.Locks;
with SK.CPU_Global;
with SK.Subjects;

package body SK.Dump
with
   SPARK_Mode => Off
is

   -------------------------------------------------------------------------

   procedure Print_Segment
     (Name : String;
      Seg  : Segment_Type)
   is
   begin
      Locks.Acquire;
      KC.Put_String (Item => Name);
      KC.Put_String (Item => ":  ");
      KC.Put_Word16 (Item => SK.Word16 (Seg.Selector));
      KC.Put_String (Item => ":");
      KC.Put_Word64 (Item => Seg.Base);
      KC.Put_String (Item => ":");
      KC.Put_Word32 (Item => Seg.Limit);
      KC.Put_String (Item => ":");
      KC.Put_Word32 (Item => Seg.Access_Rights);
      KC.New_Line;
      Locks.Release;
   end Print_Segment;

   -------------------------------------------------------------------------

   procedure Print_CPU_IDs
     (CPU_ID  : SK.Byte;
      APIC_ID : SK.Byte)
   is
   begin
      Locks.Acquire;
      KC.Put_String (Item => "CPU ");
      KC.Put_Byte   (Item => CPU_ID);
      KC.Put_String (Item => " registered with APIC ID ");
      KC.Put_Byte   (Item => APIC_ID);
      KC.New_Line;
      Locks.Release;
   end Print_CPU_IDs;

   -------------------------------------------------------------------------

   procedure Print_IRQ_Routing
     (RTE_Index    : Skp.Interrupts.RTE_Index_Type;
      IRQ          : SK.Byte;
      Vector       : SK.Byte;
      CPU_ID       : SK.Byte;
      Dest_ID      : SK.Byte;
      Dest_ID_Name : String)
   is
   begin
      Locks.Acquire;
      KC.Put_String (Item => "I/O APIC RTE ");
      KC.Put_Byte   (Item => SK.Byte (RTE_Index));
      KC.Put_String (Item => ": Routing IRQ ");
      KC.Put_Byte   (Item => IRQ);
      KC.Put_String (Item => " as vector ");
      KC.Put_Byte   (Item => Vector);
      KC.Put_String (Item => " to CPU ");
      KC.Put_Byte   (Item => CPU_ID);
      KC.Put_String (Item => ", ");
      KC.Put_String (Item => Dest_ID_Name);
      KC.Put_String (Item => " ");
      KC.Put_Byte (Item => Dest_ID);
      KC.New_Line;
      Locks.Release;
   end Print_IRQ_Routing;

   -------------------------------------------------------------------------

   procedure Print_Registers
     (Regs : CPU_Registers_Type;
      RIP, CS, RFL, RSP, SS, CR0, CR3, CR4 : Word64)
   is
   begin
      Locks.Acquire;
      KC.Put_String ("RIP: ");
      KC.Put_Word64 (Item => RIP);
      KC.Put_String (" CS : ");
      KC.Put_Word16 (Item => Word16 (CS));
      KC.New_Line;
      KC.Put_String ("RSP: ");
      KC.Put_Word64 (Item => RSP);
      KC.Put_String (" SS : ");
      KC.Put_Word16 (Item => Word16 (SS));
      KC.New_Line;

      KC.Put_String (Item => "RAX: ");
      KC.Put_Word64 (Item => Regs.RAX);
      KC.Put_String (Item => " RBX: ");
      KC.Put_Word64 (Item => Regs.RBX);
      KC.Put_String (Item => " RCX: ");
      KC.Put_Word64 (Item => Regs.RCX);
      KC.New_Line;

      KC.Put_String (Item => "RDX: ");
      KC.Put_Word64 (Item => Regs.RDX);
      KC.Put_String (Item => " RSI: ");
      KC.Put_Word64 (Item => Regs.RSI);
      KC.Put_String (Item => " RDI: ");
      KC.Put_Word64 (Item => Regs.RDI);
      KC.New_Line;

      KC.Put_String (Item => "RBP: ");
      KC.Put_Word64 (Item => Regs.RBP);
      KC.Put_String (Item => " R08: ");
      KC.Put_Word64 (Item => Regs.R08);
      KC.Put_String (Item => " R09: ");
      KC.Put_Word64 (Item => Regs.R09);
      KC.New_Line;

      KC.Put_String (Item => "R10: ");
      KC.Put_Word64 (Item => Regs.R10);
      KC.Put_String (Item => " R11: ");
      KC.Put_Word64 (Item => Regs.R11);
      KC.Put_String (Item => " R12: ");
      KC.Put_Word64 (Item => Regs.R12);
      KC.New_Line;

      KC.Put_String (Item => "R13: ");
      KC.Put_Word64 (Item => Regs.R13);
      KC.Put_String (Item => " R14: ");
      KC.Put_Word64 (Item => Regs.R14);
      KC.Put_String (Item => " R15: ");
      KC.Put_Word64 (Item => Regs.R15);
      KC.New_Line;

      KC.Put_String (Item => "CR0: ");
      KC.Put_Word64 (Item => CR0);
      KC.Put_String (Item => " CR2: ");
      KC.Put_Word64 (Item => Regs.CR2);
      KC.Put_String (Item => " CR3: ");
      KC.Put_Word64 (Item => CR3);
      KC.New_Line;

      KC.Put_String (Item => "CR4: ");
      KC.Put_Word64 (Item => CR4);
      KC.Put_String (" EFL: ");
      KC.Put_Word32 (Item => Word32 (RFL));
      KC.New_Line;
      Locks.Release;
   end Print_Registers;

   -------------------------------------------------------------------------

   procedure Print_ISR_State (Context : Isr_Context_Type)
   is
   begin
      Locks.Acquire;
      KC.New_Line;
      KC.Put_String (Item => "[CPU ");
      KC.Put_Byte   (Item => Byte (CPU_Global.CPU_ID));
      KC.Put_String (Item => ":");
      KC.Put_Byte   (Item => Apic.Get_ID);
      KC.Put_Line   (Item => " KERNEL PANIC]");

      KC.Put_String (Item => "Vector: ");
      KC.Put_Byte   (Item => Byte (Context.Vector));
      KC.Put_String (Item => ", Error: ");
      KC.Put_Word64 (Item => Context.Error_Code);
      KC.New_Line;
      KC.New_Line;
      Locks.Release;

      Print_Registers (Regs => Context.Regs,
                       RIP  => Context.RIP,
                       CS   => Context.CS,
                       RFL  => Context.RFLAGS,
                       RSP  => Context.RSP,
                       SS   => Context.SS,
                       CR0  => CPU.Get_CR0,
                       CR3  => CPU.Get_CR3,
                       CR4  => CPU.Get_CR4);
   end Print_ISR_State;

   -------------------------------------------------------------------------

   procedure Print_Message_8 (Msg : String; Item : SK.Byte)
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

   procedure Print_Message_16 (Msg : String; Item : SK.Word16)
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

   procedure Print_Message_32 (Msg : String; Item : SK.Word32)
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

   procedure Print_Message_64 (Msg : String; Item : SK.Word64)
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
      Event_Nr        : SK.Word64)
   is
   begin
      Locks.Acquire;
      KC.Put_String (Item => "Ignoring spurious event ");
      KC.Put_Word64 (Item => Event_Nr);
      KC.Put_String (Item => " from subject 0x");
      KC.Put_Byte   (Item => SK.Byte (Current_Subject));
      KC.New_Line;
      Locks.Release;
   end Print_Spurious_Event;

   -------------------------------------------------------------------------

   procedure Print_Subject (Subject_Id : Skp.Subject_Id_Type)
   is
      State : SK.Subject_State_Type;
   begin
      Locks.Acquire;
      State := Subjects.Get_State (Id => Subject_Id);
      KC.Put_String (Item => "Subject 0x");
      KC.Put_Byte   (Item =>  Byte (Subject_Id));
      KC.Put_String (Item => ", Exit info ");
      KC.Put_Word16 (Item => Word16 (State.Exit_Reason));
      KC.Put_String (Item => ":");
      KC.Put_Word32 (Item => Word32 (State.Exit_Qualification));
      KC.Put_String (Item => ":");
      KC.Put_Word32 (Item => Word32 (State.Interrupt_Info));
      KC.New_Line;
      Locks.Release;
      Print_Registers (Regs => State.Regs,
                       RIP  => State.RIP,
                       CS   => State.CS,
                       RFL  => State.RFLAGS,
                       RSP  => State.RSP,
                       SS   => State.SS,
                       CR0  => State.CR0,
                       CR3  => State.CR3,
                       CR4  => State.CR4);
   end Print_Subject;

   -------------------------------------------------------------------------

   procedure Print_VMX_Entry_Error
     (Current_Subject : Skp.Subject_Id_Type;
      Exit_Reason     : SK.Word64)
   is
   begin
      Locks.Acquire;
      KC.Put_Line (Item => "VM-entry failure");
      Locks.Release;
      Print_Subject (Subject_Id => Current_Subject);
   end Print_VMX_Entry_Error;

   -------------------------------------------------------------------------

   procedure Print_VMX_Error
   is
      Error   : SK.Word64;
      Success : Boolean;
   begin
      Locks.Acquire;
      KC.Put_String (Item => "Error running subject 0x");
      KC.Put_Byte   (Item => SK.Byte (CPU_Global.Get_Current_Subject_ID));
      KC.New_Line;

      CPU.VMREAD (Field   => Constants.VMX_INST_ERROR,
                  Value   => Error,
                  Success => Success);

      if Success then
         KC.Put_String (Item => "VM instruction error: ");
         KC.Put_Byte   (Item => Byte (Error));
         KC.New_Line;
      else
         KC.Put_Line   (Item => "Unable to read VMX instruction error");
      end if;
      Locks.Release;
   end Print_VMX_Error;

end SK.Dump;
