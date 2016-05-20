--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with Debuglog.Client;

with Subject_Info;

package body Debug_Ops
with
   SPARK_Mode => Off
is

   package Ifa renames Interfaces;

   -------------------------------------------------------------------------

   procedure Dump_State
   is
      use Subject_Info;
      use type SK.Word64;
   begin
      Debuglog.Client.Put
        (Item => "Halting associated subject after EXIT (");
      Debuglog.Client.Put_Word16 (Item => Ifa.Unsigned_16 (State.Exit_Reason));
      Debuglog.Client.Put (Item => ":");
      Debuglog.Client.Put_Word32
        (Item => Ifa.Unsigned_32 (State.Exit_Qualification));
      Debuglog.Client.Put (Item => ":");
      Debuglog.Client.Put_Word32
        (Item => Ifa.Unsigned_32 (State.Interrupt_Info));
      Debuglog.Client.Put_Line (Item => ")");

      if (State.IA32_EFER and 16#400#) = 0 then
         Debuglog.Client.Put ("EIP: ");
         Debuglog.Client.Put_Word32 (Item => Ifa.Unsigned_32 (State.RIP));
         Debuglog.Client.Put (" CS : ");
         Debuglog.Client.Put_Word16 (Item => Ifa.Unsigned_16
                                     (State.CS.Selector));
         Debuglog.Client.Put (" EFLAGS: ");
         Debuglog.Client.Put_Word32 (Item => Ifa.Unsigned_32 (State.RFLAGS));
         Debuglog.Client.New_Line;
         Debuglog.Client.Put ("ESP: ");
         Debuglog.Client.Put_Word32 (Item => Ifa.Unsigned_32 (State.RSP));
         Debuglog.Client.Put (" SS : ");
         Debuglog.Client.Put_Word16 (Item => Ifa.Unsigned_16
                                     (State.SS.Selector));
         Debuglog.Client.New_Line;

         Debuglog.Client.Put (Item => "EAX: ");
         Debuglog.Client.Put_Word32 (Item => Ifa.Unsigned_32 (State.Regs.RAX));
         Debuglog.Client.Put (Item => " EBX: ");
         Debuglog.Client.Put_Word32 (Item => Ifa.Unsigned_32 (State.Regs.RBX));
         Debuglog.Client.Put (Item => " ECX: ");
         Debuglog.Client.Put_Word32 (Item => Ifa.Unsigned_32 (State.Regs.RCX));
         Debuglog.Client.Put (Item => " EDX: ");
         Debuglog.Client.Put_Word32 (Item => Ifa.Unsigned_32 (State.Regs.RDX));
         Debuglog.Client.New_Line;

         Debuglog.Client.Put (Item => "ESI: ");
         Debuglog.Client.Put_Word32 (Item => Ifa.Unsigned_32 (State.Regs.RSI));
         Debuglog.Client.Put (Item => " EDI: ");
         Debuglog.Client.Put_Word32 (Item => Ifa.Unsigned_32 (State.Regs.RDI));
         Debuglog.Client.Put (Item => " EBP: ");
         Debuglog.Client.Put_Word32 (Item => Ifa.Unsigned_32 (State.Regs.RBP));
         Debuglog.Client.New_Line;

         Debuglog.Client.Put (Item => "CR0: ");
         Debuglog.Client.Put_Word32 (Item => Ifa.Unsigned_32 (State.CR0));
         Debuglog.Client.Put (Item => " CR2: ");
         Debuglog.Client.Put_Word32 (Item => Ifa.Unsigned_32 (State.Regs.CR2));
         Debuglog.Client.Put (Item => " CR3: ");
         Debuglog.Client.Put_Word32 (Item => Ifa.Unsigned_32 (State.CR3));
         Debuglog.Client.Put (Item => " CR4: ");
         Debuglog.Client.Put_Word32 (Item => Ifa.Unsigned_32 (State.CR4));
         Debuglog.Client.New_Line;
         Debuglog.Client.New_Line;

         Debuglog.Client.Put (Item => "Shadow CR0: ");
         Debuglog.Client.Put_Word32
           (Item => Ifa.Unsigned_32 (State.SHADOW_CR0));
         Debuglog.Client.New_Line;

         Debuglog.Client.Put (Item => "IA32_EFER : ");
         Debuglog.Client.Put_Word64
           (Item => Ifa.Unsigned_64 (State.IA32_EFER));
         Debuglog.Client.New_Line;
      else
         Debuglog.Client.Put ("RIP: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.RIP));
         Debuglog.Client.Put (" CS : ");
         Debuglog.Client.Put_Word16 (Item => Ifa.Unsigned_16
                                     (State.CS.Selector));
         Debuglog.Client.Put (" RFLAGS: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.RFLAGS));
         Debuglog.Client.New_Line;
         Debuglog.Client.Put ("RSP: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.RSP));
         Debuglog.Client.Put (" SS : ");
         Debuglog.Client.Put_Word16 (Item => Ifa.Unsigned_16
                                     (State.SS.Selector));
         Debuglog.Client.New_Line;

         Debuglog.Client.Put (Item => "RAX: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.Regs.RAX));
         Debuglog.Client.Put (Item => " RBX: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.Regs.RBX));
         Debuglog.Client.Put (Item => " RCX: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.Regs.RCX));
         Debuglog.Client.New_Line;

         Debuglog.Client.Put (Item => "RDX: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.Regs.RDX));
         Debuglog.Client.Put (Item => " RSI: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.Regs.RSI));
         Debuglog.Client.Put (Item => " RDI: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.Regs.RDI));
         Debuglog.Client.New_Line;

         Debuglog.Client.Put (Item => "RBP: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.Regs.RBP));
         Debuglog.Client.Put (Item => " R08: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.Regs.R08));
         Debuglog.Client.Put (Item => " R09: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.Regs.R09));
         Debuglog.Client.New_Line;

         Debuglog.Client.Put (Item => "R10: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.Regs.R10));
         Debuglog.Client.Put (Item => " R11: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.Regs.R11));
         Debuglog.Client.Put (Item => " R12: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.Regs.R12));
         Debuglog.Client.New_Line;

         Debuglog.Client.Put (Item => "R13: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.Regs.R13));
         Debuglog.Client.Put (Item => " R14: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.Regs.R14));
         Debuglog.Client.Put (Item => " R15: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.Regs.R15));
         Debuglog.Client.New_Line;

         Debuglog.Client.Put (Item => "CR0: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.CR0));
         Debuglog.Client.Put (Item => " CR2: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.Regs.CR2));
         Debuglog.Client.New_Line;
         Debuglog.Client.Put (Item => "CR3: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.CR3));
         Debuglog.Client.Put (Item => " CR4: ");
         Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (State.CR4));
         Debuglog.Client.New_Line;
         Debuglog.Client.New_Line;

         Debuglog.Client.Put (Item => "Shadow CR0: ");
         Debuglog.Client.Put_Word64
           (Item => Ifa.Unsigned_64 (State.SHADOW_CR0));
         Debuglog.Client.New_Line;

         Debuglog.Client.Put (Item => "IA32_EFER : ");
         Debuglog.Client.Put_Word64
           (Item => Ifa.Unsigned_64 (State.IA32_EFER));
         Debuglog.Client.New_Line;
      end if;
   end Dump_State;

   -------------------------------------------------------------------------

   procedure Put_Line (Item : String) renames Debuglog.Client.Put_Line;

   -------------------------------------------------------------------------

   procedure Put_String (Item : String) renames Debuglog.Client.Put;

   -------------------------------------------------------------------------

   procedure Put_Value8
     (Message : String;
      Value   : SK.Byte)
   is
   begin
      Debuglog.Client.Put_Reg8
        (Name  => Message,
         Value => Ifa.Unsigned_8 (Value));
   end Put_Value8;

   -------------------------------------------------------------------------

   procedure Put_Value16
     (Message : String;
      Value   : SK.Word16)
   is
   begin
      Debuglog.Client.Put_Reg16
        (Name  => Message,
         Value => Ifa.Unsigned_16 (Value));
   end Put_Value16;

   -------------------------------------------------------------------------

   procedure Put_Value32
     (Message : String;
      Value   : SK.Word32)
   is
   begin
      Debuglog.Client.Put_Reg32
        (Name  => Message,
         Value => Ifa.Unsigned_32 (Value));
   end Put_Value32;

   -------------------------------------------------------------------------

   procedure Put_Value64
     (Message : String;
      Value   : SK.Word64)
   is
   begin
      Debuglog.Client.Put_Reg64
        (Name  => Message,
         Value => Ifa.Unsigned_64 (Value));
   end Put_Value64;

   -------------------------------------------------------------------------

   procedure Put_Word16 (Item : SK.Word16)
   is
   begin
      Debuglog.Client.Put_Word16 (Item => Ifa.Unsigned_16 (Item));
   end Put_Word16;

   -------------------------------------------------------------------------

   procedure Put_Word64 (Item : SK.Word64)
   is
   begin
      Debuglog.Client.Put_Word64 (Item => Ifa.Unsigned_64 (Item));
   end Put_Word64;

end Debug_Ops;
