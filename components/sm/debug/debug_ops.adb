--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Subject.Text_IO;

with Subject_Info;

package body Debug_Ops
with
   SPARK_Mode => Off
is

   -------------------------------------------------------------------------

   procedure Dump_State
   is
      use Subject_Info;
      use type SK.Word64;
   begin
      Subject.Text_IO.Put_String
        (Item => "Halting associated subject after EXIT (");
      Subject.Text_IO.Put_Word16 (Item => SK.Word16 (State.Exit_Reason));
      Subject.Text_IO.Put_String (Item => ":");
      Subject.Text_IO.Put_Word32
        (Item => SK.Word32 (State.Exit_Qualification));
      Subject.Text_IO.Put_String (Item => ":");
      Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Interrupt_Info));
      Subject.Text_IO.Put_Line   (Item => ")");

      if (State.IA32_EFER and 16#400#) = 0 then
         Subject.Text_IO.Put_String ("EIP: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.RIP));
         Subject.Text_IO.Put_String (" CS : ");
         Subject.Text_IO.Put_Word16 (Item => SK.Word16 (State.CS));
         Subject.Text_IO.Put_String (" EFLAGS: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.RFLAGS));
         Subject.Text_IO.New_Line;
         Subject.Text_IO.Put_String ("ESP: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.RSP));
         Subject.Text_IO.Put_String (" SS : ");
         Subject.Text_IO.Put_Word16 (Item => SK.Word16 (State.SS));
         Subject.Text_IO.New_Line;

         Subject.Text_IO.Put_String (Item => "EAX: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RAX));
         Subject.Text_IO.Put_String (Item => " EBX: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RBX));
         Subject.Text_IO.Put_String (Item => " ECX: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RCX));
         Subject.Text_IO.Put_String (Item => " EDX: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RDX));
         Subject.Text_IO.New_Line;

         Subject.Text_IO.Put_String (Item => "ESI: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RSI));
         Subject.Text_IO.Put_String (Item => " EDI: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RDI));
         Subject.Text_IO.Put_String (Item => " EBP: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RBP));
         Subject.Text_IO.New_Line;

         Subject.Text_IO.Put_String (Item => "CR0: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.CR0));
         Subject.Text_IO.Put_String (Item => " CR2: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.CR2));
         Subject.Text_IO.Put_String (Item => " CR3: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.CR3));
         Subject.Text_IO.Put_String (Item => " CR4: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.CR4));
         Subject.Text_IO.New_Line;
         Subject.Text_IO.New_Line;

         Subject.Text_IO.Put_String (Item => "Shadow CR0: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.SHADOW_CR0));
         Subject.Text_IO.New_Line;

         Subject.Text_IO.Put_String (Item => "IA32_EFER : ");
         Subject.Text_IO.Put_Word64 (Item => State.IA32_EFER);
         Subject.Text_IO.New_Line;
      else
         Subject.Text_IO.Put_String ("RIP: ");
         Subject.Text_IO.Put_Word64 (Item => State.RIP);
         Subject.Text_IO.Put_String (" CS : ");
         Subject.Text_IO.Put_Word16 (Item => SK.Word16 (State.CS));
         Subject.Text_IO.Put_String (" RFLAGS: ");
         Subject.Text_IO.Put_Word64 (Item => State.RFLAGS);
         Subject.Text_IO.New_Line;
         Subject.Text_IO.Put_String ("RSP: ");
         Subject.Text_IO.Put_Word64 (Item => State.RSP);
         Subject.Text_IO.Put_String (" SS : ");
         Subject.Text_IO.Put_Word16 (Item => SK.Word16 (State.SS));
         Subject.Text_IO.New_Line;

         Subject.Text_IO.Put_String (Item => "RAX: ");
         Subject.Text_IO.Put_Word64 (Item => State.Regs.RAX);
         Subject.Text_IO.Put_String (Item => " RBX: ");
         Subject.Text_IO.Put_Word64 (Item => State.Regs.RBX);
         Subject.Text_IO.Put_String (Item => " RCX: ");
         Subject.Text_IO.Put_Word64 (Item => State.Regs.RCX);
         Subject.Text_IO.New_Line;

         Subject.Text_IO.Put_String (Item => "RDX: ");
         Subject.Text_IO.Put_Word64 (Item => State.Regs.RDX);
         Subject.Text_IO.Put_String (Item => " RSI: ");
         Subject.Text_IO.Put_Word64 (Item => State.Regs.RSI);
         Subject.Text_IO.Put_String (Item => " RDI: ");
         Subject.Text_IO.Put_Word64 (Item => State.Regs.RDI);
         Subject.Text_IO.New_Line;

         Subject.Text_IO.Put_String (Item => "RBP: ");
         Subject.Text_IO.Put_Word64 (Item => State.Regs.RBP);
         Subject.Text_IO.Put_String (Item => " R08: ");
         Subject.Text_IO.Put_Word64 (Item => State.Regs.R08);
         Subject.Text_IO.Put_String (Item => " R09: ");
         Subject.Text_IO.Put_Word64 (Item => State.Regs.R09);
         Subject.Text_IO.New_Line;

         Subject.Text_IO.Put_String (Item => "R10: ");
         Subject.Text_IO.Put_Word64 (Item => State.Regs.R10);
         Subject.Text_IO.Put_String (Item => " R11: ");
         Subject.Text_IO.Put_Word64 (Item => State.Regs.R11);
         Subject.Text_IO.Put_String (Item => " R12: ");
         Subject.Text_IO.Put_Word64 (Item => State.Regs.R12);
         Subject.Text_IO.New_Line;

         Subject.Text_IO.Put_String (Item => "R13: ");
         Subject.Text_IO.Put_Word64 (Item => State.Regs.R13);
         Subject.Text_IO.Put_String (Item => " R14: ");
         Subject.Text_IO.Put_Word64 (Item => State.Regs.R14);
         Subject.Text_IO.Put_String (Item => " R15: ");
         Subject.Text_IO.Put_Word64 (Item => State.Regs.R15);
         Subject.Text_IO.New_Line;

         Subject.Text_IO.Put_String (Item => "CR0: ");
         Subject.Text_IO.Put_Word64 (Item => State.CR0);
         Subject.Text_IO.Put_String (Item => " CR2: ");
         Subject.Text_IO.Put_Word64 (Item => State.CR2);
         Subject.Text_IO.New_Line;
         Subject.Text_IO.Put_String (Item => "CR3: ");
         Subject.Text_IO.Put_Word64 (Item => State.CR3);
         Subject.Text_IO.Put_String (Item => " CR4: ");
         Subject.Text_IO.Put_Word64 (Item => State.CR4);
         Subject.Text_IO.New_Line;
         Subject.Text_IO.New_Line;

         Subject.Text_IO.Put_String (Item => "Shadow CR0: ");
         Subject.Text_IO.Put_Word64 (Item => State.SHADOW_CR0);
         Subject.Text_IO.New_Line;

         Subject.Text_IO.Put_String (Item => "IA32_EFER : ");
         Subject.Text_IO.Put_Word64 (Item => State.IA32_EFER);
         Subject.Text_IO.New_Line;
      end if;
   end Dump_State;

   -------------------------------------------------------------------------

   procedure Put_Line (Item : String) renames Subject.Text_IO.Put_Line;

   -------------------------------------------------------------------------

   procedure Put_String (Item : String) renames Subject.Text_IO.Put_String;

   -------------------------------------------------------------------------

   procedure Put_Value8
     (Message : String;
      Value   : SK.Byte)
   is
   begin
      Subject.Text_IO.Put_String (Item => Message);
      Subject.Text_IO.Put_String (Item => " 16#");
      Subject.Text_IO.Put_Byte   (Item => Value);
      Subject.Text_IO.Put_Line   (Item => "#");
   end Put_Value8;

   -------------------------------------------------------------------------

   procedure Put_Value16
     (Message : String;
      Value   : SK.Word16)
   is
   begin
      Subject.Text_IO.Put_String (Item => Message);
      Subject.Text_IO.Put_String (Item => " 16#");
      Subject.Text_IO.Put_Word16 (Item => Value);
      Subject.Text_IO.Put_Line   (Item => "#");
   end Put_Value16;

   -------------------------------------------------------------------------

   procedure Put_Value32
     (Message : String;
      Value   : SK.Word32)
   is
   begin
      Subject.Text_IO.Put_String (Item => Message);
      Subject.Text_IO.Put_String (Item => " 16#");
      Subject.Text_IO.Put_Word32 (Item => Value);
      Subject.Text_IO.Put_Line   (Item => "#");
   end Put_Value32;

   -------------------------------------------------------------------------

   procedure Put_Value64
     (Message : String;
      Value   : SK.Word64)
   is
   begin
      Subject.Text_IO.Put_String (Item => Message);
      Subject.Text_IO.Put_String (Item => " 16#");
      Subject.Text_IO.Put_Word64 (Item => Value);
      Subject.Text_IO.Put_Line   (Item => "#");
   end Put_Value64;

   -------------------------------------------------------------------------

   procedure Put_Word16 (Item : SK.Word16) renames Subject.Text_IO.Put_Word16;

   -------------------------------------------------------------------------

   procedure Put_Word64 (Item : SK.Word64) renames Subject.Text_IO.Put_Word64;

end Debug_Ops;
