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

with SK.Bitops;
with SK.Strings;

with Debuglog.Client;

with Subject_Info;

package body Debug_Ops
with
   SPARK_Mode => Off
is

   use SK.Strings;

   package Ifa renames Interfaces;

   --  Max. writable bit position for each access width. Used in PCI config
   --  space write path to emit warnings.
   Max_Write_Widths : constant array (0 .. 2) of SK.Byte
     := (0 => 7, 1 => 15, 2 => 31);

   procedure Find_Highest_Bit_Set is new SK.Bitops.Find_Highest_Bit_Set
     (Search_Range => SK.Bitops.Word64_Pos);

   -------------------------------------------------------------------------

   procedure Check_Warn_PCI_Write_Width
     (Value     : Interfaces.Unsigned_64;
      Width_Idx : Natural)
   is
      use type Interfaces.Unsigned_8;

      High_Bit : SK.Bitops.Word64_Pos;
      Found    : Boolean;
      RIP      : constant SK.Word64 := Subject_Info.State.RIP;
   begin
      Find_Highest_Bit_Set
        (Field => Value,
         Found => Found,
         Pos   => High_Bit);
      if Found and then SK.Byte (High_Bit) > Max_Write_Widths (Width_Idx)
      then
         pragma Debug
           (Debug_Ops.Put_Line
              (Item => "Pciconf: WARNING code @ RIP "
               & SK.Strings.Img (RIP) & " tries to write bit position "
               & SK.Strings.Img (SK.Byte (High_Bit))
               & " instead of allowed max "
               & SK.Strings.Img (Max_Write_Widths (Width_Idx))));
      end if;
   end Check_Warn_PCI_Write_Width;

   -------------------------------------------------------------------------

   procedure Dump_State
   is
      use Subject_Info;
      use type SK.Word64;
   begin
      Debuglog.Client.Put
        (Item => "Halting associated subject after EXIT (");
      Debuglog.Client.Put_Line
        (Item => Img (Ifa.Unsigned_16 (State.Exit_Reason))
         & ":" & Img (State.Exit_Qualification) & ")");

      if (State.IA32_EFER and 16#400#) = 0 then
         Debuglog.Client.Put_Line
           (Item => "EIP: " & Img (Ifa.Unsigned_32 (State.RIP))
            & " CS : " & Img (Ifa.Unsigned_16 (State.Segment_Regs.CS.Selector))
            & " EFLAGS: " & Img (Ifa.Unsigned_32 (State.RFLAGS)));
         Debuglog.Client.Put_Line
           (Item => "ESP: " & Img (Ifa.Unsigned_32 (State.RSP))
            & " SS : "
            & Img (Ifa.Unsigned_16 (State.Segment_Regs.SS.Selector)));

         Debuglog.Client.Put_Line
           (Item => "EAX: " & Img (Ifa.Unsigned_32 (State.Regs.RAX))
            & " EBX: " & Img (Ifa.Unsigned_32 (State.Regs.RBX))
            & " ECX: " & Img (Ifa.Unsigned_32 (State.Regs.RCX))
            & " EDX: " & Img (Ifa.Unsigned_32 (State.Regs.RDX)));

         Debuglog.Client.Put_Line
           (Item => "ESI: " & Img (Ifa.Unsigned_32 (State.Regs.RSI))
            & " EDI: " & Img (Ifa.Unsigned_32 (State.Regs.RDI))
            & " EBP: " & Img (Ifa.Unsigned_32 (State.Regs.RBP)));

         Debuglog.Client.Put_Line
           (Item => "CR0: " & Img (Ifa.Unsigned_32 (State.CR0))
            & " CR2: " & Img (Ifa.Unsigned_32 (State.Regs.CR2))
            & " CR3: " & Img (Ifa.Unsigned_32 (State.CR3))
            & " CR4: " & Img (Ifa.Unsigned_32 (State.CR4)));

         Debuglog.Client.New_Line;

         Debuglog.Client.Put_Line
           (Item => "Shadow CR0: " & Img (Ifa.Unsigned_32 (State.SHADOW_CR0)));
         Debuglog.Client.Put_Line
           (Item => "IA32_EFER : " & Img (State.IA32_EFER));
      else
         Debuglog.Client.Put_Line
           (Item => "RIP: " & Img (State.RIP)
            & " CS : " & Img (Ifa.Unsigned_16 (State.Segment_Regs.CS.Selector))
            & " RFLAGS: " & Img (State.RFLAGS));
         Debuglog.Client.Put_Line
           (Item => "RSP: " & Img (State.RSP)
            & " SS : "
            & Img (Ifa.Unsigned_16 (State.Segment_Regs.SS.Selector)));

         Debuglog.Client.Put_Line
           (Item => "RAX: " & Img (State.Regs.RAX)
            & " RBX: " & Img (State.Regs.RBX)
            & " RCX: " & Img (State.Regs.RCX));
         Debuglog.Client.Put_Line
           (Item => "RDX: " & Img (State.Regs.RDX)
            & " RSI: " & Img (State.Regs.RSI)
            & " RDI: " & Img (State.Regs.RDI));
         Debuglog.Client.Put_Line
           (Item => "RBP: " & Img (State.Regs.RBP)
            & " R08: " & Img (State.Regs.R08)
            & " R09: " & Img (State.Regs.R09));
         Debuglog.Client.Put_Line
           (Item => "R10: " & Img (State.Regs.R10)
            & " R11: " & Img (State.Regs.R11)
            & " R12: " & Img (State.Regs.R12));
         Debuglog.Client.Put_Line
           (Item => "R13: " & Img (State.Regs.R13)
            & " R14: " & Img (State.Regs.R14)
            & " R15: " & Img (State.Regs.R15));

         Debuglog.Client.Put_Line
           (Item => "CR0: " & Img (State.CR0)
            & " CR2: " & Img (State.Regs.CR2));
         Debuglog.Client.Put_Line
           (Item => "CR3: " & Img (State.CR3)
            & " CR4: " & Img (State.CR4));
         Debuglog.Client.New_Line;

         Debuglog.Client.Put_Line
           (Item => "Shadow CR0: " & Img (State.SHADOW_CR0));
         Debuglog.Client.Put_Line
           (Item => "IA32_EFER : " & Img (State.IA32_EFER));
      end if;
   end Dump_State;

   -------------------------------------------------------------------------

   procedure Put_Line (Item : String) renames Debuglog.Client.Put_Line;

   -------------------------------------------------------------------------

   procedure Put_String (Item : String) renames Debuglog.Client.Put;

end Debug_Ops;
