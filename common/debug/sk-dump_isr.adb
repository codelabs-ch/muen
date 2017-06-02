--
--  Copyright (C) 2013-2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Strings;

package body SK.Dump_ISR
is

   use SK.Strings;

   procedure Put_Line (Item : String) renames Output_Put_Line;
   procedure New_Line renames Output_New_Line;

   -------------------------------------------------------------------------

   procedure Output_Registers
     (Regs : CPU_Registers_Type;
      RIP, CS, RFL, RSP, SS, CR0, CR3, CR4 : Word64)
   is
   begin
      Put_Line (Item => "RIP: " & Img (RIP) & " CS : " & Img (Word16 (CS)));
      Put_Line (Item => "RSP: " & Img (RSP) & " SS : " & Img (Word16 (SS)));

      Put_Line (Item => "RAX: " & Img (Regs.RAX)
                & " RBX: " & Img (Regs.RBX)
                & " RCX: " & Img (Regs.RCX));

      Put_Line (Item => "RDX: " & Img (Regs.RDX)
                & " RSI: " & Img (Regs.RSI)
                & " RDI: " & Img (Regs.RDI));

      Put_Line (Item => "RBP: " & Img (Regs.RBP)
                & " R08: " & Img (Regs.R08)
                & " R09: " & Img (Regs.R09));

      Put_Line (Item => "R10: " & Img (Regs.R10) & " R11: " & Img (Regs.R11)
                & " R12: " & Img (Regs.R12));
      Put_Line (Item => "R13: " & Img (Regs.R13) & " R14: " & Img (Regs.R14)
                & " R15: " & Img (Regs.R15));

      Put_Line (Item => "CR0: " & Img (CR0) & " CR2: " & Img (Regs.CR2)
                & " CR3: " & Img (CR3));
      Put_Line (Item => "CR4: " & Img (CR4) & " EFL: "
                & Img (Word32 (RFL)));
   end Output_Registers;

   -------------------------------------------------------------------------

   procedure Output_ISR_State
     (Context       : Isr_Context_Type;
      APIC_ID       : Byte;
      CR0, CR3, CR4 : Word64)
   is
   begin
      New_Line;
      Put_Line (Item => "[CPU with APIC ID " & Img (APIC_ID)
                & " : KERNEL PANIC]");

      Put_Line (Item => "Vector: " & Img (Byte (Context.Vector))
                & ", Error: " & Img (Context.Error_Code));
      New_Line;
      Output_Registers (Regs => Context.Regs,
                        RIP  => Context.RIP,
                        CS   => Context.CS,
                        RFL  => Context.RFLAGS,
                        RSP  => Context.RSP,
                        SS   => Context.SS,
                        CR0  => CR0,
                        CR3  => CR3,
                        CR4  => CR4);
   end Output_ISR_State;

end SK.Dump_ISR;
