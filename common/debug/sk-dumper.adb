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

package body SK.Dumper
is

   use SK.Strings;

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
     (Context : Crash_Audit_Types.Exception_Context_Type;
      APIC_ID : Byte)
   is
   begin
      New_Line;
      Put_Line (Item => "[CPU with APIC ID " & Img (APIC_ID)
                & " : KERNEL PANIC]");

      Put_Line (Item => "Vector: " & Img (Byte (Context.ISR_Ctx.Vector))
                & ", Error: " & Img (Context.ISR_Ctx.Error_Code));
      New_Line;
      Output_Registers (Regs => Context.ISR_Ctx.Regs,
                        RIP  => Context.ISR_Ctx.RIP,
                        CS   => Context.ISR_Ctx.CS,
                        RFL  => Context.ISR_Ctx.RFLAGS,
                        RSP  => Context.ISR_Ctx.RSP,
                        SS   => Context.ISR_Ctx.SS,
                        CR0  => Context.CR0,
                        CR3  => Context.CR3,
                        CR4  => Context.CR4);
   end Output_ISR_State;

   -------------------------------------------------------------------------

   procedure Output_Segment
     (Name : String;
      Seg  : Segment_Type)
   is
   begin
      Put_String (Item => Name);
      Put_Line
        (Item => ": " & Img (Word16 (Seg.Selector))
         & ":" & Img (Seg.Base)
         & ":" & Img (Seg.Limit)
         & ":" & Img (Seg.Access_Rights));
   end Output_Segment;

   -------------------------------------------------------------------------

   procedure Output_Subj_State (Context : Crash_Audit_Types.Subj_Context_Type)
   is
   begin
      Put_Line (Item => "Subject 0x" & Img (Context.Subject_ID));

      Put_Line (Item => "Exit reason: "
                & Img (Word16 (Context.Descriptor.Exit_Reason))
                & ", Exit qualification: "
                & Img (Context.Descriptor.Exit_Qualification));

      if Context.Field_Validity.Intr_Info then
         Put_String (Item => "Interrupt info: " & Img (Context.Intr_Info));
         if Context.Field_Validity.Intr_Error_Code then
            Put_String (Item => ", Interrupt error code: "
                        & Img (Context.Intr_Error_Code));
         end if;
         New_Line;
      end if;

      Output_Registers (Regs => Context.Descriptor.Regs,
                        RIP  => Context.Descriptor.RIP,
                        CS   => Context.Descriptor.CS.Selector,
                        RFL  => Context.Descriptor.RFLAGS,
                        RSP  => Context.Descriptor.RSP,
                        SS   => Context.Descriptor.SS.Selector,
                        CR0  => Context.Descriptor.CR0,
                        CR3  => Context.Descriptor.CR3,
                        CR4  => Context.Descriptor.CR4);
      Output_Segment (Name => "CS  ",
                      Seg  => Context.Descriptor.CS);
      Output_Segment (Name => "SS  ",
                      Seg  => Context.Descriptor.SS);
      Output_Segment (Name => "DS  ",
                      Seg  => Context.Descriptor.DS);
      Output_Segment (Name => "ES  ",
                      Seg  => Context.Descriptor.ES);
      Output_Segment (Name => "FS  ",
                      Seg  => Context.Descriptor.FS);
      Output_Segment (Name => "GS  ",
                      Seg  => Context.Descriptor.GS);
      Output_Segment (Name => "TR  ",
                      Seg  => Context.Descriptor.TR);
      Output_Segment (Name => "LDTR",
                      Seg  => Context.Descriptor.LDTR);
   end Output_Subj_State;

end SK.Dumper;
