--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with SK.Locks;
with SK.Dump;
with SK.Strings;

package body SK.Subjects.Debug
with
   SPARK_Mode => Off
is

   use SK.Strings;

   -------------------------------------------------------------------------

   procedure Print_State (S : Crash_Audit_Types.Subj_Context_Type)
   is
   begin
      Locks.Acquire;
      KC.Put_Line (Item => "Subject 0x" & Img (S.Subject_ID));

      KC.Put_Line (Item => "Exit reason: "
                   & Img (Word16 (S.Descriptor.Exit_Reason))
                   & ", Exit qualification: "
                   & Img (S.Descriptor.Exit_Qualification));

      if S.Field_Validity.Intr_Info then
         KC.Put_String (Item => "Interrupt info: " & Img (S.Intr_Info));
         if S.Field_Validity.Intr_Error_Code then
            KC.Put_String (Item => ", Interrupt error code: "
                           & Img (S.Intr_Error_Code));
         end if;
         KC.New_Line;
      end if;

      Dump.Print_Registers (Regs => S.Descriptor.Regs,
                            RIP  => S.Descriptor.RIP,
                            CS   => S.Descriptor.CS.Selector,
                            RFL  => S.Descriptor.RFLAGS,
                            RSP  => S.Descriptor.RSP,
                            SS   => S.Descriptor.SS.Selector,
                            CR0  => S.Descriptor.CR0,
                            CR3  => S.Descriptor.CR3,
                            CR4  => S.Descriptor.CR4);
      Dump.Print_Segment (Name => "CS  ",
                          Seg  => S.Descriptor.CS);
      Dump.Print_Segment (Name => "SS  ",
                          Seg  => S.Descriptor.SS);
      Dump.Print_Segment (Name => "DS  ",
                          Seg  => S.Descriptor.DS);
      Dump.Print_Segment (Name => "ES  ",
                          Seg  => S.Descriptor.ES);
      Dump.Print_Segment (Name => "FS  ",
                          Seg  => S.Descriptor.FS);
      Dump.Print_Segment (Name => "GS  ",
                          Seg  => S.Descriptor.GS);
      Dump.Print_Segment (Name => "TR  ",
                          Seg  => S.Descriptor.TR);
      Dump.Print_Segment (Name => "LDTR",
                          Seg  => S.Descriptor.LDTR);
      Locks.Release;
   end Print_State;

end SK.Subjects.Debug;
