--
--  Copyright (C) 2020  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2020  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with SK.Constants;
with SK.CPU;

with Startup;

with Debug_Ops;

package body Exit_Handlers.Invalid_Guest_State
is

   -------------------------------------------------------------------------

   procedure Process (Action : out Types.Subject_Action_Type)
   is
   begin
      Startup.Setup_Monitored_Subject;

      declare
         CR0 : constant SK.Word64 := Subject_Info.State.CR0;
         CR4 : SK.Word64 := Subject_Info.State.CR4;
      begin
         if not SK.Bitops.Bit_Test (Value => CR0,
                                    Pos   => SK.Constants.CR0_PE_FLAG)
         then
            pragma Debug (Debug_Ops.Put_Line
                          (Item => "Waiting for AP wakeup event"));
            SK.CPU.Sti;
            SK.CPU.Hlt;
            SK.CPU.Cli;
            pragma Debug (Debug_Ops.Put_Line
                          (Item => "AP wakeup event received"));
         end if;

         if SK.Bitops.Bit_Test (Value => CR4,
                                Pos   => SK.Constants.CR4_VMXE_FLAG)
         then
            pragma Debug (Debug_Ops.Put_Line
                          (Item => "Invalid guest state, halting subject"));
            Action := Types.Subject_Halt;
         else
            CR4 := SK.Bitops.Bit_Set
              (Value => CR4,
               Pos   => SK.Constants.CR4_VMXE_FLAG);
            Subject_Info.State.CR4 := CR4;
            Action := Types.Subject_Start;
         end if;
      end;
   end Process;

end Exit_Handlers.Invalid_Guest_State;
