--
--  Copyright (C) 2014, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK;

with Debug_Ops;

package body Devices.i8042
is

   use Types;

   -------------------------------------------------------------------------

   procedure Emulate
     (Info   :     Types.IO_Info_Type;
      Action : out Types.Subject_Action_Type)
   is
      use type SK.Byte;
      use type SK.Word16;
      use type SK.Word64;

      RAX : constant SK.Word64 := Subject_Info.State.Regs.RAX;
   begin
      Action := Types.Subject_Continue;

      if Info.Port_Number = 16#64#
        and then Info.Direction = Dir_Out
        and then SK.Byte'Mod (RAX) = 16#fe#
      then
         pragma Debug
           (Debug_Ops.Put_Line
              (Item => "Reboot requested via pulse of CPU RESET pin"));
         Action := Types.Subject_Reset;
      elsif Info.Port_Number = 16#64# and Info.Direction = Dir_In then
         Subject_Info.State.Regs.RAX := RAX and not 16#ff#;
      end if;
   end Emulate;

end Devices.i8042;
