--
--  Copyright (C) 2013, 2014, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

pragma $Release_Warnings
  (Off, "unit * is not referenced",
   Reason => "Only used for debug output");
with SK.Strings;
with Debug_Ops;
with Sm_Component.Config;
pragma $Release_Warnings (On, "unit * is not referenced");

package body Exit_Handlers.WRMSR
is

   -------------------------------------------------------------------------

   procedure Process (Action : out Types.Subject_Action_Type)
   is
      Unused_RCX : constant SK.Word64 := Subject_Info.State.Regs.RCX;
   begin
      Action := Types.Subject_Continue;
      pragma Debug (Sm_Component.Config.Debug_Wrmsr,
                    Debug_Ops.Put_Line
                      (Item => "WRMSR " & SK.Strings.Img
                         (SK.Word32'Mod (Unused_RCX))));
   end Process;

end Exit_Handlers.WRMSR;
