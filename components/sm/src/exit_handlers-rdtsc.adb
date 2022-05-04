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

with SK.Strings;

pragma $Release_Warnings
  (Off, "unit ""Sm_Component.Config"" is not referenced",
   Reason => "Only used to control debug output");
with Sm_Component.Config;
pragma $Release_Warnings
  (On, "unit ""Sm_Component.Config"" is not referenced");

with Debug_Ops;

package body Exit_Handlers.RDTSC
is

   -------------------------------------------------------------------------

   procedure Process (Action : out Types.Subject_Action_Type)
   is
      use type SK.Word64;

      TSC : constant SK.Word64 := Musinfo.Instance.TSC_Schedule_End;
   begin
      Action := Types.Subject_Continue;

      pragma Debug (Sm_Component.Config.Debug_Rdtsc,
                    Debug_Ops.Put_Line
                      (Item => "RDTSC " & SK.Strings.Img (TSC)));
      Subject_Info.State.Regs.RAX := TSC and 16#ffff_ffff#;
      Subject_Info.State.Regs.RDX := TSC / 2 ** 32;
   end Process;

end Exit_Handlers.RDTSC;
