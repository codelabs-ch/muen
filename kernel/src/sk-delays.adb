--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Skp.Kernel;

with SK.Arch;
with SK.CPU;

package body SK.Delays
is

   -------------------------------------------------------------------------

   procedure Delay_Until (Deadline : Word64)
   with
      Global => (Input => X86_64.State)
   is
      Current : Word64;
   begin
      loop
         Current := Arch.Get_Current_Timestamp;
         exit when Current >= Deadline;
         CPU.Pause;
      end loop;
   end Delay_Until;

   -------------------------------------------------------------------------

   procedure U_Delay (US : Natural)
   is
      Now : constant Word64 := Arch.Get_Current_Timestamp;
   begin
      Delay_Until (Deadline => Now + ((Word64 (US)
                   * Skp.Kernel.TSC_Khz) / 1000));
   end U_Delay;

end SK.Delays;
