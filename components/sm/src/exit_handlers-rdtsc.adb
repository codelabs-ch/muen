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

with SK;

package body Exit_Handlers.RDTSC
with
   Refined_State => (State => TSC_Counter)
is

   TSC_Counter : SK.Word64 := 0;

   -------------------------------------------------------------------------

   procedure Process (Action : out Types.Subject_Action_Type)
   with
      Refined_Global  => (In_Out => (TSC_Counter, Subject_Info.State)),
      Refined_Depends => (Action             => null,
                          TSC_Counter        =>+ null,
                          Subject_Info.State =>+ TSC_Counter)
   is
      use type SK.Word64;
   begin
      Action := Types.Subject_Continue;

      Subject_Info.State.Regs.RAX := TSC_Counter and 16#ffff_ffff#;
      Subject_Info.State.Regs.RDX := TSC_Counter / 2 ** 32;
      TSC_Counter := TSC_Counter + 1;
   end Process;

end Exit_Handlers.RDTSC;
