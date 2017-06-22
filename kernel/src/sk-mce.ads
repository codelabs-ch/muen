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

with X86_64;

with SK.Crash_Audit_Types;

package SK.MCE
is

   --  Check validity of MCE/MCA state and return results.
   procedure Check_State
     (Is_Valid : out Boolean;
      Ctx      : out Crash_Audit_Types.MCE_Init_Context_Type)
   with
      Global => (Input => X86_64.State);

   --  The procedure implements the machine-check initialization as described
   --  in Intel SDM Vol. 3B, section 15.8.
   procedure Enable
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State => X86_64.State);

end SK.MCE;
