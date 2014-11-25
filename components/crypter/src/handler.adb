--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Crypt.Debug;

package body Handler
is

   -------------------------------------------------------------------------

   procedure Handle_Interrupt (Vector : SK.Byte)
   is
   begin
      if Vector >= SK.Byte (Skp.Subject_Id_Type'First) + 32
        and then Vector <= SK.Byte (Skp.Subject_Id_Type'Last) + 32
      then
         Requesting_Subject := Integer (Vector) - 32;
      else
         Requesting_Subject := Skp.Subject_Id_Type'First;
      end if;

      pragma Debug (Vector < 32, Crypt.Debug.Put_Spurious
                    (Vector => Vector));
   end Handle_Interrupt;

end Handler;
