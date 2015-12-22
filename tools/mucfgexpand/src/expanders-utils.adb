--
--  Copyright (C) 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Expanders.Utils
is

   -------------------------------------------------------------------------

   procedure Allocate
     (Allocator : in out Number_Allocator_Type;
      Number    :    out Natural)
   is
   begin
      for I in Allocator.Numbers'Range loop
         if Allocator.Numbers (I) then
            Allocator.Numbers (I) := False;
            Number := I;
            return;
         end if;
      end loop;

      raise No_Free_Number;
   end Allocate;

end Expanders.Utils;
