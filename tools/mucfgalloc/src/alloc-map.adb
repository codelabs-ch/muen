--
--  Copyright (C) 2014  Alexander Senier <mail@senier.net>
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

package body Alloc.Map
is
   procedure Insert_Empty_Region
      (Map           : in out Map_Type;
       First_Address :        Interfaces.Unsigned_64;
       Last_Address  :        Interfaces.Unsigned_64)
   is
      use Region_List_Package;
      Position : Cursor;
      Previous : Interfaces.Unsigned_64 := 0;
   begin
      Position := First (Map.Data);
      while Position /= No_Element
      loop
         Previous := Element (Position).Last_Address;
         exit when Last_Address > Element (Position).First_Address;
         Position := Next (Position);
      end loop;

      if First_Address < Previous then
         raise Overlapping_Empty_Region;
      end if;

      Insert
         (Container => Map.Data,
          Before    => Position,
          New_Item  => Region_Type'
                        (Kind          => Empty,
                         First_Address => First_Address,
                         Last_Address  => Last_Address));

   end Insert_Empty_Region;

end Alloc.Map;
