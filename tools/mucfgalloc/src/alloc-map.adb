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
      Curr, Prev : Cursor;
   begin
      Curr := First (Map.Data);
      while Curr /= No_Element and then
            First_Address > Element (Curr).Last_Address
      loop
         Prev := Curr;
         Curr := Next (Curr);

         exit when
            Element (Prev).Last_Address < First_Address and
            (Curr = No_Element or else
             Last_Address < Element (Curr).First_Address);
      end loop;

      if Curr /= No_Element and then
         Element (Curr).First_Address <= Last_Address
      then
         raise Overlapping_Empty_Region;
      end if;

      Insert
         (Container => Map.Data,
          Before    => Curr,
          New_Item  => Region_Type'
                        (Kind          => Empty,
                         First_Address => First_Address,
                         Last_Address  => Last_Address));

   end Insert_Empty_Region;

   ----------------------------------------------------------------------------

   procedure Iterate
      (Map     : Map_Type;
       Process : not null access procedure (Region : Region_Type))
   is
      use Region_List_Package;

      procedure P (Position : Cursor);
      procedure P (Position : Cursor)
      is
      begin
         Process (Element (Position));
      end P;
   begin
      Iterate (Map.Data, P'Access);
   end Iterate;

end Alloc.Map;
