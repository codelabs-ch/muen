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
   procedure Allocate_Fixed
      (Map           : in out Map_Type;
       First_Address :        Interfaces.Unsigned_64;
       Last_Address  :        Interfaces.Unsigned_64)
   is
      pragma Unreferenced (Map, First_Address, Last_Address);
   begin
      null;
   end Allocate_Fixed;

   ----------------------------------------------------------------------------

   procedure Insert_Empty_Region
      (Map           : in out Map_Type;
       First_Address :        Interfaces.Unsigned_64;
       Last_Address  :        Interfaces.Unsigned_64)
   is
      use Region_List_Package;
      Curr, Prev : Cursor;

      --  Update the First_Address field of an region with First_Address
      procedure Set_First_Address (Element : in out Region_Type);
      procedure Set_First_Address (Element : in out Region_Type)
      is
      begin
         Element.First_Address := First_Address;
      end Set_First_Address;

      --  Update the Last_Address field of a region with Last_Address
      procedure Set_Last_Address (Element : in out Region_Type);
      procedure Set_Last_Address (Element : in out Region_Type)
      is
      begin
         Element.Last_Address := Last_Address;
      end Set_Last_Address;
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

      --  Overlap check
      if Curr /= No_Element and then
         Element (Curr).First_Address <= Last_Address
      then
         raise Overlapping_Empty_Region;
      end if;

      --  Check for adjacent areas and merge them. There can be 4 scenarios:
      --
      --    (1) Last_Address + 1 = Curr.First_Address and
      --        Prev.Last_Address + 1 = First_Address =>
      --        Prev.Last_Address := Curr.Last_Address, Remove Curr.
      --    (2) Last_Address + 1 = Curr.First_Address =>
      --        Curr.First_Address := First_Address
      --    (3) Prev.Last_Address + 1 = First_Address =>
      --        Prev.Last_Address := Last_Address
      --    (4) Otherwise =>
      --        Insert new element before Curr

      declare
         Adjacent_Right : constant Boolean := Curr /= No_Element and then
            Last_Address + 1 = Element (Curr).First_Address;
         Adjacent_Left : constant Boolean := Prev /= No_Element and then
            Element (Prev).Last_Address + 1 = First_Address;
      begin
         if Adjacent_Right and Adjacent_Left then
            --  (1)
            Update_Element (Map.Data, Prev, Set_Last_Address'Access);
            Delete (Map.Data, Curr);
         elsif Adjacent_Right then
            --  (2)
            Update_Element (Map.Data, Curr, Set_First_Address'Access);
         elsif Adjacent_Left then
            --  (3)
            Update_Element (Map.Data, Prev, Set_Last_Address'Access);
         else
            --  (4)
            Insert
               (Container => Map.Data,
                Before    => Curr,
                New_Item  => Region_Type'
                              (Kind          => Empty,
                               First_Address => First_Address,
                               Last_Address  => Last_Address));
         end if;
      end;

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
