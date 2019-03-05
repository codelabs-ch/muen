--
--  Copyright (C) 2014  Alexander Senier <mail@senier.net>
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Mutools.Utils;

package body Alloc.Map
is

   function S
     (Source : Ada.Strings.Unbounded.Unbounded_String)
      return String
      renames Ada.Strings.Unbounded.To_String;

   -------------------------------------------------------------------------

   procedure Allocate_Fixed
     (Map           : in out Map_Type;
      Name          :        Ada.Strings.Unbounded.Unbounded_String;
      First_Address :        Interfaces.Unsigned_64;
      Last_Address  :        Interfaces.Unsigned_64)
   is
      use Region_List_Package;

      Curr : Cursor := First (Map.Data);

      function Range_Image
        (First_Address : Interfaces.Unsigned_64;
         Last_Address  : Interfaces.Unsigned_64) return String;
      function Range_Image
        (First_Address : Interfaces.Unsigned_64;
         Last_Address  : Interfaces.Unsigned_64) return String
      is
         use Mutools.Utils;
      begin
         return "(" & To_Hex (Number => First_Address) & " - "
           & To_Hex (Number => Last_Address) & ")";
      end Range_Image;
   begin
      while Curr /= No_Element
      loop
         if Element (Curr).First_Address > Last_Address then
            raise Invalid_Fixed_Allocation with
              "Allocation of region '" & S (Name)
              & "' outside empty regions " &
              Range_Image (First_Address, Last_Address);
         end if;
         exit when
           (Element (Curr).Kind = Empty or Element (Curr).Kind = Device)
           and Element (Curr).First_Address <= First_Address
           and Last_Address <= Element (Curr).Last_Address;
         Next (Curr);
      end loop;

      if Curr = No_Element then
         raise Invalid_Fixed_Allocation with
           "Allocation of region '" & S (Name) & "' beyond empty regions "
           & Range_Image (First_Address, Last_Address);
      end if;

      Reserve
        (Map           => Map,
         Kind          => Fixed,
         Curr          => Curr,
         Name          => Name,
         First_Address => First_Address,
         Last_Address  => Last_Address);
   end Allocate_Fixed;

   -------------------------------------------------------------------------

   procedure Allocate_Variable
     (Map         : in out Map_Type;
      Name        :        Ada.Strings.Unbounded.Unbounded_String;
      Size        :        Interfaces.Unsigned_64;
      Upper_Limit :        Interfaces.Unsigned_64 :=
        Interfaces.Unsigned_64'Last;
      Alignment   :        Interfaces.Unsigned_64 := 1)
   is
      use Ada.Strings.Unbounded;
      use Region_List_Package;
      use Mutools.Utils;

      Curr            : Cursor := First (Map.Data);
      First_Multiple  : Interfaces.Unsigned_64;
   begin
      while Curr /= No_Element
      loop
         First_Multiple :=
           ((Element (Curr).First_Address + Alignment - 1) /
              Alignment) * Alignment;

         exit when Element (Curr).Allocatable and
           Element (Curr).Kind = Empty and
           First_Multiple + Size - 1 <= Element (Curr).Last_Address;
         Next (Curr);
      end loop;

      if Curr = No_Element then
         raise Out_Of_Memory with "No continuous block available to allocate "
           & "region '" & To_String (Name) & "' with size "
           & To_Hex (Number => Size) & ", alignment " & To_Hex
           (Number => Alignment);
      end if;

      if First_Multiple + Size - 1 > Upper_Limit then
         raise Limit_Exceeded with "Region '" & To_String (Name)
           & "' cannot be placed below " & To_Hex (Number => Upper_Limit)
           & " (Start: " & To_Hex (First_Multiple) & ")";
      end if;

      Reserve
        (Map           => Map,
         Kind          => Allocated,
         Curr          => Curr,
         Name          => Name,
         First_Address => First_Multiple,
         Last_Address  => First_Multiple + Size - 1);
   end Allocate_Variable;

   -------------------------------------------------------------------------

   procedure Clear (Map : in out Map_Type)
   is
   begin
      Map.Data.Clear;
   end Clear;

   -------------------------------------------------------------------------

   function Get_Region
     (Map  : Map_Type;
      Name : String)
      return Region_Type
   is
      use Ada.Strings.Unbounded;
      use type Region_List_Package.Cursor;

      U_Name : constant Unbounded_String
        := To_Unbounded_String (Source => Name);
      Cur    : Region_List_Package.Cursor := Map.Data.First;
   begin
      while Cur /= Region_List_Package.No_Element loop
         if Region_List_Package.Element (Position => Cur).Name = U_Name then
            return Region_List_Package.Element (Position => Cur);
         end if;
         Cur := Region_List_Package.Next (Position => Cur);
      end loop;

      raise No_Region with "Unable to find region with name '" & Name & "'";
   end Get_Region;

   -------------------------------------------------------------------------

   procedure Insert_Device_Region
     (Map           : in out Map_Type;
      Name          :        Ada.Strings.Unbounded.Unbounded_String;
      First_Address :        Interfaces.Unsigned_64;
      Last_Address  :        Interfaces.Unsigned_64)
   is
   begin
      Insert_New_Region
        (Map           => Map,
         Name          => Name,
         Allocatable   => False,
         Kind          => Device,
         First_Address => First_Address,
         Last_Address  => Last_Address);
   end Insert_Device_Region;

   ----------------------------------------------------------------------------

   procedure Insert_Empty_Region
     (Map           : in out Map_Type;
      Name          :        Ada.Strings.Unbounded.Unbounded_String;
      Allocatable   :        Boolean;
      First_Address :        Interfaces.Unsigned_64;
      Last_Address  :        Interfaces.Unsigned_64)
   is
   begin
      Insert_New_Region
        (Map           => Map,
         Name          => Name,
         Allocatable   => Allocatable,
         Kind          => Empty,
         First_Address => First_Address,
         Last_Address  => Last_Address);
   end Insert_Empty_Region;

   ----------------------------------------------------------------------------

   procedure Insert_New_Region
     (Map           : in out Map_Type;
      Name          :        Ada.Strings.Unbounded.Unbounded_String;
      Allocatable   :        Boolean;
      Kind          :        Region_Kind;
      First_Address :        Interfaces.Unsigned_64;
      Last_Address  :        Interfaces.Unsigned_64)
   is
      use Region_List_Package;

      Right, Left : Cursor;

      --  Update the First_Address field of a region with First_Address
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

      use Ada.Strings.Unbounded;
   begin
      Right := First (Map.Data);
      while Right /= No_Element and then
        First_Address > Element (Right).Last_Address
      loop
         Left := Right;
         Right := Next (Right);

         exit when
           Element (Left).Last_Address < First_Address and
           (Right = No_Element or else
            Last_Address < Element (Right).First_Address);
      end loop;

      --  Overlap check
      if Right /= No_Element and then
        Element (Right).First_Address <= Last_Address
      then
         raise Overlapping_Empty_Region with
           "Region '" & To_String (Name) & "' overlaps with region '" &
           To_String (Element (Right).Name) & "'";
      end if;

      --  Check for adjacent areas and merge them if possible.
      --
      --  A new area is adjacent to the Left (i.e. the next lower) area if:
      --
      --    (I) Adjacent_Left:
      --       (a) A Left area exists (Left /= No_Element)
      --       (b) Both, the new area and the Left area are of kind Empty
      --       (c) Both, the new area and the Left area are Allocatable
      --       (d) The last element of the Left is exactly one smaller than the
      --           first element of the new area
      --
      --  A new area is adjacent to the Right (i.e. the next higher) area if:
      --
      --    (II) Adjacent_Right:
      --       (a) A Right area exists (Right /= No_Element)
      --       (b) Both, the new area and the Right area are of kind Empty
      --       (c) Both, the new area and the Right area are Allocatable
      --       (d) The last element of the new area is exactly one smaller
      --           than the first element of the Right area
      --
      --  For a new area there are 4 options:
      --
      --    (1) Adjacent_Left and Adjacent_Right =>
      --        - Update the last element of the Left area with the last
      --          element of the Right area
      --        - Remove the Right area from the map
      --    (2) Adjacent_Left =>
      --        - Update the last element of the Left area with the last
      --          element of the new area
      --    (3) Adjacent_Right =>
      --        - Update the first element of the Right area with the first
      --          element of the new area
      --    (4) Otherwise =>
      --        - Insert new area before right area

      declare
         --  (I)
         Adjacent_Left : constant Boolean :=
           Left /= No_Element and then                          --  (I.a)
           (Kind = Empty and Element (Left).Kind = Empty and    --  (I.b)
              Allocatable and Element (Left).Allocatable and    --  (I.c)
              Element (Left).Last_Address + 1 = First_Address); --  (I.d)

         --  (II)
         Adjacent_Right : constant Boolean :=
           Right /= No_Element and then                          --  (II.a)
           (Kind = Empty and Element (Right).Kind = Empty and    --  (II.b)
              Allocatable and Element (Right).Allocatable and    --  (II.c)
              Last_Address + 1 = Element (Right).First_Address); --  (II.d)
      begin
         if Adjacent_Right and Adjacent_Left then                      --  (1)
            Update_Element (Map.Data, Left, Set_Last_Address'Access);
            Delete (Map.Data, Right);
         elsif Adjacent_Left then                                      --  (2)
            Update_Element (Map.Data, Left, Set_Last_Address'Access);
         elsif Adjacent_Right then                                     --  (3)
            Update_Element (Map.Data, Right, Set_First_Address'Access);
         else                                                          --  (4)
            Insert
              (Container => Map.Data,
               Before    => Right,
               New_Item  => Region_Type'
                 (Kind          => Kind,
                  Name          => Name,
                  Allocatable   => Allocatable,
                  First_Address => First_Address,
                  Last_Address  => Last_Address));
         end if;
      end;

   end Insert_New_Region;

   -------------------------------------------------------------------------

   procedure Iterate
     (Map     : Map_Type;
      Process : not null access procedure (Region : Region_Type);
      Filter  : Region_Kind := Any)
   is
      use Region_List_Package;

      procedure P (Position : Cursor);
      procedure P (Position : Cursor)
      is
      begin
         if Filter = Any or Element (Position).Kind = Filter then
            Process (Element (Position));
         end if;
      end P;
   begin
      Iterate (Map.Data, P'Access);
   end Iterate;

   -------------------------------------------------------------------------

   procedure Reserve
     (Map           : in out Map_Type;
      Kind          :        Region_Kind;
      Curr          :        Region_List_Package.Cursor;
      Name          :        Ada.Strings.Unbounded.Unbounded_String;
      First_Address :        Interfaces.Unsigned_64;
      Last_Address  :        Interfaces.Unsigned_64)
   is
      use Region_List_Package;

      --  Update the Kind field with Kind argument
      procedure Allocate (Element : in out Region_Type);
      procedure Allocate (Element : in out Region_Type)
      is
      begin
         Element.Kind := Kind;
         Element.Allocatable := False;
      end Allocate;

      --  Update the First_Address field of a region with Last_Address + 1
      procedure Set_First_After_Last (Element : in out Region_Type);
      procedure Set_First_After_Last (Element : in out Region_Type)
      is
      begin
         Element.First_Address := Last_Address + 1;
      end Set_First_After_Last;

      --  Update the First_Address field of a region with Last_Address + 1
      procedure Set_First_Past_Last (Element : in out Region_Type);
      procedure Set_First_Past_Last (Element : in out Region_Type)
      is
      begin
         Element.First_Address := Last_Address + 1;
      end Set_First_Past_Last;

      --  Update the First_Address field of a region with First_Address - 1
      procedure Set_First_To_First (Element : in out Region_Type);
      procedure Set_First_To_First (Element : in out Region_Type)
      is
      begin
         Element.First_Address := First_Address;
      end Set_First_To_First;

      --  Update the Name field of a region
      procedure Set_Name (Element : in out Region_Type);
      procedure Set_Name (Element : in out Region_Type)
      is
      begin
         Element.Name := Name;
      end Set_Name;

      --  Allocate a part or all of the current area (i.e. the matching empty
      --  area that has been found). The allocated area may have common
      --  addresses with the current area:
      --
      --  (I) Match_First:
      --       - The first element of the allocated area matches the first
      --         element of the current area
      --  (II) Match_Last:
      --       - The last element of the allocated area matches the first
      --         element of the current area
      --
      --  There are 4 options:
      --
      --  (1) Match_First and Match_Last =>
      --      - Set the Kind of the current area to that of the allocated area
      --      - Set Allocatable of current area to False
      --  (2) Match_First =>
      --      - Set the first element of current area to the element after
      --        the last element of the allocated area
      --      - Insert a new area with the parameters of the allocated area
      --        before the current area
      --  (3) Match_Last =>
      --      - Insert a new empty area before the current area, with a
      --        first element matching the original first element of the
      --        current area and the last element one below the first element
      --        of the allocated area, Kind is empty, Allocatable is true
      --      - Set the Kind of the current area to that of the allocated area
      --      - Set the Allocatable attribute of the current area to True
      --      - Set the first element of the current area to that of the
      --        allocated area
      --  (4) Otherwise =>
      --      - Insert a new empty area before the current current area,
      --        ranging from the current areas first element to the element
      --        before the allocated areas first element, kind empty,
      --        allocatable
      --      - Insert another new area before the current area with the
      --        parameters of the allocated area
      --      - Update the first element of the current area to the element
      --        after the last element of the allocated area

      Match_First : constant Boolean :=
        Element (Curr).First_Address = First_Address;
      Match_Last : constant Boolean :=
        Element (Curr).Last_Address = Last_Address;

   begin

      if Match_First and Match_Last then
         --  (1)
         Update_Element (Map.Data, Curr, Allocate'Access);
         Update_Element (Map.Data, Curr, Set_Name'Access);
      elsif Match_First then
         --  (2)
         Update_Element (Map.Data, Curr, Set_First_After_Last'Access);
         Insert
           (Container => Map.Data,
            Before    => Curr,
            New_Item  => Region_Type'
              (First_Address => First_Address,
               Last_Address  => Last_Address,
               Name          => Name,
               Allocatable   => False,
               Kind          => Kind));
      elsif Match_Last then
         --  (3)
         Insert
           (Container => Map.Data,
            Before    => Curr,
            New_Item  => Region_Type'
              (First_Address => Element (Curr).First_Address,
               Last_Address  => First_Address - 1,
               Name          => Element (Curr).Name,
               Allocatable   => Element (Curr).Allocatable,
               Kind          => Empty));
         Update_Element (Map.Data, Curr, Allocate'Access);
         Update_Element (Map.Data, Curr, Set_First_To_First'Access);
         Update_Element (Map.Data, Curr, Set_Name'Access);
      else
         --  (4)
         Insert
           (Container => Map.Data,
            Before    => Curr,
            New_Item  => Region_Type'
              (First_Address => Element (Curr).First_Address,
               Last_Address  => First_Address - 1,
               Name          => Element (Curr).Name,
               Allocatable   => Element (Curr).Allocatable,
               Kind          => Empty));
         Insert
           (Container => Map.Data,
            Before    => Curr,
            New_Item  => Region_Type'
              (First_Address => First_Address,
               Last_Address  => Last_Address,
               Name          => Name,
               Allocatable   => False,
               Kind          => Kind));
         Update_Element (Map.Data, Curr, Set_First_Past_Last'Access);
      end if;
   end Reserve;

end Alloc.Map;
