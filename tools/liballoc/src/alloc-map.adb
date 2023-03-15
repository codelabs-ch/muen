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

with Ada.Strings;
with Ada.Strings.Fixed;

with DOM.Core.Elements;

with Mutools.Utils;
with Mutools.Xmldebuglog;
with Muxml.Utils;

package body Alloc.Map
is

   function S
     (Source : Ada.Strings.Unbounded.Unbounded_String)
      return String
      renames Ada.Strings.Unbounded.To_String;

   --  Check that the lower bound is not bigger than the upper bound
   procedure Validate_Interval (I : Memory_Interval_Type);

   -------------------------------------------------------------------------

   procedure Add_Memory_Interval
     (List     : in out VA_Regions_Type;
      Interval :        Memory_Interval_Type)
   is
      use Memory_Intervals_Package;

      --  Insert Interval in List
      procedure Insert_Interval
        (List     : in out VA_Regions_Type;
         Interval :        Memory_Interval_Type);

      --  Delete all elements of List which are contained in Interval
      procedure Delete_Contained_Intervals
        (List     : in out VA_Regions_Type;
         Interval :        Memory_Interval_Type);

      --  Extend Input_Ival such that the output is the union of all intervals
      --  which have distance <=1 from the Input_Ival (i.e. they are contained
      --  or directly adjacent).
      procedure Extend_Interval
        (List        :     VA_Regions_Type;
         Input_Ival  :     Memory_Interval_Type;
         Output_Ival : out Memory_Interval_Type);

      ----------------------------------------------------------------------

      procedure Delete_Contained_Intervals
        (List     : in out VA_Regions_Type;
         Interval :        Memory_Interval_Type)
      is
         Curr : Memory_Intervals_Package.Cursor
           := First (Container => List.Data);
         Next_Position : Memory_Intervals_Package.Cursor
           := Curr;
      begin
         while Curr /= No_Element loop
            if Element (Curr).Last_Address < Interval.First_Address then
               Next (Curr);
            elsif
              Element (Curr).First_Address >= Interval.First_Address
              and Element (Curr).Last_Address <= Interval.Last_Address
            then
               Next_Position := Next (Curr);
               Delete (Container => List.Data,
                       Position  => Curr);
               Curr := Next_Position;
            elsif Element (Curr).First_Address > Interval.Last_Address then
               return;
            end if;
         end loop;
      end Delete_Contained_Intervals;

      ----------------------------------------------------------------------

      procedure Extend_Interval
        (List        :     VA_Regions_Type;
         Input_Ival  :     Memory_Interval_Type;
         Output_Ival : out Memory_Interval_Type)
      is
         Curr : Memory_Intervals_Package.Cursor;
      begin
         Output_Ival := Input_Ival;
         Curr := First (Container => List.Data);
         while Curr /= No_Element loop
            exit when
              Input_Ival.Last_Address < Interfaces.Unsigned_64'Last
                 and Element (Curr).First_Address > Input_Ival.Last_Address + 1;

            if Element (Curr).First_Address < Input_Ival.First_Address
              and then
              Element (Curr).Last_Address >= Input_Ival.First_Address - 1
            then
               Output_Ival.First_Address := Element (Curr).First_Address;
            end if;

            if Element (Curr).Last_Address > Input_Ival.Last_Address
              and then
              Element (Curr).First_Address <= Input_Ival.Last_Address + 1
            then
               Output_Ival.Last_Address := Element (Curr).Last_Address;
            end if;

            Next (Curr);
         end loop;
      end Extend_Interval;

      ----------------------------------------------------------------------

      procedure Insert_Interval
        (List     : in out VA_Regions_Type;
         Interval :        Memory_Interval_Type)
      is
         Curr : Memory_Intervals_Package.Cursor
           := First (Container => List.Data);
      begin
         while Curr /= No_Element loop
            if Element (Curr).First_Address < Interval.First_Address then
               Next (Curr);
            elsif Element (Curr).First_Address > Interval.First_Address then
               Insert (Container => List.Data,
                       Before    => Curr,
                       New_Item  => Interval);
               return;
            end if;
         end loop;

         --  either the list is empty or all entries start before Interval
         if Curr = No_Element then
            Append (Container => List.Data,
                    New_Item  => Interval);
         end if;
      end Insert_Interval;

      Extended_Interval : Memory_Interval_Type;

   begin
      Validate_Interval (I => Interval);
      Extend_Interval
        (List        => List,
         Input_Ival  => Interval,
         Output_Ival => Extended_Interval);
      Delete_Contained_Intervals
        (List     => List,
         Interval => Extended_Interval);
      Insert_Interval
        (List     => List,
         Interval => Extended_Interval);
   end Add_Memory_Interval;

   -------------------------------------------------------------------------

   procedure Allocate_And_Set_Single_Resource
     (Av_Mem   : in out VA_Regions_Type;
      Node     :        DOM.Core.Node;
      Run_Type :        Run_Type_Type;
      Size     :        String := "")
   is
      Size_U64    : Interfaces.Unsigned_64;
      New_Address : Interfaces.Unsigned_64;
   begin
      case Run_Type is
         when VIRTUAL_ADDRESSES =>
            if Size /= "" then
               Size_U64 := Interfaces.Unsigned_64'Value (Size);
            else
               Size_U64 := Interfaces.Unsigned_64'Value
                 (DOM.Core.Elements.Get_Attribute (Elem => Node,
                                                   Name => "size"));
            end if;
         when READER_EVENTS | WRITER_EVENTS =>
            Size_U64 := 1;
      end case;

      New_Address := Reserve_Memory (List => Av_Mem,
                                     Size => Size_U64);
      Set_Virtual_Resource
        (Node     => Node,
         Run_Type => Run_Type,
         Value    => New_Address);
   end Allocate_And_Set_Single_Resource;

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
     (Map       : in out Map_Type;
      Name      :        Ada.Strings.Unbounded.Unbounded_String;
      Size      :        Interfaces.Unsigned_64;
      Alignment :        Interfaces.Unsigned_64 := 1)
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

      Reserve
        (Map           => Map,
         Kind          => Allocated,
         Curr          => Curr,
         Name          => Name,
         First_Address => First_Multiple,
         Last_Address  => First_Multiple + Size - 1);
   end Allocate_Variable;

   -------------------------------------------------------------------------

   procedure Clear (List : in out VA_Regions_Type)
   is
   begin
      List.Data.Clear;
   end Clear;

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

   function Get_Resource_Size
     (Elem     : DOM.Core.Node;
      Run_Type : Run_Type_Type)
     return Interfaces.Unsigned_64
   is
      Tag_Name : constant String
        := DOM.Core.Elements.Get_Tag_Name (Elem => Elem);

      --  Check if Size is empty or not and convert to Unsigned_64
      function Check_Size (Size : String) return Interfaces.Unsigned_64;

      ----------------------------------------------------------------------

      function Check_Size (Size : String) return Interfaces.Unsigned_64
      is
      begin
         if Run_Type = VIRTUAL_ADDRESSES then
            if Size = "" then
               raise Validation_Error with
                 "Could not find 'size'/'elementSize' attribute in node at '"
                 & Mutools.Xmldebuglog.Get_Xpath (Node => Elem)
                 & "'";
            else
               return Interfaces.Unsigned_64'Value (Size);
            end if;
         else
            return 1;
         end if;
      end Check_Size;

   begin
      if Tag_Name = "array" then
         return Check_Size (Size => DOM.Core.Elements.Get_Attribute
              (Elem => Elem,
               Name => "elementSize"));
      else
         return Check_Size (Size => DOM.Core.Elements.Get_Attribute
              (Elem => Elem,
               Name => "size"));
      end if;
   end Get_Resource_Size;

   -------------------------------------------------------------------------

   function Get_Resource_Value
     (Elem     : DOM.Core.Node;
      Run_Type : Run_Type_Type)
     return String
   is
      Tag_Name : constant String
        := DOM.Core.Elements.Get_Tag_Name (Elem => Elem);
   begin
      case Run_Type is
         when VIRTUAL_ADDRESSES =>
            if Tag_Name = "array" then
               return DOM.Core.Elements.Get_Attribute
                 (Elem => Elem,
                  Name => "virtualAddressBase");
            else
               return DOM.Core.Elements.Get_Attribute
                 (Elem => Elem,
                  Name => "virtualAddress");
            end if;
         when READER_EVENTS =>
            if Tag_Name = "reader" then
               return DOM.Core.Elements.Get_Attribute
                 (Elem => Elem,
                  Name => "vector");
            elsif Tag_Name = "array" then
               return DOM.Core.Elements.Get_Attribute
                 (Elem => Elem,
                  Name => "vectorBase");
            elsif Tag_Name = "event" then
               declare
                  Child : constant DOM.Core.Node
                    := Muxml.Utils.Get_Unique_Element_Child
                    (Parent     => Elem,
                     Child_Name => "inject_interrupt");
               begin
                  if Child /= null then
                     return DOM.Core.Elements.Get_Attribute
                       (Elem => Child,
                        Name => "vector");
                  else
                     return "";
                  end if;
               end;
            else
               raise Validation_Error with
                 "Found unexpected node tag '"
                 & Tag_Name
                 & "' when reading attribute value for reader event";
            end if;
         when WRITER_EVENTS =>
            if Tag_Name = "writer" then
               return DOM.Core.Elements.Get_Attribute
                 (Elem => Elem,
                  Name => "event");
            elsif Tag_Name = "array" then
               return DOM.Core.Elements.Get_Attribute
                 (Elem => Elem,
                  Name => "eventBase");
            elsif Tag_Name = "event" then
               return DOM.Core.Elements.Get_Attribute
                 (Elem => Elem,
                  Name => "id");
            else
               raise Validation_Error with
                 "Found unexpected node tag '"
                 & Tag_Name
                 & "' when reading attribute value writer event";
            end if;
      end case;
   end Get_Resource_Value;

   -------------------------------------------------------------------------

   function Get_Target_String
     (Target_List : Mutools.String_Vector.Vector;
      Prefix      : String)
     return String
   is
      use Ada.Strings.Unbounded;

      function U (Input : String) return Unbounded_String
        renames To_Unbounded_String;

      Output : Unbounded_String;
   begin
      for Target of Target_List loop
         if Output /= U ("") then
            Output := Output & U (" | ");
         end if;
         Output := Output
           & U (Prefix)
           & U ("/")
           & U (Target);
      end loop;

      --  If Target_List is empty, the resulting Xpath must not match any
      --  nodes.
      if Output = U ("") then
         Output := U ("*[false()]");
      end if;

      return To_String (Output);
   end Get_Target_String;

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

   -------------------------------------------------------------------------

   function Reserve_Memory
     (List : in out VA_Regions_Type;
      Size :        Interfaces.Unsigned_64)
     return Interfaces.Unsigned_64
   is
      use Mutools.Utils;
      use Memory_Intervals_Package;

      Curr              : Memory_Intervals_Package.Cursor
                        := First (Container => List.Data);
      Output            : Interfaces.Unsigned_64;
      Curr_Size_minus_1 : Interfaces.Unsigned_64; -- Size-1 to avoid overflow
   begin
      if Size = 0 then
         raise Invalid_Region with "Cannot reserve region of size 0";
      end if;
      while Curr /= No_Element loop
         Curr_Size_minus_1 := Element (Curr).Last_Address
           - Element (Curr).First_Address;
         if Curr_Size_minus_1 > Size - 1 then
            Output := Element (Curr).First_Address;
            Replace_Element
              (Container => List.Data,
               Position => Curr,
               New_Item => (First_Address => Element (Curr).First_Address + Size,
                            Last_Address  => Element (Curr).Last_Address));
            return Output;
         elsif Curr_Size_minus_1 = Size - 1 then
            Output := Element (Curr).First_Address;
            Delete (Container => List.Data,
                    Position  => Curr);
            return Output;
         end if;
         Curr := Next (Curr);
      end loop;
      raise Out_Of_Memory with
        "Could not find free domain region of size '"
        & To_Hex (Number => Size) & "'";
   end Reserve_Memory;

   -------------------------------------------------------------------------

   procedure Set_Virtual_Resource
     (Node     : DOM.Core.Node;
      Run_Type : Run_Type_Type;
      Value    : Interfaces.Unsigned_64)
   is
   begin
      case Run_Type is
         when VIRTUAL_ADDRESSES =>
            DOM.Core.Elements.Set_Attribute
              (Elem  => Node,
               Name  => "virtualAddress",
               Value => Mutools.Utils.To_Hex (Number => Value));
         when READER_EVENTS =>
            DOM.Core.Elements.Set_Attribute
              (Elem  => Node,
               Name  => "vector",
               Value => To_String (Value));
         when WRITER_EVENTS =>
            DOM.Core.Elements.Set_Attribute
              (Elem  => Node,
               Name  => "event",
               Value => To_String (Value));
      end case;
   end Set_Virtual_Resource;

   -------------------------------------------------------------------------

   procedure Subtract_Memory_Interval
     (List          : in out VA_Regions_Type;
      First_Address :        Interfaces.Unsigned_64;
      Size          :        Interfaces.Unsigned_64)
   is
   begin
      if Size = 0 then
         return;
      elsif First_Address > Interfaces.Unsigned_64'Last - Size + 1 then
         raise Invalid_Region with
           "Unsigned_64 overflow when trying to allocate a fixed interval";
      else
         Subtract_Memory_Interval
           (List     => List,
            Interval => (First_Address => First_Address,
                         Last_Address  => First_Address + Size - 1));
      end if;
   end Subtract_Memory_Interval;

   -------------------------------------------------------------------------

   procedure Subtract_Memory_Interval
     (List     : in out VA_Regions_Type;
      Interval :        Memory_Interval_Type)
   is
      use Memory_Intervals_Package;

      Curr : Memory_Intervals_Package.Cursor
        := First (Container => List.Data);
      Next_Position : Memory_Intervals_Package.Cursor
           := Curr;
   begin
      Validate_Interval (I => Interval);

      while Curr /= No_Element loop
         if Element (Curr).Last_Address < Interval.First_Address then
            Next (Curr);
         elsif Element (Curr).First_Address < Interval.First_Address
           and Element (Curr).Last_Address >= Interval.First_Address
         then
            Insert (Container => List.Data,
                    Before    => Next (Curr),
                    New_Item  => (First_Address => Interval.First_Address,
                                  Last_Address  => Element (Curr).Last_Address));
            Replace_Element
              (Container => List.Data,
               Position => Curr,
               New_Item => (First_Address => Element (Curr).First_Address,
                            Last_Address  => Interval.First_Address - 1));
            Curr := Next (Curr);
         elsif Element (Curr).Last_Address <= Interval.Last_Address then
            Next_Position := Next (Curr);
            Delete (Container => List.Data,
                    Position  => Curr);
            Curr := Next_Position;
         elsif Element (Curr).First_Address <= Interval.Last_Address then
            Replace_Element
              (Container => List.Data,
               Position => Curr,
               New_Item => (First_Address => Interval.Last_Address + 1,
                            Last_Address  => Element (Curr).Last_Address));
            Curr := Next_Position;
         else
            return;
         end if;
      end loop;
   end Subtract_Memory_Interval;

   -------------------------------------------------------------------------

   function To_String (Number : Interfaces.Unsigned_64) return String
   is
   begin
      return Ada.Strings.Fixed.Trim (Number'Image, Ada.Strings.Both);
   end To_String;

   -------------------------------------------------------------------------

   procedure Validate_Interval (I : Memory_Interval_Type)
   is
   begin
      if I.First_Address > I.Last_Address then
         raise Invalid_Region with
           "Found invalid interval from '"
           & Mutools.Utils.To_Hex (Number => I.First_Address)
           & "' to '"
           & Mutools.Utils.To_Hex (Number => I.Last_Address)
           & "'";
      end if;
   end Validate_Interval;
end Alloc.Map;
