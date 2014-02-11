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

with DOM.Core.Nodes;
with DOM.Core.Elements;
with McKae.XML.XPath.XIA;
with Ada.Containers.Ordered_Sets;
with Ada.Text_IO;
with Ada.Exceptions;
with Mutools.Utils;

package body Alloc.Allocator
is

   -------------------------------------------------------------------------

   function "<" (Left, Right : Region_Type) return Boolean
   is
      use type Interfaces.Unsigned_64;
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Left.Upper_Limit > Right.Upper_Limit then
         return True;
      elsif Left.Upper_Limit = Right.Upper_Limit then
         if Left.Size < Right.Size then
            return True;
         elsif Left.Size = Right.Size then
            if Left.Alignment < Right.Alignment then
               return True;
            elsif Left.Alignment = Right.Alignment then
               if Left.Name > Right.Name then
                  return True;
               end if;
            end if;
         end if;
      end if;

      return False;

   end "<";

   package Ordered_Regions_Package is new
      Ada.Containers.Ordered_Sets
         (Element_Type => Region_Type);

   -------------------------------------------------------------------------

   procedure Add_Device_Regions
      (Policy      :        Muxml.XML_Data_Type;
       Map         : in out Alloc.Map.Map_Type)
   is
      Devices, Regions       : DOM.Core.Node_List;
      Physical_Address, Size : Interfaces.Unsigned_64;

      use Ada.Strings.Unbounded;
      use DOM.Core.Elements;
      use DOM.Core.Nodes;
      use type Interfaces.Unsigned_64;
   begin
      Devices := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/platform/device");

      for I in 0 .. DOM.Core.Nodes.Length (List => Devices) - 1
      loop
         Regions := McKae.XML.XPath.XIA.XPath_Query
           (N     => Item (Devices, I),
            XPath => "memory");
         for J in 0 .. DOM.Core.Nodes.Length (List => Regions) - 1
         loop
            Physical_Address := Interfaces.Unsigned_64'Value
               (Get_Attribute (Item (Regions, J), "physicalAddress"));
            Size := Interfaces.Unsigned_64'Value
               (Get_Attribute (Item (Regions, J), "size"));
            Map.Insert_Device_Region
               (Name          => To_Unbounded_String
                   (Get_Attribute (Item (Devices, I), "name") & "." &
                    Get_Attribute (Item (Regions, J), "name")),
                First_Address => Physical_Address,
                Last_Address  => Physical_Address + Size - 1);
         end loop;
      end loop;
   end Add_Device_Regions;

   ----------------------------------------------------------------------------

   procedure Add_Empty_Regions
      (Policy :        Muxml.XML_Data_Type;
       Map    : in out Alloc.Map.Map_Type)
   is
      Nodes                  : DOM.Core.Node_List;
      Physical_Address, Size : Interfaces.Unsigned_64;

      use DOM.Core.Elements;
      use DOM.Core.Nodes;
      use type Interfaces.Unsigned_64;
   begin
      Nodes := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/platform/memory/memoryBlock");

      if Length (Nodes) = 0 then
         raise Internal_Error with "No physical memory found";
      end if;

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1
      loop
         Physical_Address := Interfaces.Unsigned_64'Value
            (Get_Attribute (Item (Nodes, I), "physicalAddress"));
         Size := Interfaces.Unsigned_64'Value
            (Get_Attribute (Item (Nodes, I), "size"));
         Map.Insert_Empty_Region
            (Name          => Ada.Strings.Unbounded.To_Unbounded_String
                                 (Get_Attribute (Item (Nodes, I), "name")),
             First_Address => Physical_Address,
             Last_Address  => Physical_Address + Size - 1);
      end loop;
   end Add_Empty_Regions;

   -------------------------------------------------------------------------

   procedure Add_Fixed_Regions
      (Policy :        Muxml.XML_Data_Type;
       Map    : in out Alloc.Map.Map_Type)
   is
      Nodes                  : DOM.Core.Node_List;
      Physical_Address, Size : Interfaces.Unsigned_64;

      use DOM.Core.Elements;
      use DOM.Core.Nodes;
      use type Interfaces.Unsigned_64;
   begin
      Nodes := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/memory/*[@physicalAddress]");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1
      loop
         Physical_Address := Interfaces.Unsigned_64'Value
            (Get_Attribute (Item (Nodes, I), "physicalAddress"));
         Size := Interfaces.Unsigned_64'Value
            (Get_Attribute (Item (Nodes, I), "size"));
         Map.Allocate_Fixed
            (Name          => Ada.Strings.Unbounded.To_Unbounded_String
                                 (Get_Attribute (Item (Nodes, I), "name")),
             First_Address => Physical_Address,
             Last_Address  => Physical_Address + Size - 1);
      end loop;
   end Add_Fixed_Regions;

   -------------------------------------------------------------------------

   procedure Allocate_Variable_Empty_Regions
      (Policy :        Muxml.XML_Data_Type;
       Map    : in out Alloc.Map.Map_Type)
   is
   begin
      Allocate_Variable_Regions
         (Policy => Policy,
          Path   =>
            "/system/memory/*[not (@physicalAddress) and not (file)]",
          Map    => Map);
   end Allocate_Variable_Empty_Regions;

   -------------------------------------------------------------------------

   procedure Allocate_Variable_File_Regions
      (Policy :        Muxml.XML_Data_Type;
       Map    : in out Alloc.Map.Map_Type)
   is
   begin
      Allocate_Variable_Regions
         (Policy => Policy,
          Path   => "/system/memory/*[not (@physicalAddress) and (file)]",
          Map    => Map);
   end Allocate_Variable_File_Regions;

   -------------------------------------------------------------------------

   procedure Allocate_Variable_Regions
      (Policy :        Muxml.XML_Data_Type;
       Path   :        String;
       Map    : in out Alloc.Map.Map_Type)
   is
      Nodes                  : DOM.Core.Node_List;
      Alignment, Size, Below : Interfaces.Unsigned_64;

      use DOM.Core.Elements;
      use DOM.Core.Nodes;
      use Ordered_Regions_Package;

      Region_Set : Ordered_Regions_Package.Set;

      procedure Allocate (Position : Cursor);
      procedure Allocate (Position : Cursor)
      is
         R : constant Region_Type := Element (Position);
      begin
         Map.Allocate_Variable
            (Name        => R.Name,
             Size        => R.Size,
             Upper_Limit => R.Upper_Limit,
             Alignment   => R.Alignment);
      end Allocate;

   begin

      Clear (Region_Set);

      Nodes := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => Path);

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1
      loop
         if Get_Attribute (Item (Nodes, I), "alignment") /= "" then
            Alignment := Interfaces.Unsigned_64'Value
               (Get_Attribute (Item (Nodes, I), "alignment"));
         else
            --  Regions without alignment are assumed to be 4k-aligned
            Alignment := 4096;
         end if;

         if Get_Attribute (Item (Nodes, I), "below") /= "" then
            Below := Interfaces.Unsigned_64'Value
               (Get_Attribute (Item (Nodes, I), "below"));
         else
            Below := Interfaces.Unsigned_64'Last;
         end if;

         Size := Interfaces.Unsigned_64'Value
            (Get_Attribute (Item (Nodes, I), "size"));

         begin
            Insert
               (Container => Region_Set,
                New_Item  => Region_Type'
                  (Size        => Size,
                   Upper_Limit => Below,
                   Alignment   => Alignment,
                   Name        => Ada.Strings.Unbounded.To_Unbounded_String
                     (Get_Attribute (Item (Nodes, I), "name"))));
         exception
            when Constraint_Error => raise Duplicate_Region with
               "Region '" & Get_Attribute (Item (Nodes, I), "name") &
               "' (Size" & Size'Img & ", Alignment" & Alignment'Img &
               ", Below" & Below'Img & ") inserted twice";
         end;

      end loop;

      --  Allocate ordered regions
      Reverse_Iterate (Region_Set, Allocate'Access);

   end Allocate_Variable_Regions;

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      use Ada.Text_IO;
      Map             : Alloc.Map.Map_Type;
      Memory_Map_File : File_Type;

      procedure Dump_Memory_Map (Region : Alloc.Map.Region_Type);
      procedure Dump_Memory_Map (Region : Alloc.Map.Region_Type)
      is
         use Ada.Strings.Unbounded;
         use Mutools.Utils;
      begin
         Put_Line
            (Memory_Map_File,
             To_Hex (Region.First_Address, Normalize => True) &
             " " &
             To_Hex (Region.Last_Address, Normalize => True) &
             " " &
             Region.Kind'Img & " " & To_String (Region.Name));
      end Dump_Memory_Map;

      procedure Update_DOM (Region : Alloc.Map.Region_Type);
      procedure Update_DOM (Region : Alloc.Map.Region_Type)
      is
         N : DOM.Core.Node_List;
         use Ada.Strings.Unbounded;
         use DOM.Core.Nodes;
         use DOM.Core.Elements;
         use Mutools.Utils;
      begin
         N := McKae.XML.XPath.XIA.XPath_Query
           (N     => Policy.Doc,
            XPath => "/system/memory/*[@name='" &
               To_String (Region.Name) & "']");

         if Length (N) /= 1 then
            raise Internal_Error with
               "Allocated region '" & To_String (Region.Name) &
               "' not found in policy";
         end if;

         Set_Attribute
            (Item (N, 0),
             "physicalAddress",
             To_Hex (Number => Region.First_Address, Normalize => True));
      end Update_DOM;

   begin

      --  Add data from policy to memory map
      Add_Empty_Regions (Policy, Map);
      Add_Device_Regions (Policy, Map);
      Add_Fixed_Regions (Policy, Map);
      Allocate_Variable_File_Regions (Policy, Map);
      Allocate_Variable_Empty_Regions (Policy, Map);

      --  Update DOM tree
      Map.Iterate (Update_DOM'Access, Alloc.Map.Allocated);

      --  Create a memory map file
      Create (Memory_Map_File, Out_File, Output_Dir & "/system.map");
      Map.Iterate (Dump_Memory_Map'Access, Alloc.Map.Any);
      Close (Memory_Map_File);

      --  Write output file
      Muxml.Write
         (File => Output_Dir & "/system.xml",
          Kind => Muxml.Format_B,
          Data => Policy);

   exception
      when E : Alloc.Map.Overlapping_Empty_Region =>
         raise Overlapping_Physical_Memory with
            Ada.Exceptions.Exception_Message (E);
      when E : Alloc.Map.Limit_Exceeded =>
         raise Out_Of_Memory with
            Ada.Exceptions.Exception_Message (E);
   end Write;

end Alloc.Allocator;
