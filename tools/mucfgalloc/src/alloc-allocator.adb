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
with Mutools.Utils;

package body Alloc.Allocator
is

   function "<" (Left, Right : Region_Type) return Boolean
   is
      use type Interfaces.Unsigned_64;
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      return Left.Size < Right.Size or else
         (Left.Size = Right.Size and Left.Alignment < Right.Alignment);
   end "<";

   package Ordered_Regions_Package is new
      Ada.Containers.Ordered_Sets
         (Element_Type => Region_Type);

   ----------------------------------------------------------------------------

   procedure Add_Empty_Regions
      (Policy      :        Muxml.XML_Data_Type;
       Map         : in out Alloc.Map.Map_Type)
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
            (First_Address => Physical_Address,
             Last_Address  => Physical_Address + Size - 1);
      end loop;
   end Add_Empty_Regions;

   ----------------------------------------------------------------------------

   procedure Add_Fixed_Regions
      (Policy      :        Muxml.XML_Data_Type;
       Map         : in out Alloc.Map.Map_Type)
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
            (First_Address => Physical_Address,
             Last_Address  => Physical_Address + Size - 1);
      end loop;
   end Add_Fixed_Regions;

   ----------------------------------------------------------------------------

   procedure Allocate_Variable_Regions
      (Policy      :        Muxml.XML_Data_Type;
       Map         : in out Alloc.Map.Map_Type)
   is
      Nodes           : DOM.Core.Node_List;
      Alignment, Size : Interfaces.Unsigned_64;

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
            (Name      => R.Name,
             Size      => R.Size,
             Alignment => R.Alignment);
      end Allocate;

   begin

      Clear (Region_Set);

      Nodes := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/memory/*[not (@physicalAddress)]");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1
      loop
         if Get_Attribute (Item (Nodes, I), "alignment") /= "" then
            Alignment := Interfaces.Unsigned_64'Value
               (Get_Attribute (Item (Nodes, I), "alignment"));
         else
            --  Regions without alignement are assumed to be 4k-aligned
            Alignment := 4096;
         end if;

         Size := Interfaces.Unsigned_64'Value
            (Get_Attribute (Item (Nodes, I), "size"));

         Insert
            (Container => Region_Set,
             New_Item  => Region_Type'
               (Size      => Size,
                Alignment => Alignment,
                Name      => Ada.Strings.Unbounded.To_Unbounded_String
                  (Get_Attribute (Item (Nodes, I), "name"))));

      end loop;

      --  Allocate ordered regions
      Reverse_Iterate (Region_Set, Allocate'Access);

   end Allocate_Variable_Regions;

   ----------------------------------------------------------------------------

   procedure Write
     (Output_File : String;
      Policy      : Muxml.XML_Data_Type)
   is
      Map : Alloc.Map.Map_Type;

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
      Add_Fixed_Regions (Policy, Map);
      Allocate_Variable_Regions (Policy, Map);

      --  Update DOM tree
      Map.Iterate (Update_DOM'Access, Alloc.Map.Allocated);

      --  Write output file
      Muxml.Write
         (File => Output_File,
          Kind => Muxml.Format_B,
          Data => Policy);

   exception
      when Alloc.Map.Overlapping_Empty_Region =>
         raise Overlapping_Physical_Memory;
   end Write;

end Alloc.Allocator;
