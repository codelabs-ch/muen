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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Ada.Containers.Ordered_Sets;
with Ada.Text_IO;
with Ada.Exceptions;

with Mulog;
with Mutools.Utils;
with Muxml.Utils;

package body Allocator
is

   -------------------------------------------------------------------------

   function "<" (Left, Right : Region_Type) return Boolean
   is
      use type Interfaces.Unsigned_64;
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
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

      return False;
   end "<";

   package Ordered_Regions_Package is new
     Ada.Containers.Ordered_Sets
       (Element_Type => Region_Type);

   -------------------------------------------------------------------------

   procedure Add_Device_Regions
     (Policy :        Muxml.XML_Data_Type;
      Map    : in out Alloc.Map.Map_Type)
   is
      use Ada.Strings.Unbounded;
      use type Interfaces.Unsigned_64;

      Devices : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/hardware/devices/device");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Devices) - 1 loop
         declare
            Dev_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Devices,
                 Index => I);
            Dev_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Dev_Node,
                 Name => "name");
            Regions : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Dev_Node,
                 XPath => "memory");
         begin
            for J in 0 .. DOM.Core.Nodes.Length (List => Regions) - 1 loop
               declare
                  Reg_Node : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Regions,
                       Index => J);
                  Reg_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Reg_Node,
                       Name => "name");
                  Physical_Address : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Reg_Node,
                          Name => "physicalAddress"));
                  Size : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Reg_Node,
                          Name => "size"));
               begin
                  Map.Insert_Device_Region
                    (Name          => To_Unbounded_String
                       (Dev_Name & "." & Reg_Name),
                     First_Address => Physical_Address,
                     Last_Address  => Physical_Address + Size - 1);
               end;
            end loop;
         end;
      end loop;
   end Add_Device_Regions;

   ----------------------------------------------------------------------------

   procedure Add_Empty_Regions
     (Policy :        Muxml.XML_Data_Type;
      Map    : in out Alloc.Map.Map_Type)
   is
      Nodes                  : DOM.Core.Node_List;
      Physical_Address, Size : Interfaces.Unsigned_64;
      Allocatable            : Boolean;

      use DOM.Core.Elements;
      use DOM.Core.Nodes;
      use type Interfaces.Unsigned_64;
   begin
      Nodes := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/hardware/memory/memoryBlock");

      if Length (Nodes) = 0 then
         raise Internal_Error with "No physical memory found";
      end if;

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1
      loop
         --  @Allocatable is True if unset
         if Get_Attribute (Item (Nodes, I), "allocatable") = "" then
            Allocatable := True;
         else
            Allocatable := Boolean'Value
              (Get_Attribute (Item (Nodes, I), "allocatable"));
         end if;

         Physical_Address := Interfaces.Unsigned_64'Value
           (Get_Attribute (Item (Nodes, I), "physicalAddress"));
         Size := Interfaces.Unsigned_64'Value
           (Get_Attribute (Item (Nodes, I), "size"));
         Map.Insert_Empty_Region
           (Name          => Ada.Strings.Unbounded.To_Unbounded_String
              (Get_Attribute (Item (Nodes, I), "name")),
            Allocatable   => Allocatable,
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
           "/system/memory/*[not (@physicalAddress) and not (file or fill)]",
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

   procedure Allocate_Variable_Fill_Regions
     (Policy :        Muxml.XML_Data_Type;
      Map    : in out Alloc.Map.Map_Type)
   is
   begin

      --  Allocate system fills first to increase the chance that especially
      --  system_pt regions are below 4 GiG (which is required and validated).

      Allocate_Variable_Regions
        (Policy => Policy,
         Path   => "/system/memory/*[not (@physicalAddress) and (fill)"
         & " and starts-with(@type,'system')]",
         Map    => Map);
      Allocate_Variable_Regions
        (Policy => Policy,
         Path   => "/system/memory/*[not (@physicalAddress) and (fill)"
         & " and not (starts-with(@type,'system'))]",
         Map    => Map);
   end Allocate_Variable_Fill_Regions;

   -------------------------------------------------------------------------

   procedure Allocate_Variable_Regions
     (Policy :        Muxml.XML_Data_Type;
      Path   :        String;
      Map    : in out Alloc.Map.Map_Type)
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

         Size := Interfaces.Unsigned_64'Value
           (Get_Attribute (Item (Nodes, I), "size"));

         begin
            Insert
              (Container => Region_Set,
               New_Item  => Region_Type'
                 (Size        => Size,
                  Alignment   => Alignment,
                  Name        => Ada.Strings.Unbounded.To_Unbounded_String
                    (Get_Attribute (Item (Nodes, I), "name"))));
         exception
            when Constraint_Error => raise Duplicate_Region with
                 "Region '" & Get_Attribute (Item (Nodes, I), "name") &
                 "' (Size" & Size'Img & ", Alignment" & Alignment'Img
                 & ") inserted twice";
         end;

      end loop;

      --  Allocate ordered regions
      Reverse_Iterate (Region_Set, Allocate'Access);

   end Allocate_Variable_Regions;

   -------------------------------------------------------------------------

   procedure Write
     (Input_Policy : Muxml.XML_Data_Type;
      Output_File  : String)
   is
      use Ada.Text_IO;

      Map             : Alloc.Map.Map_Type;
      Memory_Map_File : File_Type;
      Map_Filename    : constant String := Output_File & ".map";
      Physical_Memory : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Input_Policy.Doc,
           XPath => "/system/memory/memory");

      procedure Create_Memory_Map;
      procedure Dump_Memory_Map (Region : Alloc.Map.Region_Type);
      procedure Update_DOM (Region : Alloc.Map.Region_Type);

      ----------------------------------------------------------------------

      procedure Create_Memory_Map
      is
      begin
         Create (Memory_Map_File, Out_File, Map_Filename);
         Map.Iterate (Dump_Memory_Map'Access, Alloc.Map.Any);
         Close (Memory_Map_File);
         Mulog.Log (Msg => "Memory map written to '" & Map_Filename & "'");
      end Create_Memory_Map;

      ----------------------------------------------------------------------

      procedure Dump_Memory_Map (Region : Alloc.Map.Region_Type)
      is
         use Ada.Strings.Unbounded;
         use Mutools.Utils;
      begin
         Put_Line
           (Memory_Map_File,
            To_Hex (Number => Region.First_Address)
            & " "
            & To_Hex (Number => Region.Last_Address)
            & " "
            & Region.Kind'Img & " " & To_String (Region.Name)
            & " ALLOCATABLE: " & Region.Allocatable'Img);
      end Dump_Memory_Map;

      ----------------------------------------------------------------------

      procedure Update_DOM (Region : Alloc.Map.Region_Type)
      is
         use Ada.Strings.Unbounded;
         use type DOM.Core.Node;

         Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Nodes     => Physical_Memory,
              Ref_Attr  => "name",
              Ref_Value => To_String (Region.Name));
      begin
         if Node = null then
            raise Internal_Error with
              "Allocated region '" & To_String (Region.Name) &
              "' not found in policy";
         end if;

         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "physicalAddress",
            Value => Mutools.Utils.To_Hex (Number => Region.First_Address));
      end Update_DOM;
   begin

      --  Add data from policy to memory map
      Add_Empty_Regions
        (Policy => Input_Policy,
         Map    => Map);
      Add_Device_Regions
        (Policy => Input_Policy,
         Map    => Map);
      Add_Fixed_Regions
        (Policy => Input_Policy,
         Map    => Map);

      begin
         Allocate_Variable_File_Regions
           (Policy => Input_Policy,
            Map    => Map);
         Allocate_Variable_Fill_Regions
           (Policy => Input_Policy,
            Map    => Map);
         Allocate_Variable_Empty_Regions
           (Policy => Input_Policy,
            Map    => Map);

      exception
         when Alloc.Map.Out_Of_Memory =>
            Create_Memory_Map;
            raise;
      end;

      --  Update DOM tree
      Map.Iterate (Update_DOM'Access, Alloc.Map.Allocated);

      Create_Memory_Map;

      --  Write output file
      Muxml.Write
        (File => Output_File,
         Kind => Muxml.Format_B,
         Data => Input_Policy);

   exception
      when E : Alloc.Map.Overlapping_Empty_Region =>
         raise Overlapping_Physical_Memory
           with Ada.Exceptions.Exception_Message (E);
   end Write;

end Allocator;
