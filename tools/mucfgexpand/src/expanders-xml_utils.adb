--
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
with DOM.Core.Documents;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Paging.Memory;

with Mutools.Constants;

package body Expanders.XML_Utils
is

   --  Create memory node element with given parameters.
   function Create_Memory_Node
     (Policy    : in out Muxml.XML_Data_Type;
      Name      :        String;
      Address   :        String;
      Size      :        String;
      Caching   :        String;
      Alignment :        String)
      return DOM.Core.Node;

   -------------------------------------------------------------------------

   procedure Add_Memory_Region
     (Policy    : in out Muxml.XML_Data_Type;
      Name      :        String;
      Address   :        String;
      Size      :        String;
      Caching   :        String;
      Alignment :        String)
   is
      Section    : constant DOM.Core.Node
        := DOM.Core.Nodes.Item
          (List  => McKae.XML.XPath.XIA.XPath_Query
               (N     => Policy.Doc,
                XPath => "/system/memory"),
           Index => 0);
      Dummy_Node : DOM.Core.Node;
      pragma Unreferenced (Dummy_Node);
   begin
      Dummy_Node := DOM.Core.Nodes.Append_Child
        (N         => Section,
         New_Child => Create_Memory_Node
           (Policy    => Policy,
            Name      => Name,
            Address   => Address,
            Size      => Size,
            Caching   => Caching,
            Alignment => Alignment));
   end Add_Memory_Region;

   -------------------------------------------------------------------------

   procedure Add_Memory_Region
     (Policy      : in out Muxml.XML_Data_Type;
      Name        :        String;
      Address     :        String;
      Size        :        String;
      Caching     :        String;
      Alignment   :        String;
      File_Name   :        String;
      File_Format :        String;
      File_Offset :        String)
   is
      Section   : constant DOM.Core.Node
        := DOM.Core.Nodes.Item
          (List  => McKae.XML.XPath.XIA.XPath_Query
             (N     => Policy.Doc,
              XPath => "/system/memory"),
           Index => 0);
      Mem_Node  : constant DOM.Core.Node
        := Create_Memory_Node
          (Policy    => Policy,
           Name      => Name,
           Address   => Address,
           Size      => Size,
           Caching   => Caching,
           Alignment => Alignment);
      File_Node : constant DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Policy.Doc,
           Tag_Name => "file");
   begin
      Append_Child (Node      => Section,
                    New_Child => Mem_Node);
      Append_Child (Node      => Mem_Node,
                    New_Child => File_Node);

      DOM.Core.Elements.Set_Attribute
        (Elem  => File_Node,
         Name  => "format",
         Value => File_Format);
      DOM.Core.Elements.Set_Attribute
        (Elem  => File_Node,
         Name  => "filename",
         Value => File_Name);
      DOM.Core.Elements.Set_Attribute
        (Elem  => File_Node,
         Name  => "offset",
         Value => File_Offset);
   end Add_Memory_Region;

   -------------------------------------------------------------------------

   procedure Append_Child
     (Node      : DOM.Core.Node;
      New_Child : DOM.Core.Node)
   is
      Dummy : DOM.Core.Node;
      pragma Unreferenced (Dummy);
   begin
      Dummy := DOM.Core.Nodes.Append_Child
        (N         => Node,
         New_Child => New_Child);
   end Append_Child;

   -------------------------------------------------------------------------

   function Calculate_PT_Size
     (Policy             : Muxml.XML_Data_Type;
      Dev_Virt_Mem_XPath : String;
      Virt_Mem_XPath     : String)
      return Interfaces.Unsigned_64
   is
      use type DOM.Core.Node;

      Layout       : Paging.Memory.Memory_Layout_Type;
      Device_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => Dev_Virt_Mem_XPath);
      Memory_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => Virt_Mem_XPath);
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Memory_Nodes) - 1 loop
         declare
            Logical : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Memory_Nodes,
                 Index => I);
            Physical_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Logical,
                 Name => "physical");
            Physical : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => McKae.XML.XPath.XIA.XPath_Query
                   (N     => Policy.Doc,
                    XPath => "/system/memory/memory[@name='" & Physical_Name
                    & "']"),
                 Index => 0);
            Virtual_Address : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Logical,
                    Name => "virtualAddress"));
            Size : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Physical,
                    Name => "size"));
            Alignment : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Physical,
                    Name => "alignment"));
         begin
            Paging.Memory.Add_Memory_Region
              (Mem_Layout       => Layout,
               Physical_Address => Alignment,
               Virtual_Address  => Virtual_Address,
               Size             => Size,
               Caching          => Paging.WB,
               Writable         => False,
               Executable       => False);
         end;
      end loop;

      for I in 0 .. DOM.Core.Nodes.Length (List => Device_Nodes) - 1 loop
         declare
            Logical_Mem : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Device_Nodes,
                 Index => I);
            Dev_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Parent_Node (N => Logical_Mem),
                 Name => "physical");
            Physical_Mem_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Logical_Mem,
                 Name => "physical");
            Physical_Mem : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => McKae.XML.XPath.XIA.XPath_Query
                   (N     => Policy.Doc,
                    XPath => "/system/platform/device[@name='" & Dev_Name
                    & "']/memory[@name='" & Physical_Mem_Name
                    & "']"),
                 Index => 0);
            Virtual_Address : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Logical_Mem,
                    Name => "virtualAddress"));
            Size : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Physical_Mem,
                  Name => "size"));
         begin
            Paging.Memory.Add_Memory_Region
              (Mem_Layout       => Layout,
               Physical_Address => 0,
               Virtual_Address  => Virtual_Address,
               Size             => Size,
               Caching          => Paging.WB,
               Writable         => False,
               Executable       => False);
         end;
      end loop;

      declare
         use type Interfaces.Unsigned_64;

         PML4s, PDPTs, PDs, PTs : Natural;
      begin
         Paging.Memory.Get_Table_Count
           (Mem_Layout => Layout,
            PML4_Count => PML4s,
            PDPT_Count => PDPTs,
            PD_Count   => PDs,
            PT_Count   => PTs);

         return Interfaces.Unsigned_64
           (PML4s + PDPTs + PDs + PTs) * Mutools.Constants.Page_Size;
      end;
   end Calculate_PT_Size;

   -------------------------------------------------------------------------

   function Create_Memory_Node
     (Policy    : in out Muxml.XML_Data_Type;
      Name      :        String;
      Address   :        String;
      Size      :        String;
      Caching   :        String;
      Alignment :        String)
      return DOM.Core.Node
   is
      Mem_Node : constant DOM.Core.Node := DOM.Core.Documents.Create_Element
        (Doc      => Policy.Doc,
         Tag_Name => "memory");
   begin
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "name",
         Value => Name);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "size",
         Value => Size);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "caching",
         Value => Caching);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "alignment",
         Value => Alignment);

      if Address'Length > 0 then
         DOM.Core.Elements.Set_Attribute
           (Elem  => Mem_Node,
            Name  => "physicalAddress",
            Value => Address);
      end if;

      return Mem_Node;
   end Create_Memory_Node;

   -------------------------------------------------------------------------

   function Create_Virtual_Memory_Node
     (Policy        : in out Muxml.XML_Data_Type;
      Logical_Name  :        String;
      Physical_Name :        String;
      Address       :        String;
      Writable      :        Boolean;
      Executable    :        Boolean)
      return DOM.Core.Node
   is
      Mem_Node : constant DOM.Core.Node := DOM.Core.Documents.Create_Element
        (Doc      => Policy.Doc,
         Tag_Name => "memory");
   begin
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "logical",
         Value => Logical_Name);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "physical",
         Value => Physical_Name);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "virtualAddress",
         Value => Address);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "writable",
         Value => (if Writable then "true" else "false"));
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "executable",
         Value => (if Executable then "true" else "false"));

      return Mem_Node;
   end Create_Virtual_Memory_Node;

   -------------------------------------------------------------------------

   procedure Remove_Child
     (Node       : DOM.Core.Node;
      Child_Name : String)
   is
      Children : constant DOM.Core.Node_List
        := DOM.Core.Nodes.Child_Nodes (N => Node);
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Children) - 1 loop
         declare
            Child : DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Children,
               Index => I);
         begin
            if DOM.Core.Nodes.Node_Name (N => Child) = Child_Name then
               Child := DOM.Core.Nodes.Remove_Child
                 (N         => Node,
                  Old_Child => Child);
               DOM.Core.Nodes.Free (N => Child);
               return;
            end if;
         end;
      end loop;

      raise XML_Error with "Unable to remove child '" & Child_Name
        & "' from node '" & DOM.Core.Nodes.Node_Name (N => Node) & "'";
   end Remove_Child;

end Expanders.XML_Utils;
