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
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Constants;
with Mutools.XML_Utils;
with Mutools.Match;

package body Mucfgcheck.Kernel
is

   -------------------------------------------------------------------------

   procedure CPU_Memory_Section_Count (XML_Data : Muxml.XML_Data_Type)
   is
      Active_CPUs   : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => XML_Data);
      CPU_Sections  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/kernel/memory/cpu");
      Section_Count : constant Natural
        := DOM.Core.Nodes.Length (List => CPU_Sections);
   begin
      Mulog.Log (Msg => "Checking kernel memory CPU section count");

      if Section_Count < Active_CPUs then
         raise Validation_Error with "Invalid number of kernel memory CPU "
           & "section(s)," & Section_Count'Img & " present but"
           & Active_CPUs'Img & " required";
      end if;
   end CPU_Memory_Section_Count;

   -------------------------------------------------------------------------

   procedure CPU_Store_Address_Equality (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/kernel/memory/cpu/memory[@logical='store']");
      Addr  : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Value
        (DOM.Core.Elements.Get_Attribute
           (Elem => DOM.Core.Nodes.Item (List  => Nodes,
                                         Index => 0),
            Name => "virtualAddress"));
   begin
      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "CPU Store memory",
                       Attr      => "virtualAddress",
                       Name_Attr => "physical",
                       Test      => Equals'Access,
                       Right     => Addr,
                       Error_Msg => "differs");
   end CPU_Store_Address_Equality;

   -------------------------------------------------------------------------

   procedure IOMMU_Consecutiveness (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant Muxml.Utils.Matching_Pairs_Type
        := Muxml.Utils.Get_Matching
          (XML_Data    => XML_Data,
           Left_XPath  => "/system/kernel/devices/device/memory",
           Right_XPath => "/system/platform/devices/device[capabilities/"
           & "capability/@name='iommu']",
           Match       => Mutools.Match.Is_Valid_Reference_Lparent'Access);

      --  Returns True if the left and right IOMMU memory regions are adjacent.
      function Is_Adjacent_IOMMU_Region
        (Left, Right : DOM.Core.Node)
         return Boolean;

      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
            Name => "physical");
      begin
         return "Mapping of MMIO region of IOMMU '" & Name & "' not adjacent "
           & "to other IOMMU regions";
      end Error_Msg;

      ----------------------------------------------------------------------

      function Is_Adjacent_IOMMU_Region
        (Left, Right : DOM.Core.Node)
         return Boolean
      is
         use Interfaces;

         L_Addr : constant Unsigned_64 := Unsigned_64'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Left,
               Name => "virtualAddress"));
         R_Addr : constant Unsigned_64 := Unsigned_64'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Right,
               Name => "virtualAddress"));
      begin
         return L_Addr + Mutools.Constants.Page_Size = R_Addr
           or R_Addr + Mutools.Constants.Page_Size = L_Addr;
      end Is_Adjacent_IOMMU_Region;
   begin
      if DOM.Core.Nodes.Length (List => Nodes.Left) < 2 then
         return;
      end if;

      For_Each_Match (Source_Nodes => Nodes.Left,
                      Ref_Nodes    => Nodes.Left,
                      Log_Message  => "IOMMU region(s) for consecutiveness",
                      Error        => Error_Msg'Access,
                      Match        => Is_Adjacent_IOMMU_Region'Access);
   end IOMMU_Consecutiveness;

   -------------------------------------------------------------------------

   procedure Stack_Address_Equality (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/kernel/memory/cpu/memory[@logical='stack']");
      Addr  : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Value
        (DOM.Core.Elements.Get_Attribute
           (Elem => DOM.Core.Nodes.Item (List  => Nodes,
                                         Index => 0),
            Name => "virtualAddress"));
   begin
      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "kernel stack memory",
                       Attr      => "virtualAddress",
                       Name_Attr => "physical",
                       Test      => Equals'Access,
                       Right     => Addr,
                       Error_Msg => "differs");
   end Stack_Address_Equality;

end Mucfgcheck.Kernel;
