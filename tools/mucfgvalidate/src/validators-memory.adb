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

with Ada.Strings.Fixed;

with GNAT.Regpat;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Constants;
with Mutools.Utils;

package body Validators.Memory
is

   use McKae.XML.XPath.XIA;

   One_Megabyte : constant := 16#100000#;

   --  Set size attribute of given virtual memory node to the value of
   --  the associated physical memory region. 'Ref_Nodes_Path' is the XPath
   --  used to select the reference nodes.
   procedure Set_Size
     (Virtual_Mem_Node : DOM.Core.Node;
      Ref_Nodes_Path   : String;
      XML_Data         : Muxml.XML_Data_Type);

   -------------------------------------------------------------------------

   procedure Entity_Name_Encoding (XML_Data : Muxml.XML_Data_Type)
   is
      Last_CPU     : constant Natural := Natural'Value
        (Muxml.Utils.Get_Attribute
           (Doc   => XML_Data.Doc,
            XPath => "/system/platform/processor",
            Name  => "logicalCpus")) - 1;
      Last_CPU_Str : constant String := Ada.Strings.Fixed.Trim
        (Source => Last_CPU'Img,
         Side   => Ada.Strings.Left);
      Nodes        : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory[contains(string(@name), '|')]");

      --  Return True if given name is a valid kernel entity.
      function Is_Valid_Kernel_Entity (Name : String) return Boolean;

      ----------------------------------------------------------------------

      function Is_Valid_Kernel_Entity (Name : String) return Boolean
      is
         use type GNAT.Regpat.Match_Location;

         Matches   : GNAT.Regpat.Match_Array (0 .. 1);
         Knl_Regex : constant GNAT.Regpat.Pattern_Matcher
           := GNAT.Regpat.Compile (Expression => "^kernel_[0-"
                                   & Last_CPU_Str & "]$");
      begin
         GNAT.Regpat.Match (Self    => Knl_Regex,
                            Data    => Name,
                            Matches => Matches);

         return Matches (0) /= GNAT.Regpat.No_Match;
      end Is_Valid_Kernel_Entity;
   begin
      Mulog.Log (Msg => "Checking encoded entities in" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " physical memory region(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Ref_Name    : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Item
                     (List  => Nodes,
                      Index => I),
                 Name => "name");
            Entity_Name : constant String
              := Mutools.Utils.Decode_Entity_Name (Encoded_Str => Ref_Name);
            Subjects    : constant DOM.Core.Node_List
              := XPath_Query
                (N     => XML_Data.Doc,
                 XPath => "//subjects/subject[@name='" & Entity_Name & "']");
         begin
            if not Is_Valid_Kernel_Entity (Name => Entity_Name)
              and then DOM.Core.Nodes.Length (List => Subjects) /= 1
            then
               raise Validation_Error with "Entity '" & Entity_Name & "' "
                 & "encoded in memory region '" & Ref_Name & "' does not "
                 & "exist or is invalid";
            end if;
         end;
      end loop;
   end Entity_Name_Encoding;

   -------------------------------------------------------------------------

   procedure Physical_Address_Alignment (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "//*[@physicalAddress]");
   begin
      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "physical memory",
                       Attr      => "physicalAddress",
                       Name_Attr => "name",
                       Test      => Mod_Equal_Zero'Access,
                       Right     => Mutools.Constants.Page_Size,
                       Error_Msg => "not page aligned");
   end Physical_Address_Alignment;

   -------------------------------------------------------------------------

   procedure Physical_Memory_Overlap (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes   : DOM.Core.Node_List          := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory");
      Dev_Mem : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/platform/device/memory");
   begin
      Muxml.Utils.Append (Left  => Nodes,
                          Right => Dev_Mem);
      Check_Memory_Overlap
        (Nodes        => Nodes,
         Region_Type  => "physical or device memory region",
         Address_Attr => "physicalAddress");
   end Physical_Memory_Overlap;

   -------------------------------------------------------------------------

   procedure Physical_Memory_References (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      --  Returns True if the physical and reference name match.
      function Match_Name (Left, Right : DOM.Core.Node) return Boolean;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Logical_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "logical");
         Refname      : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "physical");
      begin
         return "Physical memory '" & Refname
           & "' referenced by logical memory '" & Logical_Name
           & "' not found";
      end Error_Msg;

      ----------------------------------------------------------------------

      function Match_Name (Left, Right : DOM.Core.Node) return Boolean
      is
         Refname : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Left,
              Name => "physical");
         Phyname : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Right,
              Name => "name");
      begin
         return Refname = Phyname;
      end Match_Name;
   begin
      For_Each_Match (XML_Data     => XML_Data,
                      Source_XPath => "//memory[@virtualAddress]",
                      Ref_XPath    => "//memory[@physicalAddress]",
                      Log_Message  => "physical memory references",
                      Error        => Error_Msg'Access,
                      Match        => Match_Name'Access);
   end Physical_Memory_References;

   -------------------------------------------------------------------------

   procedure Region_Size (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "//*[@size]");
   begin
      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "physical memory",
                       Attr      => "size",
                       Name_Attr => "name",
                       Test      => Mod_Equal_Zero'Access,
                       Right     => Mutools.Constants.Page_Size,
                       Error_Msg => "not multiple of page size (4K)");
   end Region_Size;

   -------------------------------------------------------------------------

   procedure Set_Size
     (Virtual_Mem_Node : DOM.Core.Node;
      Ref_Nodes_Path   : String;
      XML_Data         : Muxml.XML_Data_Type)
   is
      Phy_Name : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Virtual_Mem_Node,
           Name => "physical");
      Phy_Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
        (List  => XPath_Query
           (N     => XML_Data.Doc,
            XPath => Ref_Nodes_Path & "[@name='" & Phy_Name & "']"),
         Index => 0);
      Cur_Size : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Phy_Node,
           Name => "size");
   begin
      DOM.Core.Elements.Set_Attribute
        (Elem  => Virtual_Mem_Node,
         Name  => "size",
         Value => Cur_Size);
   end Set_Size;

   -------------------------------------------------------------------------

   procedure Virtual_Address_Alignment (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "//*[@virtualAddress]");
   begin
      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "logical memory",
                       Attr      => "virtualAddress",
                       Name_Attr => "logical",
                       Test      => Mod_Equal_Zero'Access,
                       Right     => Mutools.Constants.Page_Size,
                       Error_Msg => "not page aligned");
   end Virtual_Address_Alignment;

   -------------------------------------------------------------------------

   procedure Virtual_Memory_Overlap (XML_Data : Muxml.XML_Data_Type)
   is
      use Interfaces;

      Subjects       : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/subjects/subject");
      CPUs           : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/kernel/memory/cpu");
      Kernel_Dev_Mem : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/kernel/devices/device/memory");
      KDev_Mem_Count : constant Natural := DOM.Core.Nodes.Length
        (List => Kernel_Dev_Mem);
   begin
      for I in 0 .. KDev_Mem_Count - 1 loop
         declare
            Cur_Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Kernel_Dev_Mem,
               Index => I);
            Dev_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => DOM.Core.Nodes.Parent_Node (N => Cur_Node),
               Name => "physical");
         begin
            Set_Size
              (Virtual_Mem_Node => DOM.Core.Nodes.Item
                 (List  => Kernel_Dev_Mem,
                  Index => I),
               Ref_Nodes_Path   => "/system/platform/device[@name='" & Dev_Name
               & "']/memory",
               XML_Data         => XML_Data);
         end;
      end loop;

      Check_CPUs :
      for I in 0 .. DOM.Core.Nodes.Length (List => CPUs) - 1 loop
         declare
            CPU    : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => CPUs,
               Index => I);
            CPU_Id : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => CPU,
                 Name => "id");
            Memory : DOM.Core.Node_List
              := XPath_Query (N     => CPU,
                              XPath => "memory");
         begin
            if DOM.Core.Nodes.Length (List => Memory) + KDev_Mem_Count > 1 then
               for J in 0 .. DOM.Core.Nodes.Length (List => Memory) - 1 loop
                  Set_Size
                    (Virtual_Mem_Node => DOM.Core.Nodes.Item
                       (List  => Memory,
                        Index => J),
                     Ref_Nodes_Path   => "/system/memory/memory",
                     XML_Data         => XML_Data);
               end loop;

               Muxml.Utils.Append (Left  => Memory,
                                   Right => Kernel_Dev_Mem);

               Check_Memory_Overlap
                 (Nodes        => Memory,
                  Region_Type  => "virtual memory region",
                  Address_Attr => "virtualAddress",
                  Name_Attr    => "logical",
                  Add_Msg      => " of kernel running on CPU " & CPU_Id);
            end if;
         end;
      end loop Check_CPUs;

      Check_Subjects :
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subject       : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Name     : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subject,
                 Name => "name");
            Memory        : DOM.Core.Node_List          := XPath_Query
              (N     => Subject,
               XPath => "memory/memory");
            Dev_Memory    : constant DOM.Core.Node_List := XPath_Query
              (N     => Subject,
               XPath => "devices/device/memory");
            Dev_Mem_Count : constant Natural
              := DOM.Core.Nodes.Length (List => Dev_Memory);
         begin
            if DOM.Core.Nodes.Length (List => Memory) + Dev_Mem_Count > 1 then
               for J in 0 .. DOM.Core.Nodes.Length (List => Memory) - 1 loop
                  Set_Size
                    (Virtual_Mem_Node => DOM.Core.Nodes.Item
                       (List  => Memory,
                        Index => J),
                     Ref_Nodes_Path   => "/system/memory/memory",
                     XML_Data         => XML_Data);
               end loop;

               for K in 0 .. Dev_Mem_Count - 1 loop
                  declare
                     Cur_Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
                       (List  => Dev_Memory,
                        Index => K);
                     Dev_Name : constant String
                       := DOM.Core.Elements.Get_Attribute
                         (Elem => DOM.Core.Nodes.Parent_Node (N => Cur_Node),
                          Name => "physical");
                  begin
                     Set_Size
                       (Virtual_Mem_Node => DOM.Core.Nodes.Item
                          (List  => Dev_Memory,
                           Index => K),
                        Ref_Nodes_Path   => "/system/platform/device[@name='"
                        & Dev_Name & "']/memory",
                        XML_Data         => XML_Data);
                  end;
               end loop;

               Muxml.Utils.Append (Left  => Memory,
                                   Right => Dev_Memory);

               Check_Memory_Overlap
                 (Nodes        => Memory,
                  Region_Type  => "virtual memory region",
                  Address_Attr => "virtualAddress",
                  Name_Attr    => "logical",
                  Add_Msg      => " of subject '" & Subj_Name & "'");
            end if;
         end;
      end loop Check_Subjects;
   end Virtual_Memory_Overlap;

   -------------------------------------------------------------------------

   procedure VMCS_In_Lowmem (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory[contains(string(@name), '|vmcs')]");
   begin
      Check_Attribute
        (Nodes     => Nodes,
         Node_Type => "VMCS memory",
         Attr      => "physicalAddress",
         Name_Attr => "name",
         Test      => Less_Than'Access,
         Right     => One_Megabyte - Mutools.Constants.Page_Size,
         Error_Msg => "not below 1 MiB");
   end VMCS_In_Lowmem;

   -------------------------------------------------------------------------

   procedure VMCS_Region_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      --  Returns True if the physical memory region name matches.
      function Match_Region_Name (Left, Right : DOM.Core.Node) return Boolean;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Subj_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Node,
              Name => "name");
         Ref_Name  : constant String := Subj_Name & "|vmcs";
      begin
         return "VMCS region '" & Ref_Name & "' for subject " & Subj_Name
           & " not found";
      end Error_Msg;

      ----------------------------------------------------------------------

      function Match_Region_Name (Left, Right : DOM.Core.Node) return Boolean
      is
         Subj_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Left,
              Name => "name");
         Ref_Name  : constant String := Subj_Name & "|vmcs";
         Mem_Name  : constant String := DOM.Core.Elements.Get_Attribute
             (Elem => Right,
              Name => "name");
      begin
         return Ref_Name = Mem_Name;
      end Match_Region_Name;
   begin
      For_Each_Match (XML_Data     => XML_Data,
                      Source_XPath => "/system/subjects/subject",
                      Ref_XPath    => "/system/memory/memory",
                      Log_Message  => "VMCS region(s) for presence",
                      Error        => Error_Msg'Access,
                      Match        => Match_Region_Name'Access);
   end VMCS_Region_Presence;

   -------------------------------------------------------------------------

   procedure VMCS_Region_Size (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory[contains(string(@name), '|vmcs')]");
   begin
      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "VMCS memory",
                       Attr      => "size",
                       Name_Attr => "name",
                       Test      => Equals'Access,
                       Right     => Mutools.Constants.Page_Size,
                       Error_Msg => "not 4K");
   end VMCS_Region_Size;

   -------------------------------------------------------------------------

   procedure VMXON_In_Lowmem (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory[contains(string(@name), '|vmxon')]");
   begin
      Check_Attribute
        (Nodes     => Nodes,
         Node_Type => "VMXON memory",
         Attr      => "physicalAddress",
         Name_Attr => "name",
         Test      => Less_Than'Access,
         Right     => One_Megabyte - Mutools.Constants.Page_Size,
         Error_Msg => "not below 1 MiB");
   end VMXON_In_Lowmem;

   -------------------------------------------------------------------------

   procedure VMXON_Region_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      CPU_Count : constant Positive := Positive'Value
        (Muxml.Utils.Get_Attribute
           (Doc   => XML_Data.Doc,
            XPath => "/system/platform/processor",
            Name  => "logicalCpus"));
      Mem_Node  : DOM.Core.Node_List;
   begin
      Mulog.Log (Msg => "Checking presence of" & CPU_Count'Img
                 & " VMXON region(s)");

      for I in 0 .. CPU_Count - 1 loop
         declare
            CPU_Str  : constant String
              := Ada.Strings.Fixed.Trim
                (Source => I'Img,
                 Side   => Ada.Strings.Left);
            Mem_Name : constant String
              := "kernel_" & CPU_Str & "|vmxon";
         begin
            Mem_Node := XPath_Query
              (N     => XML_Data.Doc,
               XPath => "/system/memory/memory[@name='" & Mem_Name & "']");
            if DOM.Core.Nodes.Length (List => Mem_Node) = 0 then
               raise Validation_Error with "VMXON region '" & Mem_Name
                 & "' for logical CPU " & CPU_Str & " not found";
            end if;
         end;
      end loop;
   end VMXON_Region_Presence;

   -------------------------------------------------------------------------

   procedure VMXON_Region_Size (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory[contains(string(@name), '|vmxon')]");
   begin
      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "VMXON memory",
                       Attr      => "size",
                       Name_Attr => "name",
                       Test      => Equals'Access,
                       Right     => Mutools.Constants.Page_Size,
                       Error_Msg => "not 4K");
   end VMXON_Region_Size;

end Validators.Memory;
