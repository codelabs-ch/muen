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
      use Interfaces;

      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory");
   begin
      Mulog.Log (Msg => "Checking overlap of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " physical memory region(s)");

      if DOM.Core.Nodes.Length (List => Nodes) < 2 then
         return;
      end if;

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 2 loop
         declare
            Cur_Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Nodes,
               Index => I);
            Cur_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Cur_Node,
                 Name => "name");
            Cur_Addr : constant Unsigned_64 := Unsigned_64'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Cur_Node,
                  Name => "physicalAddress"));
            Cur_Size : constant Unsigned_64 := Unsigned_64'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Cur_Node,
                  Name => "size"));
         begin
            for J in I + 1 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
               declare
                  Other_Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
                    (List  => Nodes,
                     Index => J);
                  Other_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Other_Node,
                       Name => "name");
                  Other_Addr : constant Unsigned_64 := Unsigned_64'Value
                    (DOM.Core.Elements.Get_Attribute
                       (Elem => Other_Node,
                        Name => "physicalAddress"));
                  Other_Size : constant Unsigned_64 := Unsigned_64'Value
                    (DOM.Core.Elements.Get_Attribute
                       (Elem => Other_Node,
                        Name => "size"));
               begin
                  if (Cur_Addr <= Other_Addr
                      and then Cur_Addr + Cur_Size > Other_Addr)
                    or
                      (Other_Addr < Cur_Addr
                       and then Other_Addr + Other_Size > Cur_Addr)
                  then
                     raise Validation_Error with "Overlap of physical memory "
                       & "region '" & Cur_Name & "' and '" & Other_Name & "'";
                  end if;
               end;
            end loop;
         end;
      end loop;
   end Physical_Memory_Overlap;

   -------------------------------------------------------------------------

   procedure Physical_Memory_References (XML_Data : Muxml.XML_Data_Type)
   is
      References     : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "//physical");
      Physical_Nodes : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory");
   begin
      Mulog.Log (Msg => "Checking" & DOM.Core.Nodes.Length
                 (List => References)'Img & " physical memory references");

      for I in 0 .. DOM.Core.Nodes.Length (List => References) - 1 loop
         declare
            Node         : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => References,
                                      Index => I);
            Logical_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
                 Name => "logical");
            Refname      : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Node,
                 Name => "name");

            Match_Found : Boolean := False;
         begin
            Find_Match :
            for J in 0 .. DOM.Core.Nodes.Length (List => Physical_Nodes) - 1
            loop
               declare
                  Physname : constant String := DOM.Core.Elements.Get_Attribute
                    (Elem => DOM.Core.Nodes.Item
                       (List  => Physical_Nodes,
                        Index => J),
                     Name => "name");
               begin
                  if Physname = Refname then
                     Match_Found := True;
                     exit Find_Match;
                  end if;
               end;
            end loop Find_Match;

            if not Match_Found then
               raise Validation_Error with "Physical memory '" & Refname
                 & "' referenced by logical memory '" & Logical_Name
                 & "' not found";
            end if;
         end;
      end loop;
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
      Subjects : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/subjects/subject/@name");
      Mem_Node : DOM.Core.Node_List;
   begin
      Mulog.Log (Msg => "Checking presence of" & DOM.Core.Nodes.Length
                 (List => Subjects)'Img & " VMCS region(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Name : constant String
              := DOM.Core.Nodes.Node_Value
                (DOM.Core.Nodes.Item
                     (List  => Subjects,
                      Index => I));
            Mem_Name  : constant String := Subj_Name & "|vmcs";
         begin
            Mem_Node := XPath_Query
              (N     => XML_Data.Doc,
               XPath => "/system/memory/memory[@name='" & Mem_Name & "']");
            if DOM.Core.Nodes.Length (List => Mem_Node) = 0 then
               raise Validation_Error with "VMCS region '" & Mem_Name
                 & "' for subject " & Subj_Name & " not found";
            end if;
         end;
      end loop;
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
