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
with Mutools.Types;
with Mutools.XML_Utils;

package body Mucfgcheck.Memory
is

   use McKae.XML.XPath.XIA;

   One_Megabyte : constant := 16#100000#;

   --  Returns True if the left and right memory regions are adjacent.
   function Is_Adjacent_Region (Left, Right : DOM.Core.Node) return Boolean;

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
      Subjects     : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject");

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
            use type DOM.Core.Node;

            Ref_Name    : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Item
                   (List  => Nodes,
                    Index => I),
                 Name => "name");
            Entity_Name : constant String
              := Mutools.Utils.Decode_Entity_Name (Encoded_Str => Ref_Name);
            Subject     : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Subjects,
                 Ref_Attr  => "name",
                 Ref_Value => Entity_Name);
         begin
            if not Is_Valid_Kernel_Entity (Name => Entity_Name)
              and then Subject = null
            then
               raise Validation_Error with "Entity '" & Entity_Name & "' "
                 & "encoded in memory region '" & Ref_Name & "' does not "
                 & "exist or is invalid";
            end if;
         end;
      end loop;
   end Entity_Name_Encoding;

   -------------------------------------------------------------------------

   function Is_Adjacent_Region (Left, Right : DOM.Core.Node) return Boolean
   is
      use Interfaces;

      L_Addr : constant Unsigned_64 := Unsigned_64'Value
        (DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "physicalAddress"));
      L_Size : constant Unsigned_64 := Unsigned_64'Value
        (DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "size"));
      R_Addr : constant Unsigned_64 := Unsigned_64'Value
        (DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "physicalAddress"));
      R_Size : constant Unsigned_64 := Unsigned_64'Value
        (DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "size"));
   begin
      return L_Addr + L_Size = R_Addr or R_Addr + R_Size = L_Addr;
   end Is_Adjacent_Region;

   -------------------------------------------------------------------------

   procedure Kernel_Memory_Mappings (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes      : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory[contains(string(@type),'kernel')]");
      Virt_Nodes : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "//memory[@physical]");
   begin
      Mulog.Log (Msg => "Checking mapping of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " kernel memory region(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Phys_Mem  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Phys_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Phys_Mem,
                 Name => "name");
            Virt_Mem  : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Virt_Nodes,
                 Ref_Attr  => "physical",
                 Ref_Value => Phys_Name);
            Virt_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Virt_Mem,
                 Name => "logical");
            Owner     : constant DOM.Core.Node
              := Muxml.Utils.Ancestor_Node
                (Node  => Virt_Mem,
                 Level => 3);
         begin
            if DOM.Core.Nodes.Node_Name (N => Owner) /= "kernel" then
               declare
                  use type Mutools.Types.Memory_Kind;

                  Mem_Type  : constant Mutools.Types.Memory_Kind
                    := Mutools.Types.Memory_Kind'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Phys_Mem,
                          Name => "type"));
                  Subj_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Muxml.Utils.Ancestor_Node
                         (Node  => Virt_Mem,
                          Level => 2),
                       Name => "name");
               begin

                  --  tau0 is allowed to map kernel interface.

                  if not (Mem_Type = Mutools.Types.Kernel_Interface
                          and then Subj_Name = "tau0")
                  then
                     raise Validation_Error with "Kernel memory region '"
                       & Phys_Name & "' mapped by logical memory region '"
                       & Virt_Name & "' of subject '" & Subj_Name & "'";
                  end if;
               end;
            end if;
         end;
      end loop;
   end Kernel_Memory_Mappings;

   -------------------------------------------------------------------------

   procedure Kernel_PT_Below_4G (XML_Data : Muxml.XML_Data_Type)
   is
      CPU_Count    : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => XML_Data);
      Physical_Mem : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory");
   begin
      Mulog.Log (Msg => "Checking physical address of" & CPU_Count'Img
                 & " kernel PT region(s)");

      for I in 0 .. CPU_Count - 1 loop
         declare
            use type DOM.Core.Node;

            CPU_Str  : constant String
              := Ada.Strings.Fixed.Trim
                (Source => I'Img,
                 Side   => Ada.Strings.Left);
            Mem_Name : constant String
              := "kernel_" & CPU_Str & "|pt";
            Node     : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Physical_Mem,
                 Ref_Attr  => "name",
                 Ref_Value => Mem_Name);
            Address  : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "physicalAddress"));
            Size    : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Node,
                    Name => "size"));
         begin
            if Address + Size >= 2 ** 32 then
               raise Validation_Error with "Kernel PT region '" & Mem_Name
                 & "' for logical CPU " & CPU_Str & " not below 4G";
            end if;
         end;
      end loop;
   end Kernel_PT_Below_4G;

   -------------------------------------------------------------------------

   procedure Kernel_PT_Region_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      CPU_Count    : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => XML_Data);
      Physical_Mem : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory");
      Node         : DOM.Core.Node;
   begin
      Mulog.Log (Msg => "Checking presence of" & CPU_Count'Img
                 & " kernel PT region(s)");

      for I in 0 .. CPU_Count - 1 loop
         declare
            use type DOM.Core.Node;

            CPU_Str  : constant String
              := Ada.Strings.Fixed.Trim
                (Source => I'Img,
                 Side   => Ada.Strings.Left);
            Mem_Name : constant String
              := "kernel_" & CPU_Str & "|pt";
         begin
            Node := Muxml.Utils.Get_Element
              (Nodes     => Physical_Mem,
               Ref_Attr  => "name",
               Ref_Value => Mem_Name);
            if Node = null then
               raise Validation_Error with "Kernel PT region '" & Mem_Name
                 & "' for logical CPU " & CPU_Str & " not found";
            end if;
         end;
      end loop;
   end Kernel_PT_Region_Presence;

   -------------------------------------------------------------------------

   procedure Kernel_Stack_Region_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      CPU_Count    : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => XML_Data);
      Physical_Mem : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory");
      Node         : DOM.Core.Node;
   begin
      Mulog.Log (Msg => "Checking presence of" & CPU_Count'Img
                 & " kernel stack region(s)");

      for I in 0 .. CPU_Count - 1 loop
         declare
            use type DOM.Core.Node;

            CPU_Str  : constant String
              := Ada.Strings.Fixed.Trim
                (Source => I'Img,
                 Side   => Ada.Strings.Left);
            Mem_Name : constant String
              := "kernel_stack_" & CPU_Str;
         begin
            Node := Muxml.Utils.Get_Element
              (Nodes     => Physical_Mem,
               Ref_Attr  => "name",
               Ref_Value => Mem_Name);
            if Node = null then
               raise Validation_Error with "Kernel stack region '" & Mem_Name
                 & "' for logical CPU " & CPU_Str & " not found";
            end if;
         end;
      end loop;
   end Kernel_Stack_Region_Presence;

   -------------------------------------------------------------------------

   procedure Kernel_Store_Region_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      CPU_Count    : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => XML_Data);
      Physical_Mem : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory");
      Node         : DOM.Core.Node;
   begin
      Mulog.Log (Msg => "Checking presence of" & CPU_Count'Img
                 & " kernel store region(s)");

      for I in 0 .. CPU_Count - 1 loop
         declare
            use type DOM.Core.Node;

            CPU_Str  : constant String
              := Ada.Strings.Fixed.Trim
                (Source => I'Img,
                 Side   => Ada.Strings.Left);
            Mem_Name : constant String
              := "kernel_store_" & CPU_Str;
         begin
            Node := Muxml.Utils.Get_Element
              (Nodes     => Physical_Mem,
               Ref_Attr  => "name",
               Ref_Value => Mem_Name);
            if Node = null then
               raise Validation_Error with "Kernel store region '" & Mem_Name
                 & "' for logical CPU " & CPU_Str & " not found";
            end if;
         end;
      end loop;
   end Kernel_Store_Region_Presence;

   -------------------------------------------------------------------------

   procedure Physical_Address_Alignment (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory");
      Count : constant Natural := DOM.Core.Nodes.Length (List => Nodes);
   begin
      Mulog.Log (Msg => "Checking alignment of" & Count'Img
                 & " physical memory region(s)");

      for I in 0 .. Count - 1 loop
         declare
            Mem_Node  : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Nodes,
               Index => I);
            Address   : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Mem_Node,
                    Name => "physicalAddress"));
            Alignment : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Mem_Node,
                    Name => "alignment"));
         begin
            if Address mod Alignment /= 0 then
               declare
                  Name : constant String := DOM.Core.Elements.Get_Attribute
                    (Elem => Mem_Node,
                     Name => "name");
               begin
                  raise Validation_Error with "Physical address of memory "
                    & "region '" & Name & "' does not honor alignment "
                    & Mutools.Utils.To_Hex (Number => Alignment);
               end;
            end if;
         end;
      end loop;
   end Physical_Address_Alignment;

   -------------------------------------------------------------------------

   procedure Physical_Memory_Name_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory");

      --  Check inequality of memory region names.
      procedure Check_Inequality (Left, Right : DOM.Core.Node);

      ----------------------------------------------------------------------

      procedure Check_Inequality (Left, Right : DOM.Core.Node)
      is
         Left_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Left,
            Name => "name");
         Right_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "name");
      begin
         if Left_Name = Right_Name then
            raise Validation_Error with "Multiple physical memory regions with"
              & " name '" & Left_Name & "'";
         end if;
      end Check_Inequality;
   begin
      Mulog.Log (Msg => "Checking uniqueness of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " physical memory region name(s)");

      Compare_All (Nodes      => Nodes,
                   Comparator => Check_Inequality'Access);
   end Physical_Memory_Name_Uniqueness;

   -------------------------------------------------------------------------

   procedure Physical_Memory_Overlap (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes   : DOM.Core.Node_List          := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory");
      Dev_Mem : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/platform/devices/device/memory");
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
   begin
      For_Each_Match (XML_Data     => XML_Data,
                      Source_XPath => "//memory/memory[@virtualAddress]",
                      Ref_XPath    => "/system/memory/memory",
                      Log_Message  => "physical memory references",
                      Error        => Error_Msg'Access,
                      Match        => Is_Valid_Reference'Access);
   end Physical_Memory_References;

   -------------------------------------------------------------------------

   procedure Region_Size (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "//memory[@size]");
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

   procedure System_Memory_Mappings (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes      : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory[contains(string(@type),'system')]");
      Virt_Nodes : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "//memory[@physical]");
   begin
      Mulog.Log (Msg => "Checking mapping of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " system memory region(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            use type DOM.Core.Node;

            Phys_Mem  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Phys_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Phys_Mem,
                 Name => "name");
            Virt_Mem  : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Virt_Nodes,
                 Ref_Attr  => "physical",
                 Ref_Value => Phys_Name);
         begin
            if Virt_Mem /= null then
               raise Validation_Error with "System memory region '"
                 & Phys_Name & "' is mapped by logical memory region '"
                 & DOM.Core.Elements.Get_Attribute (Elem => Virt_Mem,
                                                    Name => "logical") & "'";
            end if;
         end;
      end loop;
   end System_Memory_Mappings;

   -------------------------------------------------------------------------

   procedure Timer_Memory_Mappings (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes           : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory[@type='subject_timer']");
      Kernel_Mappings : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/kernel/memory/cpu/memory");
   begin
      Mulog.Log (Msg => "Checking mapping of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " timer memory region(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            use type DOM.Core.Node;

            Phys_Mem         : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Phys_Name        : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Phys_Mem,
                 Name => "name");
            Kernel_Mem       : constant DOM.Core.Node_List
              := Muxml.Utils.Get_Elements
                (Nodes     => Kernel_Mappings,
                 Ref_Attr  => "physical",
                 Ref_Value => Phys_Name);
            Kernel_Map_Count : constant Natural
              := DOM.Core.Nodes.Length (List => Kernel_Mem);
         begin
            if Kernel_Map_Count = 0 then
               raise Validation_Error with "Timer memory region '"
                 & Phys_Name & "' is not mapped by any kernel";
            elsif Kernel_Map_Count > 1 then
               raise Validation_Error with "Timer memory region '"
                 & Phys_Name & "' has multiple kernel mappings:"
                 & Kernel_Map_Count'Img;
            end if;
         end;
      end loop;
   end Timer_Memory_Mappings;

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

      Physical_Mem   : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory[not(starts-with(@type,'system'))]");
      Physical_Devs  : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/platform/devices/device");
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
            Device   : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Physical_Devs,
                 Ref_Attr  => "name",
                 Ref_Value => Dev_Name);
         begin
            Set_Size (Virtual_Mem_Node => Cur_Node,
                      Ref_Nodes        => XPath_Query
                        (N     => Device,
                         XPath => "memory"));
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
                  Set_Size (Virtual_Mem_Node => DOM.Core.Nodes.Item
                            (List  => Memory,
                             Index => J),
                            Ref_Nodes        => Physical_Mem);
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
                  Set_Size (Virtual_Mem_Node => DOM.Core.Nodes.Item
                            (List  => Memory,
                             Index => J),
                            Ref_Nodes        => Physical_Mem);
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
                     Device   : constant DOM.Core.Node
                       := Muxml.Utils.Get_Element
                         (Nodes     => Physical_Devs,
                          Ref_Attr  => "name",
                          Ref_Value => Dev_Name);
                  begin
                     Set_Size (Virtual_Mem_Node => Cur_Node,
                               Ref_Nodes        => XPath_Query
                                 (N     => Device,
                                  XPath => "memory"));
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

   procedure VMCS_Consecutiveness (XML_Data : Muxml.XML_Data_Type)
   is
      XPath : constant String
        := "/system/memory/memory[@type='system_vmcs']";

      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => XPath);

      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "name");
      begin
         return "Memory region '" & Name & "' not adjacent to other VMCS"
           & " regions";
      end Error_Msg;
   begin
      if DOM.Core.Nodes.Length (List => Nodes) < 2 then
         return;
      end if;

      For_Each_Match (XML_Data     => XML_Data,
                      Source_XPath => XPath,
                      Ref_XPath    => XPath,
                      Log_Message  => "VMCS region(s) for consecutiveness",
                      Error        => Error_Msg'Access,
                      Match        => Is_Adjacent_Region'Access);
   end VMCS_Consecutiveness;

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

   procedure VMXON_Consecutiveness (XML_Data : Muxml.XML_Data_Type)
   is
      XPath : constant String
        := "/system/memory/memory[contains(string(@name), '|vmxon')]";

      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => XPath);

      --  Returns the error message for a given reference node.
      function Error_Msg (Node : DOM.Core.Node) return String;

      ----------------------------------------------------------------------

      function Error_Msg (Node : DOM.Core.Node) return String
      is
         Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "name");
      begin
         return "Memory region '" & Name & "' not adjacent to other VMXON"
           & " regions";
      end Error_Msg;
   begin
      if DOM.Core.Nodes.Length (List => Nodes) < 2 then
         return;
      end if;

      For_Each_Match (XML_Data     => XML_Data,
                      Source_XPath => XPath,
                      Ref_XPath    => XPath,
                      Log_Message  => "VMXON region(s) for consecutiveness",
                      Error        => Error_Msg'Access,
                      Match        => Is_Adjacent_Region'Access);
   end VMXON_Consecutiveness;

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
      CPU_Count   : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => XML_Data);
      Phys_Memory : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory");
   begin
      Mulog.Log (Msg => "Checking presence of" & CPU_Count'Img
                 & " VMXON region(s)");

      for I in 0 .. CPU_Count - 1 loop
         declare
            use type DOM.Core.Node;

            CPU_Str  : constant String
              := Ada.Strings.Fixed.Trim
                (Source => I'Img,
                 Side   => Ada.Strings.Left);
            Mem_Name : constant String
              := "kernel_" & CPU_Str & "|vmxon";
         begin
            if Muxml.Utils.Get_Element
              (Nodes     => Phys_Memory,
               Ref_Attr  => "name",
               Ref_Value => Mem_Name) = null
            then
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

   -------------------------------------------------------------------------

   procedure VTd_Context_Region_Size (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory[@type='system_vtd_context']");
   begin
      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "VT-d context table",
                       Attr      => "size",
                       Name_Attr => "name",
                       Test      => Equals'Access,
                       Right     => Mutools.Constants.Page_Size,
                       Error_Msg => "not 4K");
   end VTd_Context_Region_Size;

   -------------------------------------------------------------------------

   procedure VTd_Root_Region_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      IOMMUs : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/platform/devices/device[starts-with"
           & "(string(@name),'iommu')]");
   begin
      if DOM.Core.Nodes.Length (List => IOMMUs) > 0 then
         Mulog.Log (Msg => "Checking presence of VT-d root table region");

         declare
            Nodes : constant DOM.Core.Node_List
              := XPath_Query
                (N     => XML_Data.Doc,
                 XPath => "/system/memory/memory[@type='system_vtd_root']"
                 & "/file");
         begin
            if DOM.Core.Nodes.Length (List => Nodes) /= 1 then
               raise Validation_Error with "VT-d root table memory region not"
                 & " found";
            end if;
         end;
      end if;
   end VTd_Root_Region_Presence;

   -------------------------------------------------------------------------

   procedure VTd_Root_Region_Size (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory[@type='system_vtd_root']");
   begin
      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "VT-d root table",
                       Attr      => "size",
                       Name_Attr => "name",
                       Test      => Equals'Access,
                       Right     => Mutools.Constants.Page_Size,
                       Error_Msg => "not 4K");
   end VTd_Root_Region_Size;

end Mucfgcheck.Memory;
