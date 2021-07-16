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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Constants;
with Mutools.Utils;
with Mutools.Types;
with Mutools.XML_Utils;
with Mutools.Match;

with Mucfgcheck.Utils;
with Mucfgcheck.Validation_Errors;

package body Mucfgcheck.Memory
is

   use McKae.XML.XPath.XIA;

   One_Megabyte : constant := 16#100000#;

   --  Returns True if the left and right memory regions are adjacent.
   function Is_Adjacent_Region (Left, Right : DOM.Core.Node) return Boolean;

   --  Checks kernel mappings of subject memory regions with specified region
   --  type. The following checks are performed:
   --   * Each region is mapped by exactly one kernel
   --   * CPU IDs of kernel and subject are identical
   --   * Virtual addresses of kernel mappings are relative to the same base
   --     address
   procedure Check_Subject_Region_Mappings
     (Data         : Muxml.XML_Data_Type;
      Mapping_Name : String;
      Region_Type  : String);

   --  Check presence of physical per-subject memory region with specified
   --  region type, class and size. If size is specified as 0, then the region
   --  size is not checked.
   procedure Check_Subject_Region_Presence
     (XML_Data     : Muxml.XML_Data_Type;
      Region_Type  : String;
      Region_Class : String := "subject";
      Region_Size  : Interfaces.Unsigned_64 := Mutools.Constants.Page_Size);

   --  Check presence of physical kernel memory region with given name prefix
   --  and suffix for each CPU. The specified region kind is used in log
   --  messages.
   procedure Check_Kernel_Region_Presence
     (Data        : Muxml.XML_Data_Type;
      Region_Kind : String;
      Name_Prefix : String;
      Name_Suffix : String := "");

   --  Check that the given virtual mapping address of the subject specified by
   --  ID is consistent with the given base address. If base address is not yet
   --  set the calculated base address is returned.
   procedure Check_Kernel_Mapping_Address
     (Base_Addr    : in out Interfaces.Unsigned_64;
      Mapping_Addr :        Interfaces.Unsigned_64;
      Subject_ID   :        Natural;
      Region_Type  :        String;
      Mapping_Name :        String);

   -------------------------------------------------------------------------

   procedure Check_Kernel_Mapping_Address
     (Base_Addr    : in out Interfaces.Unsigned_64;
      Mapping_Addr :        Interfaces.Unsigned_64;
      Subject_ID   :        Natural;
      Region_Type  :        String;
      Mapping_Name :        String)
   is

      --  The expected virtual address of the kernel mapping is:
      --
      --  Base_Address + (Subject_ID * Page_Size).

      Cur_Base_Addr : constant Interfaces.Unsigned_64
        := Mapping_Addr - Interfaces.Unsigned_64
          (Subject_ID * Mutools.Constants.Page_Size);
   begin
      if Base_Addr = 0 then
         Base_Addr := Cur_Base_Addr;
      elsif Base_Addr /= Cur_Base_Addr then
         declare
            Expected_Addr : constant Interfaces.Unsigned_64
              := Base_Addr + Interfaces.Unsigned_64
                (Subject_ID * Mutools.Constants.Page_Size);
         begin
            Validation_Errors.Insert
              (Msg => Mutools.Utils.Capitalize
                 (Str => Region_Type) &  " memory region '" & Mapping_Name
               & "' mapped at unexpected kernel virtual address "
               & Mutools.Utils.To_Hex (Number => Mapping_Addr)
               & ", should be "
               & Mutools.Utils.To_Hex (Number => Expected_Addr));
         end;
      end if;
   end Check_Kernel_Mapping_Address;

   -------------------------------------------------------------------------

   procedure Check_Kernel_Region_Presence
     (Data        : Muxml.XML_Data_Type;
      Region_Kind : String;
      Name_Prefix : String;
      Name_Suffix : String := "")
   is
      CPU_Count    : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => Data);
      Physical_Mem : constant DOM.Core.Node_List
        := XPath_Query
          (N     => Data.Doc,
           XPath => "/system/memory/memory");
   begin
      Mulog.Log (Msg => "Checking presence of" & CPU_Count'Img & " "
                 & Region_Kind & " region(s)");

      for I in 0 .. CPU_Count - 1 loop
         declare
            use type DOM.Core.Node;

            CPU_Str  : constant String
              := Ada.Strings.Fixed.Trim
                (Source => I'Img,
                 Side   => Ada.Strings.Left);
            Mem_Name : constant String
              := Name_Prefix & "_" & CPU_Str & Name_Suffix;
            Node     : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Physical_Mem,
                 Ref_Attr  => "name",
                 Ref_Value => Mem_Name);
         begin
            if Node = null then
               Validation_Errors.Insert
                 (Msg => Mutools.Utils.Capitalize
                    (Str => Region_Kind) & " region '" & Mem_Name
                  & "' for logical CPU " & CPU_Str & " not found");
            end if;
         end;
      end loop;
   end Check_Kernel_Region_Presence;

   -------------------------------------------------------------------------

   procedure Check_Subject_Region_Mappings
     (Data         : Muxml.XML_Data_Type;
      Mapping_Name : String;
      Region_Type  : String)
   is
      Nodes           : constant DOM.Core.Node_List
        := XPath_Query
          (N     => Data.Doc,
           XPath => "/system/memory/memory[@type='" & Region_Type & "']");
      Kernel_Mappings : constant DOM.Core.Node_List
        := XPath_Query
          (N     => Data.Doc,
           XPath => "/system/kernel/memory/cpu/memory");
      Subjects        : constant DOM.Core.Node_List
        := XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");

      Virtual_Base_Addr : Interfaces.Unsigned_64 := 0;
   begin
      Mulog.Log (Msg => "Checking mapping of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " " & Mapping_Name
                 & " memory region(s)");

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
               Validation_Errors.Insert
                 (Msg => Mutools.Utils.Capitalize
                    (Str => Mapping_Name) & " memory region '" & Phys_Name
                  & "' is not mapped by any kernel");
               return;
            elsif Kernel_Map_Count > 1 then
               Validation_Errors.Insert
                 (Msg => Mutools.Utils.Capitalize
                    (Str => Mapping_Name) & " memory region '" & Phys_Name
                  & "' has multiple kernel mappings:" & Kernel_Map_Count'Img);
            end if;

            declare
               Kernel_Mem_Node : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item (List  => Kernel_Mem,
                                         Index => 0);
               Kernel_Mem_Addr : constant Interfaces.Unsigned_64
                 := Interfaces.Unsigned_64'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Kernel_Mem_Node,
                       Name => "virtualAddress"));
               Kernel_CPU_ID   : constant Natural
                 := Natural'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => DOM.Core.Nodes.Parent_Node
                         (N => Kernel_Mem_Node),
                       Name => "id"));
               Subj_Node       : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Nodes     => Subjects,
                    Ref_Attr  => "name",
                    Ref_Value => Mutools.Utils.Decode_Entity_Name
                      (Encoded_Str => Phys_Name));
               Subj_CPU_ID     : constant Natural
                 := Natural'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Subj_Node,
                       Name => "cpu"));
               Subj_ID         : constant Natural
                 := Natural'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Subj_Node,
                       Name => "globalId"));
            begin
               if Kernel_CPU_ID /= Subj_CPU_ID then
                  Validation_Errors.Insert
                    (Msg => Mutools.Utils.Capitalize
                       (Str => Mapping_Name) & " memory region '" & Phys_Name
                     & "' mapped by kernel and subject '"
                     & DOM.Core.Elements.Get_Attribute (Elem => Subj_Node,
                                                        Name => "name")
                     & "' with different CPU ID:" & Kernel_CPU_ID'Img & " /="
                     & Subj_CPU_ID'Img);
               end if;

               --  The virtual base address calculated from the first mapping
               --  is used as reference value for comparison with all
               --  subsequent mappings.

               Check_Kernel_Mapping_Address
                 (Base_Addr    => Virtual_Base_Addr,
                  Mapping_Addr => Kernel_Mem_Addr,
                  Subject_ID   => Subj_ID,
                  Region_Type  => Mutools.Utils.Capitalize
                    (Str => Mapping_Name),
                  Mapping_Name => Phys_Name);
            end;
         end;
      end loop;
   end Check_Subject_Region_Mappings;

   -------------------------------------------------------------------------

   procedure Check_Subject_Region_Presence
     (XML_Data     : Muxml.XML_Data_Type;
      Region_Type  : String;
      Region_Class : String := "subject";
      Region_Size  : Interfaces.Unsigned_64 := Mutools.Constants.Page_Size)
   is
      --  Returns the error message for a given reference node.
      procedure Error_Msg
        (Node    :     DOM.Core.Node;
         Err_Str : out Ada.Strings.Unbounded.Unbounded_String;
         Fatal   : out Boolean);

      --  Returns True if the physical memory region name and size match.
      function Match_Region_Attrs (Left, Right : DOM.Core.Node) return Boolean;

      ----------------------------------------------------------------------

      procedure Error_Msg
        (Node    :     DOM.Core.Node;
         Err_Str : out Ada.Strings.Unbounded.Unbounded_String;
         Fatal   : out Boolean)
      is
         Subj_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Node,
              Name => "name");
         Ref_Name  : constant String := Subj_Name & "|" & Region_Type;
      begin
         Err_Str := Ada.Strings.Unbounded.To_Unbounded_String
           ("Subject " & Region_Type & " region '" & Ref_Name
            & "'"
            & (if Region_Size > 0 then
                   " with size " & Mutools.Utils.To_Hex (Number => Region_Size)
              else "")
            & " for subject '" & Subj_Name & "' not found");
         Fatal := False;
      end Error_Msg;

      ----------------------------------------------------------------------

      function Match_Region_Attrs (Left, Right : DOM.Core.Node) return Boolean
      is
         Subj_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Left,
              Name => "name");
         Ref_Name  : constant String := Subj_Name & "|" & Region_Type;
         Mem_Name  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Right,
            Name => "name");
         Mem_Size  : constant Interfaces.Unsigned_64
           := Interfaces.Unsigned_64'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => Right,
                 Name => "size"));
      begin
         return Ref_Name = Mem_Name and then
           (Region_Size = 0 or else Region_Size = Mem_Size);
      end Match_Region_Attrs;
   begin
      For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "/system/subjects/subject",
         Ref_XPath    => "/system/memory/memory[@type='"
         & Region_Class & "_" & Region_Type & "']",
         Log_Message  => "subject " & Region_Type & " region(s) for presence",
         Error        => Error_Msg'Access,
         Match        => Match_Region_Attrs'Access);
   end Check_Subject_Region_Presence;

   -------------------------------------------------------------------------

   procedure Crash_Audit_After_Image (XML_Data : Muxml.XML_Data_Type)
   is
      Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => XML_Data.Doc,
           XPath => "/system/memory/memory[@type='subject_crash_audit']");
      Addr : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64'Value
          (DOM.Core.Elements.Get_Attribute
             (Elem => Node,
              Name => "physicalAddress"));
      Imgsize : constant Interfaces.Unsigned_64
        := Mutools.XML_Utils.Get_Image_Size (Policy => XML_Data);
   begin
      if Addr < Imgsize then
         Validation_Errors.Insert
           (Msg => "Crash audit region @"
            & Mutools.Utils.To_Hex (Number => Addr) & " within system image "
            & "with end address " & Mutools.Utils.To_Hex (Number => Imgsize));
      end if;
   end Crash_Audit_After_Image;

   -------------------------------------------------------------------------

   procedure Device_Memory_Mappings (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes      : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory[starts-with(@type,'device')]");
      Virt_Nodes : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "//memory[@physical]");
   begin
      Mulog.Log (Msg => "Checking mapping of" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " device memory region(s)");

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
               declare
                  Virt_Name  : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Virt_Mem,
                       Name => "logical");
                  Owner_Name : constant String
                    := DOM.Core.Nodes.Node_Name
                      (Muxml.Utils.Ancestor_Node
                         (Node  => Virt_Mem,
                          Level => 3));
               begin
                  if Owner_Name /= "deviceDomains" then
                     Validation_Errors.Insert
                       (Msg => "Device memory region '"
                        & Phys_Name & "' is mapped by logical memory region '"
                        & Virt_Name & "'"
                        & (if Owner_Name'Length > 0 then " (Owner name: '"
                          & Owner_Name & "')" else ""));
                  end if;
               end;
            end if;
         end;
      end loop;
   end Device_Memory_Mappings;

   -------------------------------------------------------------------------

   procedure Entity_Name_Encoding (XML_Data : Muxml.XML_Data_Type)
   is
      Last_CPU : constant Natural := Natural'Value
        (Muxml.Utils.Get_Attribute
           (Doc   => XML_Data.Doc,
            XPath => "/system/hardware/processor",
            Name  => "cpuCores")) - 1;
      Nodes    : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory[contains(string(@name), '|')]");
      Subjects : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject");

      --  Return True if given name is a valid kernel entity.
      function Is_Valid_Kernel_Entity (Name : String) return Boolean;

      ----------------------------------------------------------------------

      function Is_Valid_Kernel_Entity (Name : String) return Boolean
      is
         Knl_Prefix   : constant String := "kernel_";
         Prefix_Match : constant Boolean
           := (if Name'Length > Knl_Prefix'Length then
                  Name (Name'First .. Name'First + Knl_Prefix'Length - 1)
               = Knl_Prefix
               else False);

         CPU_Nr : Natural := Last_CPU + 1;
      begin
         if Prefix_Match then
            CPU_Nr := Natural'Value
              (Name (Name'First + Knl_Prefix'Length .. Name'Last));
         end if;

         return CPU_Nr <= Last_CPU;
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
               Validation_Errors.Insert
                 (Msg => "Entity '" & Entity_Name & "' "
                  & "encoded in memory region '" & Ref_Name & "' does not "
                  & "exist or is invalid");
            end if;
         end;
      end loop;
   end Entity_Name_Encoding;

   -------------------------------------------------------------------------

   function Is_Adjacent_Region (Left, Right : DOM.Core.Node) return Boolean
   is
   begin
      return Utils.Is_Adjacent_Region
        (Left      => Left,
         Right     => Right,
         Addr_Attr => "physicalAddress");
   end Is_Adjacent_Region;

   -------------------------------------------------------------------------

   procedure Kernel_BSS_Region_Presence (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Kernel_Region_Presence
        (Data        => XML_Data,
         Region_Kind => "kernel BSS",
         Name_Prefix => "kernel_bss");
   end Kernel_BSS_Region_Presence;

   -------------------------------------------------------------------------

   procedure Kernel_Data_Region_Presence (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Kernel_Region_Presence
        (Data        => XML_Data,
         Region_Kind => "kernel data",
         Name_Prefix => "kernel_data");
   end Kernel_Data_Region_Presence;

   -------------------------------------------------------------------------

   procedure Kernel_Intr_Stack_Region_Presence
     (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Kernel_Region_Presence
        (Data        => XML_Data,
         Region_Kind => "kernel interrupt stack",
         Name_Prefix => "kernel_interrupt_stack");
   end Kernel_Intr_Stack_Region_Presence;

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
                     Validation_Errors.Insert
                       (Msg => "Kernel memory region '"
                        & Phys_Name & "' mapped by logical memory region '"
                        & Virt_Name & "' of subject '" & Subj_Name & "'");
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
               Validation_Errors.Insert
                 (Msg => "Kernel PT region '" & Mem_Name
                  & "' for logical CPU " & CPU_Str & " not below 4G");
            end if;
         end;
      end loop;
   end Kernel_PT_Below_4G;

   -------------------------------------------------------------------------

   procedure Kernel_PT_Region_Presence (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Kernel_Region_Presence
        (Data        => XML_Data,
         Region_Kind => "kernel pagetable",
         Name_Prefix => "kernel",
         Name_Suffix => "|pt");
   end Kernel_PT_Region_Presence;

   -------------------------------------------------------------------------

   procedure Kernel_Sched_Group_Info_Mappings (XML_Data : Muxml.XML_Data_Type)
   is
      Sched_Group_Map : constant Mutools.XML_Utils.ID_Map_Array
        := Mutools.XML_Utils.Get_Initial_Scheduling_Group_Subjects
          (Data => XML_Data);
      Kernel_Memory : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/kernel/memory/cpu/memory");
      Subjects : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject");
      Sched_Info_Base_Address : Interfaces.Unsigned_64 := 0;
   begin
      Mulog.Log (Msg => "Checking scheduling group info region kernel mapping"
                 & " for" & Sched_Group_Map'Length'Img
                 & " scheduling group(s)");

      for I in Sched_Group_Map'Range loop
         declare
            use type DOM.Core.Node;

            Sched_Grp_ID : constant String
              := Ada.Strings.Fixed.Trim
                (Source => I'Img,
                 Side   => Ada.Strings.Left);
            Sched_Grp_Region_Name : constant String
              := "scheduling_group_info_" & Sched_Grp_ID;
            Sched_Info_Mappings : constant DOM.Core.Node_List
              := Muxml.Utils.Get_Elements
                (Nodes     => Kernel_Memory,
                 Ref_Attr  => "physical",
                 Ref_Value => Sched_Grp_Region_Name);
            Sched_Info_Mapping : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Sched_Info_Mappings,
                                      Index => 0);
            Mappings_Count : constant Natural
              := DOM.Core.Nodes.Length (List => Sched_Info_Mappings);
            Subj_ID_Str : constant String
              := Ada.Strings.Fixed.Trim
                (Source => Sched_Group_Map (I)'Img,
                 Side   => Ada.Strings.Left);
            Subj_CPU_ID : constant Natural := Natural'Value
              (Muxml.Utils.Get_Attribute
                 (Nodes     => Subjects,
                  Ref_Attr  => "globalId",
                  Ref_Value => Subj_ID_Str,
                  Attr_Name => "cpu"));
         begin
            if Mappings_Count = 0 then
               Validation_Errors.Insert
                 (Msg => "No kernel mapping for info region"
                  & " of scheduling group " & Sched_Grp_ID);
               return;
            elsif Mappings_Count > 1 then
               Validation_Errors.Insert
                 (Msg => "Info region of scheduling group "
                  & Sched_Grp_ID & " has multiple kernel mappings:"
                  & Mappings_Count'Img);
            end if;

            declare
               Kernel_CPU_ID : constant Natural := Natural'Value
                 (DOM.Core.Elements.Get_Attribute
                    (Elem => DOM.Core.Nodes.Parent_Node
                       (N => Sched_Info_Mapping),
                     Name => "id"));
            begin
               if Kernel_CPU_ID /= Subj_CPU_ID then
                  Validation_Errors.Insert
                    (Msg => "Info region of scheduling group"
                     & " " & Sched_Grp_ID
                     & " mapped by kernel running on CPU"
                     & Kernel_CPU_ID'Img & ", should be CPU"
                     & Subj_CPU_ID'Img);
               end if;
            end;

            declare
               Virtual_Address : constant Interfaces.Unsigned_64
                 := Interfaces.Unsigned_64'Value
                   (DOM.Core.Elements.Get_Attribute
                        (Elem => Sched_Info_Mapping,
                         Name => "virtualAddress"));
               Ref_Address : constant Interfaces.Unsigned_64
                 := Sched_Info_Base_Address + Interfaces.Unsigned_64
                   (I - 1) * Mutools.Constants.Page_Size;
            begin
               if Sched_Info_Base_Address = 0 then
                  Sched_Info_Base_Address := Virtual_Address;
               elsif Virtual_Address /= Ref_Address then
                  Validation_Errors.Insert
                    (Msg => "Kernel mapping for info region "
                     & "of scheduling group" & I'Img & " at unexpected kernel "
                     & "virtual address " & Mutools.Utils.To_Hex
                       (Number => Virtual_Address) & ", should be "
                     & Mutools.Utils.To_Hex (Number => Ref_Address));
               end if;
            end;
         end;
      end loop;
   end Kernel_Sched_Group_Info_Mappings;

   -------------------------------------------------------------------------

   procedure Kernel_Stack_Region_Presence (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Kernel_Region_Presence
        (Data        => XML_Data,
         Region_Kind => "kernel stack",
         Name_Prefix => "kernel_stack");
   end Kernel_Stack_Region_Presence;

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
                  Validation_Errors.Insert
                    (Msg => "Physical address of memory "
                     & "region '" & Name & "' does not honor alignment "
                     & Mutools.Utils.To_Hex (Number => Alignment));
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
            Validation_Errors.Insert
              (Msg => "Multiple physical memory regions with"
               & " name '" & Left_Name & "'");
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
         XPath => "/system/hardware/devices/device/memory");
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
      procedure Error_Msg
        (Node    :     DOM.Core.Node;
         Err_Str : out Ada.Strings.Unbounded.Unbounded_String;
         Fatal   : out Boolean);

      ----------------------------------------------------------------------

      procedure Error_Msg
        (Node    :     DOM.Core.Node;
         Err_Str : out Ada.Strings.Unbounded.Unbounded_String;
         Fatal   : out Boolean)
      is
         Logical_Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "logical");
         Refname      : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "physical");
      begin
         Err_Str := Ada.Strings.Unbounded.To_Unbounded_String
           ("Physical memory '" & Refname
            & "' referenced by logical memory '" & Logical_Name
            & "' not found");
         Fatal := True;
      end Error_Msg;
   begin
      For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "//memory[parent::cpu|parent::memory "
         & "and @virtualAddress]",
         Ref_XPath    => "/system/memory/memory",
         Log_Message  => "physical memory references",
         Error        => Error_Msg'Access,
         Match        => Mutools.Match.Is_Valid_Reference'Access);
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
                       B         => Mutools.Constants.Page_Size,
                       Error_Msg => "not multiple of page size (4K)");
   end Region_Size;

   -------------------------------------------------------------------------

   procedure Scheduling_Group_Info_Region_Presence
     (XML_Data : Muxml.XML_Data_Type)
   is
      Sched_Groups : constant Mutools.XML_Utils.ID_Map_Array
        := Mutools.XML_Utils.Get_Initial_Scheduling_Group_Subjects
          (Data => XML_Data);
      Sched_Memory : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory[@type='subject_scheduling_info']");
   begin
      Mulog.Log (Msg => "Checking" & Sched_Groups'Length'Img
                 & " scheduling group info region(s) for presence");

      for I in Sched_Groups'Range loop
         declare
            use type DOM.Core.Node;

            Name : constant String
              := "scheduling_group_info_" & Ada.Strings.Fixed.Trim
                (Source => I'Img,
                 Side   => Ada.Strings.Left);
            Mem  : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Sched_Memory,
                 Ref_Attr  => "name",
                 Ref_Value => Name);
         begin
            if Mem = null then
               Validation_Errors.Insert
                 (Msg => "Scheduling group info region of "
                  & "scheduling group" & I'Img & " not found");
            end if;
         end;
      end loop;
   end Scheduling_Group_Info_Region_Presence;

   -------------------------------------------------------------------------

   procedure Subject_FPU_State_Mappings (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Subject_Region_Mappings
        (Data         => XML_Data,
         Mapping_Name => "Subject FPU state",
         Region_Type  => "kernel_fpu");
   end Subject_FPU_State_Mappings;

   -------------------------------------------------------------------------

   procedure Subject_FPU_State_Region_Presence
     (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Subject_Region_Presence
        (XML_Data     => XML_Data,
         Region_Type  => "fpu",
         Region_Class => "kernel");
   end Subject_FPU_State_Region_Presence;

   -------------------------------------------------------------------------

   procedure Subject_Interrupts_Mappings (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Subject_Region_Mappings
        (Data         => XML_Data,
         Mapping_Name => "subject interrupts",
         Region_Type  => "subject_interrupts");
   end Subject_Interrupts_Mappings;

   -------------------------------------------------------------------------

   procedure Subject_Interrupts_Region_Presence
     (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Subject_Region_Presence
        (XML_Data    => XML_Data,
         Region_Type => "interrupts");
   end Subject_Interrupts_Region_Presence;

   -------------------------------------------------------------------------

   procedure Subject_IOBM_Region_Presence (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Subject_Region_Presence
        (XML_Data     => XML_Data,
         Region_Type  => "iobm",
         Region_Class => "system",
         Region_Size  => 16#2000#);
   end Subject_IOBM_Region_Presence;

   -------------------------------------------------------------------------

   procedure Subject_MSR_Store_Mappings (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Subject_Region_Mappings
        (Data         => XML_Data,
         Mapping_Name => "subject MSR store",
         Region_Type  => "kernel_msrstore");
   end Subject_MSR_Store_Mappings;

   -------------------------------------------------------------------------

   procedure Subject_MSR_Store_Region_Presence
     (XML_Data : Muxml.XML_Data_Type)
   is
      MSR_Regions : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory[@type='kernel_msrstore']");
      Subjects    : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject[vcpu/msrs/msr]");
      Subj_Count  : constant Natural
        := DOM.Core.Nodes.Length (List => Subjects);
   begin
      if Subj_Count = 0 then
         return;
      end if;

      Mulog.Log (Msg => "Checking" & Subj_Count'Img & " subject MSR store "
                 & "region(s) for presence");
      for I in 0 .. Subj_Count - 1 loop
         declare
            use type DOM.Core.Node;

            Cur_Subj   : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Subjects,
                                      Index => I);
            Subj_Name  : constant String
              := DOM.Core.Elements.Get_Attribute (Elem => Cur_Subj,
                                                  Name => "name");
            MSRs       : constant DOM.Core.Node_List
              := XPath_Query
                (N     => Cur_Subj,
                 XPath => "vcpu/msrs/msr[@mode='rw' or @mode='w']");
            Controls   : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Cur_Subj,
                 XPath => "vcpu/vmx/controls");
            Debug_Ctrl : constant Boolean
              := Mutools.XML_Utils.Has_Managed_DEBUGCTL
                (Controls => Controls);
            PERF_Ctrl  : constant Boolean
              := Mutools.XML_Utils.Has_Managed_PERFGLOBALCTRL
                (Controls => Controls);
            PAT_Ctrl   : constant Boolean
              := Mutools.XML_Utils.Has_Managed_PAT (Controls => Controls);
            EFER_Ctrl  : constant Boolean
              := Mutools.XML_Utils.Has_Managed_EFER (Controls => Controls);
            MSR_Count  : constant Natural
              := Mutools.XML_Utils.Calculate_MSR_Count
                (MSRs                   => MSRs,
                 DEBUGCTL_Control       => Debug_Ctrl,
                 PAT_Control            => PAT_Ctrl,
                 PERFGLOBALCTRL_Control => PERF_Ctrl,
                 EFER_Control           => EFER_Ctrl);
         begin
            if MSR_Count > 0 then
               declare
                  MSR_Store : constant DOM.Core.Node := Muxml.Utils.Get_Element
                    (Nodes     => MSR_Regions,
                     Ref_Attr  => "name",
                     Ref_Value => Subj_Name & "|msrstore");
                  Size_Str  : constant String
                    := (if MSR_Store /= null then
                           DOM.Core.Elements.Get_Attribute
                          (Elem => MSR_Store,
                           Name => "size")
                          else "0");
               begin
                  if MSR_Store = null
                    or else Interfaces.Unsigned_64'Value (Size_Str)
                    /= Mutools.Constants.Page_Size
                  then
                     Validation_Errors.Insert
                       (Msg => "Subject MSR store region '"
                        & Subj_Name & "|msrstore' with size "
                        & Mutools.Utils.To_Hex
                          (Number => Mutools.Constants.Page_Size)
                        & " for subject '" & Subj_Name & "' not found");
                  end if;
               end;
            end if;
         end;
      end loop;
   end Subject_MSR_Store_Region_Presence;

   -------------------------------------------------------------------------

   procedure Subject_MSRBM_Region_Presence (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Subject_Region_Presence
        (XML_Data     => XML_Data,
         Region_Type  => "msrbm",
         Region_Class => "system");
   end Subject_MSRBM_Region_Presence;

   -------------------------------------------------------------------------

   procedure Subject_Sched_Group_Info_Mappings
     (XML_Data : Muxml.XML_Data_Type)
   is
      Subjects : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/subjects/subject");
      Subj_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Subjects);
   begin
      Mulog.Log (Msg => "Checking scheduling group info region mappings of"
                 & Subj_Count'Img & " subject(s)");

      for I in 0 .. Subj_Count - 1 loop
         declare
            use type DOM.Core.Node;

            Subject : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Subjects,
                                      Index => I);
            Sched_Grp_ID : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subject,
                 Name => "schedGroupId");
            Sched_Grp_Region_Name : constant String
              := "scheduling_group_info_" & Sched_Grp_ID;
            Sched_Info_Mapping : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subject,
                 XPath => "memory/memory[@physical='" & Sched_Grp_Region_Name
                 & "']");
         begin
            if Sched_Info_Mapping = null then
               declare
                  Subj_Name : constant String
                    := DOM.Core.Elements.Get_Attribute (Elem => Subject,
                                                        Name => "name");
               begin
                  Validation_Errors.Insert
                    (Msg => "Subject '" & Subj_Name
                     & "' has no mapping for info region of scheduling group "
                     & Sched_Grp_ID);
               end;
            end if;
         end;
      end loop;
   end Subject_Sched_Group_Info_Mappings;

   -------------------------------------------------------------------------

   procedure Subject_State_Mappings (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Subject_Region_Mappings
        (Data         => XML_Data,
         Mapping_Name => "subject state",
         Region_Type  => "subject_state");
   end Subject_State_Mappings;

   -------------------------------------------------------------------------

   procedure Subject_State_Region_Presence (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Subject_Region_Presence
        (XML_Data    => XML_Data,
         Region_Type => "state");
   end Subject_State_Region_Presence;

   -------------------------------------------------------------------------

   procedure Subject_Timed_Event_Mappings (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Subject_Region_Mappings
        (Data         => XML_Data,
         Mapping_Name => "timed event",
         Region_Type  => "subject_timed_event");
   end Subject_Timed_Event_Mappings;

   -------------------------------------------------------------------------

   procedure Subject_Timed_Event_Region_Presence
     (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Subject_Region_Presence
        (XML_Data    => XML_Data,
         Region_Type => "timed_event");
   end Subject_Timed_Event_Region_Presence;

   -------------------------------------------------------------------------

   procedure Subject_VMCS_Mappings (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Subject_Region_Mappings
        (Data         => XML_Data,
         Mapping_Name => "VMCS",
         Region_Type  => "kernel_vmcs");
   end Subject_VMCS_Mappings;

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
               Validation_Errors.Insert
                 (Msg => "System memory region '"
                  & Phys_Name & "' is mapped by logical memory region '"
                  & DOM.Core.Elements.Get_Attribute (Elem => Virt_Mem,
                                                     Name => "logical") & "'");
            end if;
         end;
      end loop;
   end System_Memory_Mappings;

   -------------------------------------------------------------------------

   procedure Uncached_Crash_Audit_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory[@type='subject_crash_audit']");
      Count : constant Natural := DOM.Core.Nodes.Length (List => Nodes);
   begin
      if Count /= 1 then
         Validation_Errors.Insert
           (Msg => "One crash audit region expected, found" & Count'Img);
         return;
      end if;

      declare
         Node : constant DOM.Core.Node
           := DOM.Core.Nodes.Item (List  => Nodes,
                                   Index => 0);
         Caching : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Node,
              Name => "caching");
      begin
         if Caching /= "UC" then
            Validation_Errors.Insert
              (Msg => "Crash audit region caching is " & Caching
               & " instead of UC");
         end if;
      end;
   end Uncached_Crash_Audit_Presence;

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
                       B         => Mutools.Constants.Page_Size,
                       Error_Msg => "not page aligned");
   end Virtual_Address_Alignment;

   -------------------------------------------------------------------------

   procedure VMCS_Region_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      --  Returns the error message for a given reference node.
      procedure Error_Msg
        (Node    :     DOM.Core.Node;
         Err_Str : out Ada.Strings.Unbounded.Unbounded_String;
         Fatal   : out Boolean);

      --  Returns True if the physical memory region name matches.
      function Match_Region_Name (Left, Right : DOM.Core.Node) return Boolean;

      ----------------------------------------------------------------------

      procedure Error_Msg
        (Node    :     DOM.Core.Node;
         Err_Str : out Ada.Strings.Unbounded.Unbounded_String;
         Fatal   : out Boolean)
      is
         Subj_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Node,
              Name => "name");
         Ref_Name  : constant String := Subj_Name & "|vmcs";
      begin
         Err_Str := Ada.Strings.Unbounded.To_Unbounded_String
           ("VMCS region '" & Ref_Name & "' for subject '" & Subj_Name
            & "' not found");
         Fatal := False;
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
      For_Each_Match
        (XML_Data     => XML_Data,
         Source_XPath => "/system/subjects/subject",
         Ref_XPath    => "/system/memory/memory[@type='kernel_vmcs']",
         Log_Message  => "VMCS region(s) for presence",
         Error        => Error_Msg'Access,
         Match        => Match_Region_Name'Access);
   end VMCS_Region_Presence;

   -------------------------------------------------------------------------

   procedure VMCS_Region_Size (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory[@type='kernel_vmcs']");
   begin
      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "VMCS memory",
                       Attr      => "size",
                       Name_Attr => "name",
                       Test      => Equals'Access,
                       B         => Mutools.Constants.Page_Size,
                       Error_Msg => "not 4K");
   end VMCS_Region_Size;

   -------------------------------------------------------------------------

   procedure VMXON_Consecutiveness (XML_Data : Muxml.XML_Data_Type)
   is
      XPath : constant String := "/system/memory/memory[@type='system_vmxon']";

      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => XPath);

      --  Returns the error message for a given reference node.
      procedure Error_Msg
        (Node    :     DOM.Core.Node;
         Err_Str : out Ada.Strings.Unbounded.Unbounded_String;
         Fatal   : out Boolean);

      ----------------------------------------------------------------------

      procedure Error_Msg
        (Node    :     DOM.Core.Node;
         Err_Str : out Ada.Strings.Unbounded.Unbounded_String;
         Fatal   : out Boolean)
      is
         Name : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "name");
      begin
         Err_Str := Ada.Strings.Unbounded.To_Unbounded_String
           ("Memory region '" & Name & "' not adjacent to other VMXON"
            & " regions");
         Fatal := False;
      end Error_Msg;
   begin
      if DOM.Core.Nodes.Length (List => Nodes) < 2 then
         return;
      end if;

      For_Each_Match (Source_Nodes => Nodes,
                      Ref_Nodes    => Nodes,
                      Log_Message  => "VMXON region(s) for consecutiveness",
                      Error        => Error_Msg'Access,
                      Match        => Is_Adjacent_Region'Access);
   end VMXON_Consecutiveness;

   -------------------------------------------------------------------------

   procedure VMXON_In_Lowmem (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory[@type='system_vmxon']");
   begin
      Check_Attribute
        (Nodes     => Nodes,
         Node_Type => "VMXON memory",
         Attr      => "physicalAddress",
         Name_Attr => "name",
         Test      => Less_Than'Access,
         B         => One_Megabyte - Mutools.Constants.Page_Size,
         Error_Msg => "not below 1 MiB");
   end VMXON_In_Lowmem;

   -------------------------------------------------------------------------

   procedure VMXON_Region_Presence (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Check_Kernel_Region_Presence
        (Data        => XML_Data,
         Region_Kind => "VMXON",
         Name_Prefix => "kernel",
         Name_Suffix => "|vmxon");
   end VMXON_Region_Presence;

   -------------------------------------------------------------------------

   procedure VMXON_Region_Size (XML_Data : Muxml.XML_Data_Type)
   is
      All_Nodes : constant DOM.Core.Node_List := XPath_Query
        (N     => XML_Data.Doc,
         XPath => "/system/memory/memory");
      Nodes     : constant DOM.Core.Node_List
        := Muxml.Utils.Get_Elements (Nodes     => All_Nodes,
                                     Ref_Attr  => "type",
                                     Ref_Value => "system_vmxon");
   begin
      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "VMXON memory",
                       Attr      => "size",
                       Name_Attr => "name",
                       Test      => Equals'Access,
                       B         => Mutools.Constants.Page_Size,
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
                       B         => Mutools.Constants.Page_Size,
                       Error_Msg => "not 4K");
   end VTd_Context_Region_Size;

   -------------------------------------------------------------------------

   procedure VTd_IRT_Region_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      use type DOM.Core.Node;

      Region : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => XML_Data.Doc,
           XPath => "/system/memory/memory[@type='system_vtd_ir']");
   begin
      Mulog.Log (Msg => "Checking presence of VT-d interrupt remapping table"
                 & " memory region");

      if Region = null then
         Validation_Errors.Insert
           (Msg => "VT-d interrupt remapping table memory region not found");
      end if;
   end VTd_IRT_Region_Presence;

   -------------------------------------------------------------------------

   procedure VTd_Root_Region_Presence (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory[@type='system_vtd_root']");
   begin
      Mulog.Log (Msg => "Checking presence of VT-d root table region");

      if DOM.Core.Nodes.Length (List => Nodes) /= 1 then
         Validation_Errors.Insert
           (Msg => "VT-d root table memory region not found");
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
                       B         => Mutools.Constants.Page_Size,
                       Error_Msg => "not 4K");
   end VTd_Root_Region_Size;

end Mucfgcheck.Memory;
