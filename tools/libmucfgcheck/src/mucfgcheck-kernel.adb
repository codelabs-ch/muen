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

with DOM.Core.Append_Node;
with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Utils;
with Mutools.XML_Utils;
with Mutools.Match;

with Mucfgcheck.Utils;

package body Mucfgcheck.Kernel
is

   -------------------------------------------------------------------------

   procedure CPU_Local_BSS_Address_Equality (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/kernel/memory/cpu/memory[@logical='bss']");
      Addr  : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Value
        (DOM.Core.Elements.Get_Attribute
           (Elem => DOM.Core.Nodes.Item (List  => Nodes,
                                         Index => 0),
            Name => "virtualAddress"));
   begin
      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "kernel CPU-local BSS",
                       Attr      => "virtualAddress",
                       Name_Attr => "physical",
                       Test      => Equals'Access,
                       B         => Addr,
                       Error_Msg => "differs");
   end CPU_Local_BSS_Address_Equality;

   -------------------------------------------------------------------------

   procedure CPU_Local_Data_Address_Equality (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/kernel/memory/cpu/memory[@logical='data']");
      Addr  : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Value
        (DOM.Core.Elements.Get_Attribute
           (Elem => DOM.Core.Nodes.Item (List  => Nodes,
                                         Index => 0),
            Name => "virtualAddress"));
   begin
      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "kernel CPU-local data",
                       Attr      => "virtualAddress",
                       Name_Attr => "physical",
                       Test      => Equals'Access,
                       B         => Addr,
                       Error_Msg => "differs");
   end CPU_Local_Data_Address_Equality;

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

   procedure Crash_Audit_Address_Equality (XML_Data : Muxml.XML_Data_Type)
   is
      Phys_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => XML_Data.Doc,
           XPath => "/system/memory/memory[@type='subject_crash_audit']");
      Phys_Name : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Phys_Node,
           Name => "name");
      Mappings : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/kernel/memory/cpu/memory[@physical='"
           & Phys_Name & "']");
      Count : constant Natural
        := DOM.Core.Nodes.Length (List => Mappings);
      CPU_Count : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => XML_Data);
      Addr : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64'Value
          (DOM.Core.Elements.Get_Attribute
             (Elem => DOM.Core.Nodes.Item
                (List  => Mappings,
                 Index => 0),
              Name => "virtualAddress"));
   begin
      if Count /= CPU_Count then
         raise Validation_Error with "Required crash audit mappings not "
           & "present (expected" &  CPU_Count'Img & ", found" & Count'Img
           & ")";
      end if;

      Check_Attribute (Nodes     => Mappings,
                       Node_Type => "crash audit region",
                       Attr      => "virtualAddress",
                       Name_Attr => "physical",
                       Test      => Equals'Access,
                       B         => Addr,
                       Error_Msg => "differs");
   end Crash_Audit_Address_Equality;

   -------------------------------------------------------------------------

   procedure Global_Data_Address_Equality (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes     : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/kernel/memory/cpu/"
           & "memory[@physical='kernel_global_data']");
      Map_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Nodes);
      CPU_Count : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => XML_Data);
      Addr      : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64'Value
          (DOM.Core.Elements.Get_Attribute
             (Elem => DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => 0),
              Name => "virtualAddress"));
   begin
      if Map_Count /= CPU_Count then
         raise Validation_Error with "Required kernel global data mappings not"
           & " present (expected" &  CPU_Count'Img & ", found" & Map_Count'Img
           & ")";
      end if;

      Check_Attribute (Nodes     => Nodes,
                       Node_Type => "kernel global data",
                       Attr      => "virtualAddress",
                       Name_Attr => "physical",
                       Test      => Equals'Access,
                       B         => Addr,
                       Error_Msg => "differs");
   end Global_Data_Address_Equality;

   -------------------------------------------------------------------------

   procedure IOMMU_Consecutiveness (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant Muxml.Utils.Matching_Pairs_Type
        := Muxml.Utils.Get_Matching
          (XML_Data    => XML_Data,
           Left_XPath  => "/system/kernel/devices/device/memory",
           Right_XPath => "/system/hardware/devices/device[capabilities/"
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
      begin
         return Utils.Is_Adjacent_Region
           (Left      => Left,
            Right     => Right,
            Addr_Attr => "virtualAddress");
      end Is_Adjacent_IOMMU_Region;
   begin
      if DOM.Core.Nodes.Length (List => Nodes.Left) < 2 then
         return;
      end if;

      Mutools.XML_Utils.Set_Memory_Size
        (Virtual_Mem_Nodes => Nodes.Left,
         Ref_Nodes         => McKae.XML.XPath.XIA.XPath_Query
           (N     => XML_Data.Doc,
            XPath => "/system/hardware/devices/device/memory"));
      For_Each_Match (Source_Nodes => Nodes.Left,
                      Ref_Nodes    => Nodes.Left,
                      Log_Message  => "IOMMU region(s) for consecutiveness",
                      Error        => Error_Msg'Access,
                      Match        => Is_Adjacent_IOMMU_Region'Access);
   end IOMMU_Consecutiveness;

   -------------------------------------------------------------------------

   procedure Stack_Address_Equality (XML_Data : Muxml.XML_Data_Type)
   is
      Mem_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/kernel/memory/cpu/memory");
   begin
      Stack :
      declare
         Nodes : constant DOM.Core.Node_List
           := Muxml.Utils.Get_Elements
             (Nodes     => Mem_Nodes,
              Ref_Attr  => "logical",
              Ref_Value => "stack");
         Addr  : constant Interfaces.Unsigned_64
           := Interfaces.Unsigned_64'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Item
                   (List  => Nodes,
                    Index => 0),
                 Name => "virtualAddress"));
      begin
         Check_Attribute (Nodes     => Nodes,
                          Node_Type => "kernel stack memory",
                          Attr      => "virtualAddress",
                          Name_Attr => "physical",
                          Test      => Equals'Access,
                          B         => Addr,
                          Error_Msg => "differs");
      end Stack;

      Interrupt_Stack :
      declare
         Nodes : constant DOM.Core.Node_List
           := Muxml.Utils.Get_Elements
             (Nodes     => Mem_Nodes,
              Ref_Attr  => "logical",
              Ref_Value => "interrupt_stack");
         Addr  : constant Interfaces.Unsigned_64
           := Interfaces.Unsigned_64'Value
             (DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Item
                   (List  => Nodes,
                    Index => 0),
                 Name => "virtualAddress"));
      begin
         Check_Attribute (Nodes     => Nodes,
                          Node_Type => "kernel interrupt stack memory",
                          Attr      => "virtualAddress",
                          Name_Attr => "physical",
                          Test      => Equals'Access,
                          B         => Addr,
                          Error_Msg => "differs");
      end Interrupt_Stack;
   end Stack_Address_Equality;

   -------------------------------------------------------------------------

   procedure Stack_Layout (XML_Data : Muxml.XML_Data_Type)
   is
      Phys_Mem  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory");
      CPU_Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/kernel/memory/cpu");
      CPU_Count : constant Positive
        := DOM.Core.Nodes.Length (List => CPU_Nodes);
   begin
      Mulog.Log (Msg => "Checking kernel stack layout of"
                 & CPU_Count'Img & " CPUs");

      for I in 0 .. CPU_Count - 1 loop
         declare
            use type DOM.Core.Node;

            CPU_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => CPU_Nodes,
                                      Index => I);
            Virt_Mem : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => CPU_Node,
                 XPath => "memory");
            Stack_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Virt_Mem,
                 Ref_Attr  => "logical",
                 Ref_Value => "stack");
            Intr_Stack_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Virt_Mem,
                 Ref_Attr  => "logical",
                 Ref_Value => "interrupt_stack");

            --  Check guard pages below and above given virtual stack node.
            procedure Check_Guards (Stack_Node : DOM.Core.Node);

            ----------------------------------------------------------------

            procedure Check_Guards (Stack_Node : DOM.Core.Node)
            is
               Stack_Phys_Name : constant String
                 := DOM.Core.Elements.Get_Attribute
                   (Elem => Stack_Node,
                    Name => "physical");
               Stack_Phys_Node : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Nodes     => Phys_Mem,
                    Ref_Attr  => "name",
                    Ref_Value => Stack_Phys_Name);
               Stack_Size : constant Interfaces.Unsigned_64
                 := Interfaces.Unsigned_64'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Stack_Phys_Node,
                       Name => "size"));
               Stack_Addr : constant Interfaces.Unsigned_64
                 := Interfaces.Unsigned_64'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Stack_Node,
                       Name => "virtualAddress"));
               Node : DOM.Core.Node;
            begin

               --  Check guard page below.

               Node := Mutools.XML_Utils.Get_Enclosing_Virtual_Region
                 (Virtual_Address => Stack_Addr - 1,
                  Physical_Memory => Phys_Mem,
                  Logical_Memory  => Virt_Mem);
               if Node /= null then
                  raise Validation_Error with "Expected unmapped page on CPU"
                    & I'Img & " below kernel stack @ "
                    & Mutools.Utils.To_Hex (Number => Stack_Addr)
                    & ", found '"
                    & DOM.Core.Elements.Get_Attribute
                    (Elem => Node,
                     Name => "logical") & "'";
               end if;

               --  Check guard page above.

               Node := Mutools.XML_Utils.Get_Enclosing_Virtual_Region
                 (Virtual_Address => Stack_Addr + Stack_Size,
                  Physical_Memory => Phys_Mem,
                  Logical_Memory  => Virt_Mem);
               if Node /= null then
                  raise Validation_Error with "Expected unmapped page on CPU"
                    & I'Img & " above kernel stack @ "
                    & Mutools.Utils.To_Hex
                    (Number => Stack_Addr + Stack_Size - 1)
                    & ", found '"
                    & DOM.Core.Elements.Get_Attribute
                    (Elem => Node,
                     Name => "logical") & "'";
               end if;
            end Check_Guards;
         begin
            if Stack_Node = null then
               raise Validation_Error with "CPU" & I'Img & " has no stack "
                 & "region mapping";
            end if;
            if Intr_Stack_Node = null then
               raise Validation_Error with "CPU" & I'Img & " has no interrupt "
                 & "stack region mapping";
            end if;

            Check_Guards (Stack_Node => Stack_Node);
            Check_Guards (Stack_Node => Intr_Stack_Node);
         end;
      end loop;
   end Stack_Layout;

   -------------------------------------------------------------------------

   procedure System_Board_Reference (XML_Data : Muxml.XML_Data_Type)
   is
      Pairs : constant Muxml.Utils.Matching_Pairs_Type
        := Muxml.Utils.Get_Matching
          (XML_Data    => XML_Data,
           Left_XPath  => "/system/kernel/devices/device",
           Right_XPath => "/system/hardware/devices/device[capabilities/"
           & "capability/@name='systemboard']",
           Match       => Mutools.Match.Is_Valid_Reference'Access);
   begin
      Mulog.Log (Msg => "Checking kernel system board reference");

      if DOM.Core.Nodes.Length (List => Pairs.Right) /= 1 then
         raise Validation_Error with "Kernel system board reference not "
           & "present";
      end if;

      declare
         use type DOM.Core.Node;

         Reset_Port : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => DOM.Core.Nodes.Item
                (List  => Pairs.Left,
                 Index => 0),
              XPath => "ioPort[@logical='reset_port']");
         Poweroff_Port : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => DOM.Core.Nodes.Item
                (List  => Pairs.Left,
                 Index => 0),
              XPath => "ioPort[@logical='poweroff_port']");
      begin
         if Reset_Port = null then
            raise Validation_Error with "Kernel system board reference does "
              & "not provide logical reset port";
         end if;
         if Poweroff_Port = null then
            raise Validation_Error with "Kernel system board reference does "
              & "not provide logical poweroff port";
         end if;
      end;
   end System_Board_Reference;

   -------------------------------------------------------------------------

   procedure Virtual_Memory_Overlap (XML_Data : Muxml.XML_Data_Type)
   is
      Physical_Mem   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/memory/memory");
      Physical_Devs  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/hardware/devices/device");
      CPUs           : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/kernel/memory/cpu");
      Kernel_Dev_Mem : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => XML_Data.Doc,
           XPath => "/system/kernel/devices/device/memory");
      KDev_Mem_Count : constant Natural := DOM.Core.Nodes.Length
        (List => Kernel_Dev_Mem);
      Dev_Mem_Nodes  : DOM.Core.Node_List;
   begin
      for I in 0 .. KDev_Mem_Count - 1 loop
         declare
            Cur_Node      : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Kernel_Dev_Mem,
               Index => I);
            Dev_Node      : constant DOM.Core.Node
              := DOM.Core.Nodes.Parent_Node (N => Cur_Node);
            Phys_Dev_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Dev_Node,
               Name => "physical");
            Log_Dev_Name  : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Dev_Node,
               Name => "logical");
            Device        : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Physical_Devs,
                 Ref_Attr  => "name",
                 Ref_Value => Phys_Dev_Name);
            Mem_Node      : constant DOM.Core.Node := DOM.Core.Nodes.Clone_Node
              (N    => Cur_Node,
               Deep => False);
            Mem_Name      : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Mem_Node,
                 Name => "logical");
         begin
            Mutools.XML_Utils.Set_Memory_Size
              (Virtual_Mem_Node => Mem_Node,
               Ref_Nodes        => McKae.XML.XPath.XIA.XPath_Query
                 (N     => Device,
                  XPath => "memory"));
            DOM.Core.Elements.Set_Attribute
              (Elem  => Mem_Node,
               Name  => "logical",
               Value => Log_Dev_Name & "->" & Mem_Name);
            DOM.Core.Append_Node (List => Dev_Mem_Nodes,
                                  N    => Mem_Node);
         end;
      end loop;

      Check_CPUs :
      for I in 0 .. DOM.Core.Nodes.Length (List => CPUs) - 1 loop
         declare
            CPU    : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => CPUs,
               Index => I);
            CPU_ID : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => CPU,
                 Name => "id");
            Memory : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query (N     => CPU,
                                                  XPath => "memory");
            Mem_Nodes : DOM.Core.Node_List;
         begin
            if DOM.Core.Nodes.Length (List => Memory) + KDev_Mem_Count > 1 then
               Mulog.Log (Msg => "Checking virtual memory overlap of kernel "
                          & "running on CPU " & CPU_ID);

               for J in 0 .. DOM.Core.Nodes.Length (List => Memory) - 1 loop
                  declare
                     Cur_Node : constant DOM.Core.Node
                       := DOM.Core.Nodes.Clone_Node
                         (N    => DOM.Core.Nodes.Item
                              (List  => Memory,
                               Index => J),
                          Deep => False);
                  begin
                     Mutools.XML_Utils.Set_Memory_Size
                       (Virtual_Mem_Node => Cur_Node,
                        Ref_Nodes        => Physical_Mem);
                     DOM.Core.Append_Node (List => Mem_Nodes,
                                           N    => Cur_Node);
                  end;
               end loop;

               Muxml.Utils.Append (Left  => Mem_Nodes,
                                   Right => Dev_Mem_Nodes);

               Check_Memory_Overlap
                 (Nodes        => Mem_Nodes,
                  Region_Type  => "virtual memory region",
                  Address_Attr => "virtualAddress",
                  Name_Attr    => "logical",
                  Add_Msg      => " of kernel running on CPU " & CPU_ID);
            end if;
         end;
      end loop Check_CPUs;
   end Virtual_Memory_Overlap;

end Mucfgcheck.Kernel;
