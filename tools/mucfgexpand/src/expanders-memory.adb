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

with Ada.Exceptions;
with Ada.Strings.Fixed;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Utils;
with Mutools.Constants;
with Mutools.XML_Utils;
with Mucfgcheck.Kernel;
with Mucfgcheck.Subject;

with Expanders.Config;
with Expanders.XML_Utils;

package body Expanders.Memory
is

   --  Physical start address of VMX-related memory regions.
   VMX_Start_Address : constant Interfaces.Unsigned_64 := 16#8000#;

   --  Add physical memory region with specified parameters for each subject to
   --  given XML policy. Region type, e.g. "pt" specifies the usage of the
   --  memory region while region class, e.g. "subject", defines if the region
   --  is mappable by subjects or only the kernel.
   procedure Add_Subject_Memory_Region
     (Data         : in out Muxml.XML_Data_Type;
      Region_Type  :        String;
      Region_Class :        String := "subject";
      Address      :        String := "";
      Size         :        String := "16#1000#";
      Alignment    :        String := "16#1000#";
      Caching      :        String := "WB");

   -------------------------------------------------------------------------

   procedure Add_AP_Trampoline (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Mulog.Log (Msg => "Adding AP trampoline memory region");
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => "trampoline",
         Address     => "16#1000#",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "system");
   end Add_AP_Trampoline;

   -------------------------------------------------------------------------

   procedure Add_Kernel_CPU_Local_Memory (Data : in out Muxml.XML_Data_Type)
   is
      use type Interfaces.Unsigned_64;

      CPU_Count   : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => Data);
      Data_Addr   : constant String := Mutools.Utils.To_Hex
        (Number => Config.Kernel_Data_Section_Addr);
      Data_Size   : constant String := Mutools.Utils.To_Hex
        (Number => Config.Kernel_Data_Section_Size);
      Data_Offset : constant String := Mutools.Utils.To_Hex
        (Number => Config.Kernel_Data_Section_Addr
         - Config.Kernel_Text_Section_Addr);
      BSS_Addr    : constant String := Mutools.Utils.To_Hex
        (Number => Config.Kernel_BSS_Section_Addr);
      BSS_Size    : constant String := Mutools.Utils.To_Hex
        (Number => Config.Kernel_BSS_Section_Size);
   begin
      for I in Natural range 0 .. CPU_Count - 1 loop
         declare
            CPU_Str : constant String := Ada.Strings.Fixed.Trim
              (Source => I'Img,
               Side   => Ada.Strings.Left);
         begin
            Mulog.Log (Msg => "Adding kernel local memory regions for CPU "
                       & CPU_Str);

            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => "kernel_data_" & CPU_Str,
               Address     => (if I = 0 then Data_Addr else ""),
               Size        => Data_Size,
               Caching     => "WB",
               Alignment   => "16#1000#",
               File_Name   => "kernel",
               File_Offset => Data_Offset,
               Memory_Type => "kernel_binary");
            Mutools.XML_Utils.Add_Memory_Region
              (Policy       => Data,
               Name         => "kernel_bss_" & CPU_Str,
               Address      => (if I = 0 then BSS_Addr else ""),
               Size         => BSS_Size,
               Caching      => "WB",
               Alignment    => "16#1000#",
               Memory_Type  => "kernel_binary",
               Fill_Pattern => "16#00#");
         end;
      end loop;
   end Add_Kernel_CPU_Local_Memory;

   -------------------------------------------------------------------------

   procedure Add_Kernel_PTs (Data : in out Muxml.XML_Data_Type)
   is
      CPU_Count : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => Data);
   begin

      --  Validate that there are no overlapping kernel memory mappings and
      --  raise a sensible error message if necessary. This special check is
      --  required here because expanded memory regions might collide with
      --  existing ones.

      Mucfgcheck.Kernel.Virtual_Memory_Overlap (XML_Data => Data);

      for I in 0 .. CPU_Count - 1 loop
         declare
            CPU_Str : constant String := Ada.Strings.Fixed.Trim
              (Source => I'Img,
               Side   => Ada.Strings.Left);
            Size : constant Interfaces.Unsigned_64
              := XML_Utils.Calculate_PT_Size
                (Policy             => Data,
                 Paging_Levels      => 4,
                 Large_Pages        => False,
                 Dev_Virt_Mem_XPath => "/system/kernel/devices/device/memory",
                 Virt_Mem_XPath     => "/system/kernel/memory/cpu[@id='" &
                   CPU_Str & "']/memory");
            Size_Str : constant String := Mutools.Utils.To_Hex
              (Number => Size);
         begin
            Mulog.Log (Msg => "Adding pagetable region with size " & Size_Str
                       & " for CPU " & CPU_Str);
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => "kernel_" & CPU_Str & "|pt",
               Address     => "",
               Size        => Size_Str,
               Caching     => "WB",
               Alignment   => "16#1000#",
               Memory_Type => "system_pt",
               File_Name   => "kernel_pt_" & CPU_Str,
               File_Offset => "none");
         end;
      end loop;
   end Add_Kernel_PTs;

   -------------------------------------------------------------------------

   procedure Add_Kernel_Shared_Memory (Data : in out Muxml.XML_Data_Type)
   is
      use type Interfaces.Unsigned_64;
   begin
      Mulog.Log (Msg => "Adding kernel shared memory regions");

      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => "kernel_text",
         Address     => Mutools.Utils.To_Hex
           (Number => Config.Kernel_Text_Section_Addr),
         Size        => Mutools.Utils.To_Hex
           (Number => Config.Kernel_Text_Section_Size),
         Caching     => "WB",
         Alignment   => "16#1000#",
         File_Name   => "kernel",
         File_Offset => "16#0000#",
         Memory_Type => "kernel_binary");
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => "kernel_global_data",
         Address     => Mutools.Utils.To_Hex
           (Number => Config.Kernel_Global_Data_Section_Addr),
         Size        => Mutools.Utils.To_Hex
           (Number => Config.Kernel_Global_Data_Section_Size),
         Caching     => "WB",
         Alignment   => "16#1000#",
         File_Name   => "kernel",
         File_Offset => Mutools.Utils.To_Hex
           (Number => Config.Kernel_Global_Data_Section_Addr
            - Config.Kernel_Text_Section_Addr),
         Memory_Type => "kernel_binary");
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => "kernel_ro",
         Address     => Mutools.Utils.To_Hex
           (Number => Config.Kernel_RO_Section_Addr),
         Size        => Mutools.Utils.To_Hex
           (Number => Config.Kernel_RO_Section_Size),
         Caching     => "WB",
         Alignment   => "16#1000#",
         File_Name   => "kernel",
         File_Offset => Mutools.Utils.To_Hex
           (Number => Config.Kernel_RO_Section_Addr
            - Config.Kernel_Text_Section_Addr),
         Memory_Type => "kernel_binary");
   end Add_Kernel_Shared_Memory;

   -------------------------------------------------------------------------

   procedure Add_Kernel_Stack (Data : in out Muxml.XML_Data_Type)
   is
      CPU_Count       : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => Data);
      Stack_Size      : constant String
        := Mutools.Utils.To_Hex (Number => Config.Kernel_Stack_Size);
      Intr_Stack_Size : constant String
        := Mutools.Utils.To_Hex (Number => Config.Kernel_Interrupt_Stack_Size);
   begin
      Mulog.Log (Msg => "Adding kernel stack memory regions for"
                 & CPU_Count'Img & " CPU(s)");

      for I in 0 .. CPU_Count - 1 loop
         declare
            CPU_Str : constant String := Ada.Strings.Fixed.Trim
              (Source => I'Img,
               Side   => Ada.Strings.Left);
         begin
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => "kernel_stack_" & CPU_Str,
               Address     => "",
               Size        => Stack_Size,
               Caching     => "WB",
               Alignment   => "16#1000#",
               Memory_Type => "kernel");
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => "kernel_interrupt_stack_" & CPU_Str,
               Address     => "",
               Size        => Intr_Stack_Size,
               Caching     => "WB",
               Alignment   => "16#1000#",
               Memory_Type => "kernel");
         end;
      end loop;
   end Add_Kernel_Stack;

   -------------------------------------------------------------------------

   procedure Add_Missing_Attributes (Data : in out Muxml.XML_Data_Type)
   is
      Align : constant String := "16#1000#";
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/memory/memory[not(@alignment) or not(@type)]");
   begin
      Mulog.Log (Msg => "Adding alignment to"
                 & DOM.Core.Nodes.Length (List => Nodes)'Img
                 & " memory region(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Mem : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
         begin
            if DOM.Core.Elements.Get_Attribute
              (Elem => Mem,
               Name => "alignment") = ""
            then
               DOM.Core.Elements.Set_Attribute
                 (Elem  => Mem,
                  Name  => "alignment",
                  Value => Align);
            end if;

            if DOM.Core.Elements.Get_Attribute
              (Elem => Mem,
               Name => "type") = ""
            then
               DOM.Core.Elements.Set_Attribute
                 (Elem  => Mem,
                  Name  => "type",
                  Value => "subject");
            end if;
         end;
      end loop;
   end Add_Missing_Attributes;

   -------------------------------------------------------------------------

   procedure Add_Reserved_Memory_Regions (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/hardware/memory/reservedMemory");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Region_Node  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Region_Name  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Region_Node,
                 Name => "name");
            Phys_Address : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Region_Node,
                 Name => "physicalAddress");
            Size_Str     : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Region_Node,
                 Name => "size");
         begin
            Mulog.Log (Msg => "Adding reserved memory region with size "
                       & Size_Str & " @ physical address " & Phys_Address);
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => Region_Name,
               Address     => Phys_Address,
               Size        => Size_Str,
               Caching     => "WB",
               Alignment   => "16#1000#",
               Memory_Type => "device_rmrr");
         end;
      end loop;
   end Add_Reserved_Memory_Regions;

   -------------------------------------------------------------------------

   procedure Add_Scheduling_Group_Info_Regions
     (Data : in out Muxml.XML_Data_Type)
   is
      package MXU renames Mutools.XML_Utils;

      Sched_Group_Count : Natural;
   begin
      begin
         Sched_Group_Count := MXU.Get_Initial_Scheduling_Group_Subjects
           (Data => Data)'Length;

      exception
         when E : others =>
            raise Expansion_Error with "Error adding scheduling group info "
              & "regions - " & Ada.Exceptions.Exception_Message (X => E);
      end;

      Mulog.Log (Msg => "Adding" & Sched_Group_Count'Img
                 & " scheduling group info region(s)");
      for I in 1 .. Sched_Group_Count loop
         Mutools.XML_Utils.Add_Memory_Region
           (Policy      => Data,
            Name        => "scheduling_group_info_"
            & Ada.Strings.Fixed.Trim (Source => I'Img,
                                      Side   => Ada.Strings.Left),
            Address     => "",
            Size        => "16#1000#",
            Caching     => "WB",
            Alignment   => "16#1000#",
            Memory_Type => "subject_scheduling_info");
      end loop;
   end Add_Scheduling_Group_Info_Regions;

   -------------------------------------------------------------------------

   procedure Add_Subject_Bitmaps (Data : in out Muxml.XML_Data_Type)
   is
      IOBM_Size  : constant := Mutools.Constants.Page_Size * 2;
      MSRBM_Size : constant := Mutools.Constants.Page_Size;
      Nodes      : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
         begin
            Mulog.Log (Msg => "Adding I/O and MSR bitmap memory regions for"
                       & " subject '" & Subj_Name & "'");
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => Subj_Name & "|iobm",
               Address     => "",
               Size        => Mutools.Utils.To_Hex (Number => IOBM_Size),
               Caching     => "WB",
               Alignment   => "16#1000#",
               Memory_Type => "system_iobm",
               File_Name   => Subj_Name & "_iobm",
               File_Offset => "none");
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => Subj_Name & "|msrbm",
               Address     => "",
               Size        => Mutools.Utils.To_Hex (Number => MSRBM_Size),
               Caching     => "WB",
               Alignment   => "16#1000#",
               Memory_Type => "system_msrbm",
               File_Name   => Subj_Name & "_msrbm",
               File_Offset => "none");
         end;
      end loop;
   end Add_Subject_Bitmaps;

   -------------------------------------------------------------------------

   procedure Add_Subject_FPU_State_Regions (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Add_Subject_Memory_Region
        (Data         => Data,
         Region_Type  => "fpu",
         Region_Class => "kernel");
   end Add_Subject_FPU_State_Regions;

   -------------------------------------------------------------------------

   procedure Add_Subject_Interrupts_Pages (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Add_Subject_Memory_Region
        (Data        => Data,
         Region_Type => "interrupts");
   end Add_Subject_Interrupts_Pages;

   -------------------------------------------------------------------------

   procedure Add_Subject_Memory_Region
     (Data         : in out Muxml.XML_Data_Type;
      Region_Type  :        String;
      Region_Class :        String := "subject";
      Address      :        String := "";
      Size         :        String := "16#1000#";
      Alignment    :        String := "16#1000#";
      Caching      :        String := "WB")
   is
      Subjects      : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
      Subject_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Subjects);
   begin
      Mulog.Log (Msg => "Adding " & Region_Type & " memory regions for"
                 & Subject_Count'Img & " subject(s)");

      for I in 0 .. Subject_Count - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Subjects,
                                      Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
         begin
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => Subj_Name & "|" & Region_Type,
               Address     => Address,
               Size        => Size,
               Caching     => Caching,
               Alignment   => Alignment,
               Memory_Type => Region_Class & "_" & Region_Type);
         end;
      end loop;
   end Add_Subject_Memory_Region;

   -------------------------------------------------------------------------

   procedure Add_Subject_MSR_Store (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[vcpu/registers/msrs/msr]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            package MXU renames Mutools.XML_Utils;

            use type Interfaces.Unsigned_64;

            Subj_Node  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Subj_Name  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Registers  : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subj_Node,
                 XPath => "vcpu/registers/msrs/msr[@mode='rw' or @mode='w']");
            Ctrls_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "vcpu/vmx/controls");
            MSR_Count  : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64
                (MXU.Calculate_MSR_Count
                   (MSRs                   => Registers,
                    DEBUGCTL_Control       => MXU.Has_Managed_DEBUGCTL
                      (Controls => Ctrls_Node),
                    PAT_Control            => MXU.Has_Managed_PAT
                      (Controls => Ctrls_Node),
                    PERFGLOBALCTRL_Control => MXU.Has_Managed_PERFGLOBALCTRL
                      (Controls => Ctrls_Node),
                    EFER_Control           => MXU.Has_Managed_EFER
                      (Controls => Ctrls_Node)));

            --  MSR store size rounded up to next multiple of page size.

            Store_Size : constant Interfaces.Unsigned_64
              := Mutools.Constants.Page_Size *
                (((MSR_Count * Mutools.Constants.MSR_Store_Entry_Size) +
                 (Mutools.Constants.Page_Size - 1)) /
                   Mutools.Constants.Page_Size);
            Size_Str   : constant String
              := Mutools.Utils.To_Hex (Number => Store_Size);
         begin
            if MSR_Count > 0 then
               Mulog.Log (Msg => "Adding MSR store region with size "
                          & Size_Str & " for subject '" & Subj_Name & "'");
               Mutools.XML_Utils.Add_Memory_Region
                 (Policy      => Data,
                  Name        => Subj_Name & "|msrstore",
                  Address     => "",
                  Size        => Size_Str,
                  Caching     => "WB",
                  Alignment   => "16#1000#",
                  Memory_Type => "kernel_msrstore",
                  File_Name   =>  Subj_Name & "_msrstore",
                  File_Offset => "none");
            end if;
         end;
      end loop;
   end Add_Subject_MSR_Store;

   -------------------------------------------------------------------------

   procedure Add_Subject_PTs (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin

      --  Validate that there are no overlapping subject memory mappings and
      --  raise a sensible error message if necessary. This special check is
      --  required here because expanded memory regions might collide with
      --  existing/user-defined ones.

      Mucfgcheck.Subject.Virtual_Memory_Overlap (XML_Data => Data);

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Size      : constant Interfaces.Unsigned_64
              := XML_Utils.Calculate_PT_Size
                (Policy             => Data,
                 Paging_Levels      => 4,
                 Large_Pages        => False,
                 Dev_Virt_Mem_XPath => "/system/subjects/subject[@name='"
                 & Subj_Name & "']/devices/device/memory",
                 Virt_Mem_XPath     => "/system/subjects/subject[@name='"
                 & Subj_Name & "']/memory/memory");
            Size_Str  : constant String := Mutools.Utils.To_Hex
              (Number => Size);
         begin
            Mulog.Log (Msg => "Adding pagetable region with size " & Size_Str
                       & " for subject '" & Subj_Name & "'");
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => Subj_Name & "|pt",
               Address     => "",
               Size        => Size_Str,
               Caching     => "WB",
               Alignment   => "16#1000#",
               Memory_Type => "system_pt",
               File_Name   =>  Subj_Name & "_pt",
               File_Offset => "none");
         end;
      end loop;
   end Add_Subject_PTs;

   -------------------------------------------------------------------------

   procedure Add_Subject_States (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Add_Subject_Memory_Region
        (Data        => Data,
         Region_Type => "state");
   end Add_Subject_States;

   -------------------------------------------------------------------------

   procedure Add_Subject_Timed_Event_Pages (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Add_Subject_Memory_Region
        (Data        => Data,
         Region_Type => "timed_event");
   end Add_Subject_Timed_Event_Pages;

   -------------------------------------------------------------------------

   procedure Add_Tau0_Interface (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Mulog.Log (Msg => "Adding tau0 interface memory region");

      Mutools.XML_Utils.Add_Memory_Region
        (Policy       => Data,
         Name         => "sys_interface",
         Address      => "",
         Size         => "16#1000#",
         Caching      => "WB",
         Alignment    => "16#1000#",
         Memory_Type  => "kernel_interface",
         Fill_Pattern => "16#00#");
   end Add_Tau0_Interface;

   -------------------------------------------------------------------------

   procedure Add_VMCS_Regions (Data : in out Muxml.XML_Data_Type)
   is
      use type Interfaces.Unsigned_64;

      CPU_Count : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64
          (Mutools.XML_Utils.Get_Active_CPU_Count
             (Data => Data));
      Curr_Addr : Interfaces.Unsigned_64 := VMX_Start_Address +
        CPU_Count * Mutools.Constants.Page_Size;
      Nodes     : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
         begin
            Mulog.Log (Msg => "Adding VMCS region for subject '"
                       & Subj_Name & "' at address "
                       & Mutools.Utils.To_Hex (Number => Curr_Addr));
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => Subj_Name & "|vmcs",
               Address     => Mutools.Utils.To_Hex (Number => Curr_Addr),
               Size        => "16#1000#",
               Caching     => "WB",
               Alignment   => "16#1000#",
               Memory_Type => "kernel_vmcs");
            Curr_Addr := Curr_Addr + Mutools.Constants.Page_Size;
         end;
      end loop;
   end Add_VMCS_Regions;

   -------------------------------------------------------------------------

   procedure Add_VMXON_Regions (Data : in out Muxml.XML_Data_Type)
   is
      Curr_Addr : Interfaces.Unsigned_64 := VMX_Start_Address;
      CPU_Count : constant Positive
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => Data);
   begin
      for I in 0 .. CPU_Count - 1 loop
         declare
            use type Interfaces.Unsigned_64;

            CPU_Str : constant String := Ada.Strings.Fixed.Trim
              (Source => I'Img,
               Side   => Ada.Strings.Left);
         begin
            Mulog.Log (Msg => "Adding VMXON region for CPU " & CPU_Str & " at "
                       & "address " & Mutools.Utils.To_Hex
                         (Number => Curr_Addr));
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => "kernel_" & CPU_Str & "|vmxon",
               Address     => Mutools.Utils.To_Hex (Number => Curr_Addr),
               Size        => "16#1000#",
               Caching     => "WB",
               Alignment   => "16#1000#",
               Memory_Type => "system_vmxon");
            Curr_Addr := Curr_Addr + Mutools.Constants.Page_Size;
         end;
      end loop;
   end Add_VMXON_Regions;

end Expanders.Memory;
