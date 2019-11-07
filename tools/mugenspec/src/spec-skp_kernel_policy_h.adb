--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with Ada.Strings.Unbounded;

with Interfaces;

with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.XML_Utils;
with Mutools.Templates;

with Spec.Utils;

with String_Templates;

package body Spec.Skp_Kernel_Policy_H
is

   use Ada.Strings.Unbounded;
   use Interfaces;

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   --  Returns the kernel PML4 addresses as string for inclusion in policy.h.
   function Get_Kernel_PML4_Addrs
     (Physical_Memory : DOM.Core.Node_List;
      CPU_Count       : Positive)
      return String;

   --  Calculate base address of MSR store mappings.
   function Calculate_MSR_Store_Base_Address
     (Policy : Muxml.XML_Data_Type)
      return Unsigned_64;

   -------------------------------------------------------------------------

   function Calculate_MSR_Store_Base_Address
     (Policy : Muxml.XML_Data_Type)
      return Unsigned_64
   is
      use type DOM.Core.Node;

      Phys_Region : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Policy.Doc,
           XPath => "/system/memory/memory[@type='kernel_msrstore']");
   begin
      if Phys_Region = null then

         --  No MSR store region present.

         return 0;
      end if;

      declare
         Phys_Region_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Phys_Region,
              Name => "name");
         Phys_Region_Size : constant Unsigned_64 := Unsigned_64'Value
           (DOM.Core.Elements.Get_Attribute
             (Elem => Phys_Region,
              Name => "size"));
         Kernel_Mapping : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/kernel/memory/cpu/memory"
              & "[@physical='" & Phys_Region_Name & "']");
         Kernel_Virt_Addr : constant Unsigned_64 := Unsigned_64'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Kernel_Mapping,
               Name => "virtualAddress"));
         Subject_Name : constant String := Mutools.Utils.Decode_Entity_Name
           (Encoded_Str => Phys_Region_Name);
         Subject_ID : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/subjects/subject[@name='"
               & Subject_Name & "']",
               Name  => "globalId"));
      begin

         --  The base address is calculated by subtracting the subject
         --  specific offset from the virtual address of any MSR store kernel
         --  mapping.

         return Kernel_Virt_Addr - (Subject_ID * Phys_Region_Size);
      end;
   end Calculate_MSR_Store_Base_Address;

   -------------------------------------------------------------------------

   function Get_Kernel_PML4_Addrs
     (Physical_Memory : DOM.Core.Node_List;
      CPU_Count       : Positive)
      return String
   is
      Buffer : Unbounded_String;
   begin
      for I in Natural range 0 .. CPU_Count - 1 loop
         declare
            CPU_ID    : constant String
              := Ada.Strings.Fixed.Trim
                (Source  => I'Img,
                 Side    => Ada.Strings.Left);
            PML4_Addr : constant Unsigned_64
              := Unsigned_64'Value
                (Muxml.Utils.Get_Attribute
                   (Nodes     => Physical_Memory,
                    Refs      => ((Name  => U ("type"),
                                   Value => U ("system_pt")),
                                  (Name  => U ("name"),
                                   Value => U ("kernel_" & CPU_ID & "|pt"))),
                    Attr_Name => "physicalAddress"));
         begin
            Buffer := Buffer & ASCII.LF & Indent
              (N         => 1,
               Unit_Size => 4);
            Buffer := Buffer  & ".long 0x" & Mutools.Utils.To_Hex
              (Number     => PML4_Addr,
               Normalize  => False);
         end;
      end loop;
      return To_String (Buffer);
   end Get_Kernel_PML4_Addrs;

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Phys_Memory : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
      Stack_Node  : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Policy.Doc,
           XPath => "/system/kernel/memory/cpu[@id='0']/"
           & "memory[@logical='stack']");
      Stack_Ref   : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Stack_Node,
         Name => "physical");
      Stack_Size  : constant Unsigned_64 := Unsigned_64'Value
        (Muxml.Utils.Get_Attribute
           (Nodes     => Phys_Memory,
            Ref_Attr  => "name",
            Ref_Value => Stack_Ref,
            Attr_Name => "size"));
      Stack_Top   : constant Unsigned_64 := Unsigned_64'Value
        (DOM.Core.Elements.Get_Attribute
           (Elem => Stack_Node,
            Name => "virtualAddress")) + Stack_Size;

      Tau0_Iface_Addr : constant Unsigned_64 := Unsigned_64'Value
        (Muxml.Utils.Get_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/kernel/memory/cpu/"
            & "memory[@logical='tau0_interface']",
            Name  => "virtualAddress"));

      --  Write C header for kernel to specified output directory.
      procedure Write_Kernel_Header
        (Output_Dir : String;
         Policy     : Muxml.XML_Data_Type);

      --  Write SPARK specification for kernel to specified output directory.
      procedure Write_Kernel_Spec
        (Output_Dir : String;
         Policy     : Muxml.XML_Data_Type);

      ----------------------------------------------------------------------

      procedure Write_Kernel_Header
        (Output_Dir : String;
         Policy     : Muxml.XML_Data_Type)
      is
         CPU_Count  : constant Positive
           := Mutools.XML_Utils.Get_Active_CPU_Count (Data => Policy);
         VMXON_Addr : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Nodes     => Phys_Memory,
               Refs      => ((Name  => U ("type"),
                              Value => U ("system_vmxon")),
                             (Name  => U ("name"),
                              Value => U ("kernel_0|vmxon"))),
               Attr_Name => "physicalAddress"));

         CPU_IDs    : constant Utils.APIC_To_CPU_ID_Array
           := Utils.Get_APIC_CPU_ID_Map
             (CPU_Nodes => McKae.XML.XPath.XIA.XPath_Query
                (N     => Policy.Doc,
                 XPath => "/system/hardware/processor/cpu[@cpuId <"
                 & CPU_Count'Img & "]"));
         CPU_ID_Str : Unbounded_String;

         Tmpl : Mutools.Templates.Template_Type;
      begin
         Mulog.Log (Msg => "Writing kernel header file to '"
                    & Output_Dir & "/policy.h'");

         Tmpl := Mutools.Templates.Create
           (Content => String_Templates.policy_h);
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__stack_addr__",
            Content  => Mutools.Utils.To_Hex
              (Number    => Stack_Top,
               Normalize => False));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__cpu_count__",
            Content  => Ada.Strings.Fixed.Trim
              (Source => CPU_Count'Img,
               Side   => Ada.Strings.Left));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__vmxon_addr__",
            Content  => Mutools.Utils.To_Hex
              (Number    => VMXON_Addr,
               Normalize => False));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__kernel_pml4_addrs__",
            Content  => Get_Kernel_PML4_Addrs
              (Physical_Memory => Phys_Memory,
               CPU_Count       => CPU_Count));

         for Idx in CPU_IDs'Range loop
            CPU_ID_Str := CPU_ID_Str & Ada.Strings.Fixed.Trim
              (Source => CPU_IDs (Idx)'Img,
               Side   => Ada.Strings.Left);
            if Idx < CPU_IDs'Last then
               CPU_ID_Str := CPU_ID_Str & ",";
            end if;
         end loop;

         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__kernel_cpu_ids__",
            Content  => To_String (CPU_ID_Str));

         Mutools. Templates.Write
           (Template => Tmpl,
            Filename => Output_Dir & "/policy.h");
      end Write_Kernel_Header;

      ----------------------------------------------------------------------

      procedure Write_Kernel_Spec
        (Output_Dir : String;
         Policy     : Muxml.XML_Data_Type)
      is
         Subj_States_Addr : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/kernel/memory/cpu/"
               & "memory[@logical='tau0|state']",
               Name  => "virtualAddress"));
         Subj_Timed_Events_Addr : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/kernel/memory/cpu/"
               & "memory[@logical='tau0|timed_event']",
               Name  => "virtualAddress"));
         Subj_VMCS_Addr : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/kernel/memory/cpu/"
               & "memory[@logical='tau0|vmcs']",
               Name  => "virtualAddress"));
         Subj_Interrupts_Addr : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/kernel/memory/cpu/"
               & "memory[@logical='tau0|interrupts']",
               Name  => "virtualAddress"));
         Subj_FPU_State_Addr : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/kernel/memory/cpu/"
               & "memory[@logical='tau0|fpu']",
               Name  => "virtualAddress"));
         Sched_Grp_Info_Addr : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/kernel/memory/cpu/"
               & "memory[@logical='scheduling_group_info_1']",
               Name  => "virtualAddress"));
         IO_Apic_Addr : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/kernel/devices/device"
               & "[starts-with(@logical,'ioapic')]/memory",
               Name  => "virtualAddress"));
         Subj_MSR_Store_Addr : constant Unsigned_64
           := Calculate_MSR_Store_Base_Address (Policy => Policy);

         Intr_Stack_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/kernel/memory/cpu[@id='0']/"
              & "memory[@logical='interrupt_stack']");
         Intr_Stack_Ref : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Intr_Stack_Node,
            Name => "physical");
         Intr_Stack_Size : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Nodes     => Phys_Memory,
               Ref_Attr  => "name",
               Ref_Value => Intr_Stack_Ref,
               Attr_Name => "size"));
         Intr_Stack_Top : constant Unsigned_64 := Unsigned_64'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Intr_Stack_Node,
               Name => "virtualAddress")) + Intr_Stack_Size;

         Crash_Audit_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/kernel/memory/cpu[@id='0']/"
              & "memory[@logical='crash_audit']");
         Crash_Audit_Ref : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Crash_Audit_Node,
              Name => "physical");
         Crash_Audit_Size : constant String
           := Muxml.Utils.Get_Attribute
             (Nodes     => Phys_Memory,
              Ref_Attr  => "name",
              Ref_Value => Crash_Audit_Ref,
              Attr_Name => "size");
         Crash_Audit_Addr : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Crash_Audit_Node,
              Name => "virtualAddress");

         TSC_Mhz : constant String
           := Muxml.Utils.Get_Attribute
             (Doc   => Policy.Doc,
              XPath => "/system/hardware/processor",
              Name  => "speed");

         Tmpl : Mutools.Templates.Template_Type;
      begin
         Mulog.Log (Msg => "Writing kernel spec to '"
                    & Output_Dir & "/skp-kernel.ads'");

         Tmpl := Mutools.Templates.Create
           (Content => String_Templates.skp_kernel_ads);
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__stack_addr__",
            Content  => Mutools.Utils.To_Hex (Number => Stack_Top));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__tau0_iface_addr__",
            Content  => Mutools.Utils.To_Hex (Number => Tau0_Iface_Addr));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__subj_states_addr__",
            Content  => Mutools.Utils.To_Hex (Number => Subj_States_Addr));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__subj_timed_events_addr__",
            Content  => Mutools.Utils.To_Hex
              (Number => Subj_Timed_Events_Addr));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__subj_interrupts_addr__",
            Content  => Mutools.Utils.To_Hex
              (Number => Subj_Interrupts_Addr));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__subj_msr_store_addr__",
            Content  => Mutools.Utils.To_Hex
              (Number => Subj_MSR_Store_Addr));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__subj_fpu_state_addr__",
            Content  => Mutools.Utils.To_Hex
              (Number => Subj_FPU_State_Addr));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__subj_vmcs_addr__",
            Content  => Mutools.Utils.To_Hex  (Number => Subj_VMCS_Addr));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__sched_group_info_addr__",
            Content  => Mutools.Utils.To_Hex (Number => Sched_Grp_Info_Addr));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__ioapic_addr__",
            Content  => Mutools.Utils.To_Hex (Number => IO_Apic_Addr));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__intr_stack_addr__",
            Content  => Mutools.Utils.To_Hex (Number => Intr_Stack_Top));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__crash_audit_addr__",
            Content  => Crash_Audit_Addr);
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__crash_audit_size__",
            Content  => Crash_Audit_Size);
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__tsc_mhz__",
            Content  => TSC_Mhz);

         Mutools.Templates.Write
           (Template => Tmpl,
            Filename => Output_Dir & "/skp-kernel.ads");
      end Write_Kernel_Spec;
   begin
      Write_Kernel_Spec (Output_Dir => Output_Dir,
                         Policy     => Policy);
      Write_Kernel_Header (Output_Dir => Output_Dir,
                           Policy     => Policy);
   end Write;

end Spec.Skp_Kernel_Policy_H;
