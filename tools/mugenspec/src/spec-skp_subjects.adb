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

with Ada.Strings.Unbounded;

with Interfaces;

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.XML_Utils;
with Mutools.Templates;

with Spec.VMX_Types;

with String_Templates;

package body Spec.Skp_Subjects
is

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      use Ada.Strings.Unbounded;
      use Interfaces;

      Subjects      : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject");
      Phys_Memory   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
      Subj_Count    : constant Natural
        := DOM.Core.Nodes.Length (List => Subjects);
      Events        : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/events/event");
      Event_Targets : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject/events/target/event");

      Buffer : Unbounded_String;
      Tmpl   : Mutools.Templates.Template_Type;

      --  Add event entry to template buffer.
      procedure Add_Event (Event : DOM.Core.Node);

      --  Add trap entry to template buffer.
      procedure Add_Trap (Trap : DOM.Core.Node);

      --  Append SPARK specification of given subject to template buffer.
      procedure Write_Subject_Spec (Subject : DOM.Core.Node);

      -------------------------------------------------------------------

      procedure Add_Event (Event : DOM.Core.Node)
      is
         Event_Id : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Event,
              Name => "id");
         Phys_Event_Ref : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Event,
              Name => "physical");
         Event_Target : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Nodes     => Event_Targets,
              Ref_Attr  => "physical",
              Ref_Value => Phys_Event_Ref);
         Dst_Id : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Muxml.Utils.Ancestor_Node
                (Node  => Event_Target,
                 Level => 3),
              Name => "id");
         Dst_Vector : constant String
           := Muxml.Utils.Get_Attribute
             (Doc   => Event_Target,
              XPath => "inject_interrupt",
              Name  => "vector");
         Notify_Mode : constant String
           := Muxml.Utils.Get_Attribute
             (Nodes     => Events,
              Ref_Attr  => "name",
              Ref_Value => Phys_Event_Ref,
              Attr_Name => "mode");
      begin
         Buffer := Buffer & Indent (N => 3)  & " "
           & Event_Id & " => Event_Entry_Type'("
           & ASCII.LF
           & Indent (N => 4) & "Dst_Subject => " & Dst_Id & ","
           & ASCII.LF
           & Indent (N => 4) & "Dst_Vector  => ";

         if Dst_Vector = "" then
            Buffer := Buffer & "Skp.Invalid_Vector,";
         else
            Buffer := Buffer & Dst_Vector & ",";
         end if;

         Buffer := Buffer & ASCII.LF & Indent (N => 4) & "Handover    => ";
         if Notify_Mode = "switch" then
            Buffer := Buffer & "True,";
         else
            Buffer := Buffer & "False,";
         end if;

         Buffer := Buffer & ASCII.LF & Indent (N => 4) & "Send_IPI    => ";
         if Notify_Mode = "ipi" then
            Buffer := Buffer & "True)";
         else
            Buffer := Buffer & "False)";
         end if;
      end Add_Event;

      -------------------------------------------------------------------

      procedure Add_Trap (Trap : DOM.Core.Node)
      is
         Trap_Id : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Trap,
              Name => "id");
         Phys_Event_Ref : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Trap,
              Name => "physical");
         Event_Target : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Nodes     => Event_Targets,
              Ref_Attr  => "physical",
              Ref_Value => Phys_Event_Ref);

         Dst_Id : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Muxml.Utils.Ancestor_Node
                  (Node  => Event_Target,
                   Level => 3),
              Name => "id");
         Dst_Vector : constant String
           := Muxml.Utils.Get_Attribute
             (Doc   => Event_Target,
              XPath => "inject_interrupt",
              Name  => "vector");
      begin
         Buffer := Buffer & Indent (N => 3) & " "
           & Trap_Id & " => Trap_Entry_Type'(Dst_Subject => " & Dst_Id
           & ", Dst_Vector => ";

         if Dst_Vector = "" then
            Buffer := Buffer & "Skp.Invalid_Vector)";
         else
            Buffer := Buffer & Dst_Vector & ")";
         end if;
      end Add_Trap;

      ----------------------------------------------------------------------

      procedure Write_Subject_Spec (Subject : DOM.Core.Node)
      is
         use type DOM.Core.Node;

         package VMX renames VMX_Types;

         function U
           (Source : String)
            return Unbounded_String
            renames To_Unbounded_String;

         --  EPT memory type WB, page-walk length 4
         EPT_Flags : constant := 16#1e#;

         Name    : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Subject,
            Name => "name");
         Subj_Id : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Subject,
            Name => "id");
         CPU_Id  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Subject,
            Name => "cpu");

         PML4_Addr  : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Nodes     => Phys_Memory,
               Refs      => ((Name  => U ("type"),
                              Value => U ("system_pt")),
                             (Name  => U ("name"),
                              Value => U (Name & "|pt"))),
               Attr_Name => "physicalAddress"));
         Entry_Addr : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Element_Value
              (Doc   => Subject,
               XPath => "vcpu/registers/gpr/rip"));
         Stack_Addr : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Element_Value
              (Doc   => Subject,
               XPath => "vcpu/registers/gpr/rsp"));
         VMCS_Addr  : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Nodes     => Phys_Memory,
               Refs      => ((Name  => U ("type"),
                              Value => U ("system_vmcs")),
                             (Name  => U ("name"),
                              Value => U (Name & "|vmcs"))),
               Attr_Name => "physicalAddress"));
         IOBM_Addr  : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Nodes     => Phys_Memory,
               Refs      => ((Name  => U ("type"),
                              Value => U ("system_iobm")),
                             (Name  => U ("name"),
                              Value => U (Name & "|iobm"))),
               Attr_Name => "physicalAddress"));
         MSRBM_Addr : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Nodes     => Phys_Memory,
               Refs      => ((Name  => U ("type"),
                              Value => U ("system_msrbm")),
                             (Name  => U ("name"),
                              Value => U (Name & "|msrbm"))),
               Attr_Name => "physicalAddress"));

         MSR_Store_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Nodes => Phys_Memory,
              Refs  => ((Name  => U ("type"),
                         Value => U ("system_msrstore")),
                        (Name  => U ("name"),
                         Value => U (Name & "|msrstore"))));
         MSR_Store_Addr : Unsigned_64 := 0;
         MSR_Count      : Natural     := 0;

         CS_Access : constant String := Muxml.Utils.Get_Attribute
           (Doc   => Subject,
            XPath => "vcpu/segments/cs",
            Name  => "access");

         Pin_Ctrls   : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/controls/pin/*");
         Proc_Ctrls  : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/controls/proc/*");
         Proc2_Ctrls : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/controls/proc2/*");
         Entry_Ctrls : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/controls/entry/*");
         Exit_Ctrls  : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/controls/exit/*");
         CR0_Value   : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/registers/cr0/*");
         CR0_Mask    : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/masks/cr0/*");
         CR4_Value   : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/registers/cr4/*");
         CR4_Mask    : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/masks/cr4/*");
         Exceptions  : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/masks/exception/*");
         Traps       : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "events/source/group[@name='vmx_exit']/*");
         Trap_Count  : constant Natural := DOM.Core.Nodes.Length
           (List => Traps);
         Events      : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "events/source/group[@name='vmcall']/*");
         Event_Count : constant Natural := DOM.Core.Nodes.Length
           (List => Events);
      begin
         if MSR_Store_Node /= null then
            MSR_Store_Addr := Unsigned_64'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => MSR_Store_Node,
                  Name => "physicalAddress"));
            declare
               Ctrls_Node : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Doc   => Subject,
                    XPath => "vcpu/vmx/controls");
               Debug_Ctrl : constant Boolean
                 := Mutools.XML_Utils.Has_Managed_DEBUGCTL
                   (Controls => Ctrls_Node);
               PERF_Ctrl  : constant Boolean
                 := Mutools.XML_Utils.Has_Managed_PERFGLOBALCTRL
                   (Controls => Ctrls_Node);
               PAT_Ctrl   : constant Boolean
                 := Mutools.XML_Utils.Has_Managed_PAT (Controls => Ctrls_Node);
               EFER_Ctrl  : constant Boolean
                 := Mutools.XML_Utils.Has_Managed_EFER
                   (Controls => Ctrls_Node);
            begin
               MSR_Count := Mutools.XML_Utils.Calculate_MSR_Count
                 (MSRs                   => McKae.XML.XPath.XIA.XPath_Query
                    (N     => Subject,
                     XPath => "vcpu/registers/msrs/msr"
                     & "[@mode='w' or @mode='rw']"),
                  DEBUGCTL_Control       => Debug_Ctrl,
                  PAT_Control            => PAT_Ctrl,
                  PERFGLOBALCTRL_Control => PERF_Ctrl,
                  EFER_Control           => EFER_Ctrl);
            end;
         end if;

         Buffer := Buffer & Indent (N => 2) & Subj_Id
           & " => Subject_Spec_Type'("
           & ASCII.LF
           & Indent & "    CPU_Id             => " & CPU_Id & ","
           & ASCII.LF;

         if Muxml.Utils.Get_Element_Value
           (Doc   => Subject,
            XPath => "vcpu/vmx/controls/proc2/EnableEPT") = "1"
         then
            Buffer := Buffer
              & Indent & "    PML4_Address       => 0,"
              & ASCII.LF
              & Indent & "    EPT_Pointer        => "
              & Mutools.Utils.To_Hex (Number => PML4_Addr + EPT_Flags)
              & ",";
         else
            Buffer := Buffer
              & Indent & "    PML4_Address       => "
              & Mutools.Utils.To_Hex (Number => PML4_Addr) & ","
              & ASCII.LF
              & Indent & "    EPT_Pointer        => 0,";
         end if;

         Buffer := Buffer & ASCII.LF
           & Indent & "    VMCS_Address       => "
           & Mutools.Utils.To_Hex (Number => VMCS_Addr) & ","
           & ASCII.LF
           & Indent & "    IO_Bitmap_Address  => "
           & Mutools.Utils.To_Hex (Number => IOBM_Addr) & ","
           & ASCII.LF
           & Indent & "    MSR_Bitmap_Address => "
           & Mutools.Utils.To_Hex (Number => MSRBM_Addr) & ","
           & ASCII.LF
           & Indent & "    MSR_Store_Address  => "
           & Mutools.Utils.To_Hex (Number => MSR_Store_Addr) & ","
           & ASCII.LF
           & Indent & "    Stack_Address      => "
           & Mutools.Utils.To_Hex (Number => Stack_Addr) & ","
           & ASCII.LF
           & Indent & "    Entry_Point        => "
           & Mutools.Utils.To_Hex (Number => Entry_Addr) & ","
           & ASCII.LF
           & Indent & "    CR0_Value          => "
           & Mutools.Utils.To_Hex (Number => VMX.Get_CR0 (Fields => CR0_Value))
           & "," & ASCII.LF
           & Indent & "    CR0_Mask           => "
           & Mutools.Utils.To_Hex (Number => VMX.Get_CR0 (Fields => CR0_Mask))
           & "," & ASCII.LF
           & Indent & "    CR4_Value          => "
           & Mutools.Utils.To_Hex (Number => VMX.Get_CR4 (Fields => CR4_Value))
           & "," & ASCII.LF
           & Indent & "    CR4_Mask           => "
           & Mutools.Utils.To_Hex (Number => VMX.Get_CR4 (Fields => CR4_Mask))
           & "," & ASCII.LF
           & Indent & "    CS_Access          => " & CS_Access & ","
           & ASCII.LF
           & Indent & "    Exception_Bitmap   => "
           & Mutools.Utils.To_Hex
           (Number => VMX.Get_Exceptions (Fields  => Exceptions,
                                          Default => 16#ffff_ffff#)) & ","
           & ASCII.LF
           & Indent & "    MSR_Count          =>" & MSR_Count'Img & ","
           & ASCII.LF
           & Indent & "    VMX_Controls       => VMX_Controls_Type'("
           & ASCII.LF
           & Indent (N => 3) & " Exec_Pin    =>"
           & VMX.Get_Pin_Controls (Fields => Pin_Ctrls)'Img  & ","
           & ASCII.LF
           & Indent (N => 3) & " Exec_Proc   =>"
           & VMX.Get_Proc_Controls (Fields => Proc_Ctrls)'Img  & ","
           & ASCII.LF
           & Indent (N => 3) & " Exec_Proc2  =>"
           & VMX.Get_Proc2_Controls (Fields => Proc2_Ctrls)'Img  & ","
           & ASCII.LF
           & Indent (N => 3) & " Exit_Ctrls  =>"
           & VMX.Get_Exit_Controls (Fields => Exit_Ctrls)'Img  & ","
           & ASCII.LF
           & Indent (N => 3) & " Entry_Ctrls =>"
           & VMX.Get_Entry_Controls (Fields => Entry_Ctrls)'Img  & "),"
           & ASCII.LF
           & Indent & "    Trap_Table         => ";

         if Trap_Count = 0 then
            Buffer := Buffer & "Null_Trap_Table,";
         else
            Buffer := Buffer & "Trap_Table_Type'(" & ASCII.LF;
            for I in 0 .. Trap_Count - 1 loop
               Add_Trap (Trap   => DOM.Core.Nodes.Item
                         (List  => Traps,
                          Index => I));

               if I < Trap_Count - 1 then
                  Buffer := Buffer & "," & ASCII.LF;
               end if;
            end loop;

            Buffer := Buffer & "," & ASCII.LF & Indent (N => 3)
              & " others => Null_Trap),";
         end if;

         Buffer := Buffer & ASCII.LF
           & Indent & "    Event_Table        => ";

         if Event_Count = 0 then
            Buffer := Buffer & "Null_Event_Table)";
         else
            Buffer := Buffer & "Event_Table_Type'(" & ASCII.LF;
            for I in 0 .. Event_Count - 1 loop
               Add_Event (Event => DOM.Core.Nodes.Item
                          (List  => Events,
                           Index => I));

               if I < Event_Count - 1 then
                  Buffer := Buffer & "," & ASCII.LF;
               end if;
            end loop;

            if Event_Count /= 32 then
               Buffer := Buffer & "," & ASCII.LF & Indent (N => 3)
                 & " others => Null_Event";
            end if;
            Buffer := Buffer & "))";
         end if;
      end Write_Subject_Spec;
   begin
      Mulog.Log (Msg => "Writing subject spec to '"
                 & Output_Dir & "/skp-subjects.adb'");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_subjects_ads);
      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-subjects.ads");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_subjects_adb);

      for I in 0 .. Subj_Count - 1 loop
         Write_Subject_Spec
           (Subject => DOM.Core.Nodes.Item
              (List  => Subjects,
               Index => I));

         if I < Subj_Count - 1 then
            Buffer := Buffer & "," & ASCII.LF;
         end if;
      end loop;

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__subjects__",
         Content  => To_String (Buffer));

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-subjects.adb");
   end Write;

end Spec.Skp_Subjects;
