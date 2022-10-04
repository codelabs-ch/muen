--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with Ada.Strings.Unbounded;

with DOM.Core.Documents;
with DOM.Core.Elements;

with Muxml.Utils;

with Expanders.Scheduling;

package body Expanders.Kernel.Test_Data is

   -------------------------------------------------------------------------

   procedure Set_Up (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Set_Up;

   -------------------------------------------------------------------------

   procedure Tear_Down (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Tear_Down;

   -------------------------------------------------------------------------

   procedure Pre_Sched_Group_Info_Mappings (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Scheduling.Add_CPU_IDs (Data => Data);
      Subjects.Add_Tau0 (Data => Data);
      Pre_Subj_Mappings (Data => Data);
   end Pre_Sched_Group_Info_Mappings;

   -------------------------------------------------------------------------

   procedure Pre_Subj_Mappings (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Scheduling.Add_CPU_IDs (Data => Data);
      Add_Section_Skeleton (Data => Data);
      Subjects.Add_Global_IDs (Data => Data);
      Subjects.Add_CPU_IDs (Data => Data);
   end Pre_Subj_Mappings;

   -------------------------------------------------------------------------

   procedure Pre_Subj_MSR_Store_Mappings (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Pre_Subj_Mappings (Data => Data);
      Subjects.Add_Missing_Elements (Data => Data);
      Components.Add_Subject_Profile_VCPU (Data => Data);
      Siblings.Add_Subject_Profile_VCPU (Data => Data);
      Subjects.Handle_Profile (Data => Data);
      Memory.Add_Subject_MSR_Store (Data => Data);
   end Pre_Subj_MSR_Store_Mappings;

   -------------------------------------------------------------------------

   procedure Pre_Map_Tau0_Interface (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Add_Section_Skeleton (Data => Data);
      Hardware.Add_Processor_CPU_IDs (Data => Data);
   end Pre_Map_Tau0_Interface;

   -------------------------------------------------------------------------

   procedure Pre_Vga_Diagnostics (Data : in out Muxml.XML_Data_Type)
   is
      Diag_Dev : constant DOM.Core.Node := Muxml.Utils.Get_Element
        (Doc   => Data.Doc,
         XPath => "/system/platform/kernelDiagnostics/device");
      Mem_Node : constant DOM.Core.Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "memory");
   begin
      DOM.Core.Elements.Set_Attribute
        (Elem  => Diag_Dev,
         Name  => "physical",
         Value => "vga");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem_Node,
         Name  => "physical",
         Value => "buffer");
      Muxml.Utils.Insert_Before
        (Parent    => Diag_Dev,
         New_Child => Mem_Node,
         Ref_Names =>
           (1 => Ada.Strings.Unbounded.To_Unbounded_String ("ioPort")));
      Muxml.Utils.Set_Attribute
        (Doc   => Diag_Dev,
         XPath => "ioPort",
         Name  => "physical",
         Value => "ports");

      Add_Section_Skeleton (Data => Data);
   end Pre_Vga_Diagnostics;

end Expanders.Kernel.Test_Data;
