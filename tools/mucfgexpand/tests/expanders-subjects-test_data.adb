--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with Mucfgcheck.Validation_Errors;
with Expanders.Scheduling;

package body Expanders.Subjects.Test_Data is

   -------------------------------------------------------------------------

   procedure Set_Up (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      Mucfgcheck.Validation_Errors.Clear;
   end Set_Up;

   -------------------------------------------------------------------------

   procedure Tear_Down (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      Mucfgcheck.Validation_Errors.Clear;
   end Tear_Down;

   -------------------------------------------------------------------------

   procedure Inject_Idle_Subject (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/scheduling/majorFrame/cpu/minorFrame[1]",
         Name  => "subject",
         Value => "mugenschedcfg_auto_idle_0");
   end Inject_Idle_Subject;

   -------------------------------------------------------------------------

   procedure Prepare_Channel_Events (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Add_Missing_Elements (Data => Data);
      Components.Add_Library_Resources (Data => Data);
      Components.Add_Channel_Arrays (Data => Data);
      Components.Add_Channels (Data => Data);
      Add_Channel_Events (Data => Data);
   end Prepare_Channel_Events;

   -------------------------------------------------------------------------

   procedure Prepare_Loader_Expansion (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Prepare_Profile (Data => Data);
      Add_Sinfo_Regions (Data => Data);
      Handle_Profile (Data => Data);
   end Prepare_Loader_Expansion;

   -------------------------------------------------------------------------

   procedure Prepare_Local_IDs (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Scheduling.Add_CPU_IDs (Data => Data);
      Add_CPU_IDs (Data => Data);
   end Prepare_Local_IDs;

   -------------------------------------------------------------------------

   procedure Prepare_Profile (Data: in out Muxml.XML_Data_Type)
   is
   begin
      Add_Missing_Elements (Data => Data);
      Components.Add_Subject_Profile_VCPU (Data => Data);
      Siblings.Add_Subject_Profile_VCPU (Data => Data);
   end Prepare_Profile;

   -------------------------------------------------------------------------

   procedure Prepare_Sched_Info_Mappings (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Scheduling.Add_CPU_IDs (Data => Data);
      Add_Missing_Elements (Data => Data);
      Add_Tau0 (Data => Data);
      Add_Global_IDs (Data => Data);
      Scheduling.Add_Group_IDs (Data => Data);
   end Prepare_Sched_Info_Mappings;

   -------------------------------------------------------------------------

   procedure Prepare_Sibling_Mappings (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Prepare_Sched_Info_Mappings (Data => Data);
      Add_CPU_IDs (Data => Data);
      Add_Sched_Group_Info_Mappings (Data => Data);
      Add_Timed_Event_Mappings (Data => Data);
   end Prepare_Sibling_Mappings;

   -------------------------------------------------------------------------

   procedure Prepare_Unmask_Events (Data: in out Muxml.XML_Data_Type)
   is
   begin
      Hardware.Add_PCI_Device_MSI_IRQs (Data => Data);
      Prepare_Profile (Data => Data);
      Add_Device_Vectors (Data => Data);
   end Prepare_Unmask_Events;

   -------------------------------------------------------------------------

   procedure Remove_Subj_Device_Resources (Data : in out Muxml.XML_Data_Type)
   is
      Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/subjects/subject[@name='lnx']/devices/"
           & "device[@logical='xhci']");
      Resources : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Node,
           XPath => "irq|memory|ioPort");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Resources) - 1 loop
         declare
            Res : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Resources,
               Index => I);
         begin
            Muxml.Utils.Remove_Child
              (Node       => Node,
               Child_Name => DOM.Core.Nodes.Node_Name (N => Res));
         end;
      end loop;
   end Remove_Subj_Device_Resources;

end Expanders.Subjects.Test_Data;
