--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with Ada.Exceptions;

with McKae.XML.XPath.XIA;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with Muxml.Utils;
with Mucfgcheck;

with Expanders.Hardware.Test_Data;
with Expanders.Platform;
with Expanders.Components;
with Expanders.Siblings;
with Test_Utils.Expander;

package Expanders.Subjects.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   --  Remove resources of logical device 'xhci'.
   procedure Remove_Subj_Device_Resources (Data : in out Muxml.XML_Data_Type);

   --  Prepare subjects for loader expansion step.
   procedure Prepare_Loader_Expansion (Data : in out Muxml.XML_Data_Type);

   --  Prepare subject for profile expansion step.
   procedure Prepare_Profile (Data: in out Muxml.XML_Data_Type);

   --  Prepare subjects for scheduling group info mappings expansion step.
   procedure Prepare_Sched_Info_Mappings (Data : in out Muxml.XML_Data_Type);

   --  Prepare devices and subjects for unmask IRQ event expansion step.
   procedure Prepare_Unmask_Events (Data: in out Muxml.XML_Data_Type);

   --  Inject Mugenschedcfg idle subject.
   procedure Inject_Idle_Subject (Data : in out Muxml.XML_Data_Type);

   --  Prepare subject channels for event expansion.
   procedure Prepare_Channel_Events (Data : in out Muxml.XML_Data_Type);

   --  Prepare subjects for sibling memory expansion.
   procedure Prepare_Sibling_Mappings (Data : in out Muxml.XML_Data_Type);

   --  Prepare subjects for local ID expansion step.
   procedure Prepare_Local_IDs (Data : in out Muxml.XML_Data_Type);

end Expanders.Subjects.Test_Data;
