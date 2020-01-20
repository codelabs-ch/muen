--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Expanders.Components.Test_Data is

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

   procedure Prepare_Component_Events (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Subjects.Add_Missing_Elements (Data => Data);
      Add_Library_Resources (Data => Data);
   end Prepare_Component_Events;

   -------------------------------------------------------------------------

   procedure Pre_Component_Events_Missing_Sections
     (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Muxml.Utils.Remove_Elements
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='subject1']/events/source");
      Muxml.Utils.Remove_Elements
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='subject1']/events/target");
      Prepare_Component_Events (Data => Data);
   end Pre_Component_Events_Missing_Sections;

end Expanders.Components.Test_Data;
