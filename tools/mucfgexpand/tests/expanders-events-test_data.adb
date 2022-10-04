--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with Expanders.Scheduling;

package body Expanders.Events.Test_Data is

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

   procedure Prepare_Asap_Events (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Scheduling.Add_CPU_IDs (Data => Data);
      Subjects.Add_CPU_IDs (Data => Data);
      Subjects.Test_Data.Prepare_Channel_Events (Data => Data);
   end Prepare_Asap_Events;

end Expanders.Events.Test_Data;
