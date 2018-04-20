--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Expanders.Siblings.Test_Data is

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

   procedure Prepare_Profile (Data: in out Muxml.XML_Data_Type)
   is
   begin
      Subjects.Add_Missing_Elements (Data => Data);
      Components.Add_Subject_Profile_VCPU (Data => Data);
   end Prepare_Profile;

end Expanders.Siblings.Test_Data;
