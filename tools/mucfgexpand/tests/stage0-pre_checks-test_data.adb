--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Stage0.Pre_Checks.Test_Data is

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
      Clear;
      Test_Counter := 0;
   end Tear_Down;

   -------------------------------------------------------------------------

   procedure Inc_Counter (XML_Data : Muxml.XML_Data_Type)
   is
   begin
      Test_Counter := Test_Counter + 1;
   end Inc_Counter;

end Stage0.Pre_Checks.Test_Data;
