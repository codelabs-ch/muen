--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with Mucfgcheck.Validation_Errors;

package body Memhashes.Pre_Checks.Test_Data is

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

   procedure Inc_Counter (Data : Muxml.XML_Data_Type)
   is
   begin
      Test_Counter := Test_Counter + 1;
   end Inc_Counter;

end Memhashes.Pre_Checks.Test_Data;
