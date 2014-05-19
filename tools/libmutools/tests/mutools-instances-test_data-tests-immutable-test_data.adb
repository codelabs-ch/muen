--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Mutools.Instances.Test_Data.Tests.Immutable.Test_Data is


   Test_Natural : aliased Natural := 12;

   -------------------------------------------------------------------------

   procedure Dummy (Data : Natural)
   is
   begin
      Instances.Counter := Counter + Data;
   end Dummy;

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
      Instances.Counter := 0;
   end Tear_Down;

   -------------------------------------------------------------------------

   procedure User_Set_Up (Gnattest_T : in out New_Test) is
   begin
      Gnattest_T.Param := Test_Natural'Access;
      Instances.Immutable.Register (Process => Dummy'Access);
   end User_Set_Up;

   -------------------------------------------------------------------------

   procedure User_Tear_Down (Gnattest_T : in out New_Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      Instances.Immutable.Clear;
   end User_Tear_Down;

end Mutools.Instances.Test_Data.Tests.Immutable.Test_Data;
