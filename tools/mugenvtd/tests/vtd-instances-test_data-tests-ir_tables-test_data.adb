--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body VTd.Instances.Test_Data.Tests.IR_Tables.Test_Data is

   procedure Set_Up (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Tear_Down;

   procedure User_Set_Up (Gnattest_T : in out New_Test) is
   begin
      Instances.IR_Tables.Add_Entry
        (IRT    => Gnattest_T.IRT,
         Index  => Test_Range'Last,
         Vector => 12,
         DST    => 122344,
         SID    => 404,
         TM     => 1);
   end User_Set_Up;

   procedure User_Tear_Down (Gnattest_T : in out New_Test) is
   begin
      Instances.IR_Tables.Add_Entry
        (IRT    => Gnattest_T.IRT,
         Index  => Test_Range'Last,
         Vector => 0,
         DST    => 0,
         SID    => 0,
         TM     => 0);
   end User_Tear_Down;

end VTd.Instances.Test_Data.Tests.IR_Tables.Test_Data;
