--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body VTd.Tables.IR.Test_Data is

   procedure Set_Up (Gnattest_T : in out Test) is
      X : Test'Class renames Test'Class (Gnattest_T);
   begin
      X.User_Set_Up;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test) is
      X : Test'Class renames Test'Class (Gnattest_T);
   begin
      X.User_Tear_Down;
   end Tear_Down;

end VTd.Tables.IR.Test_Data;
