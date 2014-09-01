--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

with Ada.Directories;

with Test_Utils;

generic
   type GNATtest_Test_Type is new AUnit.Test_Fixtures.Test_Fixture
     with private;
package VTd.Tables.IR.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with record
      IRT : IR_Table_Type;
   end record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   procedure User_Set_Up (Gnattest_T : in out Test)is null;
   procedure User_Tear_Down (Gnattest_T : in out Test)is null;

end VTd.Tables.IR.Test_Data;
