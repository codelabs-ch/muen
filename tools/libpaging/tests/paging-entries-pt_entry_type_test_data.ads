--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with Paging.Entries.Table_Entry_Type_Test_Data.Table_Entry_Type_Tests;

with GNATtest_Generated;

package Paging.Entries.PT_Entry_Type_Test_Data is

--  begin read only
   type Test_PT_Entry_Type is new
     GNATtest_Generated.GNATtest_Standard.Paging.Entries.Table_Entry_Type_Test_Data.Table_Entry_Type_Tests.Test_Table_Entry_Type
--  end read only
   with null record;

   procedure Set_Up (Gnattest_T : in out Test_PT_Entry_Type);
   procedure Tear_Down (Gnattest_T : in out Test_PT_Entry_Type);

end Paging.Entries.PT_Entry_Type_Test_Data;
