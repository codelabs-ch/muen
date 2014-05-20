--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Paging.Entries.PML4_Entry_Type_Test_Data is

   Local_PML4_Entry_Type : aliased GNATtest_Generated.GNATtest_Standard.Paging.Entries.PML4_Entry_Type;
   procedure Set_Up (Gnattest_T : in out Test_PML4_Entry_Type) is
   begin
     GNATtest_Generated.GNATtest_Standard.Paging.Entries.Table_Entry_Type_Test_Data.Table_Entry_Type_Tests.Test_Table_Entry_Type(Gnattest_T).Set_Up;
      Gnattest_T.Fixture := Local_PML4_Entry_Type'Access;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test_PML4_Entry_Type) is
   begin
     GNATtest_Generated.GNATtest_Standard.Paging.Entries.Table_Entry_Type_Test_Data.Table_Entry_Type_Tests.Test_Table_Entry_Type(Gnattest_T).Tear_Down;
   end Tear_Down;

end Paging.Entries.PML4_Entry_Type_Test_Data;
