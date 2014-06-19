--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paging.Pagetables.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Paging.Pagetables.Test_Data.Tests is


--  begin read only
   procedure Test_Add_Entry (Gnattest_T : in out Test);
   procedure Test_Add_Entry_6ca8ef (Gnattest_T : in out Test) renames Test_Add_Entry;
--  id:2.2/6ca8ef1f4acbbefa/Add_Entry/1/0/
   procedure Test_Add_Entry (Gnattest_T : in out Test) is
   --  paging-pagetables.ads:33:4:Add_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);
      use type Ada.Containers.Count_Type;

      Table : Page_Table_Type;
      Dummy : Entries.Table_Entry_Type;
   begin
      Assert (Condition => Table.Data.Length = 0,
              Message   => "Table not empty");
      Add_Entry (Table => Table,
                 Index => 0,
                 E     => Dummy);
      Assert (Condition => Table.Data.Length = 1,
              Message   => "Entry not added");

      begin
         Add_Entry (Table => Table,
                    Index => 0,
                    E     => Dummy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Duplicate_Entry => null;
      end;
--  begin read only
   end Test_Add_Entry;
--  end read only


--  begin read only
   procedure Test_Count (Gnattest_T : in out Test);
   procedure Test_Count_6a945c (Gnattest_T : in out Test) renames Test_Count;
--  id:2.2/6a945c690d2f3ca4/Count/1/0/
   procedure Test_Count (Gnattest_T : in out Test) is
   --  paging-pagetables.ads:39:4:Count
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Count;
--  end read only


--  begin read only
   procedure Test_Contains (Gnattest_T : in out Test);
   procedure Test_Contains_8c356e (Gnattest_T : in out Test) renames Test_Contains;
--  id:2.2/8c356e46cf460ac0/Contains/1/0/
   procedure Test_Contains (Gnattest_T : in out Test) is
   --  paging-pagetables.ads:42:4:Contains
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Contains;
--  end read only


--  begin read only
   procedure Test_Get_Physical_Address (Gnattest_T : in out Test);
   procedure Test_Get_Physical_Address_de8c59 (Gnattest_T : in out Test) renames Test_Get_Physical_Address;
--  id:2.2/de8c59157bee67b2/Get_Physical_Address/1/0/
   procedure Test_Get_Physical_Address (Gnattest_T : in out Test) is
   --  paging-pagetables.ads:48:4:Get_Physical_Address
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_Physical_Address;
--  end read only


--  begin read only
   procedure Test_Set_Physical_Address (Gnattest_T : in out Test);
   procedure Test_Set_Physical_Address_d2226a (Gnattest_T : in out Test) renames Test_Set_Physical_Address;
--  id:2.2/d2226a8ed6e4a42a/Set_Physical_Address/1/0/
   procedure Test_Set_Physical_Address (Gnattest_T : in out Test) is
   --  paging-pagetables.ads:53:4:Set_Physical_Address
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Set_Physical_Address;
--  end read only


--  begin read only
   procedure Test_Iterate (Gnattest_T : in out Test);
   procedure Test_Iterate_3a84e7 (Gnattest_T : in out Test) renames Test_Iterate;
--  id:2.2/3a84e7bdee61443d/Iterate/1/0/
   procedure Test_Iterate (Gnattest_T : in out Test) is
   --  paging-pagetables.ads:59:4:Iterate
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Iterate;
--  end read only


--  begin read only
   procedure Test_Update (Gnattest_T : in out Test);
   procedure Test_Update_6460f9 (Gnattest_T : in out Test) renames Test_Update;
--  id:2.2/6460f968a8123f94/Update/1/0/
   procedure Test_Update (Gnattest_T : in out Test) is
   --  paging-pagetables.ads:67:4:Update
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Update;
--  end read only


--  begin read only
   procedure Test_Clear (Gnattest_T : in out Test);
   procedure Test_Clear_12b9e7 (Gnattest_T : in out Test) renames Test_Clear;
--  id:2.2/12b9e7be190f37f2/Clear/1/0/
   procedure Test_Clear (Gnattest_T : in out Test) is
   --  paging-pagetables.ads:74:4:Clear
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Clear;
--  end read only

end Paging.Pagetables.Test_Data.Tests;
