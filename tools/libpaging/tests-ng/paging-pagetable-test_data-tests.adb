--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paging.Pagetable.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Paging.Pagetable.Test_Data.Tests is


--  begin read only
   procedure Test_1_Add_Entry (Gnattest_T : in out Test);
   procedure Test_Add_Entry_a4734c (Gnattest_T : in out Test) renames Test_1_Add_Entry;
--  id:2.2/a4734cdda1d68840/Add_Entry/1/0/
   procedure Test_1_Add_Entry (Gnattest_T : in out Test) is
   --  paging-pagetable.ads:44:4:Add_Entry
--  end read only
      use type Ada.Containers.Count_Type;
   begin
      Add_Entry (Table => Gnattest_T.Table,
                 Index => 0,
                 E     => Gnattest_T.TEntry);
      Assert (Condition => Gnattest_T.Table.Data.Length = 1,
              Message   => "Entry not added");
--  begin read only
   end Test_1_Add_Entry;
--  end read only


--  begin read only
   procedure Test_Count (Gnattest_T : in out Test);
   procedure Test_Count_728cee (Gnattest_T : in out Test) renames Test_Count;
--  id:2.2/728cee6eaf41b673/Count/1/0/
   procedure Test_Count (Gnattest_T : in out Test) is
   --  paging-pagetable.ads:50:4:Count
--  end read only
      use type Ada.Containers.Count_Type;
   begin
      Assert (Condition => Count (Table => Gnattest_T.Table) = 0,
              Message   => "Table not empty");
      Add_Entry (Table => Gnattest_T.Table,
                 Index => 0,
                 E     => Gnattest_T.TEntry);
      Assert (Condition => Count (Table => Gnattest_T.Table) = 1,
              Message   => "Count mismatch");
--  begin read only
   end Test_Count;
--  end read only


--  begin read only
   procedure Test_1_Contains (Gnattest_T : in out Test);
   procedure Test_Contains_3fab00 (Gnattest_T : in out Test) renames Test_1_Contains;
--  id:2.2/3fab003ac73e2394/Contains/1/0/
   procedure Test_1_Contains (Gnattest_T : in out Test) is
   --  paging-pagetable.ads:53:4:Contains
--  end read only
   begin
      Assert (Condition => not Contains
              (Table => Gnattest_T.Table,
               Index => 3),
              Message   => "Unexpected entry");

      Add_Entry (Table => Gnattest_T.Table,
                 Index => 12,
                 E     => Gnattest_T.TEntry);
      Assert (Condition => Contains
              (Table => Gnattest_T.Table,
               Index => 12),
              Message   => "Entry not found");
--  begin read only
   end Test_1_Contains;
--  end read only


--  begin read only
   procedure Test_Get_Physical_Address (Gnattest_T : in out Test);
   procedure Test_Get_Physical_Address_885458 (Gnattest_T : in out Test) renames Test_Get_Physical_Address;
--  id:2.2/885458a50a007433/Get_Physical_Address/1/0/
   procedure Test_Get_Physical_Address (Gnattest_T : in out Test) is
   --  paging-pagetable.ads:59:4:Get_Physical_Address
--  end read only
      use type Interfaces.Unsigned_64;
   begin
      Assert (Condition => Gnattest_T.Table.Address = 0,
              Message   => "Address not zero");

      Gnattest_T.Table.Address := 16#3000#;
      Assert (Condition => Get_Physical_Address
              (Table => Gnattest_T.Table) = 16#3000#,
              Message   => "Address mismatch");
--  begin read only
   end Test_Get_Physical_Address;
--  end read only


--  begin read only
   procedure Test_Set_Physical_Address (Gnattest_T : in out Test);
   procedure Test_Set_Physical_Address_5ba9eb (Gnattest_T : in out Test) renames Test_Set_Physical_Address;
--  id:2.2/5ba9eb0c08b7fd0e/Set_Physical_Address/1/0/
   procedure Test_Set_Physical_Address (Gnattest_T : in out Test) is
   --  paging-pagetable.ads:64:4:Set_Physical_Address
--  end read only
      use type Interfaces.Unsigned_64;
   begin
      Set_Physical_Address (Table   => Gnattest_T.Table,
                            Address => 16#5000#);
      Assert (Condition => Gnattest_T.Table.Address = 16#5000#,
              Message   => "Address mismatch");
--  begin read only
   end Test_Set_Physical_Address;
--  end read only


--  begin read only
   procedure Test_1_Iterate (Gnattest_T : in out Test);
   procedure Test_Iterate_0f169c (Gnattest_T : in out Test) renames Test_1_Iterate;
--  id:2.2/0f169cd13ace2737/Iterate/1/0/
   procedure Test_1_Iterate (Gnattest_T : in out Test) is
   --  paging-pagetable.ads:70:4:Iterate
--  end read only
   begin
      Add_Entry (Table => Gnattest_T.Table,
                 Index => 1,
                 E     => Gnattest_T.TEntry);
      Add_Entry (Table => Gnattest_T.Table,
                 Index => 2,
                 E     => Gnattest_T.TEntry);

      Iterate (Table   => Gnattest_T.Table,
               Process => Inc_Counter_1'Access);
      Assert (Condition => Test_Counter = 2,
              Message   => "Counter mismatch" & Test_Counter'Img);
--  begin read only
   end Test_1_Iterate;
--  end read only


--  begin read only
   procedure Test_1_Update (Gnattest_T : in out Test);
   procedure Test_Update_29bdb2 (Gnattest_T : in out Test) renames Test_1_Update;
--  id:2.2/29bdb22fb145e685/Update/1/0/
   procedure Test_1_Update (Gnattest_T : in out Test) is
   --  paging-pagetable.ads:78:4:Update
--  end read only
   begin
      Add_Entry (Table => Gnattest_T.Table,
                 Index => 1,
                 E     => Gnattest_T.TEntry);
      Update (Table   => Gnattest_T.Table,
              Process => Inc_Counter_2'Access);
      Assert (Condition => Test_Counter = 1,
              Message   => "Counter mismatch" & Test_Counter'Img);
--  begin read only
   end Test_1_Update;
--  end read only


--  begin read only
   procedure Test_1_Clear (Gnattest_T : in out Test);
   procedure Test_Clear_6aaa65 (Gnattest_T : in out Test) renames Test_1_Clear;
--  id:2.2/6aaa657ec64c7fcd/Clear/1/0/
   procedure Test_1_Clear (Gnattest_T : in out Test) is
   --  paging-pagetable.ads:85:4:Clear
--  end read only
      use type Ada.Containers.Count_Type;
      use type Interfaces.Unsigned_64;
   begin
      Add_Entry (Table => Gnattest_T.Table,
                 Index => 1,
                 E     => Gnattest_T.TEntry);
      Gnattest_T.Table.Address := 16#4000#;

      Clear (Table => Gnattest_T.Table);

      Assert (Condition => Gnattest_T.Table.Data.Length = 0,
              Message   => "Table not cleared");
      Assert (Condition => Gnattest_T.Table.Address = 0,
              Message   => "Table address not zero");
--  begin read only
   end Test_1_Clear;
--  end read only


--  begin read only
   procedure Test_2_Contains (Gnattest_T : in out Test);
   procedure Test_Contains_9d1091 (Gnattest_T : in out Test) renames Test_2_Contains;
--  id:2.2/9d1091ed14a6fdf7/Contains/0/0/
   procedure Test_2_Contains (Gnattest_T : in out Test) is
   --  paging-pagetable.ads:92:4:Contains
--  end read only
   begin
      Assert (Condition => not Contains
              (Map          => Gnattest_T.Map,
               Table_Number => 6,
               Entry_Index  => 4),
              Message   => "Unexpected entry");

      Add_Entry (Map          => Gnattest_T.Map,
                 Table_Number => 2,
                 Entry_Index  => 3,
                 Table_Entry  => Gnattest_T.TEntry);
      Assert (Condition => Contains
              (Map          => Gnattest_T.Map,
               Table_Number => 2,
               Entry_Index  => 3),
              Message   => "Entry not found");
--  begin read only
   end Test_2_Contains;
--  end read only


--  begin read only
   procedure Test_2_Add_Entry (Gnattest_T : in out Test);
   procedure Test_Add_Entry_65d51a (Gnattest_T : in out Test) renames Test_2_Add_Entry;
--  id:2.2/65d51aa67b84994c/Add_Entry/0/0/
   procedure Test_2_Add_Entry (Gnattest_T : in out Test) is
   --  paging-pagetable.ads:100:4:Add_Entry
--  end read only
      use type Ada.Containers.Count_Type;
   begin
      Add_Entry (Map          => Gnattest_T.Map,
                 Table_Number => 2,
                 Entry_Index  => 3,
                 Table_Entry  => Gnattest_T.TEntry);
      Assert (Condition => Gnattest_T.Map.Tables.Length = 1,
              Message   => "Entry not added");

      begin
         Add_Entry (Map          => Gnattest_T.Map,
                    Table_Number => 2,
                    Entry_Index  => 3,
                    Table_Entry  => Gnattest_T.TEntry);

      exception
         when Duplicate_Entry => null;
      end;
--  begin read only
   end Test_2_Add_Entry;
--  end read only


--  begin read only
   procedure Test_Get_Table_Address (Gnattest_T : in out Test);
   procedure Test_Get_Table_Address_fc9ed7 (Gnattest_T : in out Test) renames Test_Get_Table_Address;
--  id:2.2/fc9ed7f2652cb3f4/Get_Table_Address/1/0/
   procedure Test_Get_Table_Address (Gnattest_T : in out Test) is
   --  paging-pagetable.ads:107:4:Get_Table_Address
--  end read only
      use type Interfaces.Unsigned_64;

      Address : Interfaces.Unsigned_64;
   begin
      Add_Entry (Map          => Gnattest_T.Map,
                 Table_Number => 2,
                 Entry_Index  => 3,
                 Table_Entry  => Gnattest_T.TEntry);
      Assert (Condition => Get_Table_Address
              (Map          => Gnattest_T.Map,
               Table_Number => 2) = 0,
              Message   => "Unexpected address");

      begin
         Address := Get_Table_Address
           (Map          => Gnattest_T.Map,
            Table_Number => 12);

      exception
         when Missing_Table => null;
      end;
--  begin read only
   end Test_Get_Table_Address;
--  end read only


--  begin read only
   procedure Test_Length (Gnattest_T : in out Test);
   procedure Test_Length_9893fe (Gnattest_T : in out Test) renames Test_Length;
--  id:2.2/9893fe3cdac3dfa6/Length/1/0/
   procedure Test_Length (Gnattest_T : in out Test) is
   --  paging-pagetable.ads:113:4:Length
--  end read only
      use type Ada.Containers.Count_Type;
   begin
      Assert (Condition => Length (Map => Gnattest_T.Map) = 0,
              Message   => "Map not empty");
      Add_Entry (Map          => Gnattest_T.Map,
                 Table_Number => 2,
                 Entry_Index  => 3,
                 Table_Entry  => Gnattest_T.TEntry);
      Assert (Condition => Length (Map => Gnattest_T.Map) = 1,
              Message   => "Length mismatch");
--  begin read only
   end Test_Length;
--  end read only


--  begin read only
   procedure Test_2_Update (Gnattest_T : in out Test);
   procedure Test_Update_70239d (Gnattest_T : in out Test) renames Test_2_Update;
--  id:2.2/70239d729314ca12/Update/0/0/
   procedure Test_2_Update (Gnattest_T : in out Test) is
   --  paging-pagetable.ads:117:4:Update
--  end read only
   begin
      Add_Entry (Map          => Gnattest_T.Map,
                 Table_Number => 2,
                 Entry_Index  => 3,
                 Table_Entry  => Gnattest_T.TEntry);
      Update (Map     => Gnattest_T.Map,
              Process => Inc_Counter_4'Access);
      Assert (Condition => Test_Counter = 1,
              Message   => "Counter mismatch" & Test_Counter'Img);
--  begin read only
   end Test_2_Update;
--  end read only


--  begin read only
   procedure Test_2_Iterate (Gnattest_T : in out Test);
   procedure Test_Iterate_5faa5a (Gnattest_T : in out Test) renames Test_2_Iterate;
--  id:2.2/5faa5a0a5bad1a74/Iterate/0/0/
   procedure Test_2_Iterate (Gnattest_T : in out Test) is
   --  paging-pagetable.ads:125:4:Iterate
--  end read only
   begin
      Add_Entry (Map          => Gnattest_T.Map,
                 Table_Number => 1,
                 Entry_Index  => 1,
                 Table_Entry  => Gnattest_T.TEntry);
      Add_Entry (Map          => Gnattest_T.Map,
                 Table_Number => 2,
                 Entry_Index  => 2,
                 Table_Entry  => Gnattest_T.TEntry);
      Iterate (Map     => Gnattest_T.Map,
               Process => Inc_Counter_3'Access);
      Assert (Condition => Test_Counter = 2,
              Message   => "Counter mismatch" & Test_Counter'Img);
--  begin read only
   end Test_2_Iterate;
--  end read only


--  begin read only
   procedure Test_2_Clear (Gnattest_T : in out Test);
   procedure Test_Clear_6001b9 (Gnattest_T : in out Test) renames Test_2_Clear;
--  id:2.2/6001b9bac3147d11/Clear/0/0/
   procedure Test_2_Clear (Gnattest_T : in out Test) is
   --  paging-pagetable.ads:132:4:Clear
--  end read only
      use type Ada.Containers.Count_Type;
      use type Interfaces.Unsigned_64;
   begin
      Add_Entry (Map          => Gnattest_T.Map,
                 Table_Number => 1,
                 Entry_Index  => 1,
                 Table_Entry  => Gnattest_T.TEntry);
      Clear (Map => Gnattest_T.Map);

      Assert (Condition => Gnattest_T.Map.Tables.Length = 0,
              Message   => "Map not cleared");
--  begin read only
   end Test_2_Clear;
--  end read only

end Paging.Pagetable.Test_Data.Tests;
