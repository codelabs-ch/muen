--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paging.Tables.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Paging.Entries.Table_Entry_Type_Test_Data;

--  begin read only
--  end read only
package body Paging.Tables.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add_Entry (Gnattest_T : in out Test);
   procedure Test_Add_Entry_e4a533 (Gnattest_T : in out Test) renames Test_Add_Entry;
--  id:2.2/e4a533e0a4731676/Add_Entry/1/0/
   procedure Test_Add_Entry (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Table : Page_Table_Type;
      Dummy : Entries.Table_Entry_Type;
   begin
      Assert (Condition => Table.Length = 0,
              Message   => "Table not empty");
      Add_Entry (Table => Table,
                 Index => 0,
                 E     => Entries.Table_Entry_Type_Test_Data.Test_Entry);
      Assert (Condition => Table.Length = 1,
              Message   => "Entry not added");

      --  Adding the identical entry must not raise an exception.

      Add_Entry (Table => Table,
                 Index => 0,
                 E     => Entries.Table_Entry_Type_Test_Data.Test_Entry);

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
   procedure Test_Get_Entry (Gnattest_T : in out Test);
   procedure Test_Get_Entry_e3f7ba (Gnattest_T : in out Test) renames Test_Get_Entry;
--  id:2.2/e3f7bab7ca565b5b/Get_Entry/1/0/
   procedure Test_Get_Entry (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Entries.Table_Entry_Type;

      Table : Page_Table_Type;
      Dummy : Entries.Table_Entry_Type
        := Entries.Create
          (Dst_Index   => 24,
           Dst_Address => 16#1000#,
           Readable    => True,
           Writable    => False,
           Executable  => True,
           Maps_Page   => True,
           Global      => False,
           Caching     => UC);
   begin
      Table.Data (25) := Dummy;

      Assert (Condition => Get_Entry (Table => Table,
                                      Index => 25) = Dummy,
              Message   => "Entry mismatch");

      begin
         Dummy := Get_Entry (Table => Table,
                             Index => 42);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Missing_Entry => null;
      end;
--  begin read only
   end Test_Get_Entry;
--  end read only


--  begin read only
   procedure Test_Count (Gnattest_T : in out Test);
   procedure Test_Count_165b07 (Gnattest_T : in out Test) renames Test_Count;
--  id:2.2/165b0799cddc8fab/Count/1/0/
   procedure Test_Count (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Table : Page_Table_Type := Null_Table;
      Dummy : constant Entries.Table_Entry_Type
        := Entries.Create (Dst_Index   => 5,
                           Dst_Address => 16#1000#,
                           Readable    => True,
                           Writable    => True,
                           Executable  => False,
                           Maps_Page   => True,
                           Global      => False,
                           Caching     => WB);
   begin
      Assert (Condition => Count (Table => Table) = 0,
              Message   => "Null table not empty");
      Add_Entry (Table => Table,
                 Index => 0,
                 E     => Dummy);
      Assert (Condition => Count (Table => Table) = 1,
              Message   => "Count mismatch");
--  begin read only
   end Test_Count;
--  end read only


--  begin read only
   procedure Test_Contains (Gnattest_T : in out Test);
   procedure Test_Contains_790fb8 (Gnattest_T : in out Test) renames Test_Contains;
--  id:2.2/790fb83d2013d298/Contains/1/0/
   procedure Test_Contains (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Table : Page_Table_Type := Null_Table;
      Dummy : constant Entries.Table_Entry_Type
        := Entries.Create (Dst_Index   => 5,
                           Dst_Address => 16#1000#,
                           Readable    => True,
                           Writable    => True,
                           Executable  => False,
                           Maps_Page   => True,
                           Global      => False,
                           Caching     => WB);
   begin
      Assert (Condition => not Contains
              (Table => Table,
               Index => 3),
              Message   => "Unexpected entry");

      Add_Entry (Table => Table,
                 Index => 12,
                 E     => Dummy);
      Assert (Condition => Contains
              (Table => Table,
               Index => 12),
              Message   => "Entry not found");
--  begin read only
   end Test_Contains;
--  end read only


--  begin read only
   procedure Test_Get_Physical_Address (Gnattest_T : in out Test);
   procedure Test_Get_Physical_Address_696158 (Gnattest_T : in out Test) renames Test_Get_Physical_Address;
--  id:2.2/69615859ff9354ca/Get_Physical_Address/1/0/
   procedure Test_Get_Physical_Address (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Table : Page_Table_Type := Null_Table;
   begin
      Assert (Condition => Table.Address = 0,
              Message   => "Address not zero");

      Table.Address := 16#3000#;
      Assert (Condition => Get_Physical_Address (Table => Table) = 16#3000#,
              Message   => "Address mismatch");
--  begin read only
   end Test_Get_Physical_Address;
--  end read only


--  begin read only
   procedure Test_Set_Physical_Address (Gnattest_T : in out Test);
   procedure Test_Set_Physical_Address_8a6f74 (Gnattest_T : in out Test) renames Test_Set_Physical_Address;
--  id:2.2/8a6f7428d81aac8e/Set_Physical_Address/1/0/
   procedure Test_Set_Physical_Address (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Table : Page_Table_Type := Null_Table;
   begin
      Set_Physical_Address (Table   => Table,
                            Address => 16#5000#);
      Assert (Condition => Table.Address = 16#5000#,
              Message   => "Address mismatch");
--  begin read only
   end Test_Set_Physical_Address;
--  end read only


--  begin read only
   procedure Test_Iterate (Gnattest_T : in out Test);
   procedure Test_Iterate_a130ac (Gnattest_T : in out Test) renames Test_Iterate;
--  id:2.2/a130ac86f3412004/Iterate/1/0/
   procedure Test_Iterate (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Counter : Natural         := 0;
      Table   : Page_Table_Type := Null_Table;
      Dummy   : Entries.Table_Entry_Type;

      ----------------------------------------------------------------------

      procedure Inc_Counter
        (Index  : Entry_Range;
         TEntry : Entries.Table_Entry_Type)
      is
      begin
         Counter := Counter + 1;
      end Inc_Counter;
   begin
      Add_Entry (Table => Table,
                 Index => 1,
                 E     => Dummy);
      Add_Entry (Table => Table,
                 Index => 2,
                 E     => Dummy);

      Iterate (Table   => Table,
               Process => Inc_Counter'Access);
      Assert (Condition => Counter = 2,
              Message   => "Counter mismatch" & Counter'Img);
--  begin read only
   end Test_Iterate;
--  end read only


--  begin read only
   procedure Test_Update (Gnattest_T : in out Test);
   procedure Test_Update_c7a83f (Gnattest_T : in out Test) renames Test_Update;
--  id:2.2/c7a83fd5009ccda3/Update/1/0/
   procedure Test_Update (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Counter : Natural         := 0;
      Table   : Page_Table_Type := Null_Table;
      Dummy   : Entries.Table_Entry_Type;

      ----------------------------------------------------------------------

      procedure Inc_Counter
        (Index  :        Entry_Range;
         TEntry : in out Entries.Table_Entry_Type)
      is
      begin
         Counter := Counter + 1;
      end Inc_Counter;
   begin
      Add_Entry (Table => Table,
                 Index => 1,
                 E     => Dummy);
      Add_Entry (Table => Table,
                 Index => 2,
                 E     => Dummy);

      Update (Table   => Table,
              Process => Inc_Counter'Access);
      Assert (Condition => Counter = 2,
              Message   => "Counter mismatch" & Counter'Img);
--  begin read only
   end Test_Update;
--  end read only


--  begin read only
   procedure Test_Clear (Gnattest_T : in out Test);
   procedure Test_Clear_5b2240 (Gnattest_T : in out Test) renames Test_Clear;
--  id:2.2/5b2240cb4b547858/Clear/1/0/
   procedure Test_Clear (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Table : Page_Table_Type;
      Dummy : constant Entries.Table_Entry_Type
        := Entries.Create (Dst_Index   => 5,
                           Dst_Address => 16#1000#,
                           Readable    => True,
                           Writable    => True,
                           Executable  => False,
                           Maps_Page   => True,
                           Global      => False,
                           Caching     => WB);
   begin
      Add_Entry (Table => Table,
                 Index => 1,
                 E     => Dummy);
      Table.Address := 16#4000#;

      Clear (Table => Table);

      Assert (Condition => Table.Length = 0,
              Message   => "Table not cleared");
      Assert (Condition => Table.Address = 0,
              Message   => "Table address not zero");
--  begin read only
   end Test_Clear;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Paging.Tables.Test_Data.Tests;
