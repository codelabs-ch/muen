--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Paging.Maps.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Paging.Maps.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

   Test_Entry : constant Entries.Table_Entry_Type
     := Entries.Create (Dst_Index   => 5,
                        Dst_Address => 16#1000#,
                        Readable    => True,
                        Writable    => True,
                        Executable  => False,
                        Maps_Page   => True,
                        Global      => False,
                        Caching     => WB);

--  begin read only
--  end read only

--  begin read only
   procedure Test_Contains (Gnattest_T : in out Test);
   procedure Test_Contains_300f40 (Gnattest_T : in out Test) renames Test_Contains;
--  id:2.2/300f4034d9b459b2/Contains/1/0/
   procedure Test_Contains (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Map   : Page_Table_Map;
   begin
      Assert (Condition => not Contains
              (Map          => Map,
               Table_Number => 6,
               Entry_Index  => 4),
              Message   => "Unexpected entry");

      Add_Entry (Map          => Map,
                 Table_Number => 2,
                 Entry_Index  => 3,
                 Table_Entry  => Test_Entry);
      Assert (Condition => Contains
              (Map          => Map,
               Table_Number => 2,
               Entry_Index  => 3),
              Message   => "Entry not found");
--  begin read only
   end Test_Contains;
--  end read only


--  begin read only
   procedure Test_Add_Entry (Gnattest_T : in out Test);
   procedure Test_Add_Entry_b86e7b (Gnattest_T : in out Test) renames Test_Add_Entry;
--  id:2.2/b86e7b2eeb5eeb34/Add_Entry/1/0/
   procedure Test_Add_Entry (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Containers.Count_Type;

      Map : Page_Table_Map;
   begin
      Add_Entry (Map          => Map,
                 Table_Number => 2,
                 Entry_Index  => 3,
                 Table_Entry  => Test_Entry);
      Assert (Condition => Map.Tables.Length = 1,
              Message   => "Entry not added");
--  begin read only
   end Test_Add_Entry;
--  end read only


--  begin read only
   procedure Test_Get_Entry (Gnattest_T : in out Test);
   procedure Test_Get_Entry_359f73 (Gnattest_T : in out Test) renames Test_Get_Entry;
--  id:2.2/359f73bad9810504/Get_Entry/1/0/
   procedure Test_Get_Entry (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Entries.Table_Entry_Type;

      Map : Page_Table_Map;
   begin
      Add_Entry (Map          => Map,
                 Table_Number => 2,
                 Entry_Index  => 3,
                 Table_Entry  => Test_Entry);
      Assert (Condition => Get_Entry
              (Map          => Map,
               Table_Number => 2,
               Entry_Index  => 3) = Test_Entry,
              Message   => "Entry mismatch");

      declare
         Dummy : Entries.Table_Entry_Type;
      begin
         Dummy := Get_Entry (Map          => Map,
                             Table_Number => 25,
                             Entry_Index  => 25);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Missing_Table => null;
      end;
--  begin read only
   end Test_Get_Entry;
--  end read only


--  begin read only
   procedure Test_Get_Table_Address (Gnattest_T : in out Test);
   procedure Test_Get_Table_Address_8b3274 (Gnattest_T : in out Test) renames Test_Get_Table_Address;
--  id:2.2/8b3274220cfac2b3/Get_Table_Address/1/0/
   procedure Test_Get_Table_Address (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      Map : Page_Table_Map;
   begin
      Add_Entry (Map          => Map,
                 Table_Number => 2,
                 Entry_Index  => 3,
                 Table_Entry  => Test_Entry);
      Assert (Condition => Get_Table_Address
              (Map          => Map,
               Table_Number => 2) = 0,
              Message   => "Unexpected address");

      declare
         Unused : Interfaces.Unsigned_64;
      begin
         Unused := Get_Table_Address
           (Map          => Map,
            Table_Number => 12);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Missing_Table => null;
      end;
--  begin read only
   end Test_Get_Table_Address;
--  end read only


--  begin read only
   procedure Test_Length (Gnattest_T : in out Test);
   procedure Test_Length_f363cb (Gnattest_T : in out Test) renames Test_Length;
--  id:2.2/f363cb6cb38c39e2/Length/1/0/
   procedure Test_Length (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Containers.Count_Type;

      Map : Page_Table_Map;
   begin
      Assert (Condition => Length (Map => Map) = 0,
              Message   => "Map not empty");
      Add_Entry (Map          => Map,
                 Table_Number => 2,
                 Entry_Index  => 3,
                 Table_Entry  => Test_Entry);
      Assert (Condition => Length (Map => Map) = 1,
              Message   => "Length mismatch");
--  begin read only
   end Test_Length;
--  end read only


--  begin read only
   procedure Test_Update (Gnattest_T : in out Test);
   procedure Test_Update_91c512 (Gnattest_T : in out Test) renames Test_Update;
--  id:2.2/91c512879b519688/Update/1/0/
   procedure Test_Update (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Map     : Page_Table_Map;
      Counter : Natural := 0;

      ----------------------------------------------------------------------

      procedure Inc_Counter
        (Table_Number :        Table_Range;
         Table        : in out Tables.Page_Table_Type)
      is
      begin
         Counter := Counter + 1;
      end Inc_Counter;
   begin
      Add_Entry (Map          => Map,
                 Table_Number => 2,
                 Entry_Index  => 3,
                 Table_Entry  => Test_Entry);
      Update (Map     => Map,
              Process => Inc_Counter'Access);
      Assert (Condition => Counter = 1,
              Message   => "Counter mismatch" & Counter'Img);
--  begin read only
   end Test_Update;
--  end read only


--  begin read only
   procedure Test_Iterate (Gnattest_T : in out Test);
   procedure Test_Iterate_869b21 (Gnattest_T : in out Test) renames Test_Iterate;
--  id:2.2/869b215615633eeb/Iterate/1/0/
   procedure Test_Iterate (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Map     : Page_Table_Map;
      Counter : Natural := 0;

      ----------------------------------------------------------------------

      procedure Inc_Counter
        (Table_Number : Table_Range;
         Table        : Tables.Page_Table_Type)
      is
      begin
         Counter := Counter + 1;
      end Inc_Counter;
   begin
      Add_Entry (Map          => Map,
                 Table_Number => 1,
                 Entry_Index  => 1,
                 Table_Entry  => Test_Entry);
      Add_Entry (Map          => Map,
                 Table_Number => 2,
                 Entry_Index  => 2,
                 Table_Entry  => Test_Entry);
      Iterate (Map     => Map,
               Process => Inc_Counter'Access);
      Assert (Condition => Counter = 2,
              Message   => "Counter mismatch" & Counter'Img);
--  begin read only
   end Test_Iterate;
--  end read only


--  begin read only
   procedure Test_Clear (Gnattest_T : in out Test);
   procedure Test_Clear_ed2306 (Gnattest_T : in out Test) renames Test_Clear;
--  id:2.2/ed2306f05183c90b/Clear/1/0/
   procedure Test_Clear (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Containers.Count_Type;

      Map : Page_Table_Map;
   begin
      Add_Entry (Map          => Map,
                 Table_Number => 1,
                 Entry_Index  => 1,
                 Table_Entry  => Test_Entry);
      Clear (Map => Map);

      Assert (Condition => Map.Tables.Length = 0,
              Message   => "Map not cleared");
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
end Paging.Maps.Test_Data.Tests;
