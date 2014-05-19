--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with AUnit.Test_Fixtures;

generic
   type GNATtest_Test_Type is new AUnit.Test_Fixtures.Test_Fixture
     with private;
package Paging.Pagetable.Test_Data is

--  begin read only
   type Test is new AUnit.Test_Fixtures.Test_Fixture
--  end read only
   with record
      Table  : Page_Table_Type;
      Map    : Page_Table_Map;
      TEntry : Entry_Type;
   end record;

   procedure Set_Up (Gnattest_T : in out Test);
   procedure Tear_Down (Gnattest_T : in out Test);

   procedure User_Set_Up (Gnattest_T : in out Test)is null;
   procedure User_Tear_Down (Gnattest_T : in out Test)is null;

   Test_Counter : Natural := 0;

   procedure Inc_Counter_1
     (Index  : Table_Range;
      TEntry : Entry_Type);

   procedure Inc_Counter_2
     (Index  :        Table_Range;
      TEntry : in out Entry_Type);

   procedure Inc_Counter_3
     (Table_Number : Table_Range;
      Table        : Page_Table_Type);

   procedure Inc_Counter_4
     (Table_Number :        Table_Range;
      Table        : in out Page_Table_Type);

end Paging.Pagetable.Test_Data;
