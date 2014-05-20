--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Paging.Pagetable.Test_Data is

   -------------------------------------------------------------------------

   procedure Set_Up (Gnattest_T : in out Test) is
      X : Test'Class renames Test'Class (Gnattest_T);
   begin
      X.User_Set_Up;
   end Set_Up;

   -------------------------------------------------------------------------

   procedure Tear_Down (Gnattest_T : in out Test) is
      X : Test'Class renames Test'Class (Gnattest_T);
   begin
      X.User_Tear_Down;
      Test_Counter := 0;
      Clear (Table => Gnattest_T.Table);
      Clear (Map => Gnattest_T.Map);
   end Tear_Down;

   -------------------------------------------------------------------------

   procedure Inc_Counter_1
     (Index  : Table_Range;
      TEntry : Entry_Type)
   is
   begin
      Test_Counter := Test_Counter + 1;
   end Inc_Counter_1;

   -------------------------------------------------------------------------

   procedure Inc_Counter_2
     (Index  :        Table_Range;
      TEntry : in out Entry_Type)
   is
   begin
      Test_Counter := Test_Counter + 1;
   end Inc_Counter_2;

   -------------------------------------------------------------------------

   procedure Inc_Counter_3
     (Table_Number : Table_Range;
      Table        : Page_Table_Type)
   is
   begin
      Test_Counter := Test_Counter + 1;
   end Inc_Counter_3;

   -------------------------------------------------------------------------

   procedure Inc_Counter_4
     (Table_Number :        Table_Range;
      Table        : in out Page_Table_Type)
   is
   begin
      Test_Counter := Test_Counter + 1;
   end Inc_Counter_4;

end Paging.Pagetable.Test_Data;
