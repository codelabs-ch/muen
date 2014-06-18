--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into VTd.Tables.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body VTd.Tables.Test_Data.Tests is


--  begin read only
   procedure Test_Add_Entry (Gnattest_T : in out Test);
   procedure Test_Add_Entry_c1683a (Gnattest_T : in out Test) renames Test_Add_Entry;
--  id:2.2/c1683ae3e6351283/Add_Entry/1/0/
   procedure Test_Add_Entry (Gnattest_T : in out Test) is
   --  vtd-tables.ads:37:4:Add_Entry
--  end read only

      pragma Unreferenced (Gnattest_T);

      Root_Table : Root_Table_Type;
   begin
      Add_Entry (RT  => Root_Table,
                 Bus => 12,
                 CTP => 16#0010_0000#);
      Assert (Condition => Root_Table.Entries (12).Present = 1,
              Message   => "Entry not present");
      Assert (Condition => Root_Table.Entries (12).CTP = 16#0010_0000#,
              Message   => "Entry CTP mismatch");
--  begin read only
   end Test_Add_Entry;
--  end read only


--  begin read only
   procedure Test_Serialize (Gnattest_T : in out Test);
   procedure Test_Serialize_3e830c (Gnattest_T : in out Test) renames Test_Serialize;
--  id:2.2/3e830c731d3b0f3a/Serialize/1/0/
   procedure Test_Serialize (Gnattest_T : in out Test) is
   --  vtd-tables.ads:43:4:Serialize
--  end read only

      pragma Unreferenced (Gnattest_T);

      Root_Table : Root_Table_Type;
   begin
      Serialize (RT       => Root_Table,
                 Filename => "obj/serialize_default");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/serialize_default",
               Filename2 => "data/serialize_default"),
              Message   => "Default table mismatch");
      Ada.Directories.Delete_File (Name => "obj/serialize_default");

      Add_Entry (RT  => Root_Table,
                 Bus => Bus_Range'First,
                 CTP => CT_Pointer_Type'Last);
      Add_Entry (RT  => Root_Table,
                 Bus => Bus_Range'Last,
                 CTP => CT_Pointer_Type'Last);
      Serialize (RT       => Root_Table,
                 Filename => "obj/serialize");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/serialize",
               Filename2 => "data/serialize"),
              Message   => "Table mismatch");
      Ada.Directories.Delete_File (Name => "obj/serialize");
--  begin read only
   end Test_Serialize;
--  end read only

end VTd.Tables.Test_Data.Tests;
