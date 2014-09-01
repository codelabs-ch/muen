--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into VTd.Tables.IR.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body VTd.Tables.IR.Test_Data.Tests is


--  begin read only
   procedure Test_Add_Entry (Gnattest_T : in out Test);
   procedure Test_Add_Entry_5b70f8 (Gnattest_T : in out Test) renames Test_Add_Entry;
--  id:2.2/5b70f8c6d03e95bc/Add_Entry/1/0/
   procedure Test_Add_Entry (Gnattest_T : in out Test) is
   --  vtd-tables-ir.ads:33:4:Add_Entry
--  end read only
      use type Interfaces.Unsigned_8;
      use type Interfaces.Unsigned_16;
      use type Interfaces.Unsigned_32;

      D : constant IR_Entry_Type := Gnattest_T.IRT.Entries (Index_Range'First);
      E : constant IR_Entry_Type := Gnattest_T.IRT.Entries (Index_Range'Last);
   begin
      Assert (Condition => D.Present = 0,
              Message   => "Default is present");

      Assert (Condition => E.Present = 1,
              Message   => "Entry not present");
      Assert (Condition => E.V = 12,
              Message   => "Vector mismatch");
      Assert (Condition => E.DST = 122344,
              Message   => "DST mismatch");
      Assert (Condition => E.SID = 404,
              Message   => "SID mismatch");
--  begin read only
   end Test_Add_Entry;
--  end read only

end VTd.Tables.IR.Test_Data.Tests;
