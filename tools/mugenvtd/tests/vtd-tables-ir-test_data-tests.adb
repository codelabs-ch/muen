--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into VTd.Tables.IR.Test_Data.

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
package body VTd.Tables.IR.Test_Data.Tests is

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
   procedure Test_Add_Entry_45a094 (Gnattest_T : in out Test) renames Test_Add_Entry;
--  id:2.2/45a0947c33b7369f/Add_Entry/1/0/
   procedure Test_Add_Entry (Gnattest_T : in out Test) is
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
      Assert (Condition => E.TM = 1,
              Message   => "TM mismatch");
--  begin read only
   end Test_Add_Entry;
--  end read only


--  begin read only
   procedure Test_Serialize (Gnattest_T : in out Test);
   procedure Test_Serialize_5ca693 (Gnattest_T : in out Test) renames Test_Serialize;
--  id:2.2/5ca693daea6db0b3/Serialize/1/0/
   procedure Test_Serialize (Gnattest_T : in out Test) is
--  end read only
   begin
      Serialize (IRT      => Gnattest_T.IRT,
                 Filename => "obj/serialize_irt");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/serialize_irt",
               Filename2 => "data/serialize_irt"),
              Message   => "Table mismatch");
      Ada.Directories.Delete_File (Name => "obj/serialize_irt");
--  begin read only
   end Test_Serialize;
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
end VTd.Tables.IR.Test_Data.Tests;
