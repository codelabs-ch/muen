--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Musinfo.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Musinfo.Utils.Test_Data.Tests is


--  begin read only
   procedure Test_Names_Match (Gnattest_T : in out Test);
   procedure Test_Names_Match_5a0f19 (Gnattest_T : in out Test) renames Test_Names_Match;
--  id:2.2/5a0f194a9746674a/Names_Match/1/0/
   procedure Test_Names_Match (Gnattest_T : in out Test) is
   --  musinfo-utils.ads:33:4:Names_Match
--  end read only

      pragma Unreferenced (Gnattest_T);

      Ref_Name : constant Name_Type
        := Name_Type'(Length  => 12,
                      Padding => (others => 0),
                      Data    => Name_Data_Type'
                        (1 .. 12 => 'a', others => ASCII.NUL));
   begin
      Assert (Condition => Names_Match
              (N1 => Null_Name,
               N2 => ""),
              Message   => "Null name does not match");
      Assert (Condition => not Names_Match
              (N1 => Null_Name,
               N2 => "something"),
              Message   => "Null name matches (1)");
      Assert (Condition => not Names_Match
              (N1 => Ref_Name,
               N2 => ""),
              Message   => "Null name matches (2)");
      Assert (Condition => Names_Match
              (N1 => Ref_Name,
               N2 => "aaaaaaaaaaaa"),
              Message   => "Name does not match");
--  begin read only
   end Test_Names_Match;
--  end read only

end Musinfo.Utils.Test_Data.Tests;
