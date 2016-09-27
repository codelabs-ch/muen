--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Sinfo.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Sinfo.Utils.Test_Data.Tests is


--  begin read only
   procedure Test_To_Hash (Gnattest_T : in out Test);
   procedure Test_To_Hash_96113d (Gnattest_T : in out Test) renames Test_To_Hash;
--  id:2.2/96113d1af88939fd/To_Hash/1/0/
   procedure Test_To_Hash (Gnattest_T : in out Test) is
   --  sinfo-utils.ads:25:4:To_Hash
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Musinfo.Hash_Type;

      H : Musinfo.Hash_Type
        := (16#e1#, 16#0e#, 16#6c#, 16#3b#, 16#5f#, 16#7e#, 16#8e#, 16#b8#,
            16#a5#, 16#51#, 16#0b#, 16#85#, 16#62#, 16#ed#, 16#44#, 16#91#,
            16#00#, 16#a8#, 16#56#, 16#e0#, 16#2a#, 16#31#, 16#a2#, 16#7d#,
            16#30#, 16#bf#, 16#5e#, 16#76#, 16#ef#, 16#d9#, 16#12#, 16#35#);
   begin
      Assert (Condition => To_Hash
              (Hex => "16#e10e6c3b5f7e8eb8a5510b8562ed449100a856e02a31a27d30bf"
               & "5e76efd91235#") = H,
              Message   => "Hash mismatch");
--  begin read only
   end Test_To_Hash;
--  end read only

end Sinfo.Utils.Test_Data.Tests;