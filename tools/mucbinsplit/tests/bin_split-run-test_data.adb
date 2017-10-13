--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Bin_Split.Run.Test_Data is

   procedure Set_Up (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);

      Out_Dir : constant String := "test-out-dir";
   begin

      if Ada.Directories.Exists (Name => Out_Dir) then
         Ada.Directories.Delete_Tree (Directory => Out_Dir);
      end if;

   end Tear_Down;

end Bin_Split.Run.Test_Data;
