--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Expanders.Utils.Test_Data.Tests is


--  begin read only
   procedure Test_Allocate (Gnattest_T : in out Test);
   procedure Test_Allocate_ceb92c (Gnattest_T : in out Test) renames Test_Allocate;
--  id:2.2/ceb92c0fcf2b5351/Allocate/1/0/
   procedure Test_Allocate (Gnattest_T : in out Test) is
   --  expanders-utils.ads:26:4:Allocate
--  end read only

      pragma Unreferenced (Gnattest_T);

      Alloc : Number_Allocator_Type (Range_Start => 32,
                                     Range_End   => 255);
      Num   : Natural;
   begin
      Allocate (Allocator => Alloc,
                Number    => Num);
      Assert (Condition => Num = 32,
              Message   => "Allocated number mismatch (1)");
      Assert (Condition => not Alloc.Numbers (32),
              Message   => "Number not marked as allocated");

      Alloc.Numbers (33) := False;
      Allocate (Allocator => Alloc,
                Number    => Num);
      Assert (Condition => Num = 34,
              Message   => "Allocated number mismatch (2)");

      begin
         Alloc.Numbers := (others => False);
         Allocate (Allocator => Alloc,
                   Number    => Num);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when No_Free_Number => null;
      end;
--  begin read only
   end Test_Allocate;
--  end read only

end Expanders.Utils.Test_Data.Tests;
