--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Ptcheck.Test_Data.

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
package body Ptcheck.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Do_Walk (Gnattest_T : in out Test);
   procedure Test_Do_Walk_597330 (Gnattest_T : in out Test) renames Test_Do_Walk;
--  id:2.2/597330b7b94744c8/Do_Walk/1/0/
   procedure Test_Do_Walk (Gnattest_T : in out Test) is
   --  ptcheck.ads:36:4:Do_Walk
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_64;

      type Ref_Type is record
         VAddr   : Interfaces.Unsigned_64;
         PAddr   : Interfaces.Unsigned_64;
         Success : Boolean;
      end record;

      Refs : constant array (Natural range <>) of Ref_Type
        := (1 => (VAddr   => 16#9000_0000#,
                  PAddr   => 16#00ae_0000#,
                  Success => True),
            2 => (VAddr   => 16#0040_00fe#,
                  PAddr   => 16#1161_c0fe#,
                  Success => True),
            3 => (VAddr   => 16#2000_0000#,
                  PAddr   => 16#0000_0000#,
                  Success => False));

      PT_File : Ada.Streams.Stream_IO.File_Type;
      Success : Boolean;
      Address : Interfaces.Unsigned_64;
   begin
      Mutools.Files.Open (Filename => "data/nic_linux_pt",
                          File     => PT_File,
                          Writable => False);

      for R of Refs loop
         Do_Walk (Virtual_Address => R.VAddr,
                  File            => PT_File,
                  PT_Pointer      => 16#00fa_0000#,
                  PT_Type         => Paging.EPT_Mode,
                  Level           => 1,
                  PT_Address      => 16#00fa_0000#,
                  Success         => Success,
                  Translated_Addr => Address);
         Assert (Condition => R.Success = Success,
                 Message   => "Unexpected success: " & Success'Img
                 & " /=" & R.Success'Img);
         Assert (Condition => R.PAddr = Address,
                 Message   => "Unexpected address: "
                 & Mutools.Utils.To_Hex (Number => Address) & " /="
                 & Mutools.Utils.To_Hex (Number => R.PAddr));
      end loop;

      Ada.Streams.Stream_IO.Close (File => PT_File);

   exception
      when others =>
         Ada.Streams.Stream_IO.Close (File => PT_File);
         raise;
--  begin read only
   end Test_Do_Walk;
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
end Ptcheck.Test_Data.Tests;
