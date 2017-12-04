--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Ukvm.Generator.Test_Data.

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
package body Ukvm.Generator.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_23ab15 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/23ab1562ae4604fa/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
   --  ukvm-generator.ads:26:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;

      Uni1_Ukvm : constant String := "obj/unikernel1_ukvmbi";
      Uni2_Ukvm : constant String := "obj/unikernel2_ukvmbi";
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Write (Output_Dir => "obj",
             Policy     => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/unikernel1_ukvmbi",
               Filename2 => Uni1_Ukvm),
              Message   => "Unikernel 1 UKVM boot info mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/unikernel2_ukvmbi",
               Filename2 => Uni2_Ukvm),
              Message   => "Unikernel 2 UKVM boot info mismatch");

      Ada.Directories.Delete_File (Name => Uni1_Ukvm);
      Ada.Directories.Delete_File (Name => Uni2_Ukvm);
--  begin read only
   end Test_Write;
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
end Ukvm.Generator.Test_Data.Tests;
