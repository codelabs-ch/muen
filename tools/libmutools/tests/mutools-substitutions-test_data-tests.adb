--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Substitutions.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Mutools.Substitutions.Test_Data.Tests is


--  begin read only
   procedure Test_Process_Attributes (Gnattest_T : in out Test);
   procedure Test_Process_Attributes_b56a16 (Gnattest_T : in out Test) renames Test_Process_Attributes;
--  id:2.2/b56a16e5501c94a3/Process_Attributes/1/0/
   procedure Test_Process_Attributes (Gnattest_T : in out Test) is
   --  mutools-substitutions.ads:26:4:Process_Attributes
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.None,
                   File => "data/substitution.xml");
      Process_Attributes (Data => Data);
      Muxml.Write (Data => Data,
                   Kind => Muxml.None,
                   File => "obj/substitution.xml");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/substitution.ref.xml",
               Filename2 => "obj/substitution.xml"),
              Message   => "Substituted XML mismatch");
      Ada.Directories.Delete_File (Name => "obj/substitution.xml");
--  begin read only
   end Test_Process_Attributes;
--  end read only

end Mutools.Substitutions.Test_Data.Tests;
