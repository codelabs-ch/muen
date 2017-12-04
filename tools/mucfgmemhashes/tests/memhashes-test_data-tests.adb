--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Memhashes.Test_Data.

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
package body Memhashes.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Run (Gnattest_T : in out Test);
   procedure Test_Run_e84213 (Gnattest_T : in out Test) renames Test_Run;
--  id:2.2/e84213e130018c54/Run/1/0/
   procedure Test_Run (Gnattest_T : in out Test) is
   --  memhashes.ads:27:4:Run
--  end read only

      pragma Unreferenced (Gnattest_T);

      Fname : constant String := "obj/test_policy_hashes.xml";
   begin
      Run (Policy_In  => "data/test_policy.xml",
           Policy_Out => Fname,
           Input_Dir  => "data");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Fname,
               Filename2 => "data/test_policy_hashes.xml"),
              Message   => "Policy mismatch");
      Ada.Directories.Delete_File (Name => Fname);
--  begin read only
   end Test_Run;
--  end read only


--  begin read only
   procedure Test_Resolve_Refs (Gnattest_T : in out Test);
   procedure Test_Resolve_Refs_eb1e6d (Gnattest_T : in out Test) renames Test_Resolve_Refs;
--  id:2.2/eb1e6dac99bddea5/Resolve_Refs/1/0/
   procedure Test_Resolve_Refs (Gnattest_T : in out Test) is
   --  memhashes.ads:35:4:Resolve_Refs
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      begin
         Resolve_Refs (Policy => Policy);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Reference_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical memory 'src' referenced by hashRef of memory "
                    & "'dst1' does not provide hash element",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Resolve_Refs;
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
end Memhashes.Test_Data.Tests;
