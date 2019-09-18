--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Compjoin.Utils.Test_Data.

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
package body Compjoin.Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add_Component (Gnattest_T : in out Test);
   procedure Test_Add_Component_19aeb6 (Gnattest_T : in out Test) renames Test_Add_Component;
--  id:2.2/19aeb62704f48800/Add_Component/1/0/
   procedure Test_Add_Component (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Add_Component (Policy         => Policy,
                     Component_File => "data/component_debug.xml");
      Add_Component (Policy         => Policy,
                     Component_File => "data/library_debug.xml");

      Muxml.Write (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "obj/policy_joined.xml");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/policy_joined.ref.xml",
               Filename2 => "obj/policy_joined.xml"),
              Message   => "Joined policy mismatch");
      Ada.Directories.Delete_File (Name => "obj/policy_joined.xml");

      begin
         Add_Component (Policy         => Policy,
                        Component_File => "data/invalid_component.xml");
         Assert (Condition => False,
                 Message   => "Exception expected (1)");

      exception
         when Muxml.Validation_Error => null;
      end;

      begin
         Add_Component (Policy         => Policy,
                        Component_File => "nonexistent");
         Assert (Condition => False,
                 Message   => "Exception expected (2)");

      exception
         when Muxml.XML_Input_Error => null;
      end;
--  begin read only
   end Test_Add_Component;
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
end Compjoin.Utils.Test_Data.Tests;
