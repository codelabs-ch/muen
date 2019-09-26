--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.MSR.Test_Data.

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
package body Mucfgcheck.MSR.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Start_Smaller_End (Gnattest_T : in out Test);
   procedure Test_Start_Smaller_End_3ff191 (Gnattest_T : in out Test) renames Test_Start_Smaller_End;
--  id:2.2/3ff19145334275c7/Start_Smaller_End/1/0/
   procedure Test_Start_Smaller_End (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/vcpu/msrs/"
         & "msr[@end='16#0176#']",
         Name  => "end",
         Value => "16#0170#");

      begin
         Start_Smaller_End (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "MSR start 16#0174# larger than end 16#0170#"
                    & " (Subject 'linux')",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Start_Smaller_End;
--  end read only


--  begin read only
   procedure Test_Low_Or_High (Gnattest_T : in out Test);
   procedure Test_Low_Or_High_2e4f48 (Gnattest_T : in out Test) renames Test_Low_Or_High;
--  id:2.2/2e4f4877eabfbdff/Low_Or_High/1/0/
   procedure Test_Low_Or_High (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject/vcpu/msrs/"
         & "msr[@end='16#0176#']",
         Name  => "end",
         Value => "16#c000_0800#");

      begin
         Low_Or_High (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "MSR start 16#0174# and end 16#c000_0800# in different"
                    & " low/high range (Subject 'linux')",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Low_Or_High;
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
end Mucfgcheck.MSR.Test_Data.Tests;
