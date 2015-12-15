--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Platform.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mucfgcheck.Platform.Test_Data.Tests is


--  begin read only
   procedure Test_Physical_Device_References (Gnattest_T : in out Test);
   procedure Test_Physical_Device_References_b4cc94 (Gnattest_T : in out Test) renames Test_Physical_Device_References;
--  id:2.2/b4cc947cfd4d6ff0/Physical_Device_References/1/0/
   procedure Test_Physical_Device_References (Gnattest_T : in out Test) is
   --  mucfgcheck-platform.ads:25:4:Physical_Device_References
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive tests, must no raise an exception.

      Physical_Device_References (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/platform/mappings/aliases/alias[@name='nic']",
         Name  => "physical",
         Value => "nonexistent");

      begin
         Physical_Device_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Physical device 'nonexistent' referenced by device "
                    & "alias 'nic' not found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Physical_Device_References;
--  end read only

end Mucfgcheck.Platform.Test_Data.Tests;
