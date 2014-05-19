--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Kernel.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mucfgcheck.Kernel.Test_Data.Tests is


--  begin read only
   procedure Test_CPU_Store_Address_Equality (Gnattest_T : in out Test);
   procedure Test_CPU_Store_Address_Equality_d15328 (Gnattest_T : in out Test) renames Test_CPU_Store_Address_Equality;
--  id:2.2/d1532861b18cecda/CPU_Store_Address_Equality/1/0/
   procedure Test_CPU_Store_Address_Equality (Gnattest_T : in out Test) is
   --  mucfgcheck-kernel.ads:25:4:CPU_Store_Address_Equality
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory"
         & "[@physical='kernel_store_1']",
         Name  => "virtualAddress",
         Value => "16#0021_0000#");

      begin
         CPU_Store_Address_Equality (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'virtualAddress => 16#0021_0000#' of "
                    & "'kernel_store_1' CPU Store memory element differs",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_CPU_Store_Address_Equality;
--  end read only


--  begin read only
   procedure Test_Stack_Address_Equality (Gnattest_T : in out Test);
   procedure Test_Stack_Address_Equality_61fb48 (Gnattest_T : in out Test) renames Test_Stack_Address_Equality;
--  id:2.2/61fb4824388cbd39/Stack_Address_Equality/1/0/
   procedure Test_Stack_Address_Equality (Gnattest_T : in out Test) is
   --  mucfgcheck-kernel.ads:28:4:Stack_Address_Equality
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/kernel/memory/cpu/memory"
         & "[@physical='kernel_stack_1']",
         Name  => "virtualAddress",
         Value => "16#0031_0000#");

      begin
         Stack_Address_Equality (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Attribute 'virtualAddress => 16#0031_0000#' of "
                    & "'kernel_stack_1' kernel stack memory element differs",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Stack_Address_Equality;
--  end read only

end Mucfgcheck.Kernel.Test_Data.Tests;
