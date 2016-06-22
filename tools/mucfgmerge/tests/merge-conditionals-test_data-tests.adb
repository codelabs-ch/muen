--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Merge.Conditionals.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Merge.Conditionals.Test_Data.Tests is


--  begin read only
   procedure Test_Evaluate (Gnattest_T : in out Test);
   procedure Test_Evaluate_6dd7a2 (Gnattest_T : in out Test) renames Test_Evaluate;
--  id:2.2/6dd7a238e46c8db9/Evaluate/1/0/
   procedure Test_Evaluate (Gnattest_T : in out Test) is
   --  merge-conditionals.ads:29:4:Evaluate
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type DOM.Core.Node;

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse
        (Data => Data,
         Kind => Muxml.None,
         File => "data/test_policy.xml");

      Evaluate (Policy => Data,
                Parent => Muxml.Utils.Get_Element
                  (Doc   => Data.Doc,
                   XPath => "/system/memory"));
      Assert (Condition => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/system/memory/memory[@name='extra_mem']") /= null,
              Message   => "Conditional evaluation failed (1)");

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/subjects/subject[@name='lnx']/memory/if"
         & "[@variable='iommu_enabled']",
         Name  => "value",
         Value => "false");

      Evaluate (Policy => Data,
                Parent => Muxml.Utils.Get_Element
                  (Doc   => Data.Doc,
                   XPath => "/system/subjects/subject[@name='lnx']/memory"));
      Assert (Condition => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/system/subjects/subject[@name='lnx']/"
               & "memory/memory[@name='extra_mem']") = null,
              Message   => "Conditional evaluation failed (2)");
--  begin read only
   end Test_Evaluate;
--  end read only

end Merge.Conditionals.Test_Data.Tests;