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
   procedure Test_Expand (Gnattest_T : in out Test);
   procedure Test_Expand_150aa9 (Gnattest_T : in out Test) renames Test_Expand;
--  id:2.2/150aa91f5cdabaeb/Expand/1/0/
   procedure Test_Expand (Gnattest_T : in out Test) is
   --  merge-conditionals.ads:27:4:Expand
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure No_Conditionals
      is
         Output : constant String := "obj/config_no_conditionals.xml";
         Data   : Muxml.XML_Data_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy.xml");

         Muxml.Utils.Remove_Elements (Doc   => Data.Doc,
                                      XPath => "//if");

         Expand (Policy => Data);

         Muxml.Write (Data => Data,
                      Kind => Muxml.None,
                      File => Output);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/config_no_conditionals.xml",
                  Filename2 => Output),
                 Message   => "Policy mismatch: " & Output);

         Ada.Directories.Delete_File (Name => Output);
      end No_Conditionals;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Output : constant String := "obj/config_conditionals.xml";
         Data   : Muxml.XML_Data_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy.xml");

         Expand (Policy => Data);

         Muxml.Write (Data => Data,
                      Kind => Muxml.None,
                      File => Output);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/config_conditionals.xml",
                  Filename2 => Output),
                 Message   => "Policy mismatch: " & Output);

         Ada.Directories.Delete_File (Name => Output);
      end Positive_Test;
   begin
      Positive_Test;
      No_Conditionals;
--  begin read only
   end Test_Expand;
--  end read only


--  begin read only
   procedure Test_Evaluate (Gnattest_T : in out Test);
   procedure Test_Evaluate_6dd7a2 (Gnattest_T : in out Test) renames Test_Evaluate;
--  id:2.2/6dd7a238e46c8db9/Evaluate/1/0/
   procedure Test_Evaluate (Gnattest_T : in out Test) is
   --  merge-conditionals.ads:32:4:Evaluate
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

      Evaluate (Policy => Data,
                Parent => Muxml.Utils.Get_Element
                  (Doc   => Data.Doc,
                   XPath => "/system/subjects/subject[@name='lnx']"));
      Assert (Condition => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/system/subjects/subject[@name='lnx']/"
               & "devices/device[@logical='usb']") /= null,
              Message   => "Conditional evaluation failed (3)");
      Assert (Condition => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/system/subjects/subject[@name='lnx']/"
               & "devices/device[@logical='usb']/ioPort") = null,
              Message   => "Conditional evaluation failed (4)");
      Assert (Condition => Muxml.Utils.Get_Element
              (Doc   => Data.Doc,
               XPath => "/system/subjects/subject[@name='lnx']/"
               & "devices/device[@logical='nic']") = null,
              Message   => "Conditional evaluation failed (5)");
--  begin read only
   end Test_Evaluate;
--  end read only

end Merge.Conditionals.Test_Data.Tests;
