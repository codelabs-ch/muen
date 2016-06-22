--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Merge.Expressions.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Merge.Expressions.Test_Data.Tests is


--  begin read only
   procedure Test_Int_Value (Gnattest_T : in out Test);
   procedure Test_Int_Value_1fbfca (Gnattest_T : in out Test) renames Test_Int_Value;
--  id:2.2/1fbfcac9d59ea29e/Int_Value/1/0/
   procedure Test_Int_Value (Gnattest_T : in out Test) is
   --  merge-expressions.ads:28:4:Int_Value
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Invalid_Integer
      is
         Impl : DOM.Core.DOM_Implementation;
         Data : Muxml.XML_Data_Type;
         Node : DOM.Core.Node;
      begin
         Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

         Node := DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "foobar");
         Muxml.Utils.Append_Child
           (Node      => Data.Doc,
            New_Child => Node);

         declare
            Dummy : Integer;
         begin
            Dummy := Int_Value (Policy => Data,
                                Node   => Node);
            Assert (Condition => False,
                    Message   => "Exception expected");

         exception
            when E : Invalid_Expression =>
               Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                       = "Invalid integer type 'foobar'",
                       Message   => "Exception message mismatch");
         end;
      end Invalid_Integer;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         Data : Muxml.XML_Data_Type;
      begin
         Muxml.Parse
           (Data => Data,
            Kind => Muxml.None,
            File => "data/test_policy.xml");

         Assert (Condition => 4 = Int_Value
                 (Policy => Data,
                  Node   => Muxml.Utils.Get_Element
                    (Doc   => Data.Doc,
                     XPath => "/system/expressions/expression"
                     & "[@name='session2_enabled']/gt/variable")),
                 Message   => "Integer value mismatch (1)");
         Assert (Condition => 1 = Int_Value
                 (Policy => Data,
                  Node   => Muxml.Utils.Get_Element
                    (Doc   => Data.Doc,
                     XPath => "/system/expressions/expression"
                     & "[@name='session2_enabled']/gt/integer")),
                 Message   => "Integer value mismatch (2)");
      end Positive_Test;
   begin
      Positive_Test;
      Invalid_Integer;
--  begin read only
   end Test_Int_Value;
--  end read only

end Merge.Expressions.Test_Data.Tests;