--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Expanders.Utils.Test_Data.Tests is


--  begin read only
   procedure Test_Allocate (Gnattest_T : in out Test);
   procedure Test_Allocate_ceb92c (Gnattest_T : in out Test) renames Test_Allocate;
--  id:2.2/ceb92c0fcf2b5351/Allocate/1/0/
   procedure Test_Allocate (Gnattest_T : in out Test) is
   --  expanders-utils.ads:30:4:Allocate
--  end read only

      pragma Unreferenced (Gnattest_T);

      Alloc : Number_Allocator_Type (Range_Start => 32,
                                     Range_End   => 255);
      Num   : Natural;
   begin
      Allocate (Allocator => Alloc,
                Number    => Num);
      Assert (Condition => Num = 32,
              Message   => "Allocated number mismatch (1)");
      Assert (Condition => not Alloc.Numbers (32),
              Message   => "Number not marked as allocated");

      Alloc.Numbers (33) := False;
      Allocate (Allocator => Alloc,
                Number    => Num);
      Assert (Condition => Num = 34,
              Message   => "Allocated number mismatch (2)");

      begin
         Alloc.Numbers := (others => False);
         Allocate (Allocator => Alloc,
                   Number    => Num);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when No_Free_Number => null;
      end;
--  begin read only
   end Test_Allocate;
--  end read only


--  begin read only
   procedure Test_Reserve_Numbers (Gnattest_T : in out Test);
   procedure Test_Reserve_Numbers_a46fcf (Gnattest_T : in out Test) renames Test_Reserve_Numbers;
--  id:2.2/a46fcff52807a761/Reserve_Numbers/1/0/
   procedure Test_Reserve_Numbers (Gnattest_T : in out Test) is
   --  expanders-utils.ads:38:4:Reserve_Numbers
--  end read only

      pragma Unreferenced (Gnattest_T);

      subtype Reserved_Range is Natural range 23 .. 42;

      Policy   : Muxml.XML_Data_Type;
      Dom_Impl : DOM.Core.DOM_Implementation;
      Nodes    : DOM.Core.Node_List;
      Node     : DOM.Core.Node;
      Alloc    : Number_Allocator_Type (Range_Start => 0,
                                        Range_End   => 1024);
   begin
      Policy.Doc := DOM.Core.Create_Document (Implementation => Dom_Impl);

      for I in Reserved_Range loop
         Node := DOM.Core.Documents.Create_Element
           (Doc      => Policy.Doc,
            Tag_Name => "foo");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "number",
            Value => Ada.Strings.Fixed.Trim
              (Source => I'Img,
               Side   => Ada.Strings.Left));
         DOM.Core.Append_Node (List => Nodes,
                               N    => Node);
      end loop;

      Reserve_Numbers (Allocator => Alloc,
                       Nodes     => Nodes,
                       Attribute => "number");

      for I in Alloc.Numbers'Range loop
         if I in Reserved_Range then
            Assert (Condition => not Alloc.Numbers (I),
                    Message   => "Number not reserved");
         else
            Assert (Condition => Alloc.Numbers (I),
                    Message   => "Unreserved number allocated");
         end if;
      end loop;

      begin
         Reserve_Numbers (Allocator => Alloc,
                          Nodes     => Nodes,
                          Attribute => "nonexistent");
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Invalid_Attribute =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Node 'foo' has no valid number attribute 'nonexistent'",
                    Message   => "Exeption message mismatch");
      end;
--  begin read only
   end Test_Reserve_Numbers;
--  end read only

end Expanders.Utils.Test_Data.Tests;
