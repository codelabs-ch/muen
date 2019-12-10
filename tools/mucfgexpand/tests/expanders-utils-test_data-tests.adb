--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Utils.Test_Data.

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
package body Expanders.Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Allocate (Gnattest_T : in out Test);
   procedure Test_Allocate_ceb92c (Gnattest_T : in out Test) renames Test_Allocate;
--  id:2.2/ceb92c0fcf2b5351/Allocate/1/0/
   procedure Test_Allocate (Gnattest_T : in out Test) is
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
   procedure Test_Allocate_Range (Gnattest_T : in out Test);
   procedure Test_Allocate_Range_3f3623 (Gnattest_T : in out Test) renames Test_Allocate_Range;
--  id:2.2/3f3623cda56b09d3/Allocate_Range/1/0/
   procedure Test_Allocate_Range (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      procedure Alloc_Range
      is
         Alloc : Number_Allocator_Type (Range_Start => 1,
                                        Range_End   => 30);
         Range_Start, Range_End : Natural;
      begin
         Allocate_Range (Allocator   => Alloc,
                         Range_Size  => 10,
                         Range_Start => Range_Start,
                         Range_End   => Range_End);
         Assert (Condition => Range_Start = 1,
                 Message   => "Range start mismatch (1)");
         Assert (Condition => Range_End = 10,
                 Message   => "Range end mismatch (1)");

         Alloc.Numbers (16) := False;

         Allocate_Range (Allocator   => Alloc,
                         Range_Size  => 6,
                         Range_Start => Range_Start,
                         Range_End   => Range_End);
         Assert (Condition => Range_Start = 17,
                 Message   => "Range start mismatch (2)");
         Assert (Condition => Range_End = 22,
                 Message   => "Range end mismatch (2)");

         Allocate_Range (Allocator   => Alloc,
                         Range_Size  => 4,
                         Range_Start => Range_Start,
                         Range_End   => Range_End);
         Assert (Condition => Range_Start = 11,
                 Message   => "Range start mismatch (3)");
         Assert (Condition => Range_End = 14,
                 Message   => "Range end mismatch (3)");

         Allocate_Range (Allocator   => Alloc,
                         Range_Size  => 1,
                         Range_Start => Range_Start,
                         Range_End   => Range_End);
         Assert (Condition => Range_Start = 15,
                 Message   => "Range start mismatch (4)");
         Assert (Condition => Range_End = 15,
                 Message   => "Range end mismatch (4)");

         Allocate_Range (Allocator   => Alloc,
                         Range_Size  => 8,
                         Range_Start => Range_Start,
                         Range_End   => Range_End);
         Assert (Condition => Range_Start = 23,
                 Message   => "Range start mismatch (5)");
         Assert (Condition => Range_End = 30,
                 Message   => "Range end mismatch (5)");
      end Alloc_Range;

      ----------------------------------------------------------------------

      procedure Size_Too_Large
      is
         Alloc : Number_Allocator_Type (Range_Start => 1,
                                        Range_End   => 20);
         Range_Start, Range_End : Natural;
      begin
         Allocate_Range (Allocator   => Alloc,
                         Range_Size  => 21,
                         Range_Start => Range_Start,
                         Range_End   => Range_End);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when No_Free_Number => null;
      end Size_Too_Large;
   begin
      Alloc_Range;
      Size_Too_Large;
--  begin read only
   end Test_Allocate_Range;
--  end read only


--  begin read only
   procedure Test_Reserve_Number (Gnattest_T : in out Test);
   procedure Test_Reserve_Number_29180f (Gnattest_T : in out Test) renames Test_Reserve_Number;
--  id:2.2/29180f23996359d7/Reserve_Number/1/0/
   procedure Test_Reserve_Number (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      subtype Reserve_Range is Natural range 10 .. 64;

      Alloc : Number_Allocator_Type (Range_Start => 42,
                                     Range_End   => 97);
   begin
      for I in Reserve_Range loop
         Reserve_Number (Allocator => Alloc,
                         Number    => I);
      end loop;

      for I in Natural range Alloc.Numbers'Range loop
         if I in Reserve_Range then
            Assert (Condition => not Alloc.Numbers (I),
                    Message   => "Number" & I'Img & " not reserved");
         else
            Assert (Condition => Alloc.Numbers (I),
                    Message   => "Number" & I'Img & " reserved");
         end if;
      end loop;
--  begin read only
   end Test_Reserve_Number;
--  end read only


--  begin read only
   procedure Test_Reserve_Numbers (Gnattest_T : in out Test);
   procedure Test_Reserve_Numbers_a46fcf (Gnattest_T : in out Test) renames Test_Reserve_Numbers;
--  id:2.2/a46fcff52807a761/Reserve_Numbers/1/0/
   procedure Test_Reserve_Numbers (Gnattest_T : in out Test) is
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
                    Message   => "Exception message mismatch");
      end;

      --  Reserve all numbers.

      for I in Alloc.Range_Start .. Alloc.Range_End loop
         if I not in Reserved_Range then
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
         end if;
      end loop;

      Reserve_Numbers (Allocator => Alloc,
                       Nodes     => Nodes,
                       Attribute => "number");
      declare
         Dummy : Natural;
      begin
         Utils.Allocate (Allocator => Alloc,
                         Number    => Dummy);
         Assert (Condition => False,
                 Message   => "Exception expected (No free number)");

      exception
         when No_Free_Number => null;
      end;
--  begin read only
   end Test_Reserve_Numbers;
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
end Expanders.Utils.Test_Data.Tests;
