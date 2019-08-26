--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Memhashes.Utils.Test_Data.

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
package body Memhashes.Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_To_Stream (Gnattest_T : in out Test);
   procedure Test_To_Stream_ef1605 (Gnattest_T : in out Test) renames Test_To_Stream;
--  id:2.2/ef16057605ed9e8d/To_Stream/1/0/
   procedure Test_To_Stream (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Ada.Streams.Stream_Element_Array;

      Impl         : DOM.Core.DOM_Implementation;
      Mem, Content : DOM.Core.Node;
      Policy       : Muxml.XML_Data_Type;
   begin
      Policy.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Mem := DOM.Core.Documents.Create_Element
        (Doc      => Policy.Doc,
         Tag_Name => "memory");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Mem,
         Name  => "size",
         Value => "24");

      Content := DOM.Core.Documents.Create_Element
        (Doc      => Policy.Doc,
         Tag_Name => "fill");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Content,
         Name  => "pattern",
         Value => "16#32#");
      Content := DOM.Core.Nodes.Append_Child
        (N         => Mem,
         New_Child => Content);

      Assert (Condition => To_Stream (Node => Mem) =
                Ada.Streams.Stream_Element_Array'
                  (1 .. 24 => 16#32#),
              Message   => "Fill content mismatch");

      Muxml.Utils.Remove_Child (Node       => Mem,
                                Child_Name => "fill");
      Content := DOM.Core.Documents.Create_Element
        (Doc      => Policy.Doc,
         Tag_Name => "file");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Content,
         Name  => "filename",
         Value => "testfile");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Content,
         Name  => "offset",
         Value => "none");
      Content := DOM.Core.Nodes.Append_Child
        (N         => Mem,
         New_Child => Content);

      declare
         Ref1 : constant Ada.Streams.Stream_Element_Array (1 .. 24)
           := (1      => 16#3a#,
               2      => 16#ae#,
               3      => 16#f3#,
               4      => 16#fb#,
               others => 0);
         Ref2 : constant Ada.Streams.Stream_Element_Array (1 .. 24)
           := (1      => 16#f3#,
               2      => 16#fb#,
               others => 0);
      begin
         Assert (Condition => To_Stream
                 (Node      => Mem,
                  Input_Dir => "data") = Ref1,
                 Message   => "File content mismatch (1)");

         DOM.Core.Elements.Set_Attribute
           (Elem  => Content,
            Name  => "offset",
            Value => "2");
         Assert (Condition => To_Stream
                 (Node      => Mem,
                  Input_Dir => "data") = Ref2,
                 Message   => "File content mismatch (2)");
      end;
--  begin read only
   end Test_To_Stream;
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
end Memhashes.Utils.Test_Data.Tests;
