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
with McKae.XML.XPath.XIA;
with Muxml.Utils;
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
   procedure Test_SHA256_Digest (Gnattest_T : in out Test);
   procedure Test_SHA256_Digest_be9005 (Gnattest_T : in out Test) renames Test_SHA256_Digest;
--  id:2.2/be900524d3954a67/SHA256_Digest/1/0/
   procedure Test_SHA256_Digest (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Nodes  : DOM.Core.Node_List;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy_hashes.xml");
      Nodes := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/memory/memory/*[self::fill or self::file]/..");
      Assert (Condition => DOM.Core.Nodes.Length (List => Nodes) > 0,
              Message   => "No content regions in reference policy");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Cur_Mem : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Nodes,
               Index => I);
            Mem_Name : constant String
              := DOM.Core.Elements.Get_Attribute (Elem => Cur_Mem,
                                                  Name => "name");
            Ref_Hash : constant String
              := Muxml.Utils.Get_Attribute (Doc   => Cur_Mem,
                                            XPath => "hash",
                                            Name  => "value");
         begin
            if Ref_Hash'Length > 0 and Ref_Hash /= "none" then
               Assert (Condition => SHA256_Digest
                       (Node      => Cur_Mem,
                        Input_Dir => "data") = Ref_Hash,
                       Message   => "Hash mismatch of memory region '"
                       & Mem_Name & "'");
            end if;
         end;
      end loop;
--  begin read only
   end Test_SHA256_Digest;
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
