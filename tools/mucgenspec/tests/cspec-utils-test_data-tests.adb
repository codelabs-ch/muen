--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Cspec.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Cspec.Utils.Test_Data.Tests is


--  begin read only
   procedure Test_Is_Present (Gnattest_T : in out Test);
   procedure Test_Is_Present_db9984 (Gnattest_T : in out Test) renames Test_Is_Present;
--  id:2.2/db9984f99b04946e/Is_Present/1/0/
   procedure Test_Is_Present (Gnattest_T : in out Test) is
   --  cspec-utils.ads:29:4:Is_Present
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Assert (Condition => Is_Present
              (Policy    => Policy,
               Comp_Name => "vt"),
              Message   => "Component sm not found");
      Assert (Condition => not Is_Present
              (Policy    => Policy,
               Comp_Name => "nonexistent"),
              Message   => "Unexpected component");
--  begin read only
   end Test_Is_Present;
--  end read only


--  begin read only
   procedure Test_To_Memory_Str (Gnattest_T : in out Test);
   procedure Test_To_Memory_Str_ea699a (Gnattest_T : in out Test) renames Test_To_Memory_Str;
--  id:2.2/ea699a34dcb2416e/To_Memory_Str/1/0/
   procedure Test_To_Memory_Str (Gnattest_T : in out Test) is
   --  cspec-utils.ads:35:4:To_Memory_Str
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;

      Ref : constant String :=
        "   Input_Address    : constant := 16#f000#;"       & ASCII.LF
        & "   Input_Size       : constant := 16#2000#;"     & ASCII.LF
        & "   Input_Executable : constant Boolean := True;" & ASCII.LF
        & "   Input_Writable   : constant Boolean := False;";
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "memory");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "logical",
         Value => "input");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "virtualAddress",
         Value => "16#f000#");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "size",
         Value => "16#2000#");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "executable",
         Value => "true");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "writable",
         Value => "false");
      Assert (Condition => To_Memory_Str (Memory => Node) = Ref,
              Message   => "String mismatch");
--  begin read only
   end Test_To_Memory_Str;
--  end read only


--  begin read only
   procedure Test_To_Channel_Str (Gnattest_T : in out Test);
   procedure Test_To_Channel_Str_046b8f (Gnattest_T : in out Test) renames Test_To_Channel_Str;
--  id:2.2/046b8f08d89e579c/To_Channel_Str/1/0/
   procedure Test_To_Channel_Str (Gnattest_T : in out Test) is
   --  cspec-utils.ads:38:4:To_Channel_Str
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;

      Ref1 : constant String :=
        "   Input_Address : constant := 16#f000#;" & ASCII.LF
        & "   Input_Size    : constant := 16#2000#;" & ASCII.LF
        & "   Input_Kind    : constant Channel_Kind := Channel_Reader;";
      Ref2 : constant String := Ref1 & ASCII.LF
        & "   Input_Vector  : constant := 54;";
      Ref3 : constant String := Ref2 & ASCII.LF
        & "   Input_Event   : constant := 234;";
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "reader");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "logical",
         Value => "input");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "virtualAddress",
         Value => "16#f000#");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "size",
         Value => "16#2000#");

      Assert (Condition => To_Channel_Str (Channel => Node) = Ref1,
              Message   => "String mismatch (1)");

      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "vector",
         Value => "54");
      Assert (Condition => To_Channel_Str (Channel => Node) = Ref2,
              Message   => "String mismatch (2)");

      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "event",
         Value => "234");
      Assert (Condition => To_Channel_Str (Channel => Node) = Ref3,
              Message   => "String mismatch (3)");
--  begin read only
   end Test_To_Channel_Str;
--  end read only


--  begin read only
   procedure Test_Memory_Attrs_As_String (Gnattest_T : in out Test);
   procedure Test_Memory_Attrs_As_String_9abdd9 (Gnattest_T : in out Test) renames Test_Memory_Attrs_As_String;
--  id:2.2/9abdd97303e68ce6/Memory_Attrs_As_String/1/0/
   procedure Test_Memory_Attrs_As_String (Gnattest_T : in out Test) is
   --  cspec-utils.ads:45:4:Memory_Attrs_As_String
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;

      Name, Addr, Size : Unbounded_String;

      Ref_Name : constant String := "mem1";
      Ref_Addr : constant String := "16#1000#";
      Ref_Size : constant String := "16#3000#";
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "memory");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "logical",
         Value => Ref_Name);

      begin
         Memory_Attrs_As_String
           (Node            => Node,
            Logical_Name    => Name,
            Virtual_Address => Addr,
            Size            => Size);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Attribute_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Memory node does not provide expected attributes",
                    Message   => "Exception mismatch");
      end;

      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "virtualAddress",
         Value => Ref_Addr);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "size",
         Value => Ref_Size);

      Memory_Attrs_As_String
        (Node            => Node,
         Logical_Name    => Name,
         Virtual_Address => Addr,
         Size            => Size);
      Assert (Condition => To_String (Name) = Ref_Name,
              Message   => "Logical name mismatch");
      Assert (Condition => To_String (Addr) = Ref_Addr,
              Message   => "Virtual address mismatch");
      Assert (Condition => To_String (Size) = Ref_Size,
              Message   => "Size mismatch");
--  begin read only
   end Test_Memory_Attrs_As_String;
--  end read only


--  begin read only
   procedure Test_Memory_Perm_Attrs_As_String (Gnattest_T : in out Test);
   procedure Test_Memory_Perm_Attrs_As_String_7d468f (Gnattest_T : in out Test) renames Test_Memory_Perm_Attrs_As_String;
--  id:2.2/7d468f4cce634a46/Memory_Perm_Attrs_As_String/1/0/
   procedure Test_Memory_Perm_Attrs_As_String (Gnattest_T : in out Test) is
   --  cspec-utils.ads:52:4:Memory_Perm_Attrs_As_String
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;

      Executable, Writable : Unbounded_String;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "memory");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "executable",
         Value => "true");

      begin
         Memory_Perm_Attrs_As_String
           (Node       => Node,
            Executable => Executable,
            Writable   => Writable);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Attribute_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Memory node does not provide expected permission "
                    & "attributes",
                    Message   => "Exception mismatch");
      end;

      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "writable",
         Value => "false");

      Memory_Perm_Attrs_As_String
        (Node       => Node,
         Executable => Executable,
         Writable   => Writable);
      Assert (Condition => To_String (Executable) = "True",
              Message   => "Executable mismatch");
      Assert (Condition => To_String (Writable) = "False",
              Message   => "Writable mismatch");
--  begin read only
   end Test_Memory_Perm_Attrs_As_String;
--  end read only


--  begin read only
   procedure Test_Channel_Attrs_As_String (Gnattest_T : in out Test);
   procedure Test_Channel_Attrs_As_String_c33843 (Gnattest_T : in out Test) renames Test_Channel_Attrs_As_String;
--  id:2.2/c3384320b577cc0b/Channel_Attrs_As_String/1/0/
   procedure Test_Channel_Attrs_As_String (Gnattest_T : in out Test) is
   --  cspec-utils.ads:59:4:Channel_Attrs_As_String
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
      Node : DOM.Core.Node;

      Kind, Vector, Event : Unbounded_String;

      Ref_Kind   : constant String := "writer";
      Ref_Vector : constant String := "12";
      Ref_Event  : constant String := "245";
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "memory");

      begin
         Channel_Attrs_As_String
           (Node   => Node,
            Kind   => Kind,
            Vector => Vector,
            Event  => Event);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when Attribute_Error => null;
      end;

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "writer");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "vector",
         Value => Ref_Vector);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "event",
         Value => Ref_Event);

      Channel_Attrs_As_String
        (Node   => Node,
         Kind   => Kind,
         Vector => Vector,
         Event  => Event);
      Assert (Condition => To_String (Kind) = Ref_Kind,
              Message   => "Channel kind mismatch");
      Assert (Condition => To_String (Vector) = Ref_Vector,
              Message   => "Vector mismatch");
      Assert (Condition => To_String (Event) = Ref_Event,
              Message   => "Event mismatch");
--  begin read only
   end Test_Channel_Attrs_As_String;
--  end read only

end Cspec.Utils.Test_Data.Tests;