--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Cspec.Utils.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Cspec.Utils.Test_Data.Tests is


--  begin read only
   procedure Test_Memory_Attrs_As_String (Gnattest_T : in out Test);
   procedure Test_Memory_Attrs_As_String_9abdd9 (Gnattest_T : in out Test) renames Test_Memory_Attrs_As_String;
--  id:2.2/9abdd97303e68ce6/Memory_Attrs_As_String/1/0/
   procedure Test_Memory_Attrs_As_String (Gnattest_T : in out Test) is
   --  cspec-utils.ads:31:4:Memory_Attrs_As_String
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
         when Attribute_Error => null;
      end;

      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "virtualAddress",
         Value => Ref_Addr);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "size",
         Value => Ref_Size);

      Memory_Attrs_As_String (Node            => Node,
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
   procedure Test_Channel_Attrs_As_String (Gnattest_T : in out Test);
   procedure Test_Channel_Attrs_As_String_c33843 (Gnattest_T : in out Test) renames Test_Channel_Attrs_As_String;
--  id:2.2/c3384320b577cc0b/Channel_Attrs_As_String/1/0/
   procedure Test_Channel_Attrs_As_String (Gnattest_T : in out Test) is
   --  cspec-utils.ads:39:4:Channel_Attrs_As_String
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
