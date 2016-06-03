--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Cspec.Utils.Test_Data is

   procedure Set_Up (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Tear_Down;

   function Create_Memory_Node
     (Data       : in out Muxml.XML_Data_Type;
      Logical    :        String;
      Address    :        String;
      Size       :        String;
      Executable :        String;
      Writable   :        String)
      return DOM.Core.Node
   is
      Impl : DOM.Core.DOM_Implementation;
      Node : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "memory");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "logical",
         Value => Logical);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "virtualAddress",
         Value => Address);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "size",
         Value => Size);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "executable",
         Value => Executable);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "writable",
         Value => Writable);

      return Node;
   end Create_Memory_Node;

end Cspec.Utils.Test_Data;
