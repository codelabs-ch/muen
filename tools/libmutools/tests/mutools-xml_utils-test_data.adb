--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Mutools.XML_Utils.Test_Data is

   -------------------------------------------------------------------------

   procedure Set_Up (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Set_Up;

   -------------------------------------------------------------------------

   procedure Tear_Down (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Tear_Down;

   -------------------------------------------------------------------------

   function Create_Mem_Node
     (Doc     : DOM.Core.Document;
      Name    : String;
      Address : String;
      Size    : String)
      return DOM.Core.Node
   is
   begin
      return Node : DOM.Core.Node do
         Node := DOM.Core.Documents.Create_Element
           (Doc      => Doc,
            Tag_Name => "memory");
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "name",
            Value => Name);
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "address",
            Value => Address);
         DOM.Core.Elements.Set_Attribute
           (Elem  => Node,
            Name  => "size",
            Value => Size);
      end return;
   end Create_Mem_Node;

end Mutools.XML_Utils.Test_Data;
