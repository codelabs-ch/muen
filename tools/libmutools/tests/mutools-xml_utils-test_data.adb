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

   procedure Append_MSR
     (Doc       : in out DOM.Core.Document;
      List      : in out DOM.Core.Node_List;
      MSR_Start :        String;
      MSR_End   :        String;
      Mode      :        String)
   is
      Node : DOM.Core.Node;
   begin
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Doc,
         Tag_Name => "msr");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "mode",
         Value => Mode);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "start",
         Value => MSR_Start);
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "end",
         Value => MSR_End);
      DOM.Core.Append_Node (List => List,
                            N    => Node);
   end Append_MSR;

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
