--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Mucfgcheck.Test_Data is

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

   -------------------------------------------------------------------------

   function Match_Name (Left, Right : DOM.Core.Node) return Boolean
   is
      Left_Name : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Left,
         Name => "name");
      Right_Name : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Right,
         Name => "name");
   begin
      return Left_Name = Right_Name;
   end Match_Name;

end Mucfgcheck.Test_Data;
