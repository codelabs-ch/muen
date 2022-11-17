--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Muxml.Utils.Test_Data is

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

   function Match_Name (Left, Right : DOM.Core.Node) return Boolean
   is
      Left_Name : constant String  := DOM.Core.Elements.Get_Attribute
        (Elem => Left,
         Name => "name");
      Right_Name : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Right,
         Name => "name");
   begin
      return Left_Name'Length > 0
        and then Right_Name'Length > 0
        and then Left_Name = Right_Name;
   end Match_Name;

end Muxml.Utils.Test_Data;
