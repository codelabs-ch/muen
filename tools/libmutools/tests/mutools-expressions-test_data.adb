--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with McKae.XML.XPath.XIA;

package body Mutools.Expressions.Test_Data is

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

   procedure Initialize_Node_Access_Testing
      (Data        :        Muxml.XML_Data_Type;
       Node_Access : in out Access_Hashmaps_Type)
   is
      Config_And_Exprs : constant DOM.Core.Node_List
         := McKae.XML.XPath.XIA.XPath_Query
               (N     => Data.Doc,
                XPath =>  "/*/config/boolean | "
                   & "/*/config/integer | "
                   & "/*/config/string | "
                   & "/*/expressions/expression");
   begin
      Node_Access.Input.Clear;
      Node_Access.Output_Boolean.Clear;
      Node_Access.Output_Integer.Clear;
      Node_Access.Output_String.Clear;

      Initialize_Node_Access
         (Node_Access      => Node_Access,
          Config_And_Exprs => Config_And_Exprs);
   end Initialize_Node_Access_Testing;

end Mutools.Expressions.Test_Data;
