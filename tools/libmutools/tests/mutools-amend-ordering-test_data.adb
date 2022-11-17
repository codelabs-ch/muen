--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with Muxml.system_src_schema;

package body Mutools.Amend.Ordering.Test_Data is

   procedure Set_Up (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      -- reinitialize Order_Info with correct schema
      Clear_Order_Info;
      Init_Order_Information (Schema_XML_Data => Muxml.system_src_schema.Data);
   end Tear_Down;

   procedure Clear_Order_Info
   is
   begin
      Order_Info.Type_To_Children.Clear;
      Order_Info.Name_To_Type.Clear;
   end Clear_Order_Info;


end Mutools.Amend.Ordering.Test_Data;
