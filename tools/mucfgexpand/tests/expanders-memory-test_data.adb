--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with Expanders.Platform;

package body Expanders.Memory.Test_Data is

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

   procedure Add_Missing_Elems_Resolve_Aliases
     (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Add_Missing_Attributes (Data => Data);
      Platform.Resolve_Device_Aliases (Data => Data);
   end Add_Missing_Elems_Resolve_Aliases;

end Expanders.Memory.Test_Data;
