--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Paging.Layouts.Test_Data is

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

   procedure Write
     (Stream : in out Memory_Stream_Type;
      Item   :        Ada.Streams.Stream_Element_Array)
   is
      use Ada.Streams;

      End_Idx : constant Stream_Element_Offset
        := Stream.Write_Idx + (Item'Length - 1);
   begin
      if End_Idx > Stream.Buffer'Last then
         raise Constraint_Error with "Stream buffer too small for object, "
           & "increase size (offset" & End_Idx'Img & " requested, max is"
           & Stream.Buffer'Last'Img & ")";
      end if;

      Stream.Buffer (Stream.Write_Idx .. End_Idx) := Item;
      Stream.Write_Idx                            := End_Idx + 1;
   end Write;

end Paging.Layouts.Test_Data;
