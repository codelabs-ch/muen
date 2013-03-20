package body Skp.Test
is

   -------------------------------------------------------------------------

   function First (Layout : Memory_Layout_Type) return Memory_Region_Type
   is
   begin
      return Layout.Regions.First_Element;
   end First;

end Skp.Test;
