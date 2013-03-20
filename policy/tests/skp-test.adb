package body Skp.Test
is

   -------------------------------------------------------------------------

   function First (Layout : Memory_Layout_Type) return Memory_Region_Type
   is
   begin
      return Layout.Regions.First_Element;
   end First;

   -------------------------------------------------------------------------

   function First (Policy : Policy_Type) return Subject_Type
   is
   begin
      return Policy.Subjects.First_Element;
   end First;

   -------------------------------------------------------------------------

   function First (Ports : IO_Ports_Type) return IO_Port_Range
   is
   begin
      return Ports.Ranges.First_Element;
   end First;

   -------------------------------------------------------------------------

   function Last (Layout : Memory_Layout_Type) return Memory_Region_Type
   is
   begin
      return Layout.Regions.Last_Element;
   end Last;

end Skp.Test;
