with System;

package body Dumper_Kernel_Iface
is

   Descriptors : SK.Subject_State_Array (Descriptors_Range);
   for Descriptors'Address use System'To_Address (16#5000#);

   -------------------------------------------------------------------------

   function Get_Subject_State
     (Id : Descriptors_Range)
      return SK.Subject_State_Type
   is
      State : SK.Subject_State_Type;
   begin
      State := Descriptors (Id);
      return State;
   end Get_Subject_State;

end Dumper_Kernel_Iface;
