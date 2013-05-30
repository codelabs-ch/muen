with System;

package body Knl_States_Interface
is

   Descriptors : SK.Subject_State_Array (Skp.Subject_Id_Type);
   for Descriptors'Address use System'To_Address (16#1fe000#);

   -------------------------------------------------------------------------

   function Get_Subject_State
     (Id : Skp.Subject_Id_Type)
      return SK.Subject_State_Type
   is
      State : SK.Subject_State_Type;
   begin
      State := Descriptors (Id);
      return State;
   end Get_Subject_State;

   -------------------------------------------------------------------------

   procedure Set_Subject_State
     (Id    : Skp.Subject_Id_Type;
      State : SK.Subject_State_Type)
   is
   begin
      Descriptors (Id) := State;
   end Set_Subject_State;

end Knl_States_Interface;
