with System;

package body SK.Subjects
is

   subtype Descriptor_Array is SK.Subject_State_Array (Skp.Subject_Id_Type);

   --  Descriptors used to manage subject states.
   --# accept Warning, 396, Descriptors, "Not an external variable";
   Descriptors : Descriptor_Array;
   for Descriptors'Address use System'To_Address (16#001fe000#);
   --# end accept;

   -------------------------------------------------------------------------

   function Get_State (Id : Skp.Subject_Id_Type) return SK.Subject_State_Type
   is
   begin
      return Descriptors (Id);
   end Get_State;

   -------------------------------------------------------------------------

   procedure Set_State
     (Id    : Skp.Subject_Id_Type;
      State : SK.Subject_State_Type)
   is
   begin
      Descriptors (Id) := State;
   end Set_State;

begin
   Descriptors := Descriptor_Array'
     (others => SK.Subject_State_Type'
        (Launched => False,
         Regs     => SK.CPU_Null_Regs));
end SK.Subjects;
