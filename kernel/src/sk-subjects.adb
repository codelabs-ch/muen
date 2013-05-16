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

   procedure Set_Launched
     (Id    : Skp.Subject_Id_Type;
      Value : Boolean)
   is
   begin
      Descriptors (Id).Launched := Value;
   end Set_Launched;

   -------------------------------------------------------------------------

   procedure Set_RIP
     (Id    : Skp.Subject_Id_Type;
      Value : SK.Word64)
   is
   begin
      Descriptors (Id).RIP := Value;
   end Set_RIP;

   -------------------------------------------------------------------------

   procedure Set_State
     (Id    : Skp.Subject_Id_Type;
      State : SK.Subject_State_Type)
   is
      Vector : SK.Byte;
   begin
      Vector := Descriptors (Id).Pending_Event;
      Descriptors (Id) := State;
      Descriptors (Id).Pending_Event := Vector;
   end Set_State;

   -------------------------------------------------------------------------

   procedure Set_Pending_Event
     (Id     : Skp.Subject_Id_Type;
      Vector : SK.Byte)
   is
   begin
      Descriptors (Id).Pending_Event := Vector;
   end Set_Pending_Event;

begin
   Descriptors := Descriptor_Array'
     (others => SK.Subject_State_Type'
        (SK.Null_Subject_State));
end SK.Subjects;
