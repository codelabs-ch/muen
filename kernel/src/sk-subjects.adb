with System;

package body SK.Subjects
--# own
--#    State is Descriptors, Events;
is

   subtype Descriptor_Array is SK.Subject_State_Array (Skp.Subject_Id_Type);

   --  Descriptors used to manage subject states.
   --# accept Warning, 396, Descriptors, "Not an external variable";
   Descriptors : Descriptor_Array;
   for Descriptors'Address use System'To_Address (16#001fe000#);
   --# end accept;

   --  Subject events.
   type Events_Array is array (Skp.Subject_Id_Type) of SK.Byte;
   Events : Events_Array;

   -------------------------------------------------------------------------

   function Get_Pending_Event (Id : Skp.Subject_Id_Type) return SK.Byte
   --# global
   --#    Events;
   --# return
   --#    Events (Id);
   is
   begin
      return Events (Id);
   end Get_Pending_Event;

   -------------------------------------------------------------------------

   function Get_State (Id : Skp.Subject_Id_Type) return SK.Subject_State_Type
   --# global
   --#    Descriptors;
   --# return
   --#    Descriptors (Id);
   is
   begin
      return Descriptors (Id);
   end Get_State;

   -------------------------------------------------------------------------

   procedure Set_RIP
     (Id    : Skp.Subject_Id_Type;
      Value : SK.Word64)
   --# global
   --#    Descriptors;
   --# derives
   --#    Descriptors from *, Id, Value;
   --# post
   --#    Descriptors (Id).RIP = Value;
   is
   begin
      Descriptors (Id).RIP := Value;
   end Set_RIP;

   -------------------------------------------------------------------------

   procedure Set_RSP
     (Id    : Skp.Subject_Id_Type;
      Value : SK.Word64)
   --# global
   --#    Descriptors;
   --# derives
   --#    Descriptors from *, Id, Value;
   --# post
   --#    Descriptors (Id).RSP = Value;
   is
   begin
      Descriptors (Id).RSP := Value;
   end Set_RSP;

   -------------------------------------------------------------------------

   procedure Set_State
     (Id            : Skp.Subject_Id_Type;
      Subject_State : SK.Subject_State_Type)
   --# global
   --#    Descriptors;
   --# derives
   --#    Descriptors from *, Id, Subject_State;
   --# post
   --#    Descriptors (Id) = Subject_State;
   is
   begin
      Descriptors (Id) := Subject_State;
   end Set_State;

   -------------------------------------------------------------------------

   procedure Set_Pending_Event
     (Id     : Skp.Subject_Id_Type;
      Vector : SK.Byte)
   --# global
   --#    Events;
   --# derives
   --#    Events from *, Id, Vector;
   --# post
   --#    Events (Id) = Vector;
   is
   begin
      Events (Id) := Vector;
   end Set_Pending_Event;

begin
   Events      := Events_Array'(others => 0);
   Descriptors := Descriptor_Array'
     (others => SK.Subject_State_Type'
        (SK.Null_Subject_State));
end SK.Subjects;
