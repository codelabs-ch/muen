with System;

with SK.KC;
with SK.Locks;

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
   type Subject_Events_Length is range 0 .. 32;
   subtype Subject_Event_Index is Subject_Events_Length range 1 .. 32;
   type Subject_Event_Array is array (Subject_Event_Index) of SK.Byte;

   type Subject_Events_Type is record
      Length : Subject_Events_Length;
      Data   : Subject_Event_Array;
   end record;

   type Events_Array is array (Skp.Subject_Id_Type) of Subject_Events_Type;

   Events : Events_Array;

   -------------------------------------------------------------------------

   procedure Get_Pending_Event
     (Id    :     Skp.Subject_Id_Type;
      Event : out SK.Byte)
   --# global
   --#    in out Events;
   --#    in out Locks.State;
   --# derives
   --#    Events      from *, Id      &
   --#    Event       from Events, Id &
   --#    Locks.State from *;
   --# post
   --#    (Events~ (Id).Length not in Subject_Event_Index ->
   --#       (Event = 0 and Events = Events~)) and
   --#    (Events~ (Id).Length in     Subject_Event_Index ->
   --#       (Event = Events (Id).Data (Events~ (Id).Length) and
   --#        Events (Id).Length = Events~ (Id).Length - 1));
   is
   begin
      Event := 0;
      Locks.Spin_Lock;
      if Events (Id).Length in Subject_Event_Index then
         Event := Events (Id).Data (Events (Id).Length);
         Events (Id).Length := Events (Id).Length - 1;
      end if;
      Locks.Unlock;
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

   function Has_Pending_Events (Id : Skp.Subject_Id_Type) return Boolean
   --# global
   --#    Events;
   --# return
   --#    Events (Id).Length > Subject_Events_Length'First;
   is
   begin
      return Events (Id).Length > Subject_Events_Length'First;
   end Has_Pending_Events;

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
   --#    in out Events;
   --#    in out Locks.State;
   --# derives
   --#    Events      from *, Id, Vector &
   --#    Locks.State from *;
   --# post
   --#    (Events~ (Id).Length < Subject_Event_Index'Last ->
   --#       (Events (Id).Length = Events~ (Id).Length + 1 and
   --#        Events (Id).Data (Events (Id).Length) = Vector)) and
   --#    (Events~ (Id).Length = Subject_Event_Index'Last -> Events = Events~);
   is
      Length : Subject_Events_Length;
   begin
      Locks.Spin_Lock;
      Length := Events (Id).Length;
      if Length < Subject_Event_Index'Last then
         Events (Id).Length := Events (Id).Length + 1;
         Events (Id).Data (Events (Id).Length) := Vector;
      end if;
      Locks.Unlock;
      pragma Debug (Length = Subject_Event_Index'Last,
                    KC.Put_String (Item => "Subject "));
      pragma Debug (Length = Subject_Event_Index'Last,
                    KC.Put_Byte (Item => SK.Byte (Id)));
      pragma Debug (Length = Subject_Event_Index'Last,
                    KC.Put_Line (Item => " - Event queue overflow"));
   end Set_Pending_Event;

begin
   Events      := Events_Array'
     (others => Subject_Events_Type'
        (Length => Subject_Events_Length'First,
         Data   => Subject_Event_Array'(others => 0)));
   Descriptors := Descriptor_Array'
     (others => SK.Subject_State_Type'
        (SK.Null_Subject_State));
end SK.Subjects;
