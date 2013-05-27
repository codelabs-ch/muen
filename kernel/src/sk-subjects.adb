with System;

package body SK.Subjects
--# own
--#    State is Descriptors;
is

   subtype Descriptor_Array is SK.Subject_State_Array (Skp.Subject_Id_Type);

   --  Descriptors used to manage subject states.
   --# accept Warning, 396, Descriptors, "Not an external variable";
   Descriptors : Descriptor_Array;
   for Descriptors'Address use System'To_Address (16#001fe000#);
   --# end accept;

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
   --#    Descriptors (Id) =  Descriptors (Id)
   --#      [Launched           => Subject_State.Launched;
   --#       Regs               => Subject_State.Regs;
   --#       Exit_Reason        => Subject_State.Exit_Reason;
   --#       Exit_Qualification => Subject_State.Exit_Qualification;
   --#       Pending_Event      => Descriptors~ (Id).Pending_Event;
   --#       Interrupt_Info     => Subject_State.Interrupt_Info;
   --#       Instruction_Len    => Subject_State.Instruction_Len;
   --#       RIP                => Subject_State.RIP;
   --#       CS                 => Subject_State.CS;
   --#       RSP                => Subject_State.RSP;
   --#       SS                 => Subject_State.SS;
   --#       CR0                => Subject_State.CR0;
   --#       CR3                => Subject_State.CR3;
   --#       CR4                => Subject_State.CR4;
   --#       RFLAGS             => Subject_State.RFLAGS];
   is
      Vector : SK.Byte;
   begin
      Vector := Descriptors (Id).Pending_Event;
      Descriptors (Id) := Subject_State;
      Descriptors (Id).Pending_Event := Vector;
   end Set_State;

   -------------------------------------------------------------------------

   procedure Set_Pending_Event
     (Id     : Skp.Subject_Id_Type;
      Vector : SK.Byte)
   --# global
   --#    Descriptors;
   --# derives
   --#    Descriptors from *, Id, Vector;
   --# post
   --#    Descriptors (Id).Pending_Event = Vector;
   is
   begin
      Descriptors (Id).Pending_Event := Vector;
   end Set_Pending_Event;

begin
   Descriptors := Descriptor_Array'
     (others => SK.Subject_State_Type'
        (SK.Null_Subject_State));
end SK.Subjects;
