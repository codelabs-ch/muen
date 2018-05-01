package Skp.Events
is

   Event_Bits : constant := 6;
   Event_Mask : constant := 2 ** Event_Bits - 1;

   type Target_Event_Range is range 0 .. 2 ** Event_Bits;

   subtype Event_Range is Target_Event_Range range
     Target_Event_Range'First .. Target_Event_Range'Last - 1;

   Invalid_Target_Event : constant Target_Event_Range
     := Target_Event_Range'Last;

   type Trap_Range is range 0 .. 59;

   type Event_Action_Kind is
     (System_Reboot,
      System_Poweroff,
      No_Action,
      Inject_Interrupt,
      Reset);

   subtype Source_Event_Action_Kind is Event_Action_Kind range
     System_Reboot .. No_Action;

   subtype Target_Event_Action_Kind is Event_Action_Kind range
     No_Action .. Reset;

   type Event_Entry_Type is record
      Source_Action  : Source_Event_Action_Kind;
      Target_Subject : Dst_Subject_Type;
      Target_Event   : Target_Event_Range;
      Handover       : Boolean;
      Send_IPI       : Boolean;
   end record;

   Null_Event : constant Event_Entry_Type := Event_Entry_Type'
     (Source_Action  => No_Action,
      Target_Subject => Invalid_Subject,
      Target_Event   => Invalid_Target_Event,
      Handover       => False,
      Send_IPI       => False);

   type Event_Action_Type is private;

   Null_Event_Action : constant Event_Action_Type;

   function Get_Kind
     (Event_Action : Event_Action_Type)
      return Target_Event_Action_Kind;

   function Get_Vector
     (Event_Action : Event_Action_Type)
      return Vector_Range
   with
      Pre => Get_Kind (Event_Action) = Inject_Interrupt;

   function Get_Trap
     (Subject_ID : Global_Subject_ID_Type;
      Trap_Nr    : Trap_Range)
      return Event_Entry_Type;

   function Get_Source_Event
     (Subject_ID : Global_Subject_ID_Type;
      Event_Nr   : Event_Range)
      return Event_Entry_Type;

   function Get_Target_Event
     (Subject_ID : Global_Subject_ID_Type;
      Event_Nr   : Event_Range)
      return Event_Action_Type;

private

   type Event_Action_Type is record
      Kind   : Target_Event_Action_Kind;
      Vector : Dst_Vector_Range;
   end record
     with Dynamic_Predicate =>
       (case Event_Action_Type.Kind is
           when Inject_Interrupt =>
             Event_Action_Type.Vector /= Invalid_Vector,
           when others           =>
             Event_Action_Type.Vector = Invalid_Vector);

   function Get_Kind
     (Event_Action : Event_Action_Type)
      return Target_Event_Action_Kind
   is (Event_Action.Kind);

   function Get_Vector
     (Event_Action : Event_Action_Type)
      return Vector_Range
   is (Vector_Range (Event_Action.Vector));

   Null_Event_Action : constant Event_Action_Type := Event_Action_Type'
     (Kind   => No_Action,
      Vector => Invalid_Vector);

end Skp.Events;
