package Skp.Events
is

   type Target_Event_Range is range 0 .. 32;

   subtype Event_Range is Target_Event_Range range 0 .. 31;

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
      Target_Subject : Skp.Dst_Subject_Type;
      Target_Event   : Target_Event_Range;
      Handover       : Boolean;
      Send_IPI       : Boolean;
   end record;

   Null_Event : constant Event_Entry_Type := Event_Entry_Type'
     (Source_Action  => No_Action,
      Target_Subject => Skp.Invalid_Subject,
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
      return Skp.Vector_Range
   with
      Pre => Get_Kind (Event_Action) = Inject_Interrupt;

   function Get_Trap
     (Subject_ID : Skp.Subject_Id_Type;
      Trap_Nr    : Trap_Range)
      return Event_Entry_Type;

   function Get_Source_Event
     (Subject_ID : Skp.Subject_Id_Type;
      Event_Nr   : Event_Range)
      return Event_Entry_Type;

   function Get_Target_Event
     (Subject_ID : Skp.Subject_Id_Type;
      Event_Nr   : Event_Range)
      return Event_Action_Type;

private

   type Event_Action_Type is record
      Kind   : Target_Event_Action_Kind;
      Vector : Skp.Dst_Vector_Range;
   end record
     with Dynamic_Predicate =>
       (case Event_Action_Type.Kind is
           when Inject_Interrupt =>
             Event_Action_Type.Vector /= Skp.Invalid_Vector,
           when others           =>
             Event_Action_Type.Vector = Skp.Invalid_Vector);

   function Get_Kind
     (Event_Action : Event_Action_Type)
      return Target_Event_Action_Kind
   is (Event_Action.Kind);

   function Get_Vector
     (Event_Action : Event_Action_Type)
      return Skp.Vector_Range
   is (Skp.Vector_Range (Event_Action.Vector));

   Null_Event_Action : constant Event_Action_Type := Event_Action_Type'
     (Kind   => No_Action,
      Vector => Skp.Invalid_Vector);

end Skp.Events;
