with Skp.Interrupts;

with SK;

--D @Interface
--D This package contains subject source/target event as well as trap
--D definitions as specified by the system policy.
package Skp.Events
is

   use type SK.Byte;

   Event_Bits : constant := __event_bits__;
   Event_Mask : constant := 2 ** Event_Bits - 1;

   type Target_Event_Range is range 0 .. 2 ** Event_Bits;

   subtype Event_Range is Target_Event_Range range
     Target_Event_Range'First .. Target_Event_Range'Last - 1;

   Invalid_Target_Event : constant Target_Event_Range
     := Target_Event_Range'Last;

   type Trap_Range is range 0 .. __max_trap_id__;

__event_kind_types__
   type Source_Event_Type is record
      Source_Action  : Source_Event_Action_Kind;
      Target_Subject : Dst_Subject_Type;
      Target_Event   : Target_Event_Range;
      Handover       : Boolean;
      Send_IPI       : Boolean;
      IRQ_Number     : SK.Byte;
   end record
     with Dynamic_Predicate =>
       (case Source_Event_Type.Source_Action is
          when Unmask_Irq =>
            Source_Event_Type.IRQ_Number >= SK.Byte
          (Interrupts.RTE_Index_Type'First) and
            Source_Event_Type.IRQ_Number <= SK.Byte
          (Interrupts.RTE_Index_Type'Last),
          when others           =>
            Source_Event_Type.IRQ_Number = 0);

   Null_Source_Event : constant Source_Event_Type := Source_Event_Type'
     (Source_Action  => No_Action,
      Target_Subject => Invalid_Subject,
      Target_Event   => Invalid_Target_Event,
      Handover       => False,
      Send_IPI       => False,
      IRQ_Number     => 0);

   subtype Trap_Event_Type is Source_Event_Type with
     Dynamic_Predicate =>
       (if Trap_Event_Type.Source_Action = No_Action then
          Trap_Event_Type.Target_Subject /= Skp.Invalid_Subject);

   Invalid_Trap_Event : constant Trap_Event_Type := Trap_Event_Type'
     (Source_Action  => System_Panic,
      Target_Subject => Invalid_Subject,
      Target_Event   => Invalid_Target_Event,
      Handover       => False,
      Send_IPI       => False,
      IRQ_Number     => 0);

   type Target_Event_Type is private;

   Null_Target_Event : constant Target_Event_Type;

   function Get_Kind
     (Target_Event : Target_Event_Type)
      return Target_Event_Action_Kind;

   function Get_Vector
     (Target_Event : Target_Event_Type)
      return Vector_Range
   with
      Pre => Get_Kind (Target_Event) = Inject_Interrupt;

   function Get_Trap
     (Subject_ID : Global_Subject_ID_Type;
      Trap_Nr    : Trap_Range)
      return Trap_Event_Type;

   function Get_Source_Event
     (Subject_ID : Global_Subject_ID_Type;
      Event_Nr   : Event_Range)
      return Source_Event_Type;

   function Get_Target_Event
     (Subject_ID : Global_Subject_ID_Type;
      Event_Nr   : Event_Range)
      return Target_Event_Type;

private

   type Target_Event_Type is record
      Kind   : Target_Event_Action_Kind;
      Vector : Dst_Vector_Range;
   end record
     with Dynamic_Predicate =>
       (case Target_Event_Type.Kind is
           when Inject_Interrupt =>
             Target_Event_Type.Vector /= Invalid_Vector,
           when others           =>
             Target_Event_Type.Vector = Invalid_Vector);

   function Get_Kind
     (Target_Event : Target_Event_Type)
      return Target_Event_Action_Kind
   is (Target_Event.Kind);

   function Get_Vector
     (Target_Event : Target_Event_Type)
      return Vector_Range
   is (Vector_Range (Target_Event.Vector));

   Null_Target_Event : constant Target_Event_Type := Target_Event_Type'
     (Kind   => No_Action,
      Vector => Invalid_Vector);

end Skp.Events;
