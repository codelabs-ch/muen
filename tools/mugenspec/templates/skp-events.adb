--  Disable line length check
pragma Style_Checks ("-m");

package body Skp.Events
is

   type Trap_Table_Type is array (Trap_Range) of Source_Event_Type;

   Null_Trap_Table : constant Trap_Table_Type := Trap_Table_Type'
     (others => Null_Source_Event);

   type Source_Event_Table_Type is array (Event_Range) of Source_Event_Type;

   Null_Source_Event_Table : constant Source_Event_Table_Type
     := Source_Event_Table_Type'(others => Null_Source_Event);

   type Event_Action_Table_Type is array (Event_Range) of Event_Action_Type;

   Null_Event_Action_Table : constant Event_Action_Table_Type
     := Event_Action_Table_Type'(others => Null_Event_Action);

   type Subject_Events_Type is record
      Source_Traps  : Trap_Table_Type;
      Source_Events : Source_Event_Table_Type;
      Target_Events : Event_Action_Table_Type;
   end record;

   type Subjects_Events_Array is array (Global_Subject_ID_Type)
     of Subject_Events_Type;

   Subject_Events : constant Subjects_Events_Array := Subjects_Events_Array'(
__events__);

   -------------------------------------------------------------------------

   function Get_Source_Event
     (Subject_ID : Global_Subject_ID_Type;
      Event_Nr   : Event_Range)
      return Source_Event_Type
   is (Subject_Events (Subject_ID).Source_Events (Event_Nr));

   -------------------------------------------------------------------------

   function Get_Target_Event
     (Subject_ID : Global_Subject_ID_Type;
      Event_Nr   : Event_Range)
      return Event_Action_Type
   is (Subject_Events (Subject_ID).Target_Events (Event_Nr));

   -------------------------------------------------------------------------

   function Get_Trap
     (Subject_ID : Global_Subject_ID_Type;
      Trap_Nr    : Trap_Range)
      return Source_Event_Type
   is (Subject_Events (Subject_ID).Source_Traps (Trap_Nr));

end Skp.Events;
