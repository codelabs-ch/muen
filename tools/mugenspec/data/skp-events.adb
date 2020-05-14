--  Disable line length check
pragma Style_Checks ("-m");

package body Skp.Events
is

   type Trap_Table_Type is array (Trap_Range) of Source_Event_Type;

   type Source_Event_Table_Type is array (Event_Range) of Source_Event_Type;

   Null_Source_Event_Table : constant Source_Event_Table_Type
     := Source_Event_Table_Type'(others => Null_Source_Event);

   type Target_Event_Table_Type is array (Event_Range) of Target_Event_Type;

   Null_Target_Event_Table : constant Target_Event_Table_Type
     := Target_Event_Table_Type'(others => Null_Target_Event);

   type Subject_Events_Type is record
      Source_Traps  : Trap_Table_Type;
      Source_Events : Source_Event_Table_Type;
      Target_Events : Target_Event_Table_Type;
   end record;

   type Subjects_Events_Array is array (Global_Subject_ID_Type)
     of Subject_Events_Type;

   Subject_Events : constant Subjects_Events_Array := Subjects_Events_Array'(
      0 => Subject_Events_Type'(
       Source_Traps  => Trap_Table_Type'(
           0 |  2 |  3 |  4 |  5 |  6 |
           8 |  9 | 10 | 11 | 12 | 13 |
          14 | 15 | 16 | 17 | 18 | 19 |
          20 | 21 | 22 | 23 | 24 | 25 |
          26 | 27 | 28 | 29 | 30 | 31 |
          32 | 33 | 34 | 36 | 37 | 39 |
          40 | 43 | 44 | 45 | 46 | 47 |
          49 | 50 | 51 | 53 | 54 | 55 |
          56 | 57 | 58 | 59 => (
            Source_Action  => No_Action,
            Target_Subject => 2,
            Target_Event   => 0,
            Handover       => True,
            Send_IPI       => False,
            IRQ_Number     => 0),
          48 => (
            Source_Action  => No_Action,
            Target_Subject => 2,
            Target_Event   => 1,
            Handover       => True,
            Send_IPI       => False,
            IRQ_Number     => 0),
           1 |  7 | 35 | 38 | 41 | 42 |
          52 => (
            Source_Action  => System_Panic,
            Target_Subject => Invalid_Subject,
            Target_Event   => Invalid_Target_Event,
            Handover       => False,
            Send_IPI       => False,
            IRQ_Number     => 0)),
       Source_Events => Source_Event_Table_Type'(
          17 => (
            Source_Action  => No_Action,
            Target_Subject => 1,
            Target_Event   => 0,
            Handover       => False,
            Send_IPI       => True,
            IRQ_Number     => 0),
          18 => (
            Source_Action  => No_Action,
            Target_Subject => 2,
            Target_Event   => 0,
            Handover       => True,
            Send_IPI       => False,
            IRQ_Number     => 0),
          19 => (
            Source_Action  => System_Reboot,
            Target_Subject => Invalid_Subject,
            Target_Event   => Invalid_Target_Event,
            Handover       => False,
            Send_IPI       => False,
            IRQ_Number     => 0),
          others => Null_Source_Event),
       Target_Events => Target_Event_Table_Type'(
          0 => Target_Event_Type'(
            Kind   => Inject_Interrupt,
            Vector => 32),
          others => Null_Target_Event)),
      1 => Subject_Events_Type'(
       Source_Traps  => Trap_Table_Type'(
         others => (
            Source_Action  => System_Panic,
            Target_Subject => Invalid_Subject,
            Target_Event   => Invalid_Target_Event,
            Handover       => False,
            Send_IPI       => False,
            IRQ_Number     => 0)),
       Source_Events => Source_Event_Table_Type'(
           1 => (
            Source_Action  => Unmask_Irq,
            Target_Subject => Invalid_Subject,
            Target_Event   => Invalid_Target_Event,
            Handover       => False,
            Send_IPI       => False,
            IRQ_Number     => 22),
          others => Null_Source_Event),
       Target_Events => Target_Event_Table_Type'(
          0 => Target_Event_Type'(
            Kind   => Inject_Interrupt,
            Vector => 32),
          others => Null_Target_Event)),
      2 => Subject_Events_Type'(
       Source_Traps  => Trap_Table_Type'(
           0 |  2 |  3 |  4 |  5 |  6 |
           8 |  9 | 10 | 11 | 12 | 13 |
          14 | 15 | 16 | 17 | 18 | 19 |
          20 | 21 | 22 | 23 | 24 | 25 |
          26 | 27 | 28 | 29 | 30 | 31 |
          32 | 33 | 34 | 36 | 37 | 39 |
          40 | 43 | 44 | 45 | 46 | 47 |
          48 | 49 | 50 | 51 | 53 | 54 |
          55 | 56 | 57 | 58 | 59 => (
            Source_Action  => No_Action,
            Target_Subject => 0,
            Target_Event   => 0,
            Handover       => True,
            Send_IPI       => False,
            IRQ_Number     => 0),
           1 |  7 | 35 | 38 | 41 | 42 |
          52 => (
            Source_Action  => System_Panic,
            Target_Subject => Invalid_Subject,
            Target_Event   => Invalid_Target_Event,
            Handover       => False,
            Send_IPI       => False,
            IRQ_Number     => 0)),
       Source_Events => Source_Event_Table_Type'(
           1 => (
            Source_Action  => No_Action,
            Target_Subject => 2,
            Target_Event   => 2,
            Handover       => False,
            Send_IPI       => False,
            IRQ_Number     => 0),
          others => Null_Source_Event),
       Target_Events => Target_Event_Table_Type'(
          1 => Target_Event_Type'(
            Kind   => Inject_Interrupt,
            Vector => 12),
          2 => Target_Event_Type'(
            Kind   => Reset,
            Vector => Invalid_Vector),
          others => Null_Target_Event)),
      3 => Subject_Events_Type'(
       Source_Traps  => Trap_Table_Type'(
         others => (
            Source_Action  => System_Panic,
            Target_Subject => Invalid_Subject,
            Target_Event   => Invalid_Target_Event,
            Handover       => False,
            Send_IPI       => False,
            IRQ_Number     => 0)),
       Source_Events => Null_Source_Event_Table,
       Target_Events => Null_Target_Event_Table));

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
      return Target_Event_Type
   is (Subject_Events (Subject_ID).Target_Events (Event_Nr));

   -------------------------------------------------------------------------

   function Get_Trap
     (Subject_ID : Global_Subject_ID_Type;
      Trap_Nr    : Trap_Range)
      return Trap_Event_Type
   is (Subject_Events (Subject_ID).Source_Traps (Trap_Nr));

end Skp.Events;
