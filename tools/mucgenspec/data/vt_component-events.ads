pragma Style_Checks (Off);

package Vt_Component.Events
is

   type Event_Action_Kind is
     (Subject_Sleep,
      Subject_Yield,
      System_Reboot,
      System_Panic,
      System_Poweroff,
      Unmask_Irq,
      No_Action,
      Inject_Interrupt,
      Reset);

   subtype Source_Event_Action_Kind is Event_Action_Kind range
     Subject_Sleep .. No_Action;

   subtype Target_Event_Action_Kind is Event_Action_Kind range
     No_Action .. Reset;

   Handover_ID     : constant := 1;
   Handover_Action : constant Source_Event_Action_Kind := No_Action;

   Unmask_Irq_22_ID     : constant := 2;
   Unmask_Irq_22_Number : constant := 22;
   Unmask_Irq_22_Action : constant Source_Event_Action_Kind := Unmask_Irq;

   Reboot_ID     : constant := 30;
   Reboot_Action : constant Source_Event_Action_Kind := System_Reboot;

   Shutdown_ID     : constant := 31;
   Shutdown_Action : constant Source_Event_Action_Kind := System_Poweroff;

   Timer_Vector : constant := 37;
   Timer_Action : constant Target_Event_Action_Kind := Inject_Interrupt;

   Reset_Action : constant Target_Event_Action_Kind := Reset;

   Foo_Action : constant Target_Event_Action_Kind := No_Action;

end Vt_Component.Events;
