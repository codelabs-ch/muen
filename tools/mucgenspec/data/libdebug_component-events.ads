pragma Style_Checks (Off);

package Libdebug_Component.Events
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

   Lib_Event_ID     : constant := 1;
   Lib_Event_Action : constant Source_Event_Action_Kind := No_Action;

end Libdebug_Component.Events;
