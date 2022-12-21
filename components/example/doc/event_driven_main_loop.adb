Main_Loop :
loop
   if not Pending_Interrupt_Present then

      --  Go to sleep when no pending interrupts are present.

      CPU.Hlt;
   end if;

   --  Clear pending interrupts prior to processing by writing to our own
   --  interrupts page that is mapped into our address space using the
   --  monitor mechanism. This way interrupts that arrive while we are
   --  processing the ones that woke us up from sleep, will be marked as
   --  pending and not get lost.

   Clear_Pending_Interrupts;

   Process_Loop :
   loop
      Channel.Read (Data         => Buffer,
                    Data_Present => Received);
      exit when not Received;
      Process (Data => Buffer);
   end loop Process_Loop;
end loop Main_Loop;
