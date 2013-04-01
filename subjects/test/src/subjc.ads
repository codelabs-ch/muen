with SK;

package SubjC
is

   Counter : SK.Word64;
   pragma Atomic (Counter);

   --  Exported ISR which increments the interrupt counter.
   procedure Increment_Counter;
   pragma Export (C, Increment_Counter, "dispatch_interrupt");

   --  Reset interrupt counter and install ISR.
   procedure Initialize;

end SubjC;
