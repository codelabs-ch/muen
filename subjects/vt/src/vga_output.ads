package VGA_Output
is

   --  Session slots.
   type Slot_Range is range 1 .. 6;

   --  Set active session slot.
   procedure Set (Slot : Slot_Range);

   --  Synchronize VGA console with active session's framebuffer.
   procedure Sync;

end VGA_Output;
