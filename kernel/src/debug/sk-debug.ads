package SK.Debug
is

   --  Dump execution environment from interrupt context.
   procedure Isr_Dump
     (RIP    : Word64;
      CS     : Word64;
      RFLAGS : Word64;
      RSP    : Word64;
      SS     : Word64);
   pragma Inline (Isr_Dump);

end SK.Debug;
