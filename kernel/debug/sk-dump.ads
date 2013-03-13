with SK.Subjects;

package SK.Dump
is

   --  Print state of subject specified by index to kernel console.
   procedure Print_State (Subject : Subjects.Id_Type);

   --  Print execution environment from interrupt context.
   procedure Print_Isr_Context
     (RDI : Word64; RSI : Word64; RDX : Word64; RCX : Word64; R08 : Word64;
      R09 : Word64; RAX : Word64; RBX : Word64; RBP : Word64; R10 : Word64;
      R11 : Word64; R12 : Word64; R13 : Word64; R14 : Word64; R15 : Word64;
      Vec : Word64; Err : Word64; RIP : Word64; CS  : Word64; RFL : Word64;
      RSP : Word64; SS  : Word64);
   pragma Export (C, Print_Isr_Context, "dispatch_interrupt");

end SK.Dump;
