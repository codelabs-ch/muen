with SK.CPU;
with SK.Subjects;

package SK.Dump
is

   --  Print state of subject specified by index to kernel console.
   procedure Print_State (Subject : Subjects.Id_Type);

   --  ISR execution environment state.
   type Isr_Context_Type is record
      GPR        : CPU.Registers_Type;
      Vector     : SK.Word64;
      Error_Code : SK.Word64;
      RIP        : SK.Word64;
      CS         : SK.Word64;
      RFLAGS     : SK.Word64;
      RSP        : SK.Word64;
      SS         : SK.Word64;
   end record;

   --  Print ISR execution environment state.
   procedure Print_State (Context : Isr_Context_Type);
   pragma Export (C, Print_State, "dispatch_interrupt");

end SK.Dump;
