with System.Machine_Code;

package body SK.Hypercall
is

   -------------------------------------------------------------------------

   procedure Signal (Number : SK.Byte)
   is
      --# hide Signal;

      Id : SK.Word64 := Word64 (Number);
   begin
      System.Machine_Code.Asm
        (Template => "movq %0, %%rax; vmcall",
         Inputs   => (Word64'Asm_Input ("m", Id)),
         Volatile => True);
   end Signal;

end SK.Hypercall;
