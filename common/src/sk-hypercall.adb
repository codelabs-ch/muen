with System.Machine_Code;

package body SK.Hypercall
is

   -------------------------------------------------------------------------

   procedure Swap_Relaunch (Subject_Id : SK.Byte)
   is
      --# hide Swap_Relaunch;

      Id : SK.Word64 := Word64 (Subject_Id);
   begin
      System.Machine_Code.Asm
        (Template => "movq %0, %%rax; vmcall",
         Inputs   => (Word64'Asm_Input ("m", Id)),
         Volatile => True);
   end Swap_Relaunch;

end SK.Hypercall;
