with System.Machine_Code;

package body SK.Kernel
is

   -------------------------------------------------------------------------

   procedure Main
   is
   begin
      System.Machine_Code.Asm
        (Template => "movl $0x07690748, 0xB80F0",
         Volatile => True);
   end Main;

end SK.Kernel;
