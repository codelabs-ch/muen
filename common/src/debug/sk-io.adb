with System.Machine_Code;

package body SK.IO
is

   -------------------------------------------------------------------------

   procedure Inb
     (Port  :     Word16;
      Value : out Byte)
   is
   begin
      System.Machine_Code.Asm
        (Template => "inb %1, %0",
         Inputs   => (Word16'Asm_Input ("d", Port)),
         Outputs  => (Byte'Asm_Output ("=a", Value)),
         Volatile => True);
   end Inb;

   -------------------------------------------------------------------------

   procedure Outb
     (Port  : Word16;
      Value : Byte)
   is
   begin
      System.Machine_Code.Asm
        (Template => "outb %0, %1",
         Inputs   => (Byte'Asm_Input ("a", Value),
                      Word16'Asm_Input ("d", Port)),
         Volatile => True);
   end Outb;

end SK.IO;
