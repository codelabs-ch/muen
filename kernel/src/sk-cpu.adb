with System.Machine_Code;

package body SK.CPU
is

   -------------------------------------------------------------------------

   procedure CPUID
     (EAX : in out SK.Word32;
      EBX :    out SK.Word32;
      ECX : in out SK.Word32;
      EDX :    out SK.Word32)
   is
      --# hide CPUID
   begin
      System.Machine_Code.Asm
        (Template => "cpuid",
         Inputs   => (SK.Word32'Asm_Input ("a", EAX),
                      SK.Word32'Asm_Input ("c", ECX)),
         Outputs  => (SK.Word32'Asm_Output ("=a", EAX),
                      SK.Word32'Asm_Output ("=b", EBX),
                      SK.Word32'Asm_Output ("=c", ECX),
                      SK.Word32'Asm_Output ("=d", EDX)),
         Volatile => True);
   end CPUID;

end SK.CPU;
