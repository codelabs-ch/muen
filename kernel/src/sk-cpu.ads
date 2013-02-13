--# inherit
--#    SK;
package SK.CPU
is

   --  Execute CPUID instruction.
   procedure CPUID
      (EAX : in out SK.Word32;
       EBX :    out SK.Word32;
       ECX : in out SK.Word32;
       EDX :    out SK.Word32);
   --# derives
   --#    EAX, EBX, ECX, EDX from EAX, ECX;

   --  Return current value of CR0 register.
   function Get_CR0 return SK.Word64;

end SK.CPU;
