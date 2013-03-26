--# inherit
--#    X86_64,
--#    SK.CPU,
--#    SK.Constants;
package SK.Apic
is

   --  Place local APIC in x2APIC mode and set bit 8 of the APIC spurious
   --  vector register (SVR).
   procedure Enable;
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *;

end SK.Apic;
