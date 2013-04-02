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

   --  Startup AP processors by sending INIT-SIPI-SIPI IPI sequence, see Intel
   --  SDM 3A chapter 8.4.4.
   procedure Start_AP_Processors;
   --# global
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *;

end SK.Apic;
