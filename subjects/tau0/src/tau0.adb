with SK.KC;

with Tau0_Kernel_Iface;

use type SK.Word32;

--# inherit
--#    SK,
--#    Tau0_Kernel_Iface;

--# main_program
procedure Tau0
--# global
--#    in out Tau0_Kernel_Iface.State;
is
   Counter : SK.Word32;
begin
   Counter := 0;

   pragma Debug (SK.KC.Put_Line (Item => "Tau0: Starting ..."));

   loop
      if Counter mod 2**20 = 0 then
         Tau0_Kernel_Iface.Switch_Major_Frame;
      end if;
      Counter := Counter + 1;
   end loop;
end Tau0;
