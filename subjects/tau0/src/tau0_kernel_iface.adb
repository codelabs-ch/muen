with System;

with Skp.Scheduling;

use type Skp.Scheduling.Major_Frame_Range;

package body Tau0_Kernel_Iface
--# own
--#    State is out Active_Major, Cur_Major;
is

   Cur_Major : Skp.Scheduling.Major_Frame_Range
     := Skp.Scheduling.Major_Frame_Range'First;

   Active_Major : Skp.Scheduling.Major_Frame_Range;
   for Active_Major'Address use System'To_Address (16#4000#);
   pragma Atomic (Active_Major);

   -------------------------------------------------------------------------

   procedure Switch_Major_Frame
   --# global
   --#       out Active_Major;
   --#    in out Cur_Major;
   --# derives
   --#    Cur_Major    from * &
   --#    Active_Major from Cur_Major;
   is
   begin
      if Cur_Major = Skp.Scheduling.Major_Frame_Range'Last then
         Cur_Major := Skp.Scheduling.Major_Frame_Range'First;
      else
         Cur_Major := Cur_Major + 1;
      end if;

      Active_Major := Cur_Major;
   end Switch_Major_Frame;

end Tau0_Kernel_Iface;
