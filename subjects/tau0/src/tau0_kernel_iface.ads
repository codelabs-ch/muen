with SK;

use type SK.Major_Frame_Range;

--# inherit
--#    SK;
package Tau0_Kernel_Iface
--# own
--#    State;
--# initializes
--#    State;
is

   procedure Switch_Major_Frame;
   --# global
   --#    in out State;
   --# derives
   --#    State from *;

end Tau0_Kernel_Iface;
