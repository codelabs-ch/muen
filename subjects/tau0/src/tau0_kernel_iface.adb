with System;

package body Tau0_Kernel_Iface
--# own
--#    State is out New_Major, Index;
is

   type Index_Type is mod 2 ** 1;

   Index : Index_Type := 0;

   New_Major : SK.Major_Frame_Range;
   for New_Major'Address use System'To_Address (16#4000#);
   pragma Atomic (New_Major);

   -------------------------------------------------------------------------

   procedure Switch_Major_Frame
   --# global
   --#       out New_Major;
   --#    in out Index;
   --# derives
   --#    Index     from * &
   --#    New_Major from Index;
   is
   begin
      Index     := Index + 1;
      New_Major := SK.Major_Frame_Range (Index);
   end Switch_Major_Frame;

end Tau0_Kernel_Iface;
