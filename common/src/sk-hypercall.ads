--# inherit
--#    SK;
package SK.Hypercall
is

   --  Swap caller subject id in scheduling plan with given id. Relaunch the
   --  swapped subject by resetting its state.
   procedure Swap_Relaunch (Subject_Id : SK.Byte);

end SK.Hypercall;
