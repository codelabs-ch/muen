--# inherit
--#    Skp.Scheduling,
--#    SK;
package SK.MP
--# own
--#    Barrier;
--# initializes
--#    Barrier;
is

   --  Blocks until all logical processors are waiting on barrier.
   procedure Wait_For_All;
   --# global
   --#    in out Barrier;
   --# derives
   --#    Barrier from *;

end SK.MP;
