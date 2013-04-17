--# inherit
--#    Skp.Scheduling,
--#    SK;
package SK.MP
--# own
--#    Barrier : SK.Byte;
--# initializes
--#    Barrier;
is

   --  Reset MP barrier.
   procedure Reset_Barrier;
   --# global
   --#    out Barrier;
   --# derives
   --#    Barrier from ;
   --# post
   --#    Barrier = 0;

   --  Blocks until all logical processors are waiting on barrier.
   procedure Wait_For_All;
   --# global
   --#    in out Barrier;
   --# derives
   --#    Barrier from *;

end SK.MP;
