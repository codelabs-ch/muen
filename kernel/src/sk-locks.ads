--# inherit
--#    SK;
package SK.Locks
--# own
--#    State;
--# initializes
--#    State;
is

   --  Spin until lock is acquired.
   procedure Spin_Lock;
   --# global
   --#    in out State;
   --# derives
   --#    State from *;

   --  Unlock.
   procedure Unlock;
   --# global
   --#    in out State;
   --# derives
   --#    State from *;

end SK.Locks;
