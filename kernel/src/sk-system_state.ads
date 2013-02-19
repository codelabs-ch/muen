--# inherit
--#    X86_64,
--#    SK.CPU,
--#    SK.Constants;
package SK.System_State
is

   --  Check validity of initial system state.
   function Is_Valid return Boolean;
   --# global
   --#    X86_64.State;

end SK.System_State;
