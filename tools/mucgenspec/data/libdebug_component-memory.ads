pragma Style_Checks (Off);

package Libdebug_Component.Memory
is

   State_Address    : constant := 16#1000#;
   State_Size       : constant := 16#1000#;
   State_Executable : constant Boolean := False;
   State_Writable   : constant Boolean := True;

end Libdebug_Component.Memory;
