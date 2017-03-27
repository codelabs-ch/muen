pragma Style_Checks (Off);

package Inc_Component.Memory
is

   Lowmem_Address    : constant := 16#0002_0000#;
   Lowmem_Size       : constant := 16#0008_0000#;
   Lowmem_Executable : constant Boolean := False;
   Lowmem_Writable   : constant Boolean := True;

end Inc_Component.Memory;
