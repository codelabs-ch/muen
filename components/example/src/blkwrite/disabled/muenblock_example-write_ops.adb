package body Muenblock_Example.Write_Ops
is

   -------------------------------------------------------------------------

   procedure Run
     (Sector_Size :     Interfaces.Unsigned_64;
      Success     : out Boolean)
   is
      pragma Unreferenced (Sector_Size);
   begin
      Success := True;
   end Run;

end Muenblock_Example.Write_Ops;
