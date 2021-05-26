with Interfaces;

with Musinfo.Instance;

private package Muenblock_Example.Write_Ops
is
   use type Interfaces.Unsigned_64;

   --  Perform write operations (for CI).
   procedure Run
     (Sector_Size :     Interfaces.Unsigned_64;
      Success     : out Boolean)
   with
      Pre => Musinfo.Instance.Is_Valid and Sector_Size > 0;

end Muenblock_Example.Write_Ops;
