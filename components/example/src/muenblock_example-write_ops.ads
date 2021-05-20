with Interfaces;

with Musinfo.Instance;

private package Muenblock_Example.Write_Ops
is

   --  Perform write operations (for CI).
   procedure Run
     (Sector_Size :     Interfaces.Unsigned_64;
      Success     : out Boolean)
   with
      Pre => Musinfo.Instance.Is_Valid;

end Muenblock_Example.Write_Ops;
