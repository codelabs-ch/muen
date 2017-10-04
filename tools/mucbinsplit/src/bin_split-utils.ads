with Interfaces;

package Bin_Split.Utils is

   --  Rounds Address upwards to nearest multiple of memory page size.
   function Round_To_Page
     (Address : Interfaces.Unsigned_64)
      return Interfaces.Unsigned_64;

end Bin_Split.Utils;
