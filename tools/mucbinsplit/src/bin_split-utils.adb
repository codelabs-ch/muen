with Interfaces;
use type Interfaces.Unsigned_64;

with Mutools.Constants;

package body Bin_Split.Utils is

   function Round_To_Page
     (Address : Interfaces.Unsigned_64)
      return Interfaces.Unsigned_64
   is
      P : constant Interfaces.Unsigned_64 := Mutools.Constants.Page_Size;
   begin
      return ((Address + P - 1) / P) * P;
   end Round_To_Page;

end Bin_Split.Utils;
