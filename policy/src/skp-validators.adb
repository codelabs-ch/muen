with SK.Utils;

package body Skp.Validators
is

   -------------------------------------------------------------------------

   procedure Validate (Region : Memory_Region_Type)
   is
      use type SK.Word64;
   begin
      if Region.Size mod Region.Alignment /= 0 then
         raise Validation_Error with "Invalid memory region size "
           & SK.Utils.To_Hex (Item => Region.Size)
           & " for specified alignment "
           & SK.Utils.To_Hex (Item => Region.Alignment);
      end if;
   end Validate;

end Skp.Validators;
