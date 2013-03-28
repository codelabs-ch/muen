with SK.Utils;

package body Skp.Validators
is

   use type SK.Word64;

   -------------------------------------------------------------------------

   procedure Validate (Policy : Policy_Type)
   is
   begin
      if Policy.Vmxon_Address mod SK.Page_Size /= 0 then
         raise Validation_Error with "Invalid VMXON address "
           & SK.Utils.To_Hex (Item => Policy.Vmxon_Address)
           & " - address must be 4k aligned";
      end if;

      if Policy.Vmxon_Address > (16#100000# - SK.Page_Size) then
         raise Validation_Error with "Invalid VMXON address "
           & SK.Utils.To_Hex (Item => Policy.Vmxon_Address)
           & " - address must be below 1m";
      end if;
   end Validate;

   -------------------------------------------------------------------------

   procedure Validate (Region : Memory_Region_Type)
   is
   begin
      if Region.Size mod Region.Alignment /= 0 then
         raise Validation_Error with "Invalid memory region size "
           & SK.Utils.To_Hex (Item => Region.Size)
           & " for specified alignment "
           & SK.Utils.To_Hex (Item => Region.Alignment);
      end if;

      if Region.Physical_Address mod Region.Alignment /= 0 then
         raise Validation_Error with "Invalid memory region physical address "
           & SK.Utils.To_Hex (Item => Region.Physical_Address)
           & " for specified alignment "
           & SK.Utils.To_Hex (Item => Region.Alignment);
      end if;

      if Region.Virtual_Address mod Region.Alignment /= 0 then
         raise Validation_Error with "Invalid memory region virtual address "
           & SK.Utils.To_Hex (Item => Region.Virtual_Address)
           & " for specified alignment "
           & SK.Utils.To_Hex (Item => Region.Alignment);
      end if;
   end Validate;

end Skp.Validators;
