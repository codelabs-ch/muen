with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SK;

--  Generated binary definitions may be longer than 79 characters.
--  Switch off this check.
pragma Style_Checks ("-m");

package Skp.Binaries is

   type Binary_Spec_Type is record
      Name             : Unbounded_String;
      Path             : Unbounded_String;
      Physical_Address : SK.Word64;
   end record;

   type Binary_Spec_Array is array (Subject_Id_Type) of Binary_Spec_Type;

   Binary_Specs : constant Binary_Spec_Array := (
__binaries__);

end Skp.Binaries;
