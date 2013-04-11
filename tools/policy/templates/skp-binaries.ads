with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SK;

package Skp.Binaries is

   type Binary_Spec_Type is record
      Path             : Unbounded_String;
      Physical_Address : SK.Word64;
   end record;

   type Binary_Spec_Array is array (Subject_Id_Type) of Binary_Spec_Type;

   Binary_Specs : constant Binary_Spec_Array := (
__binaries__);

end Skp.Binaries;
