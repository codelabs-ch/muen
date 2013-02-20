with System.Storage_Elements;

with SK.CPU;

package body SK.Subject
is

   --# hide SK.Subject;

   -------------------------------------------------------------------------

   procedure Main
   is
   begin
      CPU.Hlt;
   end Main;

   -------------------------------------------------------------------------

   function Get_Main_Address return SK.Word64
   is
   begin
      return SK.Word64
        (System.Storage_Elements.To_Integer
           (Value => Main'Address));
   end Get_Main_Address;

end SK.Subject;
