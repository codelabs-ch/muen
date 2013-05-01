with System;

with Skp.Kernel;

package body SK.CPU_Global
is

   --# accept Warning, 396, Storage, "Not an external (stream) variable";
   Storage : Storage_Type;
   for Storage'Address use System'To_Address (Skp.Kernel.CPU_Store_Address);
   --# end accept;

   -------------------------------------------------------------------------

   procedure Set_Current_Subject (Id : Skp.Subject_Id_Type)
   is
   begin
      Storage.Current_Subject := Id;
   end Set_Current_Subject;

   -------------------------------------------------------------------------

   function Get_Current_Subject return Skp.Subject_Id_Type
   is
   begin
      return Storage.Current_Subject;
   end Get_Current_Subject;

end SK.CPU_Global;
