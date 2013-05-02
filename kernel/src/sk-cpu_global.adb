with System;

with Skp.Kernel;

package body SK.CPU_Global
is

   --# accept Warning, 396, Storage, "Not an external (stream) variable";
   Storage : Storage_Type;
   for Storage'Address use System'To_Address (Skp.Kernel.CPU_Store_Address);
   --# end accept;

   pragma Warnings (Off, "* bits of ""Storage"" unused");
   for Storage'Size use 8 * SK.Page_Size;
   pragma Warnings (On,  "* bits of ""Storage"" unused");

   -------------------------------------------------------------------------

   function Get_Current_Minor_Frame return Active_Minor_Frame_Type
   is
   begin
      return Storage.Current_Minor_Frame;
   end Get_Current_Minor_Frame;

   -------------------------------------------------------------------------

   function Get_Current_Subject return Skp.Subject_Id_Type
   is
   begin
      return Storage.Current_Subject;
   end Get_Current_Subject;

   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      Storage := Storage_Type'
        (Current_Subject     => Skp.Subject_Id_Type'First,
         Current_Minor_Frame => Active_Minor_Frame_Type'
           (Id    => Skp.Scheduling.Minor_Frame_Range'First,
            Ticks => 0));
   end Init;

   -------------------------------------------------------------------------

   procedure Set_Current_Minor (Frame : Active_Minor_Frame_Type)
   is
   begin
      Storage.Current_Minor_Frame := Frame;
   end Set_Current_Minor;

   -------------------------------------------------------------------------

   procedure Set_Current_Subject (Id : Skp.Subject_Id_Type)
   is
   begin
      Storage.Current_Subject := Id;
   end Set_Current_Subject;

end SK.CPU_Global;
