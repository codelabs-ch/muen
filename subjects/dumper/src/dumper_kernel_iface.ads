with Skp;

with SK;

package Dumper_Kernel_Iface
is

   --  Return state of subject specified by id.
   function Get_Subject_State
     (Id : Skp.Subject_Id_Type)
      return SK.Subject_State_Type;

end Dumper_Kernel_Iface;
