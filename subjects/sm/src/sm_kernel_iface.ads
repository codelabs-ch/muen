with Skp;

with SK;

package Sm_Kernel_Iface
is

   --  Return state of subject specified by id.
   function Get_Subject_State
     (Id : Skp.Subject_Id_Type)
      return SK.Subject_State_Type;

   --  Set subject specified by id to given state.
   procedure Set_Subject_State
     (Id    : Skp.Subject_Id_Type;
      State : SK.Subject_State_Type);

end Sm_Kernel_Iface;
