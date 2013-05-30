with Skp;

with SK;

package Knl_States_Interface
is

   --  Return state of subject specified by id.
   function Get_Subject_State
     (Id : Skp.Subject_Id_Type)
      return SK.Subject_State_Type;

   --  Set subject specified by id to given state.
   procedure Set_Subject_State
     (Id    : Skp.Subject_Id_Type;
      State : SK.Subject_State_Type);

end Knl_States_Interface;
