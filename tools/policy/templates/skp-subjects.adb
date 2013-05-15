package body Skp.Subjects is

   -------------------------------------------------------------------------

   function Get_CPU_Id (Subject_Id : Skp.Subject_Id_Type) return Skp.CPU_Range
   is
   begin
      --# accept Warning, 444, "CPU_Id is in CPU_Range (obviously)";
      --# assume Subject_Specs (Subject_Id).CPU_Id in Skp.CPU_Range;
      --# end accept;
      return Subject_Specs (Subject_Id).CPU_Id;
   end Get_CPU_Id;

   -------------------------------------------------------------------------

   function Get_PML4_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64
   is
   begin
      --# accept Warning, 444, "PML4_Address is Word64 (obviously)";
      --# assume Subject_Specs (Subject_Id).PML4_Address in SK.Word64;
      --# end accept;
      return Subject_Specs (Subject_Id).PML4_Address;
   end Get_PML4_Address;

   -------------------------------------------------------------------------

   function Get_VMCS_Address
     (Subject_Id : Skp.Subject_Id_Type)
      return SK.Word64
   is
   begin
      --# accept Warning, 444, "VMCS_Address is Word64 (obviously)";
      --# assume Subject_Specs (Subject_Id).VMCS_Address in SK.Word64;
      --# end accept;
      return Subject_Specs (Subject_Id).VMCS_Address;
   end Get_VMCS_Address;

end Skp.Subjects;
