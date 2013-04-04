with SK;

package Dumper_Kernel_Iface
is

   Max_Subjects : constant := 3;

   subtype Descriptors_Range is Natural range 0 .. Max_Subjects;

   --  Return state of subject specified by id.
   function Get_Subject_State
     (Id : Descriptors_Range)
      return SK.Subject_State_Type;

end Dumper_Kernel_Iface;
