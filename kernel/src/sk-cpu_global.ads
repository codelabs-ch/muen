with Skp;

--# inherit
--#    Skp;
package SK.CPU_Global
--# own
--#    Storage : Storage_Type;
is

   --  Record used internally to store per-CPU global data.
   type Storage_Type is record
      Current_Subject : Skp.Subject_Id_Type;
   end record;

   procedure Set_Current_Subject (Id : Skp.Subject_Id_Type);
   --# global
   --#    out Storage;
   --# derives
   --#    Storage from Id;
   --# post
   --#    Storage.Current_Subject = Id;

   function Get_Current_Subject return Skp.Subject_Id_Type;
   --# global
   --#    Storage;
   --# return
   --#    Storage.Current_Subject;

end SK.CPU_Global;
