with Skp.Scheduling;

--# inherit
--#    Skp.Scheduling,
--#    SK;
package SK.CPU_Global
--# own
--#    Storage : Storage_Type;
--# initializes
--#    Storage;
is

   --  Currently active minor frame.
   type Active_Minor_Frame_Type is record
      Id    : Skp.Scheduling.Minor_Frame_Range;
      Ticks : SK.Word32;
   end record;

   --  Record used internally to store per-CPU global data.
   type Storage_Type is record
      Current_Subject     : Skp.Subject_Id_Type;
      Current_Minor_Frame : Active_Minor_Frame_Type;
   end record;

   procedure Set_Current_Subject (Id : Skp.Subject_Id_Type);
   --# global
   --#    in out Storage;
   --# derives
   --#    Storage from *, Id;
   --# post
   --#    Storage.Current_Subject = Id;

   function Get_Current_Subject return Skp.Subject_Id_Type;
   --# global
   --#    Storage;
   --# return
   --#    Storage.Current_Subject;

   procedure Set_Current_Minor (Frame : Active_Minor_Frame_Type);
   --# global
   --#    in out Storage;
   --# derives
   --#    Storage from *, Frame;
   --# post
   --#    Storage.Current_Minor_Frame = Frame;

   function Get_Current_Minor_Frame return Active_Minor_Frame_Type;
   --# global
   --#    Storage;
   --# return
   --#    Storage.Current_Minor_Frame;

end SK.CPU_Global;
