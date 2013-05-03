with Skp.Scheduling;

--# inherit
--#    Skp.Scheduling,
--#    SK;
package SK.CPU_Global
--# own
--#    Storage : Storage_Type;
is

   --  Currently active minor frame.
   type Active_Minor_Frame_Type is record
      Id    : Skp.Scheduling.Minor_Frame_Range;
      Ticks : SK.Word32;
   end record;

   --  Record used internally to store per-CPU global data.
   type Storage_Type is record
      Scheduling_Plan     : Skp.Scheduling.Major_Frame_Array;
      Current_Subject     : Skp.Subject_Id_Type;
      Current_Minor_Frame : Active_Minor_Frame_Type;
   end record;

   procedure Init;
   --# global
   --#    out Storage;
   --# derives
   --#    Storage from ;

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

   procedure Set_Scheduling_Plan (Data : Skp.Scheduling.Major_Frame_Array);
   --# global
   --#    in out Storage;
   --# derives
   --#    Storage from *, Data;
   --# post
   --#    Storage.Scheduling_Plan = Data;

   --  Return number of minor frames in given scheduling plan major frame.
   function Get_Major_Length
     (Major_Id : Skp.Scheduling.Major_Frame_Range)
      return Skp.Scheduling.Minor_Frame_Range;
   --# global
   --#    Storage;

   --  Return scheduling minor frame indexed by major and minor id.
   function Get_Minor_Frame
     (Major_Id : Skp.Scheduling.Major_Frame_Range;
      Minor_Id : Skp.Scheduling.Minor_Frame_Range)
      return Skp.Scheduling.Minor_Frame_Type;
   --# global
   --#    Storage;

   --  Remove subject specified by Old_Id from the scheduling plan and replace
   --  it with the subject given by New_Id.
   procedure Swap_Subject
     (Old_Id : Skp.Subject_Id_Type;
      New_Id : Skp.Subject_Id_Type);
   --# global
   --#    in out Storage;
   --# derives
   --#    Storage from *, Old_Id, New_Id;
   --# pre
   --#    Old_Id /= New_Id;

end SK.CPU_Global;
