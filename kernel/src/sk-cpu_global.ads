with Skp.Scheduling;

--# inherit
--#    Skp.Scheduling,
--#    SK;
package SK.CPU_Global
--# own
--#    State;
is

   --  Currently active minor frame.
   type Active_Minor_Frame_Type is record
      Minor_Id   : Skp.Scheduling.Minor_Frame_Range;
      Subject_Id : Skp.Subject_Id_Type;
   end record;

   --  Initialize per-CPU storage.
   procedure Init;
   --# global
   --#    out State;
   --# derives
   --#    State from ;

   --  Set the currently active minor frame to specified frame.
   procedure Set_Current_Minor (Frame : Active_Minor_Frame_Type);
   --# global
   --#    in out State;
   --# derives
   --#    State from *, Frame;

   --  Returns the currently active minor frame.
   function Get_Current_Minor_Frame return Active_Minor_Frame_Type;
   --# global
   --#    State;

   --  Set the per-CPU scheduling plan.
   procedure Set_Scheduling_Plan (Data : Skp.Scheduling.Major_Frame_Array);
   --# global
   --#    in out State;
   --# derives
   --#    State from *, Data;

   --  Return number of minor frames in given scheduling plan major frame.
   function Get_Major_Length
     (Major_Id : Skp.Scheduling.Major_Frame_Range)
      return Skp.Scheduling.Minor_Frame_Range;
   --# global
   --#    State;

   --  Return scheduling minor frame indexed by major and minor id.
   function Get_Minor_Frame
     (Major_Id : Skp.Scheduling.Major_Frame_Range;
      Minor_Id : Skp.Scheduling.Minor_Frame_Range)
      return Skp.Scheduling.Minor_Frame_Type;
   --# global
   --#    State;

   --  Remove subject specified by Old_Id from the scheduling plan and replace
   --  it with the subject given by New_Id.
   procedure Swap_Subject
     (Old_Id : Skp.Subject_Id_Type;
      New_Id : Skp.Subject_Id_Type);
   --# global
   --#    in out State;
   --# derives
   --#    State from *, Old_Id, New_Id;
   --# pre
   --#    Old_Id /= New_Id;

end SK.CPU_Global;
