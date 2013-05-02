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

   function Get_Major_Length
     (Major_Id : Skp.Scheduling.Major_Frame_Range;
      CPU_ID   : Skp.CPU_Range)
      return Skp.Scheduling.Minor_Frame_Range
   is
   begin
      return Storage.Scheduling_Plan (Major_Id).CPUs (CPU_ID).Length;
   end Get_Major_Length;

   -------------------------------------------------------------------------

   function Get_Minor_Frame
     (Major_Id : Skp.Scheduling.Major_Frame_Range;
      Minor_Id : Skp.Scheduling.Minor_Frame_Range;
      CPU_ID   : Skp.CPU_Range)
      return Skp.Scheduling.Minor_Frame_Type
   is
   begin
      return Storage.Scheduling_Plan (Major_Id).CPUs
        (CPU_ID).Minor_Frames (Minor_Id);
   end Get_Minor_Frame;

   -------------------------------------------------------------------------

   procedure Init
   is
   begin
      Storage := Storage_Type'
        (Scheduling_Plan     => Skp.Scheduling.Null_Scheduling_Plan,
         Current_Subject     => Skp.Subject_Id_Type'First,
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

   -------------------------------------------------------------------------

   procedure Set_Scheduling_Plan (Data : Skp.Scheduling.Scheduling_Plan_Type)
   is
   begin
      Storage.Scheduling_Plan := Data;
   end Set_Scheduling_Plan;

   -------------------------------------------------------------------------

   procedure Swap_Subject
     (Old_Id : Skp.Subject_Id_Type;
      New_Id : Skp.Subject_Id_Type;
      CPU_ID : Skp.CPU_Range)
   is
   begin
      for I in Skp.Scheduling.Major_Frame_Range loop
         for J in Skp.Scheduling.Minor_Frame_Range loop
            if Storage.Scheduling_Plan (I).CPUs (CPU_ID).Minor_Frames
              (J).Subject_Id = Old_Id
            then
               Storage.Scheduling_Plan (I).CPUs (CPU_ID).Minor_Frames
                 (J).Subject_Id := New_Id;
            end if;
         end loop;
      end loop;
   end Swap_Subject;

end SK.CPU_Global;
