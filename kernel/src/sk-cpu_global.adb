--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with System;

with Skp.Kernel;

package body SK.CPU_Global
--# own
--#    State is Storage;
is

   --  Record used to store per-CPU global data.
   type Storage_Type is record
      Scheduling_Plan     : Skp.Scheduling.Major_Frame_Array;
      Current_Minor_Frame : Active_Minor_Frame_Type;
   end record;

   --# accept Warning, 396, Storage, "Not an external (stream) variable";
   Storage : Storage_Type;
   for Storage'Address use System'To_Address
     (Skp.Kernel.CPU_Store_Address + 8);
   --# end accept;

   pragma Warnings (Off, "* bits of ""Storage"" unused");
   for Storage'Size use 8 * (SK.Page_Size - 8);
   pragma Warnings (On,  "* bits of ""Storage"" unused");

   -------------------------------------------------------------------------

   function Get_Current_Minor_Frame return Active_Minor_Frame_Type
   --# global
   --#    Storage;
   --# return
   --#    Storage.Current_Minor_Frame;
   is
   begin
      return Storage.Current_Minor_Frame;
   end Get_Current_Minor_Frame;

   -------------------------------------------------------------------------

   function Get_Major_Length
     (Major_Id : Skp.Scheduling.Major_Frame_Range)
      return Skp.Scheduling.Minor_Frame_Range
   --# global
   --#    Storage;
   is
   begin
      return Storage.Scheduling_Plan (Major_Id).Length;
   end Get_Major_Length;

   -------------------------------------------------------------------------

   function Get_Minor_Frame
     (Major_Id : Skp.Scheduling.Major_Frame_Range;
      Minor_Id : Skp.Scheduling.Minor_Frame_Range)
      return Skp.Scheduling.Minor_Frame_Type
   --# global
   --#    Storage;
   is
   begin
      return Storage.Scheduling_Plan (Major_Id).Minor_Frames (Minor_Id);
   end Get_Minor_Frame;

   -------------------------------------------------------------------------

   procedure Init
   --# global
   --#    out Storage;
   --# derives
   --#    Storage from ;
   is
   begin
      Storage := Storage_Type'
        (Scheduling_Plan     => Skp.Scheduling.Null_Major_Frames,
         Current_Minor_Frame => Active_Minor_Frame_Type'
           (Minor_Id   => Skp.Scheduling.Minor_Frame_Range'First,
            Subject_Id => Skp.Subject_Id_Type'First));
   end Init;

   -------------------------------------------------------------------------

   procedure Set_Current_Minor (Frame : Active_Minor_Frame_Type)
   --# global
   --#    in out Storage;
   --# derives
   --#    Storage from *, Frame;
   --# post
   --#    Storage.Current_Minor_Frame = Frame;
   is
   begin
      Storage.Current_Minor_Frame := Frame;
   end Set_Current_Minor;

   -------------------------------------------------------------------------

   procedure Set_Scheduling_Plan (Data : Skp.Scheduling.Major_Frame_Array)
   --# global
   --#    in out Storage;
   --# derives
   --#    Storage from *, Data;
   --# post
   --#    Storage.Scheduling_Plan = Data;
   is
   begin
      Storage.Scheduling_Plan := Data;
   end Set_Scheduling_Plan;

   -------------------------------------------------------------------------

   procedure Swap_Subject
     (Old_Id : Skp.Subject_Id_Type;
      New_Id : Skp.Subject_Id_Type)
   --# global
   --#    in out Storage;
   --# derives
   --#    Storage from *, Old_Id, New_Id;
   --# pre
   --#    Old_Id /= New_Id;
   is
   begin
      for I in Skp.Scheduling.Major_Frame_Range loop
         for J in Skp.Scheduling.Minor_Frame_Range loop
            if Storage.Scheduling_Plan (I).Minor_Frames
              (J).Subject_Id = Old_Id
            then
               Storage.Scheduling_Plan (I).Minor_Frames
                 (J).Subject_Id := New_Id;
            end if;
         end loop;
      end loop;

      if Storage.Current_Minor_Frame.Subject_Id = Old_Id then
         Storage.Current_Minor_Frame.Subject_Id := New_Id;
      end if;
   end Swap_Subject;

end SK.CPU_Global;
