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

with Skp.Scheduling;

use type Skp.CPU_Range;

package SK.CPU_Global
with
   Abstract_State => State
is

   --  ID of the local CPU.
   CPU_ID : constant Skp.CPU_Range
   with
      Import,
      Convention => C,
      Link_Name  => "cpu_id";

   --  Returns True if the local CPU is the bootstrap processor.
   function Is_BSP return Boolean
   with
      Post => Is_BSP'Result = (CPU_ID = Skp.CPU_Range'First);

   --  Currently active minor frame.
   type Active_Minor_Frame_Type is record
      Minor_Id   : Skp.Scheduling.Minor_Frame_Range;
      Subject_Id : Skp.Subject_Id_Type;
      Barrier    : Skp.Scheduling.Barrier_Index_Range;
      Deadline   : SK.Word64;
   end record;

   --  Initialize per-CPU storage.
   procedure Init
   with
      Global  => (Output => State),
      Depends => (State => null);

   --  Set the currently active minor frame to specified frame.
   procedure Set_Current_Minor (Frame : Active_Minor_Frame_Type)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ Frame);

   --  Returns the currently active minor frame.
   function Get_Current_Minor_Frame return Active_Minor_Frame_Type
   with
      Global  => (Input => State);

   --  Set the per-CPU scheduling plan.
   procedure Set_Scheduling_Plan (Data : Skp.Scheduling.Major_Frame_Array)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ Data);

   --  Return number of minor frames in given scheduling plan major frame.
   function Get_Major_Length
     (Major_Id : Skp.Scheduling.Major_Frame_Range)
      return Skp.Scheduling.Minor_Frame_Range
   with
      Global  => (Input => State);

   --  Return scheduling minor frame indexed by major and minor id.
   function Get_Minor_Frame
     (Major_Id : Skp.Scheduling.Major_Frame_Range;
      Minor_Id : Skp.Scheduling.Minor_Frame_Range)
      return Skp.Scheduling.Minor_Frame_Type
   with
      Global  => (Input => State);

   --  Remove subject specified by Old_Id from the scheduling plan and replace
   --  it with the subject given by New_Id.
   procedure Swap_Subject
     (Old_Id : Skp.Subject_Id_Type;
      New_Id : Skp.Subject_Id_Type)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ (Old_Id, New_Id)),
      Pre     => Old_Id /= New_Id;

end SK.CPU_Global;
