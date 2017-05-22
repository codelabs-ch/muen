--
--  Copyright (C) 2013, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package SK.CPU_Global
with
   Abstract_State => State
is

   use type Skp.CPU_Range;

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

   --  Initialize per-CPU storage.
   procedure Init
   with
      Global => (Output => State);

   --  Set the ID of the currently active major frame to the specified value.
   procedure Set_Current_Major_Frame (ID : Skp.Scheduling.Major_Frame_Range)
   with
      Global  => (In_Out   => State,
                  Proof_In => CPU_ID),
      Depends => (State =>+ ID),
      Pre     => Is_BSP;

   --  Returns the ID of the currently active major frame.
   function Get_Current_Major_Frame_ID return Skp.Scheduling.Major_Frame_Range
   with
      Global  => (Input => State);

   --  Set the start time of the current major frame to the specified value in
   --  CPU cycles.
   procedure Set_Current_Major_Start_Cycles (TSC_Value : SK.Word64)
   with
      Global  => (In_Out   => State,
                  Proof_In => CPU_ID),
      Depends => (State =>+ TSC_Value),
      Pre     => Is_BSP;

   --  Returns the start of the current major frame in CPU cycles.
   function Get_Current_Major_Start_Cycles return SK.Word64
   with
      Global  => (Input => State);

   --  Set the ID of the currently active minor frame to the specified value.
   procedure Set_Current_Minor_Frame (ID : Skp.Scheduling.Minor_Frame_Range)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ ID);

   --  Returns the ID of the currently active minor frame.
   function Get_Current_Minor_Frame_ID return Skp.Scheduling.Minor_Frame_Range
   with
      Global  => (Input => State);

   --  Set the per-CPU scheduling groups.
   procedure Set_Scheduling_Groups
     (Data : Skp.Scheduling.Scheduling_Group_Array)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ Data);

   --  Returns the subject ID of the currently active scheduling group.
   function Get_Current_Subject_ID return Skp.Subject_Id_Type
   with
      Global  => (Input => (CPU_ID, State));

   --  Set the currently active subject ID of the current scheduling group to
   --  the given value.
   procedure Set_Current_Subject_ID (Subject_ID : Skp.Subject_Id_Type)
   with
      Global  => (Input  => CPU_ID,
                  In_Out => State),
      Depends => (State =>+ (CPU_ID, Subject_ID));

   --  Return number of minor frames in the currently active major frame.
   function Get_Current_Major_Length return Skp.Scheduling.Minor_Frame_Range
   with
      Global  => (Input => (CPU_ID, State));

end SK.CPU_Global;
