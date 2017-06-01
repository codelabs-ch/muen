--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

private with System;

private with Muschedinfo;

private with Skp.Kernel;

with Skp.Scheduling;

package SK.Scheduling_Info
with
   Abstract_State => (State with External => (Async_Readers, Async_Writers)),
   Initializes    => State
is

   --  Set TSC start/end for scheduling group specified by ID.
   procedure Set_Scheduling_Info
     (ID                 : Skp.Scheduling.Scheduling_Group_Range;
      TSC_Schedule_Start : SK.Word64;
      TSC_Schedule_End   : SK.Word64)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ (ID, TSC_Schedule_Start, TSC_Schedule_End));

private

   use type String;

   pragma Warnings
     (Off,
      "component size overrides size clause for ""Scheduling_Info_Type""",
      Reason => "Reserved memory size is bigger than actual size of type");
   pragma Warnings (GNAT, Off, "*padded by * bits");
   type Sched_Info_Array is array (Skp.Scheduling.Scheduling_Group_Range)
     of Muschedinfo.Scheduling_Info_Type
   with
      Independent_Components,
      Component_Size => Page_Size * 8,
      Alignment      => Page_Size;
   pragma Warnings (GNAT, On, "*padded by * bits");
   pragma Warnings
     (On, "component size overrides size clause for ""Scheduling_Info_Type""");

   --  Scheduling group info regions.
   Sched_Info : Sched_Info_Array
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Part_Of => State,
      Address => System'To_Address (Skp.Kernel.Sched_Group_Info_Address);

end SK.Scheduling_Info;
