--
--  Copyright (C) 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Skp;

package SK.Subjects_Sinfo
with
   Abstract_State => State,
   Initializes    => State
is

   --  Export scheduling info for subject given by ID.
   procedure Export_Scheduling_Info
     (Id                 : Skp.Subject_Id_Type;
      TSC_Schedule_Start : SK.Word64;
      TSC_Schedule_End   : SK.Word64)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ (Id, TSC_Schedule_Start, TSC_Schedule_End));

end SK.Subjects_Sinfo;
