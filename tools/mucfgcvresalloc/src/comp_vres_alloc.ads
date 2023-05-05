--
--  Copyright (C) 2023 secunet Security Networks AG
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

private with Mutools.Intervals;
private with Mutools.Vres_Alloc.Config;

package Comp_Vres_Alloc
is
   --  The main procedure (start allocation).
   procedure Run
     (Input_Spec       : String;
      Include_Path     : String;
      Output_File_Name : String);

   Validation_Error : exception;

private
   --  The default domains are copied to local variables
   --  in order to be able to change them in unittests.
   Va_Space_Native       : Mutools.Intervals.Interval_Type
     := Mutools.Vres_Alloc.Config.Default_Va_Space_Native;
   Va_Space_Vm           : Mutools.Intervals.Interval_Type
     := Mutools.Vres_Alloc.Config.Default_Va_Space_Vm;
   Vector_Numbers_Domain : Mutools.Intervals.Interval_Type
     := Mutools.Vres_Alloc.Config.Default_Vector_Numbers_Domain;
   Event_Numbers_Domain  : Mutools.Intervals.Interval_Type
     := Mutools.Vres_Alloc.Config.Default_Event_Numbers_Domain;
end Comp_Vres_Alloc;
