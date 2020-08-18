--
--  Copyright (C) 2020  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2020  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Ctrlr.Status
is

   -------------------------------------------------------------------------

   function Get_Diagnostics
     (ID : Managed_Subjects_Range)
      return Mucontrol.Status.Diagnostics_Type
   is
      Cur_Value : constant Mucontrol.Status.Diagnostics_Type
        := Status_Pages (ID).Diagnostics;
   begin
      return Cur_Value;
   end Get_Diagnostics;

   -------------------------------------------------------------------------

   function Get_Status
     (ID : Managed_Subjects_Range)
      return Mucontrol.Status.Status_Type
   is
      Cur_Value : constant Mucontrol.Status.Status_Type
        := Status_Pages (ID).Status;
   begin
      return Cur_Value;
   end Get_Status;

   -------------------------------------------------------------------------

   function Get_Watchdog
     (ID : Managed_Subjects_Range)
      return Interfaces.Unsigned_64
   is
      Cur_Value : constant Interfaces.Unsigned_64
        := Status_Pages (ID).Watchdog;
   begin
      return Cur_Value;
   end Get_Watchdog;

end Ctrlr.Status;
