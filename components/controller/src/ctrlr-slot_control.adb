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

with Ctrlr.Config;

package body Ctrlr.Slot_Control
is

   -------------------------------------------------------------------------

   function Get_Current_Era
     (ID : Managed_Subjects_Range)
      return Interfaces.Unsigned_64
   is
      Cur_Era : Interfaces.Unsigned_64 := 0;
   begin
      if Config.Instance (ID).Slot_ID = 1 then
         Cur_Era := Slot_Control_1.Era;
      end if;
      return Cur_Era;
   end Get_Current_Era;

end Ctrlr.Slot_Control;
