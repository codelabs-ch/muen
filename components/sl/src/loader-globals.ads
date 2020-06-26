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

with System;

with Interfaces;

with Musinfo;

package Loader.Globals
is
   use type Interfaces.Unsigned_64;

   Target_Sinfo : constant Musinfo.Subject_Info_Type
   with
      Import,
      Address => System'To_Address (Target_Sinfo_Address);

   --  Set sinfo offset to specified value.
   procedure Set_Current_Sinfo_Offset (O : Interfaces.Unsigned_64)
   with
      Post => Get_Current_Sinfo_Offset = O;

   --  Get current sinfo offset value.
   function Get_Current_Sinfo_Offset return Interfaces.Unsigned_64;

private

   Offset : Interfaces.Unsigned_64 := 0;

   function Get_Current_Sinfo_Offset return Interfaces.Unsigned_64 is (Offset);

end Loader.Globals;
