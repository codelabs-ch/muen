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

with System;

with Interfaces;

with Mucontrol.Status;

package Mngr.Status
is

   --  Returns the current status for subject specified by ID.
   function Get_Status
     (ID : Managed_Subjects_Range)
      return Mucontrol.Status.Status_Type
   with Volatile_Function;

   --  Returns the current watchdog value for subject specified by ID.
   function Get_Watchdog
     (ID : Managed_Subjects_Range)
      return Interfaces.Unsigned_64
   with Volatile_Function;

   --  Returns the current diagnostics value for subject specified by ID.
   function Get_Diagnostics
     (ID : Managed_Subjects_Range)
      return Mucontrol.Status.Diagnostics_Type
   with Volatile_Function;

private

   package Cspecs renames Manager_Component.Memory_Arrays;

   type Status_Array is array (Managed_Subjects_Range)
     of Mucontrol.Status.Status_Page_Type
   with
      Object_Size => Cspecs.Status_Element_Size * Cspecs.Status_Element_Count
        * 8;

   Status_Pages : Status_Array
   with
      Import,
      Volatile,
      Async_Writers,
      Address => System'To_Address (Cspecs.Status_Address_Base),
      Size    => Cspecs.Status_Element_Size * Cspecs.Status_Element_Count
        * 8;

end Mngr.Status;
