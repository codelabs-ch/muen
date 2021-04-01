--
--  Copyright (C) 2021  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2021  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK;

with Isolation_Tests_Component.Memory;

package ITS.Subject_State
is

   package Cspec renames Isolation_Tests_Component.Memory;

   pragma Warnings (Off, "* bits of ""Result_State"" unused");

   Result_State : SK.Subject_State_Type
   with
      Volatile,
      Async_Writers,
      Size    => Cspec.Result_State_Size * 8,
      Address => System'To_Address (Cspec.Result_State_Address);

   pragma Warnings (On, "* bits of ""Result_State"" unused");

end ITS.Subject_State;
