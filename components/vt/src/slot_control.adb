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

with Vt_Component.Memory;

package body Slot_Control
is

   package Cspecs renames Vt_Component.Memory;

   type Padding_Type is array (9 .. Cspecs.Slot_Control_1_Size)
     of Interfaces.Unsigned_8;

   type Slot_Control_Type is record
      Era     : Interfaces.Unsigned_64 with Atomic;
      Padding : Padding_Type;
   end record
   with Object_Size => Cspecs.Slot_Control_1_Size * 8;

   Slot_Control_1 : Slot_Control_Type := (Era     => 0,
                                          Padding => (others => 0))
   with
      Volatile,
      Async_Readers,
      Address => System'To_Address (Cspecs.Slot_Control_1_Address),
      Size    => Cspecs.Slot_Control_1_Size * 8;

   -------------------------------------------------------------------------

   procedure Reset_Slot_1
   is
      use type Interfaces.Unsigned_64;

      Cur_Era : Interfaces.Unsigned_64 := Slot_Control_1.Era;
   begin
      Cur_Era := Cur_Era + 1;
      Slot_Control_1.Era := Cur_Era;
   end Reset_Slot_1;

end Slot_Control;
