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

with Interfaces;

private with System;

private with Controller_Component.Memory;

package Ctrlr.Slot_Control
is

   --  Returns the current era for the managed subject specified by ID.
   function Get_Current_Era
     (ID : Managed_Subjects_Range)
      return Interfaces.Unsigned_64
   with Volatile_Function;

private

   package Cspecs renames Controller_Component.Memory;

   Padding_Start : constant := 9;

   type Padding_Type is array (Padding_Start .. Cspecs.Slot_Control_1_Size)
     of Interfaces.Unsigned_8
   with Size => (Cspecs.Slot_Control_1_Size - Padding_Start + 1) * 8;

   type Slot_Control_Type is record
      Era     : Interfaces.Unsigned_64 with Atomic;
      Padding : Padding_Type;
   end record
   with Object_Size => Cspecs.Slot_Control_1_Size * 8;

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "This global variable is effectively read-only.");
   Slot_Control_1 : Slot_Control_Type
   with
      Volatile,
      Async_Writers,
      Address => System'To_Address (Cspecs.Slot_Control_1_Address),
      Size    => Cspecs.Slot_Control_1_Size * 8;
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

end Ctrlr.Slot_Control;
