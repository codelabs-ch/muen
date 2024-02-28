--
--  Copyright (C) 2013-2024  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2024  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

--D @Interface
--D The Muen pending interrupts mechanism is used to keep track of pending
--D interrupts of a subject. An interrupt with a given number N is considered
--D pending if the bit at position N is set.
--D This package contains declarations for the pending interrupts data
--D structures.
package Muinterrupts
is

   Interrupt_Count          : constant := 256;
   Bits_In_Word             : constant := 64;
   Interrupt_Words          : constant := Interrupt_Count / Bits_In_Word;
   Interrupt_Interface_Size : constant := Interrupt_Count / 8;

   type Interrupt_Word_Type is range 0 .. (Interrupt_Words - 1);

   type Interrupt_Interface_Type is array (Interrupt_Word_Type)
     of Interfaces.Unsigned_64
   with
      Size => Interrupt_Interface_Size * 8;

end Muinterrupts;
