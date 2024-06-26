--
--  Copyright (C) 2023  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2023  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Dbg.Byte_Queue;

package Dbg.Subject_Consoles
is

   --  Initialize all subject consoles.
   procedure Init;

   --  Attach to subject console specified by ID. If the ID does not designate a
   --  valid subject console, then Success is set to False.
   procedure Attach
     (ID      :     Positive;
      Success : out Boolean);

   --  Detach from currently attached subject console.
   procedure Detach;

   --  Flush currently attached subject console.
   procedure Flush;

   --  Read data from the currently attached console. Success is True if a
   --  console is attached and valid data was read.
   procedure Get
     (Data    : out Interfaces.Unsigned_8;
      Success : out Boolean);

   --  Forward given data to the currently attached console. If no console is
   --  attached, then the data is silently ignored.
   procedure Put (Data : Interfaces.Unsigned_8);

   --  Output listing of forwarding consoles to given queue.
   procedure List (Queue : in out Byte_Queue.Queue_Type);

   type Keycombo_Array is array (Natural range <>) of Character;

   --  Key combination to detach console.
   Detach_Keys : constant Keycombo_Array (1 .. 2)
     := (1 => ASCII.ESC,
         2 => ASCII.ESC);

end Dbg.Subject_Consoles;
