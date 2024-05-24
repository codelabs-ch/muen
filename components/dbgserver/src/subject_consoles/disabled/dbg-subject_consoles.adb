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

package body Dbg.Subject_Consoles
is

   -------------------------------------------------------------------------

   procedure Attach
     (ID      :     Positive;
      Success : out Boolean)
   is
      pragma Unreferenced (ID);
   begin
      Success := False;
   end Attach;

   -------------------------------------------------------------------------

   procedure Detach is null;

   -------------------------------------------------------------------------

   procedure Flush is null;

   -------------------------------------------------------------------------

   procedure Get
     (Data    : out Interfaces.Unsigned_8;
      Success : out Boolean)
   is
   begin
      Data    := 0;
      Success := False;
   end Get;

   -------------------------------------------------------------------------

   procedure Init is null;

   -------------------------------------------------------------------------

   procedure List (Queue : in out Byte_Queue.Queue_Type) is null;

   -------------------------------------------------------------------------

   procedure Put (Data : Interfaces.Unsigned_8) is null;

end Dbg.Subject_Consoles;
