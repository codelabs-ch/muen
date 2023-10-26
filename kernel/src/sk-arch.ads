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

with Skp;

--D @Interface
--D This package defines an architecture-independent interface for
--D architecture-specific implementations.
package SK.Arch
is

   --  Returns the current timestamp counter value in CPU cycles.
   function Get_Current_Timestamp return Word64
   with Volatile_Function;

   --  Load hardware-specific state of subject specified by ID.
   procedure Load_Subject (ID : Skp.Global_Subject_ID_Type);

end SK.Arch;
