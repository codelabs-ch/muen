--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package SK.Barriers
is

   type Sense_Barrier_Type is private;

   --  Wait on barrier until it is released.
   procedure Wait (Barrier : in out Sense_Barrier_Type)
   with
      Depends => (Barrier =>+ null);

private

   type Sense_Barrier_Type is record
      Sense      : Boolean := False with Atomic;
      Wait_Count : SK.Byte := 0     with Atomic;
   end record
     with Volatile;

end SK.Barriers;
