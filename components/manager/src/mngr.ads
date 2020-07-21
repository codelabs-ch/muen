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

with Manager_Component.Memory_Arrays;

package Mngr
is

   type Subjects_Range is range
     0 .. Manager_Component.Memory_Arrays.Control_Element_Count;

   No_Subject : constant Subjects_Range := Subjects_Range'First;

   subtype Managed_Subjects_Range is Subjects_Range
    range 1 .. Subjects_Range'Last;

   --  Run manager.
   procedure Run;

end Mngr;
