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

with Interfaces;

package ITS
is

   pragma Warnings (Off, "use clause for type * has no effect",
                    Reason => "Simplify type usage in child packages");
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64;
   pragma Warnings (On, "use clause for type * has no effect");

   subtype Bounded_String_Range is Natural range 1 .. 255;
   subtype Bounded_String is String (Bounded_String_Range);

   Null_String : constant Bounded_String := (others => ASCII.NUL);

   function Compilation_ISO_Date return String
   with
      Import,
      Convention => Intrinsic;

   function Compilation_Time return String
   with
      Import,
      Convention => Intrinsic;

   function Enclosing_Entity return String
   with
      Import,
      Convention => Intrinsic;

   function Source_Location return String
   with
      Import,
      Convention => Intrinsic;

end ITS;
