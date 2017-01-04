--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Stackcheck.Types
is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   function Get_Max_Stack_Usage (Subprogram : Subprogram_Type) return Natural
   is
   begin
      return Subprogram.Max_Stack_Usage;
   end Get_Max_Stack_Usage;

   -------------------------------------------------------------------------

   function Get_Name (Subprogram : Subprogram_Type) return String
   is
   begin
      return To_String (Subprogram.Name);
   end Get_Name;

   -------------------------------------------------------------------------

   function Get_Stack_Usage (Subprogram : Subprogram_Type) return Natural
   is
   begin
      return Subprogram.Own_Stack_Usage;
   end Get_Stack_Usage;

end Stackcheck.Types;
