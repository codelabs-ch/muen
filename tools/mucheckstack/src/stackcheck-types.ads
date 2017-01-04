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

private with Ada.Strings.Unbounded;

package Stackcheck.Types
is

   --  Subprogram information related to stack usage.
   type Subprogram_Type is private;

   --  Returns the name of the given subprogram.
   function Get_Name (Subprogram : Subprogram_Type) return String;

   --  Returns the stack usage of the given subprogram.
   function Get_Stack_Usage (Subprogram : Subprogram_Type) return Natural;

   --  Returns the worst-case stack usage of the subprogram by considering all
   --  calls made.
   function Get_Max_Stack_Usage (Subprogram : Subprogram_Type) return Natural;

private

   type Subprogram_Type is record
      Name            : Ada.Strings.Unbounded.Unbounded_String;
      Own_Stack_Usage : Natural;
      Max_Stack_Usage : Natural;
   end record;

end Stackcheck.Types;
