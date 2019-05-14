--
--  Copyright (C) 2019  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2019  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

private with Ada.Strings.Fixed;
private with Ada.Strings.Unbounded;

with Muxml;

package Cmd_Stream
is

   --  Start the command stream generation.
   procedure Run
     (Policy      : in out Muxml.XML_Data_Type;
      Output_File :        String);

private

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   function Trim
     (Source : String;
      Side   : Ada.Strings.Trim_End := Ada.Strings.Left)
      return String
      renames Ada.Strings.Fixed.Trim;

end Cmd_Stream;
