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

private with GNAT.Command_Line;

package Stackcheck.Cmd_Line
is

   --  Init command line, use given tool description in usage output.
   procedure Init (Description : String);

   --  Return GNAT project file.
   function Get_GPR_File return String;

   --  Return stack limit (in bytes).
   function Get_Stack_Limit return Natural;

   --  Return True when subprograms with dynamic stacks are allowed.
   function Get_Allow_Dynamic return Boolean;

   Invalid_Cmd_Line : exception;

private

   GPR_File      : Ada.Strings.Unbounded.Unbounded_String;
   Stack_Limit   : Integer;
   Allow_Dynamic : Boolean;

   Parser : GNAT.Command_Line.Opt_Parser
     := GNAT.Command_Line.Command_Line_Parser;

end Stackcheck.Cmd_Line;
