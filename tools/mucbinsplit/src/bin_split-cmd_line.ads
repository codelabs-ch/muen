--
--  Copyright (C) 2017  secunet Security Networks AG
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

with Ada.Strings.Unbounded;

with GNAT.Command_Line;

package Bin_Split.Cmd_Line
is

   --  Init command line, use given tool description in usage output.
   procedure Init (Description : String);

   --  Return path to input component spec.
   function Get_Spec return String;

   --  Return path to ELF binary.
   function Get_Binary return String;

   --  Return path to output component spec.
   function Get_Output_Spec return String;

   Invalid_Cmd_Line : exception;

private

   Spec        : Ada.Strings.Unbounded.Unbounded_String;
   Binary      : Ada.Strings.Unbounded.Unbounded_String;
   Output_Spec : Ada.Strings.Unbounded.Unbounded_String;

   Parser : GNAT.Command_Line.Opt_Parser
     := GNAT.Command_Line.Command_Line_Parser;

end Bin_Split.Cmd_Line;
