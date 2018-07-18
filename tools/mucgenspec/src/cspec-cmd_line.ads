--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Cspec.Cmd_Line
is

   --  Init command line, use given tool description in usage output.
   procedure Init (Description : String);

   --  Return path to input component specification.
   function Get_Input_Spec return String;

   --  Return output directory.
   function Get_Output_Dir return String;

   --  Return output component spec.
   function Get_Output_Spec return String;

   --  Return include path.
   function Get_Include_Path return String;

   --  Return Ada package name. Returns empty string if no package name has
   --  been given on the command line.
   function Get_Package_Name return String;

   Invalid_Cmd_Line : exception;

private

   Output_Dir   : Ada.Strings.Unbounded.Unbounded_String;
   Output_Spec  : Ada.Strings.Unbounded.Unbounded_String;
   Input_Spec   : Ada.Strings.Unbounded.Unbounded_String;
   Include_Path : Ada.Strings.Unbounded.Unbounded_String;
   Package_Name : Ada.Strings.Unbounded.Unbounded_String;

   Parser : GNAT.Command_Line.Opt_Parser
     := GNAT.Command_Line.Command_Line_Parser;

end Cspec.Cmd_Line;
