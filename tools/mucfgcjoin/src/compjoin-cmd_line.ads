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

package Compjoin.Cmd_Line
is

   --  Init command line, use given tool description in usage output.
   procedure Init (Description : String);

   --  Return input file.
   function Get_Input_File return String;

   --  Return output file.
   function Get_Output_File return String;

   --  Return comma-separated list of component spec XML files.
   function Get_Component_List return String;

   Invalid_Cmd_Line : exception;

private

   Input_File     : Ada.Strings.Unbounded.Unbounded_String;
   Output_File    : Ada.Strings.Unbounded.Unbounded_String;
   Component_List : Ada.Strings.Unbounded.Unbounded_String;

   Parser : GNAT.Command_Line.Opt_Parser
     := GNAT.Command_Line.Command_Line_Parser;

end Compjoin.Cmd_Line;
