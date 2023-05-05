--
--  Copyright (C) 2023 secunet Security Networks AG
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

package Comp_Vres_Alloc.Cmd_Line
is
   --  Init command line, use given tool description in usage output.
   procedure Init (Description : String);

   --  Return filename of root of component specification.
   function Get_Input_Spec return String;

   --  Return include path.
   function Get_Include_Path return String;

   --  Return filename for output specification.
   function Get_Output_Filename return String;

   Invalid_Cmd_Line : exception;

private

   Input_Spec      : Ada.Strings.Unbounded.Unbounded_String;
   Include_Path    : Ada.Strings.Unbounded.Unbounded_String;
   Output_Filename : Ada.Strings.Unbounded.Unbounded_String;

   Parser : GNAT.Command_Line.Opt_Parser
     := GNAT.Command_Line.Command_Line_Parser;

end Comp_Vres_Alloc.Cmd_Line;
