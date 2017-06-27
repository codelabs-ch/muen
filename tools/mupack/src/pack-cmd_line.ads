--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Pack.Cmd_Line
is

   --  Init command line, use given tool description in usage output.
   procedure Init (Description : String);

   --  Return policy filename.
   function Get_Policy return String;

   --  Return output directory.
   function Get_Output_Dir return String;

   --  Return name of system image.
   function Get_Output_Imgname return String;

   --  Retun input directory.
   function Get_Input_Dir return String;

   Invalid_Cmd_Line : exception;

private

   Policy, Output_Dir, Input_Dir : Ada.Strings.Unbounded.Unbounded_String;

   Output_Imgname : Ada.Strings.Unbounded.Unbounded_String
     := Ada.Strings.Unbounded.To_Unbounded_String ("muen.img");

   Dry_Run : Boolean;

   Parser : GNAT.Command_Line.Opt_Parser
     := GNAT.Command_Line.Command_Line_Parser;

end Pack.Cmd_Line;
