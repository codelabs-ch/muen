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

private with Ada.Strings.Unbounded;
private with Ada.Directories;

with Muxml;

package Mutools.Cmd_Line.Infile_Outdir
is

   --  Init command line, use given tool description in usage output.
   procedure Init (Description : String);

   --  The Run procedure extracts the output directory and the policy path from
   --  the command line options/arguments. It then parses the policy and passes
   --  the output directory and XML data on to the given process procedure.
   procedure Run
     (Kind    : Muxml.Schema_Kind;
      Process : not null access procedure
        (Output_Dir : String;
         Policy     : Muxml.XML_Data_Type));

private

   Policy     : Ada.Strings.Unbounded.Unbounded_String;
   Output_Dir : Ada.Strings.Unbounded.Unbounded_String
     := Ada.Strings.Unbounded.To_Unbounded_String
       (Ada.Directories.Current_Directory);

   Parser : GNAT.Command_Line.Opt_Parser
     := GNAT.Command_Line.Command_Line_Parser;

end Mutools.Cmd_Line.Infile_Outdir;
