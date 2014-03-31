--
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

with Muxml;

package Mutools.Cmd_Line.Infile_Outfile
is

   --  Init command line, use given tool description in usage output.
   procedure Init (Description : String);

   type Process_Immutable is not null access procedure
     (Input_Policy : Muxml.XML_Data_Type;
      Output_File  : String);

   --  The Run procedure extracts the input and output files from the command
   --  line arguments. It then parses the input policy and passes the output
   --  file and XML data on to the given process procedure.
   procedure Run
     (Kind    : Muxml.Schema_Kind;
      Process : Process_Immutable);

   type Process_Mutable is not null access procedure
     (Policy      : in out Muxml.XML_Data_Type;
      Output_File :        String);

   --  The Run procedure extracts the input and output files from the command
   --  line arguments. It then parses the policy and passes the output file and
   --  the mutable XML data on to the given process procedure.
   procedure Run
     (Kind    : Muxml.Schema_Kind;
      Process : Process_Mutable);

   --  The Run procedure extracts the input and output files from the command
   --  line arguments and passes them to the given process procedure.
   procedure Run
     (Process : not null access procedure
        (Input_File, Output_File : String));

private

   File_In, File_Out : Ada.Strings.Unbounded.Unbounded_String;

end Mutools.Cmd_Line.Infile_Outfile;
