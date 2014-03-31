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

with Ada.Command_Line;

with GNAT.OS_Lib;
with GNAT.Command_Line;

with Mulog;

package body Mutools.Cmd_Line.Infile_Outfile
is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      Cmdline : Config_Type;
   begin
      GNAT.Command_Line.Set_Usage
        (Config => Cmdline.Data,
         Usage  => "<input_file> <output_file>",
         Help   => Description);
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Switch      => "-h",
         Long_Switch => "--help",
         Help        => "Display usage and exit");

      begin
         GNAT.Command_Line.Getopt (Config => Cmdline.Data);

      exception
         when GNAT.Command_Line.Invalid_Switch |
              GNAT.Command_Line.Exit_From_Command_Line =>
            GNAT.OS_Lib.OS_Exit (Status => Natural (Ada.Command_Line.Failure));
         when GNAT.Command_Line.Invalid_Parameter =>
            GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
            GNAT.OS_Lib.OS_Exit (Status => Natural (Ada.Command_Line.Failure));
      end;

      File_In  := To_Unbounded_String (GNAT.Command_Line.Get_Argument);
      File_Out := To_Unbounded_String (GNAT.Command_Line.Get_Argument);
      if File_In = Null_Unbounded_String
        or File_Out = Null_Unbounded_String
      then
         GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
         GNAT.OS_Lib.OS_Exit (Status => Natural (Ada.Command_Line.Failure));
      end if;
   end Init;

   -------------------------------------------------------------------------

   procedure Run
     (Kind    : Muxml.Schema_Kind;
      Process : Process_Immutable)
   is
      Data       : Muxml.XML_Data_Type;
      Policy_In  : constant String := To_String (File_In);
      Policy_Out : constant String := To_String (File_Out);
   begin
      Mulog.Log (Msg => "Processing policy '" & Policy_In & "'");
      Muxml.Parse (Data => Data,
                   Kind => Kind,
                   File => Policy_In);
      Process (Input_Policy => Data,
               Output_File  => Policy_Out);
      Mulog.Log (Msg => "Sucessfully created policy '" & Policy_Out & "'");
   end Run;

   -------------------------------------------------------------------------

   procedure Run
     (Process : not null access procedure
        (Input_File, Output_File : String))
   is
      F_In  : constant String := To_String (File_In);
      F_Out : constant String := To_String (File_Out);
   begin
      Mulog.Log (Msg => "Processing input file '" & F_In & "'");
      Process (Input_File  => F_In,
               Output_File => F_Out);
      Mulog.Log (Msg => "Sucessfully created output file '" & F_Out & "'");
   end Run;

end Mutools.Cmd_Line.Infile_Outfile;
