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

with Ada.Command_Line;

with GNAT.OS_Lib;
with GNAT.Strings;
with GNAT.Command_Line;

with Mulog;

package body Mutools.Cmd_Line.Infile_Outdir
is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      Cmdline : Config_Type;
      Out_Dir : aliased GNAT.Strings.String_Access;
   begin
      GNAT.Command_Line.Set_Usage
        (Config => Cmdline.Data,
         Usage  => "[options] <policy>",
         Help   => Description);
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => Out_Dir'Access,
         Switch      => "-o:",
         Long_Switch => "--output-directory:",
         Help        => "Output directory, default is the current directory");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Switch      => "-h",
         Long_Switch => "--help",
         Help        => "Display usage and exit");

      begin
         GNAT.Command_Line.Getopt (Config => Cmdline.Data);
         if Out_Dir'Length /= 0 then
            Output_Dir := To_Unbounded_String (Out_Dir.all);
         end if;
         GNAT.Strings.Free (X => Out_Dir);

      exception
         when GNAT.Command_Line.Invalid_Switch |
              GNAT.Command_Line.Exit_From_Command_Line =>
            GNAT.OS_Lib.OS_Exit (Status => Natural (Ada.Command_Line.Failure));
         when GNAT.Command_Line.Invalid_Parameter =>
            GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
            GNAT.OS_Lib.OS_Exit (Status => Natural (Ada.Command_Line.Failure));
      end;

      Policy := To_Unbounded_String (GNAT.Command_Line.Get_Argument);
      if Policy = Null_Unbounded_String then
         GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
         GNAT.OS_Lib.OS_Exit (Status => Natural (Ada.Command_Line.Failure));
      end if;
   end Init;

   -------------------------------------------------------------------------

   procedure Run
     (Kind    : Muxml.Schema_Kind;
      Process : not null access procedure
        (Output_Dir : String;
         Policy     : Muxml.XML_Data_Type))
   is
      Data        : Muxml.XML_Data_Type;
      Out_Dir     : constant String := To_String (Output_Dir);
      Policy_File : constant String := To_String (Policy);
   begin
      Mulog.Log (Msg => "Using output directory '" & Out_Dir & "'");
      Mulog.Log (Msg => "Processing policy '" & Policy_File & "'");
      Muxml.Parse (Data => Data,
                   Kind => Kind,
                   File => Policy_File);
      Process (Output_Dir => Out_Dir,
               Policy     => Data);
   end Run;

end Mutools.Cmd_Line.Infile_Outdir;
