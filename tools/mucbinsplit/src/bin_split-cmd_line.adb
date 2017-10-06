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

with Ada.Directories;

with Mutools.Cmd_Line;

with Mulog;

with Bin_Split.Utils;

package body Bin_Split.Cmd_Line
is

   function S
     (Source : Ada.Strings.Unbounded.Unbounded_String)
      return String
      renames Ada.Strings.Unbounded.To_String;

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   -------------------------------------------------------------------------

   function Get_Binary return String
     is (S (Binary));

   -------------------------------------------------------------------------

   function Get_Output_Spec return String
     is (S (Output_Spec));

   -------------------------------------------------------------------------

   function Get_Spec return String
     is (S (Spec));

   -------------------------------------------------------------------------

   function Get_Output_Dir return String
     is (S (Output_Dir));

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      use Ada.Strings.Unbounded;

      Cmdline : Mutools.Cmd_Line.Config_Type;

      ----------------------------------------------------------------------

      procedure Callback
        (Switch    : String;
         Parameter : String;
         Section   : String);

      procedure Callback
        (Switch    : String;
         Parameter : String;
         Section   : String)
      is

         pragma Unreferenced (Section);

      begin
         if Switch = "--output-dir" or Switch = "-d" then
            Output_Dir := U (Parameter);
         end if;
      end Callback;

   begin
      GNAT.Command_Line.Set_Usage
        (Config => Cmdline.Data,
         Usage  => "<component spec> <binary> <output component spec>",
         Help   => Description);

      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Switch      => "-d:",
         Long_Switch => "--output-dir=",
         --  Argument    => "<directory name>",
         Help        => "Output directory");

      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Switch      => "-h",
         Long_Switch => "--help",
         Help        => "Display usage and exit");

      begin

         --  The unrestricted access is necessary, since otherwise we would
         --  have to declare `Callback' at library level.
         GNAT.Command_Line.Getopt
           (Config      => Cmdline.Data,
            Parser      => Parser,
            Callback    => Callback'Unrestricted_Access);

      exception
         when GNAT.Command_Line.Invalid_Switch |
              GNAT.Command_Line.Exit_From_Command_Line =>
            raise Invalid_Cmd_Line;
         when GNAT.Command_Line.Invalid_Parameter =>
            GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
            raise Invalid_Cmd_Line;
      end;

      Spec        := U (GNAT.Command_Line.Get_Argument (Parser => Parser));
      Binary      := U (GNAT.Command_Line.Get_Argument (Parser => Parser));
      Output_Spec := U (GNAT.Command_Line.Get_Argument (Parser => Parser));

      Mulog.Log
        (Level => Mulog.Debug,
         Msg   => "Parsed command line parameters:");
      Mulog.Log
        (Level => Mulog.Debug,
         Msg   => "Spec: '" & S (Spec) & "'");
      Mulog.Log
        (Level => Mulog.Debug,
         Msg   => "Binary: '" & S (Binary) & "'");
      Mulog.Log
        (Level => Mulog.Debug,
         Msg   => "Output_Spec: '" & S (Output_Spec) & "'");
      Mulog.Log
        (Level => Mulog.Debug,
         Msg   => "Output_Dir: '" & S (Output_Dir) & "'");

      if Spec = Null_Unbounded_String
        or Binary = Null_Unbounded_String
        or Output_Spec = Null_Unbounded_String
      then
         GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
         raise Invalid_Cmd_Line;
      end if;
   end Init;

   --------------------------------------------------------------------------

   function With_Output_Dir (Filename : String) return String
   is

      Out_Dir : constant String := Get_Output_Dir;

   begin

      if Out_Dir = "" then
         return Filename;
      end if;

      Bin_Split.Utils.Make_Output_Directory (Dir_Name => Out_Dir);

      return Ada.Directories.Compose
        (Containing_Directory => Out_Dir,
         Name                 => Filename);

   end With_Output_Dir;

end Bin_Split.Cmd_Line;
