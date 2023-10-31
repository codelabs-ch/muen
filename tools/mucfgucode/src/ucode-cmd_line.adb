--
--  Copyright (C) 2023  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2023  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with GNAT.Strings;

with Mutools.Cmd_Line;

package body Ucode.Cmd_Line
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

   function Get_Policy return String is (S (Policy));

   -------------------------------------------------------------------------

   function Get_Ucode_Dir return String is (S (Ucode_Dir));

   -------------------------------------------------------------------------

   function Get_Output_Dir return String is (S (Output_Dir));

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      use Ada.Strings.Unbounded;

      Cmdline : Mutools.Cmd_Line.Config_Type;
      In_File : aliased GNAT.Strings.String_Access;
      Ucode_D : aliased GNAT.Strings.String_Access;
      Out_D   : aliased GNAT.Strings.String_Access;
   begin
      GNAT.Command_Line.Set_Usage
        (Config => Cmdline.Data,
         Usage  => "[options]",
         Help   => Description);
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => In_File'Access,
         Switch      => "-i:",
         Long_Switch => "--input-file:",
         Help        => "Input system policy");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => Ucode_D'Access,
         Switch      => "-u:",
         Long_Switch => "--ucode-dir:",
         Help        => "Path to microcode updates");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => Out_D'Access,
         Switch      => "-o:",
         Long_Switch => "--output-dir:",
         Help        => "Output directory");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Switch      => "-h",
         Long_Switch => "--help",
         Help        => "Display usage and exit");
      begin
         GNAT.Command_Line.Getopt
           (Config => Cmdline.Data,
            Parser => Parser);
         Policy     := U (In_File.all);
         Ucode_Dir  := U (Ucode_D.all);
         Output_Dir := U (Out_D.all);

         GNAT.Strings.Free (X => In_File);
         GNAT.Strings.Free (X => Ucode_D);
         GNAT.Strings.Free (X => Out_D);

         if Policy = Null_Unbounded_String or
           Ucode_Dir = Null_Unbounded_String or
           Output_Dir = Null_Unbounded_String
         then
            raise GNAT.Command_Line.Invalid_Parameter;
         end if;

      exception
         when GNAT.Command_Line.Invalid_Switch |
              GNAT.Command_Line.Exit_From_Command_Line =>
            raise Invalid_Cmd_Line;
         when GNAT.Command_Line.Invalid_Parameter =>
            GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
            raise Invalid_Cmd_Line;
      end;
   end Init;

end Ucode.Cmd_Line;
