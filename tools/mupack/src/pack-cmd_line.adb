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

with GNAT.Strings;

with Mutools.Cmd_Line;

package body Pack.Cmd_Line
is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   function Get_Input_Dir return String
   is
   begin
      return To_String (Input_Dir);
   end Get_Input_Dir;

   -------------------------------------------------------------------------

   function Get_Output_Dir return String
   is
   begin
      return To_String (Output_Dir);
   end Get_Output_Dir;

   -------------------------------------------------------------------------

   function Get_Output_Imgname return String
   is
   begin
      return To_String (Output_Imgname);
   end Get_Output_Imgname;

   -------------------------------------------------------------------------

   function Get_Policy return String
   is
   begin
      return To_String (Policy);
   end Get_Policy;

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      Cmdline                   : Mutools.Cmd_Line.Config_Type;
      Out_Dir, Out_Name, In_Dir : aliased GNAT.Strings.String_Access;
      Dry_Run_Opt               : aliased Boolean;
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
         Help        => "Output directory");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => Out_Name'Access,
         Switch      => "-n:",
         Long_Switch => "--image-name:",
         Help        => "Name of generated image (default: muen.img)");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => In_Dir'Access,
         Switch      => "-i:",
         Long_Switch => "--input-directory:",
         Help        => "Directory of input files");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => Dry_Run_Opt'Access,
         Switch      => "-d",
         Long_Switch => "--dry-run",
         Help        => "Do everything except generate the image");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Switch      => "-h",
         Long_Switch => "--help",
         Help        => "Display usage and exit");

      begin
         GNAT.Command_Line.Getopt
           (Config => Cmdline.Data,
            Parser => Parser);
         if Out_Dir'Length /= 0 then
            Output_Dir := To_Unbounded_String (Out_Dir.all);
         end if;
         if Out_Name'Length /= 0 then
            Output_Imgname := To_Unbounded_String (Out_Name.all);
         end if;
         if In_Dir'Length /= 0 then
            Input_Dir := To_Unbounded_String (In_Dir.all);
         end if;
         GNAT.Strings.Free (X => Out_Dir);
         GNAT.Strings.Free (X => Out_Name);
         GNAT.Strings.Free (X => In_Dir);

         Dry_Run := Dry_Run_Opt;

      exception
         when GNAT.Command_Line.Invalid_Switch |
              GNAT.Command_Line.Exit_From_Command_Line =>
            raise Invalid_Cmd_Line;
         when GNAT.Command_Line.Invalid_Parameter =>
            GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
            raise Invalid_Cmd_Line;
      end;

      Policy := To_Unbounded_String
        (GNAT.Command_Line.Get_Argument
           (Parser => Parser));
      if Policy = Null_Unbounded_String
        or else Input_Dir = Null_Unbounded_String
        or else Output_Dir = Null_Unbounded_String
      then
         GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
         raise Invalid_Cmd_Line;
      end if;
   end Init;

end Pack.Cmd_Line;
