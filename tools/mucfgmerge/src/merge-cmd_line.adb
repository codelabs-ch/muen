--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Merge.Cmd_Line
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

   function Get_Additional_Hardware_File return String
   is
   begin
      return S (Additional_Hw_File);
   end Get_Additional_Hardware_File;

   -------------------------------------------------------------------------

   function Get_Config_File return String
   is
   begin
      return S (Config_File);
   end Get_Config_File;

   -------------------------------------------------------------------------

   function Get_Output_File return String
   is
   begin
      return S (Output_File);
   end Get_Output_File;

   -------------------------------------------------------------------------

   function Get_Platform_File return String
   is
   begin
      return S (Platform_File);
   end Get_Platform_File;

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      use Ada.Strings.Unbounded;

      Cmdline       : Mutools.Cmd_Line.Config_Type;
      Platform      : aliased GNAT.Strings.String_Access;
      Additional_Hw : aliased GNAT.Strings.String_Access;
   begin
      GNAT.Command_Line.Set_Usage
        (Config => Cmdline.Data,
         Usage  => "[options] <config_file> <output_file>",
         Help   => Description);
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => Platform'Access,
         Switch      => "-p:",
         Long_Switch => "--platform:",
         Help        => "Platform XML file");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => Additional_Hw'Access,
         Switch      => "-a:",
         Long_Switch => "--additional-hardware:",
         Help        => "Additional hardware XML file");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Switch      => "-h",
         Long_Switch => "--help",
         Help        => "Display usage and exit");

      begin
         GNAT.Command_Line.Getopt
           (Config => Cmdline.Data,
            Parser => Parser);

         if Additional_Hw'Length /= 0 then
            Additional_Hw_File := U (Additional_Hw.all);
         end if;
         GNAT.Strings.Free (X => Additional_Hw);

         if Platform'Length /= 0 then
            Platform_File := U (Platform.all);
         end if;
         GNAT.Strings.Free (X => Platform);

      exception
         when GNAT.Command_Line.Invalid_Switch |
              GNAT.Command_Line.Exit_From_Command_Line =>
            raise Invalid_Cmd_Line;
         when GNAT.Command_Line.Invalid_Parameter =>
            GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
            raise Invalid_Cmd_Line;
      end;

      Config_File := U (GNAT.Command_Line.Get_Argument (Parser => Parser));
      Output_File := U (GNAT.Command_Line.Get_Argument (Parser => Parser));

      if Output_File = Null_Unbounded_String
        or Platform_File = Null_Unbounded_String
        or Config_File = Null_Unbounded_String
      then
         GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
         raise Invalid_Cmd_Line;
      end if;
   end Init;

end Merge.Cmd_Line;
