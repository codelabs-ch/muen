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

with Ada.Finalization;

with GNAT.Strings;

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

   type Config_Type is new
     Ada.Finalization.Limited_Controlled with record
      Data : GNAT.Command_Line.Command_Line_Configuration;
   end record;

   overriding
   procedure Finalize (Config : in out Config_Type);

   -------------------------------------------------------------------------

   procedure Finalize (Config : in out Config_Type)
   is
   begin
      GNAT.Command_Line.Free (Config => Config.Data);
   end Finalize;

      -------------------------------------------------------------------------

   function Get_Additional_Hardware_File return String
   is
   begin
      return S (Additional_Hw_File);
   end Get_Additional_Hardware_File;

   -------------------------------------------------------------------------

   function Get_Hardware_File return String
   is
   begin
      return S (Hardware_File);
   end Get_Hardware_File;

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

   function Get_Policy return String
   is
   begin
      return S (Policy);
   end Get_Policy;

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      use Ada.Strings.Unbounded;

      Cmdline       : Config_Type;
      Hardware      : aliased GNAT.Strings.String_Access;
      Additional_Hw : aliased GNAT.Strings.String_Access;
   begin
      GNAT.Command_Line.Set_Usage
        (Config => Cmdline.Data,
         Usage  => "[options] <policy> <output_file>",
         Help   => Description);
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => Hardware'Access,
         Switch      => "-w:",
         Long_Switch => "--hardware:",
         Help        => "Hardware XML file");
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
         if Hardware'Length /= 0 then
            Hardware_File := U (Hardware.all);
         end if;
         GNAT.Strings.Free (X => Hardware);

         if Additional_Hw'Length /= 0 then
            Additional_Hw_File := U (Additional_Hw.all);
         end if;
         GNAT.Strings.Free (X => Additional_Hw);

      exception
         when GNAT.Command_Line.Invalid_Switch |
              GNAT.Command_Line.Exit_From_Command_Line =>
            raise Invalid_Cmd_Line;
         when GNAT.Command_Line.Invalid_Parameter =>
            GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
            raise Invalid_Cmd_Line;
      end;

      Policy      := U (GNAT.Command_Line.Get_Argument (Parser => Parser));
      Output_File := U (GNAT.Command_Line.Get_Argument (Parser => Parser));

      if Policy = Null_Unbounded_String or Output_File = Null_Unbounded_String
        or Hardware_File = Null_Unbounded_String
      then
         GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
         raise Invalid_Cmd_Line;
      end if;
   end Init;

end Merge.Cmd_Line;
