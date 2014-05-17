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
with Ada.Finalization;

with GNAT.OS_Lib;
with GNAT.Strings;

package body Pack.Command_Line
is

   use Ada.Strings.Unbounded;

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

   function Get_Policy return String
   is
   begin
      return To_String (Policy);
   end Get_Policy;

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      Cmdline         : Config_Type;
      Out_Dir, In_Dir : aliased GNAT.Strings.String_Access;
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
         Output      => In_Dir'Access,
         Switch      => "-i:",
         Long_Switch => "--input-directory:",
         Help        => "Directory of input files");
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
         if In_Dir'Length /= 0 then
            Input_Dir := To_Unbounded_String (In_Dir.all);
         end if;
         GNAT.Strings.Free (X => Out_Dir);
         GNAT.Strings.Free (X => In_Dir);

      exception
         when GNAT.Command_Line.Invalid_Switch |
              GNAT.Command_Line.Exit_From_Command_Line =>
            GNAT.OS_Lib.OS_Exit (Status => Natural (Ada.Command_Line.Failure));
         when GNAT.Command_Line.Invalid_Parameter =>
            GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
            GNAT.OS_Lib.OS_Exit (Status => Natural (Ada.Command_Line.Failure));
      end;

      Policy := To_Unbounded_String
        (GNAT.Command_Line.Get_Argument
           (Parser => Parser));
      if Policy = Null_Unbounded_String
        or else Input_Dir = Null_Unbounded_String
        or else Output_Dir = Null_Unbounded_String
      then
         GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
         GNAT.OS_Lib.OS_Exit (Status => Natural (Ada.Command_Line.Failure));
      end if;
   end Init;

end Pack.Command_Line;
