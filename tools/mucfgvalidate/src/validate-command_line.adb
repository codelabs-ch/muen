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
with Ada.Finalization;

with GNAT.OS_Lib;

package body Validate.Command_Line
is

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

   function Get_Policy return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Policy);
   end Get_Policy;

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;

      Cmdline : Config_Type;
   begin
      GNAT.Command_Line.Set_Usage
        (Config => Cmdline.Data,
         Usage  => "[options] <policy>",
         Help   => Description);
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Switch      => "-h",
         Long_Switch => "--help",
         Help        => "Display usage and exit");

      begin
         GNAT.Command_Line.Getopt
           (Config => Cmdline.Data,
            Parser => Parser);

      exception
         when GNAT.Command_Line.Invalid_Switch |
              GNAT.Command_Line.Exit_From_Command_Line =>
            GNAT.OS_Lib.OS_Exit (Status => Natural (Ada.Command_Line.Failure));
         when GNAT.Command_Line.Invalid_Parameter =>
            GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
            GNAT.OS_Lib.OS_Exit (Status => Natural (Ada.Command_Line.Failure));
      end;

      Policy := Ada.Strings.Unbounded.To_Unbounded_String
        (GNAT.Command_Line.Get_Argument (Parser => Parser));
      if Policy = Ada.Strings.Unbounded.Null_Unbounded_String then
         GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
         GNAT.OS_Lib.OS_Exit (Status => Natural (Ada.Command_Line.Failure));
      end if;
   end Init;

end Validate.Command_Line;
