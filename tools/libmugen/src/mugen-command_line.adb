--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Mugen.Command_Line
is

   Policy : constant String := GNAT.Command_Line.Get_Argument;

   -------------------------------------------------------------------------

   function Get_Policy return String
   is
   begin
      return Policy;
   end Get_Policy;

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      Cmdline : GNAT.Command_Line.Command_Line_Configuration;
   begin
      GNAT.Command_Line.Set_Usage
        (Config => Cmdline,
         Usage  => "<policy>",
         Help   => Description);
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline,
         Switch      => "-h",
         Long_Switch => "--help",
         Help        => "Display usage and exit");

      begin
         GNAT.Command_Line.Getopt (Config => Cmdline);

      exception
         when GNAT.Command_Line.Invalid_Switch |
              GNAT.Command_Line.Exit_From_Command_Line =>
            GNAT.Command_Line.Free (Config => Cmdline);
            GNAT.OS_Lib.OS_Exit (Status => Natural (Ada.Command_Line.Failure));
      end;

      if Policy'Length = 0 then
         GNAT.Command_Line.Display_Help (Config => Cmdline);
         GNAT.Command_Line.Free (Config => Cmdline);
         GNAT.OS_Lib.OS_Exit (Status => Natural (Ada.Command_Line.Failure));
      end if;

      GNAT.Command_Line.Free (Config => Cmdline);
   end Init;

end Mugen.Command_Line;
