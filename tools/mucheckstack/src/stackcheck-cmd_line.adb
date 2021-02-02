--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Stackcheck.Cmd_Line
is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   function Get_Allow_Dynamic return Boolean
   is (Allow_Dynamic);

   -------------------------------------------------------------------------

   function Get_GPR_File return String
   is (To_String (GPR_File));

   -------------------------------------------------------------------------

   function Get_Stack_Limit return Natural
   is (Stack_Limit);

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      Cmdline   : Mutools.Cmd_Line.Config_Type;
      P_File    : aliased GNAT.Strings.String_Access;
      Stack_L   : aliased Integer := -1;
      Allow_Dyn : aliased Boolean := False;
   begin
      GNAT.Command_Line.Set_Usage
        (Config => Cmdline.Data,
         Usage  => "[options]",
         Help   => Description);
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => P_File'Access,
         Switch      => "-P:",
         Long_Switch => "--project-file:",
         Help        => "GNAT project file");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => Stack_L'Access,
         Switch      => "-l:",
         Long_Switch => "--limit:",
         Help        => "Limit of stack size in bytes",
         Initial     => -1,
         Default     => -1);
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => Allow_Dyn'Access,
         Switch      => "-d",
         Long_Switch => "--allow-dynamic",
         Help        => "Allow subprograms with dynamic unbounded stacks");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Switch      => "-h",
         Long_Switch => "--help",
         Help        => "Display usage and exit");

      begin
         GNAT.Command_Line.Getopt
           (Config => Cmdline.Data,
            Parser => Parser);
         if P_File'Length /= 0 then
            GPR_File := To_Unbounded_String (P_File.all);
         end if;

         Stack_Limit   := Stack_L;
         Allow_Dynamic := Allow_Dyn;

         GNAT.Strings.Free (X => P_File);

      exception
         when GNAT.Command_Line.Invalid_Switch |
              GNAT.Command_Line.Exit_From_Command_Line =>
            raise Invalid_Cmd_Line;
         when GNAT.Command_Line.Invalid_Parameter =>
            GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
            raise Invalid_Cmd_Line;
      end;

      if GPR_File = Null_Unbounded_String
        or not (Stack_Limit in Natural)
      then
         GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
         raise Invalid_Cmd_Line;
      end if;
   end Init;

end Stackcheck.Cmd_Line;
